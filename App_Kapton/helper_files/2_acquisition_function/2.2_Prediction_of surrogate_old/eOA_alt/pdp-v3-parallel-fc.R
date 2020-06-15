# # effectOnAquisition3 is parallelized, works for Mac and Linux, but not for Windows

#' The function measures the effect of any (numerical) feature on the Infill Criteria by changigng
#' the corresponding fetuare values in the initial data design. The results are then plotted. 
#' Teh idea is to generate something like ICE plots. Notice that i) the process is parallelized, so be sure to
#' have >1 cores in your CPU ii) the functions is based on the file 'materials_design.R' 
#' given by Larsko & it therefore uses its saved variables/objects as inputs, plus some additional inputs
#' 
#' @obj.fun.mbo: Fitness function to optimize in the MBO process
#' @design.mbo: initial design as data frame used in the MBO process
#' @ctrl.mbo: Control object from MBO
#' @par.set.mbo: Parameter set describing different aspects of the objective function parameter
#' @sm.learner: the learner used to fit the surrogate model in eacho iteration. Deafult
#' is a RF. Unfortunately the BO process with GP as sm does not work if we one column in
#  new.design has same values for every input
#' @feature: the feature of interest. So far, the function only works for one 
#' numeric feature
#' @grid.size: the size of the grid for evaluating the effect
#' @iter: the iteration in which we want to measure the effect
#' @seed: the seed we set in order to generate always the same sm, but with differant initial.data

library(mlrMBO)
library(mlr)
library(smoof)
library(ggplot2)
library(checkmate)
library(data.table)
library(foreach)
library(doParallel)
#library(tidyverse)
source("fc_notizen/materials-design-playground-fc.R")

effectOnAquisition3P <- function(obj.fun.mbo,
                                 design.mbo,
                                 ctrl.mbo,
                                 par.set.mbo,
                                 sm.learner = makeLearner("regr.randomForest", predict.type = "se"),
                                 feature,
                                 grid.size,
                                 seed) {
  # assertions and checks:
  # -assert that feature is character and 1 value
  # -assert that gridSize is numeric or integer, 1 value. The function should also work for categorical feature
  # -assert iter is a numeric value
  # -assert design.mbo is a df

  # divide the domain of the feature in eqaul parts (grid.size)
  expanded.feature <- seq(
    par.set.mbo[["pars"]][[feature]][["lower"]],
    par.set.mbo[["pars"]][[feature]][["upper"]],
    length.out = grid.size
  )

  # for each value in the grid
  # -create new df with the initial data and grid value of the feature of
  #  interest
  # -run for the given df the MBO process / iter and store the proposed point &
  #  the ei at the proposed point

  # detecting and setting the cores to parallalize calculations
  no.cores <- detectCores() - 1
  registerDoParallel(no.cores)

  results <- foreach(
    i = 1:grid.size,
    .export = names(.GlobalEnv),
    .packages = c("mlrMBO", "smoof", "mlr")
  ) %dopar% {
    # -we replace the column of the interesting feature with the i-th element
    #  of 'expandedFeature'
    new.design <- design.mbo
    new.design[, feature] <- expanded.feature[i]

    # -we run the same MBO process for the above design and store the results
    set.seed(seed)
    res.mbo <- mlrMBO::mbo(
      fun = obj.fun.mbo,
      design = new.design,
      control = ctrl.mbo,
      learner = sm.learner,
      show.info = FALSE
    )

    opt.path.df <- as.data.frame(res.mbo$opt.path,
      discretes.as.factor = FALSE
    )
    # we select only the interested columns and rows of the opt.path
    opt.path.df <- opt.path.df[
      which(opt.path.df$dob > 0),
      c(
        # names(ps[["pars"]]),
        # res.mbo[["control"]][["y.name"]],
        res.mbo[["control"]][["infill.crit"]][["id"]],
        "dob"
      )
    ]
    # opt.path.df
  }

  # clean the cluster
  stopImplicitCluster()

  # we merge the results in a single data.table & store it the global env
  names(results) <- expanded.feature
  results <- data.table::rbindlist(results, use.names = TRUE, idcol = "grid.value")
  results[, dob := as.factor(dob)]
  results[, grid.value := as.numeric(grid.value)]

  # we save the result sin the global enviroment only to check how the results look like, delete
  # this line once the funtion works
  results <<- results

  # Plotten der Ergebnisse
  plot <- ggplot(
    results,
    aes(x = grid.value, y = ei, group = dob, color = dob)
  ) +
    geom_point() +
    geom_line() +
    labs(
      title = (paste("The effect of feature", feature, "on the acquisition function")),
      x = feature
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~dob)
  plot
}


# Test the function
effectOnAquisition3P(
  obj.fun.mbo = objfun,
  par.set.mbo = ps,
  design.mbo = initial.data,
  ctrl.mbo = ctrl,
  feature = "f",
  grid.size = 3,
  seed = 1
)

