# # effectOnAquisition4 is parallelized, works for Mac and Linux, but not for Windows

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
library(tidyverse)
library(foreach)
library(doParallel)

source("fc_notizen/materials-design-playground-fc.R")

# to do:
# -works for categorical features --> for categorical feature different plot, distance measure, expanded feature
# -possibility to choose grid.values --> if is.null(grid.values) { expanded.feature <- }
#                                       else {expanded.feature <- grid.values}
# -works for multiple points and multiple objective functions

effectOnAquisition4PV2 <- function(obj.fun.mbo,
                                 design.mbo,
                                 ctrl.mbo,
                                 par.set.mbo,
                                 sm.learner = makeLearner("regr.randomForest", predict.type = "se"),
                                 feature,
                                 iterations,
                                 grid.size,
                                 seed) {
  # assertions and checks:
  # -assert that feature is character and 1 value
  # -assert that gridSize is numeric or integer, 1 value. The function should also work for categorical feature
  # -assert iter is a numeric value
  # -assert design.mbo is a df
  
  set.seed(seed)
  
  mbo.real <- mlrMBO::mbo(
    fun = obj.fun.mbo,
    design = design.mbo,
    control = ctrl.mbo,
    learner = sm.learner,
    show.info = FALSE
  )
  # store the opt. path in a df
  opt.path.real <- mbo.real$opt.path %>% as.data.frame() %>%
    dplyr::select(
      c(
        names(par.set.mbo[["pars"]]), mbo.real[["control"]][["y.name"]],
        mbo.real[["control"]][["infill.crit"]][["id"]],
        "dob"
      )
    )
  
  # divide the domain of the feature in eqaul parts (grid.size)
  expanded.feature <- seq(
    par.set.mbo[["pars"]][[feature]][["lower"]],
    par.set.mbo[["pars"]][[feature]][["upper"]],
    length.out = grid.size
  )
  # detecting and setting the cores to parallalize calculations
  no.cores <- detectCores() - 1
  registerDoParallel(no.cores)

  results.hypo <- foreach(
      j = seq_len(grid.size),
      .export = c(names(.GlobalEnv),"expanded.feature", "sm.learner"),
      .packages = c("mlrMBO", "smoof", "mlr")
    ) %dopar%  {
      
      results.iter <- list()
      
      for (i in seq_len(iterations)) {
        
        ctrl.mbo <- setMBOControlTermination(ctrl.mbo, iters = 1)
        # -we replace the column of the interesting feature with the i-th element
        #  of 'expandedFeature'
        new.design <- design.mbo
        new.design[, feature] <- expanded.feature[j]
        # -we run the same MBO process for the above design and store the results
        set.seed(seed)
        mbo.hypo <- mlrMBO::mbo(
          fun = obj.fun.mbo,
          design = new.design,
          control = ctrl.mbo,
          learner = sm.learner,
          show.info = FALSE
        )
        
        opt.path.hypo <- mbo.hypo$opt.path %>% as.data.frame() %>%
          dplyr::select(
            c(
              names(par.set.mbo[["pars"]]), mbo.hypo[["control"]][["y.name"]],
              mbo.hypo[["control"]][["infill.crit"]][["id"]],
              "dob"
            )
          )
        results.iter[[i]] <- opt.path.hypo
        # update design (with target value included) before starting iter i + 1
        # alternatively without target: design.mbo <- opt.path.real[, names(par.set.mbo[["pars"]])]
        design.mbo <- mbo.hypo[["opt.path"]][["env"]][["path"]]
      }
      
  }
  
  # clean the cluster
  stopImplicitCluster()
  #remove this line once the code works
  results <- list(real = opt.path.real, hypo = results.hypo)
  results <<-  results
}

#############################
effectOnAquisition4PV2(
  obj.fun.mbo = objfun,
  design.mbo = initial.data,
  ctrl.mbo = ctrl,
  par.set.mbo = ps,
  sm.learner = makeLearner("regr.randomForest", predict.type = "se"),
  feature = "f",
  grid.size = 3,
  iterations = 3,
  seed = 11
)

debug(effectOnAquisition4P)



#############################

# for each iteration do: 
# -update the data
# -run the true MBO process for 1 iteration -- store the PP, dob & acq.fun.
# -do in parallel: create new df with the initial data and grid value of the feature of
#  interest and run for the given df the MBO process / iter and store the proposed point &
#  the ei at the proposed point -- store the PP, dob & acq.fun.
# -store the results in a list
# -calculate the difference between the true PP and the and PP of the pdp.mbo, compare
#  also the EI 


# # select design for the next iters
# new.design <- opt.path.real[, names(par.set.mbo[["pars"]])]
# 
# # proposed points in the iteration
# pp <- opt.path.real[
#   which(opt.path.real$dob > 0),
#   names(par.set.mbo[["pars"]])
#   ]
# 
# # useful information of the mbo opt path
# info.real <- opt.path.real[
#   which(opt.path.real$dob > 0),
#   c(
#     names(ps[["pars"]]),
#     mbo.real[["control"]][["y.name"]],
#     mbo.real[["control"]][["infill.crit"]][["id"]],
#     "dob"
#   )
#   ]