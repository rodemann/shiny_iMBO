# PDP V2
#' function that calculates the effect of each feature on the EI
#' @obj.fun.mbo: Fitness function to optimize in the MBO process
#' @design.mbo: initial design as data frame used in the MBO process
#' @ctrl.mbo: Control object form MBO
#' @par.set.mbo: Parameter set describing different aspects of the objective function parameter
#' @sm.learner: the learner used to fit the surrogate model in each iteration. Deafult
#' is a RF. Unfortunately the BO process with GP as sm does not work if we one column in 
#  new.design has same values for every input 
#' @feature: the feature of interest. So far, the function only works for one feature
#' @grid.size: the size of the grid for evaluating the effect
#' @iter: the iteration in which we want to measure the effect
#' @seed: the seed we set in order to generate always the same sm, but with differant initial.data

# to do:
# -works for categorical features
# -parallelization
# -possibility to choose grid.values --> if is.null(grid.values) { expanded.feature <- }
#                                         else {expanded.feature <- grid.values}
library(iml)
library(ggplot2)
library(DiceKriging)
library(mlrMBO)
library(mlr)
library(checkmate)
library(data.table)

effectOnAquisition2 <- function(obj.fun.mbo, 
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
  #idFeatures <- names(resultsMBO[["x"]])
  #feat <- match.arg(feature, idFeatures)
  expanded.feature <- seq(par.set.mbo[["pars"]][[feature]][["lower"]],
                          par.set.mbo[["pars"]][[feature]][["upper"]], 
                          length.out = grid.size)
  
  # for each value in the grid
  # -create new df with the initial data and grid value of the feature of
  #  interest
  # -run for the given df the MBO process / iter and store the proposed point &
  #  the ei at the proposed point
  
  results <- list()
  
  for (i in 1:grid.size) {
    # -we replace the column of the interesting feature with the i-th element 
    #  of 'expandedFeature'
    new.design <- design.mbo
    new.design[,feature] <- expanded.feature[i]
    
    # -we run the same MBO process for the above design and store the results
    set.seed(seed)
    results.mbo <-  mbo(fun = obj.fun.mbo, 
                        design = new.design, 
                        control = ctrl.mbo,
                        learner = sm.learner,
                        show.info = FALSE
    )
    
    opt.path.df <- as.data.frame(results.mbo$opt.path, 
                                 discretes.as.factor = FALSE
    )
    # we select only the interested columns and rows of the opt.path
    results[[i]] <- opt.path.df[
      which(opt.path.df$dob > 0), 
      c(
        # names(ps[["pars"]]),
        # results.mbo[["control"]][["y.name"]],
        results.mbo[["control"]][["infill.crit"]][["id"]],
        "dob"
      )
      ]
    names(results)[[i]] <- expanded.feature[i]
  }
  # we merge the results in a single data.table & store it the global env
  results <- data.table::rbindlist(results, use.names = TRUE, idcol = "grid.value")
  results[,dob := as.factor(dob)]
  results[,grid.value := as.numeric(grid.value)]
  results <<- results
  
  # Plotten der Ergebnisse
  plot <- ggplot(
    results,
    aes(x = dob, y = ei, group = grid.value, color = grid.value)
  ) +
    geom_point() +
    geom_line() +
    labs(
      title = (paste("The effect of feature", feature, "on the acquisition function")),
      x = "iteration"
    ) + 
  scale_color_gradient() 
  plot
}


effectOnAquisition2(obj.fun.mbo = objfun,
                   par.set.mbo = ps,
                   design.mbo = initial.data,
                   ctrl.mbo = ctrl,
                   feature = "dv",
                   grid.size = 10, 
                   seed = 123)

debugonce(effect_on_aquisition)

# Test the colour for the plots
plot <- ggplot(
  results,
  aes(x = dob, y = ei, group = grid.value, color = grid.value)
) +
  geom_point() +
  geom_line() +
  labs(
    title = (paste("The effect of feature", feature, "on the acquisition function")),
    x = "iteration"
  ) + 
  scale_colour_viridis_c(option = "C")
