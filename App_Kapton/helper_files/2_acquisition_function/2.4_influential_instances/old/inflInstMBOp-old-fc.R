# THIS IN AN OLD VERSION OF INFLINST WHERE WE WERE INTENDED TO MEASURE INFL INST 
# IN EVERY ITERATION OF THE MBO PROCESS. THIS CAN BE TIME CONSUMING AND NOT OF PRIMER
# INTERESET

#' This function captures influential instances, we think it might be useful 
#' especially in the MBO because we train our surrogate models with not too many
#' instances. It works in the following way: first it runs the original MBO process
#' (please do not run the MBO outside of function, or if so use a seed and use it also when 
#' the function is called) and store the result in the .Globalenv (we use a seed in order to
#' reproduce the same process if needed). Because we can not run the MBO for more than 1 iter and
#' simultaneously remove the proposed points (unless we edit the mbo function) we thought of
#' sequentially adding the proposed points of the original mbo process and refit the MBO for one iter.
#' Our main goals are then to compare the proposed point & compare the predictions of the acq fun 
#' at the proposed point. Eventually we train a ML model at the end to distinguish between 
#' infl and non infl instances. Please note: 
#' 1.be sure to store all the sm with store.model.at 2.the function is parallelized, at the moment works only
#' on Mac (Windows and Linux to come) 3.Function's argument are basically the same of 

# TO DO:
# -works for multiple points
# -works for every InfillCriteria
# -once its finished do also that works for res.mno as input

library(mlrMBO) # mlr, smoof, checkmate, ParamHelpers loaded automatically
library(tidyverse)
library(data.table)
library(foreach)
library(doParallel)


source("fc_notizen/materials-design-playground-fc.R")

inflInstMBOp <- function(
  fun.mbo = NULL,
  design.mbo = NULL,
  learner.mbo = NULL,
  control.mbo = NULL,
  show.info.mbo = getOption("mlrMBO.show.info", TRUE),
  seed = NULL
) {
  # assertions and checks
  # we overwrite the need to store the surrogate models of the process or at least the first one
  # store every surrogate model -- not perfect yet, gives only warning if sma 1:5 and it+1 is 4
  # if (is.null(control$store.model.at) || control$store.model.at != 1:(control$iters + 1)) {
  #   control = makeMBOControl(control, store.model.at = 1:(iterations + 1))
  # }
  
  # store the iters of the original MBO process
  iters.mbo = control.mbo$iters
  
  # make the mbo store surrogate models of each iteration
  control.mbo$store.model.at = 1:(control$iters.mbo + 1)
  
  # run the normal mbo and store the results (pp and value af)
  set.seed(seed)
  mbo.true = mbo(
    fun = fun.mbo,
    design = design.mbo,
    learner = learner.mbo,
    control = control.mbo,
    show.info = show.info.mbo
  )
  # the results of the true / real mbo are stored in the Global.Env 
  mbo.true <<- mbo.true
  
  opdf.true = mbo.true$opt.path %>% as.data.frame() %>%
    dplyr::select(
      c(
        names(mbo.true$x), 
        mbo.true$control$y.name,
        mbo.true$control$infill.crit$id,
        "dob"
      )
    )
  # store all the proposed points & infill values in the BO process
  pp.ic.true = opdf.true[which(opdf.true$dob > 0), c(names(mbo.true$x), mbo.true$control$infill.crit$id)]
  dimnames(pp.true)[[1]] = 1:iters.mbo # wrong for multiproposal, pay attention
  
  # change the iters to 1
  control.mbo = setMBOControlTermination(control.mbo, iters = 1)
  
  # detecting and setting the cores to parallalize calculations
  no.cores <- detectCores() - 1
  registerDoParallel(no.cores)
  
  result.ii <- foreach(
    i = seq_len(iters.mbo)#,
    # .export = c("obj.fun.mbo", "design.mbo", "ctrl.mbo", "par.set.mbo", "sm.learner",
    #             "iterations", "grid.size", "seed", "expanded.feature"),
    # .packages = c("mlrMBO", "dplyr")
  ) %dopar%  {
    new.design = opdf.true[which(opdf.true$dob < i), names(mbo.true$x)]
    result.iter = lapply(1:nrow(new.design), function(to.remove.index) {
      not.removed = setdiff(1:nrow(new.design), to.remove.index)
      #set.seed(seed) -- there is no need of seed right?
      mbo.hypo = mbo(fun = fun.mbo, 
                     design = new.design[not.removed,],
                     learner = learner.mbo,
                     control = control.mbo, 
                     show.info = FALSE
      )
      opdf.hypo = as.data.frame(mbo.hypo$opt.path)
      results.mbo.hypo = list(
        # proposed points and value of the of iter i
        pp.ic = opdf.hypo[which(opdf.hypo$dob > 0),c(names(mbo.hypo$x), mbo.hypo$control$infill.crit$id)],
        # surrogate model used for the initial fit
        sm = mbo.hypo$models$"1"
      )
     })
    pp.iter = lapply(result.iter, function(x) x["pp.ic"]) %>% 
      unlist(recursive = FALSE, use.names = FALSE) %>%
      bind_rows()
    # without pipe:
    # pp.iter = lapply(result.iter, function(x) x["pp.ic"])
    # pp.iter = unlist(pp.iter, recursive = FALSE, use.names = FALSE)
    # pp.iter = dplyr::bind_rows() or eventuall data.table::rbindlist(usenames = TRUE)
    sm.iter = lapply(result.iter, function(x) x["sm"])
    result = list(proposed.points = pp.iter, surrogate.models = sm.iter)
  }
  
  # clean the cluster
  stopImplicitCluster()
  
  # store the results in the .Globalenv, not needed
  result.ii <<- result.ii
  
  # 1.compare new pp & the af to the original one

  # 2.evaluate the prediction of the pp in the new model --> ggf. train a CART to 
  #  distinguish infl & non infl instances
}

# Test
influentialInstancesP(
  fun.mbo = objfun,
  design.mbo = initial.data,
  control.mbo = ctrl,
  show.info.mbo = TRUE,
  seed = 1
)
debug(influentialInstances)
undebug(influentialInstances)
