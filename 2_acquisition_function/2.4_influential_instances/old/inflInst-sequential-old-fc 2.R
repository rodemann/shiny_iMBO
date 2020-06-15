# THIS IS THE SEQUENTIAL VERSION OF inflInstMBOp USED FOR DEBUGGING

# this function captures influential instances, we think it might be useful 
# especially in the MBO because we train our surrogate models with not too many
# instances

# TO DO:
# -works for multiple points

library(mlrMBO) # mlr, smoof, checkmate, ParamHelpers loaded automatically
library(tidyverse)
library(data.table)


source("fc_notizen/materials-design-playground-fc.R")

influentialInstances <- function(
  fun.mbo = NULL,
  design.mbo = NULL,
  learner.mbo = NULL,
  control.mbo = NULL,
  show.info.mbo = getOption("mlrMBO.show.info", TRUE),
  iters.mbo = NULL,
  seed = NULL
) {
  # assertions and checks
  # we overwrite the need to store the surrogate models of the process
  # store every surrogate model -- not perfect yet, gives only warning if sma 1:5 and it+1 is 4
  # if (is.null(control$store.model.at) || control$store.model.at != 1:(control$iters + 1)) {
  #   control = makeMBOControl(control, store.model.at = 1:(iterations + 1))
  # }
  
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
  pp.true = opdf.true[which(opdf.true$dob > 0), names(mbo.true$x)]
  dimnames(pp.true)[[1]] = 1:iters.mbo # wrong for multiproposal, pay attention
  ic.true = data.frame(opdf.true[which(opdf.true$dob > 0), mbo.true$control$infill.crit$id])
  dimnames(ic.true)[[2]] = mbo.true$control$infill.crit$id
  
  result.ii = list()
  
  # change the iters to 1
  control.mbo = setMBOControlTermination(control.mbo, iters = 1)
  
  for (i in seq_len(iters.mbo)) {
    new.design = opdf.true[which(opdf.true$dob < i), names(mbo.true$x)]
    result.iter = lapply(1:nrow(new.design), function(to.remove.index) {
      not.removed = setdiff(1:nrow(new.design), to.remove.index)
      #set.seed(seed)
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
    result.ii[[i]] = list(proposed.points = pp.iter, surrogate.models = sm.iter)
    # result.ii[[i]] = result.iter -- does not work because i do not know how to extract the pp 
  }
  # store the results in the .Globalenv
  result.ii <<- result.ii
  
  # 1.compare new pp & the af to the original one

  # 2.evaluate the prediction of the pp in the new model --> ggf. train a CART to 
  #  distinguish infl & non infl instances
}

# Test
influentialInstances(
  fun.mbo = objfun,
  design.mbo = initial.data,
  control.mbo = ctrl,
  show.info.mbo = FALSE,
  iters.mbo = 2,
  seed = 1
)
debug(influentialInstances)
undebug(influentialInstances)
########
pp.true = opdf.true[which(opdf.true$dob > 0), names(mbo.true$x)]

# in each iteration do:
# -remove one instance & train / run the MBO
# -store the results
########
opdf.hypo = lapply(1:nrow(initial.data), function(to.remove.index) {
  pp.true <- res.mbo.df[which(res.mbo.df$dob == 1), names(ps$pars)]
  set.seed(11)
  not.removed = setdiff(1:nrow(initial.data), to.remove.index)
  mbo.hypo = mbo(fun = objfun, design = initial.data[not.removed,],
                 control = ctrl, show.info = FALSE
                 )
  opdf.hypo = mbo.hypo$opt.path %>% as.data.frame() %>%
    dplyr::select(
      c(
        names(ps$pars), 
        mbo.hypo$control$y.name,
        mbo.hypo$control$infill.crit$id,
        "dob"
      )
    )
  pp.hypo = opdf.hypo[which(opdf.hypo$dob > 0), names(ps$pars)]
  influence.v(pp.true, pp.hypo)
}
)
influence = function(predicted, predicted.without) {
  dist(rbind(predicted, predicted.without), method = "euclidean", diag = TRUE)
}

df <- data.frame(cs)
df <- as.data.frame(df)

#######