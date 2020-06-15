#' This function captures influential instances, we think it might be useful 
#' especially in the MBO because we train our surrogate models with not too many
#' instances. As capturing influentiall instances in each iteration might be expensive
#' (in terms of time and resources), the function only captures infl instances in a specific
#' iteration, given as input by the user, e.g. this might be the iter that outputs the pp with the
#' highest target value. It works in the following way: for measuring the infl inst of pp x in iter i,
#' we use the initial data and the pp in iters 1,...,i-1 as design & run teh mBO process for 1 iter.
#' Our main goals are then to compare the proposed point & compare the predictions of the hypo acq fun 
#' at the original proposed point. Eventually we train a ML model at the end to distinguish between 
#' infl and non infl instances. Please note: does not work for custom ICs
#' @param res.mbo results of the mbo process
#' @param iter iteration in which we want to measure influential instances

# TO DO:
# -p.without anstatt ic.without?
# -works for multiple points -- NOT!
# -eventually parallel -- because with initial design = 100 it takes ca. 10 min
# -add target values to result.1 
# -works also for any possible objfun (in Larsko code objfun also contains model and fun)
# -measure the distance of the true pp to the design
# -predict also the values of the other seen point in the iter
# -ic arguments like se.threshold as arguments of the function, otherwise not possible to change

library(mlrMBO) # mlr, smoof, checkmate, ParamHelpers loaded automatically
library(tidyverse)
library(checkmate)
# library(data.table)
# library(doParallel)
# library(foreach)

# material-design
source("fc_notizen/materials-design-playground-fc.R")
# kapton
source("fc_notizen/kapton-playground-fc.R")
# predict functions
source("2_acquisition_function/2.4_influential_instances/predict-inflInst-fc.R")

inflInst <- function(res.mbo = NULL, iter = NULL, influence = c("absolute", "normal"), ...#seed = NULL
) {
  # assertions and checks
  if (is.null(res.mbo) | is.null(iter)) {
    stop("Either res.mbo or iter argument is NULL. Please specify the arguments")
  }
  assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
  assertNumber(iter, lower = 1, upper = res.mbo$control$iters)
  assertChoice(influence, c("absolute", "normal"))
  
  
  # store the opt.path of the original MBO process
  opdf = as.data.frame(res.mbo$opt.path)
  
  # extract the information need to run the MBO again
  fun.mbo = res.mbo$final.opt.state$opt.problem$fun
  par.set.mbo = res.mbo$opt.path$par.set
  pars.mbo = names(res.mbo$x)
  design.mbo = res.mbo$final.opt.state$opt.problem$design
  learner.mbo = res.mbo$final.opt.state$opt.problem$learner
  control.mbo = res.mbo$control
  infill.mbo = res.mbo$control$infill.crit$id
  
  # change the design
  new.design = opdf[1:(nrow(design.mbo) + iter - 1), pars.mbo]
  new.design <<- new.design
  # change the iters of the process & store the models
  control.mbo = setMBOControlTermination(control.mbo, iters = 1) #control.mbo$iters = 1 does not work!
  control.mbo$store.model.at = 1
  
  #  credits: Christoph Molnar
  result.without = lapply(1:nrow(new.design), function(to.remove.index) {
    not.removed = setdiff(1:nrow(new.design), to.remove.index)
    #set.seed(seed)
    mbo.hypo = mbo(fun = fun.mbo, 
                   design = new.design[not.removed,],
                   learner = learner.mbo,
                   control = control.mbo, 
                   show.info = FALSE
    )
    opdf.hypo = as.data.frame(mbo.hypo$opt.path)
    result.mbo.hypo = list(
      # design of the mbo process without instance i
      design = opdf.hypo[which(opdf.hypo$dob == 0), c(pars.mbo, control.mbo$y.name)],
      # proposed points and value of the of iter i, dob > 0 is fine because only 1 iter is run
      pp.ic = opdf.hypo[which(opdf.hypo$dob > 0),c(pars.mbo, mbo.hypo$control$infill.crit$id)],
      # surrogate model used for the initial fit
      sm = mbo.hypo$models$"1"
    )
  })
  
  # reshaping the results of the mbo processes
  design.without = lapply(result.without, function(x) x["design"]) %>%
    unlist(recursive = FALSE, use.names = FALSE)
  pp.ic.without = lapply(result.without, function(x) x["pp.ic"]) %>% 
    unlist(recursive = FALSE, use.names = FALSE) %>%
    bind_rows()
  sm.without = lapply(result.without, function(x) x["sm"]) %>%
    unlist(recursive = FALSE, use.names = FALSE)
  
  # we do not need this zwingend, delete once it works
  # result.without = list(design.without = design.without, pp.ic.without = pp.ic.without, sm.without = sm.without)
  
  # store the results in the .Globalenv, not needed. Delete once the function works
  # result.without <<- result.without
  
  # 1.compare new pp & the af to the original one
  # true proposed point and value of the af in the iter
  pp.ic.true = opdf[which(opdf$dob == iter), c(pars.mbo, infill.mbo)]
  dimnames(pp.ic.true)[[1]] = "true" # wrong for multiproposal, pay attention
  pp.ic.true <<- pp.ic.true
  
  # 1.1 calculate the distance to the original design points
  if (isNumeric(par.set.mbo))
    metric = "euclidean"
  else metric = "gower"
  # dist.pp.design = cluster::daisy(opdf[which(opdf$dob <= iter), c(pars.mbo)], metric = metric)
  
  # 1.2 computing the distance between original proposed point and proposed points without instance i
  # dist.vec is the distance vector
  dist.vec = rbind(pp.ic.without[, names(pp.ic.without) != infill.mbo], 
                   pp.ic.true[, names(pp.ic.true) != infill.mbo]) %>%
    cluster::daisy(metric = metric) %>% as.matrix() %>% as.data.frame() %>% dplyr::select(true) %>% dplyr::rename(distance = true)
  
  # pp.ic.all is a df with all the pp and their af values (af = acquisition function)
  pp.ic.all = rbind(pp.ic.without, pp.ic.true)
  # we merge pp.ic.without and dist.vec together, and see which instance causes
  # the biggest change in th proposed point
  # NOTE: maybe better with id and ordered rows
  result.1 = cbind(pp.ic.all, dist.vec)
  result.1 = result.1[order(result.1$distance, decreasing = TRUE),]
  
  # delete once he function works
  # result.1 <<- result.1
  
  # 2.evaluate the prediction of the pp in the new model --> ggf. train a CART to 
  #  distinguish infl & non infl instances
  
  # we separate pp and ic
  pp.true = pp.ic.true[,pars.mbo]
  ic.true = pp.ic.true[,infill.mbo]
  
  # 2.1 we compute the prediction of the pp.true with the sm stored in sm.without
  ic.without = switch(infill.mbo,
                      "mean" = sapply(seq_along(sm.without), function(x) {
                        predictMEAN(points = pp.true, models = sm.without[[x]], control = control.mbo)
                      }),
                      "se" = sapply(seq_along(sm.without), function(x) {
                        predictSE(points = pp.true, models = sm.without[[x]])
                      }),
                      "ei" = sapply(seq_along(sm.without), function(x) {
                        predictEI(points = pp.true, models = sm.without[[x]], designs = design.without[[x]], control = control.mbo,
                                  se.threshold = res.mbo$control$infill.crit$params$se.threshold)
                      }),
                      "cb" = sapply(seq_along(sm.without), function(x) {
                        predictCB(points = pp.true, models = sm.without[[x]], designs = design.without[[x]], control = control.mbo,
                                  cb.lambda = res.mbo$control$infill.crit$params$cb.lambda)
                      }),
                      "aei" = sapply(seq_along(sm.without), function(x) {
                        predictAEI(points = pp.true, models = sm.without[[x]], designs = design.without[[x]], 
                                   par.set = res.mbo$opt.path$par.set, control = control.mbo,
                                   aei.use.nugget = res.mbo$control$infill.crit$params$aei.use.nugget#, 
                                   #se.threshold = res.mbo$control$infill.crit$params$se.threshold --> i can not find it!
                        )
                      }),
                      "eqi" = sapply(seq_along(sm.without), function(x) {
                        predictEQI(points = pp.true, models = sm.without[[x]], designs = design.without[[x]], control = control.mbo,
                                   eqi.beta = res.mbo$control$infill.crit$params$eqi.beta#, 
                                   #se.threshold = res.mbo$control$infill.crit$params$se.threshold --> i can not find it!
                        )
                      })
  )
  names(ic.without) = 1:length(ic.without)
  assertNumeric(ic.without, any.missing = FALSE)
  ic.without <<- ic.without
  
  # 2.2 compute the influence of instance i the absolute difference between infill.mbo & ic.without
  influence.vec = switch(influence,
                         "absolute" = abs(ic.true - ic.without),
                         "normal" = (ic.true - ic.without) 
  )
  # NOTe: instance i has a positive influence on the prediction if the ic.true < ic.without (since internally
  #       all the infill criteria are minimized), therefore if ic.true - ic.without < 0
  #influence.vec <<- influence.vec
  
  # 2.3 train a ML model to distinguish infl and non infl instances
  #- for that we need to create a df with the removed instances of the design and their influence on ic.true
  infl.df = cbind(influence.vec,new.design)
  infl.cart = plotMLmodel(data = infl.df, ...)
  # infl.df <<- infl.df
  
  # 2.4 visualization of the influence
  # first we need to calculate the distance between design points and the true proposed point
  dist.vec.true = rbind(pp.ic.without[, names(pp.ic.without) != infill.mbo], 
                        pp.ic.true[, names(pp.ic.true) != infill.mbo]) %>%
    cluster::daisy(metric = metric) %>% as.matrix() %>% as.data.frame() %>%
    dplyr::rename(distance = true)
  
  results = list(task.1 = result.1, influence.analysis = list(df = infl.df, cart = infl.cart, plot))
}

# Test
inflInst(res.mbo = res.mbo, iter = 9, influence = "normal", maxdepth = 2)

debug(inflInst)
undebug(inflInst)
####
dist.vec.true = rbind(new.design, pp.ic.true[, names(pp.ic.true) != "mean"]) %>%
  cluster::daisy(metric = "euclidean") %>% as.matrix() %>% as.data.frame() %>% dplyr::rename(distance = true)
dist.vec.true = dist.vec.true[-nrow(dist.vec.true),"distance"]
df = cbind(id = 1:nrow(new.design), new.design, dist.vec.true, ic.without)

df2 = cbind(new.design, scale(dist.vec.true), ic.without)

plot = ggplot(data = df)


