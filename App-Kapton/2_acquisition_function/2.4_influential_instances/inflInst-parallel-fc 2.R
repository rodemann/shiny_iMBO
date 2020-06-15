# PARALLEL, does not work on WINDOWS so far 

#' @title Captures influential instances of a MBO process
#' 
#' @description Having some insights on influenial instances might be useful especially 
#' in the MBO because we train our surrogate models with not too many instances. 
#' As capturing influential instances in each iteration might be expensive
#' (in terms of time and resources), the function only captures influential instances in a specific
#' iteration, given as input by the user, e.g. this might be the iter that outputs the pp with the
#' highest target value. It works in the following way: for measuring the infl inst of pp x in iter i,
#' we use the initial data and the pp in iters 1,...,i-1 as design & run teh mBO process for 1 iter.
#' Our main goals are then to compare the proposed point & compare the predictions of the hypo acq fun 
#' at the original proposed point. We then train a ML model at the end to distinguish between 
#' infl and non infl instances. We train use a CART because it has nice visualization properties. We do not make parameter tuning because
#' it is not the intention of the model. Its intentiom some intuition on how influential
#' instances may look like.

#' @param res.mbo [\code{\link{MBOSingleObjResult}}]\cr
#' Results of the mbo process
#' @param iter [\code{numeric(1)}]\cr
#' iteration in which we want to measure influential instances
#' @param influence [\code{character(1)}]\cr
#' how should influence be measured, absolute value or normal difference? Default is \code{normal}.
#' @param ... optional argument to pass to the CART model. See \code{\link[rpart]{rpart.control}}].
#' @return A plot visualizing a the influence of possible instances. Also, if stored a list containing 
#' the proposed point of the mbo run without instance i and the results of the influence analysis, namely
#' the ML model and the df used to construct the plot.

# TO DO:
# -flip the sign of infl
# -works also for any possible objfun (in Larsko code objfun also contains model and fun)
# -measure the distance of the true pp to the design and attach to df in result
# -predict also the values of the other seen point in the iter
# -ic arguments like se.threshold as arguments of the function, otherwise not possible to change

library(mlrMBO) # mlr, smoof, checkmate, ParamHelpers loaded automatically
library(tidyverse)
library(checkmate)
library(parallel)
library(rpart.plot)

# material-design
source("fc_notizen/materials-design-playground-fc.R")
# or kapton
source("fc_notizen/kapton-playground-fc.R")
# predict functions
source("2_acquisition_function/2.4_influential_instances/predict-inflInst-fc.R")

inflInstP <- function(res.mbo, iter, influence =  "normal", ...#seed = NULL
) {
  # assertions and checks on inputs
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
  target.mbo = res.mbo$control$y.name
  components.ic = res.mbo$control$infill.crit$components
  
  # change the design
  new.design = opdf[1:(nrow(design.mbo) + iter - 1), pars.mbo]
  # change the iters of the process & store the models
  control.mbo = setMBOControlTermination(control.mbo, iters = 1) #control.mbo$iters = 1 does not work!
  control.mbo$store.model.at = 1
  
  # calulate the number of cores & initiate cluster
  no.cores = detectCores() - 1
  cl = makeCluster(no.cores, type = "FORK") # use type = "PSOCK" for Windows export variables
  
  # # Export varibles and packages
  # vars = c("new.design", "fun.mbo", "par.set.mbo", "pars.mbo", "learner.mbo", "control.mbo",
  #          "infill.mbo"
  #          )
  # clusterExport(cl, varlist = vars, envir = parent.frame())
  # clusterEvalQ(cl, library(mlrMBO))
  
  #  credits: Christoph Molnar
  result.without = parLapply(cl, 1:nrow(new.design), function(to.remove.index) {
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
      design = opdf.hypo[which(opdf.hypo$dob == 0), c(pars.mbo, target.mbo)],
      # proposed points and value of the of iter i, dob > 0 is fine because only 1 iter is run
      pp.ic = opdf.hypo[which(opdf.hypo$dob > 0),c(pars.mbo, target.mbo, infill.mbo)],
      # surrogate model used for the initial fit
      sm = mbo.hypo$models$"1",
      # additional components of the ic
      components = opdf.hypo[which(opdf.hypo$dob > 0),components.ic]
    )
  })
  
  #close the cluster
  stopCluster(cl)
  
  # reshaping the results of the mbo processes
  design.without = lapply(result.without, function(x) x["design"]) %>%
    unlist(recursive = FALSE, use.names = FALSE)
  pp.ic.without = lapply(result.without, function(x) x["pp.ic"]) %>% 
    unlist(recursive = FALSE, use.names = FALSE) %>%
    bind_rows()
  sm.without = lapply(result.without, function(x) x["sm"]) %>%
    unlist(recursive = FALSE, use.names = FALSE)
  components.without = lapply(result.without, function(x) x["components"]) %>%
    unlist(recursive = FALSE, use.names = FALSE)
  # 1.compare new pp & the af to the original one
  # true proposed point and value of the af in the iter
  pp.ic.true = opdf[which(opdf$dob == iter), c(pars.mbo, target.mbo, infill.mbo)]
  dimnames(pp.ic.true)[[1]] = "true"
  
  
  # 1.1 calculate the distance to the original design points
  if (isNumeric(par.set.mbo))
    metric = "euclidean"
  else metric = "gower"
  
  # 1.2 computing the distance between original proposed point and proposed points without instance i
  # dist.vec is the distance vector
  dist.vec = rbind(pp.ic.without[, names(pp.ic.without) != c(target.mbo,infill.mbo)], 
                   pp.ic.true[, names(pp.ic.true) != c(target.mbo,infill.mbo)]) %>%
    cluster::daisy(metric = metric) %>% as.matrix() %>% as.data.frame() %>% 
    dplyr::select(true) %>% dplyr::rename(distance = true)
  
  # pp.ic.all is a df with all the pp and their af values (af = acquisition function)
  # we merge pp.ic.without and dist.vec together, and see which instance causes
  # the biggest change in th proposed point
  pp.ic.all = rbind(pp.ic.without, pp.ic.true)
  result.1 = cbind(pp.ic.all, dist.vec)
  
  # 2.evaluate the prediction of the pp in the new model & train a CART to 
  #  distinguish infl & non infl instances
  
  # we separate pp and ic
  pp.true = pp.ic.true[,pars.mbo]
  ic.true = pp.ic.true[,infill.mbo]
  
  # 2.1 we compute the prediction of the af at pp.true with the sm stored in sm.without
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
                                   aei.use.nugget = res.mbo$control$infill.crit$params$aei.use.nugget, 
                                   se.threshold = 1e-06)
                      }),
                      "eqi" = sapply(seq_along(sm.without), function(x) {
                        predictEQI(points = pp.true, models = sm.without[[x]], designs = design.without[[x]], control = control.mbo,
                                   eqi.beta = res.mbo$control$infill.crit$params$eqi.beta,
                                   se.threshold = 1e-06)
                      }),
                      "adacb" = sapply(seq_along(sm.without), function(x) {
                        predictCB(points = pp.true, models = sm.without[[x]], designs = design.without[[x]], control = control.mbo,
                                  cb.lambda = components.without[[x]][["lambda"]])
                      })
  )
  names(ic.without) = 1:length(ic.without)
  assertNumeric(ic.without, any.missing = FALSE)
  
  # 2.2 compute the influence of instance i the absolute difference between infill.mbo & ic.without
  infl = switch(influence,
    "absolute" = abs(ic.true - ic.without),
    "normal" = (ic.true - ic.without) 
  )
  # NOTe: instance i has a positive influence on the prediction if the ic.true < ic.without (since internally
  #       all the infill criteria are minimized), therefore if ic.true - ic.without < 0

  # 2.3 train a ML model to distinguish infl and non infl instances
  #- for that we need to create a df with the removed instances of the design and their influence on ic.true
  infl.df = cbind(infl, new.design)
  task = makeRegrTask(data = infl.df, target = "infl")
  lrn =  makeLearner("regr.rpart", ...)
  mod = train(lrn, task)
  rpart.plot(mod$learner.model, roundint = FALSE)
  
  # 2.4. compute the distance between pp.true and the points in new design
  dist.to.nd = rbind(new.design, pp.true) %>%
    cluster::daisy(metric = metric) %>% as.matrix() %>% as.data.frame() %>% 
    dplyr::select(true) %>% dplyr::rename(distance = true)
  dist.df = cbind(rbind(new.design, pp.true), dist.to.nd)
  
  # 3.final results
  result = list(proposed.point.without = result.1, 
                influence.analysis = list(data.frame = infl.df, model = mod, distance.to.design = dist.df)
  )
}
