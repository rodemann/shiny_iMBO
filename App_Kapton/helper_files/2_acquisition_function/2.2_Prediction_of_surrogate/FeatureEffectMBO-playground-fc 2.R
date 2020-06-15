# IML PLAYGROUND 
#source modified function from mlrMBO
source("_Explore_Exploit_Measures/proposePointsByInfillOptimization-jr.R")
source("_Explore_Exploit_Measures/makeMBOResult.OptState-jr.R")
source("_Explore_Exploit_Measures/getSupportedInfillOptFunctions-jr.R")
source("_Explore_Exploit_Measures/proposePointsByInfillOptimization-jr.R")
source("_Explore_Exploit_Measures/getInfillOptFunction-jr.R")
source("_Explore_Exploit_Measures/checkStuff-jr.R")

# source new infill optimization functions "...Savepts"
source("_Explore_Exploit_Measures/infillOptFocusSavepts-jr.R")
source("_Explore_Exploit_Measures/infillOptEASavepts-jr.R")
source("_Explore_Exploit_Measures/infillOptCMAESSavepts-jr.R")

# material-design
# source("fc_notizen/materials-design-playground-fc.R")
# # or kapton
# source("fc_notizen/kapton-playground-fc.R")

#1
ctrl.ei = setMBOControlInfill(ctrl,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 1000,
                              crit = makeMBOInfillCritEI()
)
set.seed(1)
res.mbo.ei = mbo(objfun,
                 design = initial.data,
                 control = ctrl.ei,
                 show.info = FALSE
)
opdf.ei = as.data.frame(res.mbo.ei$opt.path)
pp.true.ei = opdf.ei[which(opdf.ei$dob > 0), c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)]
#2
ctrl.mr = setMBOControlInfill(ctrl,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 1000,
                              crit = makeMBOInfillCritMeanResponse()
)
set.seed(1)
res.mbo.mr = mbo(objfun,
                 design = initial.data,
                 control = ctrl.mr,
                 show.info = FALSE
)
opdf.mr = as.data.frame(res.mbo.mr$opt.path)
pp.true.mr = opdf.mr[which(opdf.ei$dob > 0), c(names(res.mbo.ei$x), res.mbo.mr$control$y.name)]
#3
ctrl.se = setMBOControlInfill(ctrl,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 1000,
                              crit = makeMBOInfillCritStandardError()
)
set.seed(1)
res.mbo.se = mbo(objfun,
                 design = initial.data,
                 control = ctrl.se,
                 show.info = FALSE
)
opdf.se = as.data.frame(res.mbo.se$opt.path)
pp.true.se = opdf.se[which(opdf.se$dob > 0), c(names(res.mbo.se$x), res.mbo.se$control$y.name)]
#4 Confidence bound with lambda automatically chosen, lambda = 1 numeric ps, lamba = 2 otehrwise
ctrl.cb = setMBOControlInfill(ctrl,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 1000,
                              crit = makeMBOInfillCritCB()
)
set.seed(1)
res.mbo.cb = mbo(objfun,
                 design = initial.data,
                 control = ctrl.cb,
                 show.info = FALSE
)
opdf.cb = as.data.frame(res.mbo.cb$opt.path)
pp.true.cb = opdf.cb[which(opdf.cb$dob > 0), c(names(res.mbo.cb$x), res.mbo.cb$control$y.name)]
#5
ctrl.aei = setMBOControlInfill(ctrl,
                               opt = "focussearchSavepts",
                               opt.focussearch.maxit = 20,
                               opt.focussearch.points = 1000,
                               crit = makeMBOInfillCritAEI()
)
set.seed(1)
res.mbo.aei = mbo(objfun,
                  design = initial.data,
                  control = ctrl.aei,
                  show.info = FALSE
)
opdf.aei = as.data.frame(res.mbo.aei$opt.path)
pp.true.aei = opdf.aei[which(opdf.aei$dob > 0), c(names(res.mbo.aei$x), res.mbo.aei$control$y.name)]

# #6
ctrl.eqi = setMBOControlInfill(ctrl,
                               opt = "focussearchSavepts",
                               opt.focussearch.maxit = 20,
                               opt.focussearch.points = 1000,
                               crit = makeMBOInfillCritEQI(se.threshold = 1e-07, eqi.beta = 0.6)
)
set.seed(1)
res.mbo.eqi = mbo(objfun,
                  design = initial.data,
                  control = ctrl.eqi,
                  show.info = FALSE
)
opdf.eqi = as.data.frame(res.mbo.eqi$opt.path)
pp.true.eqi = opdf.eqi[which(opdf.eqi$dob > 0), c(names(res.mbo.eqi$x), res.mbo.eqi$control$y.name)]
#7 AdaCB
# Iter termination
ctrl.adacb.iter = setMBOControlTermination(ctrl, iters = 2)
ctrl.adacb.iter = setMBOControlInfill(ctrl.adacb.iter,
                                 opt = "focussearchSavepts",
                                 opt.focussearch.maxit = 20,
                                 opt.focussearch.points = 1000,
                                 crit = makeMBOInfillCritAdaCB()
)
set.seed(1)
res.mbo.adacb.iter = mbo(objfun,
                    design = initial.data,
                    control = ctrl.adacb.iter,
                    show.info = FALSE
)
opdf.adacb.iter = as.data.frame(res.mbo.adacb.iter$opt.path)
pp.true.adacb.iter = opdf.adacb.iter[which(opdf.adacb.iter$dob > 0), c(names(res.mbo.adacb.iter$x), res.mbo.adacb.iter$control$y.name)]

# Time Budget
ctrl.adacb.tb = setMBOControlTermination(ctrl, time.budget = 15)
ctrl.adacb.tb = setMBOControlInfill(ctrl.adacb.tb,
                                      opt = "focussearchSavepts",
                                      opt.focussearch.maxit = 20,
                                      opt.focussearch.points = 1000,
                                      crit = makeMBOInfillCritAdaCB()
)
set.seed(1)
res.mbo.adacb.tb = mbo(objfun,
                         design = initial.data,
                         control = ctrl.adacb.tb,
                         show.info = FALSE
)
opdf.adacb.tb = as.data.frame(res.mbo.adacb.tb$opt.path)
pp.true.adacb.tb = opdf.adacb.tb[which(opdf.adacb.tb$dob > 0), c(names(res.mbo.adacb.tb$x), res.mbo.adacb.tb$control$y.name)]


#8 Direct indicator-based with lambda=1 is for Multiobjective