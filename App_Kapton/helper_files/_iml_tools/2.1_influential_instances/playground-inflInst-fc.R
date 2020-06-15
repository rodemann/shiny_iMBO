# material-design
source("fc_notizen/materials-design-playground-fc.R")
# or kapton
source("fc_notizen/kapton-playground-fc.R")

#1
ctrl.ei = setMBOControlInfill(ctrl,
                              opt = "focussearch",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 1000,
                              crit = makeMBOInfillCritEI()
)
set.seed(1)
res.mbo.ei = mbo(objfun,
                 design = initial.data,
                 control = ctrl.ei,
                 show.info = TRUE
)
opdf.ei = as.data.frame(res.mbo.ei$opt.path)
pp.true.ei = opdf.ei[which(opdf.ei$dob == 2), names(res.mbo.ei$x)]
#2
ctrl.mr = setMBOControlInfill(ctrl,
                              opt = "focussearch",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 1000,
                              crit = makeMBOInfillCritMeanResponse()
)
set.seed(1)
res.mbo.mr = mbo(objfun,
                 design = initial.data,
                 control = ctrl.mr,
                 show.info = TRUE
)
opdf.mr = as.data.frame(res.mbo.mr$opt.path)
#3
ctrl.se = setMBOControlInfill(ctrl,
                              opt = "focussearch",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 1000,
                              crit = makeMBOInfillCritStandardError()
)
set.seed(1)
res.mbo.se = mbo(objfun,
                 design = initial.data,
                 control = ctrl.se,
                 show.info = TRUE
)
opdf.se = as.data.frame(res.mbo.se$opt.path)
pp.true.se = opdf.se[which(opdf.se$dob == 2), names(res.mbo.se$x)]
#4 Confidence bound with lambda automatically chosen, lambda = 1 numeric ps, lamba = 2 otehrwise
ctrl.cb = setMBOControlInfill(ctrl,
                              opt = "focussearch",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 1000,
                              crit = makeMBOInfillCritCB(cb.lambda = 0.6)
)
set.seed(1)
res.mbo.cb = mbo(objfun,
                 design = initial.data,
                 control = ctrl.cb,
                 show.info = TRUE
)
opdf.cb = as.data.frame(res.mbo.cb$opt.path)
pp.true.cb = opdf.cb[which(opdf.cb$dob == 4), names(res.mbo.cb$x)]
#5
ctrl.aei = setMBOControlInfill(ctrl,
                               opt = "focussearch",
                               opt.focussearch.maxit = 20,
                               opt.focussearch.points = 1000,
                               crit = makeMBOInfillCritAEI()
)
set.seed(1)
res.mbo.aei = mbo(objfun,
                  design = initial.data,
                  control = ctrl.aei,
                  show.info = TRUE
)
opdf.aei = as.data.frame(res.mbo.aei$opt.path)
pp.true.aei = opdf.aei[which(opdf.aei$dob == 2), names(res.mbo.aei$x)]

# #6
ctrl.eqi = setMBOControlInfill(ctrl,
                               opt = "focussearch",
                               opt.focussearch.maxit = 20,
                               opt.focussearch.points = 1000,
                               crit = makeMBOInfillCritEQI(se.threshold = 1e-07, eqi.beta = 0.6)
)
set.seed(1)
res.mbo.eqi = mbo(objfun,
                  design = initial.data,
                  control = ctrl.eqi,
                  show.info = TRUE
)
opdf.eqi = as.data.frame(res.mbo.eqi$opt.path)
pp.true.eqi = opdf.eqi[which(opdf.eqi$dob == 2), names(res.mbo.eqi$x)]
#7 AdaCB
#ctrl.adacb = setMBOControlTermination(ctrl, time.budget = 100, use.for.adaptive.infill = "time.budget")
ctrl.adacb = setMBOControlInfill(ctrl,
                                 opt = "focussearch",
                                 opt.focussearch.maxit = 20,
                                 opt.focussearch.points = 1000,
                                 crit = makeMBOInfillCritAdaCB()
)
set.seed(1)
res.mbo.adacb = mbo(objfun,
                    design = initial.data,
                    control = ctrl.adacb,
                    show.info = TRUE
)
opdf.adacb = as.data.frame(res.mbo.adacb$opt.path)
pp.true.adacb = opdf.adacb[which(opdf.adacb$dob == 2), names(res.mbo.adacb$x)]

#8 Direct indicator-based with lambda=1 is for Multiobjective

debug(mbo)
makeMBOInfillCritAdaCB()