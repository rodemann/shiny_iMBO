# Some Tests
#Test Mean
a = predictMEAN(points = pp.true, models = result.without$sm.without[[1]], control = res.mbo.mr$control)
b = predictMEAN(points = pp.true, models = result.without$sm.without[[5]], control = res.mbo.mr$control)
all.equal(b,pred.without[5])

#Test SE
c = predictSE(points = pp.true.se, models= result.without$sm.without[[6]], control = res.mbo.se$control)
all.equal(c, pred.without[6])

# Test EI 
designs = opdf.ei[c(1:8,10,11), c(names(res.mbo.ei$x), ctrl.ei$y.name)]
d = predictEI(points = pp.true.ei, models = result.without$sm.without[[9]], control = res.mbo.ei$control, designs = designs)
all.equal(d, pred.without[9])

# Test CB
e = predictCB(points = pp.true.cb, models = result.without$sm.without[[4]], control = res.mbo.cb$control,
              cb.lambda = res.mbo.cb$contr$infill.crit$params$cb.lambda)
all.equal(e, pred.without[4])

#Test AEI
designs = opdf.aei[c(1:9,11), c(names(res.mbo.aei$x), ctrl.aei$y.name)]
e = predictAEI(points = pp.true.aei, models = result.without$sm.without[[10]], control = res.mbo.aei$control,
               par.set = res.mbo.aei$opt.path$par.set, designs = designs)

all.equal(e, pred.without[10])

#Test EQI
designs = opdf.eqi[c(1,2,4:11), c(names(res.mbo.eqi$x), ctrl.eqi$y.name)]
f = predictEQI(points = pp.true.eqi, models = result.without$sm.without[[3]], control = res.mbo.eqi$control,
               designs = designs, eqi.beta = res.mbo.eqi$control$infill.crit$params$eqi.beta)
all.equal(f, pred.without[3])


