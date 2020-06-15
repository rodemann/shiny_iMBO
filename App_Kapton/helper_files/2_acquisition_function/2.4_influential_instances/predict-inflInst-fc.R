# this file contains the predict functions for evaluating the prediction of the 
# proposed point in the original MBO with the surrogate model of the MBO process
# run without instance i. For that we took a slighlty modified version of the respective
# makeMBOInfillCrit functions.
# The idea is to evaulate the prediction of pp.true in iter i with every sm in 
# sm.without, which was fitted without instance i. The more the prediction differs
# from the original one, the more the corresponding instance has an influence on the 
# proposed point. Though, only listing the influence of instances is not very helpful.
# The idea is to train then an interpretable ML model to distinguish between infl and
# non infl instances. Intuitively, the more one instance is near to the true proposed
# point, the more influence it probably has on the prediction

# TO DO: 
# -actually per Default attributes = FALSE --> does NULL affect the results?
# Mean Response
# -assertions and force can actually be deleted as they are already checked with makeMBOInfillCrit
predictMEAN = function(points, models, control, par.set = NULL, designs = NULL,
                       iter = NULL, progress = NULL, attributes = NULL) {
  ifelse(control$minimize, 1, -1) * predict(models, newdata = points)$data$response
}


# Standard Error
# why -predict?????? why does control do not influence the function? ifelse(control$minimize, 1, -1)
predictSE = function(points, models, control = NULL, par.set = NULL, designs = NULL,
                     iter = NULL, progress = NULL, attributes = NULL) {
  -predict(models, newdata = points)$data$se
}

# Expected Improvement
predictEI = function(points, models, control, par.set = NULL, designs,
                     iter = NULL, progress = NULL, attributes = NULL, se.threshold = 1e-6) {
  # assertNumber(se.threshold, lower = 1e-20)
  # force(se.threshold) # do we need force?
  
  model = models
  design = designs
  maximize.mult = if (control$minimize) 1 else -1
  assertString(control$y.name)
  y = maximize.mult * design[, control$y.name]
  assertNumeric(y, any.missing = FALSE)
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  y.min = min(y)
  d = y.min - p.mu
  xcr = d / p.se
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  ei = d * xcr.prob + p.se * xcr.dens
  res = ifelse(p.se < se.threshold, 0, -ei)
}

# Confidence Bound
predictCB = function(points, models, control, par.set = NULL, designs = NULL,
                     iter = NULL, progress = NULL, attributes = NULL, cb.lambda) {
  # assertNumber(cb.lambda, lower = 0, null.ok = TRUE)
  # force(cb.lambda)
  
  model = models
  maximize.mult = if (control$minimize) 1 else -1
  p = predict(model, newdata = points)$data
  #FIXME: removed cb.inflate.se for now (see issue #309)
  # if (cb.inflate.se) {
  #   r.response = diff(range(p$response))
  #   r.se = diff(range(p$se))
  #   tol = .Machine$double.eps^0.5
  #   if (r.response < tol)
  #     return(r.se)
  #   if (r.se < tol)
  #     return(r.response)
  #   inflate = r.response / r.se
  # } else {
  inflate = 1
  #}
  res = maximize.mult * p$response - inflate * cb.lambda * p$se
}

a = predictCB(points = pp.true.adacb, models = res.mbo.adacb$models$`2`, control = res.mbo.adacb$control,
              cb.lambda = opdf.adacb[12,"lambda"])
# AdaCB
# for AdaCB we use predictCB because as we only do 1 iteration in mbo.hypo there is no
# adaptation (we don't need the progress) & we set cb.lambda as estimated from the interested
# iteration in the corresponding mbo.hypo. This can be easily found in the opt.path, see components
# of AdaCB bzw. Attributes. Original AdaCB also uses makeMBOInfillCritCB()$fun but updates the
# cb.lambda before calculating


# AEI 
# -FIXME: makeMBOInfillCritAEI(se.threshold = 1e-7) does not store the threshold
# -in order to use predict AEI we need to call getEffectiveBestPoint, which is internal function of mlrMBO
# -also we need getMinIndex from the Package BBmisc and getLearnerModel & estimateResidualVariance from mlr Package

predictAEI = function(points, models, control, par.set, designs, 
                      iter = NULL, progress = NULL, attributes = NULL, 
                      aei.use.nugget = FALSE, se.threshold = 1e-6) {
  # assertFlag(aei.use.nugget)
  # assertNumber(se.threshold, lower = 1e-20)
  # force(aei.use.nugget)
  # force(se.threshold)
  
  model = models
  design = designs
  maximize.mult = if (control$minimize) 1 else -1
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  
  ebs = getEffectiveBestPoint(design = design, model = model, par.set = par.set, control = control)
  # calculate EI with plugin, plugin val is mean response at ebs solution
  d = ebs$mu - p.mu
  xcr = d / p.se
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  
  # noise estimation
  if (aei.use.nugget) {
    pure.noise.var = mlr::getLearnerModel(model, more.unwrap = TRUE)@covariance@nugget
  } else {
    pure.noise.var = mlr::estimateResidualVariance(model, data = design, target = control$y.name)
    #argument task must not be supllied because data and target are supplied
  }
  
  tau = sqrt(pure.noise.var)
  res = (-1) * ifelse(p.se < se.threshold, 0,
                      (d * xcr.prob + p.se * xcr.dens) * (1 - tau / sqrt(tau^2 + p.se^2)))
}

getEffectiveBestPoint = function(design, model, par.set, control) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  preds = predict(model, newdata = design)
  
  mu = preds$data$response
  se = preds$data$se
  
  # FIXME: add this constant to control?
  const = 1
  
  # minimize mu (if minimization of objective), large se is always penalized
  v = (maximize.mult * mu) + const * se
  j = BBmisc::getMinIndex(v)
  
  return(list(index = j, des = design[j,, drop = FALSE], mu = mu[[j]], se = se[[j]], val = v[[j]]))
}


# EQI
# what is the FIXME ??
predictEQI = function(points, models, control, par.set = NULL, designs, 
                      iter = NULL, progress = NULL, attributes = NULL,
                      eqi.beta = 0.75, se.threshold = 1e-6) {
  # assertNumber(eqi.beta, lower = 0.5, upper = 1)
  # assertNumber(se.threshold, lower = 1e-20)
  # force(eqi.beta)
  # force(se.threshold)
  `%nin%` = Negate(`%in%`) # eventually use %nin% from Package BBmisc
  
  model = models
  design = designs
  maximize.mult = if (control$minimize) 1 else -1
  # compute q.min
  design_x = design[, (colnames(design) %nin% control$y.name), drop = FALSE]
  p.current.model = predict(object = model, newdata = design_x)$data
  q.min = min(maximize.mult * p.current.model$response + qnorm(eqi.beta) * p.current.model$se)
  
  p = predict(object = model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  
  pure.noise.var = if (inherits(model$learner, "regr.km")) {
    pure.noise.var = model$learner.model@covariance@nugget
    #FIXME: What if kriging is wrapped?
  } else {
    mlr::estimateResidualVariance(model, data = design, target = control$y.name)
  }
  tau = sqrt(pure.noise.var)
  
  mq = p.mu + qnorm(eqi.beta) * sqrt((tau * p.se^2) / (tau + p.se^2))
  sq = p.se^2 / sqrt(pure.noise.var + p.se^2)
  d = q.min - mq
  xcr = d / sq
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  
  res = -1 * ifelse(p.se < se.threshold, 0, (sq * (xcr * xcr.prob + xcr.dens)))
}


