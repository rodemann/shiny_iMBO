# Test for PredictorAf
# source the data and MBO configs
source("fc_notizen/materials-design-playground-fc.R")

# run the MBO with all single infill and Savepts, the MBO are run with 1 iter
source("2_acquisition_function/2.2_Prediction_of_surrogate/FeatureEffectMBO-playground-fc.R")

# create the PredictorAf object (source the internal functions needed)
source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/PredictorAf-fc.R")

library(testthat)
library(mlrMBO)
# create the Predictor obejcts for the differrent MBOs
# Confidence Bound
p.cb = PredictorAf$new(model = res.mbo.cb$models[[1]],
                       data = opdf.cb[1:21,c(names(res.mbo.cb$x), res.mbo.cb$control$y.name)], 
                       res.mbo = res.mbo.cb,
                       iter = 1,
                       design = opdf.cb[1:20,c(names(res.mbo.cb$x), res.mbo.cb$control$y.name)] 
)
#iter 2
p2.cb = PredictorAf$new(model = res.mbo.cb$models[[2]],
                      data = opdf.cb[1:22,c(names(res.mbo.cb$x), res.mbo.cb$control$y.name)], 
                      res.mbo = res.mbo.cb,
                      iter = 2, 
                      design = opdf.cb[1:21,c(names(res.mbo.cb$x), res.mbo.cb$control$y.name)] 
)
# EI
p.ei = PredictorAf$new(model = res.mbo.ei$models[[1]],
                       data = opdf.ei[1:21,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)], 
                       res.mbo = res.mbo.ei,
                       iter = 1, 
                       design = opdf.ei[1:20,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)] 
)
#iter 2
p2.ei = PredictorAf$new(model = res.mbo.ei$models[[2]],
                       data = opdf.ei[1:22,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)], 
                       res.mbo = res.mbo.ei,
                       iter = 2,
                       design = opdf.ei[1:21,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)] 
)
# Mean Response
#iter 1
p.mr = PredictorAf$new(model = res.mbo.mr$models[[1]],
                       data = opdf.mr[1:21,c(names(res.mbo.mr$x), res.mbo.mr$control$y.name)], 
                       res.mbo = res.mbo.mr,
                       iter = 1, 
                       design = opdf.mr[1:20,c(names(res.mbo.mr$x), res.mbo.mr$control$y.name)] 
)
#iter 2
p2.mr = PredictorAf$new(model = res.mbo.mr$models[[2]], 
                        data = opdf.mr[1:22,c(names(res.mbo.mr$x), res.mbo.mr$control$y.name)], 
                        res.mbo = res.mbo.mr,
                        iter = 2,
                        design = opdf.mr[1:21,c(names(res.mbo.mr$x), res.mbo.mr$control$y.name)] 
)
# Standard Error
# iter 1
p.se = PredictorAf$new(model = res.mbo.se$models[[1]], 
                       data = opdf.se[1:21,c(names(res.mbo.se$x), res.mbo.se$control$y.name)], 
                       res.mbo = res.mbo.se,
                       iter = 1,
                       design = opdf.se[1:20,c(names(res.mbo.se$x), res.mbo.se$control$y.name)] 
)
# iter 2
p2.se = PredictorAf$new(model = res.mbo.se$models[[2]],
                       data = opdf.se[1:22,c(names(res.mbo.se$x), res.mbo.se$control$y.name)], 
                       res.mbo = res.mbo.se,
                       iter = 2,
                       design = opdf.se[1:21,c(names(res.mbo.se$x), res.mbo.se$control$y.name)] 
)

# Adaptive EI
# iter 1
p.aei = PredictorAf$new(model = res.mbo.aei$models[[1]], 
                       data = opdf.aei[1:21,c(names(res.mbo.aei$x), res.mbo.aei$control$y.name)], 
                       res.mbo = res.mbo.aei,
                       iter = 1, 
                       design = opdf.aei[1:20,c(names(res.mbo.aei$x), res.mbo.aei$control$y.name)] 
)
# iter 2
p2.aei = PredictorAf$new(model = res.mbo.aei$models[[2]], 
                        data = opdf.aei[1:22,c(names(res.mbo.aei$x), res.mbo.aei$control$y.name)], 
                        res.mbo = res.mbo.aei,
                        iter = 2,
                        design = opdf.aei[1:21,c(names(res.mbo.aei$x), res.mbo.aei$control$y.name)] 
)

# Expected Quantile Improvement
# iter 1
p.eqi = PredictorAf$new(model = res.mbo.eqi$models[[1]], 
                        data = opdf.eqi[1:21,c(names(res.mbo.eqi$x), res.mbo.eqi$control$y.name)], 
                        res.mbo = res.mbo.eqi,
                        iter = 1, 
                        design = opdf.eqi[1:20,c(names(res.mbo.eqi$x), res.mbo.eqi$control$y.name)] 
)
# iter 2
p2.eqi = PredictorAf$new(model = res.mbo.eqi$models[[2]],
                        data = opdf.eqi[1:22,c(names(res.mbo.eqi$x), res.mbo.eqi$control$y.name)],
                        res.mbo = res.mbo.eqi,
                        iter = 2, 
                        design = opdf.eqi[1:21,c(names(res.mbo.eqi$x), res.mbo.eqi$control$y.name)] 
)


# Adaptive CB
# Term condition iters, iter 1
p.adacb.iter = PredictorAf$new(model = res.mbo.adacb.iter$models[[1]], 
                        data = opdf.adacb.iter[1:21,c(names(res.mbo.adacb.iter$x), res.mbo.adacb.iter$control$y.name)], 
                        res.mbo = res.mbo.adacb.iter,
                        iter = 1, 
                        design = opdf.adacb.iter[1:20,c(names(res.mbo.adacb.iter$x), res.mbo.adacb.iter$control$y.name)]
)
# Term condition iters, iter 2
p2.adacb.iter = PredictorAf$new(model = res.mbo.adacb.iter$models[[2]], 
                          data = opdf.adacb.iter[1:22,c(names(res.mbo.adacb.iter$x), res.mbo.adacb.iter$control$y.name)], 
                          res.mbo = res.mbo.adacb.iter,
                          iter = 2,
                          design = opdf.adacb.iter[1:21,c(names(res.mbo.adacb.iter$x), res.mbo.adacb.iter$control$y.name)]
)
# Term condition time budget, iter 5


## Tests
test_that("Predictor works for Confidence Bound", {
  # iter 1
  expect_equivalent(
    p.cb$predict(pp.true.cb[1,1:6]),
    opdf.cb[21, res.mbo.cb$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.cb$predict(pp.true.cb[2,1:6]),
    opdf.cb[22, res.mbo.cb$control$infill.crit$id]
  )
})

test_that("Predictor works for Expected Improvement", {
  # iter 1
  expect_equivalent(
    p.ei$predict(pp.true.ei[1,1:6]),
    opdf.ei[21, res.mbo.ei$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.ei$predict(pp.true.ei[2,1:6]),
    opdf.ei[22, res.mbo.ei$control$infill.crit$id]
  )
  
})

test_that("Predictor works for Mean Response", {
  #iter 1
  expect_equivalent(
    p.mr$predict(pp.true.mr[1,1:6]),
    opdf.mr[21, res.mbo.mr$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.mr$predict(pp.true.mr[2,1:6]),
    opdf.mr[22, res.mbo.mr$control$infill.crit$id]
  )
})

test_that("Predictor works for Standard Error", {
  # iter 1
  expect_equivalent(
    p.se$predict(pp.true.se[1,1:6]),
    opdf.se[21, res.mbo.se$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.se$predict(pp.true.se[2,1:6]),
    opdf.se[22, res.mbo.se$control$infill.crit$id]
  )
})

test_that("Predictor works for Augmented EI", {
  # iter 1
  expect_equivalent(
    p.aei$predict(pp.true.aei[1,1:6]),
    opdf.aei[21, res.mbo.aei$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.aei$predict(pp.true.aei[2,1:6]),
    opdf.aei[22, res.mbo.aei$control$infill.crit$id]
  )
})

test_that("Predictor works for Expected Quantile Improvement", {
  # iter 1
  expect_equivalent(
    p.eqi$predict(pp.true.eqi[1,1:6]),
    opdf.eqi[21, res.mbo.eqi$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.eqi$predict(pp.true.eqi[2,1:6]),
    opdf.eqi[22, res.mbo.eqi$control$infill.crit$id]
  )
  
  
})

test_that("Predictor works for Adaptive Confindence Bound", {
  # Term.condition iters, iter 1
  expect_equivalent(
    p.adacb.iter$predict(pp.true.adacb.iter[1,1:6]),
    opdf.adacb.iter[21, res.mbo.adacb.iter$control$infill.crit$id]
  )
  # Term.condition iters, iter 2
  expect_equivalent(
    p2.adacb.iter$predict(pp.true.adacb.iter[2,1:6]),
    opdf.adacb.iter[22, res.mbo.adacb.iter$control$infill.crit$id]
  )
  
})

# Test that PredictorAf works also if iters is not specified
test_that("Predictor works for AdaCB over multiple iterations and Opt.methods", {
  
})


