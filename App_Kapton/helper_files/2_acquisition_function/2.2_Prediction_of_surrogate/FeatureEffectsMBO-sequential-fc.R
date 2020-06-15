# FeatureEffects computes feature effects for multiple features at once. Only first order effects
# are supported

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

library(mlrMBO)
library(iml)
library(ggplot2)
library(patchwork)

FeatureEffectsMBO = function(res.mbo,
                             # which model/predictor do you want to analyse? If "surrogate" the training instances used, 
                             # if "acquisition" the seen.points are used
                            interest = "surrogate",
                            # additional arguments of the Predictor obejct
                            class = NULL,
                            predict.fun = NULL,
                            type = NULL,
                            batch.size = 1000,
                            # argument of the FeatureEffect object
                            features = NULL,
                            method = "ale",
                            grid.size = 20,
                            center.at = NULL
) {
  
  # extract other informations
  opdf =  as.data.frame(res.mbo$opt.path)
  sm = res.mbo$models
  infill.mbo = res.mbo$control$infill.crit$id
  y.name.mbo = res.mbo$control$y.name
  iters.mbo = res.mbo$control$iters
  store.model.at = res.mbo$control$store.model.at
  pars.mbo = names(res.mbo$x)
  design.mbo = res.mbo$final.opt.state$opt.problem$design
  # pp.df = opdf[which(opdf$dob > 0), pars.mbo]
  # dimnames(pp.df)[[1]] = 1:nrow(pp.df)
  control.mbo = res.mbo$control
  objfun.mbo = res.mbo$final.opt.state$opt.problem$fun
  
  # a & c
  assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
  assertChoice(interest, c("surrogate", "acquisition"))
  assertChoice(method, c("ale", "pdp", "ice", "pdp+ice"))
  assertSubset(features, pars.mbo)
  assertInteger(store.model.at, 
                lower = 1, 
                upper = iters.mbo + 1,
                any.missing = FALSE,
                min.len = iters.mbo,
                max.len = iters.mbo + 1,
                unique = TRUE
  )

  # if clauses to guarantee that it works with "seen.points" if stored, but also if not stored
  if (interest != "acquisition") {seen.points = NULL}
  if (interest == "acquisition" & !(res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts"))) {
    stop("Seen points have not been stored. Use other interest or run the MBO again with
         Savepts infill.opt")
  }
  if (interest == "acquisition" & res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts")) {
    # seen points is a list
    seen.points <- res.mbo$seen.points
    assertList(seen.points, any.missing = FALSE, len = iters.mbo)
    # we also need the target value of the seen points
    seen.points = lapply(seq_along(seen.points), function(x) { 
      target = objfun.mbo(seen.points[[x]])
      df = cbind(seen.points[[x]], target)
      # this works only for Kapton data, a specific solution in needed!
      # convert chr columns to factor
      if (any(names(df) == "gas")) {
        df$gas <- as.factor(df$gas)
      }
      data.table::setnames(df, "target", y.name.mbo)
    })
  }

  
  # # If "seen" but too many obs either quit, unchanged or change method is possible 
  # if (interest == "acquisition") {
  #   seen.point.iter = sapply(seen.points, function(x) nrow(x))
  #   threshold = 15000
  #   if (any(seen.point.iter > threshold)) {
  #     quit = askYesNo("The seen points in at least one iteration are more than 15000. \n Calculation might take a while and ICE or PDP+ICE plots might be chaotic.\n Do you want to quit?")
  #     if (quit == TRUE) {
  #       stop("Too many seen points might generate chaotic plots. You quit. Blame yourself. Again.")
  #     } else{
  #       if (method %in% c("ice", "pdp+ice")) {
  #         change.method = askYesNo("Do you want to switch to PDP?")
  #         if (change.method == TRUE) {
  #           method = "pdp"
  #         } else{
  #           method = method
  #         }
  #       }
  #     }
  #   }
  # }
  
  # in order to use parLapply and interest we need to use lists
  # seen.points is already a list 
  designs = list()
  for (i in 1:iters.mbo) { 
    designs[[i]] = opdf[1:(nrow(design.mbo) + i - 1), c(pars.mbo, y.name.mbo)]
  }
  
  # results, depends on the interest
  if (interest == "surrogate") {
    result = lapply(
      seq_len(iters.mbo),
      function(x) {
        getFeatureEffectsMBO(
          model.p = sm[[x]], data.p = designs[[x]], y.p = y.name.mbo,
          class.p = class, predict.fun.p = predict.fun, type.p = type, batch.size.p = batch.size,
          features.fe = features, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
        )
      }
    ) 
  }
  if (interest == "acquisition") {
    result = lapply(
      seq_len(iters.mbo),
      function(x) {
        getFeatureEffectsAfMBO(
          model.p = sm[[x]], data.p = seen.points[[x]], y.p = y.name.mbo, batch.size.p = batch.size,
          res.mbo.p = res.mbo, design.p = designs[[x]], iter.p = x, attributes.p = attributes,
          features.fe = features, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
        )
      }
    )  
  }
  
  return(result)
}

########################## getFeatureEffectsMBO ################################

getFeatureEffectsMBO = function(model.p, data.p, y.p, class.p, predict.fun.p, type.p, batch.size.p,
                                features.fe, method.fe, grid.size.fe, center.at.fe
) {
  # 1. create a Predictor object
  pred = iml::Predictor$new(
    model = model.p, data = data.p, y = y.p, class = class.p, predict.fun = predict.fun.p,
    type = type.p, batch.size = batch.size.p 
  )
  # 2. estimate the feature effect
  effect = iml::FeatureEffects$new(
    predictor = pred, features = features.fe, method = method.fe, grid.size = grid.size.fe,
    center.at = center.at.fe
  )
}


###################### getFetureEffectsAfMBO ###################################
getFeatureEffectsAfMBO = function(model.p, data.p, y.p, batch.size.p, res.mbo.p, design.p, iter.p, attributes.p,
                                  features.fe, method.fe, grid.size.fe, center.at.fe
) {
  # 1. create a Predictor object
  pred = PredictorAf$new(
    model = model.p, data = data.p, y = y.p, batch.size = batch.size.p,
    res.mbo = res.mbo.p, design = design.p, iter = iter.p, attributes = attributes.p
  )
  # 2. estimate the feature effect
  effect = iml::FeatureEffect$new(
    predictor = pred, features = features.fe, method = method.fe, grid.size = grid.size.fe,
    center.at = center.at.fe
  )
}

