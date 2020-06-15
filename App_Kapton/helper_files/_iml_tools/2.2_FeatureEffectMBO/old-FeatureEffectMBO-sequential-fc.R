# WARNING: if library dplyr library is used, errors might occur in Data-iml-molnar.R 
# because base::setdiff() and base::intersect() are used, which are masked from dplyr. Please make sure
# that dplyr is not attached. TO DO: base::setdiff() and base::intersect() where needed

#' @title FeatureEffectMBO computes and plots (individual) feature effects of surrogate models within 
#' a MBO process. 
#' 
#' @description In is an extension of [\code{\link{FeatureEffect}}]. For every iteration of the MBO
#' process the Effect of one (first order) or two (second order) is computed & the results a stored
#' in a R6 object. So far, works for Single.Objective and Single-Point Proposal

#' @param 
#' @param 
#' @param 
#' @param 
#' @return 

library(mlrMBO)
library(iml)
library(data.table)

# TO DO: 
# -if keep "seen" calculate target value
# -center.at proposed point in iter
# TO CHECK: 
# -effect = FeatureEffectMBO(res.mbo = res, feature = c("power","time"), method = "ale", interest = "acquisition")
#  Loading required namespace: yaImpute

OldFeatureEffectMBO = function(res.mbo,
                            # which model/predictor do you want to analyse? If "surrogate" the training instances used, 
                            # if "acquisition" the seen.points are used
                            interest = "surrogate",
                            # additional arguments to create a Predictor obejct
                            class = NULL,
                            predict.fun = NULL,#leave NULL because we give a model argument
                            type = NULL,
                            batch.size = 1000,
                            # argument of the FeatureEffect object
                            feature,
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
  assertCharacter(feature, max.len = 2, unique = TRUE)
  assertSubset(feature, pars.mbo)
  assertInteger(store.model.at, 
                lower = 1, 
                upper = iters.mbo + 1,
                any.missing = FALSE,
                min.len = iters.mbo,
                max.len = iters.mbo + 1,
                unique = TRUE
  )

  # if clauses to guarantee that it works with "seen.points" if stored, but also if not stored
  if (interest != "acquisition") seen.points = NULL
  if (interest == "acquisition" & !(res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts"))) {
    stop("Seen points have not been stored. Use other interest or run the MBO again with
         Savepts infill.opt")
  }
  if (interest == "acquisition" & res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts")) {
    # seen points is a list
    seen.points = res.mbo$seen.points
    assertList(seen.points, any.missing = FALSE, len = iters.mbo)
    # NOTE: works if stored seen points in res.mbo do not have target value!!!!
    # we also need the target value of the seen points
    seen.points = lapply(seq_along(seen.points), function(x) { 
      target = objfun.mbo(seen.points[[x]])
      df = cbind(seen.points[[x]], target)
      df = data.table::data.table(df)
      data.table::setnames(df, "target", y.name.mbo)
      df = as.data.frame(df)
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
  
  # in order to use parLapply and interest we need to use lists. Convert the designs
  # of each iter in a list 
  designs = list()
  for (i in 1:iters.mbo) { 
    designs[[i]] = opdf[1:(nrow(design.mbo) + i - 1), c(pars.mbo, y.name.mbo)]
  }

  # results, depends on the interest
  if (interest == "surrogate") {
    result <- lapply(
      seq_len(iters.mbo),
      function(x) {
        getFeatureEffectMBO(
          model.p = sm[[x]], data.p = designs[[x]], y.p = y.name.mbo,
          class.p = class, predict.fun.p = predict.fun, type.p = type, batch.size.p = batch.size,
          feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
        )
      }
    )
  }
  if (interest == "acquisition") {
    result <- lapply(
      seq_len(iters.mbo),
      function(x) {
        getFeatureEffectAfMBO(
          model.p = sm[[x]], data.p = seen.points[[x]], y.p = y.name.mbo, batch.size.p = batch.size,
          res.mbo.p = res.mbo, design.p = designs[[x]], iter.p = x,
          feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
        )
      }
    )
  }
  return(result)
}

############################ getFeatureEffect ##################################

getFeatureEffectMBO = function(model.p, data.p, y.p, class.p, predict.fun.p, type.p, batch.size.p, 
                            feature.fe, method.fe, grid.size.fe, center.at.fe
) {
  # 1. create a Predictor object
  pred = iml::Predictor$new(
    model = model.p, data = data.p, y = y.p, class = class.p, predict.fun = predict.fun.p,
    type = type.p, batch.size = batch.size.p
  )
  # 2. estimate the feature effect
  effect = iml::FeatureEffect$new(
    predictor = pred, feature = feature.fe, method = method.fe, grid.size = grid.size.fe,
    center.at = center.at.fe
  )
}

############################## getFeatureEffectAf ##############################
getFeatureEffectAfMBO = function(model.p, data.p, y.p, batch.size.p, res.mbo.p, design.p, iter.p,
                              feature.fe, method.fe, grid.size.fe, center.at.fe
) {
  # 1. create a Predictor object
  pred = PredictorAf$new(
    model = model.p, data = data.p, y = y.p, batch.size = batch.size.p,
    res.mbo = res.mbo.p, design = design.p, iter = iter.p
  )
  # 2. estimate the feature effect
  effect = iml::FeatureEffect$new(
    predictor = pred, feature = feature.fe, method = method.fe, grid.size = grid.size.fe,
    center.at = center.at.fe
  )
}