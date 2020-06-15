FeatureEffectMBOP = function(res.mbo,
                            # which model/predictor do you want to analyse? If "surrogate" the training instances used, 
                            # if "acquisition" the seen.points are used
                            interest = "surrogate",
                            # argument of the FeatureEffect object
                            feature,
                            method = "ale",
                            grid.size = 20,
                            center.at = NULL,
                            # additional arguments to create a Predictor obejct
                            batch.size = 1000,
                            parallel = FALSE
) {
  
  # extract infos from from res.mbo
  # data frames
  opdf =  as.data.frame(res.mbo$opt.path)
  design.mbo = res.mbo$final.opt.state$opt.problem$design
  # surrogate models, control object, par.set, und acq.fun
  sm = res.mbo$models
  control.mbo = res.mbo$control
  par.set.mbo = res.mbo$opt.path$par.set
  acq.fun = res.mbo$control$infill.crit$fun
  # Tags, Names, Integers
  infill.mbo = res.mbo$control$infill.crit$id
  y.name.mbo = res.mbo$control$y.name
  pars.mbo = names(res.mbo$x)
  # iters.mbo are the actually done "iterations" in the process, corresponds to dob in the opdf
  iters.mbo = opdf[nrow(opdf), "dob"]
  # stored are the stored models in the process, not every iter has a stored model, inly iters with
  # stored models can be analyzed
  stored = sort(as.integer(names(res.mbo$models)))
  
  # a & c
  assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
  assertChoice(interest, c("surrogate", "acquisition"))
  assertChoice(method, c("ale", "pdp", "ice", "pdp+ice"))
  if (is.character(feature)) {
    assertCharacter(feature, max.len = 2, unique = TRUE, any.missing = FALSE, all.missing = FALSE)
    assertSubset(feature, pars.mbo)
  } else {
    assertNumeric(feature, lower = 1, upper = length(pars.mbo), any.missing = FALSE, 
                  min.len = 1, max.len = 2)
  }
  
  
  assertInteger(stored,
                lower = 1,
                any.missing = FALSE,
                min.len = 1,
                max.len = iters.mbo + 1,
                sorted = TRUE,
                unique = TRUE #mbo automatically saves unique models, e.g. s.m.a = c(1,2,2,5)
                #models 1,2,5 are stored. it also rm NAs automatically, e.g. s.m.a = c(1,2,NA)
  )
  # stops if multi point proposal or multiobjective target fun
  if (control.mbo$propose.points > 1 | control.mbo$n.objectives > 1) 
    stop("FeatureEffectMBO not implemented for Multipoint-Proposal or Multiobjective function")
  # if clauses to guarantee that to stop if 
  if (length(stored) == 1 & max(stored) == iters.mbo + 1 & interest == "acquisition") {
    #1.case
    stop(paste0("Only the final model (default) has been stored.There are no seen points in iter ", stored,
                " because the MBO has terminated after iter ", stored - 1, ". Please use interest = surrogate if you want to analyse the final model or run the MBO with others store.model.at")
    )
  } 
  if (length(stored) > 1 & max(stored) == iters.mbo + 1 & interest == "acquisition") {
    #2.case
    # we need to remove the last model, since there are no seen.points
    stored = stored[-length(stored)]
  }
  # in all other cases the analysis can be conducted, note that if length(stored) == 1, only 1 iter
  # can be analysed, and eventually only the surrogate model (if stored = iters.mbo + 1)
  
  # if clauses to guarantee that it works with "seen.points" if stored, but also if not stored
  if (interest != "acquisition") seen.points = NULL
  if (interest == "acquisition" & !(res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts"))) {
    stop("Seen points have not been stored. Use interest = surrogate or run the MBO again with
         Savepts infill.opt")
  }
  if (interest == "acquisition" & res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts")) {
    # seen points is a list
    seen.points = res.mbo$seen.points
    assertList(seen.points, any.missing = FALSE, len = iters.mbo)
    # NOTE:- works if stored seen points in res.mbo do not have AF value
    #      - we care about the seen points, for which we stored the surrogate models, others are useless
    seen.points <- lapply(stored, function(x) {
      #assertDataFrame(seen.points[[x]], all.missing = FALSE, any.missing = FALSE)
      ic = acq.fun(
        points = seen.points[[x]],
        models = list(sm[[as.character(x)]]),
        control = control.mbo,
        par.set = par.set.mbo,
        designs = list(opdf[1:(nrow(design.mbo) + x - 1), c(pars.mbo, y.name.mbo)]),
        iter = x,
        progress = getProgressAdaCB(res.mbo = res.mbo, iter = x),
        attributes = FALSE
      )
      df = cbind(seen.points[[x]], ic)
      df = data.table::data.table(df)
      data.table::setnames(df, "ic", infill.mbo)
      df = as.data.frame(df)
    })
    names(seen.points) = stored
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
  designs = lapply(stored, function(x){
    opdf[1:(nrow(design.mbo) + x - 1), c(pars.mbo, y.name.mbo)]
  })
  names(designs) = stored
  
  if (parallel) {
    # calulate the number of cores & initiate cluster
    no.cores = parallel::detectCores() - 1
    cl = parallel::makeCluster(no.cores, type = "FORK")
    
    if (interest == "surrogate") {
      result = parallel::parLapply(
        cl,
        stored,
        function(x) {
          getFeatureEffectMBO(
            model.p = sm[[as.character(x)]], data.p = designs[[as.character(x)]], y.p = y.name.mbo,
            class.p = NULL, predict.fun.p = NULL, type.p = NULL, batch.size.p = batch.size,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      # close the cluster
      parallel::stopCluster(cl)
    }
    
    if (interest == "acquisition") {
      result = parallel::parLapply(
        cl,
        stored,
        function(x) {
          getFeatureEffectAfMBO(
            model.p = sm[[as.character(x)]], data.p = seen.points[[as.character(x)]], y.p = infill.mbo, batch.size.p = batch.size,
            res.mbo.p = res.mbo, design.p = designs[[as.character(x)]], iter.p = x,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      # close the cluster
      parallel::stopCluster(cl)
    }
    
  } else {
    # results, depends on the interest
    if (interest == "surrogate") {
      result <- lapply(
        stored,
        function(x) {
          getFeatureEffectMBO(
            model.p = sm[[as.character(x)]], data.p = designs[[as.character(x)]], y.p = y.name.mbo,
            class.p = NULL, predict.fun.p = NULL, type.p = NULL, batch.size.p = batch.size,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      names(result) = stored
    }
    if (interest == "acquisition") {
      result <- lapply(
        stored,
        function(x) {
          getFeatureEffectAfMBO(
            model.p = sm[[as.character(x)]], data.p = seen.points[[as.character(x)]], y.p = infill.mbo, batch.size.p = batch.size,
            res.mbo.p = res.mbo, design.p = designs[[as.character(x)]], iter.p = x,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      names(result) = stored
    }
  }
  return(result)
}


eff = FeatureEffectMBOP(res.mbo = res, interest = "acquisition", feature = "time", parallel = FALSE)

