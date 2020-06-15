source("fc_notizen/materials-design-playground-fc.R")

effectOnAquisition4P <- function(obj.fun.mbo,
                                 design.mbo,
                                 ctrl.mbo,
                                 par.set.mbo,
                                 sm.learner = makeLearner("regr.randomForest", predict.type = "se"),
                                 feature,
                                 iterations,
                                 grid.size,
                                 seed) {
  # assertions and checks:
  # -assert that feature is character and 1 value
  # -assert that gridSize is numeric or integer, 1 value. The function should also work for categorical feature
  # -assert iter is a numeric value
  # -assert design.mbo is a df

  # divide the domain of the feature in eqaul parts (grid.size)
  expanded.feature <- seq(
    par.set.mbo[["pars"]][[feature]][["lower"]],
    par.set.mbo[["pars"]][[feature]][["upper"]],
    length.out = grid.size
  )
  # # detecting and setting the cores to parallalize calculations
  no.cores <- detectCores() - 1
  registerDoParallel(no.cores)
  
  # store the results in a list 
  results <- list()
  
  for (i in seq_len(iterations)) {
    ctrl.mbo <- setMBOControlTermination(ctrl.mbo, iters = 1)
    
    set.seed(seed)
    mbo.real <- mlrMBO::mbo(
      fun = obj.fun.mbo,
      design = design.mbo,
      control = ctrl.mbo,
      learner = sm.learner,
      show.info = FALSE
    )
    # store the opt. path in a df
    opt.path.real <- mbo.real$opt.path %>% as.data.frame() %>%
      dplyr::select(
        c(
        names(par.set.mbo[["pars"]]), mbo.real[["control"]][["y.name"]],
        mbo.real[["control"]][["infill.crit"]][["id"]],
        "dob"
        )
      )
    
    results.hypo <- foreach(
      j = seq_len(grid.size),
      .export = names(.GlobalEnv),
      .packages = c("mlrMBO", "smoof", "mlr")
    ) %dopar%  {
      # -we replace the column of the interesting feature with the i-th element
      #  of 'expandedFeature'
      new.design <- design.mbo
      new.design[, feature] <- expanded.feature[j]
      
      # -we run the same MBO process for the above design and store the results
      set.seed(seed)
      mbo.hypo <- mlrMBO::mbo(
        fun = obj.fun.mbo,
        design = new.design,
        control = ctrl.mbo,
        learner = sm.learner,
        show.info = FALSE
      )
      
      opt.path.hypo <- mbo.hypo$opt.path %>% as.data.frame() %>%
        dplyr::select(
          c(
            names(par.set.mbo[["pars"]]), mbo.hypo[["control"]][["y.name"]],
            mbo.hypo[["control"]][["infill.crit"]][["id"]],
            "dob"
          )
        )
    }
    
    results.iter <- list(opt.path.real, results.hypo)
    results[[i]] <- results.iter
    
    # update design (with target value included) before starting iter i + 1
    # alternatively without target: design.mbo <- opt.path.real[, names(par.set.mbo[["pars"]])]
    design.mbo <- mbo.real[["opt.path"]][["env"]][["path"]]
  }
  
  # clean the cluster
  stopImplicitCluster()
  #remove this line once the code works
  results <<-  results
}


# # select design for the next iters
# new.design <- opt.path.real[, names(par.set.mbo[["pars"]])]
# 
# # proposed points in the iteration
# pp <- opt.path.real[
#   which(opt.path.real$dob > 0),
#   names(par.set.mbo[["pars"]])
#   ]
# 
# # useful information of the mbo opt path
# info.real <- opt.path.real[
#   which(opt.path.real$dob > 0),
#   c(
#     names(ps[["pars"]]),
#     mbo.real[["control"]][["y.name"]],
#     mbo.real[["control"]][["infill.crit"]][["id"]],
#     "dob"
#   )
#   ]
# Test
debug(effectOnAquisition4P)
effectOnAquisition4P(
  obj.fun.mbo = objfun,
  design.mbo = initial.data,
  ctrl.mbo = ctrl,
  par.set.mbo = ps,
  sm.learner = makeLearner("regr.randomForest", predict.type = "se"),
  feature = "f",
  grid.size = 3,
  iterations = 2,
  seed = 11
)

undebug(effectOnAquisition4P)
