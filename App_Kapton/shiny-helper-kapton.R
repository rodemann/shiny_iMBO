library(mlrMBO)
library(iml)
library(pdp)
library(foreach)
library(doParallel)
library(randomForest)
library(rgenoud)


#source("_Explore_Exploit_Measures/xplxplMBO-jr.R")
source("Tools_2_source/xplxplMBO.R")
source("helper_files/read-data-kapton-jr.R")

#global settings
iters = 2
grid.size = 20
n.cluster = min(20,iters)
set.seed(2020)

# settings from larsko_code: 

model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data, target = "ratio"))


fun = function(x) {
  df = as.data.frame(x)
  df$gas = factor(df$gas, levels = levels(data$gas))
  return(getPredictionResponse(predict(model, newdata = df)))
}

ps = makeParamSet(
  makeIntegerParam("power", lower = 10, upper = 5555),
  makeIntegerParam("time", lower = 500, upper = 20210),
  makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
  makeIntegerParam("pressure", lower = 0, upper = 1000)
)

objfun = makeSingleObjectiveFunction(
  name = "Kapton",
  fn = fun,
  par.set = ps,
  has.simple.signature = FALSE,
  minimize = FALSE
)

# sample 9 points for the initial surrogate model, stratified across gases
samples.argon = sample(rownames(data[data$gas == "Argon", ]), 3)
samples.nitro = sample(rownames(data[data$gas == "Nitrogen", ]), 3)
samples.air = sample(rownames(data[data$gas == "Air", ]), 3)
initial.data = data[c(samples.argon, samples.nitro, samples.air), ]

ctrl = makeMBOControl(y.name = "ratio", store.model.at = 1:(iters+1))
ctrl = setMBOControlInfill(ctrl, opt = "focussearchSavepts", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritEI())
ctrl = setMBOControlTermination(ctrl, iters = iters)

res.mbo = mbo(objfun, design = initial.data, control = ctrl, show.info = TRUE)

##################### Part One: ICEs, PDPs, ALEs ###############################
##################### FeatureEffectMBO #########################################
# libraries requires
library(iml)
library(foreach)
library(doParallel)
library(checkmate)

# Files to source
source("Tools_2_source/utils_fembo_inflInst.R")
source("Tools_2_source/PredictorAf.R")
source("Tools_2_source/FeatureEffectMBO.R")

# different feature config. up to 2nd order iteractions
first.order = list("power", "time", "pressure", "gas")
fo.names = c("power", "time", "pressure", "gas")

second.order = list(
  c("power", "time"), c("power", "pressure"), c("gas", "power"),
  c("time", "pressure"), c("gas", "time"),
  c("gas", "pressure")
)
so.names = c("power_time", "power_pressure", "gas_power", "time_pressure", "gas_time",
             "gas_pressure")

res = res.mbo

################## SURROGATE ##################################################
###############################################################################
#### PDP ######
# first order
surrogate.pdp.first = list()
for (i in seq_along(first.order)) {
  surrogate.pdp.first[[i]] = FeatureEffectMBO(res.mbo = res, interest = "surrogate", 
                                              feature = first.order[[i]], method = "pdp+ice")
}
names(surrogate.pdp.first) = fo.names

# second order
surrogate.pdp.second = list()
for (i in seq_along(second.order)) {
  surrogate.pdp.second[[i]] = FeatureEffectMBO(res.mbo = res, interest = "surrogate", 
                                               feature = second.order[[i]], method = "pdp")
}
names(surrogate.pdp.second) = so.names

#### ALE ######
# first order
surrogate.ale.first = list()
for (i in seq_along(first.order)) {
  surrogate.ale.first[[i]] = FeatureEffectMBO(res.mbo = res, interest = "surrogate", 
                                              feature = first.order[[i]], method = "ale")
}
names(surrogate.ale.first) = fo.names


# second order
surrogate.ale.second = list()
for (i in seq_along(second.order)) {
  surrogate.ale.second[[i]] = FeatureEffectMBO(res.mbo = res, interest = "surrogate", 
                                               feature = second.order[[i]], method = "ale")
}
names(surrogate.ale.second) = so.names

################## Acquisition #################################################
################################################################################
#### PDP ######
# first order
acquisition.pdp.first = list()
for (i in seq_along(first.order)) {
  acquisition.pdp.first[[i]] = FeatureEffectMBO(res.mbo = res, interest = "acquisition", 
                                                feature = first.order[[i]], method = "pdp+ice")
}
names(acquisition.pdp.first) = fo.names

# second order
acquisition.pdp.second = list()
for (i in seq_along(second.order)) {
  acquisition.pdp.second[[i]] = FeatureEffectMBO(res.mbo = res, interest = "acquisition", 
                                                 feature = second.order[[i]], method = "pdp")
}
names(acquisition.pdp.second) = so.names

#### ALE ######
# first order
acquisition.ale.first = list()
for (i in seq_along(first.order)) {
  acquisition.ale.first[[i]] = FeatureEffectMBO(res.mbo = res, interest = "acquisition", 
                                                feature = first.order[[i]], method = "ale")
}
names(acquisition.ale.first) = fo.names


# second order
acquisition.ale.second = list()
for (i in seq_along(second.order)) {
  acquisition.ale.second[[i]] = FeatureEffectMBO(res.mbo = res, interest = "acquisition", 
                                                 feature = second.order[[i]], method = "ale")
}
names(acquisition.ale.second) = so.names


################################################################################
############################## Results #########################################


surrogate = list(pdp = list(first = surrogate.pdp.first, second = surrogate.pdp.second),
                 ale = list(first = surrogate.ale.first, second = surrogate.ale.second)
)
af = list(pdp = list(first = acquisition.pdp.first, second = acquisition.pdp.second),
          ale = list(first = acquisition.ale.first, second = acquisition.ale.second)
)

#The final results should be stored in a (nested) list (total elements = 36 * iters)
fe.kapton = list(surrogate = surrogate, af = af)



############### PART TWO: Distance to Decision #################################
################################################################################



# extract necessary objects from resulting mbo object res.mbo
#####
opt.path = as.data.frame(res.mbo$opt.path, discretes.as.factor = FALSE)
n.init = length(opt.path$prop.type[opt.path$prop.type == "initdesign"])
n.iter = res.mbo$control$iters * res.mbo$control$propose.points
vars.total = colnames(res.mbo$seen.points[[1]]) # this way works for both vectorized and scalarized variables
targets = res.mbo$control$y.name
pars = vars.total[which(vars.total != targets)] 
par.set = res.mbo$opt.path$par.set
af.raw = res.mbo$control$infill.crit$fun
seen.points = res.mbo$seen.points
# edited
for (i in 1:length(seen.points)) {
  seen.points[[i]]$gas = as.factor(seen.points[[i]]$gas)
}
# end edited
prop.points = opt.path[-(1:n.init), pars]
#####

# rewrite extracted aquisition function
af = function(seen.points.dob, dob){
  
  af.raw(points = seen.points.dob,
         models = list(res.mbo$models[[dob]]),
         par.set = par.set,
         control = res.mbo$control,
         designs = list(opt.path[opt.path$dob < dob, c(pars, targets) ])
  )
  
}

# create list to store results
af.prop = list()

# create structure in af.prop: a nested list 
for (dob in 1:n.iter) {
  af.prop[[dob]] = list()
  for (par in pars) {
    af.prop[[dob]][[par]] = list() 
  }
}

# create grid 
par.grid = list()
for (par in pars) {
  if (par.set$pars[[par]][["type"]] != "discrete")
    par.grid[[par]] = seq(from = par.set$pars[[par]]$lower, to = par.set$pars[[par]]$upper, length.out = grid.size)
  else
    par.grid[[par]] = unlist(par.set$pars[[par]][["values"]])
}



# obtain proposed points for each grid value for each parameter
for (dob in 1:n.iter) {
  for (par.ind in 1:length(pars)) {
    par = pars[par.ind]
    if (par.set$pars[[par]]$type != "discrete") {
      # run the stuff for non-cat features
      for (grid.value in 1:grid.size) {
        seen.points.par = as.data.frame(seen.points[dob], stringsAsFactors = TRUE)
        # replace par data with grid values
        seen.points.par[,par] = rep(par.grid[[par]][grid.value], length(seen.points.par[,par]))
        # compute af values
        af.val = af(seen.points.par, dob)
        # obtain proposed point
        min.index = BBmisc::getMinIndex(af.val, ties.method = "random")
        af.prop[[dob]][[par]][[grid.value]] = seen.points.par[min.index,pars]
      }
    } else { # for cat features
      seen.points.par = as.data.frame(seen.points[dob], stringsAsFactors = TRUE)
      #replace par data with grid values (i.e. the categories)
      foreach (catg.ind = 1:length(par.grid[[par]])) %do% {
        catg = par.grid[[par]][catg.ind]
        levels = par.set$pars[[par]]$values
        seen.points.par[,par] = factor(rep(catg, length(seen.points.par[,par])), levels = levels)
        # compute af values
        af.val = af(seen.points.par, dob)
        # obtain proposed point
        min.index = BBmisc::getMinIndex(af.val, ties.method = "random")
        af.prop[[dob]][[par]][[catg]] = seen.points.par[min.index,pars]
      }
    }  
  }
}




## Total Distance
######

# create nested list for all-dec-plot in app
pdp.af.dec.all = list()
for (dob in 1:n.iter) {
  pdp.af.dec.all[[dob]] = list()
  for (par in pars) {
    pdp.af.dec.all[[dob]][[par]] = list() 
  }
}

# fill nested list for all-dec-plot in app
for (dob in 1:iters) {
     af.prop.iter = af.prop[[dob]]
     names(af.prop.iter) = pars
       for (par.ind in 1:length(pars)) {
                 par = pars[par.ind]
                 # combine with actually proposed points
                 af.prop.prop = lapply(af.prop.iter[[par]], rbind, prop.points[dob,])
                 # compute distances to actually proposed points
                 af.dec.dist.sgl = lapply(af.prop.prop, cluster::daisy, metric = "gower")
                 pdp.af.dec.all[[dob]][[par]] = data.frame("distance" = unlist(af.dec.dist.sgl), 
                                                     "parameter" = par.grid[par],
                                                     "prop.par.val" = prop.points[dob, par])
       }
}





# create list for all-dec-plot in app, distance without selected parameter (Lars Version)


# create nested list for all-dec-plot in app
pdp.af.dec.all.wopar = list()
for (dob in 1:n.iter) {
  pdp.af.dec.all.wopar[[dob]] = list()
  for (par in pars) {
    pdp.af.dec.all.wopar[[dob]][[par]] = list() 
  }
}


# fill nested list for all-dec-plot in app
for (dob in 1:iters) {
  af.prop.iter = af.prop[[dob]]
  names(af.prop.iter) = pars
  for (par.ind in 1:length(pars)) {
    par = pars[par.ind]
    # combine with actually proposed points
    af.prop.prop.all = lapply(af.prop.iter[[par]], rbind, prop.points[dob,])
    # drop selected parameter
    af.prop.prop = lapply(af.prop.prop.all, dplyr::select, -par)
    # compute distances to actually proposed points (in all but the selected dimension)
    af.dec.dist.sgl = lapply(af.prop.prop, cluster::daisy, metric = "gower")
    pdp.af.dec.all.wopar[[dob]][[par]] = data.frame("distance" = unlist(af.dec.dist.sgl), 
                                              "parameter" = par.grid[par],
                                              "prop.par.val" = prop.points[dob, par])
  }
}  















