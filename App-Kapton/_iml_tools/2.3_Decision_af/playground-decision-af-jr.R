library(iml)
library(pdp)

source("jr_notizen/materials-design-playground-jr.R")
set.seed(43)

#res.mbo = mbo(objfun, design = initial.data, control = ctrl, show.info = TRUE)


# 
# n.init = length(opt.path$prop.type[opt.path$prop.type == "initdesign"])
# n.tot = nrow(opt.path)
# n.iter = res.mbo$control$iters * res.mbo$control$propose.points
# initial.data = opt.path[1:n.init,pars]
# infill.crit = res.mbo$control$infill.crit
# infill.crit.id = res.mbo$control$infill.crit$id
# n.objectives = res.mbo$control$n.objectives
# n.prop.points = res.mbo$control$propose.points
# 


# General Idea: We use the seen points and the af to interpret features effects

# first, we try to apply Feature Importance Measure
# Just like Permutation Feature Importance (PFI), we shuffle each feature and 
# observe the change in the proposed point. We compare the newly proposed point
# to the old one by euclidean distance/gower distance.


# extract necessary objects from resulting mbo object res.mbo
opt.path = as.data.frame(res.mbo$opt.path, discretes.as.factor = FALSE)
n.iter = res.mbo$control$iters * res.mbo$control$propose.points
vars.total = colnames(res.mbo$seen.points[[1]]) # this way works for both vectorized and scalarized variables
targets = res.mbo$control$y.name
pars = vars.total[which(vars.total != targets)] 
par.set = res.mbo$opt.path$par.set
af.raw = res.mbo$control$infill.crit$fun
seen.points = res.mbo$seen.points

# rewrite extracted aquisition function
af = function(seen.points.dob, dob){
  
  af.raw(points = seen.points.dob,
   models = list(res.mbo$models[[dob]]),
   par.set = par.set,
   control = res.mbo$control,
   designs = list(opt.path[opt.path$dob < dob, c(pars, targets) ])
   )
  
}



#reshuffle data for each feature in each information and save the newly proposed points
prop.points.global = list()
for (dob in 1:n.iter) {
  seen.points.dob = as.data.frame(seen.points[dob])

  prop.points.local = list()
  for (i in seq_along(pars)) {
    par = pars[i]
    seen.points.par = seen.points.dob
    #shuffle par
    seen.points.par[,i] = sample(seen.points.par[,i])
    af.val = af(seen.points.dob, dob)
    # get propsed point 
    min.index = getMinIndex(af.val, ties.method = "random")
    prop.points.local[[par]] = seen.points.par[min.index,pars]
  }
  prop.points.global[[dob]] = prop.points.local
}

prop.points.global
# results show that this approach does not make sense


# PDPs for af 
dob = 1
seen.points.dob = as.data.frame(seen.points[dob])
#seen.points.dob$af = af(seen.points.dob, dob)

# task = makeRegrTask(data = seen.points.dob, target = "af")
# learner = makeLearner("regr.lm")
# 
# Predictor$new(data = seen.points.dob, predict.function = af, y ="af")
# 
# Predictor$new()



dob = 1
seen.points.dob = as.data.frame(seen.points[dob])
af.pred = list()
af.prop = list()

cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)


foreach(i = 1:3, .combine = 'c') %dopar% {
  seen.points.dob = as.data.frame(seen.points[dob])
  af.pred[[dob]] = list()
  af.prop[[dob]] = list()
  
  for (i in seq_along(pars)) {
    par = pars[i]
    # create grid
    par.grid = seq(from = par.set$pars[[par]]$lower, to = par.set$pars[[par]]$upper, length.out = 101)
    #create vectors to store average predictions/proposed points
    af.pred[[dob]][[par]] = vector("double", length = length(par.grid)) 
    af.prop[[dob]][[par]] = list()
    
    for (grid.value in seq_along(par.grid)) {
      seen.points.par = seen.points.dob
      # replace par data with grid values
      seen.points.par[,i] = par.grid[grid.value]
      # compute af values
      af.val = af(seen.points.par, dob)
      # obtain average af-value for df with grid values
      af.pred[[dob]][[par]][grid.value] = mean(af.val) 
      # obtain propsed point 
      min.index = getMinIndex(af.val, ties.method = "random")
      af.prop[[dob]][[par]][[grid.value]] = seen.points.par[min.index,pars]
    }
  }
}









