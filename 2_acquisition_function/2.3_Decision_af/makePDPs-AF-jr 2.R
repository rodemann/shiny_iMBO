library(iml)
library(pdp)
library(foreach)
library(doParallel)
source("_Explore_Exploit_Measures/xplxplMBO-jr.R")
source("jr_notizen/materials-design-playground-jr.R")
set.seed(43)


makePDPListsAF <- function(res.mbo, grid.size = 101) {
  
  # Input checking TODO
  
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

  
  # create lists to store results
  min.af.pred = list()
  mean.af.pred = list()
  af.prop = list()
  # create grid for PDPs
  par.grid = list()
  for (par in pars) {
  par.grid[[par]] = seq(from = par.set$pars[[par]]$lower, to = par.set$pars[[par]]$upper, length.out = grid.size)
  #create vector/list to store average predictions/proposed points
  mean.af.pred[[par]] = vector("double", length = grid.size) 
  min.af.pred[[par]] = vector("double", length = grid.size) 
  af.prop[[par]] = list()
  }
  # initialize clusters for parallelization
  cores = detectCores()
  cl = makeCluster(cores[1] - 1) 
  cl = parallel::makeCluster(n.iter)


#obtain mean af-values (original PDP values)  
doParallel::registerDoParallel(cl)
mean.af.pred = foreach(dob = 1:n.iter,
                  .packages = c("mlrMBO","foreach")) %dopar% { 
                    foreach(par.ind = 1:length(pars)) %do% {
                      foreach(grid.value = 1:grid.size) %do% {
                        par = pars[par.ind]
                        seen.points.par = as.data.frame(seen.points[dob])
                        # replace par data with grid values
                        seen.points.par[,par] = rep(par.grid[[par]][grid.value], length(seen.points.par[,par]))
                        # compute af values
                        af.val = af(seen.points.par, dob)
                        #get mean af-value 
                        mean.af.pred[[par]][grid.value] = mean(af.val)
                      } } }  
  
#obtain minimal af-values (af values of the proposed point)  
doParallel::registerDoParallel(cl)
min.af.pred = foreach(dob = 1:n.iter,
                  .packages = c("mlrMBO","foreach")) %dopar% { 
                    foreach(par.ind = 1:length(pars)) %do% {
                      foreach(grid.value = 1:grid.size) %do% {
                        par = pars[par.ind]
                        seen.points.par = as.data.frame(seen.points[dob])
                        # replace par data with grid values
                        seen.points.par[,par] = rep(par.grid[[par]][grid.value], length(seen.points.par[,par]))
                        # compute af values
                        af.val = af(seen.points.par, dob)
                        #get minimum (af value of the proposed point) 
                        min.af.pred[[par]][grid.value] = min(af.val)
                      } } }  

# obtain proposed point for each grid value for each parameter  
doParallel::registerDoParallel(cl)
af.prop = foreach(dob = 1:n.iter,
        .packages = c("mlrMBO","foreach")) %dopar% { 
       foreach(par.ind = 1:length(pars)) %do% {
         foreach(grid.value = 1:grid.size) %do% {
           par = pars[par.ind]
           seen.points.par = as.data.frame(seen.points[dob])
           # replace par data with grid values
           seen.points.par[,par] = rep(par.grid[[par]][grid.value], length(seen.points.par[,par]))
           # compute af values
           af.val = af(seen.points.par, dob)
           # obtain propsed point 
           min.index = getMinIndex(af.val, ties.method = "random")
           af.prop[[par]][[grid.value]] = seen.points.par[min.index,pars]
         } } }    

# create shiny-ready data frames
mean.af.df = data.table::rbindlist(mean.af.pred[iter])
colnames(mean.af.df) = pars
min.af.df = data.table::rbindlist(min.af.pred[iter])
colnames(min.af.df) = pars
af.prop = data.table::rbindlist(af.prop[iter])
colnames(af.prop) = pars


# Responsive Plotting function for Shiny

makePDPclassic = function(mean.af.pred = mean.af.pred, par = par, pars = pars, 
                          par.grid = par.grid, iter = iter){
  # some conversions to data frames
  mean.af.df = data.table::rbindlist(mean.af.pred[iter])
  colnames(mean.af.df) = pars
  pdp.df.classic = as.data.frame(cbind(par.grid[[par]], mean.af.df[[par]]))
  colnames(pdp.df.classic) =  c(par,"AF")
  
  plot(pdp.df.classic[[par]],pdp.df.classic[["AF"]], "l", xlab = par, ylab = "Acquisition Function")
  
}

  
  
  
  pdp.df.classic[par] = as.numeric(as.vector(unlist(pdp.df.classic[par])))
  pdp.df.classic["AF"] = as.numeric(as.vector(unlist(pdp.df.classic["AF"])))
  
  ggplot2::ggplot(data = pdp.df.classic, aes(x = par, y = AF)) +
        geom_area(aes, stat = "bin")
        geom_line(aes(y = AF), color = "magenta2")
  
  ggplot(data = pdp.df.classic, aes(x=par, y=AF)) +
    geom_point(aes(x=par, y=AF))
    
  geom_jitter()
    geom_line()+
    geom_point()
  
  ggplot2::ggplot(pdp.df.classic, aes(x=par, y=AF)) +
    geom_point() #+
    geom_point() 
  +
    geom_rug(data=rug_data, aes(x=score, alpha = n), col="steelblue", size=1.5, sides="b")
  
  
  +
    labs(
      title = (paste("The effect of feature", feature, "on the acquisition function")),
      x = feature
    ) + 
    theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
    facet_wrap(~dob)
  plot
  
  
}

iter = 1
par.ind = 1

plot(unlist(par.grid[par]), unlist(mean.af.pred[[iter]][[par.ind]]), "l")


plot = ggplotify::as.grob( function() plot(unlist(par.grid[par]), unlist(mean.af.pred[[iter]][[par.ind]])))

grid::grid.newpage()
grid::grid.draw(plot)






}






# 
# comb <- function(x, ...) {
#   lapply(seq_along(x),
#          function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
# }
# 
# doParallel::registerDoParallel(cl)
# af.pdp.val = foreach(dob = 1:n.iter,
#         .packages = c("foreach"),
#         .combine = "comb",
#         .multicombine = TRUE,
#         .init = list(list(), list(), list())) %dopar% {
#      foreach(par.ind = 1:length(pars)) %do% {
#        foreach(grid.value = 1:grid.size) %do% {
#          print(grid.value)
#          par = pars[par.ind]
#          seen.points.par = as.data.frame(seen.points[dob])
#          # replace par data with grid values
#          seen.points.par[,par] = rep(par.grid[[par]][grid.value], length(seen.points.par[,par]))
#          # compute af values
#          af.val = af(seen.points.par, dob)
#          # obtain average af-value for df that includes grid values
#          mean.af.pred[[par]][grid.value] = mean(af.val)
#          # and minimum (af value of the proposed point)
#          min.af.pred[[par]][grid.value] = min(af.val)
#          # obtain propsed point
#          min.index = getMinIndex(af.val, ties.method = "random")
#          af.prop[[par]][[grid.value]] = seen.points.par[min.index,pars]
#          # return list
#          list(mean.af.pred, min.af.pred, af.prop)
#   } } }
# mean.af.pred = af.pdp.val[1]
# min.af.pred = af.pdp.val[2]
# af.prop = af.pdp.val[3]



