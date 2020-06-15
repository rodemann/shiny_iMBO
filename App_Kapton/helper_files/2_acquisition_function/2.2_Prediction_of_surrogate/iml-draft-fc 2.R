library(sloop)
# iml playground 
library(iml)
library(ggplot2)
library(gridExtra)
pars.mbo = names(res.mbo.ei$x)

#1. EI: prediction of initial data with surrogate models stored
pred.ei.1 = predict(res.mbo.ei$models$`1`, newdata = initial.data[,pars.mbo])
pred.ei.2 = predict(res.mbo.ei$models$`2`, newdata = initial.data[,pars.mbo])

 #prediction of the proposed point in iter i
 pred.pp.ei.1 = predict(res.mbo.ei$models$`1`, newdata = opdf.ei[10,pars.mbo])
 # is the same as opdf.ei[10,c("mean", "se")]
 
 # try to use iml package to see how the predictions of the sm on the initial data changes
 # for iter 1:
 # 1.create a predictor obejct that holds model and data
   X = initial.data[,names(res.mbo.ei$x)]
   predictor.1 = Predictor$new(model = res.mbo.ei$models$`1`, data = X, 
                               y = initial.data[,"ratio"], type = "se")   
   
 # 2. compute the Feature Effect (ALE, ICE, PDP)
   # for feature "power"
   # PDP
   pdp.1 = FeatureEffect$new(predictor = predictor.1, feature = c("gas"), 
                             method = "pdp",
                             grid.size = 30)
   pdp.1   
  # plot
    a = pdp.1$plot() + ggtitle("We can combine results with ggplot")
  # df.1 - results - pdp is the average over all observations 30x3
   df.pdp.1 = pdp.1$results
   # results do not contain se only mean -- we need se to construct infill criteria
   # with predict we could predict the mean at the propsed point
   pdp.1$predict() #n-- the same as pdp.1$results
   
  # ICE 
   ice.1 = FeatureEffect$new(predictor = predictor.1, feature = "power", method = "pdp+ice",
                             grid.size = 30)
   #plot
   b = ice.1$plot()
   # df.1 - results - 300x4 = (pdp*grid + nrow(initial.data)*grid)
   df.ice.1 = ice.1$results 
   
   # pdp for every feature
   pdp.1.all = FeatureEffect$new(predictor = predictor.1, method = "pdp",
                                 grid.size = 30)
   #Error in private$sanitize.feature(feature, predictor$data$feature.names) : 
   # l'argomento "feature"  non è specificato e non ha un valore predefinito
   # --> we shoul use Feature Effects
   
   #plotting next to each other 
   grid.arrange(a,b, ncol = 2)
   #use $results to build custom plots (icd, pdp line ubereinander, ein Pfeil pro Iter)
 # 3. computing 2 order effects
   pdp.order2 = FeatureEffect$new(predictor = predictor.1, method = "pdp",
                                  feature = c("power", "pressure"), grid.size = c(30)
                                  )
   pdp.order2$plot()
   b = pdp.order2$results
{## Fazit: 
#  -using iml with surrogate models is not difficult, we can easily build a function to see
#   how the prediction of the surrogate model changes over the iteration   
#  -on my opinion measuring how the prediction of the initial data changes within 
#  one and between iteration has no sense because we are not intrested in the predictions of
#  the initial data, but in the predictions of the proposed points!
#  -the interesting part of the task is to see how the infill criterion changes over
#   the iters & which points from the seen one are chosen based on the infill criteria: 
#   for that we have following solutions:
#   1. we try to edit the iml package create_pred_fun & Line 111 in Predictor
#      --> can be very difficult (Obj.Or.Progr R6), even impossible 
#      --> we could try to use other packages like pdp, ICE etc.   
#   2. we use the results of the iml package and compute the values of the infill ourselves
#     --> could work(probably the easiest way), 
#         BUT for the most criteria we also need the SE, iml also gives Response back. Could we use somehow mlr::getPredictionSE()?
#         we could evetually predict again
#     --> also we should create plots ourselves
}
# Now we try to integrate the function of the infill criteria in the pred.fun of Predictor
# The function to predict newdata. Only needed if model is not a model from mlr or 
# caret package. The first argument of predict.fun has to be the model, 
# the second the newdata: function(model, newdata)

   # 1.create a Predictor
   X = res.mbo$seen.points[[1]][1:100,]
   # y = opdf[101, res.mbo$control$y.name]
   
   # create custom predict function
   # MEAN RESPONSE
   pfMEAN = function(model, newdata, res.mbo) {
      ifelse(res.mbo$control$minimize, 1, -1) * predict(model, newdata = newdata)$data$response
   }
   mean.response = pfMEAN(model = res.mbo$models$`1`, newdata = X, res.mbo = res.mbo)
   
   pred.1 = Predictor$new(model = res.mbo$models$`1`, data = X, y = NULL, predict.fun = pfMEAN)

   # 2. create FeatureEffect Obeject
   # this is with the customized predict function which measures the MEAN 
   effect.mean = FeatureEffect$new(pred.1, feature = "f", method = "ice", 
                              grid.size = 20,  center.at = NULL)
   effect.mean$plot()

   
   # since MeanResponse is simply the Mean we should get the same results, if we run the 
   # normal FeatureEffect
   pred.1.plain = Predictor$new(model = res.mbo$models$`1`, data = X, y = NULL)
   effect.mean.plain = FeatureEffect$new(pred.1.plain, feature = "f", method = "ice", 
                                         grid.size = 20,  center.at = NULL)
   effect.mean.plain$plot()
   effect.mean.plain$predict(X[1:3,])

   # Standard Error
   pfSE = function(model, newdata) {
      -predict(model, newdata = newdata)$data$se
   }
   se = pfSE(model = res.mbo$models$`1`, newdata = X)
   
   pred.2 = Predictor$new(model = res.mbo$models$`1`, data = X, y = NULL, predict.fun = pfSE)
   effect.se = FeatureEffect$new(pred.2, feature = "f", method = "ice", 
                                 grid.size = 20,  center.at = NULL)
   
   grid.arrange(effect.se$plot(), effect.mean.plain$plot())
   
##########################
Predictor
names(pred.1) # list all the methods and fields

# Debugging
X = initial.data[,pars.mbo]
# Enabling debugging for all future instances of a class
Predictor$debug("create_predict_fun")
predictor.1 = Predictor$new(model = res.mbo.ei$models$`1`, data = X)  
predictor.1$create_predict_fun()
Predictor$undebug("create_predict_fun")
#Debugging methods in individual objects
debug(predictor.1$prediction.function)
predictor.1$prediction.function()
undebug(predictor.1$prediction.function)
## in predictor.1 it gets automatically in create_fpred_fun_WrappedModel
predictor.2 = Predictor$new(model = res.mbo.ei$models$`1`, data = X, predict.fun = pfMEAN)
debug(predictor.2$prediction.function)
predictor.2$prediction.function()
undebug(predictor.2$prediction.function)
## also in predictor.2
predictor.3 = Predictor$new(model = NULL, data = X, predict.fun = pfMEAN)
debug(predictor.3$prediction.function)
predictor.3$prediction.function()
undebug(predictor.3$prediction.function)
## if model = NULL is goes to create_pred_fun_NULL and uses argument predict.fun to predict,
## but Error in ifelse(res.mbo$control$minimize, 1, -1) : l'argomento "res.mbo"  non è specificato e non ha un valore predefinito
predictor.4 = Predictor$new(model = NULL, data = X)
debug(predictor.4$prediction.function)
predictor.4$prediction.function()
undebug(predictor.4$prediction.function)
## Error in .subset2(public_bind_env, "initialize")(...) : Provide a model, a predict.fun or both!