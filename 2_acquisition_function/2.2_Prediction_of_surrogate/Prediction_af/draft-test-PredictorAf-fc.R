# This script is intended to test whether PredictorAf works properly

# source the data and MBO configs
source("fc_notizen/materials-design-playground-fc.R")

# run the MBO with CB and Savepts
source("2_acquisition_function/2.2_Prediction_of_surrogate/FeatureEffectMBO-playground-fc.R")

# create the PredictorAf object
source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/PredictorAf-fc.R")

# Test for the Confindence Bound auf seen points (nur die ersten 100) in iter 1 
# create data and design
X = res.mbo.cb$seen.points[[1]][1:100,]
X_y = objfun(X)# Zielvariable
XX = cbind(X, target = X_y) # Tielvariable muss korrekten Namen haben
opdf = as.data.frame(res.mbo.cb$opt.path)
# create the Predictor object
p.cb = PredictorAf$new(model = res.mbo.cb$models[[1]], # erstes Surrogate Model
                       data = XX, # inkl. Zielvariable
                       res.mbo = res.mbo.cb,
                       #iter.mbo = 1, 
                       par.set = res.mbo.cb$opt.path$par.set, 
                       control = res.mbo.cb$control, 
                       designs = opdf[1:20,1:7] # Trainingsdaten in der ersten Iteration, inkl. Zielvariable (Spalte "target")
)
# test if they predict the same values
# mit $predict() koennen wir predicten
pred.cb = p.cb$predict(X)
# pred.cb.with.af ist mit der normalen AF extrahiert aus res.mbo -- siehe test-julia
pred.cb.with.af = res.mbo.cb$control$infill.crit$fun(points = X, 
                                                     models = list(res.mbo.cb$models[[1]]), 
                                                     control = res.mbo.cb$control, 
                                                     par.set = res.mbo.cb$opt.path$par.set, 
                                                     designs = list(opdf[1:20,1:7]), 
                                                     iter = NULL, 
                                                     progress = NULL, 
                                                     attributes = FALSE)
df = cbind(pred.cb, pred.cb.with.af)
names(df) = c("beliebig", "beliebig")
all.equal(df[,1], df[,2]) # falls TRUE dann koennen wir mit erhalten wir mit pred.cb$predict die 
# gleichen Ergebnissen :) 

# Erstellen eine Object FeatureEffect mit dem man PDP, ICE, ALE berechnen kann argumnent "method"
eff.cb = FeatureEffect$new(predictor = p.cb, feature = c("y","x"), method = "pdp")

# so kann man plots erstelle
eff.cb$plot()
# oder so
plot = plot(eff.cb) # so ist besser da es ein ggplot Objekt ist und veranerderbar ist

# sonst kann auch die Ergbnisse speichern und eigene Plots erstellen 
r = eff.cb$results # ist ein data frame 

# test if it actually is different from normal iml
pred = Predictor$new(model = res.mbo.cb$models$`1`, data = XX[,-max(ncol(XX))])
eff = FeatureEffect$new(predictor = pred, feature = c("y","x"), method = "pdp")
eff$plot() # ja die Plots sind unterschiedlich 

###### EI ( noch nicht grundlich getestet) #####
# create data and design
X = res.mbo.ei$seen.points[[1]][1:100,]
X_y = objfun(X)
XX = cbind(X, X_y)
opdf = as.data.frame(res.mbo.ei$opt.path)
# create the Predictor obeject
p.ei = PredictorAf$new(model = res.mbo.ei$models[[1]], 
                       data = XX[,-max(ncol(XX))], 
                       res.mbo = res.mbo.ei, 
                       #iter.mbo = 1, 
                       par.set = res.mbo.ei$opt.path$par.set, 
                       control = res.mbo.ei$control, 
                       designs = opdf[1:20,1:7], 
                       y = XX[,max(ncol(XX))])
# test if they predict the same values
pred.ei = p.ei$predict(X)
pred.ei.with.af = res.mbo.ei$control$infill.crit$fun(points = X, 
                                                     models = list(res.mbo.ei$models[[1]]), 
                                                     control = res.mbo.ei$control, 
                                                     par.set = res.mbo.ei$opt.path$par.set, 
                                                     designs = list(opdf[1:20,1:7]), 
                                                     iter = NULL, 
                                                     progress = NULL, 
                                                     attributes = FALSE)

# test if FeatureEffect$new() works with the new predictor
eff.ei = FeatureEffect$new(predictor = p.ei, feature = c("y","x"), method = "pdp")
eff.ei$plot()


