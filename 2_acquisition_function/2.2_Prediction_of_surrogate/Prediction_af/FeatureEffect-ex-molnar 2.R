
# We train a random forest on the Boston dataset:
  data("Boston", package  = "MASS", eff)
  rf = randomForest(medv ~ ., data = Boston, ntree = 50)
  mod = Predictor$new(rf, data = Boston)
  
  # Compute the accumulated local effects for the first feature
  eff = FeatureEffect$new(mod, feature = "rm",grid.size = 30)
  eff$plot()
  
  # Again, but this time with a partial dependence plot and ice curves
  eff = FeatureEffect$new(mod, feature = "rm", method = "pdp+ice", grid.size = 30)
  plot(eff)
  
  # Since the result is a ggplot object, you can extend it: 
  # If you want to do your own thing, just extract the data: 
  eff.dat = eff$results
  # we can do a lot with the plots 
    plot(eff) + 
      # Adds a title
      ggtitle("Partial dependence") +
      # Adds original predictions
      geom_point(data = Boston, aes(y = mod$predict(Boston)[[1]], x = rm), 
                 color =  "pink", size = 0.5) +
      # Adds marginal predictions for specific values (in this case PDP value for the real data)
      # only works for PDP and ALE, not zwingend necessary 
      geom_point(data = Boston, aes(y = eff$predict(Boston[1:506,]), x = rm), 
                 color =  "blue", size = 0.5) +
      # highlight one specific ICE line (the one of the proposed point), we need to store the results,
      # only needed if method involves ICE, here we highlicht the ICE of .id 1
      geom_line(data = eff.dat[which(eff.dat$.id == 1),], aes(y = .y.hat, x = rm),
                  color = "green")

  # You can also use the object to "predict" the marginal values. Predict the PDP values for specific
  #  feature value(s)
  b = eff$predict(Boston[1:100,])
  d = mod$predict(Boston[1:100,]) # d and b are not the same!!!
  # Instead of the entire data.frame, you can also use feature values:
  c = eff$predict(c(5))
  
  # You can reuse the pdp object for other features: 
  eff$set.feature("lstat")
  plot(eff)
  
  # Only plotting the aggregated partial dependence:  
  eff = FeatureEffect$new(mod, feature = "crim", method = "pdp")
  eff$plot() 
  
  # Only plotting the individual conditional expectation:  
  eff = FeatureEffect$new(mod, feature = "crim", method = "ice")
  eff$plot() 
  
  # Accumulated local effects and partial dependence plots support up to two features: 
  eff = FeatureEffect$new(mod, feature = c("crim", "lstat"))  
  plot(eff)
  