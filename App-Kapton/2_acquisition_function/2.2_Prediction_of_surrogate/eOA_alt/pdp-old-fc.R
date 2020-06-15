# PDP 

library(iml)
library(ggplot2)
library(DiceKriging)

# the sm (GP) fitted in each iteration (iters = 2)
# - in each iteration a GP is estimated, with FeatureEffect we see how the prediction 
#   of the surrogate moodel (y_hat) changes wrt to a specific feature
# - but y_hat IS NOT equal to the 'target' values for the same obs.
# - T = Object of class "matrix". Triangular matrix delivered by the Choleski 
#     decomposition of the covariance matrix.
sm_iter1 <- res_mbo[["models"]][[1]]
sm_iter2 <- res_mbo[["models"]][[2]]

# - Step 1: create a predictor: We create a Predictor object, that holds the model and the data. 
# - The iml package uses R6 classes: New objects can be created by calling Predictor$new().
predictor_iter1 <-  Predictor$new(sm_iter1, 
                                  data = as.data.frame(gp_iter1[["learner.model"]]@X), 
                                  y = sm_iter1[["learner.model"]]@y)


# calculting the PDP + ICE for feature f, in iter 1, on 50 points
pdp_iter1_f <- FeatureEffect$new(predictor = predictor_iter1, 
                                feature = "f", 
                                method = "pdp+ice",
                                grid.size = 50
                                )

# plot() generates the plots - Since the result is a ggplot object, you can extend it
plot_f <- plot(pdp_iter1_f) +
  labs(title = "PDP, ICE for feature f", y = "predicted outcome")
  

# results: data.frame with the grid of feature of interest and the predicted \hat{y}. 
# Can be used for creating custom effect plots.
res_pdp_iter1 <- pdp_iter1_f$results


#- Now that we now how to measure the effect for a feature, we need to understand how
#  to plot the EI and not the predicted outcome (the predicted ratio)
#  1.possibility: we use EI directly in the predictor
#  2.we calculate the EI for every predicted outcome and create our own plot
#- NOTE that the change in the EI in the initial surrogate model (model in iter 1)
#  does not make sense since the EI on observed points in 0
#- Q: FeatureEffect does not work for 1 single obs 
# Possibile solution: write your own function for ICE and PDP

