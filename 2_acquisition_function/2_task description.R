# 2. Interpretation of the decision of the acquisition function (based on the surrogate model, of course) in each iteration 
# (Use Friedman’s H-Statistic to check how strong interaction effects are)
# 2.1. For correlated features: Visualize the features’ effects on the proposed points by Accumulated Local Effects (ALE) Plots. 
# 2.1.1. Main effects (first-order ALE)
# 2.1.2. Interactions (second-order ALE)
# 2.1.3. Main effects and interactions combined, e.g. through a colourful   ALE matrix (main effects on the diagonal) 
# 2.2. For uncorrelated features: Visualize the features’ effects on the proposed points by Partial Dependence Plots (PDP)/Individual Conditional Expectation (ICE) and Accumulated Local Effects (ALE) 
# 2.2.1. Main effects
# 2.2.2. Interactions
# 2.2.3 Shapley Values for feature effects of the (proposed and evaluated) data point in the iteration 


# -in the BO Process the EI is maximized, therefore it is trivial to show, which decision
#  the the acquisition function makes. Still, we can see how much each parameter affect the 
#  EI of the proposed point in a given interation
# -first we compute this for a single iteration and then implement it between the iterations



