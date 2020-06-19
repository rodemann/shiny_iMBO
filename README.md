# shiny_iMBO
This repository allows running an R Shiny App to visualizie interpretable Bayesian Opimization methods from iMBO

This is how it works:

* First and foremost, clone this repo 
* Source App_Kapton/shiny-helper-kapton.R 
+ with #global setting iter and grid.size you can decide how many iterations the BO should run and how precise the grid.size should be. Consider the computational effort for many iteration and many grid values.
* Source App_Kapton/app.R
* ... and you're done. The app is deployed locally on your machine. 

Note that you can change the hyperparameter of the Bayesian Optimization that is visualized in shiny-helper-kapton.R (lines 9-13).
