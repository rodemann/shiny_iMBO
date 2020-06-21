# Shiny App on Kapton Data
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cluster)
library(data.table)
library(reshape2)
library(foreach)
library(iml)


# FOR LAUNCHING: source helper file creating all necessary objects 
#source("shiny-helper-kapton-jr.R")

ls()


pars = c("power", "time", "gas", "pressure")


# get amount of total iterations (needed as max value in app)
iter = length(fe.kapton[["surrogate"]][["pdp"]][["first"]][["power"]])

ui <- fluidPage(theme = "bootswatch-cerulean.css", 
                title = "iMBO - Interpretable Bayesian Optimization",
                
                tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 30pt !important; }")),              
                
                titlePanel(tags$h1(style = "font-family:Verdana", "Interpretable Bayesian Optimization")),
                p(
                  "This is an interactive visualization of ",
                  a("Individual Conditional Expectations (ICEs)",
                    href = "https://christophm.github.io/interpretable-ml-book/ice.html"),
                  ",",
                  a("Partial Dependence Plots (PDPs)",
                    href = "https://christophm.github.io/interpretable-ml-book/pdp.html"),
                  ",",
                  a("Accumulated Local Effects (ALEs)",
                    href = "https://christophm.github.io/interpretable-ml-book/ale.html"),
                  "and",
                  a("Distance to Decision",
                    href = "https://github.com/rodemann/ConsultingMBO"),
                  "applied on Bayesian Optimization with ",
                  a("Kapton Data",
                    href = "https://www.cs.uwyo.edu/~larsko/aimat-tut/"),
                  "."
                ),
                navbarPage(title = "Please select:", windowTitle = "iMBO App Kapton", inverse = "TRUE",
                           
# UI Surrogate Model
#####
                           navbarMenu(title = "Surrogate Model",
                                      tabPanel(title = "One Feature PDP/ICE",
                                               tabsetPanel( type = "pills",
                                                 tabPanel(title = "Single Plot",
                                                          sidebarPanel(
                                                            prettyRadioButtons(inputId = "par.sm.pdp.1.sgl",
                                                                               label = "Select One Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),
                                                            sliderInput("iter.sm.pdp.1.sgl", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE)),
                                                          ),
                                                          plotOutput("sm.pdp.1.sgl", height = "420px", width = "800")

                                                 ),
                                                 tabPanel(title = "All Plots",
                                                          sidebarPanel(
                                                            sliderInput("iter.sm.pdp.1.all", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1700, loop = FALSE))
                                                          ),
                                                          plotOutput("sm.pdp.1.all", height = "420px", width = "800")
                                                 )

                                               )
                                      ),
                                      tabPanel(title = "Two Features PDP",
                                               tabsetPanel(
                                                 tabPanel(title = "Single Plot",
                                                          sidebarPanel(
                                                            prettyRadioButtons(inputId = "par.sm.pdp.2.sgl.1",
                                                                               label = "Select First Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),
                                                            prettyRadioButtons(inputId = "par.sm.pdp.2.sgl.2",
                                                                               label = "Select Second Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),
                                                            sliderInput("iter.sm.pdp.2.sgl", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE))
                                                          ),
                                                          plotOutput("sm.pdp.2.sgl", height = "360px", width = "800")
                                                 ),
                                                 tabPanel(title = "All Plots",
                                                          sidebarPanel(
                                                            sliderInput("iter.sm.pdp.2.all", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 2500, loop = FALSE))
                                                          ),
                                                          plotOutput("sm.pdp.2.all", height = "420px", width = "800")
                                                 )
                                               )
                                      ),
                                      tabPanel(title = "One Feature ALE",
                                               tabsetPanel(
                                                 tabPanel(title = "Single Plot",
                                                          sidebarPanel(
                                                            prettyRadioButtons(inputId = "par.sm.ale.1.sgl",
                                                                               label = "Select One Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),   
                                                            sliderInput("iter.sm.ale.1.sgl", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE))
                                                          ),
                                                          plotOutput("sm.ale.1.sgl", height = "420px", width = "800")
                                                 ),
                                                 tabPanel(title = "All Plots",
                                                          sidebarPanel(
                                                            sliderInput("iter.sm.ale.1.all", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE))
                                                          ),
                                                          plotOutput("sm.ale.1.all", height = "420px", width = "800")
                                                 )
                                               )
                                      ),
                                      tabPanel(title = "Two Features ALE",
                                               tabsetPanel(
                                                 tabPanel(title = "Single Plot",
                                                          sidebarPanel(
                                                            prettyRadioButtons(inputId = "par.sm.ale.2.sgl.1",
                                                                               label = "Select First Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),  
                                                            prettyRadioButtons(inputId = "par.sm.ale.2.sgl.2",
                                                                               label = "Select Second Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),  
                                                            sliderInput("iter.sm.ale.2.sgl", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE))
                                                          ),
                                                          plotOutput("sm.ale.2.sgl", height = "360px", width = "800")
                                                 ),
                                                 tabPanel(title = "All Plots",
                                                          sidebarPanel(
                                                            sliderInput("iter.sm.ale.2.all", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE))
                                                          ),
                                                          plotOutput("sm.ale.2.all", height = "420px", width = "800")
                                                 )
                                               )
                                      )
                           ),
######                         
                           
# UI Acquisition Function 
#####
                           navbarMenu(title = "Acquisition Function",
                                      tabPanel(title = "One Feature PDP/ICE",
                                               tabsetPanel(
                                                 tabPanel(title = "Single Plot",
                                                          sidebarPanel(
                                                            prettyRadioButtons(inputId = "par.af.pdp.1.sgl",
                                                                               label = "Select One Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),
                                                            sliderInput("iter.af.pdp.1.sgl", "Choose Iteration:",
                                                                        min = 1, max = (iter - 1),
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE)),
                                                          ),
                                                          plotOutput("af.pdp.1.sgl", height = "420px", width = "800")
                                                          
                                                 ),
                                                 tabPanel(title = "All Plots",
                                                          sidebarPanel(
                                                            sliderInput("iter.af.pdp.1.all", "Choose Iteration:",
                                                                        min = 1, max = (iter - 1),
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 3000, loop = FALSE))
                                                          ),
                                                          plotOutput("af.pdp.1.all", height = "420px", width = "800")
                                                 )
                                                 
                                               )
                                      ),
                                      tabPanel(title = "Two Features PDP",
                                               tabsetPanel(
                                                 tabPanel(title = "Single Plot",
                                                          sidebarPanel(
                                                            prettyRadioButtons(inputId = "par.af.pdp.2.sgl.1",
                                                                               label = "Select First Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),
                                                            prettyRadioButtons(inputId = "par.af.pdp.2.sgl.2",
                                                                               label = "Select Second Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),
                                                            sliderInput("iter.af.pdp.2.sgl", "Choose Iteration:",
                                                                        min = 1, max = (iter - 1),
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1900, loop = FALSE))
                                                          ),
                                                          plotOutput("af.pdp.2.sgl", height = "360px", width = "800")
                                                 ),
                                                 tabPanel(title = "All Plots",
                                                          sidebarPanel(
                                                            sliderInput("iter.af.pdp.2.all", "Choose Iteration:",
                                                                        min = 1, max = (iter - 1),
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 3000, loop = FALSE))
                                                          ),
                                                          plotOutput("af.pdp.2.all", height = "420px", width = "800")
                                                 )
                                               )
                                      ),
                                      tabPanel(title = "One Feature ALE",
                                               tabsetPanel(
                                                 tabPanel(title = "Single Plot",
                                                          sidebarPanel(
                                                            prettyRadioButtons(inputId = "par.af.ale.1.sgl",
                                                                               label = "Select One Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),   
                                                            sliderInput("iter.af.ale.1.sgl", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1900, loop = FALSE))
                                                          ),
                                                          plotOutput("af.ale.1.sgl", height = "420px", width = "800")
                                                 ),
                                                 tabPanel(title = "All Plots",
                                                          sidebarPanel(
                                                            sliderInput("iter.af.ale.1.all", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE))
                                                          ),
                                                          plotOutput("af.ale.1.all", height = "420px", width = "800")
                                                 )
                                               )
                                      ),
                                      tabPanel(title = "Two Features ALE",
                                               tabsetPanel(
                                                 tabPanel(title = "Single Plot",
                                                          sidebarPanel(
                                                            prettyRadioButtons(inputId = "par.af.ale.2.sgl.1",
                                                                               label = "Select First Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),  
                                                            prettyRadioButtons(inputId = "par.af.ale.2.sgl.2",
                                                                               label = "Select Second Parameter", thick = TRUE,
                                                                               choices = pars, fill = TRUE, inline = TRUE,
                                                                               animation = "pulse", status = "info"),  
                                                            sliderInput("iter.af.ale.2.sgl", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE))
                                                          ),
                                                          plotOutput("af.ale.2.sgl", height = "360px", width = "800")
                                                 ),
                                                 tabPanel(title = "All Plots",
                                                          sidebarPanel(
                                                            sliderInput("iter.af.ale.2.all", "Choose Iteration:",
                                                                        min = 1, max = iter,
                                                                        value = 1, step = 1,
                                                                        animate = animationOptions(interval = 1000, loop = FALSE))
                                                          ),
                                                          plotOutput("af.ale.2.all", height = "420px", width = "800")
                                                 )
                                               )
                                      )
                           ),
#####


# UI Distance to Decision
#####
                           navbarMenu(title = "Proposed Points",
                                      tabPanel(title = "Total Distance",
                                               tabsetPanel(
                                                 tabPanel(title = "Single Plot",
                                                     sidebarPanel(
                                                       prettyRadioButtons(inputId = "par.dec.sgl",
                                                                          label = "Select One Parameter", thick = TRUE,
                                                                          choices = pars, fill = TRUE, inline = TRUE,
                                                                          animation = "pulse", status = "info"),
                                                       sliderInput("iter.dec.sgl", "Choose Iteration:",
                                                                   min = 1, max = iter,
                                                                   value = 1, step = 1,
                                                                   animate =
                                                                     animationOptions(interval = 1700, loop = FALSE)),
                                                       prettySwitch(inputId = "show.prop.sgl",
                                                                    label = "Show Proposed Value",
                                                                    fill = TRUE, status = "primary"),
                                                       prettySwitch(inputId = "stand.sgl",
                                                                    label = "Standardized Data",
                                                                    fill = TRUE, status = "primary")
                                                           ),
                                               plotOutput("af.dec.sgl")
                                            ),
                                            tabPanel(title = "All Plots",
                                                     sidebarPanel(
                                                       sliderInput("iter.dec.all", "Choose Iteration:",
                                                                   min = 1, max = iter,
                                                                   value = 1, step = 1,
                                                                   animate =
                                                                     animationOptions(interval = 1700, loop = FALSE)),
                                                       prettySwitch(inputId = "show.prop.all",
                                                                    label = "Show Proposed Value",
                                                                    fill = TRUE, status = "primary"),
                                                       prettySwitch(inputId = "stand.all",
                                                                    label = "Standardized Data",
                                                                    fill = TRUE, status = "primary")
                                                     ),
                                                     plotOutput("af.dec.all")
                                            )
                                          )  
                                      ),
                                      tabPanel("Selected Dimension excluded (Lars Version)",
                                      tabsetPanel(
                                        tabPanel(title = "Single Plot",
                                                 sidebarPanel(
                                                   prettyRadioButtons(inputId = "par.dec.sgl.wopar",
                                                                      label = "Select One Parameter", thick = TRUE,
                                                                      choices = pars, fill = TRUE, inline = TRUE,
                                                                      animation = "pulse", status = "info"),
                                                   sliderInput("iter.dec.sgl.wopar", "Choose Iteration:",
                                                               min = 1, max = iter,
                                                               value = 1, step = 1,
                                                               animate =
                                                                 animationOptions(interval = 1700, loop = FALSE)),
                                                   prettySwitch(inputId = "show.prop.sgl.wopar",
                                                                label = "Show Proposed Value",
                                                                fill = TRUE, status = "primary"),
                                                   prettySwitch(inputId = "stand.sgl.wopar",
                                                                label = "Standardized Data",
                                                                fill = TRUE, status = "primary")
                                                 ),
                                                 plotOutput("af.dec.wopar.sgl")
                                        ),
                                        tabPanel(title = "All Plots",
                                                 sidebarPanel(
                                                   sliderInput("iter.dec.all.wopar", "Choose Iteration:",
                                                               min = 1, max = iter,
                                                               value = 1, step = 1,
                                                               animate =
                                                                 animationOptions(interval = 1700, loop = FALSE)),
                                                   prettySwitch(inputId = "show.prop.all.wopar",
                                                                label = "Show Proposed Value",
                                                                fill = TRUE, status = "primary"),
                                                   prettySwitch(inputId = "stand.all.wopar",
                                                                label = "Standardized Data",
                                                                fill = TRUE, status = "primary")
                                                 ),
                                                 plotOutput("af.dec.wopar.all")
                                        )
                                      )
                                      
                           )
                 )
#####
      )
)

server <- function(input, output) {
  
  
  ######################
  ##### reactive objects
  ######################
 
  # Distance to Decision, Total Distance
  ######
  
  pdp.af.dec.sgl.plots = reactive({
    # call standardized data frame
    if (input$stand.sgl == TRUE) {
      pdp.af.dec.sgl = as.data.frame(pdp.af.dec.all.stand[[input$iter.dec.sgl]][[input$par.dec.sgl]])
      return(as.data.frame(pdp.af.dec.sgl))
    }
    # or call non-standardized df:
    pdp.af.dec.sgl = as.data.frame(pdp.af.dec.all[[input$iter.dec.sgl]][[input$par.dec.sgl]])
    pdp.af.dec.sgl
  })
  
  pdp.af.dec.all.plots = reactive({
    # call standardized data frame
    if (input$stand.all == TRUE) {
      pdp.af.dec.all.plot = pdp.af.dec.all.stand[[input$iter.dec.all]]
    } else # or call non-standardized data:
    pdp.af.dec.all.dfs = pdp.af.dec.all[[input$iter.dec.all]]

    
    # generate all plots
  dec.plots = list()
    for (param in pars) {
      pdp.af.dec.all.plot = as.data.frame(pdp.af.dec.all.dfs[[param]])
      if (is.numeric(pdp.af.dec.all.plot[[param]])) { 
        # numeric variables  
        plot = ggplot(data = pdp.af.dec.all.plot, aes(x = .data[[param]], y = .data[["distance"]])) +
          geom_line(color = "steelblue", linetype = 1, size = 1.4) +
          geom_rug(sides = "l") +
          labs(title = (paste("tbd"))) +
          xlab(paste("Parameter", param)) +
          ylab("Distance to Proposed Point")
        
        if (input$show.prop.all == TRUE) {
          plot = plot + geom_point(x = pdp.af.dec.all.plot[["prop.par.val"]][1], y = min(pdp.af.dec.all.plot[["distance"]]), 
                                   color = "red", fill = "red", shape = 25, size = 3) 
        }
      } else {
        # categorical variables 
        plot = ggplot(data = pdp.af.dec.all.plot, aes(x = .data[[param]], y = .data[["distance"]])) +
          geom_col(color = "steelblue") +
          labs(title = (paste("tbd"))) +
          xlab(paste("Parameter", param)) +
          ylab("Distance to Proposed Point")
        if (input$show.prop.all == TRUE) {
        plot = plot + geom_point(x = pdp.af.dec.sgl[["prop.par.val"]][1], y = min(pdp.af.dec.sgl[["distance"]]), 
                                   color = "red", fill = "red", shape = 25, size = 3) 
          }
      }
      dec.plots[[param]] = plot 
      }
   dec.plots
  })
  #####
  
  # Distance to Decision, Selected Dim excluded (Lars Version)
  ######
  
  pdp.af.dec.sgl.wopar.plots = reactive({
    # call standardized/non-standardiized data frame
    if (input$stand.sgl == TRUE) {
      pdp.af.dec.sgl = as.data.frame(pdp.af.dec.all.wopar.stand[[input$iter.dec.sgl.wopar]][[input$par.dec.sgl.wopar]])
      return(as.data.frame(pdp.af.dec.sgl))
    }
    # or call non-standardized data:
    pdp.af.dec.sgl = as.data.frame(pdp.af.dec.all.wopar[[input$iter.dec.sgl.wopar]][[input$par.dec.sgl.wopar]])
    pdp.af.dec.sgl
  })
  
  pdp.af.dec.all.wopar.plots = reactive({
    # call standardized/n data frame
    if (input$stand.all == TRUE) {
      pdp.af.dec.all.plot = pdp.af.dec.all.wopar.stand[[input$iter.dec.all.wopar]]
    } else # or call non-standardized data:
      pdp.af.dec.all.dfs = pdp.af.dec.all.wopar[[input$iter.dec.all.wopar]]
    
    
    # generate all plots
    dec.plots = list()
    for (param in pars) {
      pdp.af.dec.all.plot = as.data.frame(pdp.af.dec.all.dfs[[param]])
      if (is.numeric(pdp.af.dec.all.plot[[param]])) { 
        # numeric variables  
        plot = ggplot(data = pdp.af.dec.all.plot, aes(x = .data[[param]], y = .data[["distance"]])) +
          geom_line(color = "steelblue", linetype = 1, size = 1.4) +
          geom_rug(sides = "l") +
          labs(title = (paste("tbd"))) +
          xlab(paste("Parameter", param)) +
          ylab("Distance to Proposed Point")
        
        if (input$show.prop.all.wopar == TRUE) {
          plot = plot + geom_point(x = pdp.af.dec.all.plot[["prop.par.val"]][1], y = min(pdp.af.dec.all.plot[["distance"]]), 
                                   color = "red", fill = "red", shape = 25, size = 3) 
        }
      } else {
        # categorical variables 
        plot = ggplot(data = pdp.af.dec.all.plot, aes(x = .data[[param]], y = .data[["distance"]])) +
          geom_col(color = "steelblue") +
          labs(title = (paste("tbd"))) +
          xlab(paste("Parameter", param)) +
          ylab("Distance to Proposed Point")
        if (input$show.prop.all.wopar == TRUE) {
          plot = plot + geom_point(x = pdp.af.dec.sgl[["prop.par.val"]][1], y = min(pdp.af.dec.sgl[["distance"]]), 
                                   color = "red", fill = "red", shape = 25, size = 3) 
        }
      }
      dec.plots[[param]] = plot 
    }
    dec.plots
  })
  #####
  
  
  # Two-feature PDP/ALE
  ######
  # parameter combination for two features PDP, surrogate model
  par.sm.pdp.2.sgl = reactive({
    if (input$par.sm.pdp.2.sgl.1 == input$par.sm.pdp.2.sgl.2) 
      par.sm.pdp.2.sgl = NULL
    else {
    sel.pars = paste(input$par.sm.pdp.2.sgl.1, input$par.sm.pdp.2.sgl.2, sep = "_")
    par.sm.pdp.2.sgl = switch(sel.pars,
                              "power_time" = "power_time",
                              "time_power" = "power_time",
                              "power_pressure" = "power_pressure",
                              "pressure_power" = "power_pressure",
                              "gas_power" = "gas_power",
                              "power_gas" = "gas_power",
                              "gas_pressure" = "gas_pressure",
                              "pressure_gas" = "gas_pressure",
                              "time_pressure" = "time_pressure",
                              "pressure_time" = "time_pressure", 
                              "gas_time" = "gas_time",
                              "time_gas" = "gas_time")
    }
    par.sm.pdp.2.sgl
    
  })

  # parameter combination for second-order ALE, surrogate model
  par.sm.ale.2.sgl = reactive({
    if (input$par.sm.ale.2.sgl.1 == input$par.sm.ale.2.sgl.2) 
      par.sm.ale.2.sgl = NULL
    else {
      sel.pars = paste(input$par.sm.ale.2.sgl.1, input$par.sm.ale.2.sgl.2, sep = "_")
      par.sm.ale.2.sgl = switch(sel.pars,
                                "power_time" = "power_time",
                                "time_power" = "power_time",
                                "power_pressure" = "power_pressure",
                                "pressure_power" = "power_pressure",
                                "gas_power" = "gas_power",
                                "power_gas" = "gas_power",
                                "gas_pressure" = "gas_pressure",
                                "pressure_gas" = "gas_pressure",
                                "time_pressure" = "time_pressure",
                                "pressure_time" = "time_pressure", 
                                "gas_time" = "gas_time",
                                "time_gas" = "gas_time")
    }
    par.sm.ale.2.sgl
    
  })
  
  
  # parameter combination for two features PDP, Acquistion Function
  par.af.pdp.2.sgl = reactive({
    if (input$par.af.pdp.2.sgl.1 == input$par.af.pdp.2.sgl.2) 
      par.af.pdp.2.sgl = NULL
    else {
      sel.pars = paste(input$par.af.pdp.2.sgl.1, input$par.af.pdp.2.sgl.2, sep = "_")
      par.af.pdp.2.sgl = switch(sel.pars,
                                "power_time" = "power_time",
                                "time_power" = "power_time",
                                "power_pressure" = "power_pressure",
                                "pressure_power" = "power_pressure",
                                "gas_power" = "gas_power",
                                "power_gas" = "gas_power",
                                "gas_pressure" = "gas_pressure",
                                "pressure_gas" = "gas_pressure",
                                "time_pressure" = "time_pressure",
                                "pressure_time" = "time_pressure", 
                                "gas_time" = "gas_time",
                                "time_gas" = "gas_time")
    }
    par.af.pdp.2.sgl
    
  })
  
  
  # parameter combination for second-order ALE, acquisition function
  par.af.ale.2.sgl = reactive({
    if (input$par.af.ale.2.sgl.1 == input$par.af.ale.2.sgl.2) 
      par.af.ale.2.sgl = NULL
    else {
      sel.pars = paste(input$par.af.ale.2.sgl.1, input$par.af.ale.2.sgl.2, sep = "_")
      par.af.ale.2.sgl = switch(sel.pars,
                                "power_time" = "power_time",
                                "time_power" = "power_time",
                                "power_pressure" = "power_pressure",
                                "pressure_power" = "power_pressure",
                                "gas_power" = "gas_power",
                                "power_gas" = "gas_power",
                                "gas_pressure" = "gas_pressure",
                                "pressure_gas" = "gas_pressure",
                                "time_pressure" = "time_pressure",
                                "pressure_time" = "time_pressure", 
                                "gas_time" = "gas_time",
                                "time_gas" = "gas_time")
    }
    par.af.ale.2.sgl
    
  })
  #######  
  
  #####################
  ##### plots #########
  #####################
  
# Surrogate Model  
##### 
  
  
  # surrogate, pdp, 1st order, single
  output$sm.pdp.1.sgl = renderPlot({
   env = fe.kapton$surrogate$pdp$first[[input$par.sm.pdp.1.sgl]][[input$iter.sm.pdp.1.sgl]]
    p1 = env$plot()
    gridExtra::grid.arrange(p1)
  }, width = "auto", height = "auto")

  # surrogate, pdp, 1st order, all
  output$sm.pdp.1.all = renderPlot({
    p1 = plot(fe.kapton$surrogate$pdp$first[["pressure"]][[input$iter.sm.pdp.1.all]])
    p2 = plot(fe.kapton$surrogate$pdp$first[["power"]][[input$iter.sm.pdp.1.all]])
    p3 = plot(fe.kapton$surrogate$pdp$first[["time"]][[input$iter.sm.pdp.1.all]])
    p4 = plot(fe.kapton$surrogate$pdp$first[["gas"]][[input$iter.sm.pdp.1.all]])
    gridExtra::grid.arrange(p1,p2,p3,p4)
  }, width = "auto", height = "auto")
  
  
  
  # surrogate, pdp, 2nd order, single
  output$sm.pdp.2.sgl = renderPlot({
    if (input$par.sm.pdp.2.sgl.1 == input$par.sm.pdp.2.sgl.2) {
      env = fe.kapton$surrogate$pdp$first[[input$par.sm.pdp.2.sgl.1]][[input$iter.sm.pdp.2.sgl]]
      p1 = env$plot()
    } else {
    env = fe.kapton$surrogate$pdp$second[[par.sm.pdp.2.sgl()]][[input$iter.sm.pdp.2.sgl]]
    p1 = env$plot()
    }
    gridExtra::grid.arrange(p1)
  }, width = "auto", height = "auto")
  
  # surrogate, pdp, 2nd order, all
  output$sm.pdp.2.all = renderPlot({
    so.names = c("power_time", "power_pressure", "gas_power", "time_pressure", "gas_time",
                 "gas_pressure")
    all.plots = list()
    all.plots = lapply(so.names, function(x)
      plot(fe.kapton$surrogate$pdp$second[[x]][[input$iter.sm.pdp.1.all]])
      )
    gridExtra::grid.arrange(grobs = all.plots)
  }, width = "auto", height = "auto")
  

  # surrogate, ale, 1st order, single
  output$sm.ale.1.sgl = renderPlot({
    env = fe.kapton$surrogate$ale$first[[input$par.sm.ale.1.sgl]][[input$iter.sm.ale.1.sgl]]
    p1 = env$plot()
    gridExtra::grid.arrange(p1)
  }, width = "auto", height = "auto")
  
  # surrogate, ale, 1st order, all
  output$sm.ale.1.all = renderPlot({
    p1 = plot(fe.kapton$surrogate$ale$first[["pressure"]][[input$iter.sm.ale.1.all]])
    p2 = plot(fe.kapton$surrogate$ale$first[["power"]][[input$iter.sm.ale.1.all]])
    p3 = plot(fe.kapton$surrogate$ale$first[["time"]][[input$iter.sm.ale.1.all]])
    p4 = plot(fe.kapton$surrogate$ale$first[["gas"]][[input$iter.sm.ale.1.all]])
    gridExtra::grid.arrange(p1,p2,p3,p4)
  }, width = "auto", height = "auto")
  
  # surrogate, ale, 2nd order, single
  output$sm.ale.2.sgl = renderPlot({
    if (input$par.sm.ale.2.sgl.1 == input$par.sm.ale.2.sgl.2) {
      env = fe.kapton$surrogate$ale$first[[input$par.sm.ale.2.sgl.1]][[input$iter.sm.ale.2.sgl]]
      p1 = env$plot()
    } else {
      env = fe.kapton$surrogate$ale$second[[par.sm.ale.2.sgl()]][[input$iter.sm.ale.2.sgl]]
      p1 = env$plot()
    }
    gridExtra::grid.arrange(p1)
  }, width = "auto", height = "auto")
  
  # surrogate, ale, 2nd order, all
  output$sm.ale.2.all = renderPlot({
    so.names = c("power_time", "power_pressure", "gas_power", "time_pressure", "gas_time",
                 "gas_pressure")
    all.plots = list()
    all.plots = lapply(so.names, function(x)
      plot(fe.kapton$surrogate$ale$second[[x]][[input$iter.sm.ale.1.all]])
    )
    gridExtra::grid.arrange(grobs = all.plots)
  }, width = "auto", height = "auto")
  
#####

  
# Acquistion Function    
#####
  
  
  # af, pdp, 1st order, single
  output$af.pdp.1.sgl = renderPlot({
    env = fe.kapton$af$pdp$first[[input$par.af.pdp.1.sgl]][[input$iter.af.pdp.1.sgl]]
    p1 = env$plot()
    gridExtra::grid.arrange(p1)
  }, width = "auto", height = "auto")
  
  # af, pdp, 1st order, all
  output$af.pdp.1.all = renderPlot({
    p1 = plot(fe.kapton$af$pdp$first[["pressure"]][[input$iter.af.pdp.1.all]])
    p2 = plot(fe.kapton$af$pdp$first[["power"]][[input$iter.af.pdp.1.all]])
    p3 = plot(fe.kapton$af$pdp$first[["time"]][[input$iter.af.pdp.1.all]])
    p4 = plot(fe.kapton$af$pdp$first[["gas"]][[input$iter.af.pdp.1.all]])
    gridExtra::grid.arrange(p1,p2,p3,p4)
  }, width = "auto", height = "auto")
  
  
  
  # af, pdp, 2nd order, single
  output$af.pdp.2.sgl = renderPlot({
    if (input$par.af.pdp.2.sgl.1 == input$par.af.pdp.2.sgl.2) {
      env = fe.kapton$af$pdp$first[[input$par.af.pdp.2.sgl.1]][[input$iter.af.pdp.2.sgl]]
      p1 = env$plot()
    } else {
      env = fe.kapton$af$pdp$second[[par.af.pdp.2.sgl()]][[input$iter.af.pdp.2.sgl]]
      p1 = env$plot()
    }
    gridExtra::grid.arrange(p1)
  }, width = "auto", height = "auto")
  
  # af, pdp, 2nd order, all
  output$af.pdp.2.all = renderPlot({
    so.names = c("power_time", "power_pressure", "gas_power", "time_pressure", "gas_time",
                 "gas_pressure")
    all.plots = list()
    all.plots = lapply(so.names, function(x)
      plot(fe.kapton$af$pdp$second[[x]][[input$iter.af.pdp.1.all]])
    )
    gridExtra::grid.arrange(grobs = all.plots)
  }, width = "auto", height = "auto")
  
  
  # af, ale, 1st order, single
  output$af.ale.1.sgl = renderPlot({
    env = fe.kapton$af$ale$first[[input$par.af.ale.1.sgl]][[input$iter.af.ale.1.sgl]]
    p1 = env$plot()
    gridExtra::grid.arrange(p1)
  }, width = "auto", height = "auto")
  
  # af, ale, 1st order, all
  output$af.ale.1.all = renderPlot({
    p1 = plot(fe.kapton$af$ale$first[["pressure"]][[input$iter.af.ale.1.all]])
    p2 = plot(fe.kapton$af$ale$first[["power"]][[input$iter.af.ale.1.all]])
    p3 = plot(fe.kapton$af$ale$first[["time"]][[input$iter.af.ale.1.all]])
    p4 = plot(fe.kapton$af$ale$first[["gas"]][[input$iter.af.ale.1.all]])
    gridExtra::grid.arrange(p1,p2,p3,p4)
  }, width = "auto", height = "auto")
  
  # af, ale, 2nd order, single
  output$af.ale.2.sgl = renderPlot({
    if (input$par.af.ale.2.sgl.1 == input$par.af.ale.2.sgl.2) {
      env = fe.kapton$af$ale$first[[input$par.af.ale.2.sgl.1]][[input$iter.af.ale.2.sgl]]
      p1 = env$plot()
    } else {
      env = fe.kapton$af$ale$second[[par.af.ale.2.sgl()]][[input$iter.af.ale.2.sgl]]
      p1 = env$plot()
    }
    gridExtra::grid.arrange(p1)
  }, width = "auto", height = "auto")
  
  # af, ale, 2nd order, all
  output$af.ale.2.all = renderPlot({
    so.names = c("power_time", "power_pressure", "gas_power", "time_pressure", "gas_time",
                 "gas_pressure")
    all.plots = list()
    all.plots = lapply(so.names, function(x)
      plot(fe.kapton$af$ale$second[[x]][[input$iter.af.ale.1.all]])
    )
    gridExtra::grid.arrange(grobs = all.plots)
  }, width = "auto", height = "auto")
  

#####  

# Distance to Decision
#####
  
  # Total Distance
  #####
   output$af.dec.sgl = renderPlot({
     pdp.af.dec.sgl = pdp.af.dec.sgl.plots()
     param = input$par.dec.sgl
    if (is.numeric(pdp.af.dec.sgl[[param]])) { 
    # numeric variables  
    plot = ggplot(data = pdp.af.dec.sgl, aes(x = .data[[param]], y = .data[["distance"]])) +
      geom_line(color = "steelblue", linetype = 1, size = 1.4) +
      geom_rug(sides = "l") +
      labs(title = (paste("tbd"))) +
      xlab(paste("Parameter", param)) +
      ylab("Distance to Proposed Point")
    
    if (input$show.prop.sgl == TRUE) {
      plot = plot + geom_point(x = pdp.af.dec.sgl[["prop.par.val"]][1], y = min(pdp.af.dec.sgl[["distance"]]), 
                               color = "red", fill = "red", shape = 25, size = 3) 
      }
    } else 
    # categorical variables 
      plot = ggplot(data = pdp.af.dec.sgl, aes(x = .data[[param]], y = .data[["distance"]])) +
        geom_col(color = "steelblue") +
        labs(title = (paste("tbd"))) +
        xlab(paste("Parameter", param)) +
        ylab("Distance to Proposed Point")
    
    if (input$show.prop.sgl == TRUE) {
      plot = plot + geom_point(x = pdp.af.dec.sgl[["prop.par.val"]][1], y = min(pdp.af.dec.sgl[["distance"]]), 
                               color = "red", fill = "red", shape = 25, size = 3) 
    }  
    
    plot
  }, width = "auto", height = "auto")
  
  
  
  output$af.dec.all = renderPlot({
    plots = gridExtra::arrangeGrob(grobs = pdp.af.dec.all.plots())
    plot(plots)
  }, width = "auto", height = "auto")  
  #####
  
  # Distance without selected Dim (Lars Version)
  #####
  output$af.dec.wopar.sgl = renderPlot({
    pdp.af.dec.sgl = pdp.af.dec.sgl.wopar.plots()
    param = input$par.dec.sgl.wopar
    if (is.numeric(pdp.af.dec.sgl[[param]])) { 
      # numeric variables  
      plot = ggplot(data = pdp.af.dec.sgl, aes(x = .data[[param]], y = .data[["distance"]])) +
        geom_line(color = "steelblue", linetype = 1, size = 1.4) +
        geom_rug(sides = "l") +
        labs(title = (paste("tbd"))) +
        xlab(paste("Parameter", param)) +
        ylab("Distance to Proposed Point")
      
      if (input$show.prop.sgl.wopar == TRUE) {
        plot = plot + geom_point(x = pdp.af.dec.sgl[["prop.par.val"]][1], y = min(pdp.af.dec.sgl[["distance"]]), 
                                 color = "red", fill = "red", shape = 25, size = 3) 
      }
    } else 
      # categorical variables 
      plot = ggplot(data = pdp.af.dec.sgl, aes(x = .data[[param]], y = .data[["distance"]])) +
      geom_col(color = "steelblue") +
      labs(title = (paste("tbd"))) +
      xlab(paste("Parameter", param)) +
      ylab("Distance to Proposed Point")
    
    if (input$show.prop.sgl == TRUE) {
      plot = plot + geom_point(x = pdp.af.dec.sgl[["prop.par.val"]][1], y = min(pdp.af.dec.sgl[["distance"]]), 
                               color = "red", fill = "red", shape = 25, size = 3) 
    }  
    
    plot
  }, width = "auto", height = "auto")
  
  
  
  output$af.dec.wopar.all = renderPlot({
    plots = gridExtra::arrangeGrob(grobs = pdp.af.dec.all.wopar.plots())
    plot(plots)
  }, width = "auto", height = "auto")  
  #####
  
#####
  
  
  
  
  
}


shinyApp(ui = ui, server = server)


