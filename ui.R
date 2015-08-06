###############################################################################
## Interactive Shiny based app for exploring simple in session 
## link personalisation for MSE
## front end : ui.r
##
## See server.R code for more info
##  
## ps August 2015
###############################################################################

###############################################################################
## todo
## see server.R
## 
###############################################################################



###############################################################################
## setup

library(shiny)

## end setup
###############################################################################


###############################################################################
## UI

shinyUI(
  # tab style UI
  navbarPage("Simple link personalisation model for MSE",  
            inverse = TRUE,
            header = 
              tags$head(
                tags$link(rel = 'stylesheet', type = 'text/css', href = 'http://fonts.googleapis.com/css?family=Yanone+Kaffeesatz:400,700'),
                tags$link(rel = 'stylesheet', type = 'text/css', href = 'http://images2.moneysavingexpert.com/stylesheets/mb_homepage_css/homepage_main.1438782489.css')
              ),
  
  # First and only Tab
  tabPanel("",
           
    # Application title again
    titlePanel(
      HTML(
        '<div id="stats_header">
        Simple link personalisation model for MSE based on collaborative filtering of sessions
        <a href="http://www.moneysavingsexpert.com" target="_blank">
        <img id="stats_logo" align="right" alt="SNAP Logo" src="dataMsmLogoF_white_small.png" />
        </a>
        </div>'
      )
    ),
    
    div("Simple approach to personalised link recommendations", style = "font-size:140%"),
    div("Does not require any personal information- simply done by comparing current pages in sesion to sessions of others", style = "font-size:120%"),
    div("First find those sessions that are closest to your session so far", style = "font-size:120%"),
    div("Then explore pages viewed in those nearest sessions and filter out any pages already viewed in your session", style = "font-size:120%"),
    br(),
  
    fluidPage(  
        h5(' Enter Parameters '),
        fluidRow(
          column(4,
                 sliderInput("nTypes", "Enter number of different agent types:", 
                      min=2, max=10, value=2)
          ),
          column(4,
                 sliderInput("emptyP", "Enter proportion of empty houses:", 
                             min=0.1, max=0.5, value=0.2, step= 0.1)
          ),
          column(4,
                sliderInput("simThresh", "Enter similarity threshold:", 
                      min=0.1, max=0.9, value=0.5, step= 0.1)
          )
          #helpText(htmlOutput("lbHelp")), 
        ),
        br(),
        hr(),
        h5(' Actual page visited '),
        sliderInput("nIters", "Enter number of epochs to run:", 
                    min=10, max=50, value=20, step= 2),
        actionButton("runSim", "Run Agent Based Model"),
        helpText("Note - this may take a minute or so to run"),
        fluidRow(
          column(10,
                 htmlOutput("inc")
          )
        ),
        hr()
        #helpText(htmlOutput("timeHelp")),
    )
  )
))


## end UI
###############################################################################
