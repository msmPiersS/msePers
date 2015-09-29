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
        <img id="stats_logo" align="right" alt="SNAP Logo" src="http://www.googledrive.com/host/0B8-urh68sOqfc0xieXVnNDBTdDQ" />
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
        fluidRow(
          column(8,
                 h5(' Actual Page Being Visited '),
                 htmlOutput("currentPage"),
                 htmlOutput("pageView")
          ),
          column(4,
                 fluidRow(
                   column(6,
                          h5(' Select Session to use '),
                          sliderInput("tgtId", "Session Id:", 1, min = 1, max = 200, step = 1, ticks = FALSE)),
                   column(6,
                          h5(' Cycle through session '),
                          uiOutput("seqInput"))
                 ),
                 #numericInput("seqId", "Sequence Id:", 1, min = 1, max = 10),
                 h5(' Number of reommended pages to display '),
                 sliderInput("nRec", "Number of recs:", 3, min = 1, max = 10, ticks = FALSE),
                 br(),
                 h5(' Pages you might like '),
                 htmlOutput("recPages"),
                 br(),
                 h5(' Most popular pages '),
                 htmlOutput("topPages"),
                 br(),
                 h5(' Pages you have visited '),
                 htmlOutput("currentPages")
          ) 
        ),
        br(),
        hr()
        #helpText(htmlOutput("timeHelp")),
    )
  )
))


## end UI
###############################################################################
