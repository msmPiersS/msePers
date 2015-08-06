###############################################################################
## Interactive Shiny based app for exploring simple in session 
## link personalisation for MSE
## shiny server : server.r
##
##  Simple link personalisation approach - requires no actual knoweldge of individual
##  Generated purely from raw clickstream data (in this case one day - 4th July 2015)
##
##  Process:
##  Create training data set - in this case one day of clickstream excluding forums
##  Fields required- sessionid pagename and sequence (optional)
##  Generate a lookup table for sessions and pages, 
##  then a clean table of sessionids and pageids for fast access
##
##  Create a nearest neighbour function - 
##  given a set of input pages find the closest k sessions
##  (uses simple process- categorise sesssion by number of overlap pages and pick top k
##  use overall activity for tie breaking)
##
##  Create a page recommendatio function - 
##  given a set of input pages call nearest neighbour function to find closest k sessions
##  then pull pages from those session, picking top n based on overlap and then popularity
##  
##
## ps Aug 2015
###############################################################################

#############################
## todo
## input checking of functions
##
#############################



###############################################################################
## setup

  library(data.table)
  library(ggplot2)
  library(RColorBrewer)
  library(shiny)

  #setwd("C:/Piers/git/r_abm")
  options(shiny.trace=FALSE) # set to True to debug
  options(shiny.maxRequestSize=100*1024^2) # set max input file size to 100MB

  #load functions
  #source("genROC.r")
  #source("genMSE.r")  

## end setup
###############################################################################


###############################################################################
## initiate functions

    #function to pull k nearest sessions
    getKNearest <- function(data, pages, k) {
      # function to pull the k closest sessions to the input
      # data: data.table containing the clean data- two columns, 
      # user(uId) and page(pId)- should be keyed by pId then uId
      # pages: list of pages (pId) defining input session
      # k: number of nearest neighbours to pull
      
      # pages = c(1,3)
      # data = copy(cleanDedup)
      # k = 5
      
      # check for key and keyby if not
      if (key(data)[1] == "pId" && key(data)[2] == "uId") {
        setkeyv(data, c("pId", "uId"))
      }
      
      # characterise sessions by number of pages in comman
      # filter to only those sessions that include the input pages
      # count number of overlap pages by session
      # also include number of hits across those pages (including repeats)
      # order resulting sessions by overlap and the by total hits- include top k
      
      targetList = data[pId %in% pages, 
                        list(samePages = .N, totalHits = sum(hits), overlap = .N/length(pages)), 
                        by = list(uId)][order(-samePages, -totalHits)]
      return(targetList[1:k, list(uId, overlap, totalHits)])
      
    }
  

    #function to pull N best recommendations
    getNRecs <- function(data, pages, pLookup, n, k) {
      # function to pull the n best recommended pages for the input
      # data: data.table containing the clean data- two columns, 
      # user(uId) and page(pId)- should be keyed by pId then uId
      # pages: list of pages (pId) defining input session
      # pLookup: list of all pages with summary info- keyed by pId
      # n: number of recommended pages to pull
      # k: number of nearest neighbours to use
      
      # pages = c(1,3)
      # data = copy(cleanDedup)
      # pLookup = copy(pageLookup)
      # k = 5
      # n = 3
      
      # check for key and keyby if not
      if (key(pLookup)[1] == "pId") {
        setkeyv(pLookup, c("pId"))
      }
      
      # first get k nearest neighbours
      nnUids = getKNearest(data, pages, k)
      
      # pull all pages, with counts by session
      # filter to only those pages visited in nn sessions
      # count number of overlap nn sessions by page
      # also generate total hits in those sessions by page, as well as overlap of sessions
      
      targetPages = data[uId %in% nnUids[, uId], 
                         list(sameSessions = .N, sameHits = sum(hits), overlap = .N/nrow(nnUids)), 
                         by = list(pId)][order(-sameSessions, -sameHits)]
      
      # remove any pages already visited
      targetPages = targetPages[!(pId %in% pages), ]
      
      # add in total page activity and remove any exclusions
      setkey(targetPages, pId)
      targetPages = pLookup[targetPages][exclude==0, list(pId, overlap, sameHits, totHits, pagePath)][order(-overlap, -sameHits, -totHits)]
      
      return(targetPages[1:n, ])
      
    }
  
  

## end initiate functions
###############################################################################



###############################################################################
## recursive section

shinyServer(function(input, output, session) {
 
  #do everything in an observer
  obs = observe({
      
    #render page in iframe function
#     getPage<-function() {
#       return(tags$iframe(src = "http://www.bbc.co.uk"
#                          , style="width:100%;",  frameborder="0"
#                          ,id="iframe"
#                          , height = "500px"))
#     }
#     
    getPage<-function(tgt) {
      return(tags$iframe(src = tgt
                         , style="width:100%;",  frameborder="0"
                         ,id="iframe"
                         , height = "500px"))
    }
    
    #generate page
    tgtUrl = 'http://www.moneysavingexpert.com/credit-cards/'
    
    output$inc<-renderUI({
      x <- tgtUrl 
      getPage(tgtUrl)
    })
    
    #setup inputs
    nTypesIn = reactive({
            if (is.null(input$nTypes)) {
              nTypes = 2
            } else {
              nTypes = input$nTypes
            }
            return(nTypes)
          })
    
    emptyPIn = reactive({
      if (is.null(input$emptyP)) {
        emptyP = 0.2
      } else {
        emptyP = input$emptyP
      }
      return(emptyP)
    })
    
    simThreshIn = reactive({
      if (is.null(input$simThresh)) {
        simThresh = 0.5
      } else {
        simThresh = input$simThresh
      }
      return(simThresh)
    })

    
    nItersIn = reactive({
      if (is.null(input$nIters)) {
        nIters = 20
      } else {
        nIters = input$nIters
      }
      return(nIters)
    })    
    
    
    set.seed(1234)
    
    data <- reactiveValues()
    plots <- reactiveValues()
    #data=list()
    #plots = list()
    
      
    # initiate shelling
    #data$gridInit = initiateShelling(dimensions = c(30, 30), n_types = nTypesIn(), perc_empty = emptyPIn())
    #data$gridInit = initiateShelling(dimensions = c(30, 30), n_types = 2, perc_empty = 0.2)
    #data$gridInit2 = data$gridInit
    # plot shelling
    #plots$gridInit = plotShelling(data$gridInit, title = "Model after 0 epochs")
    
    output$startPlot = renderPlot({
      print(plots$gridInit)
    })
      
# 
#     if (input$runSim==0) {
#       return
#     } else {
#       isolate({
#         withProgress(message = 'Calculating iterations', 
#                      detail = 'This may take a while...', value = 0, {
#           ## iterate n/2 times
#           data$gridM <- iterate(shelling = data$gridInit, n = nItersIn()/2, similiarity_threshold = simThreshIn())
#           ## plot the result after 10 iterations
#           plots$gridM <- plotShelling(shelling = data$gridM, title = paste("Model after ",nItersIn()/2," epochs", sep=""))
#         
#           output$midPlot = renderPlot({ 
#             print(plots$gridM)
#           })
#           
#           incProgress(1/2)
#           
#           ## iterate another n/2 times
#           data$gridF <- iterate(data$gridM, n = nItersIn()/2, similiarity_threshold = simThreshIn())
#           ## plot again after 20 iterations total
#           plots$gridF <- plotShelling(shelling = data$gridF, title = paste("Model after ",nItersIn()," epochs", sep=""))
#           
#           incProgress(19/20)
#           
#           output$endPlot = renderPlot({
#             print(plots$gridF)
#           })
#         }) #end progress
#         
#       }) #end isolate
#       
#     } #end action button
    
  }) #end observe
}) #end

  



## end recursive section
###############################################################################
