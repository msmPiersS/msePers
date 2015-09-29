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
##  Create a page recommendation function - 
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
  #library(ggplot2)
  #library(RColorBrewer)
  #library(shiny)

  #setwd("C:/Piers/git/r_abm")
  options(shiny.trace=FALSE) # set to True to debug
  #options(shiny.error=browser) # set error to browser
  options(shiny.maxRequestSize=100*1024^2) # set max input file size to 100MB

  #load data
  load('msePersData.rdata')
  
  #sort sessions by length
  excludeIds = c(2,4,5,8,10,13)
  testIds = testIds[!excludeIds, ]
  
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
  
    # funciton to pull url and display in iframe
    getPage<-function(tgt) {
      #return(tags$iframe(src = tgt
      #                   , style="width:100%;",  frameborder="0"
      #                   ,id="iframe"
      #                   , height = "500px"))
      return(tags$iframe(src = tgt
                         , align = "top"
                         , width = "1024px"
                         , frameborder="0"
                         , id="iframe"
                         , height = "700px"
                         , style="-webkit-transform:scale(0.55);-moz-transform-scale(0.55);"))
    }
    
    

## end initiate functions
###############################################################################



###############################################################################
## recursive section

shinyServer(function(input, output, session) {
 
  #do everything in an observer
  obs = observe({

      
        
      
      #get target session from input
      tgtIdClean <- reactive({
        if (is.null(input$tgtId))
          1
        else
          input$tgtId
      })
      

        
      #get target sequence id in session from input
      seqIdClean <- reactive({
        if (is.null(input$seqId))
          1
        else
          input$seqId
      })
      

      
      tgtPages = testClean[uid == testIds[tgtIdClean(), uid], pId]
      sessionLength = length(tgtPages)
      
      isolate ({
      output$seqInput <- renderUI({
        sliderInput("seqId", "Sequence Id:", seqIdClean(), min = 1, max = sessionLength, step=1, ticks = FALSE, 
                    animate = animationOptions(loop = FALSE, interval = 500))
      })
      
      }) # end isolate  
      
      
      #get target number of recommendations from inpu
      nRecClean <- reactive({
        if (is.null(input$nRec))
          1
        else
          input$nRec
      }) 
      

    
    #load pages and recommendations
    i=seqIdClean()
    tgtUrl = paste("http://",pageLookup[pId %in% tgtPages[i], pagePath], sep="")
    output$currentPage<-renderUI({tgtUrl})
    output$pageView<-renderUI({
      x <- tgtUrl 
      getPage(tgtUrl)
    })
    
    inPages = tgtPages[1:i]
    inPageNames = pageLookup[pId %in% inPages, pagePath]
    recs = getNRecs(cleanDedup, inPages, pageLookup, nRecClean(), 10)
    
    output$recPages<-renderUI({ paste(recs[, pagePath], sep=" ", collapse = "\n") })
    output$topPages<-renderUI({ paste(pageLookup[!(pId %in% unique(c(inPages, recs[, pId]))), ] [1:nRecClean(), pagePath], sep=" ", collapse = "\n") })
    output$currentPages<-renderUI({ paste(inPageNames, sep=" ", collapse = "\n") })
    
      
  }) #end observe
}) #end

  



## end recursive section
###############################################################################
