#######################################################################################
## Quick look at MSE clickstream
##
## msmPiersS - Aug 2015
#######################################################################################


#######################################################################################
## Set up

  #locations
  homeDir = '/Users/piers.stobbs/Documents/piers/datascience/mse/clickstream/msePers/'
  dataDir = '/Users/piers.stobbs/Documents/piers/datascience/mse/clickstream/msePers/'
  setwd(homeDir)
  getwd()

  #files
  dataFile = 'results-20150729-175027.csv'
  exclusionList = 'pageExclusions.csv'
  
  
  #libraries
  library(data.table)
  library(bit64)
  
  
  
## End Set up
#######################################################################################




#######################################################################################
## Load data and explore

  #load raw data
  raw = fread(paste(dataDir, dataFile, sep=""))
  pExclude = fread(paste(dataDir, exclusionList, sep=""))
  setkey(pExclude, pagePath)
  str(raw)

  #simulate real sitution- split raw data into training and set to make predictions on
  # first pull sessions with start time- use 75% for training
  trainPct = 0.75
  sessionsTime = unique(raw[, uid, visitStartTime])[order(visitStartTime)]
  trainSessions = sessionsTime[1:floor(trainPct*nrow(sessionsTime)), uid] 
  
  train = raw[ uid %in% trainSessions, ]
  test = raw[ !(uid %in% trainSessions), ]
  
  #create dicts
  pageLookup = train[, list(totHits = .N), by=list(pagePath)][order(-totHits)]
  pageLookup[, pId:= seq(1:nrow(pageLookup))]
  setkey(pageLookup, pagePath)
  pageLookup = pExclude[pageLookup]
  pageLookup[is.na(exclude), exclude:=0]
  
  uidLookup = train[, list(totPages = .N), by=list(uid)][order(-totPages)]
  uidLookup[, uId:= seq(1:nrow(uidLookup))]
  setkey(uidLookup, uid)
  
  fidLookup = train[, list(tot = .N), by=list(fid)][order(-tot)]
  fidLookup[, fId:= seq(1:nrow(fidLookup))]
  setkey(fidLookup, fid)
  
  vidLookup = train[, list(tot = .N), by=list(vid)][order(-tot)]
  vidLookup[, vId:= seq(1:nrow(vidLookup))]
  setkey(vidLookup, vid)
  
  uidLookup[totPages>3,] 
  
  #create clean links
  setkey(train,pagePath)
  clean = train[pageLookup][, list(uid, pId)]
  setkey(clean, uid)
  clean = clean[uidLookup][, list(uId,pId)]
  setkey(clean, uId, pId)

  cleanDedup = clean[, list(hits = .N), by= list(uId, pId)]
  setkey(cleanDedup, pId, uId)
  setkey(pageLookup, pId)
  
## End Load data and explore
#######################################################################################
  
  

  
  
###########################################################  
## Look into simple collaborative filtering

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
  
  
  
  
  inPages = c(1,2,3,4,5)
  getKNearest(cleanDedup, inPages, 5)
  
  recIds = getNRecs(cleanDedup, inPages, pageLookup, 3, 10)
  
  
  pagesViewed = pageLookup[pId %in% inPages]
  pageRecs = pageLookup[pId %in% recIds[, pId]]
  
  
  
  
      
  
## End Look into collaborative filtering
###########################################################  
  
