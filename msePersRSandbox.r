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
  
  #libraries
  library(data.table)
  library(bit64)
  
  
## End Set up
#######################################################################################




#######################################################################################
## Load data and explore

  raw = fread(paste(dataDir, dataFile, sep=""))
  str(raw)

  #create dicts
  pageLookup = raw[, list(tot = .N), by=list(pagePath)][order(-tot)]
  pageLookup[, pId:= seq(1:nrow(pageLookup))]
  setkey(pageLookup, pagePath)
  
  uidLookup = raw[, list(tot = .N), by=list(uid)][order(-tot)]
  uidLookup[, uId:= seq(1:nrow(uidLookup))]
  setkey(uidLookup, uid)
  
  fidLookup = raw[, list(tot = .N), by=list(fid)][order(-tot)]
  fidLookup[, fId:= seq(1:nrow(fidLookup))]
  setkey(fidLookup, fid)
  
  vidLookup = raw[, list(tot = .N), by=list(vid)][order(-tot)]
  vidLookup[, vId:= seq(1:nrow(vidLookup))]
  setkey(vidLookup, vid)
  
  uidLookup[tot>3,] 
  
  #create clean links
  setkey(raw,pagePath)
  clean = raw[pageLookup][, list(uid, pId)]
  setkey(clean, uid)
  clean = clean[uidLookup][, list(uId,pId)]
  setkey(clean, uId)

## End Load data and explore
#######################################################################################
  
  

  
  
###########################################################  
## End Look into collaborative filtering
#http://blog.yhathq.com/posts/recommender-system-in-r.html    
    

  common_sessions_by_id <- function(pid1, pid2) {
    #pid1 = 1
    #pid2 = 3
    sessions1 <- unique(subset(clean, pId==pid1)[order(uId)][, uId])
    sessions2 <- unique(subset(clean, pId==pid2)[order(uId)][, uId])
    
    sessions_sameset <- intersect(sessions1, sessions2)
    if (length(sessions_sameset)==0) {
      NA
    } else {
      sessions_sameset
    }
  }
  
  common_sessions_by_id(1,4)

  common_pages_by_id <- function(uid1, uid2) {
    #uid1 = 2
    #uid2 = 4
    pages1 <- unique(subset(clean, uId==uid1)[order(pId)][, pId])
    pages2 <- unique(subset(clean, uId==uid2)[order(pId)][, pId])
    
    pages_sameset <- intersect(pages1, pages2)
    if (length(pages_sameset)==0) {
      NA
    } else {
      pages_sameset
    }
  }
  
  common_pages_by_id(2,4)
      
  
## End Look into collaborative filtering
###########################################################  
  
