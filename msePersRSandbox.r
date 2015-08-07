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
  visDir = '/Users/piers.stobbs/Documents/piers/datascience/mse/clickstream/msePers/2dvis/'
  setwd(homeDir)
  getwd()

  #files
  #dataFile = 'results-20150729-175027.csv'
  dataFile = 'AnalyticsData_20150704.csv'
  exclusionList = 'pageExclusions.csv'
  
  
  #libraries
  library(data.table)
  library(bit64)
  library(fpc)
  library(igraph)
  library(ggplot2)  
  library(plyr)
  library(jsonlite)
  library(RColorBrewer)
  
  
## End Set up
#######################################################################################




#######################################################################################
## Load data and explore

  #load raw data
  raw = fread(paste(dataDir, dataFile, sep=""))
  pExclude = fread(paste(dataDir, exclusionList, sep=""))
  setkey(pExclude, pagePath)
  str(raw)

  #create full dicts
  pageLookupAll = raw[, list(totHits = .N), by=list(pagePath)][order(-totHits)]
  pageLookupAll[, pId:= seq(1:nrow(pageLookupAll))]
  setkey(pageLookupAll, pagePath)
  pageLookupAll = pExclude[pageLookupAll]
  pageLookupAll[is.na(exclude), exclude:=0]
  
  
  uidLookupAll = raw[, list(totPages = .N), by=list(uid)][order(-totPages)]
  uidLookupAll[, uId:= seq(1:nrow(uidLookupAll))]
  setkey(uidLookupAll, uid)
  
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
  
  
  #explore
  
  inPages = c(1,2,3,4,5)
  getKNearest(cleanDedup, inPages, 5)
  
  recIds = getNRecs(cleanDedup, inPages, pageLookup, 3, 10)
  
  
  pagesViewed = pageLookup[pId %in% inPages]
  pageRecs = pageLookup[pId %in% recIds[, pId]]
  
  
  #test on larger set
  testClean = test[, list(uid, rank, pagePath)]
  setkey(testClean, pagePath)
  setkey(pageLookupAll, pagePath)
  testClean = pageLookupAll[testClean][, list(uid, rank, pId)]
  setkey(testClean, uid, rank)
  
  testIds = testClean[, list(maxRank = max(rank)), by = uid]
  testIds = testIds[maxRank>2, ]
  
  # export relevant files for use in shiny app
  save(testIds, testClean, pageLookup, cleanDedup , file = paste(dataDir, "msePersData.rdata", sep=""))
  
  
  tgtId = 1
  tgtPages = testClean[uid == testIds[tgtId, uid], pId]
  sessionLength = length(tgtPages)
  recLength = 3
  
  for (i in 1:sessionLength) {
    #i=1
    inPages = tgtPages[1:i]
    inPageNames = pageLookup[pId %in% inPages, pagePath]
    recs = getNRecs(cleanDedup, inPages, pageLookup, recLength, 10)
    cat(paste("Pages you have visited: \n", sep=""))
    cat(paste(inPageNames, sep=" ", collapse = "\n"))
    cat("\n")
    cat("\n")
    cat(paste("Pages you might like: \n", sep=""))
    cat(paste(recs[, pagePath], sep=" ", collapse = "\n"))
    cat("\n")
    cat("\n")
    cat(paste("Most Popular Pages: \n", sep=""))
    cat(paste(pageLookup[!(pId %in% unique(c(inPages, recs[, pId]))), ] [1:recLength, pagePath], sep=" ", collapse = "\n"))
    cat("\n")
    cat("\n")
    cat("\n")
    Sys.sleep(1)
  }
  
  
## End Look into collaborative filtering
###########################################################  
  


  
###########################################################  
## Look into a link graph

  #create full dicts
  #pageLookupAll = raw[, list(totHits = .N), by=list(pagePath)][order(-totHits)]
  #pageLookupAll[, pId:= seq(1:nrow(pageLookupAll))]
  #setkey(pageLookupAll, pagePath)
  #pageLookupAll = pExclude[pageLookupAll]
  #pageLookupAll[is.na(exclude), exclude:=0]
    
  #uidLookupAll = raw[, list(totPages = .N), by=list(uid)][order(-totPages)]
  #uidLookupAll[, uId:= seq(1:nrow(uidLookupAll))]
  #setkey(uidLookupAll, uid)
  
  
  #create clean links
  setkey(raw,pagePath)
  setkey(pageLookupAll, pagePath)
  links = raw[pageLookupAll][, list(uid, pId, rank)]
  setkey(links, uid)
  links = links[uidLookupAll][, list(uId, pId, rank)]
  setkey(links, uId, rank)
  setkey(pageLookupAll, pId)
  
  links2 = copy(links)
  links2[, rank:=rank+1]
  setkey(links2, uId, rank)
  
  linksf = links[links2][!is.na(pId), list(tot = .N), by = list(page1 = i.pId, page2 = pId)]
  #remove self links
  linksf = linksf[page1 != page2, ]
  #combine directions
  linksf2 = copy(linksf)
  setkey(linksf, page1, page2)
  setkey(linksf2, page2, page1)
  
  edges = linksf[linksf2, nomatch=0][page1<page2, list(page1, page2, weight = tot + i.tot)]
  nodes = data.table(pId = sort(unique(c(edges[, page1], edges[, page2]))))
  setkey(nodes, pId)
  nodes = pageLookupAll[nodes]
  nodes[, rank:= seq(1,nrow(nodes),1)]
  setcolorder(nodes,c("pId", "pagePath", "totHits", "rank", "exclude"))
  #edges[weight>1, ]

  #hit distribution
  ggplot() + geom_point(data=nodes, aes(x=rank, y=floor(log(1+totHits))))
  hitFreq = nodes[, .N, by = list(totHits)]
  
  minHits = 200
  nodes = nodes[totHits>=minHits, ]
  edges = edges[page1 %in% nodes[, pId] & page2 %in% nodes[, pId], ]
  
  saveFlag = 1
  if (saveFlag == 1) {
    save(nodes, edges, file = paste(dataDir,"nodesEdgesExForum20150704.rdata", sep=""))
  }
  
  # create graph, generate layout and pull positions
  g = graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  
  set.seed(1234)
  layoutStart = matrix(runif(2*nrow(nodes)), nrow(nodes), 2)
  tmpLayout = layout_with_fr(g, coords=layoutStart) 
  
  nodesDT = as.data.table(get.data.frame(g, what="vertices"))
  nodesDT[, Xorg:=tmpLayout[, 1]]
  nodesDT[, Yorg:=tmpLayout[, 2]]
  
  capX = 100
  sum(abs(nodesDT[, Xorg])>capX)/nrow(nodes)
  capY = 100
  sum(abs(nodesDT[, Yorg])>capY)/nrow(nodes)
  
  nodesDT[, X:=Xorg]
  nodesDT[abs(Xorg)>capX, X:= capX*sign(Xorg)]
  nodesDT[, Y:=Yorg]
  nodesDT[abs(Yorg)>capY, Y:= capY*sign(Yorg)]
  
  #clustering
  stds = apply(nodesDT[, list(X, Y)], 2, sd)
  ds <- dbscan(nodesDT[, list(X, Y)], mean(stds), MinPts = floor(nrow(nodesDT)/500))
  #ds <- dbscan(nodesDT[, list(Xorg, Yorg)], mean(stds)/10, MinPts = floor(nrow(nodesDT)/20))
  #dbscan(nodesDT[, list(X, Y)], eps=5, MinPts = 5)
  
  useClusters = 0
  if (useClusters != 1) {
    nodesDT[, typeId:=floor(log(1+max(totHits))) - floor(log(1+totHits))]
    
  } else {
    nodesDT[, typeId:=ds$cluster]
    
  }
         
  #minX = min(tmp[, X])
  #stdDevX = sd(tmp[, X])
  #maxX = max(tmp[, X])
  #medX = median(tmp[, X])
  #minY = min(tmp[, Y])
  #maxY = max(tmp[, Y])
  #stdDevY = sd(tmp[, Y])
  #medY = median(tmp[, Y])
  
  edgesDT = as.data.table(get.data.frame(g, what="edges"))
  setkey(edgesDT, from)
  setkey(nodesDT, name)
  
  edgesDT = nodesDT[edgesDT][, list(to, from=name, weight, X1=X, Y1=Y)]
  setkey(edgesDT, to)
  edgesDT = nodesDT[edgesDT][, list(to = name, from, weight, X1, Y1, X2=X, Y2=Y)]
  setkey(edgesDT, from)
  
  #pull in node ids names
  #setkey(edgesDT, toName)    
  #edgesDT[, to:=nodesDT[edgesDT, id]]
  #setkey(edgesDT, fromName)
  #edgesDT[, from:=networkDT[edgesDT, id]]
  
  localPlot = 0
  
  if (localPlot == 1) {
    defaultSize = 10
    pnet <- ggplot() + geom_segment(data=edgesDT, aes(x=X1, y=Y1, xend = X2, yend = Y2,size = weight), 
                                    colour="grey", alpha=0.5)
    
    
    pnet <- pnet  + 
      geom_point(data=nodesDT, aes(X, Y), color=nodesDT[, exclude]+1, size=nodesDT[, log(totHits)]*defaultSize/10, alpha=0.8) +
      geom_text(data=nodesDT, aes(X, Y, label = name), size = 3) +
      #scale_colour_manual(guide=FALSE, name="",  values = c("node"="purple", "FALSE"="dark green"))+
      scale_colour_brewer(type = "seq", palette = 1) +
      scale_x_continuous(breaks = NULL) + 
      scale_y_continuous(breaks = NULL) +
      # discard default grid + titles in ggplot2 
      theme(panel.background = element_blank()) + 
      theme(legend.position="none") +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank()) +
      theme(legend.background = element_rect(colour = NA)) + 
      theme(panel.background = element_rect(fill = "white", 
                                            colour = NA)) + 
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank())
   plot(pnet) 
  }
  
  #set up json export for js vis
  edgesExport = copy(edgesDT[, list(from, to, weight)])
  setnames(edgesExport, c("from", "to", "weight"), c("source", "target", "size"))
  edgesExport[, id:=1:nrow(edgesExport)]
  edgesExport[, label:=""]
  edgesExport[, color:="rgb(234,246,249)"]
  # round everything
  edgesExport[, size := round(size*1000)/1000]
  #make everything characters
  fieldClasses = laply(edgesExport, class)
  edgesExport[, (colnames(edgesExport)[fieldClasses!="character"]) := lapply(.SD, as.character), .SDcols = colnames(edgesExport)[fieldClasses!="character"]] 
  
  edgesExportJ = jsonlite::toJSON(edgesExport, pretty=FALSE)
  
  nodesExport = copy(nodesDT[, list(id = name, label = pagePath, size = totHits, typeId, x = X, y = Y)])
  #setnames(nodesExport, c("name", "X", "Y"), c("label", "x", "y"))
  
  tmpcolList = brewer.pal(9,"Set1")
  
  tmpcol = data.table(typeId = sort(unique(nodesExport[, typeId])), color =  tmpcolList[1:length(unique(nodesExport[, typeId]))])
  
  for (i in 1:nrow(tmpcol)) {
    #i=1
    tmpcol[i, color:=paste("rgb(",paste(col2rgb(tmpcol[i, color]), collapse=","),")", sep="")]
  }
  
  setkey(tmpcol, typeId)
  setkey(nodesExport, typeId)  
  nodesExport = nodesExport[tmpcol]
  setkey(nodesExport, id)
  # round everything
  #nodesExport[, size := round(log(1+size)*1000)/1000]
  nodesExport[, size := round(size*1000)/1000]
  nodesExport[, x := round(x*1000)/1000]
  nodesExport[, y := round(y*1000)/1000]
  
  #make everything characters
  fieldClasses = laply(nodesExport, class)
  nodesExport[, (colnames(nodesExport)[fieldClasses!="character"]) := lapply(.SD, as.character), .SDcols = colnames(nodesExport)[fieldClasses!="character"]] 
  
  
  #nodesExportJ = paste(ldply(seq(1,nrow(nodesExport),1), function(x) toJSON(nodesExport[x,])), sep=",")
  nodesExportJ = jsonlite::toJSON(nodesExport, pretty=FALSE)
  
  #export data json
  sink(paste(visDir,"data.json", sep=""))
  cat('{"edges":',edgesExportJ,',"nodes":',nodesExportJ,'}', sep="")
  sink()
  
  
  ## config file for javascript vis
  configText = '
  {
  "type": "network",
  "version": "1.0",
  "data": "data.json",
  "logo": {
  "text": "",
  "file": "",
  "link": ""
  },
  "text": {
  "title": "visTitle",
  "more": "visMore",
  "intro": "visIntro"
  },
  "legend": {
  "edgeLabel": "edgeLabelText",
  "colorLabel": "Page Volume",
  "nodeLabel": "nodeLabelText"
  },
  "features": {
  "search": true,
  "groupSelectorAttribute": true,
  "hoverBehavior": "default"
  },
  "informationPanel": {
  "imageAttribute": false,
  "groupByEdgeDirection": false
  },
  "sigma": {
  "graphProperties": {
  "minEdgeSize": 0.5,
  "maxNodeSize": 20,
  "maxEdgeSize": 3,
  "minNodeSize": 1
  },
  "drawingProperties": {
  "labelThreshold": 20,
  "hoverFontStyle": "bold",
  "defaultEdgeType": "curve",
  "defaultLabelColor": "#000",
  "defaultLabelHoverColor": "#fff",
  "defaultLabelSize": 14,
  "activeFontStyle": "bold",
  "fontStyle": "bold",
  "defaultHoverLabelBGColor": "#002147",
  "defaultLabelBGColor": "#ddd"
  },
  "mouseProperties": {
  "minRatio": 0.75,
  "maxRatio": 100
  }
  }
  }
  '
  
  
  
  #export config json
  visTitle = "Clickstream visualisation of MoneySavingsExpert.com"
  visMore = "Network visualisation based on direct page transitions at MSE (excluding forum) on 04/Jul/2015"
  visIntro = "Layout based on Force Directed Graph algorithm- attraction between pages proportional to volume of transitions"
  edgeLabelText = "Link between pages"
  nodeLabelText = "Pages"       
  
  configExport = configText
  configExport = gsub("visTitle", visTitle, configExport)
  configExport = gsub("visMore", visMore, configExport)
  configExport = gsub("visIntro", visIntro, configExport)
  configExport = gsub("edgeLabelText", edgeLabelText, configExport)
  configExport = gsub("nodeLabelText", nodeLabelText, configExport)
  
  #export data json
  sink(paste(visDir,"config.json", sep=""))
  cat(configExport)
  sink()
  
  # set web server running - start local websever
  # python -m SimpleHTTPServer 8000
  #system(paste("cd ", visDir, sep=""))
  #system("python -m SimpleHTTPServer 8000")

  #note you then need to top from command line and find the python pid
  # then sudo kill pid
  
  #then access at http://0.0.0.0:8000/
  
## End Look into a link graph
###########################################################  
