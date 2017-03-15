setGeneric(
  name = "findPartner",
  def = function(datasource,dataset,A1, dist, maxtime,mintime,tablename)
  {
    .loadPackages()
    if (!require(TrajDataAccess, quietly = TRUE)) {
      stop('The package TrajDataAccess was not installed')
    }
    standardGeneric("findPartner")
  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findPartner",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo","Track", "numeric", "numeric","numeric","missing"),
  definition = function(datasource,dataset,A1, dist, maxtime,mintime,tablename)
  {
    tempo=maxtime


    allPartner <- data.frame(Begin=as.POSIXct(character()),
                              End=as.POSIXct(character()),
                              Id1=character(),
                              Id2=character(),
                              stringsAsFactors=FALSE)

    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = datasource@db,
                     host = datasource@host, port = datasource@port,
                     user = datasource@user, password = datasource@password)
    on.exit(dbDisconnect(con))

    i = 1
  ##  stboxexample <- stbox(A1)
  ##  referencestbox <- STBox(xMin = stboxexample[[1]][1],xMax = stboxexample[[1]][2],yMin = stboxexample[[2]][1],yMax = stboxexample[[2]][2],tMin = as.character(stboxexample[[3]][1]),tMax = as.character(stboxexample[[3]][2]))
   ## idealList <- getIdealGroupsInSTBox(con,dataset,referencestbox)
  ##   for(el in 1:length(idealList)){
    TrajectoryList <- getTrajectoryByTrack(datasource,dataset,A1,dist/5)
     ##  TrajectoryList <- getTrajectoryByIDList(datasource,dataset,idealList[[el]])

    if(class(TrajectoryList)=="TracksCollection"){
      if(length(TrajectoryList@tracksCollection)>0){
        for (n in 1:length(TrajectoryList@tracksCollection)) {
         ## foreach (m =1:length(TrajectoryList@tracksCollection[[n]]@tracks))%dopar% {
          for (m in 1:length(TrajectoryList@tracksCollection[[n]]@tracks)) {
            if(length(TrajectoryList@tracksCollection[[n]]@tracks[[m]])>15){
           ga<- partner(A1,TrajectoryList@tracksCollection[[n]]@tracks[[m]],dist,tempo,mintime,FALSE)
          }
             if(class(ga)=="data.frame"){
          if (nrow(ga)>0){
           for(j in 1:nrow(ga)){
               if(ga[[j,3]]!=ga[[j,4]]){
               allPartner[nrow(allPartner)+1,]<-ga[j,]
                }

            }}}
          }
        }}}
   else if(class(TrajectoryList)=="Tracks"){
      if(length(TrajectoryList@tracks)>0){

          for (m in 1:length(TrajectoryList@tracks)) {
            if(length(TrajectoryList@tracks[[m]])>15){
            ga<- partner(A1,TrajectoryList@tracks[[m]],dist,tempo,mintime,FALSE)
          }
             if(class(ga)=="data.frame"){
            if (nrow(ga)>1){
              for(j in 1:nrow(ga)){
                allPartner[nrow(allPartner)+1,]<-ga[j,]

              }}}
          }
        }}
    else if(class(TrajectoryList)=="Track"){

            ga<- partner(A1,TrajectoryList,dist,tempo,mintime,con,FALSE)
            if(class(ga)=="data.frame"){
            if (nrow(ga)>1){
              for(j in 1:nrow(ga)){
                allPartner[nrow(allPartner)+1,]<-ga[j,]

              }}}
          }
    ## }
    dbDisconnect(con)
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findPartner",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo","TracksCollection", "numeric", "numeric","numeric","missing"),
  definition = function(datasource,dataset,A1, dist, maxtime,mintime,tablename)
  {
    PartnerList <- list()
    allPartner <- data.frame(Begin=as.POSIXct(character()),
                             End=as.POSIXct(character()),
                             Id1=character(),
                             Id2=character(),
                             stringsAsFactors=FALSE)
    i = 1
    for (n in 1:length(A1@tracksCollection)){
      for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

        FoundPartners <- findPartner(datasource,dataset,A1@tracksCollection[[n]]@tracks[[m]], dist, maxtime,mintime)
        if(nrow(FoundPartners)>0){
          for(j in 1:nrow(FoundPartners)){
            allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

          }
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findPartner",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo","Track", "numeric", "numeric","numeric","character"),
  definition = function(datasource,dataset,A1, dist, maxtime,mintime,tablename)
  {
    tempo=maxtime


    allPartner <- data.frame(Begin=as.POSIXct(character()),
                             End=as.POSIXct(character()),
                             Id1=character(),
                             Id2=character(),
                             stringsAsFactors=FALSE)

    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = datasource@db,
                     host = datasource@host, port = datasource@port,
                     user = datasource@user, password = datasource@password)
    on.exit(dbDisconnect(con))

    i = 1
    ##  stboxexample <- stbox(A1)
    ##  referencestbox <- STBox(xMin = stboxexample[[1]][1],xMax = stboxexample[[1]][2],yMin = stboxexample[[2]][1],yMax = stboxexample[[2]][2],tMin = as.character(stboxexample[[3]][1]),tMax = as.character(stboxexample[[3]][2]))
    ## idealList <- getIdealGroupsInSTBox(con,dataset,referencestbox)
    ##   for(el in 1:length(idealList)){
    TrajectoryList <- getTrajectoryByTrack(datasource,dataset,A1,dist/5)
    ##  TrajectoryList <- getTrajectoryByIDList(datasource,dataset,idealList[[el]])

    if(class(TrajectoryList)=="TracksCollection"){
      if(length(TrajectoryList@tracksCollection)>0){
        for (n in 1:length(TrajectoryList@tracksCollection)) {
          ## foreach (m =1:length(TrajectoryList@tracksCollection[[n]]@tracks))%dopar% {
          for (m in 1:length(TrajectoryList@tracksCollection[[n]]@tracks)) {
            if(length(TrajectoryList@tracksCollection[[n]]@tracks[[m]])>15){
              ga<- partner(A1,TrajectoryList@tracksCollection[[n]]@tracks[[m]],dist,tempo,mintime,con,tablename)
            }
            # if(class(ga)=="data.frame"){
            #if (nrow(ga)>0){
            #for(j in 1:nrow(ga)){
            #    if(ga[[j,3]]!=ga[[j,4]]){
            #   allPartner[nrow(allPartner)+1,]<-ga[j,]
            #  }

            #}}}
          }
        }}}
    else if(class(TrajectoryList)=="Tracks"){
      if(length(TrajectoryList@tracks)>0){

        for (m in 1:length(TrajectoryList@tracks)) {
          if(length(TrajectoryList@tracks[[m]])>15){
            ga<- partner(A1,TrajectoryList@tracks[[m]],dist,tempo,mintime,con,tablename)
          }
          # if(class(ga)=="data.frame"){
          #if (nrow(ga)>1){
          # for(j in 1:nrow(ga)){
          #  allPartner[nrow(allPartner)+1,]<-ga[j,]

          #}}}
        }
      }}
    else if(class(TrajectoryList)=="Track"){

      ga<- partner(A1,TrajectoryList,dist,tempo,mintime,con,tablename)
      # if(class(ga)=="data.frame"){
      #if (nrow(ga)>1){
      # for(j in 1:nrow(ga)){
      #  allPartner[nrow(allPartner)+1,]<-ga[j,]

      #}}}
    }
    ## }
    dbDisconnect(con)
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findPartner",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo","TracksCollection", "numeric", "numeric","numeric","missing"),
  definition = function(datasource,dataset,A1, dist, maxtime,mintime,tablename)
  {

    allPartner <- data.frame(Begin=as.POSIXct(character()),
                             End=as.POSIXct(character()),
                             Id1=character(),
                             Id2=character(),
                             stringsAsFactors=FALSE)
    i = 1
    for (n in 1:length(A1@tracksCollection)){
      for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

        FoundPartners <- findPartner(datasource,dataset,A1@tracksCollection[[n]]@tracks[[m]], dist, maxtime,mintime,tablename)
        if(nrow(FoundPartners)>0){
          for(j in 1:nrow(FoundPartners)){
            allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

          }
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)
