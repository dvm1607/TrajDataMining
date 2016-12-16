setGeneric(
  name = "FindGoAlong",
  def = function(datasource,dataset,A1, dist, maxtime,mintime)
  {
    loadPackages()
    standardGeneric("FindGoAlong")
  }
)

setMethod(
  f = "FindGoAlong",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo","Track", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, maxtime,mintime)
  {
    tempo=maxtime
    cores = detectCores(all.tests = FALSE, logical = TRUE)
    registerDoMC(cores)

    allGoAlong <- data.frame(Begin=as.POSIXct(character()),
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
    TrajectoryList <- getTrajectoryByTrack(datasource,dataset,A1)
     ##  TrajectoryList <- getTrajectoryByIDList(datasource,dataset,idealList[[el]])

    if(class(TrajectoryList)=="TracksCollection"){
      if(length(TrajectoryList@tracksCollection)>0){
        for (n in 1:length(TrajectoryList@tracksCollection)) {
          for (m in 1:length(TrajectoryList@tracksCollection[[n]]@tracks)) {
           ga<- GoAlong(A1,TrajectoryList@tracksCollection[[n]]@tracks[[m]],dist,tempo,mintime,con)
          # if(class(ga)=="data.frame"){
           #if (nrow(ga)>0){
           #for(j in 1:nrow(ga)){
            #    if(ga[[j,3]]!=ga[[j,4]]){
             #   allGoAlong[nrow(allGoAlong)+1,]<-ga[j,]
              #  }

            #}}}
          }
        }}}
   else if(class(TrajectoryList)=="Tracks"){
      if(length(TrajectoryList@tracks)>0){

          for (m in 1:length(TrajectoryList@tracks)) {
            ga<- GoAlong(A1,TrajectoryList@tracks[[m]],dist,tempo,mintime,con)
           # if(class(ga)=="data.frame"){
            #if (nrow(ga)>1){
             # for(j in 1:nrow(ga)){
              #  allGoAlong[nrow(allGoAlong)+1,]<-ga[j,]

              #}}}
          }
        }}
    else if(class(TrajectoryList)=="Track"){

            ga<- GoAlong(A1,TrajectoryList,dist,tempo,mintime,con)
           # if(class(ga)=="data.frame"){
            #if (nrow(ga)>1){
             # for(j in 1:nrow(ga)){
              #  allGoAlong[nrow(allGoAlong)+1,]<-ga[j,]

              #}}}
          }
    ## }
    dbDisconnect(con)
    return (allGoAlong)

  }
)
