setGeneric(
  name = "FindGoAlong",
  def = function(datasource,dataset,A1, dist, tempo)
  {
    loadPackages()
    standardGeneric("FindGoAlong")
  }
)

setMethod(
  f = "FindGoAlong",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo","Track", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, tempo)
  {
    cores = detectCores(all.tests = FALSE, logical = TRUE)
    registerDoMC(cores)
    TrajectoryList <- getTrajectoryByTrack(datasource,dataset,A1)
    allGoAlong <- data.frame(Begin=as.POSIXct(character()),
                              End=as.POSIXct(character()),
                              Id1=character(),
                              Id2=character(),
                              stringsAsFactors=FALSE)
    i = 1
    if(class(TrajectoryList)=="TracksCollection"){
      if(length(TrajectoryList@tracksCollection)>0){
        for (n in 1:length(TrajectoryList@tracksCollection)) {
          for (m in 1:length(TrajectoryList@tracksCollection[[n]]@tracks)) {
           ga<- GoAlong(A1,TrajectoryList@tracksCollection[[n]]@tracks[[m]],dist,tempo)
           if (nrow(ga)>0){
           for(j in 1:nrow(ga)){
                if(ga[[j,3]]!=ga[[j,4]]){
                allGoAlong[nrow(allGoAlong)+1,]<-ga[j,]
                }

            }}
          }
        }}}
   else if(class(TrajectoryList)=="Tracks"){
      if(length(TrajectoryList@tracks)>0){

          for (m in 1:length(TrajectoryList@tracks)) {
            ga<- GoAlong(A1,TrajectoryList@tracks[[m]],dist,tempo)
            if (nrow(ga)>1){
              for(j in 1:nrow(ga)){
                allGoAlong[nrow(allGoAlong)+1,]<-ga[j,]

              }}
          }
        }}
    else if(class(TrajectoryList)=="Track"){

            ga<- GoAlong(A1,TrajectoryList,dist,tempo)
            if (nrow(ga)>1){
              for(j in 1:nrow(ga)){
                allGoAlong[nrow(allGoAlong)+1,]<-ga[j,]

              }}
          }

    return (allGoAlong)

  }
)
