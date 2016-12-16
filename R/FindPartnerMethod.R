setGeneric(
  name = "FindPartner",
  def = function(datasource,dataset,A1, dist, tempo)
  {
    loadPackages()
    standardGeneric("FindPartner")
  }
)

setMethod(
  f = "FindPartner",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo","Track", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, tempo)
  {
    cores = detectCores(all.tests = FALSE, logical = TRUE)
    registerDoMC(cores)
    TrajectoryList <- getTrajectoryByTrack(datasource,dataset,A1)
    PartnerList <- list()
    i = 1
    if(class(TrajectoryList)=="TracksCollection")
    if(length(TrajectoryList@tracksCollection)>0){
    for (n in 1:length(TrajectoryList@tracksCollection)) {
      for(m in 1:length(TrajectoryList@tracksCollection[[n]]@tracks)){
      if (PartnerTrajectory(A1,TrajectoryList@tracksCollection[[n]]@tracks[[m]],dist,tempo)) {
        PartnerList[i] <- TrajectoryList@tracksCollection[[n]]@tracks[[m]]
        i = i + 1
      }
}
    }}
    return (PartnerList)

  }
)
