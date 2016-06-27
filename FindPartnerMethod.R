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
  signature = c("DataSourceInfo","DataSetInfo","Track", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, tempo)
  {

    TrajectoryList <- getTrajectoryByTrack(datasource,dataset,A1)
    PartnerList <- list()
    i = 1
    if(length(TrajectoryList)>0){
    for (n in 1:length(TrajectoryList)) {
      for (m in 1:length(TrajectoryList[[n]]@tracks)) {
      if (PartnerTrajectory(A1,TrajectoryList[[n]]@tracks[[m]],dist,tempo)) {
        PartnerList[i] <- TrajectoryList[[n]]@tracks[[m]]
        i = i + 1
      }
}
    }}
    return (PartnerList)

  }
)
