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
  signature = c("DataSource","DataSet","Track", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, tempo)
  {

    TrajectoryList <- getTrajectory(datasource,dataset,A1)
    PartnerList <- list()
    i = 0
    for (n in 1:length(TrajectoryList)) {
      if (PartnerTrajectory(A1,TrajectoryList[n],dist,tempo)) {
        PartnerList[i] <- TrajectoryList[n]
        i = i + 1
      }

    }
    return (PartnerList)

  }
)
