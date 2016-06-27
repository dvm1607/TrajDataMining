setGeneric(
  name = "FindAllPartnerPairs",
  def = function(datasource,dataset,A1, dist, tempo)
  {
    loadPackages()
    standardGeneric("FindPartner")
  }
)

setMethod(
  f = "FindAllPartnerPairs",
  signature = c("DataSourceInfo","DataSetInfo","Tracks", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, tempo)
  {
    PartnerList <- list()
    for (n in 1:length(A1)){
      for (m in 1:length(A1[[n]]@tracks)){
        i = 1
        FoundPartners <- FindPartner(datasource,dataset,A1[[n]]@tracks[[m]], dist, tempo)
        for (k in 1:FoundPartners){
          pair = c(FoundPartners[[k]]@data$name, A1[[n]]@tracks[[m]]@data$name)
          PartnerList[i] <- pair
          i = i + 1
        }
      }
    }

    return (PartnerList)

  }
)
