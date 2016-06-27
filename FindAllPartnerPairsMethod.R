setGeneric(
  name = "FindAllPartnerPairs",
  def = function(datasource,dataset,A1, dist, tempo)
  {
    loadPackages()
    standardGeneric("FindAllPartnerPairs")
  }
)

setMethod(
  f = "FindAllPartnerPairs",
  signature = c("DataSourceInfo","DataSetInfo","list", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, tempo)
  {
    PartnerList <- list()
    for (n in 1:length(A1)){
      for (m in 1:length(A1[[n]]@tracks)){
        i = 1
        FoundPartners <- FindPartner(datasource,dataset,A1[[n]]@tracks[[m]], dist, tempo)
       if(length(FoundPartners)>0){
         for (k in 1:length(FoundPartners)){
          pair = c(levels(FoundPartners[[k]]@data$traj[[1]]), levels(A1[[n]]@tracks[[m]]@data$traj[[1]]))
          PartnerList[n+m+k] <- list(pair)
          i = i + 1
         }
       }
      }
    }
    if(length(PartnerList)==0){
      return("ListaVazia")
    }
    return (PartnerList)

  }
)
