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
  signature = c("DataSourceInfo","DataSetInfo","TracksCollection", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, tempo)
  {
    PartnerList <- list()
    i = 1
    for (n in 1:length(A1@tracksCollection)){
      for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

        FoundPartners <- FindPartner(datasource,dataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo)
       if(length(FoundPartners)>0){
         for (k in 1:length(FoundPartners)){
          pair = c(levels(FoundPartners[[k]]@data$traj[[1]]), levels(A1@tracksCollection[[n]]@tracks[[m]]@data$traj[[1]]))
          if(pair[1]!=pair[2]){
          PartnerList[i] <- list(pair)
          i = i + 1}
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
