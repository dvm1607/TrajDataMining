setGeneric(
  name = "FindAllGoAlong",
  def = function(datasource,dataset,A1, dist, tempo)
  {
    loadPackages()
    standardGeneric("FindAllGoAlong")
  }
)

setMethod(
  f = "FindAllGoAlong",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo","TracksCollection", "numeric", "numeric"),
  definition = function(datasource,dataset,A1, dist, tempo)
  {
    PartnerList <- list()
    allGoAlong <- data.frame(Begin=as.POSIXct(character()),
                             End=as.POSIXct(character()),
                             Id1=character(),
                             Id2=character(),
                             stringsAsFactors=FALSE)
    i = 1
    for (n in 1:length(A1@tracksCollection)){
      for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

        FoundPartners <- FindGoAlong(datasource,dataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo)
        if(nrow(FoundPartners)>0){
          for(j in 1:nrow(FoundPartners)){
            allGoAlong[nrow(allGoAlong)+1,]<-FoundPartners[j,]

          }
        }
      }
    }
    if(nrow(allGoAlong)==0){
      return("ListaVazia")
    }
    return (allGoAlong)

  }
)
