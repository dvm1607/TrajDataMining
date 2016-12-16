setGeneric(
  name = "FindGoAlongDB",
  def = function(datasource,dataset, dist, tempo)
  {
    loadPackages()
    standardGeneric("FindGoAlongDB")
  }
)

setMethod(
  f = "FindGoAlongDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric"),
  definition = function(datasource,dataset, dist, tempo)
  {
    PartnerList <- list()
    allGoAlong <- data.frame(Begin=as.POSIXct(character()),
                             End=as.POSIXct(character()),
                             Id1=character(),
                             Id2=character(),
                             stringsAsFactors=FALSE)
    A1 <- getTrajectory(datasource,dataset)
    i = 1
    if(class(A1)=="TracksCollection"){
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
    }

   else if(class(A1)=="Tracks"){

        for (m in 1:length(A1@tracks)){

          FoundPartners <- FindGoAlong(datasource,dataset,A1@tracks[[m]], dist, tempo)
          if(nrow(FoundPartners)>0){
            for(j in 1:nrow(FoundPartners)){
              allGoAlong[nrow(allGoAlong)+1,]<-FoundPartners[j,]

            }
          }
        }
   }
    else if(class(A1)=="Track"){



        FoundPartners <- FindGoAlong(datasource,dataset,A1, dist, tempo)
        if(nrow(FoundPartners)>0){
          for(j in 1:nrow(FoundPartners)){
            allGoAlong[nrow(allGoAlong)+1,]<-FoundPartners[j,]


        }
      }
    }

    if(nrow(allGoAlong)==0){
      return("ListaVazia")
    }
    return (allGoAlong)

  }
)
