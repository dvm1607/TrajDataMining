setGeneric(
  name = "FindBigGoAlongDB",
  def = function(datasource,trajectorydataset, dist, tempo,mintime,stboxes)
  {
    loadPackages()
    standardGeneric("FindBigGoAlongDB")
  }
)

setMethod(
  f = "FindBigGoAlongDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","list"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes)
  {
    PartnerList <- list()
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
    ##if data can be brought at once skip step o selecting it by groups
    for(i in 1:length(stboxes)){
      if(length(stboxes)==1){
        idealList<-list(1)
      }
      else{
      idealList <- getIdealGroupsInSTBox(con,trajectorydataset,stboxes[[i]])
      }
      for(el in 1:length(idealList)){
        if(length(idealList)==1){
          A1 <- getTrajectoryBySTBox(datasource,trajectorydataset,stboxes[[i]])
        }
        else{
      A1 <- getTrajectoryByIDList(datasource,trajectorydataset,idealList[[el]])
        }

    if(class(A1)=="TracksCollection"){
      for (n in 1:length(A1@tracksCollection)){
        for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

          FoundPartners <- FindGoAlong(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime)
         # if(nrow(FoundPartners)>0){
          #  for(j in 1:nrow(FoundPartners)){
           #   allGoAlong[nrow(allGoAlong)+1,]<-FoundPartners[j,]

          #  }
         # }
        }
      }
    }

    else if(class(A1)=="Tracks"){

      for (m in 1:length(A1@tracks)){

        FoundPartners <- FindGoAlong(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime)
      #  if(nrow(FoundPartners)>0){
       #   for(j in 1:nrow(FoundPartners)){
         #   allGoAlong[nrow(allGoAlong)+1,]<-FoundPartners[j,]

        #  }
       # }
      }
    }
    else if(class(A1)=="Track"){



      FoundPartners <- FindGoAlong(datasource,trajectorydataset,A1, dist, tempo,mintime)
      #if(nrow(FoundPartners)>0){
       # for(j in 1:nrow(FoundPartners)){
        #  allGoAlong[nrow(allGoAlong)+1,]<-FoundPartners[j,]


        #}
      #}
    }
    }
}
    if(nrow(allGoAlong)==0){
      return("ListaVazia")
    }
    return (allGoAlong)

  }
)
