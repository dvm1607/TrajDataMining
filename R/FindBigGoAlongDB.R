setGeneric(
  name = "findBigPartnerDB",
  def = function(datasource,trajectorydataset, dist, tempo,mintime,stboxes,cores,tablename)
  {
    .loadPackages()
    if (!require(TrajDataAccess, quietly = TRUE)) {
      stop('The package TrajDataAccess was not installed')
    }
    standardGeneric("findBigPartnerDB")
  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findBigPartnerDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","list","missing","missing"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes,cores,tablename)
  {
 #   cores = detectCores(all.tests = FALSE, logical = TRUE)
 #   cores = 1;
#    registerDoMC(cores)
    PartnerList <- list()
    allPartner <- data.frame(Begin=as.POSIXct(character()),
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
      ##foreach (n = 1:length(A1@tracksCollection))%dopar%{
      for (n in 1:length(A1@tracksCollection)){
        ##foreach (m = 1:length(A1@tracksCollection[[n]]@tracks))%dopar%{
        for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

          FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime)
         # if(nrow(FoundPartners)>0){
          #  for(j in 1:nrow(FoundPartners)){
           #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

          #  }
         # }
        }
      }
    }

    else if(class(A1)=="Tracks"){

      ##foreach (m = 1:length(A1@tracks))%dopar%{
      for (m in 1:length(A1@tracks)){

        FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime)
      #  if(nrow(FoundPartners)>0){
       #   for(j in 1:nrow(FoundPartners)){
         #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

        #  }
       # }
      }
    }
    else if(class(A1)=="Track"){



      FoundPartners <- findPartner(datasource,trajectorydataset,A1, dist, tempo,mintime)
      #if(nrow(FoundPartners)>0){
       # for(j in 1:nrow(FoundPartners)){
        #  allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]


        #}
      #}
    }
    }
}
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findBigPartnerDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","missing","missing","missing"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes,cores,tablename)
  {
   # cores = detectCores(all.tests = FALSE, logical = TRUE)
    cores = 1;
    registerDoMC(cores)
    PartnerList <- list()
    stboxes<- getIdealBoxes(datasource,trajectorydataset)
    allPartner <- data.frame(Begin=as.POSIXct(character()),
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
          ##foreach (n = 1:length(A1@tracksCollection))%dopar%{
          for (n in 1:length(A1@tracksCollection)){
            ##foreach (m = 1:length(A1@tracksCollection[[n]]@tracks))%dopar%{
            for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

              FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime)
              # if(nrow(FoundPartners)>0){
              #  for(j in 1:nrow(FoundPartners)){
              #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

              #  }
              # }
            }
          }
        }

        else if(class(A1)=="Tracks"){

          ##foreach (m = 1:length(A1@tracks))%dopar%{
          for (m in 1:length(A1@tracks)){

            FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime)
            #  if(nrow(FoundPartners)>0){
            #   for(j in 1:nrow(FoundPartners)){
            #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

            #  }
            # }
          }
        }
        else if(class(A1)=="Track"){



          FoundPartners <- findPartner(datasource,trajectorydataset,A1, dist, tempo,mintime)
          #if(nrow(FoundPartners)>0){
          # for(j in 1:nrow(FoundPartners)){
          #  allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]


          #}
          #}
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findBigPartnerDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","list","numeric","missing"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes,cores,tablename)
  {
    cores_in = detectCores(all.tests = FALSE, logical = TRUE)
    if(cores>cores_in){
      cores=cores_in
    }
    if(cores<1){
    cores = 1;
    }
    registerDoMC(cores)
    PartnerList <- list()
    allPartner <- data.frame(Begin=as.POSIXct(character()),
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
          foreach (n = 1:length(A1@tracksCollection))%dopar%{
            ##for (n in 1:length(A1@tracksCollection)){
            ##foreach (m = 1:length(A1@tracksCollection[[n]]@tracks))%dopar%{
            for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

              FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime)
              # if(nrow(FoundPartners)>0){
              #  for(j in 1:nrow(FoundPartners)){
              #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

              #  }
              # }
            }
          }
        }

        else if(class(A1)=="Tracks"){

          ##foreach (m = 1:length(A1@tracks))%dopar%{
          for (m in 1:length(A1@tracks)){

            FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime)
            #  if(nrow(FoundPartners)>0){
            #   for(j in 1:nrow(FoundPartners)){
            #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

            #  }
            # }
          }
        }
        else if(class(A1)=="Track"){



          FoundPartners <- findPartner(datasource,trajectorydataset,A1, dist, tempo,mintime)
          #if(nrow(FoundPartners)>0){
          # for(j in 1:nrow(FoundPartners)){
          #  allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]


          #}
          #}
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findBigPartnerDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","missing","numeric","missing"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes,cores,tablename)
  {
    cores_in = detectCores(all.tests = FALSE, logical = TRUE)
    if(cores>cores_in){
      cores=cores_in
    }
    if(cores<1){
      cores = 1;
    }
    registerDoMC(cores)
    PartnerList <- list()
    stboxes<- getIdealBoxes(datasource,trajectorydataset)
    allPartner <- data.frame(Begin=as.POSIXct(character()),
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
          foreach (n = 1:length(A1@tracksCollection))%dopar%{
            ##for (n in 1:length(A1@tracksCollection)){
            ##foreach (m = 1:length(A1@tracksCollection[[n]]@tracks))%dopar%{
            for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

              FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime)
              # if(nrow(FoundPartners)>0){
              #  for(j in 1:nrow(FoundPartners)){
              #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

              #  }
              # }
            }
          }
        }

        else if(class(A1)=="Tracks"){

          ##foreach (m = 1:length(A1@tracks))%dopar%{
          for (m in 1:length(A1@tracks)){

            FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime)
            #  if(nrow(FoundPartners)>0){
            #   for(j in 1:nrow(FoundPartners)){
            #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

            #  }
            # }
          }
        }
        else if(class(A1)=="Track"){



          FoundPartners <- findPartner(datasource,trajectorydataset,A1, dist, tempo,mintime)
          #if(nrow(FoundPartners)>0){
          # for(j in 1:nrow(FoundPartners)){
          #  allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]


          #}
          #}
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findBigPartnerDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","list","missing","character"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes,cores,tablename)
  {
    #   cores = detectCores(all.tests = FALSE, logical = TRUE)
    #   cores = 1;
    #    registerDoMC(cores)
    PartnerList <- list()
    allPartner <- data.frame(Begin=as.POSIXct(character()),
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
          ##foreach (n = 1:length(A1@tracksCollection))%dopar%{
          for (n in 1:length(A1@tracksCollection)){
            ##foreach (m = 1:length(A1@tracksCollection[[n]]@tracks))%dopar%{
            for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

              FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime,tablename)
              # if(nrow(FoundPartners)>0){
              #  for(j in 1:nrow(FoundPartners)){
              #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

              #  }
              # }
            }
          }
        }

        else if(class(A1)=="Tracks"){

          ##foreach (m = 1:length(A1@tracks))%dopar%{
          for (m in 1:length(A1@tracks)){

            FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime,tablename)
            #  if(nrow(FoundPartners)>0){
            #   for(j in 1:nrow(FoundPartners)){
            #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

            #  }
            # }
          }
        }
        else if(class(A1)=="Track"){



          FoundPartners <- findPartner(datasource,trajectorydataset,A1, dist, tempo,mintime,tablename)
          #if(nrow(FoundPartners)>0){
          # for(j in 1:nrow(FoundPartners)){
          #  allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]


          #}
          #}
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findBigPartnerDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","missing","missing","character"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes,cores,tablename)
  {
    # cores = detectCores(all.tests = FALSE, logical = TRUE)
    cores = 1;
    registerDoMC(cores)
    PartnerList <- list()
    stboxes<- getIdealBoxes(datasource,trajectorydataset)
    allPartner <- data.frame(Begin=as.POSIXct(character()),
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
          ##foreach (n = 1:length(A1@tracksCollection))%dopar%{
          for (n in 1:length(A1@tracksCollection)){
            ##foreach (m = 1:length(A1@tracksCollection[[n]]@tracks))%dopar%{
            for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

              FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime,tablename)
              # if(nrow(FoundPartners)>0){
              #  for(j in 1:nrow(FoundPartners)){
              #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

              #  }
              # }
            }
          }
        }

        else if(class(A1)=="Tracks"){

          ##foreach (m = 1:length(A1@tracks))%dopar%{
          for (m in 1:length(A1@tracks)){

            FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime,tablename)
            #  if(nrow(FoundPartners)>0){
            #   for(j in 1:nrow(FoundPartners)){
            #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

            #  }
            # }
          }
        }
        else if(class(A1)=="Track"){



          FoundPartners <- findPartner(datasource,trajectorydataset,A1, dist, tempo,mintime,tablename)
          #if(nrow(FoundPartners)>0){
          # for(j in 1:nrow(FoundPartners)){
          #  allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]


          #}
          #}
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findBigPartnerDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","list","numeric","character"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes,cores,tablename)
  {
    cores_in = detectCores(all.tests = FALSE, logical = TRUE)
    if(cores>cores_in){
      cores=cores_in
    }
    if(cores<1){
      cores = 1;
    }
    registerDoMC(cores)
    PartnerList <- list()
    allPartner <- data.frame(Begin=as.POSIXct(character()),
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
          foreach (n = 1:length(A1@tracksCollection))%dopar%{
            ##for (n in 1:length(A1@tracksCollection)){
            ##foreach (m = 1:length(A1@tracksCollection[[n]]@tracks))%dopar%{
            for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

              FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime,tablename)
              # if(nrow(FoundPartners)>0){
              #  for(j in 1:nrow(FoundPartners)){
              #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

              #  }
              # }
            }
          }
        }

        else if(class(A1)=="Tracks"){

          ##foreach (m = 1:length(A1@tracks))%dopar%{
          for (m in 1:length(A1@tracks)){

            FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime,tablename)
            #  if(nrow(FoundPartners)>0){
            #   for(j in 1:nrow(FoundPartners)){
            #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

            #  }
            # }
          }
        }
        else if(class(A1)=="Track"){



          FoundPartners <- findPartner(datasource,trajectorydataset,A1, dist, tempo,mintime,tablename)
          #if(nrow(FoundPartners)>0){
          # for(j in 1:nrow(FoundPartners)){
          #  allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]


          #}
          #}
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)

#precisa do TrajDataAccess
setMethod(
  f = "findBigPartnerDB",
  signature = c("DataSourceInfo","TrajectoryDataSetInfo", "numeric", "numeric","numeric","missing","numeric","character"),
  definition = function(datasource,trajectorydataset, dist, tempo, mintime ,stboxes,cores,tablename)
  {
    cores_in = detectCores(all.tests = FALSE, logical = TRUE)
    if(cores>cores_in){
      cores=cores_in
    }
    if(cores<1){
      cores = 1;
    }
    registerDoMC(cores)
    PartnerList <- list()
    stboxes<- getIdealBoxes(datasource,trajectorydataset)
    allPartner <- data.frame(Begin=as.POSIXct(character()),
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
          foreach (n = 1:length(A1@tracksCollection))%dopar%{
            ##for (n in 1:length(A1@tracksCollection)){
            ##foreach (m = 1:length(A1@tracksCollection[[n]]@tracks))%dopar%{
            for (m in 1:length(A1@tracksCollection[[n]]@tracks)){

              FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracksCollection[[n]]@tracks[[m]], dist, tempo,mintime,tablename)
              # if(nrow(FoundPartners)>0){
              #  for(j in 1:nrow(FoundPartners)){
              #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

              #  }
              # }
            }
          }
        }

        else if(class(A1)=="Tracks"){

          ##foreach (m = 1:length(A1@tracks))%dopar%{
          for (m in 1:length(A1@tracks)){

            FoundPartners <- findPartner(datasource,trajectorydataset,A1@tracks[[m]], dist, tempo,mintime,tablename)
            #  if(nrow(FoundPartners)>0){
            #   for(j in 1:nrow(FoundPartners)){
            #   allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]

            #  }
            # }
          }
        }
        else if(class(A1)=="Track"){



          FoundPartners <- findPartner(datasource,trajectorydataset,A1, dist, tempo,mintime,tablename)
          #if(nrow(FoundPartners)>0){
          # for(j in 1:nrow(FoundPartners)){
          #  allPartner[nrow(allPartner)+1,]<-FoundPartners[j,]


          #}
          #}
        }
      }
    }
    if(nrow(allPartner)==0){
      return("ListaVazia")
    }
    return (allPartner)

  }
)
