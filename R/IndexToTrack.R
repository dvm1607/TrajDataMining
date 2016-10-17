setGeneric(
  name = "IndexToTrack",
  def = function(A1, index)
  {
    loadPackages()
    standardGeneric("IndexToTrack")
  }
)

setMethod(
  f = "IndexToTrack",
  signature = c("Track", "list"),
  definition = function(A1, index)
  {
    timelist<-list()
    ylist<-list()
    xlist<-list()

    pointIndexsToKeep<- unlist(index, recursive=FALSE)
    pointIndexsToKeep <- sort(pointIndexsToKeep,method="quick")
    for (n in 1:length(pointIndexsToKeep)){
      i <- pointIndexsToKeep[n]
      timelist<-c(timelist,as.character(as.POSIXct(A1@endTime[i])))
      ylist<-c(ylist,A1@sp[i,]@coords[2])
      xlist<-c(xlist,A1@sp[i,]@coords[1])
    }

    xlist=unlist(xlist,recursive = FALSE)
    ylist=unlist(ylist,recursive = FALSE)
    dat <- data.frame(x=xlist,y=ylist)
    xy <- coordinates(dat)

    timelist<- unlist(timelist)
    for(n in 1:length(timelist)){
      print(timelist[n])
      if(is.na(timelist[n])){
        timelist[n]=(timelist[n-1]+timelist[n+1])/2 ##tentando substituir valor se houver apenas um faltante
      }
    }
    timelist<-as.POSIXct(timelist ,format="%Y-%m-%d %H:%M:%S")
    ##dat here is a place holderst
    sti<- STI(SpatialPoints(xy, A1@sp@proj4string),timelist,timelist)
    ##sti<- STIDF(SpatialPoints(xy, A1@sp@proj4string),timelist,dat,timelist)

    AR = Track(sti)

    return(AR)


  }
)
