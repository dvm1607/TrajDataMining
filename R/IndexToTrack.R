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
    timelist<-as.POSIXct(timelist ,format="%Y-%m-%d %H:%M:%S")
    sti<- STI(SpatialPoints(xy, A1@sp@proj4string),timelist,timelist)

    AR = Track(sti)

    return(AR)


  }
)
