setGeneric(
  name = "douglasPeucker",
  def = function(A1, dist)
  {
    .loadPackages()
    standardGeneric("douglasPeucker")
  }
)

setGeneric(
  name = ".douglasPeuckerRP",
  def = function(A1,firstp,lastp, dist)
  {
    .loadPackages()
    standardGeneric(".douglasPeuckerRP")
  }
)

setMethod(
  f = "douglasPeucker",
  signature = c("Track", "numeric"),
  definition = function(A1, dist)
  {
    if (is.null(A1)|| length(A1@sp) < 3){
      return (A1)}

    firstPoint = 1
    lastPoint = length(A1@sp)
    pointIndexsToKeep <- list()
    pointIndexsToKeep[1]
    ##while (all(A1@sp[firstPoint]==A1@sp[lastPoint]))
    ##{
    ##  lastPoint = lastPoint-1
    ##}
    timelist<-list()
    ylist<-list()
    xlist<-list()


    pointIndexsToKeep[1] = firstPoint
    pointIndexsToKeep[2] = lastPoint
    pointIndexsToKeep <- c(pointIndexsToKeep,.douglasPeuckerRP(A1,firstPoint,lastPoint,dist))
    pointIndexsToKeep<- unlist(pointIndexsToKeep, recursive=FALSE)
    pointIndexsToKeep <- sort(pointIndexsToKeep,method="quick")

    saveddf<-as.data.frame(matrix(0,ncol = length(A1@data),nrow=length(pointIndexsToKeep)))
    colnames(saveddf)<-colnames(A1@data)

    for (n in 1:length(pointIndexsToKeep)){
      i <- pointIndexsToKeep[n]
      timelist<-c(timelist,as.character(as.POSIXct(A1@endTime[i])))
      ylist<-c(ylist,A1@sp[i,]@coords[2])
      xlist<-c(xlist,A1@sp[i,]@coords[1])
      saveddf[n,]<-A1@data[i,]
    }

    xlist=unlist(xlist,recursive = FALSE)
    ylist=unlist(ylist,recursive = FALSE)
    dat <- data.frame(x=xlist,y=ylist)
    xy <- coordinates(dat)

    timelist<- unlist(timelist)
    timelist<-as.POSIXct(timelist ,format="%Y-%m-%d %H:%M:%S")
    sti<- STIDF(SpatialPoints(xy, A1@sp@proj4string),timelist,saveddf,timelist)

    AR = Track(sti)

    return(AR)
    ##return(pointIndexsToKeep)
  }
)

setMethod(
  f = ".douglasPeuckerRP",
  signature = c("Track","numeric","numeric","numeric"),
  definition = function(A1,firstp,lastp, dist)
  {
    maxDistance = 0;
    indexFarthest = 1;
    pointIndexsToKeep <- list()
    for (n in  firstp:lastp)
    {
      distance <- dist2gc(A1@sp[firstp],A1@sp[lastp],A1@sp[n])
      if (distance > maxDistance)
      {
        maxDistance = distance;
        indexFarthest = n;
      }
    }

    if (maxDistance > dist && indexFarthest != 1)
    {
      ##Add the largest point that exceeds the tolerance
      pointIndexsToKeep <- c(pointIndexsToKeep,indexFarthest)
      pointIndexsToKeep <- c(pointIndexsToKeep,.douglasPeuckerRP(A1,firstp,indexFarthest,dist))
      pointIndexsToKeep <- c(pointIndexsToKeep,.douglasPeuckerRP(A1,indexFarthest,lastp,dist))
    }
    return (pointIndexsToKeep)
  }
)
