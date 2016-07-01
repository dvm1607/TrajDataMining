setGeneric(
  name = "OWMeratniaBy",
  def = function(A1, dist, speed)
  {
    loadPackages()
    standardGeneric("OWMeratniaBy")
  }
)

setMethod(
  f = "OWMeratniaBy",
  signature = c("Track", "numeric", "numeric"),
  definition = function(A1, dist, speed)
  {

    if (is.null(A1)|| length(A1@sp) < 3){
      return (A1)}

    stop_point = FALSE
    anchor = 1
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
size <- (length(A1@sp)-1)
    for (e in 1+anchor:size){
      if (stop_point==FALSE){
        for (i in 1+anchor:e){
          if (stop_point==FALSE && i < size){
            dte<- as.numeric(A1@endTime[e]-A1@endTime[anchor])
            dti<- as.numeric(A1@endTime[i]-A1@endTime[anchor])
            xyline <- (A1@sp[e,]@coords-A1@sp[anchor,]@coords)*(dti/dte)+A1@sp[anchor,]@coords
            sp1<- A1@connections$speed[i-1]
            sp2<- A1@connections$speed[i]
            dsp <- sp1-sp2
            print(paste("eu sou o size", e))
print(i)
            if((dsp < 0) ==TRUE){
              dsp <-dsp*-1
            }
            if(distCosine(xyline, A1@sp[i,]@coords, r=6378137)>dist||dsp>speed){
              stop_point=TRUE
            }
          }
        }
      }
             if(stop_point==TRUE){
               pointIndexsToKeep <- c(pointIndexsToKeep,i)
               anchor = i
               stop_point=FALSE
             }
    }
    pointIndexsToKeep<- unlist(pointIndexsToKeep, recursive=FALSE)
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
