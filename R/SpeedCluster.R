setGeneric(
  name = "SpeedCluster",
  def = function(track, avg, minT , sl)
  {
    loadPackages()
    standardGeneric("SpeedCluster")
  }
)

setMethod(
  f = "SpeedCluster",
  signature = c("Track","numeric", "numeric", "numeric"),
  definition = function(track, avg, minT , sl)
  {

    if (is.null(track)|| length(track) < 2){
      return (0)}
    cl<-list()
    clusterId = 1;
    speedOrder <- order(track@connections$speed)
    for(n in 1:length(speedOrder)){
    x<-LimitedNeighborhood(track,speedOrder[n],minT,clusterId , cl, avg , sl)
      if(is.list(x)){
        clusterId = clusterId + 1
        cl <- x
      }
    }
   return (cl)

  }
)
