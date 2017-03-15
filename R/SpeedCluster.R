setGeneric(
  name = "speedCluster",
  def = function(track, avg, minT , sl)
  {
    .loadPackages()
    standardGeneric("speedCluster")
  }
)
##Creeates the speeed clusters Track is a given track, avg is the average speed,
##minT is the minimum period at the speed. And sl is the speed limit
setMethod(
  f = "speedCluster",
  signature = c("Track","numeric", "numeric", "numeric"),
  definition = function(track, avg, minT , sl)
  {

    if (is.null(track)|| length(track) < 2){
      return (0)}
    cl<-list()
    clusterId = 1;
    ##Order the speed so it will start with the slowest speed cluster
    speedOrder <- order(track@connections$speed)
    for(n in 1:length(speedOrder)){
    x<-.LimitedNeighborhood(track,speedOrder[n],minT,clusterId , cl, avg , sl)
      if(is.list(x)){
        clusterId = clusterId + 1
        cl <- x
      }
    }
   return (cl)

  }
)
