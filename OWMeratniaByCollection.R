setGeneric(
  name = "owMeratniaByCollection",
  def = function(A1, dist, speed)
  {
    .loadPackages()
    standardGeneric("owMeratniaByCollection")
  }
)

setMethod(
  f = "owMeratniaByCollection",
  signature = c("TracksCollection", "numeric", "numeric"),
  definition = function(A1, dist, speed)
  {
    PartnerList <- list()
    compressed <- list()
    i = 1
    for (n in 1:length(A1@tracksCollection)){
      print("Tracks")
      print(n)
      for (m in 1:length(A1@tracksCollection[[n]]@tracks)){
        print("Track")
        print(m)
        ##if(n != 12 && m!= 97)##{
        compressed[m] <- owMeratniaBy(A1@tracksCollection[[n]]@tracks[[m]], dist, speed)##}
      ##  if(n == 12 && m== 97){
        ##  compressed[m] <- owMeratniaBy(A1@tracksCollection[[n]]@tracks[[m-1]], dist, speed)##}
      }
      PartnerList <- c(PartnerList,Tracks(compressed))
      compressed <- NULL
      compressed <- list()
    }
      return (TracksCollection(PartnerList))}

  )
