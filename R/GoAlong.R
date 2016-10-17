setGeneric(
  name = "GoAlong",
  def = function(A1, A2, dist, tempo)
  {
    loadPackages()
    standardGeneric("GoAlong")
  }
)

setMethod(
  f = "GoAlong",
  signature = c("Track","Track","numeric","numeric"),
  definition = function(A1, A2, dist, tempo)
  {
    loadPackages()
    initialTime = -1
    finalTime = -1
   inCounter = FALSE
    iniFinList <- list()
    allGoAlongList <-list()
    allGoAlong <- data.frame(Begin=as.POSIXct(character()),
                     End=as.POSIXct(character()),
                     Id1=character(),
                     Id2=character(),
                     stringsAsFactors=FALSE)
    id1="1"
    id2="2"
    #Try to store trajectory unique Id if it does not work try obj id, if it doesn't work give up

    if("traj" %in% colnames(A1@data)){
      id1=as.character(levels(A1@data["traj"][[1]]))
    }
    else if("name" %in% colnames(A1@data)){
      id1=as.character(levels(A1@data["name"][[1]]))
    }

    #Try to store trajectory unique Id if it does not work try obj id, if it doesn't work give up
    if("traj" %in% colnames(A2@data)){
      id2=as.character(levels(A2@data["traj"][[1]]))
    }
    else if("name" %in% colnames(A2@data)){
      id2=as.character(levels(A2@data["name"][[1]]))
    }
    count = 0
      time  = 0

      ## print(A1@data$traj[[1]])
      ## print(A2@data$traj[[1]])

      if (!(xts::first(A1@endTime) < xts::last(A2@endTime) && xts::first(A2@endTime) < xts::last(A1@endTime)))
        return("Time itervals don't overlap!")
      if (!identicalCRS(A1, A2))
        return("CRS are not identical!")

      timeSeries <- mycompare(A2,A1)

      #Contador para as conexoes
      i = 1;
      j = 1;

        if(length(timeSeries@conns1@data$dists)>2){
          for (n in 2:(length(timeSeries@conns1@data$dists)-1)) {
              if (timeSeries@conns1@data$dists[n] <= dist && inCounter==FALSE) {
                initialTime = timeSeries@conns1@data$time[n]
                inCounter=TRUE
                print("Saved the initial time :")
                print(initialTime)
                print(id1)
                print(id2)

              }
              if (timeSeries@conns1@data$dists[n] > dist) {
                time = time + difftime(timeSeries@conns1@data$time[n+1],timeSeries@conns1@data$time[n],units="secs")
                if (time > tempo && inCounter==TRUE){
                  finalTime = timeSeries@conns1@data$time[n]
                 # iniFinList <- c(Begin=initialTime,End=finalTime,Id1=id1,Id2=id2)
                 iniFinList <- c(Begin=as.character(initialTime),End=as.character(finalTime),Id1=id1,Id2=id2)

                  allGoAlong[nrow(allGoAlong)+1,]<-c(iniFinList)
                 # allGoAlongList <- append(allGoAlongList,iniFinList)
                  iniFinList <- NULL
                  inCounter = FALSE
                  time=0
                  print("Saved the final time :")
                  print(finalTime)
                }
              }
              i = i + 1


            if(n==(length(timeSeries@conns1)-1) && inCounter==TRUE){
              finalTime = timeSeries@conns1@data$time[n]

              #iniFinList <- c(Begin=initialTime,End=finalTime,Id1=id1,Id2=id2)
               iniFinList <- c(Begin=as.character(initialTime),End=as.character(finalTime),Id1=id1,Id2=id2)
              allGoAlong[nrow(allGoAlong)+1,]<-c(iniFinList)
#              allGoAlongList <- append(allGoAlongList,iniFinList)
              iniFinList <- NULL
              inCounter = FALSE
            }
          }
}

      return (allGoAlong)
      }



)
