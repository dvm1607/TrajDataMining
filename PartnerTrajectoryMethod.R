setGeneric(
  name = "PartnerTrajectory",
  def = function(A1, A2, dist, tempo)
  {
    loadPackages()
    standardGeneric("PartnerTrajectory")
  }
)

setMethod(
  f = "PartnerTrajectory",
  signature = c("Track","Track","numeric","numeric"),
  definition = function(A1, A2, dist, tempo)
  {
    loadPackages()
    ##length é dividido por 2, a lista tem 2 campos portanto tem o length dobrado
    tdiff1 <- as.double(difftime(A1@endTime[[1]],A2@endTime[[1]], units = "hours"))
    tdiff2 <- as.double(difftime(A1@endTime[[length(A1@endTime)]],A2@endTime[[length(A2@endTime)]], units = "hours"))
    if(tdiff1<0){
      tdiff1 <- tdiff1*-1
    }
    if(tdiff2<0){
      tdiff2 <- tdiff2*-1
    }
    length1 <- length(A1@sp@coords) / 2;
    length2 <- length(A2@sp@coords) / 2;
    ini <-
      gBuffer(
        A1@sp[1,], byid = FALSE, id = NULL, width = dist, quadsegs = 5, capStyle =
          "ROUND",
        joinStyle = "ROUND"
      )
    fim <-
      gBuffer(
        A1@sp[length1,], byid = FALSE, id = NULL, width = dist, quadsegs = 5, capStyle =
          "ROUND",
        joinStyle = "ROUND"
      )
    if (gIntersects(ini, A2@sp[1,]) &&
        gIntersects(fim,A2@sp[length2,])&& tdiff1<tempo && tdiff2<tempo) {
      count = 0
      time  = 0
       print(A1@data$traj[[1]])
       print(A2@data$traj[[1]])
      timeSeries <- compare(A2,A1)

      #Contador para as conexoes
      i = 1;
      j = 1;
      for (n in 2:(length(timeSeries@track1)-1)) {
        if (timeSeries@conns1@data$time[i] < timeSeries@conns2@data$time[j]) {
          if (timeSeries@conns1@data$dists[i] > dist) {
            time = time + timeSeries@track1@connections$duration[n]
            if (time > tempo)
              return (FALSE)
          }
          i = i + 1
        }
        else if (timeSeries@conns1@data$time[i] > timeSeries@conns2@data$time[j]) {
          if (timeSeries@conns2@data$dists[j] > dist) {
            time = time + timeSeries@track1@connections$duration[n]
            if (time > tempo)
              return (FALSE)
          }
          j = j + 1
        }
        else if (!(timeSeries@conns1@data$time[i] < timeSeries@conns2@data$time[j]) &&
                 !(timeSeries@conns1@data$time[i] > timeSeries@conns2@data$time[j]))
        {
          if (timeSeries@conns2@data$dists[j] > dist) {
            time = time + timeSeries@track1@connections$duration[n]
            if (time > tempo)
              return (FALSE)
          }
          j = j + 1
          i = i + 1
        }

      }
      print("passei do compare")
      return (TRUE)
    }

    else
      return (FALSE)

  }
)
