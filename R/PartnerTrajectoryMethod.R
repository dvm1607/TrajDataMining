setGeneric(
  name = "PartnerTrajectory",
  def = function(A1, A2, dist, tempo)
  {
    standardGeneric("PartnerTrajectory")
  }
)

setMethod(
  f = "PartnerTrajectory",
  signature = c("Track","Track","numeric","numeric"),
  definition = function(A1, A2, dist, tempo)
  {
    loadPackages()
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
        gIntersects(fim,A1@sp[length2,])) {
      count = 0
      time  = 0
      timeSeries <- compare(A1,A2)
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

      return (TRUE)
    }

    else
      return (FALSE)

  }
)
