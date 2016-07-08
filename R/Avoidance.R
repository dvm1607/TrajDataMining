setGeneric(
  name = "Avoidance",
  def = function(tracks, objects, buffer , trajin)
  {
    loadPackages()
    standardGeneric("Avoidance")
  }
)

setMethod(
  f = "Avoidance",
  signature = c("TracksCollection","list", "numeric", "numeric"),
  definition = function(tracks, objects, buffer , trajin)
  {

    if (is.null(tracks)|| length(tracks) < 1){
      return (0)}

 ###Implementar o código do avoiadance

  }
)
