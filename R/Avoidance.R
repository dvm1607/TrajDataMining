setGeneric(
  name = "Avoidance",
  def = function(track, obj, buffer , trajin)
  {
    loadPackages()
    standardGeneric("Avoidance")
  }
)

setMethod(
  f = "Avoidance",
  signature = c("Track","SpatialPolygons", "numeric", "numeric"),
  definition = function(track, obj, buffer , trajin)
  {
    avoid = 0
    if (is.null(tracks)|| length(tracks) < 1){
      return (0)}

    if(gIntersects(track@sp,gBuffer(obj,width=buffer))){
               if(gIntersects(track@sp,obj)){
                   avoid=0
               }
               else if(x){##no if verificar se a intersecço ãatende o tamanho minimo
                       ##fazer o resto dos calculos
               }

    }
 ###Implementar o código do avoiadance
return (FALSE)##This is a placeholder
  }
)
