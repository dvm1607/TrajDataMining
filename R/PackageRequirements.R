loadPackages <- function() {
  if (!require(rgeos, quietly = TRUE)) {
    stop('The package rgeos was not installed')
  }
  if (!require(rgdal, quietly = TRUE)) {
    stop('The package rgdal was not installed')
  }
  if (!require(trajectories, quietly = TRUE)) {
    stop('The package trajectories was not installed')
  }
  if (!require(spacetime, quietly = TRUE)) {
    stop('The package spacetime was not installed')
  }
  if (!require(sp, quietly = TRUE)) {
    stop('The package sp was not installed')
  }
  if (!require(TrajDataAccess, quietly = TRUE)) {
    stop('The package TrajDataAccess was not installed')
  }
}
