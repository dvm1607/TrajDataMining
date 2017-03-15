.loadPackages <- function() {
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


  if (!require(geosphere, quietly = TRUE)) {
    stop('The package geosphere was not installed')
  }

  if (!require(doMC, quietly = TRUE)) {
    stop('The package doMC was not installed')
  }

  if (!require(parallel, quietly = TRUE)) {
    stop('The package parallel was not installed')
  }
}
