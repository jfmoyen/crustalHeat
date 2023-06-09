### Global variables
# Backgrounds for geotherm graphs

#' Metamorphic facies grid, within the crust
#' @importFrom png readPNG
#' @export
faciesMeta <- list(
  image = readPNG(system.file("bg_images", "faciesMeta.png", package = "crustalHeat")),
  Tmin = -7,
  Tmax = 1180,
  zmin = 1,
  zmax = 73,
  Tpos = c(0,200,400,600,800,1000)
)

#' Metamorphic facies grid, ulra-metamorphism
#' @importFrom png readPNG
#' @export
faciesUltraMeta <- list(
  image = readPNG(system.file("bg_images", "faciesUltraMeta.png", package = "crustalHeat")),
  Tmin = -10,
  Tmax = 1195,
  zmin = -0.4,
  zmax = 149,
  Tpos = c(0,200,400,600,800,1000),
  zpos=c(120,90,60,30,0)
)

#' Reactions, pelitic system
#' @importFrom png readPNG
#' @export
React <- list(
  image = readPNG(system.file("bg_images", "React.png", package = "crustalHeat")),
  Tmin = 0,
  Tmax = 1164,
  zmin = 0,
  zmax = 70,
  Tpos = c(0,200,400,600,800,1000),
  zpos=c(60,50,40,30,20,10,0)
)

#' Mantle facies
#' @importFrom png readPNG
#' @export
Ride <- list(
  image = readPNG(system.file("bg_images", "Ride.png", package = "crustalHeat")),
  Tmin = 0,
  Tmax = 1987,
  zmin = 0,
  zmax = 183,
  Tpos = c(0,500,1000,1500),
  zpos=c(140,120,100,80,60,40,20,0)
)
