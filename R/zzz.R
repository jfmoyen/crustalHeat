#' @importFrom png readPNG

.onLoad<-function(libname, pkgname){

  ## Read graph backgrounds

  faciesMeta <- list(
    image = readPNG(system.file("bg_images", "faciesMeta.png", package = "crustalHeat")),
    Tmin = -7,
    Tmax = 1180,
    zmin = 1,
    zmax = 73,
    Tpos = c(0,200,400,600,800,1000)
  )
  assign("faciesMeta", faciesMeta, envir = parent.env(environment()))

  faciesUltraMeta <- list(
    image = readPNG(system.file("bg_images", "faciesUltraMeta.png", package = "crustalHeat")),
    Tmin = -10,
    Tmax = 1195,
    zmin = -0.4,
    zmax = 149,
    Tpos = c(0,200,400,600,800,1000),
    zpos=c(120,90,60,30,0)
  )
  assign("faciesUltraMeta", faciesUltraMeta, envir = parent.env(environment()))


  React <- list(
    image = readPNG(system.file("bg_images", "React.png", package = "crustalHeat")),
    Tmin = 0,
    Tmax = 1164,
    zmin = 0,
    zmax = 70,
    Tpos = c(0,200,400,600,800,1000),
    zpos=c(60,50,40,30,20,10,0)
  )
  assign("React", React, envir = parent.env(environment()))


  Ride <- list(
    image = readPNG(system.file("bg_images", "Ride.png", package = "crustalHeat")),
    Tmin = 0,
    Tmax = 1987,
    zmin = 0,
    zmax = 183,
    Tpos = c(0,500,1000,1500),
    zpos=c(140,120,100,80,60,40,20,0)
  )
  assign("Ride", Ride, envir = parent.env(environment()))

}

.onUnload<-function(libpath){
  rm(c("faciesMeta","faciesUltraMeta","React","Ride"),envir = parent.env(environment()) )
}

