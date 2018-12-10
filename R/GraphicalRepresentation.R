#' Deletion of location that are not in New york and its suburbs
#'
#' @param train object of class dataset
#'
#' @return A new dataframe with locations of interest as observation
#' @export
#' @import dplyr
#'
cleaning <- function(train) {
  train_df <- train %>% filter(fare_amount >=0) %>%
    filter(pickup_longitude > -100, pickup_longitude < -50, pickup_latitude > 20,
           pickup_latitude < 60, dropoff_longitude > -100, dropoff_longitude < -50,
           dropoff_latitude > 20, dropoff_latitude < 60)
  return(train_df)
}


#' Title
#'
#' @param url Information online about NYC
#'
#' @return the shapefile
#' @export
#' @import maptools
#' @import utils
#' @import rgdal
proj_shp <- function (url) {
  url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nybb_13a.zip"
  shpurl <- url
  tmp <- tempfile(fileext=".zip")
  download.file(shpurl, destfile=tmp)
  files <- unzip(tmp, exdir=getwd())
  # Load & plot shapefile
  shp <- readShapePoly(files[grep(".shp$", files)])
  ### read shapefile
  shp <- readOGR("nybb_13a", "nybb")
  proj4string(shp)

  return(shp)
}



#' Title
#'
#' @param shp shapefile
#' @param train the clean version from the cleaning function
#'
#' @return Useful result to plot our data on the good grid
#' @export
#' @import sp
#'
proj_cord <- function (shp, train) {

  cord.dec = SpatialPoints(cbind(train$pickup_longitude, train$pickup_latitude), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS(proj4string(shp)))
  cord.UTM
  cord <- data.frame(cord.UTM@coords, id="A", stringsAsFactors=F)
  cord2 <- cord
  coordinates(cord) <- ~ coords.x1 + coords.x2
  proj4string(cord) <- proj4string(shp)
  cord@coords

  return(c(cord=cord,cord2=cord2))
}




### SpatialGrid object
#' Title
#'
#' @param shapefile Geographical object for NYC
#' @param largeur_cellule Parameter to know how precise we cut our travel in our cells
#'
#' @return Final spatial grid used in our functions to create the data set model
#' @export
#' @import sp
#'
grid <- function (shp,largeur_cellule = 500) {
  bb <- bbox(shp)
  cellsize <- c(3.28084, 3.28084)*largeur_cellule  # cell size 1000m
  # 1 ft = 3.28084 m
  cc <- bb[, 1] + (cellsize/2)  # cell offset
  cd <- ceiling(diff(t(bb))/cellsize)  # number of cells per direction
  grd <- GridTopology(cellcentre.offset=cc, cellsize=cellsize, cells.dim=cd)
  spatial_grid <- SpatialGridDataFrame(grd,
                                       data=data.frame(id=1:prod(cd)),
                                       proj4string=CRS(proj4string(shp)))
  return(spatial_grid)
}


#' Return the Spatial Grid that will serve as the domain
#'
#' @param
#'
#' @return The spatial grid from scratch to save it in our data folder to be used in the model side
#' @export
#'
main <- function(){
  largeur_cellule <- 500
  file <- system.file("data_clean/train-000.csv", package = "TFpackage")
  train <- read.csv(file, sep = ",")
  train <- cleaning(train)
  shp <- proj_shp("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nybb_13a.zip")
  spatial_grid <- grid(shp,largeur_cellule)
  return(spatial_grid)
}






#' Return Id from our grid given latitude and longitude inputs
#'
#' @param long double representing the longitude
#' @param lat double representing the latitude
#' @param grid the spatialgrid of interest
#'
#' @return Id from our grid
#' @export
#'
over_2 <- function (long, lat, grid) {
  cord.dec = SpatialPoints(cbind(long, lat), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS(proj4string(shp)))
  cord <- data.frame(cord.UTM@coords, id="A", stringsAsFactors=F)
  cord2 <- cord
  coordinates(cord) <- ~ long + lat
  proj4string(cord) <- proj4string(shp)
  return(over(cord,grid))
}

