


library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rgdal)
library(sp)
library(readr)
library(maptools)
library("rgdal")
library("lattice")
library(geosphere)
library(DataExplorer)
library(Hmisc)





largeur_cellule <- 500
train <- read.csv("train-000.csv", sep = ",")
test <- read.csv("data/test.csv", sep = ",")
url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nybb_13a.zip"
#head(train)



#' Clean dataframe
#'
#' @param train object of class dataset
#'
#' @return The dataframe which respects our specifications
#' @export
#'
#' @examples
cleaning <- function(train) {
  train_df <- train %>% filter(fare_amount >=0) %>% filter(pickup_longitude > -100, pickup_longitude < -50, pickup_latitude > 20, pickup_latitude < 60, dropoff_longitude > -100, dropoff_longitude < -50, dropoff_latitude > 20, dropoff_latitude < 60)
  return(train_df)
}

train <- cleaning(train)



#' Initial data exploration
#'
#' @param train objet of class dataset
#'
#' @return une suite de statistiques descriptives
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' data("destrains")
#' data_exloration("untrain")
#' }

data_exploration <- function (train) {
  # summary(train)
  plot(density(train$fare_amount))
  plot(train$fare_amount)
  abline(mean(train$fare_amount)+4*sd(train$fare_amount),0, col="blue")

  boxplot(train$fare_amount)
  hist(train$passenger_count, breaks = 8, col = sample(colours(), 10))
  plot_correlation(train)
  describe(train)
  train['abs_lat_diff'] <- abs(train['dropoff_latitude'] - train['pickup_latitude'])
  train['abs_lon_diff'] <- abs(train['dropoff_longitude'] - train['pickup_longitude'])
  train['cut'] <- cut2(train$fare_amount,seq(0,50, by=5))
  ggplot(train, aes(abs_lat_diff, abs_lon_diff)) +
    geom_point(aes(colour = factor(train$cut)))+
    xlim(0,1) +
    ylim(0,1)
}
data_exploration(train)




#' Title
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
proj_shp <- function (url) {
  shpurl <- url
  tmp    <- tempfile(fileext=".zip")
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
#' @param shp
#' @param train
#'
#' @return
#' @export
#'
#' @examples
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
#' @param shp
#' @param largeur_cellule
#'
#' @return
#' @export
#'
#' @examples
grid <- function (shp,largeur_cellule) {
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




shp <- proj_shp(url)
proj <- proj_cord(shp,train)
cord <- (proj_cord(shp,train))$cord
cord2 <- (proj_cord(shp,train))$cord2
spatial_grid <- grid(shp,largeur_cellule)
spplot(spatial_grid, "id",
       panel = function(...) {
         panel.gridplot(..., border="black")
         sp.polygons(shp)
         #panel.text(...)
       })
over(cord, spatial_grid)
summary(over(cord, spatial_grid))
# Plot coordinates
cord
plot(shp)
points(proj$cord2.coords.x1, proj$cord2.coords.x2, pch=19, col="red")





#' Return Id from our grid given latitude and longitude inputs
#'
#' @param long double representing the longitude
#' @param lat double representing the latitude
#' @param grid the spatialgrid of interest
#'
#' @return Id from our grid
#' @export
#'
#' @examples
over_2 <- function (long, lat, grid) {
  cord.dec = SpatialPoints(cbind(long, lat), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS(proj4string(shp)))
  cord <- data.frame(cord.UTM@coords, id="A", stringsAsFactors=F)
  cord2 <- cord
  coordinates(cord) <- ~ long + lat
  proj4string(cord) <- proj4string(shp)
  return(over(cord,grid))
}

