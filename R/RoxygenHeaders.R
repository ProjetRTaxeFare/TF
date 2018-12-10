



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
#' @param train objet of class dataset
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




#VERSION 2 - En utilisant la sortie de la fonction de VIGGIANO suivante :
#' Title
#'
#' @param list
#' @param grid
#'
#' @return
#' @export
#'
#' @examples
transformation <- function (list,grid){

  #We want to clean the data in order to use it efficiently late.

  #Calling the variables
  key <- list[[1]]
  price <- as.numeric(list[2])
  long1 <- as.numeric(list[4])
  lat1 <- as.numeric(list[5])
  long2 <-as.numeric( list[6])
  lat2 <- as.numeric(list[7])
  passenger <- as.numeric(list[8])

  date_final <- as.POSIXlt(key)
  hour <- date_final$hour
  day <- date_final$mday
  month <- date_final$mon
  year <- date_final$year
  week_day <- date_final$wday

  #Getting the id of the different squares
  case_depart <- as.numeric(over_2(long1,lat1,grid))
  case_arrivee <- as.numeric(over_2(long2,lat2,grid))

  return(data.frame(

    long1,lat1,long2,lat2,

    case_depart,case_arrivee,week_day,hour,price,passenger))



}



#' Title
#'
#' @param heure
#'
#' @return
#' @export
#'
#' @examples
hour_filter <- function(heure){
  return(case_when(heure %in% 0:4 ~ "0-5",
                   heure %in% 5:9 ~ "5-10",
                   heure %in% 10:15 ~ "10-16",
                   heure %in% 16:20 ~ "16-21",
                   heure %in% 21:23 ~ "21-0"))
}





precision <- 2
#Fonction qui donne le dataset final mais que pour un trajet initial (de A à B)
#' Title
#'
#' @param pretty_row
#'
#' @return
#' @export
#'
#' @examples
path <- function(pretty_row) {
  coordA <- c(pretty_row[[1]],pretty_row[[2]])
  coordB <- c(pretty_row[[3]],pretty_row[[4]])
  IDA <- pretty_row[[5]]
  IDB <- pretty_row[[6]]
  jour <- pretty_row[[7]]
  heure <- pretty_row[[8]]
  prix_parcours <- pretty_row[[9]]
  passagers <- pretty_row[[10]]

  #on calcule le pas vectoriel et le nb d'itération :
  distance <- distm(coordA,coordB)
  niteration = as.integer(distance / (largeur_cellule/precision))
  pas = 1/niteration
  pas_vectoriel <- as.vector((coordB-coordA)*pas)

  infos <- 1:niteration
  etapes_coords <- map(infos, ~ coordA + pas_vectoriel*.x)
  etapes_Ids <- map(etapes_coords, ~ over_2(.x[[1]],.x[[2]],spatial_grid)[[1,1]])

  etapes_df <- do.call(rbind.data.frame, etapes_Ids)
  names(etapes_df)[1] <- "IDs"

  final_df <- etapes_df %>% mutate("Prix" = prix_parcours/niteration, "Passagers" = passagers, "Jour" = jour, "Heure" = heure) %>% mutate("Creneau_Horaire" = hour_filter(heure))

  final_df <- final_df %>%
    group_by(IDs, Passagers, Jour, Creneau_Horaire) %>%
    summarise(precision = n(), Prix = sum(Prix)) %>%
    ungroup()
  return(final_df)
}
safe_path <- safely(path)


##Création de la base modèle


#train_test <- train
#' Title
#'
#' @param df
#' @param grid
#'
#' @return
#' @export
#'
#' @examples
good_dataframe<-function(df,grid){
  #creates a whole dataframe with clean data

  return(map_df(1:nrow(df),~transformation(df[.x,],grid)))

}
#test <- good_dataframe(train_test,spatial_grid)
#summary(test)
#' Title
#'
#' @param good_df
#'
#' @return
#' @export
#'
#' @examples
discretisation_dataframe <- function(good_df) {
  return(map_df(1:nrow(good_df), ~ safe_path(good_df[.x,])[[1]]))
}
#test_2 <- discretisation_dataframe(test)
#test_2_gpby <- test_2 %>% group_by(IDs, Passagers, Jour, Creneau_Horaire) %>% summarise(precision = sum(precision), prix_moyen = mean(Prix)) %>% filter(is.na(IDs) == FALSE)
total_test <- discretisation_dataframe(good_dataframe(train,spatial_grid)) %>% group_by(IDs, Passagers, Jour, Creneau_Horaire) %>% summarise(precision = sum(precision), prix_moyen = mean(Prix)) %>% filter(is.na(IDs) == FALSE)
write.csv(total_test, file = "final_model_result_with_200000_data.csv")


##Calcul du prix :


model_df <- total_test
ajout_precision <- 10
#' Title
#'
#' @param data_row
#'
#' @return
#' @export
#'
#' @examples
transform_row <- function(data_row) {
  xA <- as.numeric(data_row[[4]])
  yA <- as.numeric(data_row[[5]])
  xB <- as.numeric(data_row[[6]])
  yB <- as.numeric(data_row[[7]])
  key <- data_row[[3]]
  date_final <- as.POSIXlt(key)
  hour <- date_final$hour
  week_day <- date_final$wday
  passagers <- as.numeric(data_row[[8]])
  return(c(xA,yA,xB,yB,week_day,hour,passagers))
}



#' Title
#'
#' @param travel
#'
#' @return
#' @export
#'
#' @examples
predict <- function(travel) {
  #Data :
  coordA <- c(travel[[1]],travel[[2]])
  coordB <- c(travel[[3]],travel[[4]])
  jour <- travel[[5]]
  heure <- travel[[6]]
  passagers <- travel[[7]]

  #Calculs :
  creneau <- hour_filter(heure)


  #on calcule le pas vectoriel et le nb d'itération :
  distance <- distm(coordA,coordB)
  if (distance == 0) {
    return(c(0.,1.))
  } else {
    niteration = as.integer(distance / (largeur_cellule/(ajout_precision*precision)))
    pas = 1/niteration
    pas_vectoriel <- as.vector((coordB-coordA)*pas)

    infos <- 1:niteration
    etapes_coords <- map(infos, ~ coordA + pas_vectoriel*.x)
    etapes_Ids <- map(etapes_coords, ~ over_2(.x[[1]],.x[[2]],spatial_grid)[[1,1]])

    etapes_df <- do.call(rbind.data.frame, etapes_Ids)
    names(etapes_df)[1] <- "IDs"

    calcul_df <- etapes_df %>% mutate("Passagers" = passagers, "Jour" = jour, "Creneau_Horaire" = creneau)

    calcul_df <- calcul_df %>%
      group_by(IDs, Passagers, Jour, Creneau_Horaire) %>%
      summarise(nb = n()) %>%
      ungroup() %>% left_join(model_df, by = c("IDs", "Jour", "Creneau_Horaire", "Passagers"))

    missing_data <- sum(as.numeric(is.na(calcul_df$prix_moyen)) * calcul_df$nb)

    final_price <- sum(calcul_df$prix_moyen * calcul_df$nb /(precision*ajout_precision), na.rm = TRUE) * (1+missing_data/niteration)

    return(final_price)#, missing_data/niteration)) }
  }}
predict_raw_data <- compose(predict, transform_row)



#La suite a pour seul but de vérifier que notre test est plus adapté qu'une simple régression linéaire en fonction de la distance.
##Final test
#test_base_init <- read_csv("train-001.csv")
#train_lm <- train %>% rowwise() %>% mutate(distance = distm(c(pickup_longitude, #pickup_latitude),c(dropoff_longitude,dropoff_latitude))[1,1])
#
##À tester :
#test_base <- test_base_init[sample(200000,10),]
#
##safe_predict_raw_data <- safely(predict_raw_data)
#
#preds <- apply(test_base,1,predict_raw_data)
#
#preds_df <- data.frame(preds)
#data.frame(preds[[1]])
#test_pred <- test_base %>% mutate(pred = as.numeric(preds_df[[1]]))
#
#test_pred$ID = row.names(test_pred)
#final_test_plot <- test_pred  %>% gather(key = "Real_vs_Pred", value = "Cost", fare_amount, pred) %>% select(-1:-8)
#
#ggplot(data = final_test_plot , aes(x = ID, y = Cost, color = Real_vs_Pred)) + geom_point()
#
#
#ggplot(data = test_pred %>% mutate(diff = fare_amount - pred), aes(x = ID, y = diff)) + geom_point()
#
#test_pred <- test_pred %>% mutate(diff = fare_amount - pred)
#
#res.lm <- lm(fare_amount ~ distance, data = train_lm)
#summary(res.lm)
#test_pred <- test_pred %>% rowwise() %>% mutate(lm_pred = res.lm$coefficients[1] + res.lm$coefficients[2] * #distm(c(pickup_longitude, pickup_latitude),c(dropoff_longitude,dropoff_latitude))[1,1])
#
#test_pred <- test_pred %>% mutate(diff_lm = fare_amount - lm_pred)
#
#test_pred_plot <- test_pred %>% gather(key = "Real_vs_Pred", value = "Cost", diff, diff_lm) %>% select(-1:-10)
#
#
#ggplot(data = test_pred_plot , aes(x = ID, y = Cost, color = Real_vs_Pred)) + geom_point()
#
#mean(test_pred$diff)
#mean(test_pred$diff_lm)
#
#ggplot(data = test_pred_plot, aes(x = Real_vs_Pred, y = Cost)) + geom_boxplot()
