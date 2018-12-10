

#' Keep the relevant variables
#'
#' @param list List coming from a line of our initial data frame, corresponds to a taxi ride.
#'       list[1] is the date of the trip (in basic date format)
#'       list[2] is the price of the trip
#'       list[4],list[5] are the latitudes and longitudes of the pickup point
#'       list[6],list[7] are the latitudes and longitudes of the dropoff point
#'       list[8] is the ID of the passenger
#' @param grid Our spatial grid
#'
#' @return A new data frame with the variables which interest us :
#'         longitude of pickup
#'         latitude of pickup
#'         longitude of dropoff
#'         latitude of dropoff
#'         id of the pickup case
#'         id of the dropoff case
#'         the day of the week
#'         hour
#'         price
#'         passenger id
#' @export
#'
#' @examples
#' transformation(train[2,],spatial_grid)
transformation <- function (list,grid){

  #We want to clean the data in order to use it efficiently later.

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



#'Different time zones corresponding to different traffic behaviour
#'
#' @param heure the time in basic format
#'
#' @return a string which will detail which time zone the input hour corresponds to (there are 5 per day, "0-5", "5-10")
#' @export
#'
#' @examples
#' hour_filter(13:08:24)
hour_filter <- function(heure){
  return(case_when(heure %in% 0:4 ~ "0-5",
                   heure %in% 5:9 ~ "5-10",
                   heure %in% 10:15 ~ "10-16",
                   heure %in% 16:20 ~ "16-21",
                   heure %in% 21:23 ~ "21-0"))
}




#' Final dataset for one trip
#'
#' @param pretty_row A row given by the function "transformation"
#'
#' @return A data frame line which corresponds to a trip and contains :
#'          ID, passenger, day of the week, time slot, precision and price
#' @export
#' @import geosphere
#' @import purrr
#' @import dplyr
#'
#' @examples
#' path(transformation(train[2,],spatial_grid))
path <- function(pretty_row) {

  precision <- 2

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



#' Whole dataframe with clean data
#'
#' @param df The dataframe containing all the initial information
#' @param grid The spatial grid we chose to work with
#'
#' @return creates a whole dataframe with clean data
#' @export
#' @import purrr
#'
#' @examples
#' good_dataframe(train[1;10,],spatial_grid)
good_dataframe<-function(df,grid){
  #creates a whole dataframe with clean data

  return(map_df(1:nrow(df),~transformation(df[.x,],grid)))

}

#' Whole dataframe error or final data
#'
#' @param good_df
#'
#' @return A whole dataframe containing errors or the final data
#' @export
#' @import purrr
#' @examples
#' discretisation_dataframe(good_dataframe(train[1;10,],spatial_grid))
discretisation_dataframe <- function(good_df) {
  return(map_df(1:nrow(good_df), ~ safely(path(good_df[.x,])[[1]])))
}


#' Data row to vector
#'
#' @param data_row a row from a data frame
#'
#' @return a vector containing the pickup and dropoff coordinates, week day, hour and passengers
#' @export
#'
#' @examples
#' TODO check this header
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



#' Prediction of taxi prices
#'
#' @param travel List (TODO vector?) which contains the informaton about a trip we want to make
#'              (coordinates, day, hour, passengers)
#' @return The estimated price of the trip
#' @export
#' @import geosphere
#' @import dplyr
#' @import purrr
#' @examples
#' predict(TODO)
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

#predict_raw_data <- compose(predict, transform_row)



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
