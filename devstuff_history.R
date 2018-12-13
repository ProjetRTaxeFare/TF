usethis::use_build_ignore("devstuff_history.R")

usethis::use_package("dplyr")
usethis::use_package("DataExplorer")
usethis::use_package("maptools")
usethis::use_package("readr")
usethis::use_package("utils")
usethis::use_package("rgdal")
usethis::use_package("sp")
usethis::use_package("purrr")
usethis::use_package("geosphere")

#remotes::install_github("ThinkR-open/attachment")
#attachment::att_to_description(dir.v = "")

usethis::use_pipe()

devtools::load_all()
spatial_grid <- main()
usethis::use_data(spatial_grid, overwrite = TRUE)

#data("iris")
#TFpackage::iris

file <- system.file("data_clean/train-000.csv", package = "TFpackage")
train <- read.csv(file, sep = ",")
train <- cleaning(train)
usethis::use_data(train, overwrite = TRUE)

file <- system.file("data_model/final_model_result_with_200000_data.csv", package = "TFpackage")
model_df <- read.csv(file, sep = ",")
usethis::use_data(model_df, overwrite = TRUE)

file <- system.file("data_model/train_transfo2.csv", package = "TFpackage")
train_transfo2 <- read.csv(file, sep = ",")
train_transfo2 <- train_transfo2 %>% select(-1,-2)
usethis::use_data(train_transfo2, overwrite = TRUE)

train_sample <- train[1:10,]
usethis::use_data(train_sample)

usethis::use_mit_license("Gautier Dulac")

