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

remotes::install_github("ThinkR-open/attachment")
attachment::att_to_description(dir.v = "")

usethis::use_pipe()

devtools::load_all()
spatial_grid <- main()
usethis::use_data(spatial_grid)

usethis::use_data(iris)

data("iris")
TFpackage::iris

file <- system.file("data_clean/train-000.csv", package = "TFpackage")
train <- read.csv(file, sep = ",")
usethis::use_data(train)

train_sample <- train[1:10,]
usethis::use_data(train_sample)
