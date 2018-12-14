# No Remotes ----
# Attachments ----
to_install <- c("DataExplorer", "dplyr", "geosphere", "ggmap", "ggplot2", "googleway", "Hmisc", "knitr", "lattice", "lubridate", "magrittr", "maptools", "placement", "purrr", "readr", "rgdal", "shiny", "sp", "testthat", "tidyr", "utils")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }

  }