% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaxiModel.R
\name{hour_filter}
\alias{hour_filter}
\title{Different time zones corresponding to different traffic behaviour}
\usage{
hour_filter(heure)
}
\arguments{
\item{heure}{the time in basic format}
}
\value{
a string which will detail which time zone the input hour corresponds to (there are 5 per day, "0-5", "5-10")
}
\description{
Different time zones corresponding to different traffic behaviour
}
