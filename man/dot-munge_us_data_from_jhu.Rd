% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jhu_data.R
\name{.munge_us_data_from_jhu}
\alias{.munge_us_data_from_jhu}
\title{simple function to munge JHU US counties data into long-form tibble}
\usage{
.munge_us_data_from_jhu(subset)
}
\arguments{
\item{subset}{character(1) of Confirmed, Deaths}
}
\value{
a long-form tibble
}
\description{
This function takes one of two subsets--confirmed,
deaths--and munges.
}
\keyword{internal}
