% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_col.R
\name{select_col}
\alias{select_col}
\title{A convenient selector gadget}
\usage{
select_col(df, ...)
}
\arguments{
\item{df}{The data set from which to select a column}

\item{...}{columns to select, no quotes}
}
\value{
Returns a dataframe with selected columns
}
\description{
A convenient selector gadget
}
\details{
A friendly way to select a column or several columns. Mainly for non-pipe usage
It is recommended to use known select functions to do pipe manipulations. Otherwise convert to tibble
}
\examples{
select_col(iris,Petal.Length,Sepal.Length,Species,Petal.Width)
# A pipe friendly example
\dontrun{
library(dplyr)
as_tibble(iris) \%>\%
select_col(Species)
}
}
