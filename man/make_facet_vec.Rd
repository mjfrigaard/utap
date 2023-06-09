% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_facet_cols.R
\name{make_facet_vec}
\alias{make_facet_vec}
\title{Create vector of facet columns by type (helper)}
\usage{
make_facet_vec(df, type)
}
\arguments{
\item{df}{a \code{data.frame} or \code{tibble}}

\item{type}{type of column to return}
}
\value{
vector of facet columns (< 5 levels)
}
\description{
Create vector of facet columns by type (helper)
}
\examples{
require(NHANES)
require(dplyr)
facets <- make_facet_vec(
  df = dplyr::select(
    NHANES::NHANES,
    dplyr::where(is.factor)),
  type = "fct")
facets
}
