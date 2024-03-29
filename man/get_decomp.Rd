% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_decomp.R
\name{get_decomp}
\alias{get_decomp}
\title{A function for rapid sales decomposition}
\usage{
get_decomp(fit = out)
}
\arguments{
\item{fit}{Input data. Just name the model you want to inspect}
}
\description{
This function allows you to quickly view the sales decomposition for your model.
NOTE: This function was orignally named preview() and was written entirely by Johannes Ritter, Senior Consultant at Annalect Hamburg
}
\examples{
get_decomp(my_default_model) ## use the object assignment operator ' <- ' to include the dummy into your dataset
preview()
}
\keyword{decomposition}
\keyword{preview}
\keyword{quick}
\keyword{sales}
