% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rang_korrelierer.R
\name{rang_korrelierer}
\alias{rang_korrelierer}
\title{A function for collecting and exporting data for further excel makro-based processing}
\usage{
rang_korrelierer(daten, modell, cutoff = 0.1, exclude = "none")
}
\arguments{
\item{daten}{the model you want to export data for. .lm files contain a reference to the original data frame whis i used here.}

\item{modell}{the model you have}

\item{cutoff}{cut off the results set at this COR value. Accepts the range of [-1:1]. Use this for shrinking down your results set. Defaults to 0.1, as CORs below this value are usually insignificant in your average weekly MMM model. Try higher values at the beginning of your modelling.}

\item{exclude}{accepts strings for any VARs that you do not want in your results set. Only accepts a single string, may work with "xx|yy|zz" expressions but untested. Plan to expand to accept lists.}
}
\description{
This function allows you to express your love of cats.
}
\examples{
rang_korrelierer()
}
\keyword{excel}
\keyword{export}
\keyword{to}
