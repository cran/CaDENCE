\name{FraserSediment}
\alias{FraserSediment}
\docType{data}
\title{
Sediment and stream discharge data for Fraser River at Hope
}
\description{
A dataset consisting of daily observations of suspended sediment concentration
(SSC) (mg/L) and stream discharge (Q) (cu. m/s) for the years 1970-1979 at the
Fraser River at Hope station in British Columbia, Canada (Water Survey of
Canada station 08MF005). Samples are split into a seven year training period
(1970-1976) and a three year testing period (1977-1979).

In terms of structure, \code{FraserSediment} is a list with four elements:
\code{x.1970.1976}, \code{y.1970.1976}, \code{x.1977.1979},
and \code{y.1977.1979}. \code{x.1970.1976} and \code{x.1977.1979} are
matrices with predictor variables: \code{logQ}, log-transformed Q; and
\code{dQ5}, \code{dQ30}, and \code{dQ90}, 5-, 30-, and 90-day moving averages
of daily changes in Q. \code{y.1970.1976} and \code{y.1977.1979} are matrices
with the predictand variable \code{SSC}.
}
\examples{
data(FraserSediment)
pairs(cbind(FraserSediment$x.1970.1976, FraserSediment$y.1970.1976))
}
\keyword{datasets}
