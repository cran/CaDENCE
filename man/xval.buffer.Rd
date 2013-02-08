\name{xval.buffer}
\alias{xval.buffer}
\title{
Cross-validation indices with a buffer between training/validation datasets
}
\description{
Caculates training/validation indices for N-fold cross-validation of a dataset.
Cross-validation folds are taken as contiguous blocks of cases with an optional
buffer to prevent leakage of information between training/validation subsets
due to the presence of autocorrelation.
}
\usage{
xval.buffer(n.cases, n.xval=5, buffer.length=0)
}
\arguments{
  \item{n.cases}{
      an integer specifying the length of the dataset.
}
  \item{n.xval}{
      an integer specifying the desired number of cross-validation folds.
}
  \item{buffer.length}{
      an integer specifying the number of cases to be left out as a buffer
      between the training/validation subsets.
}

}
\value{
  a list with \code{n.xval} elements, each containing:
    \item{train}{indices of training cases}
    \item{valid}{indices of validation cases}
}
\examples{
  print(xval.buffer(100, n.xval = 3, buffer.length = 10))
}
\references{
Shabbar, A. and V. Kharin. 2007. An assessment of cross-validation
for estimating skill of empirical seasonal forecasts using a
global coupled model simulation. CLIVAR Exchanges. 12(4): 10-12.

Zeng, Z., W.W. Hsieh, A. Shabbar, and W.W. Burrows, 2011. Seasonal
prediction of winter extreme precipitation over Canada by
support vector regression, Hydrology and Earth System Sciences, 15: 65-74.
}
