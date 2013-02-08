\name{rbf}
\alias{rbf}
\title{
Radial basis function kernel
}
\description{
Evaluate a kernel matrix based on the radial basis function kernel. Can be 
used in conjunction with \code{\link{cadence.fit}} with \code{hidden.fcn} 
set to \code{\link{identity}} and \code{sd.norm} set to a value less than 
infinity to implement a kernel CDEN model.
}
\usage{
rbf(x, x.basis, sigma)
}
\arguments{
  \item{x}{
      matrix with number of rows equal to the number of samples and number of columns equal to the number of predictors.
  }
  \item{x.basis}{
      matrix with number of rows equal to the number of basis functions and number of columns equal to the number of predictors.
  }
  \item{sigma}{
      kernel width
  }
}
\value{
      kernel matrix with number of rows equal to the number of samples and number of columns equal to the number of basis functions.
}
\seealso{
      \code{\link{cadence.fit}}
}
