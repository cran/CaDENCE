\name{cadence.predict}
\alias{cadence.predict}
\alias{cadence.evaluate}
\title{
Predict conditional distribution parameters from a fitted CDEN model
}
\description{
Predict conditional distribution parameters from a fitted CDEN model.
The returned value is a matrix with columns corresponding to the parameters
of the probability distribution specified in the \code{distribution}
argument passed to \code{\link{cadence.fit}}.
}
\usage{
cadence.predict(x, fit)
}
\arguments{
  \item{x}{
      matrix with number of rows equal to the number of samples and number of columns equal to the number of predictor variables.
}
  \item{fit}{
      list returned by \code{\link{cadence.fit}}.
}
}
\value{
a matrix with number of rows equal to that of \code{x} and columns
corresponding to the parameters of the \code{distribution}
argument passed to \code{\link{cadence.fit}}.
}
\seealso{
  \code{\link{cadence.fit}}, \code{\link{optim}}, \code{\link{rprop}}
}
\examples{
    data(FraserSediment)
    lnorm.distribution.fixed <- list(density.fcn = dlnorm,
                                     parameters = c("meanlog", "sdlog"),
                                     parameters.fixed = "sdlog",
                                     output.fcns = c(identity, exp))
    fit <- cadence.fit(x = FraserSediment$x.1970.1976,
                       y = FraserSediment$y.1970.1976,
                       hidden.fcn = identity, maxit.Nelder = 100,
                       trace.Nelder = 1, trace = 1,
                       distribution = lnorm.distribution.fixed)
    pred <- cadence.predict(x = FraserSediment$x.1977.1979, fit = fit)
    matplot(pred, type = "l")
}
