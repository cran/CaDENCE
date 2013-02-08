\name{bgamma}
\alias{bgamma}
\alias{dbgamma}
\alias{pbgamma}
\alias{qbgamma}
\alias{rbgamma}

\title{
Bernoulli-gamma distribution
}
\description{
Functions implementing the Bernoulli-gamma distribution, in which zero values
occur with probability \code{1-prob} and non-zero values follow a gamma
distribution with \code{scale} and \code{shape} parameters.
\code{dbgamma} gives a probability density function (pdf),
\code{pbgamma} gives the cumulative distribution
function (cdf), \code{qbgamma} gives the quantile function (inverse cdf),
and \code{rbgamma} is used for generating random variates.
}
\usage{
dbgamma(x, prob, scale, shape)
pbgamma(q, prob, scale, shape)
qbgamma(p, prob, scale, shape)
rbgamma(n, prob, scale, shape)
}
\arguments{
  \item{x, q}{
      vector of quantiles.
  }
  \item{p}{
      vector of cumulative probabilities.
  }
  \item{n}{
      number of random samples.
  }
  \item{prob}{
      probability of a non-zero value.
  }
  \item{scale}{
      scale parameter of the gamma distribution.
  }
  \item{shape}{
      shape parameter of the gamma distribution.
  }
}
\value{
  \code{dbgamma} gives the pdf, \code{pbgamma} gives the cdf,
  \code{qbgamma} gives the inverse cdf (or quantile function), and
  \code{rbgamma} generates random deviates.
}
\seealso{
  \code{\link{dgamma}}, \code{\link{bweibull}}, \code{\link{bpareto2}},
  \code{\link{blnorm}}
}
\examples{
  plot(rbgamma(365, prob = 0.2, scale = 1, shape = 1), type = "h")
}
\references{
  Cannon, A.J., 2008. Probabilistic multi-site precipitation downscaling
  by an expanded Bernoulli-gamma density network. Journal of
  Hydrometeorology, 9(6): 1284-1300.
}
