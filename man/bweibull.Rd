\name{bweibull}
\alias{bweibull}
\alias{dbweibull}
\alias{pbweibull}
\alias{qbweibull}
\alias{rbweibull}

\title{
Bernoulli-Weibull distribution
}
\description{
Functions implementing the Bernoulli-Weibull distribution, in which zero values
occur with probability \code{1-prob} and non-zero values follow a Weibull
distribution with \code{scale} and \code{shape} parameters.
\code{dbweibull} gives a probability density function (pdf),
\code{pbweibull} gives the cumulative distribution
function (cdf), \code{qbweibull} gives the quantile function (inverse cdf),
and \code{rbweibull} is used for generating random variates.
}
\usage{
dbweibull(x, prob, scale, shape)
pbweibull(q, prob, scale, shape)
qbweibull(p, prob, scale, shape)
rbweibull(n, prob, scale, shape)
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
      scale parameter of the weibull distribution.
  }
  \item{shape}{
      shape parameter of the weibull distribution.
  }
}
\value{
  \code{dbweibull} gives the pdf, \code{pbweibull} gives the cdf,
  \code{qbweibull} gives the inverse cdf (or quantile function), and
  \code{rbweibull} generates random variates.
}
\seealso{
  \code{\link{dweibull}}, \code{\link{bgamma}}, \code{\link{bpareto2}},
  \code{\link{blnorm}}
}
\examples{
  plot(rbweibull(365, prob = 0.2, scale = 1, shape = 1), type = "h")
}
