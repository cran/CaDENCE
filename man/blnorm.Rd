\name{blnorm}
\alias{blnorm}
\alias{dblnorm}
\alias{pblnorm}
\alias{qblnorm}
\alias{rblnorm}

\title{
Bernoulli-lognormal distribution
}
\description{
Functions implementing the Bernoulli-lognormal distribution, in which zero values
occur with probability \code{1-prob} and non-zero values follow a lognormal
distribution with \code{meanlog} and \code{sdlog} parameters.
\code{dblnorm} gives a probability density function (pdf),
\code{pblnorm} gives the cumulative distribution
function (cdf), \code{qblnorm} gives the quantile function (inverse cdf),
and \code{rblnorm} is used for generating random variates.
}
\usage{
dblnorm(x, prob, meanlog, sdlog)
pblnorm(q, prob, meanlog, sdlog)
qblnorm(p, prob, meanlog, sdlog)
rblnorm(n, prob, meanlog, sdlog)
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
  \item{meanlog}{
      meanlog parameter of the lognormal distribution.
  }
  \item{sdlog}{
      sdlog parameter of the lognormal distribution.
  }
}
\value{
  \code{dblnorm} gives the pdf, \code{pblnorm} gives the cdf,
  \code{qblnorm} gives the inverse cdf (or quantile function), and
  \code{rblnorm} generates random variates.
}
\seealso{
  \code{\link{dlnorm}}, \code{\link{bweibull}}, \code{\link{bpareto2}},
  \code{\link{bgamma}}
}
\examples{
  plot(rblnorm(365, prob = 0.2, meanlog = 1, sdlog = 1), type = "h")
}
