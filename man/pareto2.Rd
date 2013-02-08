\name{pareto2}
\alias{pareto2}
\alias{bpareto2}
\alias{dbpareto2}
\alias{pbpareto2}
\alias{qbpareto2}
\alias{rbpareto2}
\alias{dpareto2}
\alias{ppareto2}
\alias{qpareto2}
\alias{rpareto2}

\title{
Pareto 2 (Lomax) and Bernoulli-Pareto 2 distributions
}
\description{
Functions implementing the Pareto 2 (Lomax) and Bernoulli-Pareto 2 distributions. 
In the latter case, zero values occur with probability \code{1-prob} and
non-zero values follow the Pareto 2 distribution with \code{scale} and
\code{shape} parameters. \code{dpareto2} and \code{dbpareto2} give the probability density
functions (pdf); \code{ppareto2} and \code{pbpareto2} give the cumulative distribution
functions (cdf); \code{qpareto2} and \code{qbpareto2} give the quantile functions (inverse cdfs),
and \code{rpareto2} and \code{rbpareto2} are used for generating random variates.
}
\usage{
dpareto2(x, scale, shape)
ppareto2(q, scale, shape)
qpareto2(p, scale, shape)
rpareto2(n, scale, shape)
dbpareto2(x, prob, scale, shape)
pbpareto2(q, prob, scale, shape)
qbpareto2(p, prob, scale, shape)
rbpareto2(n, prob, scale, shape)
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
      scale parameter of the pareto2 distribution.
  }
  \item{shape}{
      shape parameter of the pareto2 distribution.
  }
}
\value{
  \code{dpareto2} and \code{dbpareto2} gives the pdfs;
  \code{ppareto2} and \code{pbpareto2} gives the cdfs;
  \code{qpareto2} and \code{qbpareto2} gives the inverse cdfs
  (or quantile functions); and \code{rpareto2} and
  \code{rbpareto2} generate random variates.
}
\seealso{
  \code{\link{bgamma}}, \code{\link{bweibull}}, \code{\link{blnorm}}
}
\examples{
  plot(rbpareto2(365, prob = 0.2, scale = 1, shape = 1), type = "h")
}
\references{
Arnold, B.C., 1983. The Pareto Distributions, International Co-operative
Publishing House, Fairland, MD.

Lomax, K.S., 1954. Business failures: another example of the analysis of failure
data. Journal of the American Statistical Association, 49(268): 847-852.
}
