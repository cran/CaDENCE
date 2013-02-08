\name{cadence.cost}
\alias{cadence.cost}
\title{
Cost function for CDEN model fitting
}
\description{
The maximum likelihood cost function used for CDEN model fitting. 
Calculates the negative of the logarithm of the likelihood. A normal 
distribution prior can be set for the magnitude of the input-hidden 
layer weights, thus leading to weight penalty regularization.
}
\usage{
cadence.cost(weights, x, y, n.hidden, hidden.fcn, distribution, sd.norm,
             valid)
}
\arguments{
  \item{weights}{
      weight vector of length returned by \code{\link{cadence.initialize}}.
}
  \item{x}{
      matrix with number of rows equal to the number of samples and number of columns equal to the number of predictor variables.
}
  \item{y}{
      column matrix of predictand values with number of rows equal to the number of samples.
}
  \item{n.hidden}{
      number of hidden nodes in the CDEN model.
}
  \item{hidden.fcn}{
      hidden layer transfer function.
}
  \item{distribution}{
      a list that describes the probability density function associated with the predictand.
}
  \item{sd.norm}{
      \code{sd} parameter for normal distribution prior for the magnitude of input-hidden layer weights; equivalent to weight penalty regularization.
}
  \item{valid}{
      \code{valid} logical vector indicating which weights are non-zero or fixed at zero, i.e., due to use of \code{parameters.fixed} in \code{distribution}.
}
}
\seealso{
    \code{\link{cadence.fit}}, \code{\link{optim}}, \code{\link{rprop}}
}
