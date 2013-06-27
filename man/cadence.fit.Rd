\name{cadence.fit}
\alias{cadence.fit}

\title{
Fit a CDEN model
}
\description{
Fit a CDEN model via nonlinear optimization of the maximum likelihood cost
function.
}
\details{
Fit a CDEN model by optimizing the maximum likelihood cost function
\code{f.cost}, which is set by default to \code{\link{cadence.cost}}.
Optimization relies on the standard \code{\link{optim}} function, the
built-in \code{\link{rprop}} function, or, optionally,
the \code{\link[pso]{psoptim}} function from the \code{pso} package.

The hidden layer transfer function \code{hidden.fcn} should be set to
\code{\link{tanh}} for a nonlinear model and to \code{\link{identity}} for a
linear model. In the nonlinear case, the number of hidden nodes \code{n.hidden}
controls the overall complexity of the model. The predictand distribution
is set by the \code{distribution} argument. Parameters of the specified
distribution can be held constant via the \code{parameters.fixed} element
\code{distribution}. Weight penalty regularization for the magnitude of the
input-hidden layer weights can be applied by setting \code{sd.norm} to a value
less than \code{Inf}.

The \code{distribution} argument in \code{cadence.fit} is the most important
part of the \code{CaDENCE} modelling framework and has been designed to be
as flexible as possible. To this end, \code{distribution} is a list with three
mandatory elements: \code{density.fcn}, which specifies the R density function
for the predictand distribution; \code{parameters}, which specifies the names
of the parameters used as arguments in \code{density.fcn}; and
\code{output.fcns}, which specifies the functions used to constrain the density
function parameters to their allowable ranges (i.e., inverse link
functions). If not specified, \code{distribution} defaults to a normal
distribution. Note: the order of \code{parameters} and \code{output.fcns} must
match the order of arguments in the specified \code{density.fcn}.

A fourth element of \code{distribution}, \code{parameters.fixed}, is optional.
Setting \code{parameters.fixed}\code{="sd"} for the normal distribution would, for
example, force the \code{sd} parameter to take a constant value.

Samples of \code{distribution} lists for a variety of probability distributions
are given below for reference:
\preformatted{
# normal distribution
norm.distribution <- list(density.fcn = dnorm,
                          parameters = c("mean", "sd"),
                          parameters.fixed = NULL,
                          output.fcns = c(identity, exp))

# lognormal distribution
lnorm.distribution <- list(density.fcn = dlnorm,
                           parameters = c("meanlog", "sdlog"),
                           parameters.fixed = NULL,
                           output.fcns = c(identity, exp))

# exponential distribution
exp.distribution <- list(density.fcn = dexp,
                         parameters = c("rate"),
                         parameters.fixed = NULL,
                         output.fcns = c(exp))

# Poisson distribution
poisson.distribution <- list(density.fcn = dpois,
                             parameters = c("lambda"),
                             parameters.fixed = NULL,
                             output.fcns = c(exp))

# Bernoulli-gamma distribution
bgamma.distribution <- list(density.fcn = dbgamma,
                            parameters = c("prob", "scale", "shape"),
                            parameters.fixed = NULL,
                            output.fcns = c(logistic, exp, exp))

# Bernoulli-Weibull distribution
bweibull.distribution <- list(density.fcn = dbweibull,
                              parameters = c("prob", "scale", "shape"),
                              parameters.fixed = NULL,
                              output.fcns = c(logistic, exp, exp))

# Bernoulli-lognormal distribution
blnorm.distribution <- list(density.fcn = dblnorm,
                            parameters = c("prob", "meanlog", "sdlog"),
                            parameters.fixed = NULL,
                            output.fcns = c(logistic, identity, exp))

# Bernoulli-Pareto 2 distribution
bpareto2.distribution <- list(density.fcn = dbpareto2,
                          parameters = c("prob", "scale", "shape"),
                          parameters.fixed = NULL,
                          output.fcns = c(logistic, exp, exp))

# beta distribution
beta.distribution <- list(density.fcn=dbeta,
                          parameters=c("shape1", "shape2"),
                          parameters.fixed=NULL,
                          output.fcns=c(exp, exp))

# truncated normal distribution with lower = 0
library(msm)
dtnormal <- function(x, mean, sd) dtnorm(x, mean, sd, lower = 0)
dtnorm.distribution <- list(density.fcn = dtnormal,
                            parameters = c("mean", "sd"),
                            parameters.fixed = NULL,
                            output.fcns = c(identity, exp))

# mixture of two normal distributions (mixture density network)
library(nor1mix)
dnormix <- function(x, mu1, mu2, sig1, sig2, w1){
    if(length(x) > 1){
        dens <- mapply(dnormix, x, mu1 = mu1, mu2 = mu2,
                       sig1 = sig1, sig2 = sig2, w1 = w1)
    } else{
        mix <- norMix(mu = c(mu1, mu2), sigma = c(sig1, sig2),
                      w = c(w1, 1-w1))
        dens <- dnorMix(x, mix)
    }
        dens
}
normix.distribution <- list(density.fcn = dnormix,
                            parameters = c("mu1", "mu2", "sig1",
                                           "sig2", "w1"),
                            parameters.fixed = NULL,
                            output.fcns = c(identity, identity,
                                            exp, exp, logistic))
}

Values of the Akaike information criterion with small sample size correction
(AICc), and Bayesian information criterion (BIC) are calculated to assist in
model selection. It is possible for such criteria to fail in the face of
overfitting, for example with a nonlinear model and \code{n.hidden} set too
high, as the distribution may converge on one or more samples. This can usually
be diagnosed by inspecting the scale parameter of the distribution for near
zero values. In this case, one can apply a weight penalty (via \code{sd.norm}),
although this rules out the straightforward use of AICc/BIC for model
selection as the effective number of model parameters will no longer equal the
number of weights in the CDEN model.

Note: values of \code{x} need not be standardized or rescaled by the user.
Predictors are automatically scaled to zero mean and unit standard deviation
and are rescaled by \code{\link{cadence.predict}}.

}

\usage{
cadence.fit(x, y, iter.max = 500, n.hidden = 2, hidden.fcn = tanh,
            distribution = NULL, sd.norm = Inf, init.range = c(-0.5, 0.5),
            method = c("optim", "psoptim", "Rprop"), n.trials = 1,
            trace = 0, maxit.Nelder = 2000, trace.Nelder = 0,
            swarm.size = NULL, vectorize = TRUE,
            delta.0 = 0.1, delta.min = 1e-06, delta.max = 50, epsilon = 1e-08,
            range.mult = 2, step.tol = 1e-08, f.target = -Inf,
            f.cost = cadence.cost, max.exceptions = 500)
}
\arguments{
  \item{x}{
      matrix with number of rows equal to the number of samples and number of columns equal to the number of predictor variables.
}
  \item{y}{
      column matrix of predictand values with number of rows equal to the number of samples.
}
  \item{iter.max}{
      maximum number of iterations of the optimization function.
}
  \item{n.hidden}{
      number of hidden nodes in the CDEN model; can be a vector indicating a range of values to fit.
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
  \item{init.range}{
      range for random weights on [\code{min(init.range)}, \code{max(init.range)}]
}
  \item{method}{
      specifies the optimization method used to minimize \code{\link{cadence.cost}}; must be chosen from \code{c("optim", "psoptim", "Rprop")}.
}
  \item{n.trials}{
      number of repeated trials used to avoid shallow local minima during optimization.
}
  \item{trace}{
      the level of printing which is done during optimization. A value of \code{0} suppresses
      any progress reporting.
}
  \item{maxit.Nelder}{
      maximum number of iterations of the Nelder-Mead optimization function prior to main calling \code{method}.
}
  \item{trace.Nelder}{
      the level of printing which is done during Nelder-Mead optimization. A value of \code{0} suppresses
      any progress reporting.
}
  \item{swarm.size}{
      \code{swarm.size} if \code{\link[pso]{psoptim}} is used for optimization.
}
  \item{vectorize}{
      \code{vectorize} if \code{\link[pso]{psoptim}} is used for optimization.
}
  \item{delta.0}{
      size of the initial update-value if \code{\link{rprop}} is used for optimization.
}
  \item{delta.min}{
      minimum value for the adaptive update-value if \code{\link{rprop}} is used for optimization.
}
  \item{delta.max}{
      maximum value for the adaptive update-value if \code{\link{rprop}} is used for optimization.
}
  \item{epsilon}{
      step-size used in the finite difference calculation of the gradient if \code{\link{rprop}} is used for optimization.
}
  \item{range.mult}{
      if \code{\link[pso]{psoptim}} is used for optimization, sets the search space boundaries to \code{range.mult} times the range of weights found by the Nelder-Mead algorithm.
}
  \item{step.tol}{
      convergence criterion if \code{\link{rprop}} is used for optimization. Optimization will stop if the change in \code{f} over the previous three iterations falls below this value.
}
  \item{f.target}{
      target value of \code{f} if \code{\link{rprop}} is used for optimization. Optimization will stop if \code{f} falls below this value.
}
  \item{f.cost}{
      cost function to be optimized.
}
  \item{max.exceptions}{
      maximum number of repeated exceptions allowed during optimization.
}
}
\value{
     a list of with number of elements equal to the length of \code{n.hidden}; each list consists of:

      \item{W1}{input-hidden layer weights}
      \item{W2}{hidden-output layer weights. Attributes indicating the
       mean and standard deviation of columns of \code{x}; the value
       of \code{hidden.fcn}; the valud of \code{hidden.fcn}; the negative
       log-likelihood \code{NLL}; the number of model parameters \code{k};
       the value of the weight penalty \code{penalty} (if \code{sd.norm} is
       less than infinity); the value of the \code{BIC}, \code{AIC}, and
       \code{AICc} cost-complexity criteria; and the predictand
       \code{distribution} list are also returned.}
}
\seealso{
    \code{\link{cadence.predict}}, \code{\link{optim}}, \code{\link{rprop}},
    \code{\link{xval.buffer}}, \code{\link{logistic}}
}
\examples{
    data(FraserSediment)
    set.seed(1)
    lnorm.distribution <- list(density.fcn = dlnorm,
                               parameters = c("meanlog", "sdlog"),
                               parameters.fixed = NULL,
                               output.fcns = c(identity, exp))
    fit <- cadence.fit(x = FraserSediment$x.1970.1976[c(TRUE, rep(FALSE, 19)),],
                       y = FraserSediment$y.1970.1976[c(TRUE, rep(FALSE, 19)),,
                       drop=FALSE], n.hidden = 3, n.trials = 1,
                       maxit.Nelder = 100, trace.Nelder = 1, hidden.fcn = tanh,
                       distribution = lnorm.distribution, trace = 1)
    pred <- cadence.predict(x = FraserSediment$x.1977.1979, fit = fit)
    matplot(pred, type = "l")
}
\references{
Cannon, A.J., 2012. Neural networks for probabilistic environmental
prediction: Conditional Density Estimation Network Creation & Evaluation
(CaDENCE) in R. Computers & Geosciences 41: 126-135.
doi:10.1016/j.cageo.2011.08.023

Neuneier, R., F. Hergert, W. Finnoff, and D. Ormoneit, 1994., Estimation of
conditional densities: a comparison of neural network approaches. In:
M. Marinaro and P. Morasso (eds.), Proceedings of ICANN 94, Berlin, Springer,
p. 689-692.
}
