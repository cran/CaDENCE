\name{CaDENCE-package}
\alias{CaDENCE-package}
\alias{CaDENCE}
\docType{package}
\title{Conditional Density Estimation Network Construction and Evaluation (CaDENCE)}

\description{
A conditional density estimation network (CDEN) is a probabilistic extension
of the standard multi-layer perceptron neural network (MLP) (Neuneier et
al., 1994). A CDEN model allows users to estimate parameters of a specified
probability distribution conditioned upon values of a set of predictors
using the MLP architecture. The result is a flexible nonlinear model that can
be used to calculate the conditional mean, variance, prediction intervals, etc.
based on the specified distribution. Because the CDEN is based on the MLP,
nonlinear relationships, including those involving complicated interactions
between predictors, can be described by the modelling framework. The CaDENCE
(Conditional Density Estimation Network Creation & Evaluation) package
provides routines for creating and evaluating CDEN models in the R
programming language.
}

\details{
Procedures for fitting CaDENCE models are
provided by \code{\link{cadence.fit}}, which relies on the
standard \code{\link{optim}} function, the CaDENCE \code{\link{rprop}}
function, or, optionally, the \code{\link[pso]{psoptim}} function from the
\code{pso} package. Once a model has been developed,
\code{\link{cadence.predict}} is used to evaluate the
distribution parameters as a function of predictors.

The package also provides a variety of zero-inflated distributions, including
the Bernoulli-gamma (\code{\link{bgamma}}),
Bernoulli-Weibull (\code{\link{bweibull}}),
Bernoulli-Pareto 2 (\code{\link{bpareto2}}), and
Bernoulli-lognormal (\code{\link{blnorm}}), for use in the CaDENCE models.

\code{\link{gam.style}}, \code{\link{dummy.code}}, \code{\link{xval.buffer}},
and \code{\link{rbf}} are helper functions that may be useful for
data preprocessing, model evaluation, and interpretation of
fitted relationships.

Most other functions are used internally and should not normally need to be
called directly by the user.
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

\keyword{ package }
