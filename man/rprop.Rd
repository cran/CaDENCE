\name{rprop}
\alias{rprop}
\title{
Resilient backpropagation (Rprop) optimization algorithm
}
\description{
From Riedmiller (1994): Rprop stands for 'Resilient backpropagation' and is a
local adaptive learning scheme. The basic principle of Rprop is to eliminate the
harmful influence of the size of the partial derivative on the weight step. As a
consequence, only the sign of the derivative is considered to indicate the
direction of the weight update. The size of the weight change is exclusively
determined by a weight-specific, so called 'update-value'.

This function implements the iRprop+ algorithm from Igel and Huesken (2003).
}
\usage{
rprop(w, f, iterlim = 100, print.level = 1, delta.0 = 0.1,
      delta.min = 1e-06, delta.max = 50, epsilon = 1e-08,
      step.tol = 1e-06, f.target = -Inf, ...)
}
\arguments{
  \item{w}{
the starting parameters for the minimization.
}
  \item{f}{
the function to be minimized. If the function value has an attribute called \code{gradient},
this will be used in the calculation of updated parameter values. Otherwise, numerical
derivatives will be used.
}
  \item{iterlim}{
the maximum number of iterations before the optimization is stopped.
}
  \item{print.level}{
the level of printing which is done during optimization. A value of \code{0} suppresses
any progress reporting, whereas positive values report the value of \code{f} and the
mean change in \code{f} over the previous three iterations.
}
  \item{delta.0}{
size of the initial Rprop update-value.
}
  \item{delta.min}{
minimum value for the adaptive Rprop update-value.
}
  \item{delta.max}{
maximum value for the adaptive Rprop update-value.
}
  \item{epsilon}{
step-size used in the finite difference calculation of the gradient.
}
  \item{step.tol}{
convergence criterion. Optimization will stop if the change in \code{f} over the previous three iterations falls below this value.
}
  \item{f.target}{
target value of \code{f}. Optimization will stop if \code{f} falls below this value.
}
  \item{\dots}{
further arguments to be passed to \code{f}.
}
}
\value{
A list with elements:
  \item{par}{The best set of parameters found.}
  \item{value}{The value of \code{f} corresponding to \code{par}.}
  \item{gradient}{An estimate of the gradient at the solution found.}
}
\references{
Igel, C. and M. Huesken, 2003. Empirical evaluation of the improved Rprop
learning algorithms. Neurocomputing 50: 105-123.

Riedmiller, M., 1994. Advanced supervised learning in multilayer perceptrons -
from backpropagation to adaptive learning techniques. Computer Standards and
Interfaces 16(3): 265-278.
}
