\name{gam.style}
\alias{gam.style}
\title{
GAM-style effects plots for interpreting CDEN models
}
\description{
GAM-style effects plots provide a graphical means of interpreting
relationships between predictors and conditional pdf parameter values
predicted by a CDEN. From Plate et al. (2000): The effect of the
\code{i}th input variable at a particular input point \code{Delta.i.x}
is the change in \code{f} resulting from changing \code{X1} to \code{x1}
from \code{b1} (the baseline value [...]) while keeping the other
inputs constant. The effects are plotted as short line segments, centered
at (\code{x.i}, \code{Delta.i.x}), where the slope of the segment
is given by the partial derivative. Variables that strongly influence
the function value have a large total vertical range of effects.
Functions without interactions appear as possibly broken straight lines
(linear functions) or curves (nonlinear functions). Interactions show up as
vertical spread at a particular horizontal location, that is, a vertical
scattering of segments. Interactions are present when the effect of
a variable depends on the values of other variables.
}

\usage{
gam.style(x, fit, column, baseline = mean(x[,column]),
         additive.scale = FALSE, epsilon = 1e-5,
         seg.len = 0.02, seg.cols = "black", plot = TRUE,
         return.results = FALSE, ...)
}
\arguments{
  \item{x}{
      matrix with number of rows equal to the number of samples and number of columns equal to the number of predictor variables.
}
  \item{fit}{
      element from list returned by \code{\link{cadence.fit}}.
}
  \item{column}{
      column of \code{x} for which effects plots should be returned.
}
  \item{baseline}{
      value of \code{x[,column]} to be used as the baseline for calculation of predictor effects; defaults to \code{mean(x[,column])}.
}
  \item{additive.scale}{
      if \code{TRUE} then predictor effects and partial derivatives are calculated before the inverse link functions for the distribution parameters are applied; if \code{FALSE} (the default) then values are calculated after the inverse link functions are applied.
}
  \item{epsilon}{
      step-size used in the finite difference calculation of the partial derivatives.
}
  \item{seg.len}{
      length of effects line segments expressed as a fraction of the range of \code{x[,column]}.
}
  \item{seg.cols}{
      colors of effects line segments.
}
  \item{plot}{
      if \code{TRUE} (the default) then an effects plots for each distribution parameter is produced.
}
  \item{return.results}{
      if \code{TRUE} then values of effects and partial derivatives for each distribution parameter are returned.
}
  \item{\dots}{
      further arguments to be passed to \code{plot}.
}
}
\seealso{
    \code{\link{cadence.fit}}, \code{\link{cadence.predict}}
}
\value{
A list with elements:
  \item{effects}{a matrix of predictor effects.}
  \item{partials}{a matrix of predictor partial derivatives.}
}
\examples{
    data(FraserSediment)
    set.seed(1)
    lnorm.distribution <- list(density.fcn = dlnorm,
                               parameters = c("meanlog", "sdlog"),
                               parameters.fixed = NULL,
                               output.fcns = c(identity, exp))
    x <- FraserSediment$x.1970.1976[c(TRUE, rep(FALSE, 24)),]
    y <- FraserSediment$y.1970.1976[c(TRUE, rep(FALSE, 24)),,drop=FALSE]
    fit.nlin <- cadence.fit(x, y, n.hidden = 2, n.trials = 1,
                            hidden.fcn = tanh, distribution =
                            lnorm.distribution, maxit.Nelder = 100,
                            trace.Nelder = 1, trace = 1)
    fit.lin <- cadence.fit(x, y, hidden.fcn = identity, n.trials = 1,
                           distribution = lnorm.distribution,
                           maxit.Nelder = 100, trace.Nelder = 1,
                           trace = 1)
    gam.style(x, fit = fit.nlin[[1]], column = 1,
              main = "Nonlinear")
    gam.style(x, fit = fit.lin[[1]], column = 1,
              additive.scale = TRUE,
              main = "Linear (additive.scale = TRUE)")
}
\references{
Cannon, A.J. and I.G. McKendry, 2002. A graphical sensitivity analysis
for interpreting statistical climate models: Application to Indian
monsoon rainfall prediction by artificial neural networks and
multiple linear regression models. International Journal of
Climatology, 22:1687-1708.

Plate, T., J. Bert, J. Grace, and P. Band, 2000. Visualizing the function
computed by a feedforward neural network. Neural Computation,
12(6): 1337-1354.
}
