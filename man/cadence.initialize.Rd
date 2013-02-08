\name{cadence.initialize}
\alias{cadence.initialize}
\alias{cadence.reshape}
\title{
Initialize a CDEN weight vector
}
\description{
Random initialization of the weight vector used during fitting of the CDEN model.
}
\usage{
cadence.initialize(x, n.hidden, init.range, distribution)
}
\arguments{
  \item{x}{
      matrix with number of rows equal to the number of samples and number of columns equal to the number of predictors.
}
  \item{n.hidden}{
      number of hidden nodes in the CDEN model.
}
  \item{init.range}{
      range for random weights on [\code{min(init.range)}, \code{max(init.range)}]
}
  \item{distribution}{
      list used to specify the predictand distribution
}
}
