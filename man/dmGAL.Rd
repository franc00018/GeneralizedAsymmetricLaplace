\name{dmGAL}
\alias{dmGAL}
\title{Derivative vector of the mean and standard deviation of GAL distribution}
\usage{
dmGAL(param, order, type = "mu", log = FALSE)
}
\arguments{
  \item{param}{Parameter vector}

  \item{order}{1 for mean, 2 for standard deviation}

  \item{type}{Choose between "mu" or "kappa"
  parametrization}

  \item{log}{Logical for log-parameters}
}
\value{
A vector of the derivative of the analytical moment
}
\description{
Derivative vector of the mean and standard deviation of GAL
distribution
}
\author{
Francois Pelletier
}

