\name{dscaleGAL}
\alias{dscaleGAL}
\title{Derivative of scale and location transform to the GAL distribution}
\usage{
dscaleGAL(param, type = "kappa", location, scale, log = FALSE)
}
\arguments{
  \item{param}{Parameter vector}

  \item{type}{Choose between "mu" or "kappa"
  parametrization}

  \item{location}{Location shift (unitary)}

  \item{scale}{Scale shift (in standard deviations)}

  \item{log}{Logical for log-parameters}
}
\value{
The matrix derivative of the transformed parameter vector
}
\description{
Derivative of scale and location transform to the GAL
distribution
}
\author{
Francois Pelletier
}

