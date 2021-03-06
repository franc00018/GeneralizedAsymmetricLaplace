\name{scaleGAL}
\alias{scaleGAL}
\title{Apply scale and location transform to the GAL distribution}
\usage{
scaleGAL(param, type = "kappa", location, scale, log = FALSE)
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
The transformed parameter vector
}
\description{
Apply scale and location transform to the GAL distribution
}
\author{
Francois Pelletier
}

