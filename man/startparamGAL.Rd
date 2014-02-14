\name{startparamGAL}
\alias{startparamGAL}
\title{Method of moments with a twist for GAL distribution}
\usage{
startparamGAL(data, type = "mu", log = FALSE)
}
\arguments{
  \item{data}{Sample}

  \item{type}{Choose between "mu" or "kappa"
  parametrization}

  \item{log}{Logical for log-parameters}
}
\value{
a vector of estimated parameters
}
\description{
Estimating the parameters of GAL distribution using a twist
on method of moments by Seneta (2004)
}
\author{
Francois Pelletier
}

