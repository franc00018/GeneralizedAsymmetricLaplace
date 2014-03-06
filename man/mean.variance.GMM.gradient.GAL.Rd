\name{mean.variance.GMM.gradient.GAL}
\alias{mean.variance.GMM.gradient.GAL}
\title{Gradient of the GMM moment conditions on mean and variance}
\usage{
\method{mean}{variance.GMM.gradient.GAL}(param, Data, type = "mu")
}
\arguments{
  \item{param}{GAL parameters}

  \item{Data}{Data sample}

  \item{type}{Choose between "mu" or "kappa"
  parametrization}
}
\value{
gradient matrix
}
\description{
Derivative of the moment conditions according to the vector
of parameters
}
\author{
François Pelletier
}

