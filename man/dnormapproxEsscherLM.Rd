\name{dnormapproxEsscherLM}
\alias{dnormapproxEsscherLM}
\title{Normal approximation of the density function of the
Esscher transform of a Laplace Motion}
\usage{
dnormapproxEsscherLM(x, param, hEsscher = 0, eval.time = 1, type = "mu",
  log = FALSE, start = 0)
}
\arguments{
  \item{x}{vector of quantiles}

  \item{param}{Parameter vector}

  \item{hEsscher}{Esscher transform parameter}

  \item{eval.time}{Time of the process}

  \item{type}{Choose between "mu" or "kappa"
  parametrization}

  \item{log}{Logical for log-parameters}

  \item{start}{Starting value of the process}
}
\value{
Normal density function approximation of the Esscher
transform of the specified Laplace motion
}
\description{
Normal approximation of the density function of the Esscher
transform of a Laplace Motion
}

