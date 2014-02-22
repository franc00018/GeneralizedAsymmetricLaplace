\name{cfLM}
\alias{cfLM}
\title{Characteristic function of Laplace motion}
\usage{
cfLM(u, time, param, type = "mu", log = FALSE, start = 0)
}
\arguments{
  \item{u}{Transform variate}

  \item{time}{Time of the process}

  \item{param}{Parameter vector}

  \item{type}{Choose between "mu" or "kappa"
  parametrization}

  \item{log}{Logical for log-parameters}

  \item{start}{Starting value of the process}
}
\value{
Characteristic function value at point u for given
parameter vector
}
\description{
Characteristic function of Laplace motion
}
\author{
Francois Pelletier
}

