\name{cfLM}
\alias{cfLM}
\title{Characteristic function of Laplace motion}
\usage{
cfLM(u, param, time1, time2, type = "mu", log = FALSE, start = 0)
}
\arguments{
  \item{u}{Transform variate}

  \item{param}{Parameter vector}

  \item{time1}{Start time of the process}

  \item{time2}{End time of the process}

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

