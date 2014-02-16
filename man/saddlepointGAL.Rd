\name{saddlepointGAL}
\alias{saddlepointGAL}
\title{Evaluation of the saddlepoint of the GAL distribution for given quantiles}
\usage{
saddlepointGAL(x, param, eval.time = 1, type = "mu", log = FALSE)
}
\arguments{
  \item{x}{vector of quantiles}

  \item{param}{Parameters of the GAL distribution}

  \item{eval.time}{Time of the process}

  \item{type}{Choose between "mu" or "kappa"
  parametrization}

  \item{log}{Logical for log-parameters}
}
\value{
The value of the saddlepoint for each point of the vector
of quantiles
}
\description{
Evaluation of the saddlepoint of the GAL distribution for
given quantiles
}
\author{
Francois Pelletier
}

