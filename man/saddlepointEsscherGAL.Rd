\name{saddlepointEsscherGAL}
\alias{saddlepointEsscherGAL}
\title{Evaluation of the saddlepoint of the Esscher transform with
parameter 1 of the GAL distribution for given quantiles}
\usage{
saddlepointEsscherGAL(x, param, eval.time = 1, type = "mu", log = FALSE)
}
\arguments{
  \item{x}{vector of quantiles}

  \item{param}{Parameters of the underlying GAL
  distribution}

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
Evaluation of the saddlepoint of the Esscher transform with
parameter 1 of the GAL distribution for given quantiles
}
\author{
Francois Pelletier
}

