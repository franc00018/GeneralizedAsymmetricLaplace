# Saddlepoint approximation of the density function of the 
# GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Saddlepoint approximation of the density function of the 
#' GAL distribution
#' @param x vector of quantiles
#' @param param Parameter vector
#' @param eval.time Time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters

#' @return Saddlepoint approximation of the density function
dsaddleapproxGAL <- function(x,param,eval.time=1,type="mu",log=FALSE)
{
	s <- saddlepointGAL(x,param,eval.time,type,log)
	1/sqrt(2*pi*diffcgfGAL(s,2,param,eval.time,type,log)) * exp(cgfGAL(s,param,type,log)-s*x)
}
