# Normal approximation of the density function of the Esscher 
# transform of a Laplace Motion
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Normal approximation of the density function of the 
#' Esscher transform of a Laplace Motion
#' @param x vector of quantiles
#' @param param Parameter vector
#' @param hEsscher Esscher transform parameter
#' @param eval.time Time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @param start Starting value of the process

#' @return Normal density function approximation of the Esscher transform
#' of the specified Laplace motion
dnormapproxEsscherLM <- function(x,param,hEsscher=0,eval.time=1,type="mu",log=FALSE,start=0)
{
	dnorm(x,start+eval.time*(mGAL(1,param,type,log)+hEsscher*cmGAL(2,param,type,log)),
			sqrt(eval.time*cmGAL(2,param,type,log)))
}



