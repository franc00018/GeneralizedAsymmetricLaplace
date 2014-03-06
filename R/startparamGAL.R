# Method of moments with a twist for GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Method of moments with a twist for GAL distribution
#' 
#' Estimating the parameters of GAL distribution using a 
#' twist on method of moments by Seneta (2004)
#' @param data Sample
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return a vector of estimated parameters
#' 
#' @author Francois Pelletier
startparamGAL <- function(data,type="mu",log=FALSE)
{
	if(type=="mu")
	{
		mom <- c(mean(data),var(data),moments::skewness(data),moments::kurtosis(data)-3)
		tau <- 3/(mom[4])
		sigma <- sqrt(mom[2]/(2*tau))
		mu <- mom[3]*sigma*sqrt(2/(3*(mom[4])))
		theta <- mom[1]-tau*mu
		if(log==FALSE)
			return(c(theta,sigma,mu,tau))
		else
			return(log(c(theta,sigma,mu,tau)))
	}
	if(type=="kappa")
	{
		return(changetypeGAL(startparamGAL(data,type="mu",log),type="kappa",target="mu",log))
	}
}
