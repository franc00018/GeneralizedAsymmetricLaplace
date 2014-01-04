# Random number generator for the GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Random number generator for the GAL distribution
#' @param n number of observations
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters 
#' @return A vector of random numbers
#' 
#' @author Francois Pelletier
rGAL <- function(n,param,type="mu",log=FALSE)
{
	testparGAL(param,type,log)
	if(log)
	{
		if(type=="mu")
		{
			rGAL(n,changetypeGAL(param,type="mu",target="kappa"),type="kappa",log=log)
		}
		if(type=="kappa")
		{
			# simulation de deux variables gamma
			rgamma1 <- rgamma(n, shape = exp(param[4]), scale = 1/exp(param[3]))
			rgamma2 <- rgamma(n, shape = exp(param[4]), scale = exp(param[3]))
			
			# simulation de la variable GAL
			exp(param[1]) + exp(param[2])/sqrt(2)*(rgamma1 - rgamma2)			
		}
	}
	else
	{
		if(type=="mu")
		{
			rGAL(n,changetypeGAL(param,type="mu",target="kappa"),type="kappa",log=log)
		}
		if(type=="kappa")
		{
			# simulation de deux variables gamma
			rgamma1 <- rgamma(n, shape = param[4], scale = 1/param[3])
			rgamma2 <- rgamma(n, shape = param[4], scale = param[3])
			
			# simulation de la variable GAL
			param[1] + param[2]/sqrt(2)*(rgamma1 - rgamma2)
		}
	}
}
