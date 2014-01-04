# Change type of parametrization of GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Change type of parametrization of GAL distribution
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param target Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return The converted parameter vector
#' 
#' @author Francois Pelletier
changetypeGAL <- function(param,type="mu",target="kappa",log=FALSE)
{
	testparGAL(param,type,log)
	if(log)
	{
		if(type=="mu" && target=="kappa")
		{
			c(param[1],param[2],log((sqrt(4*exp(param[2])^2+exp(param[3])^2)-exp(param[3]))/(2*exp(param[2]))),param[4])
		}
		else if(type=="kappa" && target=="mu")
		{
			c(param[1],param[2],log(exp(param[2])*(1/exp(param[3])-exp(param[3]))/sqrt(2)),param[4])
		}				
	}
	else
	{
		if(type=="mu" && target=="kappa")
		{
			c(param[1],param[2],(sqrt(4*param[2]^2+param[3]^2)-param[3])/(2*param[2]),param[4])
		}
		else if(type=="kappa" && target=="mu")
		{
			c(param[1],param[2],param[2]*(1/param[3]-param[3])/sqrt(2),param[4])
		}		
	}
}

