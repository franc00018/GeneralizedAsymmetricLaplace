# Cumulant generating function of the 
# Esscher transform with parameter 1 of GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Cumulant generating function of the 
#' Esscher transform with parameter 1 of GAL distribution
#' @param u Transform variate
#' @param param Parameter vector
#' @param eval.time Time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return Cumulant generating function value at point u for given parameter vector
#' @export cgfEsscherGAL
#' @author Francois Pelletier
cgfEsscherGAL <- function(u,param,eval.time=1,type="mu",log=FALSE)
{
	if(type=="mu")
	{
		return(log((exp(param[1]*(u+1))/(1-(1/2)*param[2]^2*(u+1)^2-param[3]*(u+1))^param[4])^eval.time/
						(exp(param[1])/(1-(1/2)*param[2]^2-param[3])^param[4])^eval.time))
	}
	if(type=="kappa")
	{
		
	}
}


