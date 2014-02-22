# Derivative vector of the type change function
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Derivative vector of the type change function
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param target Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return The derivative matrix of the type change function
#' 
#' @author Francois Pelletier
dchangetypeGAL <- function(param,type="mu",target="kappa",log=FALSE)
{
	testparGAL(param,type,log)
	if(log)
	{
		if(type=="mu" && target=="kappa")
		{
			
		}
		else if(type=="kappa" && target=="mu")
		{
			
		}				
	}
	else
	{
		if(type=="mu" && target=="kappa")
		{
			matrix(c(1,0,0,0,
							0,1,0,0,
							0,(param[3]*sqrt(4*param[2]^2+param[3]^2)-param[3]^2)/(2*param[2]^2*sqrt(4*param[2]^2+param[3]^2)),-(sqrt(4*param[2]^2+param[3]^2)-param[3])/(2*param[2]*sqrt(4*param[2]^2+param[3]^2)),0,
							0,0,0,1),4,4)
		}
		else if(type=="kappa" && target=="mu")
		{
			matrix(c(1,0,0,0,
							0,1,0,0,
							0,-(param[3]^2-1)/(sqrt(2)*param[3]),-((param[3]^2+1)*param[2])/(sqrt(2)*param[3]^2),0,
							0,0,0,1),4,4)
		}		
	}
}