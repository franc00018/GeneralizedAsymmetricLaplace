# Differenciation of the cumulant generating fonction of the 
# Esscher transform with parameter 1 of the GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Differenciation of the cumulant generating fonction of the 
#' Esscher transform with parameter 1 of the GAL distribution
#' @param u Transform variate point of evaluation
#' @param order Order of differenciation
#' @param param Parameters of the GAL distirbution
#' @param eval.time Time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return The value of the derivative at the transform variate point of evaluation
#' @export diffcgfEsscherGAL
#' @author Francois Pelletier
diffcgfEsscherGAL <- function(u,order,param,eval.time=1,type="mu",log=FALSE)
{
	if(type=="mu")
	{
		if(order==1)
		{
		}
		if(order==2)
		{
			return(2*eval.time*param[4]*(2*param[3]^2+param[2]^4+2*param[2]^2+param[2]^4*u^2+2*param[2]^2*u*param[3]+2*param[3]*param[2]^2+2*param[2]^4*u)/(-2+param[2]^2*u^2+2*param[2]^2*u+param[2]^2+2*param[3]*u+2*param[3])^2)
		}
		if(order==3)
		{
			return(-4*eval.time*param[4]*(6*param[2]^4+param[2]^6+3*param[2]^4*u^2*param[3]+6*param[2]^4*u*param[3]+6*param[2]^2*u*param[3]^2+4*param[3]^3+6*param[3]*param[2]^2+6*param[2]^4*u+param[2]^6*u^3+3*param[2]^6*u^2+3*param[2]^6*u+3*param[2]^4*param[3]+6*param[2]^2*param[3]^2)/(-2+param[2]^2*u^2+2*param[2]^2*u+param[2]^2+2*param[3]*u+2*param[3])^3)
		}
		if(order==4)
		{
			
		}
	}
	else if(type=="kappa")
	{
		if(order==1)
		{
			
		}
		if(order==2)
		{
			
		}
		if(order==3)
		{
			
		}
		if(order==4)
		{
			
		}
	}
}