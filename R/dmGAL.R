# Derivative vector of the mean and standard deviation of GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Derivative vector of the mean and standard deviation of GAL distribution
#' @param param Parameter vector
#' @param order 1 for mean, 2 for standard deviation
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return A vector of the derivative of the analytical moment
#' 
#' @author Francois Pelletier
dmGAL <- function(param,order,type="mu",log=FALSE)
{		if(log)
	{
		eparam <- exp(param)
		if(order==1)
		{
			if(type=="mu")
			{
				c(1,0,eparam[4],eparam[3])
			}
			else if(type=="kappa")
			{
				c(1,
						-(1/2)*sqrt(2)*eparam[4]*(-1+eparam[3]^2)/eparam[3],
						-(1/2)*eparam[4]*eparam[2]*sqrt(2)*(eparam[3]^2+1)/eparam[3]^2,
						-(1/2)*sqrt(2)*eparam[2]*(-1+eparam[3]^2)/eparam[3])
			}				
		}
		if(order==2)
		{
			if(type=="mu")
			{
				c(0, 
						eparam[4]*eparam[2]/sqrt(eparam[4]*eparam[2]^2+eparam[4]*eparam[3]^2), 
						eparam[4]*eparam[3]/sqrt(eparam[4]*eparam[2]^2+eparam[4]*eparam[3]^2), 
						(1/2)*(eparam[2]^2+eparam[3]^2)/sqrt(eparam[4]*eparam[2]^2+eparam[4]*eparam[3]^2))
			}
			else if(type=="kappa")
			{
				c(0, 
						(1/2)*sqrt(2)*eparam[4]*eparam[2]*(eparam[3]^4+1)/(sqrt(eparam[4]*eparam[2]^2*(eparam[3]^4+1)/eparam[3]^2)*eparam[3]^2),
						(1/2)*sqrt(2)*eparam[4]*eparam[2]^2*(eparam[3]^4-1)/(sqrt(eparam[4]*eparam[2]^2*(eparam[3]^4+1)/eparam[3]^2)*eparam[3]^3), 
						(1/4)*sqrt(2)*eparam[2]^2*(eparam[3]^4+1)/(sqrt(eparam[4]*eparam[2]^2*(eparam[3]^4+1)/eparam[3]^2)*eparam[3]^2))
			}				
		}
	}
	else
	{
		if(order==1)
		{
			if(type=="mu")
			{
				c(1,0,param[4],param[3])
			}
			else if(type=="kappa")
			{
				c(1,
						-(1/2)*sqrt(2)*param[4]*(-1+param[3]^2)/param[3],
						-(1/2)*param[4]*param[2]*sqrt(2)*(param[3]^2+1)/param[3]^2,
						-(1/2)*sqrt(2)*param[2]*(-1+param[3]^2)/param[3])
			}				
		}
		if(order==2)
		{
			if(type=="mu")
			{
				c(0, 
						param[4]*param[2]/sqrt(param[4]*param[2]^2+param[4]*param[3]^2), 
						param[4]*param[3]/sqrt(param[4]*param[2]^2+param[4]*param[3]^2), 
						(1/2)*(param[2]^2+param[3]^2)/sqrt(param[4]*param[2]^2+param[4]*param[3]^2))
			}
			else if(type=="kappa")
			{
				c(0, 
						(1/2)*sqrt(2)*param[4]*param[2]*(param[3]^4+1)/(sqrt(param[4]*param[2]^2*(param[3]^4+1)/param[3]^2)*param[3]^2),
						(1/2)*sqrt(2)*param[4]*param[2]^2*(param[3]^4-1)/(sqrt(param[4]*param[2]^2*(param[3]^4+1)/param[3]^2)*param[3]^3), 
						(1/4)*sqrt(2)*param[2]^2*(param[3]^4+1)/(sqrt(param[4]*param[2]^2*(param[3]^4+1)/param[3]^2)*param[3]^2))
			}				
		}
	}
}
