# Centered moments of the GAL distribution
# Skewness and (adjusted) kurtosis of the GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Centered moments of the GAL distribution
#' @param param Parameter vector
#' @param order Order of raw moment
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return A numeric value of the centered moment
#' @export cmGAL
#' @author Francois Pelletier
cmGAL <- function(param,order,type="mu",log=FALSE)
{
	testparGAL(param,type,log)
	if(log)
	{
		return(cmGAL(exp(param),order,type,log=FALSE))
	}
	else
	{
		if(type=="mu")
		{
			if(order==2)
			{
				return(param[4]*param[2]^2+param[4]*param[3]^2)
			}
			if(order==3)
			{
				return(3*param[3]*param[4]*param[2]^2+2*param[3]^3*param[4])
			}
			
			if(order==4)
			{
				return((3*param[4]^2+3*param[4])*param[2]^4+(6*param[3]^2*param[4]^2+12*param[3]^2*param[4])*param[2]^2+3*param[3]^4*param[4]^2+6*param[3]^4*param[4])
			}
			else
				stop("order must be 2,3 or 4")
		}
		if(type=="kappa")
		{
			if(order==2)
			{
				return((1/2)*param[4]*param[2]^2*(param[3]^4+1)/param[3]^2)
			}
			if(order==3)
			{
				return((1/2)*param[4]*param[2]^3*sqrt(2)*(1-param[3]^6)/param[3]^3)
			}
			if(order==4)
			{
				return((3/4)*param[4]*((param[4]+2)*param[3]^8+2*param[4]*param[3]^4+param[4]+2)*param[2]^4/param[3]^4)
			}
			else
				stop("order must be 2,3 or 4")
		}
	}
}

#' Skewness of the GAL distribution
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return A numeric value of the skewness
#' 
#' @author Francois Pelletier
skewnessGAL <- function(param,type="mu",log=FALSE)
{
	testparGAL(param,type,log)
	if(log)
	{
		return(skewnessGAL(exp(param),type,log=FALSE))
	}
	else
	{
		return(cmGAL(3,param,type) / (cmGAL(2,param,type)^(3/2)))
	}
}

#' (Adjusted) kurtosis of the GAL distribution
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @param adjust Logical to use the adjusted kurtosis
#' @return A numeric value of the kurtosis
#' 
#' @author Francois Pelletier
kurtosisGAL <- function(param,type="mu",log=FALSE,adjust=TRUE)
{
	testparGAL(param,type,log)
	if(log)
	{
		return(kurtosisGAL(exp(param),type,log=FALSE))
	}
	else
	{
		return(cmGAL(4,param,type) / (cmGAL(2,param,type)^2) - 3*adjust)
	}
}