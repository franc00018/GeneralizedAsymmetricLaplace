# Derivative of scale and location transform to the GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Derivative of scale and location transform to the GAL distribution
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param location Location shift (unitary)
#' @param scale Scale shift (in standard deviations)
#' @param log Logical for log-parameters
#' @return The matrix derivative of the transformed parameter vector
#' @export dscaleGAL
#' @author Francois Pelletier
dscaleGAL <- function(param,type="kappa",location,scale,log=FALSE)
{
	if(log)
	{
		
	}
	else
	{
		if(type=="kappa")
		{
			return(diag(c(scale,scale,1,1)))
		}
		else if (type=="mu")
		{

		}
	}
}
