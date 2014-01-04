# Check for the validity of a parameter vector. Stop at error.
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Check for the validity of a parameter vector. Stop at error.
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return logical
#' 
#' @author Francois Pelletier
testparGAL <- function(param,type="mu",log=FALSE)
{
	if(log)
	{
		if(type=="mu")
		{
			if(exp(param[2])<=0)
				stop("param 2 must be positive")
			if(exp(param[4])<=0)
				stop("param 4 must be positive")
		}
		if(type=="kappa")
		{
			if(exp(param[2])<=0)
				stop("param 2 must be positive")
			if(exp(param[4])<=0)
				stop("param 4 must be positive")
		}
	}
	else
	{
		if(type=="mu")
		{
			if(param[2]<=0)
				stop("param 2 must be positive")
			if(param[4]<=0)
				stop("param 4 must be positive")
		}
		if(type=="kappa")
		{
			if(param[2]<=0)
				stop("param 2 must be positive")
			if(param[4]<=0)
				stop("param 4 must be positive")	
		}		
	}
	TRUE
}
