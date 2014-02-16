# Evaluation of the saddlepoint of the GAL distribution for given quantiles
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Evaluation of the saddlepoint of the GAL distribution for given quantiles
#' @param x vector of quantiles
#' @param param Parameters of the GAL distribution
#' @param eval.time Time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return The value of the saddlepoint for each point of the vector of quantiles
#' 
#' @author Francois Pelletier
saddlepointGAL <- function(x,param,eval.time=1,type="mu",log=FALSE)
{
	if(type=="mu")
	{
		(-eval.time*param[1]*param[3]+eval.time*param[4]*param[2]^2+x*param[3]-
					(eval.time^2*param[1]^2*param[3]^2-2*eval.time*param[1]*param[3]^2*x+
						eval.time^2*param[4]^2*param[2]^4+x^2*param[3]^2+2*eval.time^2*param[1]^2*
						param[2]^2-4*eval.time*param[1]*param[2]^2*x+2*x^2*param[2]^2)^(1/2))/
				param[2]^2/(eval.time*param[1]-x)
	}
	if(type=="kappa")
	{
		
	}
}
