# Characteristic function of Laplace motion
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Characteristic function of Laplace motion
#' @param u Transform variate
#' @param param Parameter vector
#' @param time1 Start time of the process
#' @param time2 End time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @param start Starting value of the process
#' @return Characteristic function value at point u for given parameter vector
#' @export cfLM
#' @author Francois Pelletier
cfLM <- function(u,param,time1,time2,type="mu",log=FALSE,start=0)
{
	time <- time2-time1
	testparGAL(param,type,log)
	if(log)
	{
		if(type=="mu")
		{
			return(exp(1i*(start+exp(param[1])*time)*u)*(1+(exp(param[2])^2*u^2)/2-1i*exp(param[3])*u)^(-exp(param[4])*time))
		}
		if(type=="kappa")
		{
			return(exp(1i*(start+exp(param[1])*time)*u)*(1+(exp(param[2])^2*u^2)/2-(1/2*1i)*
						exp(param[2])*sqrt(2)*(1/exp(param[3])-exp(param[3]))*u)^(-exp(param[4])*time))
		}
	}
	else
	{
		if(type=="mu")
		{
			return(exp(1i*(start+param[1]*time)*u)*(1+(param[2]^2*u^2)/2-1i*param[3]*u)^(-param[4]*time))
		}
		if(type=="kappa")
		{
			return(exp(1i*(start+param[1]*time)*u)*(1+(param[2]^2*u^2)/2-(1/2*1i)*
						param[2]*sqrt(2)*(1/param[3]-param[3])*u)^(-param[4]*time))
		}
	}
}

