# Characteristic function of GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Characteristic function of GAL distribution
#' @param u Transform variate
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Boolean for log-parameters
#' @return Characteristic function value at point u for given parameter vector
#' 
#' @author Francois Pelletier

characteristicfn <- function(u,param,type="mu",log=FALSE)
{
	if(log)
	{
		if(type=="mu")
		{
			exp(1i*exp(param[1])*u)*(1+(exp(param[2])^2*u^2)/2-1i*exp(param[3])*u)^(-exp(param[4]))
		}
		if(type=="kappa")
		{
			exp(1i*exp(param[1])*u)*(1+(exp(param[2])^2*u^2)/2-(1/2*1i)*
						exp(param[2])*sqrt(2)*(1/exp(param[3])-exp(param[3]))*u)^(-exp(param[4]))
		}
	}
	else
	{
		if(type=="mu")
		{
			exp(1i*param[1]*u)*(1+(param[2]^2*u^2)/2-1i*param[3]*u)^(-param[4])
		}
		if(type=="kappa")
		{
			exp(1i*param[1]*u)*(1+(param[2]^2*u^2)/2-(1/2*1i)*
						param[2]*sqrt(2)*(1/param[3]-param[3])*u)^(-param[4])
		}
	}
}

