# Moment generating function of GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Moment generating function of GAL distribution
#' @param u Transform variate
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return Moment generating function value at point u for given parameter vector
#' 
#' @author Francois Pelletier
mgfGAL <- function(u,param,type="mu",log=FALSE)
{
	if(log)
	{
		if(type=="mu")
		{
			exp(exp(param[1])*u)*(1-(1/2)*exp(param[2])^2*u^2-exp(param[3])*u)^(-exp(param[4]))
		}
		if(type=="kappa")
		{
			exp(exp(param[1])*u)*((exp(param[2])^2*u^2)/2+(exp(param[3])*exp(param[2])*u)/sqrt(2)-(exp(param[2])*u)/(sqrt(2)*exp(param[3]))+1)^(-exp(param[4]))
		}
	}
	else
	{
		if(type=="mu")
		{
			exp(param[1]*u)*(1-(1/2)*param[2]^2*u^2-param[3]*u)^(-param[4])
		}
		if(type=="kappa")
		{
			exp(param[1]*u)*((param[2]^2*u^2)/2+(param[3]*param[2]*u)/sqrt(2)-(param[2]*u)/(sqrt(2)*param[3])+1)^(-param[4])
		}
	}
}