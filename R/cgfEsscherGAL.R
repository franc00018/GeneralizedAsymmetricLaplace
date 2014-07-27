# Cumulant generating function of the 
# Esscher transform of GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Cumulant generating function of the 
#' Esscher transform with parameter 1 of GAL distribution
#' @param u Transform variate
#' @param param Parameter vector
#' @param ess.param Esscher Transform Parameter
#' @param eval.time Time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return Cumulant generating function value at point u for given parameter vector
#' @export cgfEsscherGAL
#' @author Francois Pelletier
cgfEsscherGAL <- function(u,param,ess.param=1,eval.time=1,type="mu",log=FALSE)
{
	testparGAL(param,type,log)
	if(log)
	{
		if(type=="mu")
		{
			return(log(exp(exp(param[1])*(u+ess.param))*(1-(1/2)*exp(param[2])^2*(u+ess.param)^2-exp(param[3])*(u+ess.param))^(-exp(param[4]))))
		}
		if(type=="kappa")
		{
			return(log(exp(exp(param[1])*(u+ess.param))*((exp(param[2])^2*(u+ess.param)^2)/2+(exp(param[3])*exp(param[2])*(u+ess.param))/sqrt(2)-(exp(param[2])*(u+ess.param))/(sqrt(2)*exp(param[3]))+1)^(-exp(param[4]))))
		}
	}
	else
	{
		if(type=="mu")
		{
			return(log(exp(param[1]*(u+ess.param))*(1-(1/2)*param[2]^2*(u+ess.param)^2-param[3]*(u+ess.param))^(-param[4])))
		}
		if(type=="kappa")
		{
			return(log(exp(param[1]*(u+ess.param))*((param[2]^2*(u+ess.param)^2)/2+(param[3]*param[2]*(u+ess.param))/sqrt(2)-(param[2]*(u+ess.param))/(sqrt(2)*param[3])+1)^(-param[4])))
		}
	}
}


