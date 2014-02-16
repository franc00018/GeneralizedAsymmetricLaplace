# Differenciation of the cumulant generating fonction of the 
# GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Differenciation of the cumulant generating fonction of the 
#' GAL distribution
#' @param u Transform variate point of evaluation
#' @param order Order of differenciation
#' @param param Parameters of the GAL distirbution
#' @param eval.time Time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return The value of the derivative at the transform variate point 
#' of evaluation
#' 
#' @author Francois Pelletier
diffcgfGAL <- function(u,order,param,eval.time=1,type="mu",log=FALSE)
{
	if(type=="mu")
	{
		if(order==1)
		{
			
		}
		if(order==2)
		{
			2*eval.time*param[4]*(2*param[2]^2*u*param[3]+
						2*param[3]^2+2*param[2]^2+param[2]^4*u^2)/
					(-2+param[2]^2*u^2+2*param[3]*u)^2
		}
		if(order==3)
		{
			-4*eval.time*param[4]*(3*param[2]^4*u^2*param[3]+
						6*param[2]^2*u*param[3]^2+param[2]^6*u^3+
						6*param[2]^4*u+6*param[2]^2*param[3]+4*param[3]^3)/
					(-2+param[2]^2*u^2+2*param[3]*u)^3
		}
		if(order==4)
		{
			(12*param[2]^8*param[4]*u^4+48*param[3]*param[2]^6*param[4]*u^3+
						(144*param[2]^6+144*param[3]^2*param[2]^4)*param[4]*u^2+
						(288*param[3]*param[2]^4+192*param[3]^3*param[2]^2)*param[4]*u+
						(48*param[2]^4+192*param[3]^2*param[2]^2+96*param[3]^4)*param[4])/
					(param[2]^8*u^8+8*param[3]*param[2]^6*u^7+
						(24*param[3]^2*param[2]^4-8*param[2]^6)*u^6+
						(32*param[3]^3*param[2]^2-48*param[3]*param[2]^4)*u^5+
						(24*param[2]^4-96*param[3]^2*param[2]^2+16*param[3]^4)*u^4+
						(96*param[3]*param[2]^2-64*param[3]^3)*u^3+
						(96*param[3]^2-32*param[2]^2)*u^2+(-64)*param[3]*u+16)
		}
	}
	if(type=="kappa")
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
