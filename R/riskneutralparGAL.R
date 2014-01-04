# Risk neutral conversion of parameters of GAL distirbution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Risk neutral conversion of parameters of GAL distirbution
#' @param param Parameter vector
#' @param riskfree Risk-free rate
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return Risk neutral parameter vector
#' 
#' @author Francois Pelletier
riskneutralparGAL <- function(param,riskfree,type="mu",log=FALSE)
{
	testparGAL(param,type,log)
	if(type=="kappa")
	{
		riskneutralparGAL(changetypeGAL(param,type="kappa",target="mu"),riskfree,type="mu",log)
	}
	if(type=="mu")
	{
		if(log)
		{
			c(log(riskfree+log(1-exp(param[3])-exp(param[2])^2/2)*param[4]),param[2],param[3],param[4])
		}
		else
		{
			c(riskfree+log(1-param[3]-param[2]^2/2)*param[4],param[2],param[3],param[4])
		}
	}
}


