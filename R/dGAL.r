#' Density function for the GAL distribution
#' @param x vector of quantiles
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return density at quantile x
#' @export dGAL
#' @author Francois Pelletier
dGAL <- function(x,param,type="mu",log=FALSE)
{
	testparGAL(param,type,log)
	if(log)
	{
		if(type=="mu")
		{
			return(dGAL(x,changetypeGAL(param=param,log=log),type="kappa",log=log))
		}
		else if(type=="kappa")
		{
			num1 <- sqrt(2)*exp(sqrt(2)/(2*exp(param[2]))*(1/exp(param[3])-exp(param[3]))*(x-exp(param[1])))
			denom1 <- sqrt(pi)*exp(param[2])^(exp(param[4])+1/2)*gamma(exp(param[4]))
			num2 <- sqrt(2)*abs(x-exp(param[1]))
			denom2 <- exp(param[3])+1/exp(param[3])
			expo1 <- exp(param[4])-1/2
			besselarg <- sqrt(2)/(2*exp(param[2]))*(1/exp(param[3])+exp(param[3]))*abs(x-exp(param[1]))
			return(num1/denom1 * (num2/denom2)^expo1 * besselK(besselarg,expo1))
		}				
	}
	else
	{
		if(type=="mu")
		{
			return(dGAL(x,changetypeGAL(param=param,log=log),type="kappa",log=log))
		}
		else if(type=="kappa")
		{
			num1 <- sqrt(2)*exp(sqrt(2)/(2*param[2])*(1/param[3]-param[3])*(x-param[1]))
			denom1 <- sqrt(pi)*param[2]^(param[4]+1/2)*gamma(param[4])
			num2 <- sqrt(2)*abs(x-param[1])
			denom2 <- param[3]+1/param[3]
			expo1 <- param[4]-1/2
			besselarg <- sqrt(2)/(2*param[2])*(1/param[3]+param[3])*abs(x-param[1])
			return(num1/denom1 * (num2/denom2)^expo1 * besselK(besselarg,expo1))
		}		
	}
}