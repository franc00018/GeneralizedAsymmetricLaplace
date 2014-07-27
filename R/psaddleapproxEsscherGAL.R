# Saddlepoint approximation of the distribution function of the Esscher 
# transform of the GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Saddlepoint approximation of the distribution function of the Esscher 
#' transform of the GAL distribution
#' @param x vector of quantiles
#' @param param Parameter vector
#' @param eval.time Time of the process
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return Saddlepoint approximation of the distribution function
#' @export psaddleapproxEsscherGAL
#' @author Francois Pelletier
psaddleapproxEsscherGAL <- function(x,param,eval.time=1,type="mu",log=FALSE)
{
	s <- saddlepointEsscherGAL(x,param,eval.time,type,log)
	u <- s * sqrt(diffcgfEsscherGAL(s,2,param,eval.time,type,log))
	w <- sign(s)*sqrt(2*(s*x-cgfEsscherGAL(s,param,1,eval.time,type,log)))
	
	(x==round(mGAL(param,1,type,log),4))*
			(1/2 + diffcgfEsscherGAL(0,3,param,eval.time,type,log)/
				(6*sqrt(2*pi)*diffcgfEsscherGAL(0,2,param,eval.time,type,log)^(3/2)))
	+(x!=round(mGAL(param,1,type,log),4)) * (pnorm(w)+dnorm(w)*(1/w-1/u))
	
}