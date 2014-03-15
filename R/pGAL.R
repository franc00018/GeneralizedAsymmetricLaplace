# Distribution function for the GAL distribution
# 
# Author: François Pelletier
#
# LGPL 3.0
###############################################################################

#' Distribution function for the GAL distribution
#' @param x vector of quantiles
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return distribution at quantile x
#' @export pGAL
#' @author Francois Pelletier
pGAL <- function(x,param,type="mu",log=FALSE)
{
	x.l <- as.list(x)
	integrate1 <- function(x,param,type,log) integrate(dGAL,-Inf,x,param=param,type=type,log=log)$value
	unlist(lapply(x.l,integrate1,param,type,log))
}
