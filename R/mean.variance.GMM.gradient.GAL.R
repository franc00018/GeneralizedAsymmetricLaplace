# Gradient of the GMM moment conditions on mean and variance
# 
# Author: Francois Pelletier
#
# LGPL-3.0
###############################################################################


#' Gradient of the GMM moment conditions on mean and variance
#'
#' Derivative of the moment conditions according to the vector of parameters
#' @param param GAL parameters
#' @param Data Data sample
#' @param type Choose between "mu" or "kappa" parametrization
#' @return gradient matrix
#' @export mean.variance.GMM.gradient.GAL
#' @author François Pelletier
mean.variance.GMM.gradient.GAL <- function(param,Data,type="mu")
{
	if(type=="mu")
	{
		return(matrix(c(-1,0,-param[4],-param[3],
						-2*(mean(Data)-param[1]-param[3]*param[4]),-2*param[2]*param[4],2*param[4]*(mean(Data)-param[1]-param[3]*param[4])+2*param[3]*param[4],-2*param[3]*(mean(Data)-param[1]-param[3]*param[4])-param[2]^2-param[3]^2),
				nrow=4,ncol=2))
	}
}

