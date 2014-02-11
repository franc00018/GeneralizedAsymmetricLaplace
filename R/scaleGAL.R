# Apply scale and location transform to the GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Apply scale and location transform to the GAL distribution
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param location Location shift (unitary)
#' @param scale Scale shift (in standard deviations)
#' @return The transformed parameter vector
#' 
#' @author Francois Pelletier
scaleGAL <- function(param,type="kappa",location,scale)
{
	if(type=="kappa")
	{
		param * c(scale,scale,1,1) + c(location,0,0,0)
	}
	else
	{
		changetypeGAL(
				scaleGAL(changetypeGAL(param,type="mu",target="kappa"),type="kappa",location,scale),
				type="kappa",target="mu")
	}
}
