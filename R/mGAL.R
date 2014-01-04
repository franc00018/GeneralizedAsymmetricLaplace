# Raw moments of the GAL distribution
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Raw moments of the GAL distribution
#' @param order Order of raw moment
#' @param param Parameter vector
#' @param type Choose between "mu" or "kappa" parametrization
#' @param log Logical for log-parameters
#' @return A numeric value of the raw moment
#' 
#' @author Francois Pelletier
mGAL <- function(order,param,type="mu",log=FALSE)
{
	testparGAL(param,type,log)
	if(log)
	{
		mGAL(order,exp(param),type,log=FALSE)
	}
	else
	{
		if(type=="mu")
		{
			if(order==1)
			{
				param[1]+param[4]*param[3]
			}
			if(order==2)
			{
			  	param[1]^2+2*param[1]*param[4]*param[3]+param[4]^2*param[3]^2+
					  param[4]*param[2]^2+param[4]*param[3]^2
		  	}
		  	if(order==3)
			{
				3*param[1]*param[4]*param[2]^2+3*param[1]^2*param[4]*param[3]+
					  3*param[1]*param[4]^2*param[3]^2+param[1]^3+
					  3*param[1]*param[4]*param[3]^2+param[4]^3*param[3]^3+
					  3*param[4]^2*param[3]*param[2]^2+3*param[4]^2*param[3]^3+
					  3*param[4]*param[2]^2*param[3]+2*param[4]*param[3]^3
			}
			  
		  	if(order==4)
			{
				6*param[1]^2*param[4]*param[3]^2+18*param[4]^2*param[3]^2*param[2]^2+
					  12*param[4]*param[2]^2*param[3]^2+
					  12*param[1]*param[4]*param[2]^2*param[3]+4*param[1]^3*param[4]*param[3]+
					  8*param[1]*param[4]*param[3]^3+12*param[1]*param[4]^2*param[3]*param[2]^2+
					  6*param[1]^2*param[4]^2*param[3]^2+param[1]^4+
					  6*param[1]^2*param[4]*param[2]^2+param[4]^4*param[3]^4+
					  6*param[4]^3*param[3]^2*param[2]^2+6*param[4]^3*param[3]^4+
					  3*param[4]^2*param[2]^4+11*param[4]^2*param[3]^4+
					  3*param[4]*param[2]^4+6*param[4]*param[3]^4+
					  12*param[1]*param[4]^2*param[3]^3+4*param[1]*param[4]^3*param[3]^3
			}
			else
				stop("order must be 1,2,3 or 4")
		}
		if(type=="kappa")
		{
			if(order==1)
			{
				(1/2)*(param[4]*param[2]*sqrt(2)-param[4]*param[2]*sqrt(2)*param[2]^2+2*param[1]*param[2])/param[2]
			}
			if(order==2)
			{
				(1/2)*(-2*param[1]*param[4]*param[2]*(param[2]+1)*(param[2]-1)*param[2]*sqrt(2)+
							param[4]*param[2]^2*(param[4]+1)*param[2]^4+(-2*param[4]^2*param[2]^2+2*param[1]^2)*param[2]^2+
							param[4]*param[2]^2*(param[4]+1))/param[2]^2
			}
			if(order==3)
			{
				(1/4)*(-(param[2]-1)*param[4]*param[2]*(param[2]+1)*(((-2*param[2]^2+1+param[2]^4)*param[4]+
									2*param[2]^2+2*param[2]^4+2)*(param[4]+1)*param[2]^2+
								6*param[1]^2*param[2]^2)*sqrt(2)+(6*(param[4]*((-2*param[2]^2+1+param[2]^4)*param[4]+
										param[2]^4+1)*param[2]^2+(2/3)*param[1]^2*param[2]^2))*
							param[2]*param[1])/param[2]^3
			}
			
			if(order==4)
			{
				1/4*(4*param[1]^4*param[2]^4+param[4]^4*param[2]^4+6*param[4]^3*param[2]^4+11*param[4]^2*param[2]^4+6*param[4]*param[2]^4-24*param[4]^2*param[2]^4*param[1]^2*param[2]^2+12*param[4]^2*param[2]^2*param[1]^2*param[2]^2+12*param[4]^2*param[2]^6*param[1]^2*param[2]^2+12*param[4]*param[2]^6*param[1]^2*param[2]^2-4*param[4]^4*param[2]^4*param[2]^2+6*param[4]^4*param[2]^4*param[2]^4-4*param[4]^4*param[2]^4*param[2]^6+param[4]^4*param[2]^4*param[2]^8-12*param[4]^3*param[2]^4*param[2]^2+12*param[4]^3*param[2]^4*param[2]^4-12*param[4]^3*param[2]^4*param[2]^6+6*param[4]^3*param[2]^4*param[2]^8+11*param[4]^2*param[2]^4*param[2]^8-8*param[4]^2*param[2]^4*param[2]^2+6*param[4]^2*param[2]^4*param[2]^4-8*param[4]^2*param[2]^4*param[2]^6+6*param[4]*param[2]^4*param[2]^8+12*param[4]*param[2]^2*param[1]^2*param[2]^2-12*param[4]^3*param[2]^3*param[2]^3*param[1]*2^(1/2)+4*param[4]^3*param[2]^3*param[2]*param[1]*2^(1/2)+12*param[4]^2*param[2]^3*param[2]*param[1]*2^(1/2)+12*param[4]^3*param[2]^3*param[2]^5*param[1]*2^(1/2)+12*param[4]^2*param[2]^3*param[2]^5*param[1]*2^(1/2)+8*param[4]*param[2]^3*param[2]*param[1]*2^(1/2)+8*param[4]*param[2]*param[2]^3*param[1]^3*2^(1/2)-12*param[4]^2*param[2]^3*param[2]^3*param[1]*2^(1/2)-4*param[4]^3*param[2]^3*param[2]^7*param[1]*2^(1/2)-12*param[4]^2*param[2]^3*param[2]^7*param[1]*2^(1/2)-8*param[4]*param[2]^3*param[2]^7*param[1]*2^(1/2)-8*param[4]*param[2]*param[2]^5*param[1]^3*2^(1/2))/param[2]^4
			}
			else
				stop("order must be 1,2,3 or 4")
		}
	}
}
