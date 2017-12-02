

library(triangle)

LCOE_SOLAR_PV <- function(i=0.10, terms=20, count=10000){
  # Solar PV
  
  # Terms is the estimated lifetime of power plant
  # count is the number of Monte Carlo replications
  # i is interest rate (%)
  
  ##############################################################################
  Cf <- matrix(nrow=count, ncol=1)  # Capacity Factor (%)
  C_c <- matrix(nrow=count, ncol=1) # Capital Cost ($/MWh)
  OMf <- matrix(nrow=count, ncol=1) # Fixed OM Costs ($/MW/Yr)
  OMv <- matrix(nrow=count, ncol=1) # Variable OM Costs ($/MWh)
  ##############################################################################
  ## model Capacity Factor (%) from Normal Distn.
  ii <- 1
  while( is.na(Cf[length(Cf)]) ){
    
    temp <- rnorm(n=1, mean=21.74, sd=sqrt(3.13))
    if(temp >= 15.48 && temp <= 28){
      Cf[ii,1] <- temp/100
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Capital Costs with Log Normal Distn.
  ii <- 1
  while( is.na(C_c[length(C_c)]) ){
    
    temp <- rlnorm(n=1, meanlog = 7.933, sdlog = 0.292)
    if(temp >= 1554 && temp <= 5000){
      C_c[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Fixed OM Costs from Normal Distn.
  ii <- 1
  while( is.na(OMf[length(OMf)]) ){
    
    temp <- rnorm(n=1, mean=21, sd=sqrt(6.86))
    if(temp >= 7.28 && temp <= 34.72){
      OMf[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Variable OM Costs from Log Normal Distn.
  ii <- 1
  while( is.na(OMv[length(OMv)]) ){
    
    temp <- rlnorm(n=1, meanlog = 2.418, sdlog = 0.325)
    if(temp >= 5.86 && temp <= 21.5){
      OMv[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  P <- (C_c)*  ( i+ (i/ ( ((i+ 1)^terms)- 1) ) )
  
  LCOE_solar_pv= ( (P+ OMf )/ (8760* Cf) )+ OMv
  ##############################################################################
  return(LCOE_solar_pv)
  
}

