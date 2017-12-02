

library(triangle)

LCOE_WIND <- function(i=0.10, terms=20, count=10000){
  # Wind
  
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
    
    temp <- rnorm(n=1, mean=36.75, sd=sqrt(7))
    if(temp >= 22.75 && temp <= 50.75){
      Cf[ii,1] <- temp/100
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Capital Costs with Normal Distn.
  ii <- 1
  while( is.na(C_c[length(C_c)]) ){
    
    temp <- rnorm(n=1, mean=1970, sd=sqrt(350))
    if(temp >= 1270 && temp <= 2670){
      C_c[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Fixed OM Costs from Triangular Distn.
  ii <- 1
  while( is.na(OMf[length(OMf)]) ){
    
    temp <- rtriangle(1, 12, 60, 35)
    if(temp >= 12 && temp <= 60){
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
  
  LCOE_wind= ( (P+ OMf )/ (8760* Cf) )+ OMv
  ##############################################################################
  return(LCOE_wind)
  
}

