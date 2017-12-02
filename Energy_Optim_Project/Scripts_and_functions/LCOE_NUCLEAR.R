

library(triangle)

LCOE_NUCLEAR <- function(i=0.10, Fc=0.65, terms=40, count=10000){
  # Nuclear
  
  # Terms is the estimated lifetime of power plant
  # count is the number of Monte Carlo replications
  # i is the interest rate on capital costs
  # Fc is fuel cost ($/MMBtu) - convert in-situ
  
  ##############################################################################
  Cf <- matrix(nrow=count, ncol=1)  # Capacity Factor (%)
  C_c <- matrix(nrow=count, ncol=1) # Capital Cost ($/MWh)
  OMf <- matrix(nrow=count, ncol=1) # Fixed OM Costs ($/MW/Yr)
  Q <- matrix(nrow=count, ncol=1)   # Heat Rate (Btu/KWh) - convert in-situ
  OMv <- matrix(nrow=count, ncol=1) # Variable OM Costs ($/MWh)

  ##############################################################################
  ## model Capacity Factor (%) from Normal Distn.
  ii <- 1
  while( is.na(Cf[length(Cf)]) ){
    
    temp <- rnorm(n=1, mean=87.5, sd=sqrt(1.25))
    if(temp >= 85 && temp <= 90){
      Cf[ii,1] <- temp/100
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Capital Costs with Log Normal Distn.
  ii <- 1
  while( is.na(C_c[length(C_c)]) ){
    
    temp <- rlnorm(n=1, meanlog = 8.7, sdlog = 0.185)
    if(temp >= 4146 && temp <= 8691){
      C_c[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Fixed OM Costs from Normal Distn.
  ii <- 1
  while( is.na(OMf[length(OMf)]) ){
    
    temp <- rnorm(n=1, mean=87.69, sd=sqrt(16.75)) 
    if(temp >= 54.19 && temp <= 121.19){
      OMf[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Heat Rate from Normal Distn.
  ii <- 1
  while( is.na(Q[length(Q)]) ){
    
    temp <- rnorm(n=1, mean=10450, sd=sqrt(15))
    if(temp >= 10420 && temp <= 10480){
      Q[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Variable OM Costs from Triangle Distn.
  ii <- 1
  while( is.na(OMv[length(OMv)]) ){
    
    temp <- rtriangle(1, 0.42, 2.14, 1.28)
    if(temp >= 0.42 && temp <= 2.14){
      OMv[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  P <- (C_c)*  ( i+ (i/ ( ((i+ 1)^terms)- 1) ) )
  
  LCOE_nuclear= ( (P+ OMf )/ (8760* Cf) )+ (Fc* Q* 0.001)+ OMv
  ##############################################################################
  return(LCOE_nuclear)
  
}

