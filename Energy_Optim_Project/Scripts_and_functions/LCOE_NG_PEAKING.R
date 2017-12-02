

library(triangle)

LCOE_NG_PEAKING <- function(terms=20, count=10000){
  # Natural Gas Peaking
  
  # Terms is the estimated lifetime of power plant
  # count is the number of Monte Carlo replications
  
  ##############################################################################
  i <- matrix(nrow=count, ncol=1)   # interest rate (%)
  Cf <- matrix(nrow=count, ncol=1)  # Capacity Factor (%)
  C_c <- matrix(nrow=count, ncol=1) # Capital Cost ($/MWh)
  OMf <- matrix(nrow=count, ncol=1) # Fixed OM Costs ($/MW/Yr)
  Fc <- matrix(nrow=count, ncol=1)  # Fuel Cost ($/MMBtu) - convert in-situ
  Q <- matrix(nrow=count, ncol=1)   # Heat Rate (Btu/KWh) - convert in-situ
  OMv <- matrix(nrow=count, ncol=1) # Variable OM Costs ($/MWh)
  ##############################################################################
  ## model interest rate (%) from Triangular Distn.
  ii <- 1
  while( is.na(i[length(i)]) ){
    
    temp <- rtriangle(1, 5, 15, 10)
    if(temp >= 5 && temp <= 15){
      i[ii,1] <- temp/100
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Capacity Factor (%) from Normal Distn.
  ii <- 1
  while( is.na(Cf[length(Cf)]) ){
    
    temp <- rnorm(n=1, mean=7.5, sd=sqrt(1.25))
    if(temp >= 5 && temp <= 10){
      Cf[ii,1] <- temp/100
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Capital Costs with Triangular Distn.
  ii <- 1
  while( is.na(C_c[length(C_c)]) ){
    
    temp <- rtriangle(1, 600, 1200, 850)
    if(temp >= 600 && temp <= 1200){
      C_c[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Fixed OM Costs from Log Normal Distn.
  ii <- 1
  while( is.na(OMf[length(OMf)]) ){
    
    temp <- rlnorm(n=1, meanlog = 2.39, sdlog = 0.4)
    if(temp >= 4.9 && temp <= 24.30){
      OMf[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Fuel Cost from Triangular Distn.
  ii <- 1
  while( is.na(Fc[length(Fc)]) ){
    
    temp <- rtriangle(1, 3.42, 9.02, 4.5)
    if(temp >= 3.42 && temp <= 9.02){
      Fc[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Heat Rate from Normal Distn.
  ii <- 1
  while( is.na(Q[length(Q)]) ){
    
    temp <- rnorm(n=1, mean=9925, sd=sqrt(462.5))
    if(temp >= 9000 && temp <= 10850){
      Q[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Variable OM Costs from Log Normal Distn.
  ii <- 1
  while( is.na(OMv[length(OMv)]) ){
    
    temp <- rlnorm(n=1, meanlog = 1.95, sdlog = 0.418)
    if(temp >= 3.05 && temp <= 16.22){
      OMv[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  P <- (C_c)*  ( i+ (i/ ( ((i+ 1)^terms)- 1) ) )
  
  LCOE_ng_peaking= ( (P+ OMf )/ (8760* Cf) )+ (Fc* Q* 0.001)+ OMv
  ##############################################################################
  return(LCOE_ng_peaking)
  
}

