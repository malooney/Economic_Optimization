

library(triangle)

LCOE_NGCC <- function(terms=20, count=10000){
  # Natural Gas Combustion Cycle
  
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
  ## model Capacity Factor (%) from Triangular Distn.
  ii <- 1
  while( is.na(Cf[length(Cf)]) ){
    
    temp <- rtriangle(1, 40, 87, 80)
    if(temp >= 40 && temp <= 87){
      Cf[ii,1] <- temp/100
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Capital Costs with Log Normal Distn.
  ii <- 1
  while( is.na(C_c[length(C_c)]) ){
    
    temp <- rlnorm(n=1, meanlog = 6.927, sdlog = 0.3)
    if(temp >= 559 && temp <= 1858){
      C_c[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Fixed OM Costs from Triangular Distn.
  ii <- 1
  while( is.na(OMf[length(OMf)]) ){
    
    temp <- rtriangle(1, 5.5, 15.37, 7.28)
    if(temp >= 5.5 && temp <= 15.37){
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
    
    temp <- rnorm(n=1, mean=6740, sd=sqrt(155))
    if(temp >= 6430 && temp <= 7050){
      Q[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Variable OM Costs from Normal Distn.
  ii <- 1
  while( is.na(OMv[length(OMv)]) ){
    
    temp <- rnorm(n=1, mean=2.57, sd=sqrt(0.58))
    if(temp >= 1.41 && temp <= 3.73){
      OMv[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  P <- (C_c)*  ( i+ (i/ ( ((i+ 1)^terms)- 1) ) )
  
  LCOE_ngcc= ( (P+ OMf )/ (8760* Cf) )+ (Fc* Q* 0.001)+ OMv
  ##############################################################################
  return(LCOE_ngcc)
  
}

