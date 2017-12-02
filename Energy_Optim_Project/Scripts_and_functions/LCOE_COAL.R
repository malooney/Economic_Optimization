

library(triangle)

LCOE_COAL <- function(terms=40, Cf=0.93, count=10000){
  # Coal
  
  # Terms is the estimated lifetime of power plant
  # count is the number of Monte Carlo replications
  
  ##############################################################################
  i <- matrix(nrow=count, ncol=1)   # interest rate (%)
  C_c <- matrix(nrow=count, ncol=1) # Capital Cost ($/MWh)
  OMf <- matrix(nrow=count, ncol=1) # Fixed OM Costs ($/MW/Yr)
  Fc <- matrix(nrow=count, ncol=1)  # Fuel Cost ($/MMBtu) - convert in-situ
  Q <- matrix(nrow=count, ncol=1)   # Heat Rate (Btu/KWh) - convert in-situ
  OMv <- matrix(nrow=count, ncol=1) # Variable OM Costs ($/MWh)
  ##############################################################################
  ## model interest rate (%) from Triangular Distn.
  ii <- 1
  while( is.na(i[length(i)]) ){
    
    temp <- rtriangle(1, 5, 12, 7)
    if(temp >= 5 && temp <= 12){
      i[ii,1] <- temp/100
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Capital Costs with Log Normal Distn.
  ii <- 1
  while( is.na(C_c[length(C_c)]) ){
    
    temp <- rlnorm(n=1, meanlog = 8.182, sdlog = 0.407)
    if(temp >= 1584 && temp <= 8071){
      C_c[ii,1] <- temp/0.001
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Fixed OM Costs from Normal Distn.
  ii <- 1
  while( is.na(OMf[length(OMf)]) ){
    
    temp <- rnorm(n=1, mean=25.27, sd=sqrt(2.8)) 
    if(temp >= 19.67 && temp <= 30.80){
    OMf[ii,1] <- temp/0.001
    ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Fuel Cost from Normal Distn.
  ii <- 1
  while( is.na(Fc[length(Fc)]) ){
    
    temp <- rnorm(n=1, mean=1.84, sd=sqrt(0.285)) 
    if(temp >= 1.27 && temp <= 2.41){
      Fc[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Heat Rate from Normal Distn.
  ii <- 1
  while( is.na(Q[length(Q)]) ){
    
    temp <- rnorm(n=1, mean=10380, sd=sqrt(812.5))
    if(temp >= 8755 && temp <= 12005){
      Q[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  ## model Variable OM Costs from Normal Distn.
  ii <- 1
  while( is.na(OMv[length(OMv)]) ){
    
    temp <- rnorm(n=1, mean=4.15, sd=sqrt(0.975))
    if(temp >= 2.2 && temp <= 6.1){
      OMv[ii,1] <- temp
      ii <- ii+ 1 
    } else{}
  }
  ##############################################################################
  P <- (C_c)*  ( i+ (i/ ( ((i+ 1)^terms)- 1) ) )
  
  LCOE_c= ( (P+ OMf )/ (8760* Cf) )+ (Fc* Q* 0.001)+ OMv
  ##############################################################################
  return(LCOE_c)
  
}

