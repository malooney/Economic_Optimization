
################################################################################
##### Energy Portfolio Optimization using Quadratic Programing Model ###########
##### Energy Demand= using 2016 Total Net energy Generation for ERCOT ##########
################################################################################

optim_model_2016_demand <- function(){
###############################################################################
## Housekeeping
library(quadprog)
###############################################################################

################################################################################
# Expxected cost of resources in 2022 [million USD/TWh]
# Source: http://en.wikipedia.org/wiki/Cost_of_electricity_by_source
c <- matrix(c(71, 90, 59, 88, 111, 156, 76))

#Standard deviation of resource cost [million USD/TWh]
sig <- c(11, 30, 9, 9.5, 30, 16, 10)

#Maximum expected cost [million USD/TWh]
cmax <- 80

#ERCOT Demand in 2016 [TWh]
d <- 351

Q <- diag(2* sig^2)
R <- c(rep(0, 7))

a1 <- matrix(rep(-1, 7), nrow=1) #sum of source mix >= demand
a2 <- t(c-cmax) # cost constraint
a3 <- diag(c(-1, -1, -1, -1, -1, -1, -1)) #Non-negativity
a4 <- matrix(c(0, 1, 0, 0, 0, 0, 0), nrow=1) #Hydro <=2 TWh 
a5 <- matrix(c(-1, 0, -1, -1, 0, 0, 0), nrow=1) # 60% <= base power 
a6 <- matrix(c(1, 0, 1, 1, 0, 0, 0), nrow=1) #base power <= 90%
a7 <- matrix(c(0, 0, 0, 0, 0, 1, 0), nrow=1) #PV Solar <= 7%
a8 <- matrix(c(0, 0, 0, 0, 0, 0, 1), nrow=1) #wind <= 25%
a9 <- matrix(c(0, 0, 0, 0, 1, 0, 0), nrow=1) #wind <= 0.05%

# constraints A=LHS, b=RHS
A <- rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9)
b <- c(d, 0, 0, 0, 0, 0, 0, 0, 0, -d*0.005, d*.6, -d*0.9, -d*0.07, -d*0.25, -d*0.005)

qp <- solve.QP(Dmat=Q, dvec=R, Amat=t(A), bvec=b, factorized=F)

qp.soln <- data.frame(id=1:length(c), source= -qp$solution)

rownames(qp.soln) <- c("coal", "hydro", "natural_gas", "nuclear", "biomass", "solar", "wind")

prcnt <- data.frame(percent=100*qp.soln[,2]/sum(qp.soln[,2]))

qp.soln <- cbind(qp.soln, prcnt)

plot(qp.soln[,2], type="h", ylab="Power provisioned from resource i [TWh]")

################################################################################

################################################################################
#Maximum expected cost [million USD/TWh]
cmax_range <- 61:150
qp.value <- data.frame(risk=rep(NA, length(cmax_range)))
i <- 1
for(i in 1:length(cmax_range)){
  
  a1_range <- matrix(rep(-1, 7), nrow=1)
  a2_range <- t(c- cmax_range[i])
  a3_range <- diag(c(-1, -1, -1, -1, -1, -1, -1))
  a4_range <- matrix(c(0, 1, 0, 0, 0, 0, 0), nrow=1)
  a5_range <- matrix(c(-1, 0, -1, -1, 0, 0, 0), nrow=1) # 60% <= base power 
  a6_range <- matrix(c(1, 0, 1, 1, 0, 0, 0), nrow=1) #base power <= 90%
  a7_range <- matrix(c(0, 0, 0, 0, 0, 1, 0), nrow=1) #PV Solar <= 7%
  a8_range <- matrix(c(0, 0, 0, 0, 0, 0, 1), nrow=1) #wind <= 25%
  a9_range <- matrix(c(0, 0, 0, 0, 1, 0, 0), nrow=1) #wind <= 0.05%
  
  A_range <- rbind(a1_range, a2_range, a3_range, a4_range, a5_range, a6_range, a7_range, a8_range, a9_range)
  
  qp_range <- solve.QP(Dmat=Q, dvec=R, Amat=t(A_range), bvec=b)
  qp.value[i,1] <- qp_range$value
  
  i <- i+1
}

plot(x=cmax_range, y=qp.value[,1]/1000000, type="l", xlab="Max Expected Energy Cost in 2020 [million USD/TWh]", ylab="Variance / Risk [Billion USD^2]")

}