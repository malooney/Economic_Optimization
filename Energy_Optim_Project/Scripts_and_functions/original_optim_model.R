
################################################################################
##### Energy Portfolio Optimization using Quadratic Programing Model
##### Reproduce Orgiginal Model
################################################################################

original_optim_model <- function(){

library(quadprog)

# Expxected cost of resources in 2022 [million USD/TWh]
# Source: http://en.wikipedia.org/wiki/Cost_of_electricity_by_source
c <- matrix(c(100, 90, 130, 108, 111, 90, 144, 87))

#Standard deviation of resource cost [million USD/TWh]
sig <- c(22, 30, 15, 20, 30, 36, 32, 40)

#Maximum expected cost [million USD/TWh]
cmax <- 100

#ERCOT Demand in 2022 [TWh]
d <- 225

Q <- diag(2* sig^2)
R <- c(rep(0, 8))

a1 <- matrix(rep(-1, 8), nrow=1)
a2 <- t(c-cmax)
a3 <- diag(c(-1, -1, -1, -1, -1, -1, -1, -1))

# constraints A=LHS, b=RHS
A <- rbind(a1, a2, a3)
b <- c(d, 1, 0, 0, 0, 0, 0, 0, 0, 0)

qp <- solve.QP(Dmat=Q, dvec=R, Amat=t(A), bvec=b, factorized=F)

qp.soln <- data.frame(id=1:length(c), source= -qp$solution)

rownames(qp.soln) <- c("coal", "hydro", "natural_gas", "nuclear", "biomass", "geo", "solar", "wind")

plot(qp.soln[,2], type="h", ylab="Power provisioned from resource i [TWh]")

################################################################################
#Maximum expected cost [million USD/TWh]
cmax_range <- 88:150
qp.value <- data.frame(risk=rep(NA, length(cmax_range)))
i <- 1
for(i in 1:length(cmax_range)){
  a1_range <- matrix(rep(-1, 8), nrow=1)
  a2_range <- t(c- cmax_range[i])
  a3_range <- diag(c(-1, -1, -1, -1, -1, -1, -1, -1))
  A_range <- rbind(a1_range, a2_range, a3_range)
  
  qp_range <- solve.QP(Dmat=Q, dvec=R, Amat=t(A_range), bvec=b)
  qp.value[i,1] <- qp_range$value
  
  i <- i+1
}

plot(x=cmax_range, y=qp.value[,1]/1000000, type="l", xlab="Max Expected Energy Cost in 2020 [million USD/TWh]", ylab="Variance / Risk [Billion USD^2]")

}
