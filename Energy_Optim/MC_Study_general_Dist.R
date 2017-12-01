

library(MASS)

cat("\014")
rm(list=ls())
set.seed(12345)

source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim/LCOE_Coal.R")

mu <- c(0,0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow=2)
e <- mvrnorm(10000, mu, Sigma)

LCOE_c <- LCOE_coal()
LCOE2 <- rnorm(n=10000, mean=58.97, sd=sqrt(112.5))

LCOE_c <- matrix(LCOE_c+ e[,1], ncol=1)
l2 <- matrix(LCOE2+ e[,2], ncol=1)


var(LCOE_c)
var(l2)
cov(LCOE_c, l2)
plot(density(LCOE_c))
summary(LCOE_c)
