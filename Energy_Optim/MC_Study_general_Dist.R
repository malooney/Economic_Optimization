

library(MASS)

cat("\014")
rm(list=ls())
set.seed(12345)

source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim/LCOE_COAL.R")
source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim/LCOE_NGCC.R")
source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim/LCOE_NG_PEAKING.R")
source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim/LCOE_NUCLEAR.R")
source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim/LCOE_WIND.R")
source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim/LCOE_SOLAR_PV.R")

mu <- c(0,0,0,0,0,0)
Sigma <- matrix(c(1, 0.5, 0.5, 0.5, 0.5, 0.5,
                  0.5, 1, 0.5, 0.5, 0.5, 0.5,
                  0.5, 0.5, 1, 0.5, 0.5, 0.5,
                  0.5, 0.5, 0.5, 1, 0.5, 0.5,
                  0.5, 0.5, 0.5, 0.5, 1, 0.5,
                  0.5, 0.5, 0.5, 0.5, 0.5, 1), nrow=6)

e <- mvrnorm(10000, mu, Sigma)

LCOE_c <- LCOE_COAL()
LCOE_ngcc <- LCOE_NGCC()
LCOE_ng_peaking <- LCOE_NG_PEAKING()
LCOE_nuclear <- LCOE_NUCLEAR()
LCOE_wind <- LCOE_WIND()
LCOE_solar_pv <- LCOE_SOLAR_PV()

LCOE_c <- matrix(LCOE_c+ e[,1], ncol=1)
LCOE_ngcc <- matrix(LCOE_ngcc+ e[,2], ncol=1)
LCOE_ng_peaking <- matrix(LCOE_ng_peaking+ e[,3], ncol=1)
LCOE_nuclear <- matrix(LCOE_nuclear+ e[,4], ncol=1)
LCOE_wind <- matrix(LCOE_wind+ e[,5], ncol=1)
LCOE_solar_pv <- matrix(LCOE_solar_pv+ e[,6], ncol=1)


sqrt(var(LCOE_c))
sqrt(var(LCOE_ngcc))
sqrt(var(LCOE_ng_peaking))
sqrt(var(LCOE_nuclear))
sqrt(var(LCOE_wind))
sqrt(var(LCOE_solar_pv))
################################################################################
## plot LCOE density for Coal, NGCC, NG_Peaking, Nuclear
data_matrix <- data.frame(cbind(LCOE_c, LCOE_ngcc, LCOE_ng_peaking, 
                                LCOE_nuclear, LCOE_wind, LCOE_solar_pv))
colnames(data_matrix) <-  c("Coal", "NGCC", "NG_Peaking", "Nuclear", "Wind", 
                            "Solar_PV")
data_matrix <- melt(data_matrix[,1:4])
ggplot(data_matrix, aes(value, fill = variable, colour = variable))+ 
  geom_density(alpha = 0.1)+ geom_rug(sides = "bl")

summary(LCOE_c)
summary(LCOE_ngcc)
summary(LCOE_ng_peaking)
summary(LCOE_nuclear)
summary(LCOE_wind)
summary(LCOE_solar_pv)
