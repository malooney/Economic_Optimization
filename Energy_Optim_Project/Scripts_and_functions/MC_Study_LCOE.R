

MC_Study_LCOE <- function(){

################################################################################
## Housekeeping
library('MASS')
library('reshape2')
library('ggplot2')

#cat("\014") # clear workspace
#rm(list=ls()) # remove all variables from environment
#set.seed(12345) # set random seed for reproducibility
################################################################################

################################################################################
## import LCOE functions
source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim_Project/Scripts_and_functions/LCOE_COAL.R")

source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim_Project/Scripts_and_functions/LCOE_NGCC.R")

source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim_Project/Scripts_and_functions/LCOE_NG_PEAKING.R")

source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim_Project/Scripts_and_functions/LCOE_NUCLEAR.R")

source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim_Project/Scripts_and_functions/LCOE_WIND.R")

source("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim_Project/Scripts_and_functions/LCOE_SOLAR_PV.R")
################################################################################

################################################################################
## create variance-covariance matrix framework
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

################################################################################
## plot LCOE density for Coal, NGCC, NG_Peaking, Nuclear

LCOE_MC_data_matrix <- data.frame(cbind(LCOE_c, LCOE_ngcc, LCOE_ng_peaking, 
                                LCOE_nuclear, LCOE_wind, LCOE_solar_pv))

colnames(LCOE_MC_data_matrix) <-  c("Coal", "NGCC", "NG_Peaking", "Nuclear", "Wind", "Solar_PV")

LCOE_MC_data_matrix <<- LCOE_MC_data_matrix

LCOE_MC_data_matrix_1_4 <- melt(LCOE_MC_data_matrix[,1:4], id.vars = NULL)

 density_plot1 <<- ggplot(LCOE_MC_data_matrix_1_4, aes(value, fill = variable, colour = variable))+ 
  geom_density(alpha = 0.1)+ 
  geom_rug(sides = "bl", alpha = 0.1)+ 
  theme_bw()+ 
  xlab("LCOE ($/MWh)")
################################################################################

################################################################################
LCOE_MC_data_matrix_5_6 <- melt(LCOE_MC_data_matrix[,5:6], id.vars = NULL)

density_plot2 <<- ggplot(LCOE_MC_data_matrix_5_6, aes(value, fill = variable, colour = variable))+ 
  geom_density(alpha = 0.1)+ 
  geom_rug(sides = "bl", alpha = 0.1)+ 
  theme_bw()+ 
  xlab("LCOE ($/MWh)")
################################################################################


}
