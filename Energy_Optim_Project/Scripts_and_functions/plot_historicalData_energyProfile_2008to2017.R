
################################################################################
######## Historical data plot for energy profile mix from 2008-2017 ############
################################################################################

plot_historicalData_energyProfile_2008to2017 <- function(){
################################################################################
## Housekeeping
library(readr)
library(ggthemes)
library(ggplot2)
library(reshape2)
################################################################################

################################################################################
## data import and manipulation
energy_mix_percent_csv <<- read_csv("~/Desktop/Energy/energy_mix_percent_csv.csv")
years <- 2008:2017

gas_plot<- as.numeric(energy_mix_percent_csv[2, 2:10])
gas_plot <- matrix(gas_plot, nrow=9)

coal_plot<- as.numeric(energy_mix_percent_csv[4, 2:10])
coal_plot <- matrix(coal_plot, nrow=9)

nuke_plot<- as.numeric(energy_mix_percent_csv[5, 2:10])
nuke_plot <- matrix(nuke_plot, nrow=9)

wind_plot<- as.numeric(energy_mix_percent_csv[6, 2:10])
wind_plot <- matrix(wind_plot, nrow=9)

hydro_plot<- as.numeric(energy_mix_percent_csv[8, 2:10])
hydro_plot <- matrix(hydro_plot, nrow=9)

#solar_plot<- as.numeric(energy_mix_percent_csv[7, 2:11])
#solar_plot <- matrix(solar_plot, nrow=10)

sourcePercent_plot <- data.frame(Natural_Gas=gas_plot, Coal=coal_plot, 
                                 Nuclear=nuke_plot, Wind=wind_plot, 
                                 Hyrdroelectric=hydro_plot, years=years[-10])

melted_sourcePercent_plot <- melt(sourcePercent_plot, id.vars="years")
colnames(melted_sourcePercent_plot) <- c("years", "source", "source_percent")

################################################################################
## plot
plt <- ggplot(data=melted_sourcePercent_plot, aes(years, source_percent, 
                                                  col=source))+
  geom_point()+
  stat_smooth(se=F)+ 
  theme_bw()+
  ggtitle("Historical Data")

################################################################################

return(plt)

################################################################################
}