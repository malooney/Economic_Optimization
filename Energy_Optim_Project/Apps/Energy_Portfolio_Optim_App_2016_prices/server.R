

cat("\014")
rm(list=ls())
set.seed(12345)
options(scipen=999)

library(readr)
library(shiny)
library(DT)
library(quadprog)
library(dygraphs)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output) {
  
  output$HistPlot <- renderPlot({
    
    energy_mix_percent_csv <<- read_csv("/Users/malooney/Google Drive/digitalLibrary/*AAEC6305_Economic_Optimization/Economic_Optimization/Energy_Optim_Project/data/energy_mix_percent_csv.csv")
    
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
    
    sourcePercent_plot <- data.frame(Natural_Gas=gas_plot, Coal=coal_plot, Nuclear=nuke_plot, Wind=wind_plot, Hyrdroelectric=hydro_plot, years=years[-10])
    
    melted_sourcePercent_plot <- melt(sourcePercent_plot, id.vars="years")
    colnames(melted_sourcePercent_plot) <- c("years", "source", "source_percent")
    
    ggplot(data=melted_sourcePercent_plot, aes(years, source_percent, col=source))+
      geom_point()+
      stat_smooth(se=F)+ 
      theme_bw()+
      ggtitle("Historical Data")
    
  })
    
    output$tbl1 = DT::renderDataTable(
      
      datatable(energy_mix_percent_csv[1:13,c(1,5:10)], options = list(dom = 't'))
      
      )
    
   
  output$distPlot <- renderPlot({
    
    # Expxected cost of resources in 2022 [million USD/TWh]
    # Source: http://en.wikipedia.org/wiki/Cost_of_electricity_by_source
    c <- matrix(c(71, 90, 59, 88, 111, 156, 76))
    
    #Maximum expected cost [million USD/TWh]
    cmax <- input$cost 
    
    #ERCOT Demand in 2022 [TWh]
    d <- input$demand
    
    #Standard deviation of resource cost [million USD/TWh]
    sig <- c(11, 30, 9, 9.5, 30, 16, 10)
    
    Q <- diag(2* sig^2)
    R <- c(rep(0,7))
    
    a1 <- matrix(rep(-1, 7), nrow=1) #sum of source mix >= demand
    a2 <- t(c-cmax) # cost constraint
    a3 <- diag(c(-1, -1, -1, -1, -1, -1, -1)) #Non-negativity
    a4 <- matrix(c(0, 1, 0, 0, 0, 0, 0), nrow=1) #Hydro <=0.05% TWh 
    a5 <- matrix(c(-1, 0, -1, -1, 0, 0, 0), nrow=1) # 60% <= base power 
    a6 <- matrix(c(1, 0, 1, 1, 0, 0, 0), nrow=1) #base power <= 90%
    a7 <- matrix(c(0, 0, 0, 0, 0, 1, 0), nrow=1) #PV Solar <= 7%
    a8 <- matrix(c(0, 0, 0, 0, 0, 0, 1), nrow=1) #wind <= 25%
    a9 <- matrix(c(0, 0, 0, 0, 1, 0, 0), nrow=1) #wind <= 0.05%
    
    # constraints A=LHS, b=RHS
    A <- rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9)
    b <- c(d, 0, 0, 0, 0, 0, 0, 0, 0, -d*0.005, d*.6, -d*0.9, -d*0.07, -d*0.25, -d*0.005)
    
    qp <- solve.QP(Dmat=Q, dvec=R, Amat=t(A), bvec=b)
    
    qp.soln <- data.frame(id=1:length(qp$solution), source_TWh=-qp$solution)
    
    rownames(qp.soln) <- c("coal", "hydro", "natural_gas", "nuclear", "biomass", "solar", "wind")
    
    qp.soln.tmp <- data.frame(percent=(100*(qp.soln$source/d)))
    
    qp.soln <- cbind(qp.soln, qp.soln.tmp)
    
    qp.soln <- round(qp.soln, digits = 2)
    
    plot(qp.soln[,2], type="h", ylab="Power provisioned from resource i [TWh]")
    
    output$tbl = DT::renderDataTable(datatable(qp.soln, options = list(dom = 't')))
    
  })
  
  #output$distPlot1 <- renderPlot({
  output$gatePlot <- renderDygraph({
    
    # Expxected cost of resources in 2022 [million USD/TWh]
    # Source: http://en.wikipedia.org/wiki/Cost_of_electricity_by_source
    c <- matrix(c(71, 90, 59, 88, 111, 156, 76))
    
    #Standard deviation of resource cost [million USD/TWh]
    sig <- c(11, 30, 9, 9.5, 30, 16, 10)
    
    Q <- diag(2* sig^2)
    R <- c(rep(0,7))
    
    #Maximum expected cost [million USD/TWh]
    cmax_range <- 61:150
    
    #ERCOT Demand in 2016 [TWh]
    d <- input$demand
    
    b <- c(d, 0, 0, 0, 0, 0, 0, 0, 0, -d*0.005, d*.6, -d*0.9, -d*0.07, -d*0.25, -d*0.005)
  
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
    data <- data.frame(cmax=cmax_range, risk=qp.value[,1]/1000000)
      
      dygraph(data,
              main= "Portfolio Risk / variance Plot",
              xlab= "Max Expected Energy Cost in 2020 [million USD/TWh]",
              ylab= "Variance / Risk [Billion USD^2]") %>%
        dyRangeSelector(height= 20) %>%
        dySeries("risk", label= "Risk") %>%
        dyLegend(show= "always", hideOnMouseOut= FALSE)
      
    })
    
  })
