# ApexRMS
# Updated 2025-02-25
# Run after step4-FinalScenarios.R
# This script creates figures for single-cell models without transitions

library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

rootPath <- "E:/gitprojects/A329-LucasBarataria/"

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

pathOut <- paste0(rootPath,"Models/",modelName,"/OutputFigures/")

if(!dir.exists(pathOut)){
  dir.create(pathOut)
}

pathOutEmergent <- paste0(pathOut,"Emergent")

if(!dir.exists(pathOutEmergent)){
  dir.create(pathOutEmergent)
}

pathOutForest <- paste0(pathOut,"Forest")

if(!dir.exists(pathOutForest)){
  dir.create(pathOutForest)
}

scenarioList <- scenario(myProject, summary = T, results = T)

estId <- scenarioList$ScenarioId[grep("Estuarine Emergent Wetland: Add Uncertainty",scenarioList$Name)]

myScenarioE <- scenario(myProject, scenario=max(estId))

myDataStockE <- datasheet(myScenarioE, "stsim_OutputStock")
myDataFluxE <- datasheet(myScenarioE, "stsim_OutputFlow")

palId <- scenarioList$ScenarioId[grep("Palustrine Emergent Wetland: Add Uncertainty",scenarioList$Name)]

myScenarioP <- scenario(myProject, scenario=max(palId))

myDataStockP <- datasheet(myScenarioP, "stsim_OutputStock")
myDataFluxP <- datasheet(myScenarioP, "stsim_OutputFlow")

# Summarize Stocks

#unique(myDataStockE$StockGroupId)

plotStocks <- c("Biomass: Aboveground",
                "Biomass: Belowground",
                "DOM: Litter",
                "Biomass: Fine Root [Type]",
                "Biomass: Foliage [Type]",
                "DOM: Aboveground Very Fast [Type]",
                "DOM: Belowground Slow [Type]",
                "DOM: Belowground Very Fast [Type]")

plotStocksChange <- c("DOM: Soil",
                      "Deep Soil [Type]",
                      "Ecosystem Carbon Storage (tons C)",
                      "Biomass: Aboveground",
                      "Biomass: Belowground",
                      "DOM: Litter",
                      "Biomass: Fine Root [Type]",
                      "Biomass: Foliage [Type]",
                      "DOM: Aboveground Very Fast [Type]",
                      "DOM: Belowground Slow [Type]",
                      "DOM: Belowground Very Fast [Type]")

for (i in 1:length(plotStocksChange)){
  
  plotName <- gsub("[Type]","",gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotStocksChange[i]), fixed = T),fixed = T)), fixed = T)
  
  myDataStockEcarbonStorage <- myDataStockE %>%
    filter(StockGroupId == plotStocksChange[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Estuarine Emergent Wetland",
           Color = "#993C2D")
  
  myDataStockPcarbonStorage <- myDataStockP %>%
    filter(StockGroupId == plotStocksChange[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Palustrine Emergent Wetland",
           Color = "#6E89C2")
  
  myDataCarbonStorage <- myDataStockEcarbonStorage %>%
    bind_rows(myDataStockPcarbonStorage)
  
  table(table(myDataCarbonStorage$Timestep) == 2)
  
  col <- as.character(myDataCarbonStorage$Color)
  names(col) <- as.character(myDataCarbonStorage$Scenario)
  
  p3 <- ggplot(myDataCarbonStorage, aes(x = Timestep, y = mean, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = min, ymax = max, fill = Scenario, group = Scenario), alpha = 0.25, colour = NA) +
    theme_bw() +
    scale_color_manual(values=col) +
    scale_fill_manual(values=col) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nYear") + 
    ylab(paste0(gsub("Deep Soil","Deep Soil Accumulation",gsub("(tons C)","",gsub(" [Type]","",plotStocksChange[i], fixed = T), fixed = T)),"\n(tons C per ha)\n"))+
    ylim(0,NA)
  
  p3
  
  ggsave(paste0(pathOutEmergent,"/",plotName,"_","EmergentWetland_Line",".png"), p3, width = 3.3, height = 3.4, dpi = 600)
  
  rm(myDataStockEcarbonStorage,myDataStockPcarbonStorage,myDataCarbonStorage,plotName)
  
}

for (i in 1:length(plotStocks)){
  
  plotName <- gsub("[Type]","",gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotStocks[i]), fixed = T),fixed = T)), fixed = T)
  
  myDataStockEcarbonStorage <- myDataStockE %>%
    filter(StockGroupId == plotStocks[i]) %>%
    filter(Timestep == 2001) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Estuarine \nEmergent \nWetland",
           Color = "#993C2D")
  
  myDataStockPcarbonStorage <- myDataStockP %>%
    filter(StockGroupId == plotStocks[i]) %>%
    filter(Timestep == 2001) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Palustrine \nEmergent \nWetland",
           Color = "#6E89C2")
  
  myDataCarbonStorage <- myDataStockEcarbonStorage %>%
    bind_rows(myDataStockPcarbonStorage)
  
  col <- as.character(myDataCarbonStorage$Color)
  names(col) <- as.character(myDataCarbonStorage$Scenario)
  
  p3 <- ggplot(myDataCarbonStorage, aes(x = Scenario, y = totalC, fill = Scenario)) +
    geom_boxplot(alpha = 0.8) +
    theme_bw() +
    scale_fill_manual(values=col) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none",
          legend.title=element_blank()) +
    xlab("\nLand Cover Class") + 
    ylab(paste0(gsub("(tons C)","",gsub(" [Type]","",plotStocks[i], fixed = T), fixed = T),"\n(tons C per ha)\n"))+
    ylim(0,NA)
  
  p3
  
  ggsave(paste0(pathOutEmergent,"/",plotName,"_","EmergentWetland",".png"), p3, width = 3.3, height = 3.4, dpi = 600)
  
  rm(myDataStockEcarbonStorage,myDataStockPcarbonStorage,myDataCarbonStorage,plotName)
  
}

# Summarize Flows

#unique(myDataFluxE$FlowGroupId)

plotFlows<- c("Annual Emissions: CO2 (tons C per year)",             
              "Annual Emissions: CO2 (tons CO2-eq per year)",        
              "Annual Emissions: CO2 and CH4 (tons C per year)",     
              "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
              "Annual Net Flux (tons C per year)",                   
              "Annual Net Flux (tons CO2-eq per year)",              
              "Annual Net Growth (tons C per year)",                
              "Annual Net Growth (tons CO2-eq per year)",            
              "Annual Emissions: CH4 (tons C per year)",             
              "Annual Emissions: CH4 (tons CO2-eq per year)",        
              "Annual Lateral Flux (tons C per year)",               
              "Annual Lateral Flux (tons CO2-eq per year)",
              "Annual Net Ecosystem Carbon Balance (tons C per year)",
              "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)",
              "Decay: AG Very Fast -> BG Slow [Type]")

for (i in 1:length(plotFlows)){
  
  plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotFlows[i]), fixed = T),fixed = T))
  
  myDataFluxEnecb <- myDataFluxE %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    filter(Timestep == 2002) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Estuarine \nEmergent \nWetland",
           Color = "#993C2D")
  
  myDataFluxPnecb <- myDataFluxP %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    filter(Timestep == 2002) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Palustrine \nEmergent \nWetland",
           Color = "#6E89C2")
  
  myDataNECB <- myDataFluxEnecb %>%
    bind_rows(myDataFluxPnecb)
  
  minVal <- min(myDataNECB$totalC)
  maxVal <- max(myDataNECB$totalC)
  
  if(minVal > 0 & maxVal >0){
    minVal = 0
  } else if (minVal < 0 & maxVal < 0){
    maxVal = 0
  } 
  
  col <- as.character(myDataNECB$Color)
  names(col) <- as.character(myDataNECB$Scenario)
  
  p5 <- ggplot(myDataNECB, aes(x = Scenario, y = totalC, fill = Scenario)) +
    geom_boxplot(alpha = 0.8) +
    theme_bw() +
    scale_fill_manual(values=col) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none",
          legend.title=element_blank()) +
    xlab("\nLand Cover Class") + 
    ylab(paste0(gsub("Annual Net Ecosystem Carbon Balance","Annual NECB",
                     gsub(")"," per ha)",
                          gsub("(","\n(",plotFlows[i], fixed = T), fixed = T)),"\n"))+
    ylim(minVal,maxVal)
  
  p5
  
  ggsave(paste0(pathOutEmergent,"/",plotName,"_","EmergentWetland",".png"), p5, width = 3.3, height = 3.4, dpi = 600)
  
  rm(myDataFluxEnecb, myDataFluxPnecb,myDataNECB,plotName)

}

for (i in 1:length(plotFlows)){
  
  plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotFlows[i]), fixed = T),fixed = T))
  
  myDataFluxEnecb <- myDataFluxE %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Estuarine Emergent Wetland",
           Color = "#993C2D")
  
  myDataFluxPnecb <- myDataFluxP %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Palustrine Emergent Wetland",
           Color = "#6E89C2")
  
  myDataNECB <- myDataFluxEnecb %>%
    bind_rows(myDataFluxPnecb)
  
  minVal <- min(myDataNECB$min)
  maxVal <- max(myDataNECB$max)
  
  if(minVal > 0 & maxVal > 0){
    minVal = 0
  } else if (minVal < 0 & maxVal < 0){
    maxVal = 0
  } 
  
  
  table(table(myDataNECB$Timestep) == 2)
  
  col <- as.character(myDataNECB$Color)
  names(col) <- as.character(myDataNECB$Scenario)
  
  p5 <- ggplot(myDataNECB, aes(x = Timestep, y = mean, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = min, ymax = max, fill = Scenario, group = Scenario), alpha = 0.25, colour = NA) +
    theme_bw() +
    scale_color_manual(values=col) +
    scale_fill_manual(values=col) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nYear") + 
    ylab(paste0(gsub("Annual Net Ecosystem Carbon Balance","Annual NECB",
                     gsub(")"," per ha)",
                          gsub("(","\n(",plotFlows[i], fixed = T), fixed = T)),"\n"))+
    ylim(minVal,maxVal)
  
  p5
  
  ggsave(paste0(pathOutEmergent,"/",plotName,"_","EmergentWetland_Line",".png"), p5, width = 3.3, height = 3.4, dpi = 600)
  
  rm(myDataFluxEnecb, myDataFluxPnecb,myDataNECB,plotName)
  
}

rm(myDataStockE,
   myDataFluxE,
   myDataStockP,
   myDataFluxP)
gc()

forId <- scenarioList$ScenarioId[grep("Original Oak Gum Cypress Forest",scenarioList$Name)]

myScenarioF <- scenario(myProject, scenario=max(forId))

myDataStockF <- datasheet(myScenarioF, "stsim_OutputStock")
myDataFluxF <- datasheet(myScenarioF, "stsim_OutputFlow")

excludeId <- scenarioList$ScenarioId[grep("Updated: Ag to Palustrine Forested Wetland",scenarioList$Name)]

forWetId <- scenarioList$ScenarioId[grep("Palustrine Forested Wetland: Add Uncertainty",scenarioList$Name)]
#forWetId <- scenarioList$ScenarioId[grep("Palustrine Forested Wetland: Mean",scenarioList$Name)]

forWetId <- forWetId[!(forWetId %in% excludeId)]

myScenarioFW <- scenario(myProject, scenario=max(forWetId))

myDataStockFW <- datasheet(myScenarioFW, "stsim_OutputStock")
myDataFluxFW <- datasheet(myScenarioFW, "stsim_OutputFlow")

# Summarize Stocks

#unique(myDataStockF$StockGroupId)

plotStocks <- c("Biomass: Aboveground",
                "Biomass: Belowground",
                "DOM: Deadwood",
                "DOM: Litter",
                "DOM: Soil",
                "Biomass: Coarse Root [Type]",
                "Biomass: Fine Root [Type]",
                "Biomass: Foliage [Type]",
                "Biomass: Merchantable [Type]",
                "Biomass: Other Wood [Type]",
                "Deep Soil [Type]",
                "DOM: Aboveground Fast [Type]",
                "DOM: Aboveground Medium [Type]",
                "DOM: Aboveground Slow [Type]",
                "DOM: Aboveground Very Fast [Type]",
                "DOM: Belowground Fast [Type]",
                "DOM: Belowground Slow [Type]",
                "DOM: Belowground Very Fast [Type]",
                "DOM: Snag Branch [Type]",
                "DOM: Snag Stem [Type]",
                "Ecosystem Carbon Storage (tons C)")

for (i in 1:length(plotStocks)){
  print(i)
  plotName <- gsub("[Type]","",gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotStocks[i]), fixed = T),fixed = T)),fixed = T)
  
  myDataStockFcarbonStorage <- myDataStockF %>%
    filter(StockGroupId == plotStocks[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Upland Forest",
           Color = "#993C2D")
  
  myDataStockFWcarbonStorage <- myDataStockFW %>%
    filter(StockGroupId == plotStocks[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Palustrine Forested Wetland",
           Color = "#6E89C2")
  
  myDataCarbonStorage <- myDataStockFcarbonStorage %>%
    bind_rows(myDataStockFWcarbonStorage)
  
  table(table(myDataCarbonStorage$Timestep) == 2)
  
  col <- as.character(myDataCarbonStorage$Color)
  names(col) <- as.character(myDataCarbonStorage$Scenario)
  
  myDataCarbonStorage$Timestep <- myDataCarbonStorage$Timestep-2000
  
  p3 <- ggplot(myDataCarbonStorage, aes(x = Timestep, y = mean, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = min, ymax = max, fill = Scenario, group = Scenario), alpha = 0.25, colour = NA) +
    theme_bw() +
    scale_color_manual(values=col) +
    scale_fill_manual(values=col) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nAge") + 
    ylab(paste0(gsub("Deep Soil","Deep Soil Accumulation",gsub("(tons C)","",gsub(" [Type]","",plotStocks[i], fixed = T), fixed = T)),"\n(tons C per ha)\n"))+
    ylim(0,NA)
  
  p3
  
  ggsave(paste0(pathOutForest,"/",plotName,"_","ForestedWetland",".png"), p3, width = 3.3, height = 3.4, dpi = 600)
  
  print(range(myDataCarbonStorage$mean))
  
  rm(myDataStockFcarbonStorage,myDataStockFWcarbonStorage,myDataCarbonStorage,plotName)
  
}

# Summarize Flows

#unique(myDataFluxF$FlowGroupId)

plotFlows<- c("Annual Emissions: CO2 (tons C per year)",             
              "Annual Emissions: CO2 (tons CO2-eq per year)",        
              "Annual Emissions: CO2 and CH4 (tons C per year)",     
              "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
              "Annual Net Flux (tons C per year)",                   
              "Annual Net Flux (tons CO2-eq per year)",              
              "Annual Net Growth (tons C per year)",                
              "Annual Net Growth (tons CO2-eq per year)",            
              "Annual Emissions: CH4 (tons C per year)",             
              "Annual Emissions: CH4 (tons CO2-eq per year)",        
              "Annual Lateral Flux (tons C per year)",               
              "Annual Lateral Flux (tons CO2-eq per year)",
              "Annual Net Ecosystem Carbon Balance (tons C per year)",
              "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)")

for (i in 1:length(plotFlows)){
  
  plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotFlows[i]), fixed = T),fixed = T))
  
  myDataFluxFnecb <- myDataFluxF %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Upland Forest",
           Color = "#993C2D")
  
  myDataFluxFWnecb <- myDataFluxFW %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Palustrine Forested Wetland",
           Color = "#6E89C2")
  
  if (nrow(myDataFluxFnecb) == 0){
    myDataFluxFnecb <- data.frame(mean = 0,
                                  min = 0, 
                                  max = 0,
                                  Scenario = "Upland Forest",
                                  Color = "#993C2D",
                                  Timestep = myDataFluxFWnecb$Timestep)
  }
  
  myDataNECB <- myDataFluxFnecb %>%
    bind_rows(myDataFluxFWnecb)
  
  minVal <- min(myDataNECB$min)
  maxVal <- max(myDataNECB$max)
  
  if(minVal > 0 & maxVal > 0){
    minVal = 0
  } else if (minVal < 0 & maxVal < 0){
    maxVal = 0
  } 
  
  
  table(table(myDataNECB$Timestep) == 2)
  
  col <- as.character(myDataNECB$Color)
  names(col) <- as.character(myDataNECB$Scenario)
  
  myDataNECB$Timestep <- myDataNECB$Timestep-2000
  
  p5 <- ggplot(myDataNECB, aes(x = Timestep, y = mean, color = Scenario, group = Scenario)) +
    geom_hline(yintercept = 0, color = "darkgrey",linewidth = 0.5, linetype = 2) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = min, ymax = max, fill = Scenario, group = Scenario), alpha = 0.25, colour = NA) +
    theme_bw() +
    scale_color_manual(values=col) +
    scale_fill_manual(values=col) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nAge") + 
    ylab(paste0(gsub("Annual Net Ecosystem Carbon Balance","Annual NECB",
                     gsub(")"," per ha)",
                          gsub("(","\n(",plotFlows[i], fixed = T), fixed = T)),"\n"))+
    ylim(minVal,maxVal)
    #ylim(-50,10)
    #ylim(-10,5)
  
  p5
  
  ggsave(paste0(pathOutForest,"/",plotName,"_","ForestedWetland",".png"), p5, width = 3.3, height = 3.4, dpi = 600)
  
  rm(myDataFluxFnecb, myDataFluxFWnecb,myDataNECB,plotName)
  
}

stockGroupIds <- datasheet(myProject, name = "stsim_StockGroup", includeKey = T)
flowGroupIds <- datasheet(myProject, name = "stsim_FlowGroup", includeKey = T)
transitionGroupIds <- datasheet(myProject, name = "stsim_TransitionGroup", includeKey = T)


