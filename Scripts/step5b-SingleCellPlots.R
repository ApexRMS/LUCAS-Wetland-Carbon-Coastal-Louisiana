# ApexRMS
# Updated 2025-02-25
# Run after step5a-SingleCellPlots.R
# This script creates figures for single-cell models without transitions

library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

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

pathOutTransitions <- paste0(pathOut,"TransitionsWetlandWater")

if(!dir.exists(pathOutTransitions)){
  dir.create(pathOutTransitions)
}


funCompareCharts <- function(scenariosToCompare,scenarioNames,plotNamePrefix){
  
  scenarioList <- scenario(myProject, summary = T, results = T)
  
  tEId <- scenarioList$ScenarioId[grep(scenariosToCompare[1],scenarioList$Name)]
  tPId <- scenarioList$ScenarioId[grep(scenariosToCompare[2],scenarioList$Name)]
  
  myScenarioTE <- scenario(myProject, scenario=max(tEId))
  myScenarioTP <- scenario(myProject, scenario=max(tPId))
  
  myDataStockTE <- datasheet(myScenarioTE, "stsim_OutputStock")
  myDataStockTP <- datasheet(myScenarioTP, "stsim_OutputStock")
  
  # print(myDataStockTE$Amount[myDataStockTE$StockGroupId == "DOM: Belowground Slow [Type]" & 
  #                             myDataStockTE$Timestep %in% c(2001,2202)])
  # 
  # print(myDataStockTP$Amount[myDataStockTP$StockGroupId == "DOM: Belowground Slow [Type]" & 
  #                             myDataStockTP$Timestep %in% c(2001,2202)])
  
  print(myDataStockTE$Amount[myDataStockTE$StockGroupId == "DOM: Belowground Slow [Type]" &
                               myDataStockTE$Timestep %in% c(2019,2220)])
  
  print(myDataStockTP$Amount[myDataStockTP$StockGroupId == "DOM: Belowground Slow [Type]" &
                               myDataStockTP$Timestep %in% c(2019,2220)])
  
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
    
    myDataStockEcarbonStorage <- myDataStockTE %>%
      filter(StockGroupId == plotStocksChange[i]) %>%
      group_by(Timestep,Iteration) %>%
      summarize(totalC = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      group_by(Timestep) %>%
      summarize(mean = mean(totalC),
                min = min(totalC),
                max = max(totalC)) %>%
      mutate(Scenario = scenarioNames[1],
             Color = "#993C2D")
    
    myDataStockPcarbonStorage <- myDataStockTP %>%
      filter(StockGroupId == plotStocksChange[i]) %>%
      group_by(Timestep,Iteration) %>%
      summarize(totalC = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      group_by(Timestep) %>%
      summarize(mean = mean(totalC),
                min = min(totalC),
                max = max(totalC)) %>%
      mutate(Scenario = scenarioNames[2],
             Color = "#6E89C2")
    
    myDataCarbonStorage <- myDataStockEcarbonStorage %>%
      bind_rows(myDataStockPcarbonStorage)
    
    table(table(myDataCarbonStorage$Timestep) == 2)
    
    col <- as.character(myDataCarbonStorage$Color)
    names(col) <- as.character(myDataCarbonStorage$Scenario)
    
    p3 <- ggplot(myDataCarbonStorage, aes(x = Timestep, y = mean, color = Scenario, group = Scenario)) +
      geom_line(linewidth = 0.8) +
      #geom_ribbon(aes(ymin = min, ymax = max, fill = Scenario, group = Scenario), alpha = 0.25, colour = NA) +
      theme_bw() +
      scale_color_manual(values=col) +
      #scale_fill_manual(values=col) +
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
      ylab(paste0(gsub("(tons C)","",gsub(" [Type]","",plotStocksChange[i], fixed = T), fixed = T),"\n(tons C per ha)\n"))
    #ylim(0,NA)
    
    p3
    
    ggsave(paste0(pathOutTransitions,"/",plotName,"_",plotNamePrefix,".png"), p3, width = 3.3, height = 3.4, dpi = 600)
    
    rm(myDataStockEcarbonStorage,myDataStockPcarbonStorage,myDataCarbonStorage,plotName)
    
  }
  
}

funCompareCharts(scenariosToCompare = c("Transition: Estuarine Emergent Wetland to Water IPCC",
                                        "Transition: Palustrine Emergent Wetland to Water IPCC"),
                 scenarioNames = c("Estuarine Emergent to Water",
                                   "Palustrine Emergent to Water"),
                 plotNamePrefix = "EmergentWetlandWaterIPCC")

funCompareCharts(scenariosToCompare = c("Transition: Estuarine Emergent Wetland to Water S",
                                        "Transition: Palustrine Emergent Wetland to Water S"),
                 scenarioNames = c("Estuarine Emergent to Water",
                                   "Palustrine Emergent to Water"),
                 plotNamePrefix = "EmergentWetlandWaterSchoolmaster")

funCompareCharts(scenariosToCompare = c("Transition: Estuarine Emergent Wetland to Unvegetated IPCC",
                                        "Transition: Palustrine Emergent Wetland to Unvegetated IPCC"),
                 scenarioNames = c("Estuarine Emergent to Unvegetated",
                                   "Palustrine Emergent to Unvegetated"),
                 plotNamePrefix = "EmergentWetlandUnvegetatedIPCC")

funCompareCharts(scenariosToCompare = c("Transition: Estuarine Emergent Wetland to Unvegetated S",
                                        "Transition: Palustrine Emergent Wetland to Unvegetated S"),
                 scenarioNames = c("Estuarine Emergent to Unvegetated",
                                   "Palustrine Emergent to Unvegetated"),
                 plotNamePrefix = "EmergentWetlandUnvegetatedSchoolmaster")

funCompareCharts(scenariosToCompare = c("Transition: Palustrine Forested Wetland to Water IPCC",
                                        "Transition: Palustrine Forested Wetland to Water S"),
                 scenarioNames = c("IPCC",
                                   "Schoolmaster"),
                 plotNamePrefix = "ForestedWetlandWater")

funCompareCharts(scenariosToCompare = c("Transition: Palustrine Forested Wetland to Unvegetated IPCC",
                                        "Transition: Palustrine Forested Wetland to Unvegetated S"),
                 scenarioNames = c("IPCC",
                                   "Schoolmaster"),
                 plotNamePrefix = "ForestedWetlandUnvegetated")
