# Created by Amanda Schwantes, ApexRMS
# Updated 2025-04-16
# Run after step11-SpatialMaps.R
# This script creates data release files

library(rsyncrosim)
library(tidyverse)
library(terra)
library(viridis)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

rootPath <- "E:/gitprojects/A329-LucasBarataria/"

outpathDatasheets <- paste0(rootpath,"Data/Datasheets Wetland/")

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

pathOut <- paste0(rootPath,"Models/",modelName,"/Output Data Release/")


# Create File Structure

if(!dir.exists(pathOut)){
  dir.create(pathOut)
}

pathOutSpatial <- paste0(pathOut,"Spatial Data/")
if(!dir.exists(pathOutSpatial)){
  dir.create(pathOutSpatial)
}

pathOutTabular <- paste0(pathOut,"Tabular Data/")
if(!dir.exists(pathOutTabular)){
  dir.create(pathOutTabular)
}

pathOutSpatialSub <- paste0(pathOutSpatial,c("Land Cover Area",
                                             "Carbon Stocks",
                                             "Carbon Fluxes"))

for (i in 1:length(pathOutSpatialSub)){
  if(!dir.exists(pathOutSpatialSub[i])){
    dir.create(pathOutSpatialSub[i])
  }
}


# Extract ID names for Spatial Data

stockGroupIDs <- datasheet(myProject, name = "stsim_StockGroup", includeKey = T)
flowGroupIDs <- datasheet(myProject, name = "stsim_FlowGroup", includeKey = T)
transitionGroupIDs <- datasheet(myProject, name = "stsim_TransitionGroup", includeKey = T)

# Loop through all Scenarios

scenList <- c("1 No Land Cover Change and Climate",
              "2 Land Cover Change and Climate",
              "3 Land Cover Change, Climate, Erosion",
              "4 Land Cover Change, Climate, and No Forested Wetland")

scenNames <- c("NoLandCoverChange",
               "LandCoverChangeSchoolmaster",
               "LandCoverChangeIPCC",
               "LandCoverChangeNoForestedWetland")

for (i in 1:length(scenList)){
  
  scenID <- scenarioListAll$ScenarioId[grep(scenList[i],scenarioListAll$Name)]
  
  myScenario <- scenario(myProject, scenario=max(scenID))
  
  # Tabular Data
  
  # Load Land Cover Data
  tabLand <- datasheet(myScenario, "stsim_OutputStratumState")
  
  tabLandSub <- tabLand %>%
    select(-c(Iteration,StateLabelXId,StateLabelYId,AgeMin,AgeMax,AgeClass,StratumId,SecondaryStratumId)) %>%
    rename(Year = Timestep,
           StateClass = StateClassId) %>% 
    group_by(Year,StateClass) %>%
    summarize(Area_ha = sum(Amount)) %>%
    ungroup()
  
  write.csv(tabLandSub,paste0(pathOutTabular,"LandCoverArea_",
                              scenNames[i],".csv"),
            row.names = F)
  
  rm(tabLand,tabLandSub)
  
  # Load Stock Data
  tabStock <- datasheet(myScenario, "stsim_OutputStock")
  
  keepStocks1 <- c("Biomass: Aboveground",
                   "Biomass: Belowground",
                   "DOM: Deadwood",
                   "DOM: Litter",
                   "DOM: Soil")
  
  keepStocks2 <- c("Biomass: Coarse Root [Type]",
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
  
  keepStocks2NoType <- gsub(" [Type]","",keepStocks2, fixed = T)
  
  tabStockSub <- tabStock %>%
    select(-c(Iteration,StratumId,SecondaryStratumId)) %>%
    filter(StockGroupId %in% c(keepStocks1,keepStocks2)) %>%
    rename(Year = Timestep,
           StateClass = StateClassId,
           StockGroup = StockGroupId) %>%
    mutate(StockGroup = gsub(" [Type]","",StockGroup, fixed = T)) %>%
    group_by(Year,StateClass,StockGroup) %>%
    summarize(Amount_tonsC = sum(Amount)) %>%
    ungroup()
  
  tabStockSubIPCC <- tabStockSub %>%
    filter(StockGroup %in% c(keepStocks1))
  
  tabStockSubLUCAS <- tabStockSub %>%
    filter(StockGroup %in% c(keepStocks2NoType))
  
  write.csv(tabStockSubIPCC,paste0(pathOutTabular,"CarbonStocksIPCC_",
                                   scenNames[i],".csv"),
            row.names = F)
  
  write.csv(tabStockSubLUCAS,paste0(pathOutTabular,"CarbonStocksLUCAS_",
                                    scenNames[i],".csv"),
            row.names = F)
  
  rm(tabStock,tabStockSub,tabStockSubIPCC,tabStockSubLUCAS)
  
  # Load Flux Data
  tabFlux <- datasheet(myScenario, "stsim_OutputFlow")
  
  keepFluxes <- c("Annual Emissions: CO2 (tons C per year)",             
                  "Annual Emissions: CO2 (tons CO2-eq per year)",        
                  "Annual Emissions: CO2 and CH4 (tons C per year)",     
                  "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
                  "Annual Net Growth (tons C per year)",                
                  "Annual Net Growth (tons CO2-eq per year)",            
                  "Annual Emissions: CH4 (tons C per year)",             
                  "Annual Emissions: CH4 (tons CO2-eq per year)",        
                  "Annual Lateral Flux (tons C per year)",               
                  "Annual Lateral Flux (tons CO2-eq per year)",
                  "Annual Net Ecosystem Carbon Balance (tons C per year)",
                  "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)")
  
  tabFluxSub <- tabFlux %>%
    select(-c(Iteration,FromStateClassId,FromStockTypeId,TransitionTypeId,
              ToStratumId,ToStateClassId,ToStockTypeId,EndStratumId,
              EndSecondaryStratumId,EndStateClassId,EndMinAge,FromSecondaryStratumId,FromStratumId)) %>%
    filter((FlowGroupId %in% keepFluxes)) %>%
    rename(Year = Timestep,
           FlowGroup = FlowGroupId,
           Amount1 = Amount) %>%
    group_by(Year,FlowGroup) %>%
    summarize(Amount = sum(Amount1)) %>%
    ungroup()
  
  
  write.csv(tabFluxSub,paste0(pathOutTabular,"CarbonFluxes_",
                              scenNames[i],".csv"),
            row.names = F)
  
  rm(tabFlux,tabFluxSub)
  
  # Load Transition Data
  tabTransition <- datasheet(myScenario, "stsim_OutputStratumTransition")
  
  # keepTransitions <- c("Restoration Wetland: Palustrine Forested [Type]",
  #                      "Urbanization",
  #                      "Intensification",
  #                      "Forest Harvest",
  #                      "Ag Expansion",
  #                      "Ag Contraction")
  
  tabTransitionSub <- tabTransition %>%
    select(-c(Iteration,AgeMin,AgeMax,AgeClass,SizeClassId,EventId,SecondaryStratumId,StratumId)) %>%
    #filter(TransitionGroupId %in% keepTransitions) %>%
    rename(Year = Timestep,
           TransitionGroup = TransitionGroupId) %>%
    mutate(TransitionGroup = gsub(" [Type]","",TransitionGroup, fixed = T)) %>%
    group_by(Year,TransitionGroup) %>%
    summarize(Area_ha = sum(Amount)) %>%
    ungroup()
  
  write.csv(tabTransitionSub,paste0(pathOutTabular,"LandCoverTransitions_",
                                    scenNames[i],".csv"),
            row.names = F)
  
  rm(tabTransition,tabTransitionSub)
  
  
  # Spatial Data
  
  # Land Cover Area
  
  listLandCover <- list.files(paste0(rootPath,"/Models/",
                                     modelName,"/",
                                     modelName,".ssim.data/Scenario-",
                                     scenarioId(myScenario),
                                     "/stsim_OutputSpatialState"),
                              pattern = ".tif",
                              full.names = T)
  
  pathOutLandCover <- paste0(pathOutSpatial,"Land Cover Area/",scenList[i])
  
  if(!dir.exists(pathOutLandCover)){
    dir.create(pathOutLandCover)
  }
  
  file.copy(listLandCover,
            pathOutLandCover)
  
  # Carbon Stocks
  
  listStocks <- list.files(paste0(rootPath,"/Models/",
                                  modelName,"/",
                                  modelName,".ssim.data/Scenario-",
                                  scenarioId(myScenario),
                                  "/stsim_OutputAverageSpatialStockGroup"),
                           pattern = ".tif",
                           full.names = T)
  
  keepStocksSpatial <- c("Biomass: Aboveground",
                         "Biomass: Belowground",
                         "DOM: Deadwood",
                         "DOM: Litter",
                         "DOM: Soil",
                         "Ecosystem Carbon Storage (tons C)")
  
  for (j in 1:length(keepStocksSpatial)){
    
    stockId <- stockGroupIDs %>%
      filter(Name == keepStocksSpatial[j]) %>%
      pull(StockGroupId)
    
    listStocksSub <- grep(stockId,listStocks, value = T)
    
    stockName <- gsub(")","",gsub("(","",gsub(": "," ",keepStocksSpatial[j]), fixed = T),fixed = T)
    
    pathOutStocks1 <- paste0(pathOutSpatial,"Carbon Stocks/",
                             stockName,"/")
    pathOutStocks2 <- paste0(pathOutStocks1,scenList[i])
    
    if(!dir.exists(pathOutStocks1)){
      dir.create(pathOutStocks1)
    }
    
    if(!dir.exists(pathOutStocks2)){
      dir.create(pathOutStocks2)
    }
    
    file.copy(listStocksSub,
              pathOutStocks2)
    
    rm(stockId,listStocksSub,stockName,pathOutStocks1,pathOutStocks2)
    
  }
  
  # Carbon Fluxes
  
  listFluxes <- list.files(paste0(rootPath,"/Models/",
                                  modelName,"/",
                                  modelName,".ssim.data/Scenario-",
                                  scenarioId(myScenario),
                                  "/stsim_OutputAverageSpatialFlowGroup"),
                           pattern = ".tif",
                           full.names = T)
  
  keepFluxesSpatial <- c("Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)",
                         "Annual Emissions: CH4 (tons CO2-eq per year)",
                         "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
                         "Annual Net Growth (tons CO2-eq per year)",
                         "Annual Emissions: CO2 (tons CO2-eq per year)",
                         "Annual Lateral Flux (tons CO2-eq per year)")
  
  for (k in 1:length(keepFluxesSpatial)){
    
    fluxId <- flowGroupIDs %>%
      filter(Name == keepFluxesSpatial[k]) %>%
      pull(FlowGroupId)
    
    listFluxesSub <- grep(fluxId,listFluxes, value = T)
    
    fluxName <- gsub(")","",gsub("(","",keepFluxesSpatial[k], fixed = T),fixed = T)
    fluxName <- gsub(": "," ",fluxName)
    fluxName <- gsub("-"," ",fluxName)
    
    pathOutFluxes1 <- paste0(pathOutSpatial,"Carbon Fluxes/",
                             fluxName,"/")
    pathOutFluxes2 <- paste0(pathOutFluxes1,scenList[i])
    
    if(!dir.exists(pathOutFluxes1)){
      dir.create(pathOutFluxes1)
    }
    
    if(!dir.exists(pathOutFluxes2)){
      dir.create(pathOutFluxes2)
    }
    
    file.copy(listFluxesSub,
              pathOutFluxes2)
    
    rm(fluxId,listFluxesSub,fluxName,pathOutFluxes1,pathOutFluxes2)
    
  }
  
  
  rm(scenID,myScenario)
  
  rm(listLandCover,pathOutLandCover,
     listHarvest,harvestId,pathOutHarvest,
     listStocks,listFluxes)
  
}

stockGroupIDs %>% 
  filter(Name %in% keepStocksSpatial) %>%
  select(-Description)

flowGroupIDs %>%
  filter(Name %in% keepFluxesSpatial) %>%
  select(-Description)

# Extent, Project, Spatial Resolution

scenID <- scenarioListAll$ScenarioId[grep(scenList[1],scenarioListAll$Name)]

r1 <- rast(paste0(rootPath,"/Models/",
                  modelName,"/",
                  modelName,".ssim.data/Scenario-",
                  scenID,
                  "/stsim_OutputSpatialState/sc.it1.ts2001.tif"))

r1
