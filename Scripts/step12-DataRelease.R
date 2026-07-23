# ApexRMS
# Updated 2025-04-16
# Run after step11-SpatialMaps.R
# This script creates data release files

library(rsyncrosim)
library(tidyverse)
library(terra)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

dataPath <- "Data/"
modelPath <- "Models/"

#modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

scenariosBasin <- c("Baseline",
                    "No Palustrine Forested Wetland",
                    "IPCC")

scenariosSingleCell <- c("Original Oak Gum Cypress Forest",
                         "Palustrine Forested Wetland: Add Uncertainty",
                         "Palustrine Emergent Wetland: Add Uncertainty",
                         "Estuarine Emergent Wetland: Add Uncertainty")

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

pathOutSpatialSub <- paste0(pathOutSpatial,scenariosBasin)

for (i in 1:length(pathOutSpatialSub)){
  if(!dir.exists(pathOutSpatialSub[i])){
    dir.create(pathOutSpatialSub[i])
  }
}

# Extract ID names for Spatial Data

stockGroupIDs <- datasheet(myProject, name = "stsim_StockGroup", includeKey = T)
flowGroupIDs <- datasheet(myProject, name = "stsim_FlowGroup", includeKey = T)

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

lookupName <- data.frame(Name = c("Ecosystem Carbon Storage (tons C)",
                                  "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)",
                                  "Annual Net Ecosystem Carbon Balance (tons C per year)",
                                  "Annual Emissions: CH4 (tons CO2-eq per year)",
                                  "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
                                  "Annual Net Growth (tons CO2-eq per year)",
                                  "Annual Emissions: CO2 (tons CO2-eq per year)",
                                  "Annual Lateral Flux (tons CO2-eq per year)",
                                  "Annual Emissions: CH4 (tons C per year)",
                                  "Annual Emissions: CO2 and CH4 (tons C per year)",
                                  "Annual Net Growth (tons C per year)",
                                  "Annual Emissions: CO2 (tons C per year)",
                                  "Annual Lateral Flux (tons C per year)"),
                         NameShort = c("EcoStorage",
                                       "AnnNECBCO2e",
                                       "AnnNECBMgC",
                                       "AnnCH4CO2e",
                                       "AnnTotEmissCO2e",
                                       "AnnGrowCO2e",
                                       "AnnCO2CO2e",
                                       "AnnLatCO2e",
                                       "AnnCH4MgC",
                                       "AnnTotEmissMgC",
                                       "AnnGrowMgC",
                                       "AnnCO2MgC",
                                       "AnnLatMgC"))

landToChange <- c("Agriculture: Cropland",
                  "Developed: Medium Intensity",
                  "Water: All",
                  "Water: Previously Emergent Wetland",
                  "Water: Previously Forested Wetland",
                  "Wetland: Unvegetated Emergent",
                  "Wetland: Unvegetated Forested",
                  "Grassland: Annual",
                  "Shrubland: Non-sage")

keepFluxesSpatial <- keepFluxes

keepStocksSpatial <- c("Ecosystem Carbon Storage (tons C)")

scenarioListAll <- scenario(myProject, summary = T, results = T)

# Loop through all Basin Scenarios

for (i in 1:length(scenariosBasin)){
  
  scenID <- scenarioListAll$ScenarioId[grep(paste0("Basin ",scenariosBasin[i]),scenarioListAll$Name)]
  
  myScenario <- scenario(myProject, scenario=max(scenID))
  
  if (i == 1){
    
    # Tabular Data
    
    # Load Land Cover Data
    tabLand <- datasheet(myScenario, "stsim_OutputStratumState")
    
    tabLandSub <- tabLand %>%
      select(-c(Iteration,StateLabelXId,StateLabelYId,AgeMin,
                AgeMax,AgeClass,StratumId,SecondaryStratumId,ResolutionId)) %>%
      rename(Year = Timestep) %>%
      mutate(StateClass = case_when(StateClassId == "Agriculture: Cropland"~"Agriculture",
                                    StateClassId == "Developed: Medium Intensity"~"Developed",
                                    StateClassId == "Grassland: Annual"~"Grassland",
                                    StateClassId == "Shrubland: Non-sage"~"Shrubland",
                                    StateClassId == "Water: All"~"Water",
                                    StateClassId == "Water: Previously Emergent Wetland"~"Water",
                                    StateClassId == "Water: Previously Forested Wetland"~"Water",
                                    StateClassId == "Wetland: Unvegetated Emergent"~"Wetland: Unconsolidated Shore",
                                    StateClassId == "Wetland: Unvegetated Forested"~"Wetland: Unconsolidated Shore",
                                    !(StateClassId %in% landToChange)~StateClassId)) %>%
      select(-StateClassId) %>%
      group_by(Year,StateClass) %>%
      summarize(Area_ha = sum(Amount)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosBasin[i])
    
    rm(tabLand)
    
    # Load Stock Data
    tabStock <- datasheet(myScenario, "stsim_OutputStock")
    
    tabStockSub <- tabStock %>%
      select(-c(Iteration,StratumId,SecondaryStratumId,ResolutionId)) %>%
      filter(StockGroupId %in% c(keepStocks1,keepStocks2)) %>%
      rename(Year = Timestep,
             StockGroup = StockGroupId) %>%
      mutate(StateClass = case_when(StateClassId == "Agriculture: Cropland"~"Agriculture",
                                    StateClassId == "Developed: Medium Intensity"~"Developed",
                                    StateClassId == "Grassland: Annual"~"Grassland",
                                    StateClassId == "Shrubland: Non-sage"~"Shrubland",
                                    StateClassId == "Water: All"~"Water",
                                    StateClassId == "Water: Previously Emergent Wetland"~"Water",
                                    StateClassId == "Water: Previously Forested Wetland"~"Water",
                                    StateClassId == "Wetland: Unvegetated Emergent"~"Wetland: Unconsolidated Shore",
                                    StateClassId == "Wetland: Unvegetated Forested"~"Wetland: Unconsolidated Shore",
                                    !(StateClassId %in% landToChange)~StateClassId)) %>%
      select(-StateClassId) %>%
      mutate(StockGroup = gsub(" [Type]","",StockGroup, fixed = T)) %>%
      group_by(Year,StateClass,StockGroup) %>%
      summarize(Amount_MgC = sum(Amount)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosBasin[i])
    
    rm(tabStock)
    
    # Load Flux Data
    tabFlux <- datasheet(myScenario, "stsim_OutputFlow")
    
    # should be true
    table(tabFlux$ToStateClassId == tabFlux$FromStateClassId)
    
    tabFluxSub <- tabFlux %>%
      select(-c(Iteration,FromStockTypeId,TransitionTypeId,
                ToStratumId,ToStateClassId,ToStockTypeId,EndStratumId,
                EndSecondaryStratumId,EndStateClassId,EndMinAge,FromSecondaryStratumId,FromStratumId,
                ResolutionId)) %>%
      filter((FlowGroupId %in% keepFluxes)) %>%
      rename(Year = Timestep,
             FlowGroup = FlowGroupId,
             Amount1 = Amount) %>%
      mutate(StateClass = case_when(FromStateClassId == "Agriculture: Cropland"~"Agriculture",
                                    FromStateClassId == "Developed: Medium Intensity"~"Developed",
                                    FromStateClassId == "Grassland: Annual"~"Grassland",
                                    FromStateClassId == "Shrubland: Non-sage"~"Shrubland",
                                    FromStateClassId == "Water: All"~"Water",
                                    FromStateClassId == "Water: Previously Emergent Wetland"~"Water",
                                    FromStateClassId == "Water: Previously Forested Wetland"~"Water",
                                    FromStateClassId == "Wetland: Unvegetated Emergent"~"Wetland: Unconsolidated Shore",
                                    FromStateClassId == "Wetland: Unvegetated Forested"~"Wetland: Unconsolidated Shore",
                                    !(FromStateClassId %in% landToChange)~FromStateClassId)) %>%
      select(-FromStateClassId) %>%
      group_by(Year,StateClass,FlowGroup) %>%
      summarize(Amount = sum(Amount1)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosBasin[i])
    
    rm(tabFlux)
    
  } else {
    
    # Tabular Data
    
    # Load Land Cover Data
    tabLand <- datasheet(myScenario, "stsim_OutputStratumState")
    
    tabLandSub2 <- tabLand %>%
      select(-c(Iteration,StateLabelXId,StateLabelYId,AgeMin,
                AgeMax,AgeClass,StratumId,SecondaryStratumId,ResolutionId)) %>%
      rename(Year = Timestep) %>%
      mutate(StateClass = case_when(StateClassId == "Agriculture: Cropland"~"Agriculture",
                                    StateClassId == "Developed: Medium Intensity"~"Developed",
                                    StateClassId == "Grassland: Annual"~"Grassland",
                                    StateClassId == "Shrubland: Non-sage"~"Shrubland",
                                    StateClassId == "Water: All"~"Water",
                                    StateClassId == "Water: Previously Emergent Wetland"~"Water",
                                    StateClassId == "Water: Previously Forested Wetland"~"Water",
                                    StateClassId == "Wetland: Unvegetated Emergent"~"Wetland: Unconsolidated Shore",
                                    StateClassId == "Wetland: Unvegetated Forested"~"Wetland: Unconsolidated Shore",
                                    !(StateClassId %in% landToChange)~StateClassId)) %>%
      select(-StateClassId) %>%
      group_by(Year,StateClass) %>%
      summarize(Area_ha = sum(Amount)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosBasin[i])
    
    tabLandSub <- tabLandSub %>%
      addRow(tabLandSub2)
    
    rm(tabLand,tabLandSub2)
    
    # Load Stock Data
    tabStock <- datasheet(myScenario, "stsim_OutputStock")
    
    tabStockSub2 <- tabStock %>%
      select(-c(Iteration,StratumId,SecondaryStratumId,ResolutionId)) %>%
      filter(StockGroupId %in% c(keepStocks1,keepStocks2)) %>%
      rename(Year = Timestep,
             StockGroup = StockGroupId) %>%
      mutate(StateClass = case_when(StateClassId == "Agriculture: Cropland"~"Agriculture",
                                    StateClassId == "Developed: Medium Intensity"~"Developed",
                                    StateClassId == "Grassland: Annual"~"Grassland",
                                    StateClassId == "Shrubland: Non-sage"~"Shrubland",
                                    StateClassId == "Water: All"~"Water",
                                    StateClassId == "Water: Previously Emergent Wetland"~"Water",
                                    StateClassId == "Water: Previously Forested Wetland"~"Water",
                                    StateClassId == "Wetland: Unvegetated Emergent"~"Wetland: Unconsolidated Shore",
                                    StateClassId == "Wetland: Unvegetated Forested"~"Wetland: Unconsolidated Shore",
                                    !(StateClassId %in% landToChange)~StateClassId)) %>%
      select(-StateClassId) %>%
      mutate(StockGroup = gsub(" [Type]","",StockGroup, fixed = T)) %>%
      group_by(Year,StateClass,StockGroup) %>%
      summarize(Amount_MgC = sum(Amount)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosBasin[i])
    
    tabStockSub <- tabStockSub %>%
      addRow(tabStockSub2)
    
    rm(tabStock,tabStockSub2)
    
    # Load Flux Data
    tabFlux <- datasheet(myScenario, "stsim_OutputFlow")
    
    tabFluxSub2 <- tabFlux %>%
      select(-c(Iteration,FromStockTypeId,TransitionTypeId,
                ToStratumId,ToStateClassId,ToStockTypeId,EndStratumId,
                EndSecondaryStratumId,EndStateClassId,EndMinAge,FromSecondaryStratumId,FromStratumId,
                ResolutionId)) %>%
      filter((FlowGroupId %in% keepFluxes)) %>%
      rename(Year = Timestep,
             FlowGroup = FlowGroupId,
             Amount1 = Amount) %>%
      mutate(StateClass = case_when(FromStateClassId == "Agriculture: Cropland"~"Agriculture",
                                    FromStateClassId == "Developed: Medium Intensity"~"Developed",
                                    FromStateClassId == "Grassland: Annual"~"Grassland",
                                    FromStateClassId == "Shrubland: Non-sage"~"Shrubland",
                                    FromStateClassId == "Water: All"~"Water",
                                    FromStateClassId == "Water: Previously Emergent Wetland"~"Water",
                                    FromStateClassId == "Water: Previously Forested Wetland"~"Water",
                                    FromStateClassId == "Wetland: Unvegetated Emergent"~"Wetland: Unconsolidated Shore",
                                    FromStateClassId == "Wetland: Unvegetated Forested"~"Wetland: Unconsolidated Shore",
                                    !(FromStateClassId %in% landToChange)~FromStateClassId)) %>%
      select(-FromStateClassId) %>%
      group_by(Year,StateClass,FlowGroup) %>%
      summarize(Amount = sum(Amount1)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosBasin[i])
    
    tabFluxSub <- tabFluxSub %>%
      addRow(tabFluxSub2)
    
    rm(tabFlux,tabFluxSub2)
    
  }
  
  # Spatial Data

  pathOutSpatialAll <- paste0(pathOutSpatial,scenariosBasin[i])

  # Land Cover Area


  listLandCover <- list.files(paste0(rootPath,"Models/",
                                     modelName,"/",
                                     modelName,".ssim.data/Scenario-",
                                     scenarioId(myScenario),
                                     "/stsim_OutputSpatialState"),
                              pattern = ".tif",
                              full.names = T)

  file.copy(listLandCover,
            pathOutSpatialAll)

  rm(listLandCover)

  # Carbon Stocks

  listStocks <- list.files(paste0(rootPath,"Models/",
                                  modelName,"/",
                                  modelName,".ssim.data/Scenario-",
                                  scenarioId(myScenario),
                                  "/stsim_OutputAverageSpatialStockGroup"),
                           pattern = ".tif",
                           full.names = T)



  for (j in 1:length(keepStocksSpatial)){

    outName <- lookupName$NameShort[lookupName$Name == keepStocksSpatial[j]]

    stockId <- stockGroupIDs %>%
      filter(Name == keepStocksSpatial[j]) %>%
      pull(StockGroupId)

    listStocksSub <- grep(stockId,listStocks, value = T)

    listStocksSubFiles <- gsub(paste0(rootPath,"Models/",
                                      modelName,"/",
                                      modelName,".ssim.data/Scenario-",
                                      scenarioId(myScenario),
                                      "/stsim_OutputAverageSpatialStockGroup/"),"",listStocksSub)

    listStocksSubFiles <- gsub(stockId,outName,listStocksSubFiles)

    file.copy(listStocksSub,
              paste0(pathOutSpatialAll,"/",listStocksSubFiles))

    rm(stockId,listStocksSub,outName,listStocksSubFiles)

  }

  rm(listStocks)

  # Carbon Fluxes

  listFluxes <- list.files(paste0(rootPath,"Models/",
                                  modelName,"/",
                                  modelName,".ssim.data/Scenario-",
                                  scenarioId(myScenario),
                                  "/stsim_OutputAverageSpatialFlowGroup"),
                           pattern = ".tif",
                           full.names = T)

  for (k in 1:length(keepFluxesSpatial)){

    outName <- lookupName$NameShort[lookupName$Name == keepFluxesSpatial[k]]

    fluxId <- flowGroupIDs %>%
      filter(Name == keepFluxesSpatial[k]) %>%
      pull(FlowGroupId)

    listFluxesSub <- grep(fluxId,listFluxes, value = T)

    listFluxesSubFiles <- gsub(paste0(rootPath,"Models/",
                                      modelName,"/",
                                      modelName,".ssim.data/Scenario-",
                                      scenarioId(myScenario),
                                      "/stsim_OutputAverageSpatialFlowGroup/"),"",listFluxesSub)

    listFluxesSubFiles <- gsub(fluxId,outName,listFluxesSubFiles)

    file.copy(listFluxesSub,
              paste0(pathOutSpatialAll,"/",listFluxesSubFiles))

    rm(fluxId,listFluxesSub,outName,listFluxesSubFiles)

  }

  rm(listFluxes)
  
}


write.csv(tabLandSub,paste0(pathOutTabular,"LandCoverArea_Basin.csv"),
          row.names = F)

tabStockSubIPCC <- tabStockSub %>%
  filter(StockGroup %in% c(keepStocks1)) %>% 
  mutate(StockGroup = str_replace(StockGroup,"\\(tons","\\(Mg"))

tabStockSubLUCAS <- tabStockSub %>%
  filter(StockGroup %in% c(keepStocks2NoType)) %>% 
  mutate(StockGroup = str_replace(StockGroup,"\\(tons","\\(Mg"))

write.csv(tabStockSubIPCC,paste0(pathOutTabular,"CarbonStocksIPCC_Basin.csv"),
          row.names = F)

write.csv(tabStockSubLUCAS,paste0(pathOutTabular,"CarbonStocksLUCAS_Basin.csv"),
          row.names = F)

tabFluxSub <- tabFluxSub %>% 
  mutate(FlowGroup = str_replace(FlowGroup,"\\(tons","\\(Mg"))

write.csv(tabFluxSub,paste0(pathOutTabular,"CarbonFluxes_Basin.csv"),
          row.names = F)

rm(tabFluxSub,tabStockSubLUCAS,tabStockSubIPCC,tabLandSub)

stockGroupIDs %>% 
  filter(Name %in% keepStocksSpatial) %>%
  select(-Description)

flowGroupIDs %>%
  filter(Name %in% keepFluxesSpatial) %>%
  select(-Description)

# Extent, Project, Spatial Resolution

scenID <- max(scenarioListAll$ScenarioId[grep(paste0("Basin ",scenariosBasin[1]),scenarioListAll$Name)])

r1 <- rast(paste0(rootPath,"Models/",
                  modelName,"/",
                  modelName,".ssim.data/Scenario-",
                  scenID,
                  "/stsim_OutputSpatialState/sc.it1.ts2001.tif"))

r1


# Loop through single cell scenarios

for (i in 1:length(scenariosSingleCell)){
  
  print(i)
  
  scenID <- scenarioListAll$ScenarioId[grep(scenariosSingleCell[i],scenarioListAll$Name)]
  
  myScenario <- scenario(myProject, scenario=max(scenID))
  
  if (i == 1){
    
    # Load Stock Data
    tabStock <- datasheet(myScenario, "stsim_OutputStock")
    
    tabStockSub <- tabStock %>%
      select(-c(StratumId,SecondaryStratumId,Iteration,ResolutionId)) %>%
      filter(StockGroupId %in% c(keepStocks1,keepStocks2)) %>%
      rename(Year = Timestep,
             StateClass = StateClassId,
             StockGroup = StockGroupId) %>%
      mutate(StockGroup = gsub(" [Type]","",StockGroup, fixed = T)) %>%
      group_by(Year,StateClass,StockGroup) %>%
      summarize(Mean_MgC = mean(Amount, na.rm = T),
                Low = quantile(Amount,0.025, na.rm = T),
                High = quantile(Amount,0.975, na.rm = T)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosSingleCell[i])
    
    rm(tabStock)
    gc()
    
    # Load Flux Data
    tabFlux <- datasheet(myScenario, "stsim_OutputFlow")
    
    tabFluxSub <- tabFlux %>%
      select(-c(FromStockTypeId,TransitionTypeId,
                ToStratumId,ToStateClassId,ToStockTypeId,EndStratumId,
                EndSecondaryStratumId,EndStateClassId,EndMinAge,
                FromSecondaryStratumId,FromStratumId,
                ResolutionId)) %>%
      filter((FlowGroupId %in% keepFluxes)) %>%
      rename(Year = Timestep,
             FlowGroup = FlowGroupId,
             StateClass = FromStateClassId) %>%
      group_by(Year,StateClass,FlowGroup,Iteration) %>%
      summarize(totalC = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      group_by(Year,StateClass,FlowGroup) %>%
      summarize(Mean = mean(totalC, na.rm = T),
                Low = quantile(totalC,0.025, na.rm = T),
                High = quantile(totalC,0.975, na.rm = T)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosSingleCell[i])
    
    rm(tabFlux)
    gc()
    
  } else {
    
    # Tabular Data
    
    # Load Stock Data
    tabStock <- datasheet(myScenario, "stsim_OutputStock")
    
    tabStockSub2 <- tabStock %>%
      select(-c(StratumId,SecondaryStratumId,Iteration,ResolutionId)) %>%
      filter(StockGroupId %in% c(keepStocks1,keepStocks2)) %>%
      rename(Year = Timestep,
             StateClass = StateClassId,
             StockGroup = StockGroupId) %>%
      mutate(StockGroup = gsub(" [Type]","",StockGroup, fixed = T)) %>%
      group_by(Year,StateClass,StockGroup) %>%
      summarize(Mean_MgC = mean(Amount, na.rm = T),
                Low = quantile(Amount,0.025, na.rm = T),
                High = quantile(Amount,0.975, na.rm = T)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosSingleCell[i])
    
    tabStockSub <- tabStockSub %>%
      addRow(tabStockSub2)
    
    rm(tabStock,tabStockSub2)
    gc()
    
    # Load Flux Data
    tabFlux <- datasheet(myScenario, "stsim_OutputFlow")
    
    tabFluxSub2 <- tabFlux %>%
      select(-c(FromStockTypeId,TransitionTypeId,
                ToStratumId,ToStateClassId,ToStockTypeId,EndStratumId,
                EndSecondaryStratumId,EndStateClassId,EndMinAge,
                FromSecondaryStratumId,FromStratumId,
                ResolutionId)) %>%
      filter((FlowGroupId %in% keepFluxes)) %>%
      rename(Year = Timestep,
             FlowGroup = FlowGroupId,
             StateClass = FromStateClassId) %>%
      group_by(Year,StateClass,FlowGroup,Iteration) %>%
      summarize(totalC = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      group_by(Year,StateClass,FlowGroup) %>%
      summarize(Mean = mean(totalC, na.rm = T),
                Low = quantile(totalC,0.025, na.rm = T),
                High = quantile(totalC,0.975, na.rm = T)) %>%
      ungroup() %>%
      mutate(Scenario = scenariosSingleCell[i])
    
    tabFluxSub <- tabFluxSub %>%
      addRow(tabFluxSub2)
    
    rm(tabFlux,tabFluxSub2)
    gc()
    
  }
  
  rm(scenID,myScenario)
  
}

tabStockSub$Low[tabStockSub$Scenario == "Original Oak Gum Cypress Forest"] <- NA
tabStockSub$High[tabStockSub$Scenario == "Original Oak Gum Cypress Forest"] <- NA

tabStockSubIPCC <- tabStockSub %>%
  filter(StockGroup %in% c(keepStocks1)) %>% 
  mutate(StockGroup = str_replace(StockGroup,"\\(tons","\\(Mg")) %>%
  select(-Scenario)

tabStockSubLUCAS <- tabStockSub %>%
  filter(StockGroup %in% c(keepStocks2NoType)) %>% 
  mutate(StockGroup = str_replace(StockGroup,"\\(tons","\\(Mg")) %>%
  select(-Scenario)

write.csv(tabStockSubIPCC,paste0(pathOutTabular,"CarbonStocksIPCC_SingleCell.csv"),
          row.names = F)

write.csv(tabStockSubLUCAS,paste0(pathOutTabular,"CarbonStocksLUCAS_SingleCell.csv"),
          row.names = F)

tabFluxSub$Low[tabFluxSub$Scenario == "Original Oak Gum Cypress Forest"] <- NA
tabFluxSub$High[tabFluxSub$Scenario == "Original Oak Gum Cypress Forest"] <- NA

tabFluxSub <- tabFluxSub %>% 
  mutate(FlowGroup = str_replace(FlowGroup,"\\(tons","\\(Mg")) %>%
  select(-Scenario)

write.csv(tabFluxSub,paste0(pathOutTabular,"CarbonFluxes_SingleCell.csv"),
          row.names = F)

