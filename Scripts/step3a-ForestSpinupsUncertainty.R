# ApexRMS
# Updated 2025-02-25
# Run after step2d-ForestSpinups.R
# This script adds the lateral flux multipliers for the mean model and uncertainty models


library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

outpathDatasheets <- paste0(rootPath,"Data/Datasheets Wetland/Output/")

dataPath <- "Data/"
modelPath <- "Models/"

#modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

latForest <- c("Lateral Transport: AG Very Fast -> Aquatic",
               "Lateral Transport: BG Slow -> Aquatic",
               "Lateral Transport: BG Very Fast -> Aquatic",
               "Lateral Transport: AG Fast -> Aquatic",
               "Lateral Transport: AG Medium -> Aquatic",
               "Lateral Transport: BG Fast -> Aquatic")

# Calculate Lateral Flux average
latW <- 4.36 - 1.700 - (0.151-0.116)
latS <- 2.67 - 1.796 - (0.822-0.633)
latMean <- mean(c(latW,latS))

# Update flow multipliers with the new lat flux partions

myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "SF Flow Multipliers [Forested Wetland BGS Slower]")

myScenarioOld <- scenario(myProject, 
                          scenario="SF Flow Multipliers [Forested Wetland BGS Slower]")

myData <- datasheet(myScenarioOld, "stsim_FlowMultiplier")

# Only run, once you have the myDataLatPartition table
myDataLatPartition <- read.csv(paste0(outpathDatasheets,"LatPartition.csv"), stringsAsFactors = F)

myDataKeep <- myData %>%
  filter(!(FlowGroupId %in% c(paste0(latForest, " [Type]"),unique(as.character(myDataLatPartition$FlowGroupId)))))

myDataLatPartition2 <- myDataLatPartition %>%
  mutate(FlowGroupId = case_when(FlowGroupId == "Emission: AG Fast -> Atmosphere Temp [Type]"~ "Lateral Transport: AG Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: AG Medium -> Atmosphere Temp [Type]" ~ "Lateral Transport: AG Medium -> Aquatic [Type]",
                                 FlowGroupId == "Emission: AG Very Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: AG Very Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Slow -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Very Fast -> Aquatic [Type]"))

myDataUpdateLat <- myData %>%
  filter(FlowGroupId %in% paste0(latForest, " [Type]")) %>%
  left_join(myDataLatPartition2, by = join_by(FlowGroupId)) %>%
  mutate(Value = Prop_MaxStock) %>%
  select(-Prop_MaxStock)

myDataUpdateEmission <- myData %>%
  filter(FlowGroupId %in% c(unique(as.character(myDataLatPartition$FlowGroupId)))) %>%
  left_join(myDataLatPartition, by = join_by(FlowGroupId)) %>%
  mutate(Value = Value - Prop_MaxStock) %>%
  select(-Prop_MaxStock)

myDataNew <- myDataKeep %>%
  addRow(myDataUpdateLat) %>%
  addRow(myDataUpdateEmission)

write.csv(myDataNew,
          paste0(outpathDatasheets,"stsim_FlowMultiplier_ForestedWetlandUpdate.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_FlowMultiplier_ForestedWetlandUpdate.csv"))

myData <- datasheet(myScenario, "stsim_FlowMultiplier", optional = TRUE, empty = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

# Add Uncertainty
# Grab output data: Wetland: Palustrine Forested Spinup
scenNamesForestedWetland <- c("Updated Wetland: Palustrine Forested Spinup: Limit: Harvest S",
                              "Updated Wetland: Palustrine Forested Spinup: Limit: Harvest W")

latSites <- c(latS,latW)

for (i in 1:length(scenNamesForestedWetland)){
  
  scenarioList <- scenario(myProject, summary = T, results = T)
  
  forestId <- scenarioList$ScenarioId[grep(scenNamesForestedWetland[i],scenarioList$Name)]
  
  myScenario <- scenario(myProject, scenario=max(forestId))
  
  siteLetter <- substring(scenNamesForestedWetland[i],nchar(scenNamesForestedWetland[i]),nchar(scenNamesForestedWetland[i]))
  
  myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)
  
  # Has equilibrium been reached, difference is less than 1%, (difference in peaks, year prior to disturbance)
  peaks <- seq(125,3500,125)-1
  
  testE <- myData %>%
    filter(Timestep %in% peaks) %>%
    filter(StockGroupId == "DOM: Belowground Slow [Type]") %>%
    group_by(Timestep,StratumId,StateClassId,StockGroupId) %>%
    summarize(carbonMean = mean(Amount, na.rm = T)) %>%
    ungroup() %>%
    arrange(Timestep) %>%
    mutate(percentDiff = ((carbonMean - lag(carbonMean))/((carbonMean + lag(carbonMean))/2)) * 100)
  
  print(as.data.frame(testE))
  
  myDataFlux <- datasheet(myScenario, "stsim_OutputFlow", optional = T)
  
  myDataTotalDOM <- myData %>%
    filter(Timestep == 3565 &
             StateClassId == "Wetland: Palustrine Forested" &
             StockGroupId %in% c("DOM: Aboveground Very Fast [Type]",
                                 "DOM: Belowground Fast [Type]",
                                 "DOM: Aboveground Fast [Type]",
                                 "DOM: Belowground Very Fast [Type]",
                                 "DOM: Aboveground Medium [Type]",
                                 "DOM: Belowground Slow [Type]")) %>%
    rename(CarbonStock = Amount) %>%
    select(StockGroupId,CarbonStock)
  
  myDataLatFlows <- myDataFlux %>%
    filter(Timestep == 3565 &
             FromStateClassId == "Wetland: Palustrine Forested" &
             FlowGroupId %in% c("Transfer: Snag Stem -> AG Medium [Type]",
                                "Biomass Turnover: Coarse Roots -> BG Fast [Type]",
                                "Transfer: Snag Branch -> AG Fast [Type]",
                                "Biomass Turnover: Coarse Roots -> AG Fast [Type]",
                                "Biomass Turnover: Other Wood -> AG Fast [Type]",
                                "Biomass Turnover: Fine Roots -> BG Very Fast [Type]",
                                "Biomass Turnover: Fine Roots -> AG Very Fast [Type]",
                                "Biomass Turnover: Foliage -> AG Very Fast [Type]",
                                "Decay: Snag Stem -> BG Slow [Type]",
                                "Decay: Snag Branch -> BG Slow [Type]",
                                "Decay: AG Medium -> BG Slow [Type]")) %>%
    mutate(StockGroupId = case_when(FlowGroupId == "Transfer: Snag Stem -> AG Medium [Type]" ~ "DOM: Aboveground Medium [Type]",
                                    FlowGroupId == "Biomass Turnover: Coarse Roots -> BG Fast [Type]" ~ "DOM: Belowground Fast [Type]",
                                    FlowGroupId == "Transfer: Snag Branch -> AG Fast [Type]" ~ "DOM: Aboveground Fast [Type]",
                                    FlowGroupId == "Biomass Turnover: Coarse Roots -> AG Fast [Type]" ~ "DOM: Aboveground Fast [Type]",
                                    FlowGroupId == "Biomass Turnover: Other Wood -> AG Fast [Type]" ~ "DOM: Aboveground Fast [Type]",
                                    FlowGroupId == "Biomass Turnover: Fine Roots -> BG Very Fast [Type]" ~ "DOM: Belowground Very Fast [Type]",
                                    FlowGroupId == "Biomass Turnover: Fine Roots -> AG Very Fast [Type]" ~ "DOM: Aboveground Very Fast [Type]",
                                    FlowGroupId == "Biomass Turnover: Foliage -> AG Very Fast [Type]" ~ "DOM: Aboveground Very Fast [Type]",
                                    FlowGroupId == "Decay: Snag Stem -> BG Slow [Type]" ~ "DOM: Belowground Slow [Type]",
                                    FlowGroupId == "Decay: Snag Branch -> BG Slow [Type]" ~ "DOM: Belowground Slow [Type]",
                                    FlowGroupId == "Decay: AG Medium -> BG Slow [Type]" ~ "DOM: Belowground Slow [Type]")) %>%
    group_by(StockGroupId) %>%
    summarize(FlowIn = sum(Amount))
  
  # myDataLatFlows <- myDataLatFlows %>%
  #   addRow(tibble(StockGroupId = "DOM: Belowground Slow [Type]",
  #                 FlowIn = 0))
  
  myDataLatPartition <- myDataFlux %>%
    filter(Timestep == 3565 &
             FromStateClassId == "Wetland: Palustrine Forested" &
             FlowGroupId %in% c("Emission: AG Very Fast -> Atmosphere Temp [Type]",
                                "Emission: BG Fast -> Atmosphere Temp [Type]",
                                "Emission: AG Fast -> Atmosphere Temp [Type]",
                                "Emission: BG Very Fast -> Atmosphere Temp [Type]",
                                "Emission: AG Medium -> Atmosphere Temp [Type]",
                                "Emission: BG Slow -> Atmosphere Temp [Type]")) %>%
    mutate(Sum = sum(Amount),
           Prop = (Amount/Sum)*latSites[i]) %>%
    mutate(StockGroupId = case_when(FlowGroupId == "Emission: AG Very Fast -> Atmosphere Temp [Type]" ~ "DOM: Aboveground Very Fast [Type]",
                                    FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]" ~ "DOM: Belowground Fast [Type]",
                                    FlowGroupId == "Emission: AG Fast -> Atmosphere Temp [Type]" ~ "DOM: Aboveground Fast [Type]",
                                    FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]" ~ "DOM: Belowground Very Fast [Type]",
                                    FlowGroupId == "Emission: AG Medium -> Atmosphere Temp [Type]" ~ "DOM: Aboveground Medium [Type]",
                                    FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]" ~ "DOM: Belowground Slow [Type]"))
  
  myDataLatPartition <- merge(myDataLatPartition, myDataTotalDOM, by = "StockGroupId")
  myDataLatPartition <- merge(myDataLatPartition, myDataLatFlows, by = "StockGroupId")
  myDataLatPartition <- myDataLatPartition %>%
    mutate(Prop_MaxStock = Prop/(CarbonStock + FlowIn)) %>%
    select(FlowGroupId,Prop_MaxStock)
  
  write.csv(myDataLatPartition,paste0(outpathDatasheets,"LatPartition",siteLetter,".csv"), row.names = F)
  
  myData <- myData[myData$Timestep >= 3500,]
  myData$AgeMin <- myData$Timestep - 3499
  myData$AgeMax <- myData$Timestep - 3499
  
  myData$AgeMax[myData$AgeMax == 300] <- NA
  
  # Duplicate year 0 and year 1
  myDataZero <- myData %>%
    filter(AgeMin == 1) %>%
    mutate(AgeMin = 0,
           AgeMax = 0)
  
  myData <- myData %>%
    add_row(myDataZero)
  
  range(myData$AgeMax, na.rm = TRUE)
  range(myData$AgeMin)
  
  names(myData)
  
  tail(myData)
  
  lookup <- read_csv(paste0(rootPath,dataPath,"Additional Spinups/Initial Stock - Non Spatial.csv"))
  names(lookup) <- gsub("ID","Id",names(lookup))
  
  lookup <- rbind(lookup,c("Deep Soil","Carbon Initial Conditions: Deep Soil"))
  
  lookup <- lookup %>%
    mutate(StockGroupId = paste0(StockTypeId, " [Type]")) %>%
    select(-StockTypeId)
  
  myData2 <- myData %>%
    mutate(StockGroupId = as.character(StockGroupId)) %>%
    filter(StockGroupId %in% lookup$StockGroupId) %>%
    left_join(lookup, by = join_by(StockGroupId == StockGroupId))
  
  unique(myData2[,c("StateAttributeTypeId","StockGroupId")])
  length(unique(myData2$StateAttributeTypeId))
  
  myData3 <- myData2 %>%
    group_by(StateClassId,StateAttributeTypeId,AgeMin,AgeMax) %>%
    summarize(Value = mean(Amount, na.rm = T))
  
  # Set deep soil pool back to zero, only tracking 0-100 cm soil in the initial timestep
  myData3$Value[myData3$StateAttributeTypeId == "Carbon Initial Conditions: Deep Soil"] <- 0
  
  rm(myScenario)
  
  
  myScenario <- scenario(myProject, 
                         scenario=paste0("Init C Stocks at Equilibrium [Wetland: Palustrine Forested Updated ",siteLetter," ]"),
                         folder = "Single-Cell Sub-Scenarios")
  
  write.csv(myData3,
            paste0(outpathDatasheets,"stsim_StateAttributeValue_ForestedWetland_InitialC_2001_",siteLetter,".csv"), row.names = FALSE)
  
  # Add to model
  myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeValue_ForestedWetland_InitialC_2001_",siteLetter,".csv"))
  
  myCSV2 <- myCSV %>%
    mutate(StateClassId = "Wetland: Estuarine Forested")
  
  myData4 <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
    addRow(myCSV) %>%
    addRow(myCSV2)
  
  saveDatasheet(myScenario, myData4, "stsim_StateAttributeValue", append = FALSE)
  
  tail(myData4)
  mean(myData$Amount[myData$AgeMin == 300 & myData$StockGroupId == "DOM: Snag Stem [Type]"])
  
  rm(myData4,myScenario,myData3,myData2,myData,forestId,scenarioList, myCSV, myCSV2)
  
}


# NECB

# 3.67
# tons C to tons CO2e = 44.009/12.011
# tons C to tons CO2e = 44/12

# 33.5
#tons C to tons CH4 to tons CO2e = (16.04/12.01)*25

# 12.011
# 16.04
# 44.009