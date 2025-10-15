# ApexRMS
# Updated 2025-02-25
# Run after step2b-ScenarioCreationWetland.R
# This script adds uncertainty scenarios for emergent wetland model

library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

rootPath <- "E:/gitprojects/A329-LucasBarataria/"

pathInDatasheets <- paste0(rootPath,"Data/Datasheets Wetland/")
rootPathUpdatedTables <- paste0(rootPath,"Data/Datasheets Wetland/Emergent/")

numberOfJobs <- 3

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

# Update definitions

distFlowMultLat <- read_csv(paste0(rootPathUpdatedTables,"stsim_DistributionValue/stsim_DistributionValue Flow Multipliers Wetland Emergent Site Lat IPCC.csv"))
names(distFlowMultLat) <- gsub("ID","Id",names(distFlowMultLat))

distNPPLat <- read_csv(paste0(rootPathUpdatedTables,"stsim_DistributionValue/stsim_DistributionValue NPP Wetland Emergent Site Lat.csv"))
names(distNPPLat) <- gsub("ID","Id",names(distNPPLat))

distInitialCLat <- read_csv(paste0(rootPathUpdatedTables,"stsim_DistributionValue/stsim_DistributionValue Initial C Wetland Emergent Site Lat.csv"))
names(distInitialCLat) <- gsub("ID","Id",names(distInitialCLat))

DistributionTypeIds <- unique(c(distFlowMultLat$DistributionTypeId,
                                distNPPLat$DistributionTypeId,
                                distInitialCLat$DistributionTypeId,
                                c("Wetland: Estuarine Emergent Emission: Atmosphere Temp -> Atmosphere: CH4",
                                  "Wetland: Palustrine Emergent Emission: Atmosphere Temp -> Atmosphere: CH4")))

ExternalVariableTypeIds <- unique(c(distFlowMultLat$ExternalVariableTypeId,
                                    distNPPLat$ExternalVariableTypeId,
                                    distInitialCLat$ExternalVariableTypeId))

sheetName <- "core_DistributionType"

myData <- datasheet(myProject,sheetName, empty = TRUE, optional = TRUE) %>%
  addRow(data.frame(Name = DistributionTypeIds,
                    Description = "Emergent Wetland"))

saveDatasheet(myProject, myData, sheetName, append = TRUE)

rm(sheetName,myData)

sheetName <- "core_ExternalVariableType"
myData <- datasheet(myProject,sheetName, empty = TRUE, optional = TRUE) %>%
  addRow(data.frame(Name = ExternalVariableTypeIds,
                    Description = "Emergent Wetland"))

saveDatasheet(myProject, myData, sheetName, append = TRUE)

rm(sheetName,myData)

# Initial conditions site lat level

myScenario <- scenario(myProject, 
                       scenario="Init C Stocks at Equilibrium Emergent Wetland [Site]",
                       folder = "Single-Cell Sub-Scenarios")

myUpdate <- read_csv(paste0(rootPathUpdatedTables,"stsim_StateAttributeValue/stsim_StateAttributeValue Initial C Wetland Emergent Site.csv"))
names(myUpdate) <- gsub("ID","Id",names(myUpdate))

myUpdate <- myUpdate %>%
  mutate(DistributionFrequencyId = "Iteration Only")

myOrig <- read_csv(paste0(rootPath, dataPath, "State Attributes/", "State Attributes - Initial Stocks Emergent Wetland.csv"))
names(myOrig) <- gsub("ID","Id",names(myOrig))

wetlandTab <- myOrig %>%
  filter(!(StateAttributeTypeId %in% unique(myUpdate$StateAttributeTypeId)))

# Review, should all be zero, all stocks not computed in script
table(wetlandTab$Value)

myData <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(wetlandTab) %>%
  addRow(myUpdate)

saveDatasheet(myScenario, myData, "stsim_StateAttributeValue", append = FALSE)

rm(myData,myScenario,myUpdate,myOrig,wetlandTab)

# NPP site level

myScenario <- scenario(myProject, 
                       scenario="STSM State Attributes [Site Net Growth, Add Wetland]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "STSM State Attributes [Net Growth]")

sheetName <- "stsim_StateAttributeValue"

myUpdate <- read_csv(paste0(rootPathUpdatedTables,"stsim_StateAttributeValue/stsim_StateAttributeValue NPP Wetland Emergent Site.csv"))
names(myUpdate) <- gsub("ID","Id",names(myUpdate))

myUpdate <- myUpdate %>%
  mutate(DistributionFrequencyId = "Iteration Only")

myData <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T) %>%
  filter(!(StateClassId %in% c(myUpdate$StateClassId))) %>%
  addRow(myUpdate)

myDataPalustrineForested <- myData %>%
  filter(StateClassId == "Forest: Oak/Gum/Cypress Group") %>%
  mutate(StateClassId = "Wetland: Palustrine Forested") %>%
  select(where(~!all(is.na(.x))))

myDataEstuarineForested <- myData %>%
  filter(StateClassId == "Forest: Oak/Gum/Cypress Group") %>%
  mutate(StateClassId = "Wetland: Estuarine Forested") %>%
  select(where(~!all(is.na(.x))))

myData <- myData %>%
  select(where(~!all(is.na(.x)))) %>%
  filter(!(StateClassId %in% c("Wetland: Estuarine Forested",
                               "Wetland: Palustrine Forested"))) %>%
  addRow(myDataPalustrineForested) %>%
  addRow(myDataEstuarineForested)

saveDatasheet(myScenario, myData, "stsim_StateAttributeValue", append = FALSE)

rm(myData, myScenario, myUpdate, myDataPalustrineForested, myDataEstuarineForested)


# SF Flow Multipliers

myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Emergent Wetland, Site]",
                       folder = "Single-Cell Sub-Scenarios")

myUpdate <- read_csv(paste0(rootPathUpdatedTables,"stsimsf_FlowMultiplier/stsimsf_FlowMultiplier Wetland Emergent Site.csv"))
names(myUpdate) <- gsub("ID","Id",names(myUpdate))

myUpdate <- myUpdate %>%
  mutate(FlowGroupId = paste0(FlowGroupId, " [Type]")) %>%
  mutate(DistributionFrequencyId = "Iteration Only")

myUpdate$FlowGroupId[myUpdate$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- "Emission Emergent: BG Slow -> Atmosphere Temp [Type]"
myUpdate$FlowGroupId[myUpdate$FlowGroupId == "Lateral Transport: BG Slow -> Aquatic [Type]"] <- "Lateral Transport Emergent: BG Slow -> Aquatic [Type]"
myUpdate$FlowGroupId[myUpdate$FlowGroupId == "Stabilization: BG Slow -> Deep Soil [Type]"] <- "Stabilization Emergent: BG Slow -> Deep Soil [Type]"

stockFlowPath <- paste0(rootPath, dataPath, "Stock Flow/")
myData <- read.csv(paste0(stockFlowPath, "Flow Multipliers - Wetland.csv"))
names(myData) <- gsub("ID","Id",names(myData))

FlowTypesOld <- c("Emission: AG Very Fast -> Atmosphere [Type]",
                  "Emission: BG Slow -> Atmosphere [Type]",
                  "Emission: BG Very Fast -> Atmosphere [Type]",
                  "Lateral Transport: BG Slow -> Aquatic [Type]",
                  "Stabilization: BG Slow -> Deep Soil [Type]")

emergentWetland <- myData %>%
  filter(StateClassId %in% c("Wetland: Estuarine Emergent",
                             "Wetland: Palustrine Emergent")) %>%
  filter(!(FlowGroupId %in% unique(myUpdate$FlowGroupId))) %>%
  filter(!(FlowGroupId %in% c("AG Very Fast ->",
                              "BG Slow ->",
                              "BG Very Fast ->"))) %>%
  filter(!(FlowGroupId %in% c(FlowTypesOld)))

## Review, these should all be zero
table(emergentWetland$Value)

# For the stocks that don't exist, add in flow multipliers for Ag, which are from an upland forest, need to check on this
flowGroupsMissingValues <- unique(emergentWetland$FlowGroupId)
flowGroupsMissingValues <- flowGroupsMissingValues[!(flowGroupsMissingValues %in% c("Biomass Turnover: Fine Roots -> AG Very Fast [Type]",
                                                                                    "Decay: AG Very Fast -> AG Slow [Type]"))]

flowMultipliersAg <- read.csv(paste0(pathInDatasheets,"FlowMultipliersLAModel.csv"), stringsAsFactors = F)
names(flowMultipliersAg) <- gsub("ID","Id",names(flowMultipliersAg))

flowMultipliersAg <- flowMultipliersAg %>%
  filter(FlowGroupId %in% flowGroupsMissingValues,
         StateClassId == "Agriculture:All") %>%
  mutate(StateClassId = "Wetland: Palustrine Emergent")

flowMultipliersAg2 <- flowMultipliersAg %>%
  mutate(StateClassId = "Wetland: Estuarine Emergent")

emergentWetland <- emergentWetland %>%
  filter(!(FlowGroupId %in% flowMultipliersAg$FlowGroupId))

myZeros <- tibble(StateClassId = rep(c("Wetland: Estuarine Emergent",
                                       "Wetland: Palustrine Emergent"), 5),
                  FlowGroupId = rep(c(FlowTypesOld), each = 2),
                  Value = 0)

emergentWetland <- emergentWetland %>%
  bind_rows(myZeros,
           flowMultipliersAg,
           flowMultipliersAg2) %>%
  select(where(~!all(is.na(.x))))

myData <- datasheet(myScenario,"stsim_FlowMultiplier", optional = T, empty = T) %>%
  addRow(myUpdate) %>%
  addRow(emergentWetland)

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

rm(myScenario,myData,emergentWetland, myUpdate, flowMultipliersAg, flowMultipliersAg2)

# Update Methane

myScenario <- scenario(myProject, 
                       scenario="SAV: Methane: Forested and Emergent [Saline]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "SAV: Methane: Forested and Emergent")

myUpdate <- datasheet(myScenario, "stsim_StateAttributeValue")

myUpdateEmergent <- myUpdate %>%
  filter(StateClassId %in% c("Wetland: Estuarine Emergent",
                             "Wetland: Palustrine Emergent")) %>%
  mutate(Value = NA,
         DistributionType = paste0(StateClassId," Emission: Atmosphere Temp -> Atmosphere: CH4"),
         DistributionFrequencyId = "Iteration Only")

myUpdateSub <- myUpdate %>%
  filter(!StateClassId %in% c("Wetland: Estuarine Emergent",
                              "Wetland: Palustrine Emergent"))

saveDatasheet(myScenario, myUpdateEmergent, "stsim_StateAttributeValue", append = FALSE)
saveDatasheet(myScenario, myUpdateSub, "stsim_StateAttributeValue", append = TRUE)

rm(myUpdate,myScenario,myUpdateSub,myUpdateEmergent)

# Add distributions stsm, site and lat level

myScenario <- scenario(myProject, 
                       scenario="Distributions [NPP, Lat, Site]",
                       folder = "Single-Cell Sub-Scenarios")

distNPPLatTab <- distNPPLat %>%
  select(-c(Site.Id)) %>%
  mutate(Value = Value/100)

myData <- datasheet(myScenario, "stsim_DistributionValue", optional = T, empty = T) %>%
  addRow(distNPPLatTab)

saveDatasheet(myScenario, myData, "stsim_DistributionValue", append = FALSE)

rm(myData,myScenario)

myScenario <- scenario(myProject, 
                       scenario="Distributions [Initial C, Lat, Site]",
                       folder = "Single-Cell Sub-Scenarios")

distInitialCLatTab <- distInitialCLat %>%
  mutate(Value = Value/100)

myData <- datasheet(myScenario, "stsim_DistributionValue", optional = T, empty = T) %>%
  addRow(distInitialCLatTab)

saveDatasheet(myScenario, myData, "stsim_DistributionValue", append = FALSE)

rm(myData,myScenario)


myScenario <- scenario(myProject, 
                       scenario="Distributions [Flow Multipliers, Lat, Site]",
                       folder = "Single-Cell Sub-Scenarios")

myData <- datasheet(myScenario, "stsim_DistributionValue", optional = T, empty = T) %>%
  addRow(distFlowMultLat)

saveDatasheet(myScenario, myData, "stsim_DistributionValue", append = FALSE)

rm(myData,myScenario)

# Add distributions for Methane

myScenario <- scenario(myProject, 
                       scenario="Distributions [CH4, Lat, Site]",
                       folder = "Single-Cell Sub-Scenarios")

myUpdate <- read_csv(paste0(rootPathUpdatedTables,"stsim_DistributionValue/stsim_DistributionValue CH4 Wetland Emergent Site Lat.csv"))
names(myUpdate) <- gsub("ID","Id",names(myUpdate))

myUpdate <- myUpdate %>%
  mutate(Value = Value/100) %>%
  select(-Site.Id) %>%
  mutate(DistributionTypeId = gsub("Methane Emissions",
                                   "Emission: Atmosphere Temp -> Atmosphere: CH4",
                                   DistributionTypeId))

saveDatasheet(myScenario, myUpdate, "stsim_DistributionValue", append = FALSE)

rm(myUpdate,myScenario)

myScenario <- scenario(myProject,
                       scenario="Distributions [Site, Lat]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("Distributions [CH4, Lat, Site]",
                            "Distributions [Flow Multipliers, Lat, Site]",
                            "Distributions [Initial C, Lat, Site]",
                            "Distributions [NPP, Lat, Site]")

rm(myScenario)

# Add External Variable

myScenario <- scenario(myProject,
                       scenario="External Variable: Site ID Lat Flux Estimate",
                       folder = "Single-Cell Sub-Scenarios")

myData <- datasheet(myScenario, "core_ExternalVariableValue", optional = T, empty = T)

myUpdate <- data.frame(ExternalVariableTypeId = c("Site ID Wetland: Palustrine Emergent",
                                                  "Site ID Wetland: Estuarine Emergent"),
                       DistributionTypeId = "Uniform Integer",
                       DistributionFrequency = "Iteration Only",
                       DistributionMin = c(1,1),
                       DistributionMax = c((3*6),(9*18)))

myData <- myData %>%
  addRow(myUpdate)

saveDatasheet(myScenario, myData, "core_ExternalVariableValue", append = FALSE)

rm(myScenario,myData,myUpdate)

# Add External Variable

set.seed(598)

myScenario <- scenario(myProject,
                       scenario="External Variable: Site ID Lat Flux Estimate Set Seed",
                       folder = "Single-Cell Sub-Scenarios")

myData <- datasheet(myScenario, "core_ExternalVariableValue", optional = T, empty = T)

myUpdate <- data.frame(Iteration = c(1:1000),
                       ExternalVariableTypeId = "Site ID Wetland: Palustrine Emergent",
                       ExternalVariableValue = sample(c(1:18),1000,replace = TRUE)) %>%
  addRow(data.frame(Iteration = c(1:1000),
                    ExternalVariableTypeId = "Site ID Wetland: Estuarine Emergent",
                    ExternalVariableValue = sample(c(1:162),1000,replace = TRUE)))

saveDatasheet(myScenario, myUpdate, "core_ExternalVariableValue", append = FALSE)

rm(myScenario,myData,myUpdate)

