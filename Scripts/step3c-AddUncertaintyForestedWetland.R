# ApexRMS
# Updated 2025-02-25
# Run after step3b-AgSpinups.R
# This script adds uncertainty scenarios for the forested wetland model

library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

outpathDatasheets <- paste0(rootPath,"Data/Datasheets Wetland/Output/")
rootPathUpdatedTables <- paste0(rootPath,"Data/Datasheets Wetland/Emergent/")

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

myCSVS <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeValue_ForestedWetland_InitialC_2001_S.csv"))

myCSVS <- myCSVS %>%
  mutate(DistributionTypeId = paste0("Wetland: Palustrine Forested ", StateAttributeTypeId, " ",AgeMin))

DistributionTypeIds1 <- unique(myCSVS$DistributionTypeId)

myScenarioW <- scenario(myProject, 
                        scenario="SF Flow Multipliers [Forested Wetland BGS Slower W]")
myDataW <- datasheet(myScenarioW,"stsim_FlowMultiplier") %>%
  filter(!is.na(AgeMin)) %>%
  mutate(DistributionTypeId = paste0("Wetland: Palustrine Forested ",
                                     gsub(" [Type]","",FlowGroupId, fixed = T), 
                                     " ",AgeMin))

DistributionTypeIds2 <- unique(myDataW$DistributionTypeId)

DistributionTypeIds <- c("Wetland: Palustrine Forested Decay: BG Fast -> BG Slow",
                         "Wetland: Palustrine Forested Decay: BG Very Fast -> BG Slow",
                         "Wetland: Palustrine Forested Emission: AG Fast -> Atmosphere Temp",
                         "Wetland: Palustrine Forested Emission: AG Medium -> Atmosphere Temp",
                         "Wetland: Palustrine Forested Emission: AG Very Fast -> Atmosphere Temp",
                         "Wetland: Palustrine Forested Emission: Atmosphere Temp -> Atmosphere: CH4",
                         "Wetland: Palustrine Forested Emission: BG Fast -> Atmosphere Temp",
                         "Wetland: Palustrine Forested Emission: BG Slow -> Atmosphere Temp",
                         "Wetland: Palustrine Forested Emission: BG Very Fast -> Atmosphere Temp",
                         "Wetland: Palustrine Forested Lateral Transport: AG Fast -> Aquatic",
                         "Wetland: Palustrine Forested Lateral Transport: AG Medium -> Aquatic",
                         "Wetland: Palustrine Forested Lateral Transport: AG Very Fast -> Aquatic",
                         "Wetland: Palustrine Forested Lateral Transport: BG Fast -> Aquatic",
                         "Wetland: Palustrine Forested Lateral Transport: BG Slow -> Aquatic",
                         "Wetland: Palustrine Forested Lateral Transport: BG Very Fast -> Aquatic",
                         "Wetland: Palustrine Forested Stabilization: BG Slow -> Deep Soil")
  
ExternalVariableTypeIds <- c("Site ID Wetland: Palustrine Forested")

sheetName <- "core_DistributionType"

myData <- datasheet(myProject,sheetName, empty = TRUE, optional = TRUE) %>%
  addRow(data.frame(Name = c(DistributionTypeIds,DistributionTypeIds1,DistributionTypeIds2),
                    Description = "Forested Wetland"))

saveDatasheet(myProject, myData, sheetName, append = TRUE)

rm(sheetName,myData)

sheetName <- "core_ExternalVariableType"
myData <- datasheet(myProject,sheetName, empty = TRUE, optional = TRUE) %>%
  addRow(data.frame(Name = ExternalVariableTypeIds,
                    Description = "Forested Wetland"))

saveDatasheet(myProject, myData, sheetName, append = TRUE)

rm(sheetName,myData)


# Assign BGS pool to "Wetland: Palustrine Forested Carbon Initial Conditions: Belowground Slow"

myScenario <- scenario(myProject, 
                       scenario="Init C Stocks at Equilibrium Forested Wetland [Site]",
                       folder = "Single-Cell Sub-Scenarios")

myCSVS <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeValue_ForestedWetland_InitialC_2001_S.csv"))
myCSVW <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeValue_ForestedWetland_InitialC_2001_W.csv"))

myCSVS <- myCSVS %>%
  rename(valueS = Value)

myCSVall <- myCSVW %>%
  rename(valueW = Value) %>%
  left_join(myCSVS,by = join_by(StateClassId,StateAttributeTypeId,AgeMin,AgeMax)) %>%
  mutate(diffVal = valueW-valueS,
         Value = (valueW+valueS)/2,
         DistributionType = paste0("Wetland: Palustrine Forested ",StateAttributeTypeId, " ", AgeMin))

unique(myCSVall$StateAttributeTypeId[myCSVall$diffVal < -0.001])
unique(myCSVall$StateAttributeTypeId[myCSVall$diffVal > 0.001])

myUpdateBGS <- myCSVall %>%
  mutate(Value = NA,
         DistributionFrequencyId = "Iteration Only") %>%
  select(-valueW,-valueS,-diffVal) %>%
  distinct()

myUpdateW <- myCSVall %>%
  select(valueW,DistributionType) %>%
  mutate(ExternalVariableTypeId = "Site ID Wetland: Palustrine Forested",
         ExternalVariableMin = 1,
         ExternalVariableMax = 1,
         ValueDistributionRelativeFrequency = 1) %>%
  rename(Value = valueW,
         DistributionTypeId = DistributionType)

myUpdateS <- myCSVall %>%
  select(valueS,DistributionType) %>%
  mutate(ExternalVariableTypeId = "Site ID Wetland: Palustrine Forested",
         ExternalVariableMin = 2,
         ExternalVariableMax = 2,
         ValueDistributionRelativeFrequency = 1) %>%
  rename(Value = valueS,
         DistributionTypeId = DistributionType)

distBGS <- myUpdateW %>%
  addRow(myUpdateS)

saveDatasheet(myScenario, myUpdateBGS, "stsim_StateAttributeValue", append = FALSE)

myDataE <- datasheet(myScenario,"stsim_StateAttributeValue") %>%
  mutate(StateClassId = "Wetland: Estuarine Forested")

saveDatasheet(myScenario, myDataE, "stsim_StateAttributeValue", append = TRUE)

rm(myScenario,myUpdateBGS,myCSVS,myCSVW,myCSVall,myUpdateW,myUpdateS)


# Update Methane "Wetland: Palustrine Forested Emission: Atmosphere Temp -> Atmosphere: CH4"

myScenario <- scenario(myProject, 
                       scenario="SAV: Methane: Forested and Emergent [Site]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "SAV: Methane: Forested and Emergent [Saline]")

myUpdate <- datasheet(myScenario, "stsim_StateAttributeValue")

myUpdateForest <- myUpdate %>%
  filter(StateClassId %in% c("Wetland: Palustrine Forested",
                            "Wetland: Estuarine Forested")) %>%
  mutate(Value = NA,
         DistributionType = "Wetland: Palustrine Forested Emission: Atmosphere Temp -> Atmosphere: CH4",
         DistributionFrequencyId = "Iteration Only")

myUpdateSub <- myUpdate %>%
  filter(!StateClassId %in% c("Wetland: Palustrine Forested",
                             "Wetland: Estuarine Forested"))

saveDatasheet(myScenario, myUpdateForest, "stsim_StateAttributeValue", append = FALSE)
saveDatasheet(myScenario, myUpdateSub, "stsim_StateAttributeValue", append = TRUE)

rm(myUpdate,myScenario,myUpdateSub,myUpdateForest)


# Update flow multipliers

flowsChange <- c("Decay: BG Fast -> BG Slow",
                 "Decay: BG Very Fast -> BG Slow",
                 "Emission: AG Fast -> Atmosphere Temp",
                 "Emission: AG Medium -> Atmosphere Temp",
                 "Emission: AG Very Fast -> Atmosphere Temp",
                 "Emission: BG Fast -> Atmosphere Temp",
                 "Emission: BG Slow -> Atmosphere Temp",
                 "Emission: BG Very Fast -> Atmosphere Temp",
                 "Lateral Transport: AG Fast -> Aquatic",
                 "Lateral Transport: AG Medium -> Aquatic",
                 "Lateral Transport: AG Very Fast -> Aquatic",
                 "Lateral Transport: BG Fast -> Aquatic",
                 "Lateral Transport: BG Slow -> Aquatic",
                 "Lateral Transport: BG Very Fast -> Aquatic",
                 "Stabilization: BG Slow -> Deep Soil")

flowsChangeGrowth <- c("Net Growth Forest: Atmosphere -> Coarse Roots",
                       "Net Growth Forest: Atmosphere -> Fine Roots",
                       "Net Growth Forest: Atmosphere -> Foliage",
                       "Net Growth Forest: Atmosphere -> Merchantable",
                       "Net Growth Forest: Atmosphere -> Other Wood")

myScenarioW <- scenario(myProject, 
                        scenario="SF Flow Multipliers [Forested Wetland BGS Slower W]")
myDataW <- datasheet(myScenarioW,"stsim_FlowMultiplier") %>%
  rename(ValueW = Value)

myScenarioS <- scenario(myProject, 
                        scenario="SF Flow Multipliers [Forested Wetland BGS Slower S]")
myDataS <- datasheet(myScenarioS,"stsim_FlowMultiplier") %>%
  rename(ValueS = Value)

myDataAll <- myDataW %>%
  left_join(myDataS, by = join_by(StateClassId,AgeMin,AgeMax,FlowGroupId)) %>%
  mutate(diffVal = ValueW-ValueS,
         Value = (ValueW+ValueS)/2)

unique(myDataAll$FlowGroupId[myDataAll$diffVal < -0.00000001])
unique(myDataAll$FlowGroupId[myDataAll$diffVal > 0.00000001])

myUpdateFlows <- myDataAll %>%
  filter(FlowGroupId %in% paste0(flowsChange," [Type]"))

myUpdateFlowsAdd <- myUpdateFlows %>%
  mutate(Value = NA,
         DistributionType = paste0("Wetland: Palustrine Forested ",gsub(" [Type]","",FlowGroupId, fixed = T)),
         DistributionFrequencyId = "Iteration Only",
         AgeMin = NA,
         AgeMax = NA) %>%
  select(-ValueW,-ValueS,-diffVal) %>%
  distinct()

myUpdateFlows2 <- myDataAll %>%
  filter(FlowGroupId %in% paste0(flowsChangeGrowth," [Type]"))

myUpdateFlowsAdd2 <- myUpdateFlows2 %>%
  mutate(Value = NA,
         DistributionType = paste0("Wetland: Palustrine Forested ",
                                   gsub(" [Type]","",FlowGroupId, fixed = T),
                                   " ", AgeMin),
         DistributionFrequencyId = "Iteration Only") %>%
  select(-ValueW,-ValueS,-diffVal) %>%
  distinct()

myUpdateDistFlowsW <- myUpdateFlows %>%
  select(-Value,-diffVal,-ValueS) %>%
  mutate(DistributionTypeId = paste0("Wetland: Palustrine Forested ",gsub(" [Type]","",FlowGroupId, fixed = T)),
         ExternalVariableTypeId = "Site ID Wetland: Palustrine Forested",
         ExternalVariableMin = 1,
         ExternalVariableMax = 1) %>%
  rename(Value = ValueW) %>%
  mutate(ValueDistributionRelativeFrequency = 1)

myUpdateDistFlowsS <- myUpdateFlows %>%
  select(-Value,-diffVal,-ValueW) %>%
  mutate(DistributionTypeId = paste0("Wetland: Palustrine Forested ",gsub(" [Type]","",FlowGroupId, fixed = T)),
         ExternalVariableTypeId = "Site ID Wetland: Palustrine Forested",
         ExternalVariableMin = 2,
         ExternalVariableMax = 2) %>%
  rename(Value = ValueS) %>%
  mutate(ValueDistributionRelativeFrequency = 1)

myUpdateDistFlowsW2 <- myUpdateFlows2 %>%
  select(-Value,-diffVal,-ValueS) %>%
  mutate(DistributionTypeId = paste0("Wetland: Palustrine Forested ",
                                     gsub(" [Type]","",FlowGroupId, fixed = T),
                                     " ",AgeMin),
         ExternalVariableTypeId = "Site ID Wetland: Palustrine Forested",
         ExternalVariableMin = 1,
         ExternalVariableMax = 1) %>%
  rename(Value = ValueW) %>%
  mutate(ValueDistributionRelativeFrequency = 1)

myUpdateDistFlowsS2 <- myUpdateFlows2 %>%
  select(-Value,-diffVal,-ValueW) %>%
  mutate(DistributionTypeId = paste0("Wetland: Palustrine Forested ",
                                     gsub(" [Type]","",FlowGroupId, fixed = T),
                                     " ",AgeMin),
         ExternalVariableTypeId = "Site ID Wetland: Palustrine Forested",
         ExternalVariableMin = 2,
         ExternalVariableMax = 2) %>%
  rename(Value = ValueS) %>%
  mutate(ValueDistributionRelativeFrequency = 1)


myUpdateSub <- myDataAll %>%
  filter(!(FlowGroupId %in% paste0(c(flowsChange,flowsChangeGrowth)," [Type]"))) %>%
  select(-ValueW,-ValueS,-diffVal)

myScenario <- scenario(myProject, 
                       scenario="SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat; Site]",
                       folder = "Single-Cell Sub-Scenarios")

saveDatasheet(myScenario, myUpdateSub, "stsim_FlowMultiplier", append = FALSE)
saveDatasheet(myScenario, myUpdateFlowsAdd, "stsim_FlowMultiplier", append = TRUE)
saveDatasheet(myScenario, myUpdateFlowsAdd2, "stsim_FlowMultiplier", append = TRUE)

# Add distributions BGS

myScenario <- scenario(myProject, 
                       scenario="Distributions [Forested Wetland; Initial C]",
                       folder = "Single-Cell Sub-Scenarios")

head(distBGS)

saveDatasheet(myScenario, distBGS, "stsim_DistributionValue", append = FALSE)

rm(distBGS,myScenario)


# Add distributions for Methane

myScenario <- scenario(myProject, 
                       scenario="Distributions [Forested Wetland; CH4]",
                       folder = "Single-Cell Sub-Scenarios")

# divide by 100 g/m2/yr to tons/ha/yr

distCH4 <- data.frame(DistributionTypeId = "Wetland: Palustrine Forested Emission: Atmosphere Temp -> Atmosphere: CH4",
                     ExternalVariableTypeId = "Site ID Wetland: Palustrine Forested",
                     ExternalVariableMin = c(1:2),
                     ExternalVariableMax = c(1:2),
                     Value = c(1.047,0.008),
                     ValueDistributionRelativeFrequency = 1)

saveDatasheet(myScenario, distCH4, "stsim_DistributionValue", append = FALSE)

rm(distCH4,myScenario)

# add distributions for flow multipliers

myScenario <- scenario(myProject, 
                       scenario="Distributions [Forested Wetland; Flow Multipliers]",
                       folder = "Single-Cell Sub-Scenarios")

latForest <- c("Lateral Transport: AG Very Fast -> Aquatic",
               "Lateral Transport: BG Slow -> Aquatic",
               "Lateral Transport: BG Very Fast -> Aquatic",
               "Lateral Transport: AG Fast -> Aquatic",
               "Lateral Transport: AG Medium -> Aquatic",
               "Lateral Transport: BG Fast -> Aquatic")

myDataLatPartitionW <- read.csv(paste0(outpathDatasheets,"LatPartitionW.csv"),
                                  stringsAsFactors = F)
  
myDataKeepW <- myUpdateDistFlowsW %>%
  filter(!(FlowGroupId %in% c(paste0(latForest, " [Type]"),unique(as.character(myDataLatPartitionW$FlowGroupId))))) %>%
  mutate(ExternalVariableMin = 1,
         ExternalVariableMax = 1)
  
myDataLatPartitionW2 <- myDataLatPartitionW %>%
  mutate(FlowGroupId = case_when(FlowGroupId == "Emission: AG Fast -> Atmosphere Temp [Type]"~ "Lateral Transport: AG Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: AG Medium -> Atmosphere Temp [Type]" ~ "Lateral Transport: AG Medium -> Aquatic [Type]",
                                 FlowGroupId == "Emission: AG Very Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: AG Very Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Slow -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Very Fast -> Aquatic [Type]"))
  
myDataUpdateLatW <- myUpdateDistFlowsW %>%
  filter(FlowGroupId %in% paste0(latForest, " [Type]")) %>%
  left_join(myDataLatPartitionW2, by = join_by(FlowGroupId)) %>%
  mutate(Value = Prop_MaxStock,
         ExternalVariableMin = 1,
         ExternalVariableMax = 1) %>%
    select(-Prop_MaxStock)
  
myDataUpdateEmissionW <- myUpdateDistFlowsW %>%
  filter(FlowGroupId %in% c(unique(as.character(myDataLatPartitionW$FlowGroupId)))) %>%
  left_join(myDataLatPartitionW, by = join_by(FlowGroupId)) %>%
  mutate(Value = Value - Prop_MaxStock,
           ExternalVariableMin = 1,
           ExternalVariableMax = 1) %>%
  select(-Prop_MaxStock)
  
myDataNewW <- myDataKeepW %>%
  addRow(myDataUpdateLatW) %>%
  addRow(myDataUpdateEmissionW) %>%
  select(-StateClassId,-AgeMin,-AgeMax,-FlowGroupId)

myDataNewW <- myDataNewW[!(duplicated(myDataNewW)),]
  
saveDatasheet(myScenario, myDataNewW, "stsim_DistributionValue", append = FALSE)
  
myDataLatPartitionS <- read.csv(paste0(outpathDatasheets,"LatPartitionS.csv"),
                                  stringsAsFactors = F)
  
myDataKeepS <- myUpdateDistFlowsS %>%
  filter(!(FlowGroupId %in% c(paste0(latForest, " [Type]"),unique(as.character(myDataLatPartitionS$FlowGroupId))))) %>%
  mutate(ExternalVariableMin = 2,
         ExternalVariableMax = 2)
  
myDataLatPartitionS2 <- myDataLatPartitionS %>%
  mutate(FlowGroupId = case_when(FlowGroupId == "Emission: AG Fast -> Atmosphere Temp [Type]"~ "Lateral Transport: AG Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: AG Medium -> Atmosphere Temp [Type]" ~ "Lateral Transport: AG Medium -> Aquatic [Type]",
                                 FlowGroupId == "Emission: AG Very Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: AG Very Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Fast -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Slow -> Aquatic [Type]",
                                 FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]" ~ "Lateral Transport: BG Very Fast -> Aquatic [Type]"))
  
myDataUpdateLatS <- myUpdateDistFlowsS %>%
  filter(FlowGroupId %in% paste0(latForest, " [Type]")) %>%
  left_join(myDataLatPartitionS2, by = join_by(FlowGroupId)) %>%
  mutate(Value = Prop_MaxStock,
         ExternalVariableMin = 2,
         ExternalVariableMax = 2) %>%
  select(-Prop_MaxStock)
  
myDataUpdateEmissionS <- myUpdateDistFlowsS %>%
  filter(FlowGroupId %in% c(unique(as.character(myDataLatPartitionS$FlowGroupId)))) %>%
  left_join(myDataLatPartitionS, by = join_by(FlowGroupId)) %>%
  mutate(Value = Value - Prop_MaxStock,
           ExternalVariableMin = 2,
           ExternalVariableMax = 2) %>%
    select(-Prop_MaxStock)
  
myDataNewS <- myDataKeepS %>%
  addRow(myDataUpdateLatS) %>%
  addRow(myDataUpdateEmissionS) %>%
  select(-StateClassId,-AgeMin,-AgeMax,-FlowGroupId)

myDataNewS <- myDataNewS[!(duplicated(myDataNewS)),]
  
saveDatasheet(myScenario, myDataNewS, "stsim_DistributionValue", append = TRUE)

myUpdateDistFlowsW2 <- myUpdateDistFlowsW2 %>%
  select(-StateClassId,-AgeMin,-AgeMax,-FlowGroupId)

myUpdateDistFlowsW2 <- myUpdateDistFlowsW2[!(duplicated(myUpdateDistFlowsW2)),]

myUpdateDistFlowsS2 <- myUpdateDistFlowsS2 %>%
  select(-StateClassId,-AgeMin,-AgeMax,-FlowGroupId)

myUpdateDistFlowsS2 <- myUpdateDistFlowsS2[!(duplicated(myUpdateDistFlowsS2)),]

saveDatasheet(myScenario, myUpdateDistFlowsW2, "stsim_DistributionValue", append = TRUE)
saveDatasheet(myScenario, myUpdateDistFlowsS2, "stsim_DistributionValue", append = TRUE)



rm(myScenario)

# Add External Variable and set seed

set.seed(598)

myScenario <- scenario(myProject,
                       scenario="External Variable: Site ID Lat Flux Estimate Set Seed Add Forest",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "External Variable: Site ID Lat Flux Estimate Set Seed")

myDataExisting <- datasheet(myScenario, "core_ExternalVariableValue")

myUpdate <- data.frame(Iteration = c(1:1000),
                       ExternalVariableTypeId = "Site ID Wetland: Palustrine Forested",
                       ExternalVariableValue = sample(c(1:2),1000,replace = TRUE)) %>%
  anti_join(myDataExisting)

saveDatasheet(myScenario, myUpdate, "core_ExternalVariableValue", append = TRUE)

rm(myScenario,myUpdate,myDataExisting)

# Distributions merge all

myScenario <- scenario(myProject,
                       scenario="Distributions [Site, Lat, Forest]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("Distributions [Forested Wetland; Initial C]",
                            "Distributions [Forested Wetland; CH4]",
                            "Distributions [Forested Wetland; Flow Multipliers]",
                            "Distributions [CH4, Lat, Site]",
                            "Distributions [Flow Multipliers, Lat, Site]",
                            "Distributions [Initial C, Lat, Site]",
                            "Distributions [NPP, Lat, Site]")

rm(myScenario)



# Merge SF Flow Multipliers site level

myScenario <- scenario(myProject, 
                       scenario="SF Flow Multipliers [Site, Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T


dependency(myScenario) <- c("SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland, Site]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat; Site]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")


