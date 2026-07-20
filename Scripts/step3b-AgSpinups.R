# ApexRMS
# Updated 2025-02-25
# Run after step3a-ForestSpinupsUncertainty.R
# This script creates an agricultural spinup
# which is used for all crop, pasture, barren, and developed land

library(rsyncrosim)
library(tidyverse)

source(paste0(rootPath, "Scripts/gwpConfig.R"))
gwpVariant <- gwpVariants[[activeGWP]]

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

pathInDatasheets <- paste0(rootPath,"Data/Datasheets Wetland/")

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")


# Ag spinup
# sub-scenario: 40 MC: Ag
myScenario <- scenario(myProject, 
                       scenario = "Run Control [Spinup Agriculture; Non-Spatial; 1850-2001; 40 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 40,
                    MinimumTimestep = 1850,
                    MaximumTimestep = 2001,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# SAV: Net Growth Ag

myScenario <- scenario(myProject, 
                       scenario = "SAV: Agriculture [Net Growth]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_StateAttributeValue"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE, optional = TRUE) %>% 
  addRow(data.frame(StateClassId = c("Agriculture: Cropland",
                                     "Agriculture: Pasture",
                                     "Barren: All",
                                     "Developed: High Intensity",
                                     "Developed: Low Intensity",
                                     "Developed: Medium Intensity",
                                     "Developed: Open Space"),
                    StateAttributeTypeId = "Net Growth",
                    Value = 0))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# Transition Multipliers: Ag

transitionTypeGroup <- datasheet(myProject,name = "stsim_TransitionTypeGroup")
transitionTypes <- unique(transitionTypeGroup$TransitionGroupId)
transitionTypes <- grep("Type",transitionTypes, value = T)

myScenario <- scenario(myProject, 
                       scenario="Transition Multipliers: Single Cell - Agriculture Spinup",
                       folder = "Single-Cell Sub-Scenarios")

myData <- datasheet(myScenario, "stsim_TransitionMultiplierValue", optional = T, empty = T) %>%
  addRow(data.frame(Timestep = 0,
                    TransitionGroupId = transitionTypes,
                    Amount = 0)) %>%
  addRow(data.frame(Timestep = c(1900,1901),
                    TransitionGroupId = c("Ag Expansion: Cropland [Type]",
                                          "Ag Expansion: Cropland [Type]"),
                    Amount = c(1,0)))

saveDatasheet(myScenario, myData, "stsim_TransitionMultiplierValue", append = FALSE)

rm(myScenario,myData)

# Initial Conditions Forest: Wetland: Palustrine Forested Age 124

myScenario <- scenario(myProject, 
                       scenario = "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 124]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_InitialConditionsNonSpatial"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(TotalAmount = 1,
                    NumCells = 1,
                    CalcFromDist = TRUE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myData, sheetName)

sheetName <- "stsim_InitialConditionsNonSpatialDistribution"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE, optional = TRUE) %>% 
  addRow(data.frame(StratumId = "Study Area",
                    StateClassId = "Wetland: Palustrine Forested",
                    AgeMin = 124,
                    AgeMax = 124,
                    RelativeAmount = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# Initial Conditions Forest: Oak Gum Cypress: Age 124

myScenario <- scenario(myProject, 
                       scenario = "Initial Conditions: Single Cell - Forest: Oak/Gum/Cypress Group [Age 124]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_InitialConditionsNonSpatial"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(TotalAmount = 1,
                    NumCells = 1,
                    CalcFromDist = TRUE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myData, sheetName)

sheetName <- "stsim_InitialConditionsNonSpatialDistribution"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE, optional = TRUE) %>% 
  addRow(data.frame(StratumId = "Study Area",
                    StateClassId = "Forest: Oak/Gum/Cypress Group",
                    AgeMin = 124,
                    AgeMax = 124,
                    RelativeAmount = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# SF Flow Multipliers Non Forest Ag

myScenario <- scenario(myProject, 
                       scenario="SF Flow Multipliers [Non Forest; Updated]",
                       folder = "Single-Cell Sub-Scenarios")

flowMultipliers <- read.csv(paste0(pathInDatasheets,"FlowMultipliersLAModel.csv"), stringsAsFactors = F)
names(flowMultipliers) <- gsub("ID","Id",names(flowMultipliers))

flowMultipliersAgCrop <- flowMultipliers %>%
  filter(StateClassId == "Agriculture:All") %>%
  mutate(StateClassId = "Agriculture: Cropland") %>%
  select(StateClassId,FlowGroupId,Value)

flowMultipliersAgPast <- flowMultipliers %>%
  filter(StateClassId == "Agriculture:All") %>%
  mutate(StateClassId = "Agriculture: Pasture") %>%
  select(StateClassId,FlowGroupId,Value)

flowMultipliersBarren <- flowMultipliers %>%
  filter(StateClassId == "Agriculture:All") %>%
  mutate(StateClassId = "Barren: All") %>%
  select(StateClassId,FlowGroupId,Value)

flowMultipliersDevHigh <- flowMultipliers %>%
  filter(StateClassId == "Agriculture:All") %>%
  mutate(StateClassId = "Developed: High Intensity") %>%
  select(StateClassId,FlowGroupId,Value)

flowMultipliersDevLow <- flowMultipliersDevHigh %>%
  mutate(StateClassId = "Developed: Low Intensity") %>%
  select(StateClassId,FlowGroupId,Value)

flowMultipliersDevMed <- flowMultipliersDevHigh %>%
  mutate(StateClassId = "Developed: Medium Intensity") %>%
  select(StateClassId,FlowGroupId,Value)

flowMultipliersDevOpen <- flowMultipliersDevHigh %>%
  mutate(StateClassId = "Developed: Open Space") %>%
  select(StateClassId,FlowGroupId,Value)

flowMultipliersShore <- flowMultipliers %>%
  filter(StateClassId == "Unconsolidated Shore:All") %>%
  mutate(StateClassId = "Wetland: Unconsolidated Shore") %>%
  select(StateClassId,FlowGroupId,Value) %>%
  bind_rows(tibble(StateClassId = "Wetland: Unconsolidated Shore",
                   FlowGroupId = "Emission: BG Slow -> Atmosphere [Type]", 
                   Value = 0))#0.0033

flowMultipliersWater <- flowMultipliers %>%
  filter(StateClassId == "Water:All") %>%
  mutate(StateClassId = "Water: All") %>%
  select(StateClassId,FlowGroupId,Value) %>%
  bind_rows(tibble(StateClassId = "Water: All",
                   FlowGroupId = "Emission: BG Slow -> Atmosphere [Type]", 
                   Value = 0))#0.0033

myData <- datasheet(myScenario, "stsim_FlowMultiplier", optional = T, empty = T) %>%
  addRow(flowMultipliersAgCrop) %>%
  addRow(flowMultipliersAgPast) %>%
  addRow(flowMultipliersBarren) %>%
  addRow(flowMultipliersDevHigh) %>%
  addRow(flowMultipliersDevLow) %>%
  addRow(flowMultipliersDevMed) %>%
  addRow(flowMultipliersDevOpen) %>%
  addRow(flowMultipliersShore) %>%
  addRow(flowMultipliersWater)

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

rm(myScenario, myData,flowMultipliers,flowMultipliersAgCrop,flowMultipliersAgPast,
   flowMultipliersBarren,flowMultipliersDevHigh,flowMultipliersDevLow,
   flowMultipliersDevMed,flowMultipliersDevOpen,flowMultipliersShore,
   flowMultipliersWater)


# Now merge these tables

myScenario <- scenario(myProject, 
                       scenario="SF Flow Multipliers [Update Emergent, Ag]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")

rm(myScenario)

myScenario <- scenario(myProject,
                       scenario=vTag("SF Flow Pathways [Base Flows, Add Methane, Add Ag]", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios",
                       sourceScenario = vTag("SF Flow Pathways [Base Flows, Add Methane]", gwpVariant, style="bracket"))

myData <- datasheet(myScenario, name = "stsim_FlowPathway")

flowPathways <- read.csv(paste0(pathInDatasheets,"FlowPathwaysLAModel.csv"), stringsAsFactors = F)
names(flowPathways) <- gsub("ID","Id",names(flowPathways))

flowPathways$StateAttributeTypeId[flowPathways$StateAttributeTypeId == "NPP"] <- "Net Growth"

flowPathwaysAgCrop <- flowPathways %>%
  filter(FromStateClassId == "Agriculture:All") %>%
  mutate(FromStateClassId = "Agriculture: Cropland") %>%
  select(FromStateClassId,FromStockTypeId,ToStockTypeId,StateAttributeTypeId,FlowTypeId,Multiplier)

flowPathwaysAgCropNG <- flowPathwaysAgCrop %>%
  filter(StateAttributeTypeId == "Net Growth") %>%
  mutate(FlowTypeId = gsub("Net Growth Wetland Forested","Net Growth Non Forest",FlowTypeId))

flowPathwaysAgCropO <- flowPathwaysAgCrop %>%
  filter(!(StateAttributeTypeId == "Net Growth")) %>%
  select(-StateAttributeTypeId)

flowPathwaysAgPastNG <- flowPathwaysAgCropNG %>%
  mutate(FromStateClassId = "Agriculture: Pasture")

flowPathwaysAgPastO <- flowPathwaysAgCropO %>%
  mutate(FromStateClassId = "Agriculture: Pasture")


flowPathwaysBarrenNG <- flowPathwaysAgCropNG %>%
  mutate(FromStateClassId = "Barren: All")

flowPathwaysBarrenO <- flowPathwaysAgCropO %>%
  mutate(FromStateClassId = "Barren: All")


flowPathwaysDevHighNG <- flowPathwaysAgCropNG %>%
  mutate(FromStateClassId = "Developed: High Intensity")

flowPathwaysDevHighO <- flowPathwaysAgCropO %>%
  mutate(FromStateClassId = "Developed: High Intensity")


flowPathwaysDevLowNG <- flowPathwaysAgCropNG %>%
  mutate(FromStateClassId = "Developed: Low Intensity")

flowPathwaysDevLowO <- flowPathwaysAgCropO %>%
  mutate(FromStateClassId = "Developed: Low Intensity")


flowPathwaysDevMedNG <- flowPathwaysAgCropNG %>%
  mutate(FromStateClassId = "Developed: Medium Intensity")

flowPathwaysDevMedO <- flowPathwaysAgCropO %>%
  mutate(FromStateClassId = "Developed: Medium Intensity")


flowPathwaysDevOpenNG <- flowPathwaysAgCropNG %>%
  mutate(FromStateClassId = "Developed: Open Space")

flowPathwaysDevOpenO <- flowPathwaysAgCropO %>%
  mutate(FromStateClassId = "Developed: Open Space")


flowPathwaysShore <- flowPathways %>%
  filter(FromStateClassId == "Unconsolidated Shore:All") %>%
  mutate(FromStateClassId = "Wetland: Unconsolidated Shore") %>%
  select(FromStateClassId,FromStockTypeId,ToStockTypeId,FlowTypeId,Multiplier)


flowPathwaysWater <- flowPathways %>%
  filter(FromStateClassId == "Water:All") %>%
  mutate(FromStateClassId = "Water: All") %>%
  select(FromStateClassId,FromStockTypeId,ToStockTypeId,FlowTypeId,Multiplier)


myData1 <- datasheet(myScenario, "stsim_FlowPathway", optional = T, empty = T) %>%
  addRow(flowPathwaysAgCropNG) %>%
  addRow(flowPathwaysAgPastNG) %>%
  addRow(flowPathwaysBarrenNG) %>%
  addRow(flowPathwaysDevHighNG) %>%
  addRow(flowPathwaysDevLowNG) %>%
  addRow(flowPathwaysDevMedNG) %>%
  addRow(flowPathwaysDevOpenNG)

myData2 <- datasheet(myScenario, "stsim_FlowPathway", optional = T, empty = T) %>%
  addRow(flowPathwaysBarrenO) %>%
  addRow(flowPathwaysDevHighO) %>%
  addRow(flowPathwaysDevLowO) %>%
  addRow(flowPathwaysDevMedO) %>%
  addRow(flowPathwaysDevOpenO) %>%
  addRow(flowPathwaysShore) %>%
  addRow(flowPathwaysWater) %>%
  addRow(flowPathwaysAgCropO) %>%
  addRow(flowPathwaysAgPastO)

saveDatasheet(myScenario, myData1, "stsim_FlowPathway", append = TRUE)
saveDatasheet(myScenario, myData2, "stsim_FlowPathway", append = TRUE)

rm(myScenario,myData1,myData2,flowPathways,flowPathwaysBarrenO,flowPathwaysDevHighO,
   flowPathwaysDevLowO,flowPathwaysDevMedO,flowPathwaysDevOpenO,flowPathwaysShore,flowPathwaysWater,
   flowPathwaysAgCropO,flowPathwaysAgPastO,flowPathwaysAgCropNG,flowPathwaysAgPastNG,
   flowPathwaysBarrenNG,flowPathwaysDevHighNG,
   flowPathwaysDevLowNG,flowPathwaysDevMedNG,flowPathwaysDevOpenNG)

myScenario <- scenario(myProject, 
                       scenario="Init C Stocks at Equilibrium Water and Shore [Mean]",
                       folder = "Single-Cell Sub-Scenarios")

myUpdate <- read.csv(paste0(pathInDatasheets,"StateAttributeValuesOtherLAModel.csv"), stringsAsFactors = F)
names(myUpdate) <- gsub("ID","Id",names(myUpdate))

myUpdate <- myUpdate %>%
  select(StateClassId,StateAttributeTypeId,Value) %>%
  filter(!(StateClassId %in% c("Developed:All",
                               "Barren:All")))

saShore <- myUpdate %>%
  filter(StateClassId == "Unconsolidated Shore:All") %>%
  mutate(StateClassId = "Wetland: Unconsolidated Shore")

saWater <- myUpdate %>%
  filter(StateClassId == "Water:All") %>%
  mutate(StateClassId = "Water: All")

myData <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(saShore) %>%
  addRow(saWater)

saveDatasheet(myScenario, myData, "stsim_StateAttributeValue", append = FALSE)

rm(myScenario,myUpdate,saShore,saWater)

# Merge flow pathways

myScenario <- scenario(myProject,
                       scenario=vTag("SF Flow Pathways [Event, Base, Updated]", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Pathways [Event Flows]",
                            vTag("SF Flow Pathways [Base Flows, Add Methane, Add Ag]", gwpVariant, style="bracket"))

rm(myScenario)


myScenario <- scenario(myProject,
                       scenario=vTag("SAV: Ag [Update]", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("Init C Stocks at Equilibrium Water and Shore [Mean]",
                            "SAV: Agriculture [Net Growth]",
                            "SAV: Methane: Forested and Emergent",
                            "STSM State Attributes [Mean Net Growth, Add Wetland]",
                            vTag("Init C Stocks at Equilibrium [Forest: Oak Gum Cypress Original]", gwpVariant, style="bracket"),
                            vTag("Init C Stocks at Equilibrium [Wetland: Palustrine Forested Updated]", gwpVariant, style="bracket"),
                            "Init C Stocks at Equilibrium Emergent Wetland [Mean]")

rm(myScenario)

# Merge flow pathways
myScenario <- scenario(myProject,
                       scenario=vTag("Single Cell: Carbon and LULC: Spinup Ag", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Stock Limit [All]",
                            "STSM Transition Pathways [Update Harvest: Wetland]",
                            "SF Initial Stocks",
                            vTag("SF Stock and Flow Group Membership [Add Methane]", gwpVariant, style="bracket"),
                            vTag("SF Output Options and Filters [Add Methane]", gwpVariant, style="bracket"),
                            "SF Flow Order [Updated]",
                            vTag("SF Flow Pathways [Event, Base, Updated]", gwpVariant, style="bracket"),
                            "Pipeline")

rm(myScenario)


# Ag Spinups

myScenario <- scenario(myProject,
                       scenario=vTag("Original Agriculture: Oak Gum Cypress to Cropland Spinup", gwpVariant, style="bracket"),
                       folder = "Single-Cell Spinups")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [Spinup Agriculture; Non-Spatial; 1850-2001; 40 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            vTag("SAV: Ag [Update]", gwpVariant, style="bracket"),
                            "Initial Conditions: Single Cell - Forest: Oak/Gum/Cypress Group [Age 124]",
                            "Transition Multipliers: Single Cell - Agriculture Spinup",
                            "SF Flow Multipliers [Update Emergent, Ag]",
                            vTag("Single Cell: Carbon and LULC: Spinup Ag", gwpVariant, style="bracket"))

rm(myScenario)


# Ag Spinups

myScenario <- scenario(myProject,
                       scenario=vTag("Agriculture: Forested Wetland Update to Cropland Spinup", gwpVariant, style="bracket"),
                       folder = "Single-Cell Spinups")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [Spinup Agriculture; Non-Spatial; 1850-2001; 40 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            vTag("SAV: Ag [Update]", gwpVariant, style="bracket"),
                            "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 124]",
                            "Transition Multipliers: Single Cell - Agriculture Spinup",
                            "SF Flow Multipliers [Update Emergent, Ag]",
                            vTag("Single Cell: Carbon and LULC: Spinup Ag", gwpVariant, style="bracket"))

rm(myScenario)


# Ag Spinups

myScenario <- scenario(myProject,
                       scenario=vTag("Agriculture: Emergent Wetland Update to Cropland Spinup", gwpVariant, style="bracket"),
                       folder = "Single-Cell Spinups")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [Spinup Agriculture; Non-Spatial; 1850-2001; 40 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            vTag("SAV: Ag [Update]", gwpVariant, style="bracket"),
                            "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                            "Transition Multipliers: Single Cell - Agriculture Spinup",
                            "SF Flow Multipliers [Update Emergent, Ag]",
                            vTag("Single Cell: Carbon and LULC: Spinup Ag", gwpVariant, style="bracket"))

rm(myScenario)

runIfNeeded(myProject, vTag("Original Agriculture: Oak Gum Cypress to Cropland Spinup", gwpVariant, style="bracket"))
runIfNeeded(myProject, vTag("Agriculture: Forested Wetland Update to Cropland Spinup", gwpVariant, style="bracket"))
runIfNeeded(myProject, vTag("Agriculture: Emergent Wetland Update to Cropland Spinup", gwpVariant, style="bracket"))


# Grab output data: Ag spinup
myScenario <- getScenarioExact(myProject, vTag("Original Agriculture: Oak Gum Cypress to Cropland Spinup", gwpVariant, style="bracket"))

myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

myData <- myData[myData$Timestep == 2001,]

head(myData)

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
  group_by(StateClassId,StateAttributeTypeId) %>%
  summarize(Value = mean(Amount, na.rm = T))

rm(myScenario)

myScenario <- scenario(myProject, 
                       scenario="Init C Stocks at Equilibrium [Ag: Cropland; Prev Oak Gum Cypress Forest]",
                       folder = "Single-Cell Sub-Scenarios")

myData4 <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(myData3)

myData4$Value[myData4$Value < 0] <- 0

saveDatasheet(myScenario, myData4, "stsim_StateAttributeValue", append = FALSE)

tail(myData4)
mean(myData$Amount[myData$StockGroupId == "DOM: Snag Stem [Type]"])

rm(myData4,myScenario,myData3,myData2,myData)

# Grab output data: Ag spinup
myScenario <- getScenarioExact(myProject, vTag("Agriculture: Forested Wetland Update to Cropland Spinup", gwpVariant, style="bracket"))

myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

myData <- myData[myData$Timestep == 2001,]

head(myData)

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
  group_by(StateClassId,StateAttributeTypeId) %>%
  summarize(Value = mean(Amount, na.rm = T))

myData3$Value[myData3$StateAttributeTypeId == "Carbon Initial Conditions: Deep Soil"] <- 0

rm(myScenario)

myScenario <- scenario(myProject, 
                       scenario="Init C Stocks at Equilibrium [Ag: Cropland; Prev Wetland Forest]",
                       folder = "Single-Cell Sub-Scenarios")

myData4 <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(myData3)

myData4$Value[myData4$Value < 0] <- 0

saveDatasheet(myScenario, myData4, "stsim_StateAttributeValue", append = FALSE)

tail(myData4)
mean(myData$Amount[myData$StockGroupId == "DOM: Snag Stem [Type]"])

rm(myData4,myScenario,myData3,myData2,myData)

# Grab output data: Ag spinup
myScenario <- getScenarioExact(myProject, vTag("Agriculture: Emergent Wetland Update to Cropland Spinup", gwpVariant, style="bracket"))

myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

myData <- myData[myData$Timestep == 2001,]

head(myData)

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
  group_by(StateClassId,StateAttributeTypeId) %>%
  summarize(Value = mean(Amount, na.rm = T))

myData3$Value[myData3$StateAttributeTypeId == "Carbon Initial Conditions: Deep Soil"] <- 0

rm(myScenario)

myScenario <- scenario(myProject, 
                       scenario="Init C Stocks at Equilibrium [Ag: Cropland; Prev Emergent Wetland]",
                       folder = "Single-Cell Sub-Scenarios")

myData4 <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(myData3)

myData4$Value[myData4$Value < 0] <- 0

saveDatasheet(myScenario, myData4, "stsim_StateAttributeValue", append = FALSE)

tail(myData4)
mean(myData$Amount[myData$StockGroupId == "DOM: Snag Stem [Type]"])

rm(myData4,myScenario,myData3,myData2,myData)

