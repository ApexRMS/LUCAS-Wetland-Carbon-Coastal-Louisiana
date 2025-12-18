# ApexRMS
# Updated 2025-02-25
# Run after step2c-AddUncertainty.R
# This script runs spinups for two forested wetland sites, mean, and an oak gum cypress forest
# Estuarine forested wetland parameters are set to match a palustrine forested wetland

old <- options(pillar.sigfig = 10)

library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

rootPath <- "E:/gitprojects/A329-LucasBarataria/"

outpathDatasheets <- paste0(rootPath,"Data/Datasheets Wetland/Output/")

source(paste0(rootPath, "Scripts/calculateDecayRates.R"))

numberOfJobs <- 7

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

# Create a folder for the spinups
folderSpinup <- folder(ssimObject = myProject, folder = "Single-Cell Spinups", parentFolder = "3. Single-Cell Scenarios")


# Add transition pathway for forested wetland so it can be harvested in spinup
# Maybe don't add this to the full model just yet. Only for spinup. Not sure how this will 
# impact Ben's script or LA models

myScenario <- scenario(myProject, 
                       scenario = "STSM Transition Pathways [Harvest, Add Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "STSM Transition Pathways [Harvest]")

sheetName <- "stsim_Transition"
myData <- datasheet(myScenario, name = sheetName)

myData <- myData %>%
  filter(StateClassIdSource == "Forest: Oak/Gum/Cypress Group",
         TransitionTypeId == "Forest Harvest: Forest Clearcut") %>%
  mutate(StateClassIdSource = "Wetland: Palustrine Forested",
         StateClassIdDest = "Wetland: Palustrine Forested")

saveDatasheet(myScenario, myData, sheetName, append = TRUE)

rm(myScenario, myData, sheetName)

# Merge Scenarios
myScenario <- scenario(myProject, 
                       scenario = "STSM Transition Pathways [Update Harvest: Wetland]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("STSM Transition Pathways [Ag Contraction]",
                            "STSM Transition Pathways [Ag Expansion]",
                            "STSM Transition Pathways [Fire]",
                            "STSM Transition Pathways [Harvest, Add Forested Wetland]",
                            "STSM Transition Pathways [Intensification]",
                            "STSM Transition Pathways [Urbanization]")

rm(myScenario)

# Merge sub-scenarios: Carbon and LULC model

myScenario <- scenario(myProject, 
                       scenario="SF Flow Pathways [Event, Base, Original]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Pathways [Event Flows]",
                            "SF Flow Pathways [Base Flows, Add Methane]")

rm(myScenario)

# Add a pipeline
myScenario <- scenario(myProject, 
                       scenario = "Pipeline",
                       folder = "Single-Cell Sub-Scenarios")
myData <- data.frame(StageNameId = "ST-Sim",
                     RunOrder = 1)
saveDatasheet(myScenario, myData, "core_Pipeline", append = FALSE)
rm(myData)

# Merge flow pathways
myScenario <- scenario(myProject, 
                       scenario="Single Cell: Carbon and LULC: Spinup Original",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("STSM Transition Pathways [Update Harvest: Wetland]",
                            "SF Initial Stocks",
                            "SF Stock and Flow Group Membership [Add Methane]",
                            "SF Output Options and Filters [Add Methane]",
                            "SF Flow Order [Updated]",
                            "SF Flow Pathways [Event, Base, Original]",
                            "Pipeline")

rm(myScenario)

# Set Stock limits

myScenario <- scenario(myProject, 
                       scenario="Stock Limit [Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios")

myData <- tibble(StateClassId = rep(c("Wetland: Palustrine Forested",
                                      "Wetland: Estuarine Forested",
                                      "Wetland: Palustrine Emergent",
                                      "Wetland: Estuarine Emergent"),3),
                 StockTypeId = rep(c("DOM: Belowground Slow",
                                     "Atmosphere Temp", 
                                     "Deep Soil"), each = 4),
                 StockMinimum = 0)

saveDatasheet(myScenario, myData, "stsim_StockLimit", append = FALSE)

rm(myScenario,myData)

myScenario <- scenario(myProject, 
                       scenario="Stock Limit [All]",
                       folder = "Single-Cell Sub-Scenarios")

myData <- tibble(StockTypeId = c("DOM: Belowground Slow",
                                 "Atmosphere Temp", 
                                 "Deep Soil",
                                 "Biomass: Coarse Root",
                                 "Biomass: Fine Root",
                                 "Biomass: Foliage",
                                 "Biomass: Merchantable",
                                 "Biomass: Other Wood",
                                 "DOM: Aboveground Fast",
                                 "DOM: Aboveground Medium",
                                 "DOM: Aboveground Slow",
                                 "DOM: Aboveground Very Fast",
                                 "DOM: Belowground Fast",
                                 "DOM: Belowground Very Fast",
                                 "DOM: Snag Branch",
                                 "DOM: Snag Stem"),
                 StockMinimum = 0)

saveDatasheet(myScenario, myData, "stsim_StockLimit", append = FALSE)

rm(myScenario,myData)

# Merge State Attributes Mean, Add Methane

myScenario <- scenario(myProject, 
                       scenario="Net Growth, Methane [Updated]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SAV: Methane: Forested and Emergent",
                            "STSM State Attributes [Mean Net Growth, Add Wetland]")

rm(myScenario)

# Merge State Attributes Mean, Add Methane

myScenario <- scenario(myProject, 
                       scenario="Net Growth, Methane [Updated S]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SAV: Methane: Forested S",
                            "STSM State Attributes [Mean Net Growth, Add Wetland]")

rm(myScenario)

myScenario <- scenario(myProject, 
                       scenario="Net Growth, Methane [Updated W]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SAV: Methane: Forested W",
                            "STSM State Attributes [Mean Net Growth, Add Wetland]")

rm(myScenario)

#Turn on multiprocessing
sheetName <- "core_Multiprocessing"
multiTab <- datasheet(myLibrary,name = sheetName)

multiTab$EnableMultiprocessing <- TRUE
multiTab$MaximumJobs <- numberOfJobs

saveDatasheet(myLibrary, multiTab, sheetName, append = FALSE)

rm(multiTab,sheetName)

# Re-initialize carbon (spinup)

# sub-scenario: 40 MC: Forest
myScenario <- scenario(myProject, 
                       scenario = "Run Control [Spinup Forest; Non-Spatial; 0-1674; 40 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 40,
                    MinimumTimestep = 0,
                    MaximumTimestep = 1674,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 1 MC: Forested Wetland more cycles
myScenario <- scenario(myProject, 
                       scenario = "Run Control [Spinup Forest; Non-Spatial; 0-3799; 1 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1,
                    MinimumTimestep = 0,
                    MaximumTimestep = 3799,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 1 MC: Forested Wetland more cycles
myScenario <- scenario(myProject, 
                       scenario = "Run Control [Spinup Forest; Non-Spatial; 0-124; 1 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1,
                    MinimumTimestep = 0,
                    MaximumTimestep = 124,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# Initial Conditions Forest: Wetland: Palustrine Forested Age 0

myScenario <- scenario(myProject, 
                       scenario = "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 0]",
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
                    AgeMin = 0,
                    AgeMax = 0,
                    RelativeAmount = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# Initial Conditions Forest: Oak Gum Cypress: Age 0

myScenario <- scenario(myProject, 
                       scenario = "Initial Conditions: Single Cell - Forest: Oak/Gum/Cypress Group [Age 0]",
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
                    AgeMin = 0,
                    AgeMax = 0,
                    RelativeAmount = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)


# Transition Multipliers: Forest and Wetland
myScenario <- scenario(myProject, scenario="Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest",
                       folder = "Single-Cell Sub-Scenarios")

transitionTypeGroup <- datasheet(myProject,name = "stsim_TransitionTypeGroup")

disturbanceYears <- c(seq(125,1375,125),seq(126,1376,125))
disturbanceYears <- disturbanceYears[order(disturbanceYears)]

transitionTypes <- unique(transitionTypeGroup$TransitionGroupId)
transitionTypes <- grep("Type",transitionTypes, value = T)

myData <- datasheet(myScenario, "stsim_TransitionMultiplierValue", optional = T, empty = T) %>%
  addRow(data.frame(Timestep = 0,
                    TransitionGroupId = transitionTypes,
                    Amount = 0)) %>%
  addRow(data.frame(Timestep = disturbanceYears,
                    TransitionGroupId = "Fire: High Severity [Type]",
                    Amount = rep(c(1,0),length(disturbanceYears)/2)))

myData$TransitionGroupId[myData$Timestep == 1375] <- "Forest Harvest: Forest Clearcut [Type]"
myData$TransitionGroupId[myData$Timestep == 1376] <- "Forest Harvest: Forest Clearcut [Type]"

saveDatasheet(myScenario, myData, "stsim_TransitionMultiplierValue", append = FALSE)

rm(myScenario,disturbanceYears,myData)

# Transition Multipliers: Forest and Wetland
myScenario <- scenario(myProject, scenario="Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest - more cycles",
                       folder = "Single-Cell Sub-Scenarios")

transitionTypeGroup <- datasheet(myProject,name = "stsim_TransitionTypeGroup")

disturbanceYears <- c(seq(125,3500,125),seq(126,3501,125))
disturbanceYears <- disturbanceYears[order(disturbanceYears)]

transitionTypes <- unique(transitionTypeGroup$TransitionGroupId)
transitionTypes <- grep("Type",transitionTypes, value = T)

myData <- datasheet(myScenario, "stsim_TransitionMultiplierValue", optional = T, empty = T) %>%
  addRow(data.frame(Timestep = 0,
                    TransitionGroupId = transitionTypes,
                    Amount = 0)) %>%
  addRow(data.frame(Timestep = disturbanceYears,
                    TransitionGroupId = "Fire: High Severity [Type]",
                    Amount = rep(c(1,0),length(disturbanceYears)/2)))

myData$TransitionGroupId[myData$Timestep == 3500] <- "Forest Harvest: Forest Clearcut [Type]"
myData$TransitionGroupId[myData$Timestep == 3501] <- "Forest Harvest: Forest Clearcut [Type]"

saveDatasheet(myScenario, myData, "stsim_TransitionMultiplierValue", append = FALSE)

rm(myScenario,disturbanceYears,myData)

# Initialize Oak Gum Cypress Upland Forest

myScenario <- scenario(myProject,
                       scenario="Original Forest: Oak Gum Cypress Spinup: Harvest",
                       folder = "Single-Cell Spinups")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [Spinup Forest; Non-Spatial; 0-1674; 40 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "Net Growth, Methane [Updated]",
                            "Initial Conditions: Single Cell - Forest: Oak/Gum/Cypress Group [Age 0]",
                            "Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest",
                            "SF Flow Multipliers [Update Forested Wetland]",
                            "Single Cell: Carbon and LULC: Spinup Original")

rm(myScenario)

run(myProject, scenario="Original Forest: Oak Gum Cypress Spinup: Harvest")

# Grab output data: Oak Gum Cypress Spinup

scenarioList <- scenario(myProject, summary = T, results = T)

forestId <- scenarioList$ScenarioId[grep("Original Forest: Oak Gum Cypress Spinup: Harvest",scenarioList$Name)]

myScenario <- scenario(myProject, scenario=max(forestId))

myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

# Has equilibrium been reached, difference is less than 1%, (difference in peaks, year prior to disturbance)
peaks <- seq(125,1375,125)-1

testE <- myData %>%
  filter(Timestep %in% peaks) %>%
  filter(StockGroupId == "DOM: Belowground Slow [Type]") %>%
  group_by(Timestep,StratumId,StateClassId,StockGroupId) %>%
  summarize(carbonMean = mean(Amount, na.rm = T)) %>%
  ungroup() %>%
  arrange(Timestep) %>%
  mutate(percentDiff = (carbonMean - lag(carbonMean))/lag(carbonMean) * 100)

print(testE, n = nrow(testE))

myData <- myData[myData$Timestep >= 1375,]
myData$AgeMin <- myData$Timestep - 1374
myData$AgeMax <- myData$Timestep - 1374

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

rm(myScenario)

myScenario <- scenario(myProject,
                       scenario="Init C Stocks at Equilibrium [Forest: Oak Gum Cypress Original]",
                       folder = "Single-Cell Sub-Scenarios")

myData4 <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(myData3)

saveDatasheet(myScenario, myData4, "stsim_StateAttributeValue", append = FALSE)

tail(myData4)
mean(myData$Amount[myData$AgeMin == 300 & myData$StockGroupId == "DOM: Snag Stem [Type]"])

rm(myData4,myScenario,myData3,myData2,myData,forestId,scenarioList)

# # Updated Scenarios for mean model
# # First run for 124 years to get BGS and BGVF eq values
# myScenario <- scenario(myProject,
#                        scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest M",
#                        folder = "Single-Cell Spinups")
# 
# mergeDependencies(myScenario) <- F
# 
# dependency(myScenario) <- c("Run Control [Spinup Forest; Non-Spatial; 0-124; 1 MC]",
#                             "Output Options [Non-Spatial; Summary]",
#                             "Net Growth, Methane [Updated]",
#                             "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 0]",
#                             "Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest - more cycles",
#                             "Stock Limit [All]",
#                             "SF Flow Multipliers [Update Forested Wetland]",
#                             "Single Cell: Carbon and LULC: Spinup Original")
# 
# rm(myScenario)
# 
# run(myProject, scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest M")
# 
# scenarioList <- scenario(myProject, summary = T, results = T)
# 
# forestId <- scenarioList$ScenarioId[grep("Updated Wetland: Palustrine Forested Spinup: Limit: Harvest M",scenarioList$Name)]
# 
# myScenario <- scenario(myProject, scenario=max(forestId))
# 
# myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)
# 
# eqBGVFm <- myData %>%
#   filter(StockGroupId == "DOM: Belowground Very Fast [Type]") %>%
#   filter(Timestep == 124) %>%
#   pull(Amount)
# 
# eqFRm <- myData %>%
#   filter(StockGroupId == "Biomass: Fine Root [Type]") %>%
#   filter(Timestep == 124) %>%
#   pull(Amount)
# 
# rm(myScenario, myData)

# # Flow Multipliers Forested Wetland
# myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower]",
#                        folder = "Single-Cell Sub-Scenarios")
# 
# myData <- datasheet(myScenario, "stsim_FlowMultiplier")
# 
# soilAge <- (1290+1295)/2
# soilPoolSize <- (628.4 + 127.1)/2
# 
# poolTotal <- soilPoolSize - eqBGVFm - eqFRm
# emissionsInOut <- 1.889 - (poolTotal/soilAge)# 2
# 
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# flowMultBGStoDeep <- poolTotal/(soilAge*poolTotal)
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm
# myData$Value[myData$FlowGroupId == "Stabilization: BG Slow -> Deep Soil [Type]"] <- flowMultBGStoDeep
# 
# saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

# Then run full model
myScenario <- scenario(myProject, 
                       scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest M",
                       folder = "Single-Cell Spinups")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [Spinup Forest; Non-Spatial; 0-3799; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "Net Growth, Methane [Updated]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 0]",
                            "Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest - more cycles",
                            "Stock Limit [All]",
                            "SF Flow Multipliers [Update Forested Wetland]",
                            "Single Cell: Carbon and LULC: Spinup Original")

rm(myScenario)

# projectName: Project
# scenarioName: The spinup scenario
# targetValue: Target value is the sum of BGS, BGVF, FR, CR, and BGF. The sum of all belowground carbon.
# convergenceLevel: Convergence level is how different (% difference) can the BGS pool be from the calculated value. 
# scenarioMult: Flow multiplier values that should be updated to ensure convergence
# emissionsStart: Starting value for emissions flux
# meanBurial: Burial rate

calculateDecayRates(
  projectName = myProject,
  scenarioName = "Updated Wetland: Palustrine Forested Spinup: Limit: Harvest M",
  targetValue = ((628.4 + 127.1) / 2),
  convergenceLevel = 0.01,
  scenarioMult = "SF Flow Multipliers [Forested Wetland BGS Slower]",
  emissionsStart = 1.457301,
  meanBurial = mean(c((628.4/1290),(127.1/1295)))
)

#run(myProject, scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest M")

# # Flow Multipliers Forested Wetland
# Only run if updating flow multiplier terms to match soil carbon value
# myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower]",
#                        folder = "Single-Cell Sub-Scenarios")
# 
# myData <- datasheet(myScenario, "stsim_FlowMultiplier")
# 
# eqBGVFm <- 0.1055309
# eqFRm <- 2.682043
# 
# soilAge <- (1290+1295)/2
# soilPoolSize <- (628.4 + 127.1)/2
# 
# poolTotal <- soilPoolSize - eqBGVFm - eqFRm
# emissionsInOut <- 1.889 - (poolTotal/soilAge)# 2
# 
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm
# 
# saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)


# Updated spinup for S

# # Only run this section if net growth changes
# myScenario <- scenario(myProject,
#                        scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest S",
#                        folder = "Single-Cell Spinups")
# 
# mergeDependencies(myScenario) <- F
# 
# dependency(myScenario) <- c("Run Control [Spinup Forest; Non-Spatial; 0-124; 1 MC]",
#                             "Output Options [Non-Spatial; Summary]",
#                             "Net Growth, Methane [Updated S]",
#                             "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 0]",
#                             "Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest - more cycles",
#                             "Stock Limit [All]",
#                             "SF Flow Multipliers [Forested Wetland BGS Slower S]",
#                             "Single Cell: Carbon and LULC: Spinup Original")
# 
# rm(myScenario)
# 
# run(myProject, scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest S")
# 
# scenarioList <- scenario(myProject, summary = T, results = T)
# 
# forestId <- scenarioList$ScenarioId[grep("Updated Wetland: Palustrine Forested Spinup: Limit: Harvest S",scenarioList$Name)]
# 
# myScenario <- scenario(myProject, scenario=max(forestId))
# 
# myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)
# 
# eqBGVFs <- myData %>%
#   filter(StockGroupId == "DOM: Belowground Very Fast [Type]") %>%
#   filter(Timestep == 124) %>%
#   pull(Amount)
# 
# eqFRs <- myData %>%
#   filter(StockGroupId == "Biomass: Fine Root [Type]") %>%
#   filter(Timestep == 124) %>%
#   pull(Amount)
# 
# rm(myScenario, myData)
# 
# # Flow Multipliers Forested Wetland
# myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower S]",
#                        folder = "Single-Cell Sub-Scenarios")
# 
# myData <- datasheet(myScenario, "stsim_FlowMultiplier")
# 
# soilAge <- 1290
# soilPoolSize <- 628.4
# eqBGVFs <- 0.06819238
# eqFRs <- 1.733207
# 
# poolTotal <- soilPoolSize - eqBGVFs - eqFRs
# emissionsInOut <- 1.456 - (poolTotal/soilAge) #1.472
# 
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# flowMultBGStoDeep <- poolTotal/(soilAge*poolTotal)
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm
# myData$Value[myData$FlowGroupId == "Stabilization: BG Slow -> Deep Soil [Type]"] <- flowMultBGStoDeep
# 
# saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

myScenario <- scenario(myProject, 
                       scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest S",
                       folder = "Single-Cell Spinups")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [Spinup Forest; Non-Spatial; 0-3799; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "Net Growth, Methane [Updated S]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 0]",
                            "Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest - more cycles",
                            "Stock Limit [All]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower S]",
                            "Single Cell: Carbon and LULC: Spinup Original")

rm(myScenario)

calculateDecayRates(
  projectName = myProject,
  scenarioName = "Updated Wetland: Palustrine Forested Spinup: Limit: Harvest S",
  targetValue = 628.4,
  convergenceLevel = 0.01,
  scenarioMult = "SF Flow Multipliers [Forested Wetland BGS Slower S]",
  emissionsStart = 0.9080549,
  meanBurial = 628.4/1290 #0.4871318
)


#run(myProject, scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest S")

# # Flow Multipliers Forested Wetland
# # Only run if updating flow multiplier terms to match soil carbon value
# myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower S]",
#                        folder = "Single-Cell Sub-Scenarios")
# 
# myData <- datasheet(myScenario, "stsim_FlowMultiplier")
# 
# soilAge <- 1290
# soilPoolSize <- 628.4
# eqBGVFs <- 0.06819238
# eqFRs <- 1.733207
# 
# poolTotal <- soilPoolSize - eqBGVFs - eqFRs
# emissionsInOut <- 1.456 - (poolTotal/soilAge) #1.472
# 
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm
# 
# saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

# Updated Spinup for W
# # Only run to calculate equilibrium DOM pools
# myScenario <- scenario(myProject,
#                        scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest W",
#                        folder = "Single-Cell Spinups")
# 
# mergeDependencies(myScenario) <- F
# 
# dependency(myScenario) <- c("Run Control [Spinup Forest; Non-Spatial; 0-124; 1 MC]",
#                             "Output Options [Non-Spatial; Summary]",
#                             "Net Growth, Methane [Updated W]",
#                             "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 0]",
#                             "Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest - more cycles",
#                             "Stock Limit [All]",
#                             "SF Flow Multipliers [Forested Wetland BGS Slower W]",
#                             "Single Cell: Carbon and LULC: Spinup Original")
# 
# rm(myScenario)
# 
# run(myProject, scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest W")
# 
# scenarioList <- scenario(myProject, summary = T, results = T)
# 
# forestId <- scenarioList$ScenarioId[grep("Updated Wetland: Palustrine Forested Spinup: Limit: Harvest W",scenarioList$Name)]
# 
# myScenario <- scenario(myProject, scenario=max(forestId))
# 
# myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)
# 
# eqBGVFw <- myData %>%
#   filter(StockGroupId == "DOM: Belowground Very Fast [Type]") %>%
#   filter(Timestep == 124) %>%
#   pull(Amount)
# 
# eqFRw <- myData %>%
#   filter(StockGroupId == "Biomass: Fine Root [Type]") %>%
#   filter(Timestep == 124) %>%
#   pull(Amount)
# 
# rm(myScenario, myData)
# 
# myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower W]",
#                        folder = "Single-Cell Sub-Scenarios")
# 
# myData <- datasheet(myScenario, "stsim_FlowMultiplier")
# 
# soilAge <- 1295
# soilPoolSize <- 127.1
# eqBGVFw <- 0.1428744
# eqFRw <- 3.630878
# 
# poolTotal <- soilPoolSize - eqBGVFw - eqFRw
# emissionsInOut <- 2.531 - (poolTotal/soilAge)# 2.53
# 
# flowMultBGStoDeep <- poolTotal/(soilAge*poolTotal)
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm
# myData$Value[myData$FlowGroupId == "Stabilization: BG Slow -> Deep Soil [Type]"] <- flowMultBGStoDeep
# 
# saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

# Run model for W site
myScenario <- scenario(myProject, 
                       scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest W",
                       folder = "Single-Cell Spinups")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [Spinup Forest; Non-Spatial; 0-3799; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "Net Growth, Methane [Updated W]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 0]",
                            "Transition Multipliers: Single Cell - Forested Wetland Spinup: Harvest - more cycles",
                            "Stock Limit [All]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower W]",
                            "Single Cell: Carbon and LULC: Spinup Original")

rm(myScenario)

calculateDecayRates(
  projectName = myProject,
  scenarioName = "Updated Wetland: Palustrine Forested Spinup: Limit: Harvest W",
  targetValue = 127.1,
  convergenceLevel = 0.01,
  scenarioMult = "SF Flow Multipliers [Forested Wetland BGS Slower W]",
  emissionsStart = 1.924325,
  meanBurial = 127.1/1295 #0.09814672
)


#run(myProject, scenario="Updated Wetland: Palustrine Forested Spinup: Limit: Harvest W")

# # Flow Multipliers Forested Wetland
# #Only run if updating flow multiplier terms to match soil carbon value
# myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower W]",
#                        folder = "Single-Cell Sub-Scenarios")
# 
# myData <- datasheet(myScenario, "stsim_FlowMultiplier")
# 
# soilAge <- 1295
# soilPoolSize <- 127.1
# eqBGVFw <- 0.1428744
# eqFRw <- 3.630878
# 
# poolTotal <- soilPoolSize - eqBGVFw - eqFRw
# emissionsInOut <- 2.531 - (poolTotal/soilAge)# 2.53
# 
# flowMultBGStoDeep <- poolTotal/(soilAge*poolTotal)
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm
# 
# saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)


# Calculate Lateral Flux average
latW <- 4.36 - 1.700 - (0.151-0.116)
latS <- 2.67 - 1.796 - (0.822-0.633)
latMean <- mean(c(latW,latS))

# Grab output data: Wetland: Palustrine Forested Spinup

scenarioList <- scenario(myProject, summary = T, results = T)

forestId <- scenarioList$ScenarioId[grep("Updated Wetland: Palustrine Forested Spinup: Limit: Harvest M",scenarioList$Name)]

myScenario <- scenario(myProject, scenario=max(forestId))

myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

myDataFlux <- datasheet(myScenario, "stsim_OutputFlow", optional = T)

# Change 3499 to 3565. 66 year old forest following a harvest.

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
         Prop = (Amount/Sum)*latMean) %>%
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

write.csv(myDataLatPartition,paste0(outpathDatasheets,"LatPartition.csv"), row.names = F)

# Has equilibrium been reached, difference is less than 1%, (difference in peaks, year prior to disturbance)
peaks <- seq(125,3500,125)-1

testE <- myData %>%
  filter(Timestep %in% peaks) %>%
  filter(StockGroupId == "DOM: Belowground Slow [Type]") %>%
  group_by(Timestep,StratumId,StateClassId,StockGroupId) %>%
  summarize(carbonMean = mean(Amount, na.rm = T)) %>%
  ungroup() %>%
  arrange(Timestep) %>%
  mutate(percentDiff = (carbonMean - lag(carbonMean))/lag(carbonMean) * 100)

print(testE, n = nrow(testE))

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
                       scenario="Init C Stocks at Equilibrium [Wetland: Palustrine Forested Updated]",
                       folder = "Single-Cell Sub-Scenarios")

write.csv(myData3,
          paste0(outpathDatasheets,"stsim_StateAttributeValue_ForestedWetland_InitialC_2001.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeValue_ForestedWetland_InitialC_2001.csv"))

myCSV2 <- myCSV %>%
  mutate(StateClassId = "Wetland: Estuarine Forested")

myData4 <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(myCSV) %>%
  addRow(myCSV2)

saveDatasheet(myScenario, myData4, "stsim_StateAttributeValue", append = FALSE)

tail(myData4)
mean(myData$Amount[myData$AgeMin == 300 & myData$StockGroupId == "DOM: Snag Stem [Type]"])

rm(myData4,myScenario,myData3,myData2,myData,forestId,scenarioList, myCSV, myCSV2)

