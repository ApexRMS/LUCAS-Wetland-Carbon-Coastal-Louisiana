# ApexRMS
# Updated 2025-02-25
# Run after step3c-AddUncertaintyForestedWetland.R
# This script adds scenarios for a transition from emergent wetland to water


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

# Update state class definitions

sheetName <- "stsim_StateLabelY"

myData <- data.frame(Name = c("Previously Emergent Wetland",
                              "Unvegetated Emergent",
                              "Previously Forested Wetland",
                              "Unvegetated Forested"),
                    Description = c("Water",
                                    "Wetland",
                                    "Water",
                                    "Wetland"))

saveDatasheet(myProject, myData, sheetName, append = T)

sheetName <- "stsim_StateClass"

myData <- data.frame(Name = c("Water: Previously Emergent Wetland",
                              "Wetland: Unvegetated Emergent",
                              "Water: Previously Forested Wetland",
                              "Wetland: Unvegetated Forested"),
                    StateLabelXId = c("Water",
                                      "Wetland",
                                      "Water",
                                      "Wetland"),
                    StateLabelYId = c("Previously Emergent Wetland",
                                      "Unvegetated Emergent",
                                      "Previously Forested Wetland",
                                      "Unvegetated Forested"),
                    Id = c(13,98,14,99),
                    Color = c(
                      "255,84,117,168",
                      "255,0,242,242",
                      "255,84,117,168",
                      "255,0,242,242"
                    ))

saveDatasheet(myProject, myData, sheetName, append = T)

# Add Transition Type

sheetName <- "stsim_TransitionType"

myDataOld <- datasheet(myProject,"stsim_TransitionType")

maxId <- max(myDataOld$Id)

myData <- data.frame(Name = c("Emergent Wetland to Water",
                              "Forested Wetland to Water",
                              "Emergent Wetland to Unvegetated",
                              "Forested Wetland to Unvegetated"),
                     Id = c(1,2,3,4) + maxId)

saveDatasheet(myProject, myData, sheetName)

stateClassTable <- datasheet(myProject,"stsim_StateClass")

# Update flow multiplier type

sheetName <- "stsim_FlowMultiplierType"

myData <- data.frame(Name = "Wetland to Water Limit")

saveDatasheet(myProject, myData, sheetName, append = T)

# Update Transition Pathways

# Wetland to Water
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways [Wetland to Water]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_DeterministicTransition"

myData <- datasheet(myScenario, sheetName) %>%
  addRow(data.frame(StateClassIdSource = stateClassTable$Name,
                    Location = paste0("A",1:length(stateClassTable$Name))))
saveDatasheet(myScenario, myData, sheetName, append = F)

sheetName <- "stsim_Transition"

myData <- datasheet(myScenario, sheetName, optional = T) %>%
  addRow(data.frame(StateClassIdSource = c("Wetland: Estuarine Emergent",
                                           "Wetland: Palustrine Emergent"),
                    StateClassIdDest = "Water: Previously Emergent Wetland",
                    TransitionTypeId = "Emergent Wetland to Water",
                    Probability = 1)) %>%
  addRow(data.frame(StateClassIdSource = c("Wetland: Palustrine Forested"),
                    StateClassIdDest = "Water: Previously Forested Wetland",
                    TransitionTypeId = "Forested Wetland to Water",
                    Probability = 1))

saveDatasheet(myScenario, myData, sheetName)

# Wetland to Water
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways [Wetland to Unvegetated]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_DeterministicTransition"

myData <- datasheet(myScenario, sheetName) %>%
  addRow(data.frame(StateClassIdSource = stateClassTable$Name,
                    Location = paste0("A",1:length(stateClassTable$Name))))
saveDatasheet(myScenario, myData, sheetName, append = F)

sheetName <- "stsim_Transition"

myData <- datasheet(myScenario, sheetName, optional = T) %>%
  addRow(data.frame(StateClassIdSource = c("Wetland: Estuarine Emergent",
                                           "Wetland: Palustrine Emergent"),
                    StateClassIdDest = "Wetland: Unvegetated Emergent",
                    TransitionTypeId = "Emergent Wetland to Unvegetated",
                    Probability = 1)) %>%
  addRow(data.frame(StateClassIdSource = c("Wetland: Palustrine Forested"),
                    StateClassIdDest = "Wetland: Unvegetated Forested",
                    TransitionTypeId = "Forested Wetland to Unvegetated",
                    Probability = 1))

saveDatasheet(myScenario, myData, sheetName)

# Update Flow pathways

myScenario <- scenario(myProject,
                       scenario=vTag("SF Flow Pathways [Base Flows, Add Methane, Add Ag, Add Water]", gwpVariant, style="bracket"),
                       source=vTag("SF Flow Pathways [Base Flows, Add Methane, Add Ag]", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios")

myData <- datasheet(myScenario, name = "stsim_FlowPathway")

myDataExisting <- myData

myDataWater <- myData %>%
  filter(FromStateClassId == "Water: All") %>%
  mutate(FromStateClassId = "Water: Previously Emergent Wetland") %>%
  filter(!(FlowTypeId %in% c("Emission: AG Very Fast -> Atmosphere",
                             "Emission: BG Very Fast -> Atmosphere")))

myDataEmergent <- myData %>%
  filter(FromStateClassId == "Wetland: Palustrine Emergent") %>%
  mutate(FromStateClassId = "Water: Previously Emergent Wetland") %>%
  filter(FlowTypeId %in% c("Atmosphere Temp -> Atmosphere",
                           "Lateral Transport: AG Very Fast -> Aquatic",
                           "Emission: AG Very Fast -> Atmosphere Temp",
                           "Lateral Transport Emergent: BG Slow -> Aquatic",
                           "Emission Emergent: BG Slow -> Atmosphere Temp",
                           "Stabilization Emergent: BG Slow -> Deep Soil",
                           "Lateral Transport: BG Very Fast -> Aquatic",
                           "Emission: BG Very Fast -> Atmosphere Temp")) %>%
  select(-StateAttributeTypeId)

myDataWater <- myDataWater %>%
  addRow(myDataEmergent)

myDataUnVeg <- myDataWater %>%
  mutate(FromStateClassId = "Wetland: Unvegetated Emergent")

saveDatasheet(myScenario, anti_join(myDataWater, myDataExisting), "stsim_FlowPathway", append = TRUE)
saveDatasheet(myScenario, anti_join(myDataUnVeg, myDataExisting), "stsim_FlowPathway", append = TRUE)

myDataForestedWater <- myData %>%
  filter(FromStateClassId == "Wetland: Palustrine Forested") %>%
  mutate(FromStateClassId = "Water: Previously Forested Wetland") %>%
  filter(!(FlowTypeId %in% c("Net Growth Forest: Atmosphere -> Coarse Roots",
                             "Net Growth Forest: Atmosphere -> Fine Roots",
                             "Net Growth Forest: Atmosphere -> Foliage",
                             "Net Growth Forest: Atmosphere -> Merchantable",
                             "Net Growth Forest: Atmosphere -> Other Wood",
                             "Emission: Atmosphere Temp -> Atmosphere: CH4")))

myDataUnVegForested <- myDataForestedWater %>%
  mutate(FromStateClassId = "Wetland: Unvegetated Forested")

saveDatasheet(myScenario, anti_join(myDataForestedWater, myDataExisting), "stsim_FlowPathway", append = TRUE)
saveDatasheet(myScenario, anti_join(myDataUnVegForested, myDataExisting), "stsim_FlowPathway", append = TRUE)

# Calculate the correct partitions between lateral and emissions

siteSummary <- read_csv(paste0(pathInDatasheets,"siteSummaryLatFlux_StockBasedEquilibrium_2025_05_16.csv"))

partAGVF <- siteSummary %>%
  select(AGVFEmission_Amount,AGVFLatTrans_Amount) %>%
  mutate(eAGVF1 = AGVFEmission_Amount/(AGVFEmission_Amount+AGVFLatTrans_Amount),
         lAGVF1 = AGVFLatTrans_Amount/(AGVFEmission_Amount+AGVFLatTrans_Amount)) %>%
  summarize(eAGVF = mean(eAGVF1),
            lAGVF = mean(lAGVF1)) %>%
  select(eAGVF,lAGVF)

partAGVF$eAGVF
partAGVF$lAGVF

partBGVF <- siteSummary %>%
  select(BGVFEmission_Amount,BGVFLatTrans_Amount,BGVFStabilization_Amount) %>%
  mutate(totOut = BGVFEmission_Amount+BGVFLatTrans_Amount+BGVFStabilization_Amount,
         eBGVF1 = BGVFEmission_Amount/totOut,
         lBGVF1 = BGVFLatTrans_Amount/totOut,
         hBGVF1 = BGVFStabilization_Amount/totOut) %>%
  summarize(eBGVF = mean(eBGVF1),
            lBGVF = mean(lBGVF1),
            hBGVF = mean(hBGVF1)) %>%
  select(eBGVF,lBGVF,hBGVF)

partBGVF$eBGVF
partBGVF$lBGVF
partBGVF$hBGVF

partBGS <- siteSummary %>%
  select(BGSEmission_Amount,BGSLatTrans_Amount) %>%
  mutate(totOut = BGSEmission_Amount+BGSLatTrans_Amount,
         eBGS1 = BGSEmission_Amount/totOut,
         lBGS1 = BGSLatTrans_Amount/totOut) %>%
  summarize(eBGS = mean(eBGS1, na.rm = T),
            lBGS = mean(lBGS1, na.rm = T)) %>%
  select(eBGS,lBGS)

partBGS$eBGS
partBGS$lBGS

# Calculate flow multipliers

myScenario <- scenario(myProject, 
                       scenario="SF Flow Multipliers [Emergent Wetland to Water]",
                       folder = "Single-Cell Sub-Scenarios")

flowMultipliers <- read.csv(paste0(pathInDatasheets,"FlowMultipliersLAModel.csv"), stringsAsFactors = F)
names(flowMultipliers) <- gsub("ID","Id",names(flowMultipliers))

flowMultipliersWater <- flowMultipliers %>%
  filter(StateClassId == "Water:All") %>%
  mutate(StateClassId = "Water: Previously Emergent Wetland") %>%
  select(StateClassId,FlowGroupId,Value)
  
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Biomass Turnover: Fine Roots -> BG Very Fast [Type]"] <- 1
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Biomass Turnover: Fine Roots -> AG Very Fast [Type]"] <- 0
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Biomass Turnover: Foliage -> AG Very Fast [Type]"] <- 1
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Decay: BG Very Fast -> BG Slow [Type]"] <- 0.95*partBGVF$hBGVF
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Emission: AG Very Fast -> Atmosphere [Type]"] <- 0
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Emission: BG Very Fast -> Atmosphere [Type]"] <- 0
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Decay: AG Very Fast -> AG Slow [Type]"] <- 0

flowMultipliersWaterAdd <- data.frame(StateClassId = "Water: Previously Emergent Wetland",
                                      FlowGroupId = c("Emission: AG Very Fast -> Atmosphere Temp [Type]",
                                                      "Lateral Transport: AG Very Fast -> Aquatic [Type]"), 
                                      Value = c(0.95*partAGVF$eAGVF,
                                                0.95*partAGVF$lAGVF)) %>%
  addRow(data.frame(StateClassId = "Water: Previously Emergent Wetland",
                    FlowGroupId = c("Emission: BG Very Fast -> Atmosphere Temp [Type]",
                                    "Lateral Transport: BG Very Fast -> Aquatic [Type]"), 
                    Value = c((0.95*partBGVF$eBGVF),
                              (0.95*partBGVF$lBGVF)))) %>%
  addRow(data.frame(StateClassId = "Water: Previously Emergent Wetland",
                    FlowGroupId = c("Emission Emergent: BG Slow -> Atmosphere Temp [Type]",
                                    "Lateral Transport Emergent: BG Slow -> Aquatic [Type]",
                                    "Emission: BG Slow -> Atmosphere [Type]"), 
                    Value = c((0.00264*0.075*partBGS$eBGS),
                              (0.00264*0.075*partBGS$lBGS),
                              0))) %>%
  addRow(data.frame(StateClassId = "Water: Previously Emergent Wetland",
                    FlowGroupId = c("Stabilization Emergent: BG Slow -> Deep Soil [Type]"), 
                    Value = c(0.00264*(1-0.075))))

myData <- flowMultipliersWater %>%
  addRow(flowMultipliersWaterAdd)

myDataUnVeg <- myData %>%
  mutate(StateClassId = "Wetland: Unvegetated Emergent")

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)
saveDatasheet(myScenario, myDataUnVeg, "stsim_FlowMultiplier", append = TRUE)

# Calculate transitions

transitionTypes <- datasheet(myProject,"stsim_TransitionType")

landCoverTypes <- c("Emergent Wetland to Water",
                    "Emergent Wetland to Unvegetated",
                    "Forested Wetland to Water",
                    "Forested Wetland to Unvegetated")

for (i in 1:length(landCoverTypes)){

  # Transition Multipliers: Forest and Wetland
  myScenario <- scenario(myProject, scenario=paste0("Transition Multipliers: ",
                                                    landCoverTypes[i]),
                         folder = "Single-Cell Sub-Scenarios")

  myData <- datasheet(myScenario, "stsim_TransitionMultiplierValue", optional = T, empty = T) %>%
    addRow(data.frame(Timestep = 0,
                      TransitionGroupId = paste0(transitionTypes$Name," [Type]"),
                      Amount = 0)) %>%
    addRow(data.frame(Timestep = c(2020,2021),
                      TransitionGroupId = paste0(landCoverTypes[i], " [Type]"),
                      Amount = c(1,0)))

  saveDatasheet(myScenario, myData, "stsim_TransitionMultiplierValue", append = FALSE)

  rm(myScenario,myData)

}

# sub-scenario: 1000 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2220; 1000 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1000,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2220,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 1000 MC
myScenario <- scenario(myProject,
                       scenario = "Run Control [2001-2220; 1 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>%
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2220,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 100 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2220; 100 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 100,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2220,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 1000 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2800; 100 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 100,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2800,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)


# Add limit
myScenario <- scenario(myProject, 
                       scenario = "Flow Multiplier by Stock [Wetland to Water]",
                       folder = "Single-Cell Sub-Scenarios")

myData <- datasheet(myScenario, "stsim_FlowMultiplierByStock", optional = T, empty = T)

myData <- data.frame(StateClassId = "Water: Previously Emergent Wetland",
                     StockGroupId = "DOM: Belowground Slow [Type]",
                     Multiplier = 0,
                     StockValue = 172.96,#172.9603 #173
                     FlowGroupId = c("Emission Emergent: BG Slow -> Atmosphere Temp [Type]",
                                     "Lateral Transport Emergent: BG Slow -> Aquatic [Type]",
                                     "Stabilization Emergent: BG Slow -> Deep Soil [Type]"),
                     FlowMultiplierTypeId = "Wetland to Water Limit")

myData2 <- data.frame(StateClassId = "Water: Previously Emergent Wetland",
                     StockGroupId = "DOM: Belowground Slow [Type]",
                     Multiplier = 1,
                     StockValue = 172.97,#172.9604 #174
                     FlowGroupId = c("Emission Emergent: BG Slow -> Atmosphere Temp [Type]",
                                     "Lateral Transport Emergent: BG Slow -> Aquatic [Type]",
                                     "Stabilization Emergent: BG Slow -> Deep Soil [Type]"),
                     FlowMultiplierTypeId = "Wetland to Water Limit")

myData3 <- data.frame(StateClassId = "Water: Previously Forested Wetland",
                     StockGroupId = "DOM: Belowground Slow [Type]",
                     Multiplier = 0,
                     StockValue = 172.96,#172.9603 #173
                     FlowGroupId = c("Emission: BG Slow -> Atmosphere Temp [Type]",
                                     "Lateral Transport: BG Slow -> Aquatic [Type]",
                                     "Stabilization: BG Slow -> Deep Soil [Type]"),
                     FlowMultiplierTypeId = "Wetland to Water Limit")

myData4 <- data.frame(StateClassId = "Water: Previously Forested Wetland",
                      StockGroupId = "DOM: Belowground Slow [Type]",
                      Multiplier = 1,
                      StockValue = 172.97,#172.9604 #174
                      FlowGroupId = c("Emission: BG Slow -> Atmosphere Temp [Type]",
                                      "Lateral Transport: BG Slow -> Aquatic [Type]",
                                      "Stabilization: BG Slow -> Deep Soil [Type]"),
                      FlowMultiplierTypeId = "Wetland to Water Limit")

saveDatasheet(myScenario, myData, "stsim_FlowMultiplierByStock", append = FALSE)
saveDatasheet(myScenario, myData2, "stsim_FlowMultiplierByStock", append = TRUE)
saveDatasheet(myScenario, myData3, "stsim_FlowMultiplierByStock", append = TRUE)
saveDatasheet(myScenario, myData4, "stsim_FlowMultiplierByStock", append = TRUE)

rm(myScenario, myData, myData2,myData3,myData4)

# Calculate flow multipliers for IPCC
# All emergent live pools go to dead pools
# Emissions from all DOM pools is 1
# Technically forest live pools (coarse roots, merch, other all decay slower)?
# Think about what to do about this later

myScenario <- scenario(myProject,
                       scenario="SF Flow Multipliers [Emergent Wetland to Water IPCC]",
                       folder = "Single-Cell Sub-Scenarios")

myScenarioOld <- scenario(myProject,
                          scenario = "SF Flow Multipliers [Emergent Wetland to Water]")

myData <- datasheet(myScenarioOld, "stsim_FlowMultiplier")

myDataKeep <- myData %>%
  filter(StateClassId == "Wetland: Unvegetated Emergent")

myData <- myData %>%
  filter(StateClassId == "Water: Previously Emergent Wetland")

flowsZero <- c("Decay: AG Fast -> AG Slow [Type]",
               "Decay: AG Medium -> AG Slow [Type]",
               "Decay: AG Very Fast -> AG Slow [Type]",
               "Decay: BG Fast -> BG Slow [Type]",
               "Decay: BG Very Fast -> BG Slow [Type]",
               "Decay: Snag Branch -> AG Slow [Type]",
               "Decay: Snag Stem -> AG Slow [Type]",
               "Lateral Transport Emergent: BG Slow -> Aquatic [Type]",
               "Lateral Transport: AG Very Fast -> Aquatic [Type]",
               "Lateral Transport: BG Very Fast -> Aquatic [Type]",
               "Stabilization Emergent: BG Slow -> Deep Soil [Type]",
               "Transfer: AG Slow -> BG Slow [Type]",
               "Transfer: Snag Branch -> AG Fast [Type]",
               "Transfer: Snag Stem -> AG Medium [Type]")

flowsOne <- c("Emission Emergent: BG Slow -> Atmosphere Temp [Type]",
              "Emission: AG Fast -> Atmosphere [Type]",
              "Emission: AG Medium -> Atmosphere [Type]",
              "Emission: AG Slow -> Atmosphere [Type]",
              "Emission: AG Very Fast -> Atmosphere Temp [Type]",
              "Emission: BG Very Fast -> Atmosphere Temp [Type]",
              "Emission: BG Fast -> Atmosphere [Type]",
              "Emission: Snag Branch -> Atmosphere [Type]",
              "Emission: Snag Stem -> Atmosphere [Type]",
              "Biomass Turnover: Merchantable -> Snag Stems [Type]")

myData$Value[myData$FlowGroupId %in% flowsZero] <- 0
myData$Value[myData$FlowGroupId %in% flowsOne] <- 1

myData$Value[myData$FlowGroupId %in% c("Biomass Turnover: Coarse Roots -> AG Fast [Type]",
                                       "Biomass Turnover: Coarse Roots -> BG Fast [Type]")] <- 0.5

myData$Value[myData$FlowGroupId %in% c("Biomass Turnover: Other Wood -> AG Fast [Type]")] <- 0.75
myData$Value[myData$FlowGroupId %in% c("Biomass Turnover: Other Wood -> Snag Branches [Type]")] <- 0.25

myData

saveDatasheet(myScenario, myDataKeep, "stsim_FlowMultiplier", append = FALSE)
saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = TRUE)

# to calculate a 40% reduction after 200 years
#0.6A0=A0e^(kt)
#0.6=e^(kt) #Divide both sides by A0
#ln(0.6)=kt #Take the natural logarithm of both sides.
#k=ln(0.6)/200 #Divide by the coefficient
# To get the annual reduction 1-e^(kt), where t = 1, k = -0.002554128
# 0.002550869
# Starting point, there's carbon coming in at each time step, so might not be exactly this number


# Calculate flow multipliers for forested wetland to water

myScenario <- scenario(myProject, 
                       scenario="SF Flow Multipliers [Forested Wetland to Water]",
                       folder = "Single-Cell Sub-Scenarios")

myScenarioOld <- scenario(myProject, 
                          scenario="SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat]")

flowMultipliers <- datasheet(myScenarioOld,"stsim_FlowMultiplier") 

flowMultipliersWater <- flowMultipliers %>%
  filter(StateClassId == "Wetland: Palustrine Forested") %>%
  mutate(StateClassId = "Water: Previously Forested Wetland") %>%
  select(StateClassId,FlowGroupId,Value) %>%
  filter(!(FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                              "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                              "Net Growth Forest: Atmosphere -> Foliage [Type]",
                              "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                              "Net Growth Forest: Atmosphere -> Other Wood [Type]",
                              "Emission: Atmosphere Temp -> Atmosphere: CH4 [Type]")))


flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Biomass Turnover: Fine Roots -> BG Very Fast [Type]"] <- 0.5
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Biomass Turnover: Fine Roots -> AG Very Fast [Type]"] <- 0.5
flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Biomass Turnover: Foliage -> AG Very Fast [Type]"] <- 1

flowMultipliersWaterAGVF <- flowMultipliersWater %>%
  filter(FlowGroupId %in% c("Decay: AG Very Fast -> BG Slow [Type]",
                            "Emission: AG Very Fast -> Atmosphere Temp [Type]",
                            "Lateral Transport: AG Very Fast -> Aquatic [Type]"))

flowMultipliersWaterAGVFm <- 0.95/sum(flowMultipliersWaterAGVF$Value)

flowMultipliersWaterAGVF <- flowMultipliersWaterAGVF %>%
  mutate(Value = Value*flowMultipliersWaterAGVFm)

flowMultipliersWaterBGVF <- flowMultipliersWater %>%
  filter(FlowGroupId %in% c("Decay: BG Very Fast -> BG Slow [Type]",
                            "Emission: BG Very Fast -> Atmosphere Temp [Type]",
                            "Lateral Transport: BG Very Fast -> Aquatic [Type]"))

flowMultipliersWaterBGVFm <- 0.95/sum(flowMultipliersWaterBGVF$Value)

flowMultipliersWaterBGVF <- flowMultipliersWaterBGVF %>%
  mutate(Value = Value*flowMultipliersWaterBGVFm)

flowMultipliersWaterBGS <- flowMultipliersWater %>%
  filter(FlowGroupId %in% c("Emission: BG Slow -> Atmosphere Temp [Type]",
                            "Lateral Transport: BG Slow -> Aquatic [Type]"))

flowMultipliersWaterBGSm <- (0.00325*0.075)/sum(flowMultipliersWaterBGS$Value)

flowMultipliersWaterBGS <- flowMultipliersWaterBGS %>%
  mutate(Value = Value*flowMultipliersWaterBGSm)

flowMultipliersWater$Value[flowMultipliersWater$FlowGroupId == "Stabilization: BG Slow -> Deep Soil [Type]"] <- 0.00325*(1-0.075)

flowMultipliersWaterKeep <- flowMultipliersWater %>%
  filter(!(FlowGroupId %in% c("Decay: AG Very Fast -> BG Slow [Type]",
                              "Emission: AG Very Fast -> Atmosphere Temp [Type]",
                              "Lateral Transport: AG Very Fast -> Aquatic [Type]",
                              "Decay: BG Very Fast -> BG Slow [Type]",
                              "Emission: BG Very Fast -> Atmosphere Temp [Type]",
                              "Lateral Transport: BG Very Fast -> Aquatic [Type]",
                              "Emission: BG Slow -> Atmosphere Temp [Type]",
                              "Lateral Transport: BG Slow -> Aquatic [Type]")))

myData <- flowMultipliersWaterKeep %>%
  addRow(flowMultipliersWaterAGVF) %>%
  addRow(flowMultipliersWaterBGVF) %>%
  addRow(flowMultipliersWaterBGS)

myDataUnVeg <- myData %>%
  mutate(StateClassId = "Wetland: Unvegetated Forested")

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)
saveDatasheet(myScenario, myDataUnVeg, "stsim_FlowMultiplier", append = TRUE)


# Calculate flow multipliers for IPCC
# All live pools go to dead pools
# Emissions from all DOM pools is 1

myScenario <- scenario(myProject, 
                       scenario="SF Flow Multipliers [Forested Wetland to Water IPCC]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "SF Flow Multipliers [Forested Wetland to Water]")

myScenarioOld <- scenario(myProject,
                           scenario = "SF Flow Multipliers [Forested Wetland to Water]")

myData <- datasheet(myScenarioOld, "stsim_FlowMultiplier")

myDataKeep <- myData %>%
  filter(StateClassId == "Wetland: Unvegetated Forested")

myData <- myData %>%
  filter(StateClassId == "Water: Previously Forested Wetland")

flowsDecay <- grep("Decay:",myData$FlowGroupId, value = T)
flowsEmissions <- grep("Emission:",myData$FlowGroupId, value = T)
flowsStab <- grep("Stabilization:",myData$FlowGroupId, value = T)
flowsLat <- grep("Lateral Transport:",myData$FlowGroupId, value = T)
flowsTrans <- grep("Transfer:",myData$FlowGroupId, value = T)

myData$Value[myData$FlowGroupId %in% c("Biomass Turnover: Coarse Roots -> AG Fast [Type]",
                                       "Biomass Turnover: Coarse Roots -> BG Fast [Type]")] <- 0.5

myData$Value[myData$FlowGroupId %in% c("Biomass Turnover: Merchantable -> Snag Stems [Type]")] <- 1

myData$Value[myData$FlowGroupId %in% c("Biomass Turnover: Other Wood -> AG Fast [Type]")] <- 0.75
myData$Value[myData$FlowGroupId %in% c("Biomass Turnover: Other Wood -> Snag Branches [Type]")] <- 0.25

myData$Value[myData$FlowGroupId %in% flowsDecay] <- 0
myData$Value[myData$FlowGroupId %in% flowsEmissions & myData$Value > 0] <- 1
myData$Value[myData$FlowGroupId %in% flowsStab] <- 0
myData$Value[myData$FlowGroupId %in% flowsLat] <- 0
myData$Value[myData$FlowGroupId %in% flowsTrans] <- 0

myData$Value[myData$FlowGroupId %in% c("Emission: AG Slow -> Atmosphere [Type]")] <- 0

myData[,c("StateClassId","FlowGroupId","Value")]

saveDatasheet(myScenario, myDataKeep, "stsim_FlowMultiplier", append = FALSE)
saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = TRUE)
