# ApexRMS
# Updated 2025-03-06
# Run after step2a-ScenarioCreationEmergentForest.R
# to update emergent and forested wetland parameters


library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

outpathDatasheets <- paste0(rootPath,"Data/Datasheets Wetland/Output/")
rootPathUpdatedTables <- paste0(rootPath,"Data/Datasheets Wetland/Emergent/")
carbonDataPath <- paste0(rootPath,"Data/Datasheets Wetland/ForestedWetland/")
pathInDatasheets <- paste0(rootPath,"Data/Datasheets Wetland/")

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")


flowsToZero <- c("Decay: AG Fast -> AG Slow",
                 "Decay: AG Medium -> AG Slow",
                 "Decay: AG Very Fast -> AG Slow",
                 "Decay: Snag Branch -> AG Slow",
                 "Decay: Snag Stem -> AG Slow")

latForest <- c("Lateral Transport: AG Very Fast -> Aquatic",
               "Lateral Transport: BG Slow -> Aquatic",
               "Lateral Transport: BG Very Fast -> Aquatic",
               "Lateral Transport: AG Fast -> Aquatic",
               "Lateral Transport: AG Medium -> Aquatic",
               "Lateral Transport: BG Fast -> Aquatic")

# Initial conditions Mean No Rounding: Emergent Wetland Model

myScenario <- scenario(myProject, 
                       scenario="Init C Stocks at Equilibrium Emergent Wetland [Mean]",
                       folder = "Single-Cell Sub-Scenarios")

myUpdate <- read_csv(paste0(rootPathUpdatedTables,"stsim_StateAttributeValue/stsim_StateAttributeValue Initial C Wetland Emergent Mean.csv"))
names(myUpdate) <- gsub("ID","Id",names(myUpdate))

myUpdate <- myUpdate %>%
  mutate(Value = Value/100)

myOrig <- read_csv(paste0(rootPath, dataPath, "State Attributes/", "State Attributes - Initial Stocks Emergent Wetland.csv"))
names(myOrig) <- gsub("ID","Id",names(myOrig))

wetlandTab <- myOrig %>%
  filter(!(StateAttributeTypeId %in% unique(myUpdate$StateAttributeTypeId)))

# Review, should all be zero, all stocks not computed in script
table(wetlandTab$Value)

wetlandTab <- wetlandTab %>%
  addRow(myUpdate)

write.csv(wetlandTab,
          paste0(outpathDatasheets,"stsim_StateAttributeValue_EmergentWetland_Initial.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeValue_EmergentWetland_Initial.csv"))

myData <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_StateAttributeValue", append = FALSE)

rm(myData,myScenario,myUpdate,myOrig,wetlandTab, myCSV)

myScenario <- scenario(myProject, 
                       scenario="STSM State Attributes [Mean Net Growth, Add Wetland]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "STSM State Attributes [Net Growth]")

myUpdate <- read_csv(paste0(rootPathUpdatedTables,"stsim_StateAttributeValue/stsim_StateAttributeValue NPP Wetland Emergent Mean.csv"))
names(myUpdate) <- gsub("ID","Id",names(myUpdate))

myUpdate <- myUpdate %>%
  mutate(Value = Value/100)

write.csv(myUpdate,
          paste0(outpathDatasheets,"stsim_StateAttributeValue_EmergentWetland_NPP.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeValue_EmergentWetland_NPP.csv"))

myData <- datasheet(myScenario, "stsim_StateAttributeValue") %>%
  filter(!(StateClassId %in% c(myCSV$StateClassId))) %>%
  addRow(myCSV)

myDataPalustrineForested <- myData %>%
  filter(StateClassId == "Forest: Oak/Gum/Cypress Group") %>%
  mutate(StateClassId = "Wetland: Palustrine Forested")

myDataEstuarineForested <- myData %>%
  filter(StateClassId == "Forest: Oak/Gum/Cypress Group") %>%
  mutate(StateClassId = "Wetland: Estuarine Forested")

myData <- myData %>%
  filter(!(StateClassId %in% c("Wetland: Estuarine Forested",
                               "Wetland: Palustrine Forested"))) %>%
  addRow(myDataPalustrineForested) %>%
  addRow(myDataEstuarineForested)

saveDatasheet(myScenario, myData, "stsim_StateAttributeValue", append = FALSE)

rm(myData, myScenario, myUpdate, myCSV, myDataPalustrineForested, myDataEstuarineForested)

# SF Flow Multipliers 

myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Emergent Wetland]",
                       folder = "Single-Cell Sub-Scenarios")

myUpdate <- read_csv(paste0(rootPathUpdatedTables,"stsimsf_FlowMultiplier/stsimsf_FlowMultiplier Wetland Emergent Mean IPCC.csv"))
names(myUpdate) <- gsub("ID","Id",names(myUpdate))

myUpdate$FlowGroupId <- paste0(myUpdate$FlowGroupId, " [Type]")

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
  mutate(StateClassId = "Wetland: Palustrine Emergent") %>%
  select(-TertiaryStratumId)

flowMultipliersAg2 <- flowMultipliersAg %>%
  mutate(StateClassId = "Wetland: Estuarine Emergent")

emergentWetland <- emergentWetland %>%
  filter(!(FlowGroupId %in% flowMultipliersAg$FlowGroupId))

myZeros <- tibble(StateClassId = rep(c("Wetland: Estuarine Emergent",
                                       "Wetland: Palustrine Emergent"), 5),
                  FlowGroupId = rep(c(FlowTypesOld), each = 2),
                  Value = 0)

emergentWetland <- emergentWetland %>%
  addRow(myUpdate) %>%
  addRow(myZeros) %>%
  addRow(flowMultipliersAg) %>%
  addRow(flowMultipliersAg2) %>%
  select(where(~!all(is.na(.x))))


write.csv(emergentWetland,
          paste0(outpathDatasheets,"stsim_FlowMultiplier_EmergentWetland.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_FlowMultiplier_EmergentWetland.csv"))

myData <- datasheet(myScenario,"stsim_FlowMultiplier", optional = T, empty = T) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

rm(myScenario,myData,emergentWetland, myUpdate, myCSV,flowMultipliersAg,flowMultipliersAg2)

# State attribute for Methane Forested Wetland mean
myScenario <- scenario(myProject,
                       scenario="SAV: Methane: Forested and Emergent",
                       folder = "Single-Cell Sub-Scenarios")

# 193.7 kg CH4 ha-1 y-1
# 193.7*(1/1000)*(12.01/16.04)
methaneTab <- tibble(StateClassId = c("Wetland: Palustrine Forested",
                                      "Wetland: Estuarine Forested"),
                     StateAttributeTypeId = "Methane Emissions",
                     Value = c(0.5275))#0.008 (Savannah Site)

# gC per m2 
# 1 ton = 1,000,000 g
# 1 ha = 10,000 m2
#((104.7 + 0.8)/2)/100

methaneTab2 <- read_csv(paste0(rootPathUpdatedTables,"stsim_StateAttributeValue/stsim_StateAttributeValue CH4 Wetland Emergent Mean.csv"))
names(methaneTab2) <- gsub("ID","Id",names(methaneTab2))

methaneTab2 <- methaneTab2 %>%
  mutate(Value = Value/100)

# methaneTab2 <- tibble(StateClassId = c("Wetland: Palustrine Emergent",
#                                        "Wetland: Estuarine Emergent"),
#                       StateAttributeTypeId = "Methane Emissions",
#                       Value = c(0.1450,0.09666667))#0.1450*12/18# 6 saline sites

dataAll <- methaneTab %>%
  addRow(methaneTab2)

write.csv(dataAll,
          paste0(outpathDatasheets,"stsim_StateAttributeValue_CH4.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeValue_CH4.csv"))

myData <- datasheet(myScenario,"stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_StateAttributeValue", append = FALSE)

rm(myScenario,dataAll,methaneTab,methaneTab2,myData,myCSV)


# State attribute for Methane: Forested Wetland [Sav]
myScenario <- scenario(myProject,
                       scenario="SAV: Methane: Forested S",
                       folder = "Single-Cell Sub-Scenarios")

methaneTab <- tibble(StateClassId = c("Wetland: Palustrine Forested",
                                      "Wetland: Estuarine Forested"),
                     StateAttributeTypeId = "Methane Emissions",
                     Value = c(0.008))#0.008 (Savannah Site)

saveDatasheet(myScenario, methaneTab, "stsim_StateAttributeValue", append = FALSE)

rm(myScenario,methaneTab)

# State attribute for Methane: Forested Wetland [W]
myScenario <- scenario(myProject,
                       scenario="SAV: Methane: Forested W",
                       folder = "Single-Cell Sub-Scenarios")

methaneTab <- tibble(StateClassId = c("Wetland: Palustrine Forested",
                                      "Wetland: Estuarine Forested"),
                     StateAttributeTypeId = "Methane Emissions",
                     Value = c(1.047))#0.008 (Savannah Site)

saveDatasheet(myScenario, methaneTab, "stsim_StateAttributeValue", append = FALSE)

rm(myScenario,methaneTab)

# Flow Multipliers Forested Wetland
myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios")

stockFlowPath <- paste0(rootPath, dataPath, "Stock Flow/")
myOrig <- read.csv(paste0(stockFlowPath, "Flow Multipliers - Wetland.csv"))
names(myOrig) <- gsub("ID","Id",names(myOrig))

myOrig <- myOrig %>%
  select(where(~!all(is.na(.x)))) %>%
  filter(StateClassId %in% c("Wetland: Palustrine Forested"))

addFlows <- myOrig %>%
  filter(FlowGroupId %in% paste0(flowsToZero," [Type]")) %>%
  mutate(FlowGroupId = gsub("AG Slow","BG Slow",FlowGroupId))

myOrig$Value[myOrig$FlowGroupId %in% paste0(flowsToZero," [Type]")] <- 0

addFlowsCO2 <- myOrig %>%
  filter(FlowGroupId %in% grep("Emission",FlowGroupId, value = TRUE)) %>%
  mutate(FlowGroupId = gsub("Atmosphere","Atmosphere Temp",FlowGroupId))

emissionsToZero <- grep("Emission",myOrig$FlowGroupId, value = TRUE)
#emissionsToZero <- emissionsToZero[!(emissionsToZero %in% c("Emission: AG Slow -> Atmosphere [Type]"))]

myOrig$Value[myOrig$FlowGroupId %in% emissionsToZero] <- 0

addFlowsLat <- tibble(StateClassId = "Wetland: Palustrine Forested",
                      FlowGroupId = paste0(latForest, " [Type]"),
                      Value = 0)

myDataPalustrine <- myOrig %>%
  addRow(addFlows) %>%
  addRow(addFlowsCO2) %>%
  addRow(addFlowsLat)

myDataEstuarine <- myDataPalustrine %>%
  mutate(StateClassId = "Wetland: Estuarine Forested")

myDataAll <- myDataPalustrine %>%
  addRow(myDataEstuarine)

write.csv(myDataAll,
          paste0(outpathDatasheets,"stsim_FlowMultiplier_ForestedWetland.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_FlowMultiplier_ForestedWetland.csv"))

myData <- datasheet(myScenario, "stsim_FlowMultiplier", optional = TRUE, empty = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

rm(myScenario,myData,myOrig,addFlows,addFlowsCO2,myDataPalustrine,myDataEstuarine, myCSV,addFlowsLat)

# Values for mean model

# Mass remaining for BGVF
decompBGVF <- read.csv(paste0(carbonDataPath,"FSP_Stagg_GCC_Decomp_Decomposition.csv"),
                       stringsAsFactors = F)

decompBGVF$Percent.Mass.Remaining[decompBGVF$Percent.Mass.Remaining == "."] <- NA
decompBGVF$Percent.Mass.Remaining <- as.numeric(decompBGVF$Percent.Mass.Remaining)

# RiverID 1 is S and 2 is W

# Check to make sure these are the same, samples balanced
decompBGVFNew <- decompBGVF %>%
  filter(Sampling.Event == "T6") %>%
  filter(Site.ID == 1) %>%
  group_by(River.ID,Site.ID,Microtopography,Depth) %>%
  summarize(mean = mean(Percent.Mass.Remaining)) %>%
  group_by(River.ID,Site.ID,Microtopography) %>%
  summarize(mean = mean(mean)) %>%
  group_by(River.ID,Site.ID) %>%
  summarize(mean = mean(mean)) %>%
  group_by(River.ID) %>%
  summarize(mean = mean(mean))

as.data.frame(decompBGVFNew)

decompBGVFNew <- decompBGVF %>%
  filter(Sampling.Event == "T6") %>%
  filter(Site.ID == 1) %>%
  group_by(River.ID) %>%
  summarize(mean = mean(Percent.Mass.Remaining))

as.data.frame(decompBGVFNew)

# # Belowground Very Fast equilibrium value for an oak gum cypress forest
# eqBGVF <- 0.1055309
# eqFR <- 2.682043
# eqCR <- 20.74
# eqBGF <- 0.6

# Mass Remaining for palustrine forested sites
massRemainingBGVF <- 0.7320098466 # Savannah 0.6843146
massRemainingAGVF <- 0.246

# Flow Multipliers Forested Wetland
myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "SF Flow Multipliers [Forested Wetland]")

myScenarioOld <- scenario(myProject, scenario = "SF Flow Multipliers [Forested Wetland]")

myData <- datasheet(myScenarioOld, "stsim_FlowMultiplier")

# Calculate new humification and emission rates for BGVF
emissionFlowMultBGVF <- myData %>%
  filter(FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultBGVF <- myData %>%
  filter(FlowGroupId == "Decay: BG Very Fast -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutBGVF <- emissionFlowMultBGVF + transferFlowMultBGVF

transferFlowMultBGVFupdate <- totalOutBGVF * (massRemainingBGVF)
emissionFlowMultBGVFupdate <- totalOutBGVF - transferFlowMultBGVFupdate

emissionFlowMultAGVF <- myData %>%
  filter(FlowGroupId == "Emission: AG Very Fast -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultAGVF <- myData %>%
  filter(FlowGroupId == "Decay: AG Very Fast -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutAGVF <- emissionFlowMultAGVF + transferFlowMultAGVF

transferFlowMultAGVFupdate <- totalOutAGVF * massRemainingAGVF
emissionFlowMultAGVFupdate <- totalOutAGVF - transferFlowMultAGVFupdate

# Scale up BGF humification rate
scalerBG <- transferFlowMultBGVFupdate / transferFlowMultBGVF

emissionFlowMultBGF <- myData %>%
  filter(FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultBGF <- myData %>%
  filter(FlowGroupId == "Decay: BG Fast -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutBGF <- emissionFlowMultBGF + transferFlowMultBGF

transferFlowMultBGFupdate <- scalerBG * transferFlowMultBGF
emissionFlowMultBGFupdate <- totalOutBGF - transferFlowMultBGFupdate

# Scale up AGF humification rate
scalerAG <- transferFlowMultAGVFupdate / transferFlowMultAGVF

emissionFlowMultAGF <- myData %>%
  filter(FlowGroupId == "Emission: AG Fast -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultAGF <- myData %>%
  filter(FlowGroupId == "Decay: AG Fast -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutAGF <- emissionFlowMultAGF + transferFlowMultAGF

transferFlowMultAGFupdate <- scalerAG * transferFlowMultAGF
emissionFlowMultAGFupdate <- totalOutAGF - transferFlowMultAGFupdate

# Scale up AGM humification rate
emissionFlowMultAGM <- myData %>%
  filter(FlowGroupId == "Emission: AG Medium -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultAGM <- myData %>%
  filter(FlowGroupId == "Decay: AG Medium -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutAGM <- emissionFlowMultAGM + transferFlowMultAGM

transferFlowMultAGMupdate <- scalerAG * transferFlowMultAGM
emissionFlowMultAGMupdate <- totalOutAGM - transferFlowMultAGMupdate

# Update flow multiplier table

myData$Value[
  myData$FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultBGVFupdate
myData$Value[
  myData$FlowGroupId == "Decay: BG Very Fast -> BG Slow [Type]"
] <- transferFlowMultBGVFupdate

myData$Value[
  myData$FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultBGFupdate
myData$Value[
  myData$FlowGroupId == "Decay: BG Fast -> BG Slow [Type]"
] <- transferFlowMultBGFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Very Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGVFupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Very Fast -> BG Slow [Type]"
] <- transferFlowMultAGVFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGFupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Fast -> BG Slow [Type]"
] <- transferFlowMultAGFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Medium -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGMupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Medium -> BG Slow [Type]"
] <- transferFlowMultAGMupdate

# soilAge <- (1290+1295)/2
# soilPoolSize <- (628.4 + 127.1)/2
# 
# poolTotal <- soilPoolSize - eqBGVF - eqFR - eqCR - eqBGF
# emissionsInOut <- 1.548114
# 
# burialRate <- soilPoolSize/soilAge
# 
# flowMultBGStoDeep <- burialRate/poolTotal
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm

# Add burial rate of zero as placeholder
addFlowMult <- tibble(StateClassId = c("Wetland: Estuarine Forested",
                                       "Wetland: Palustrine Forested"),
                      FlowGroupId = "Stabilization: BG Slow -> Deep Soil [Type]",
                      Value = 0)

myData <- myData %>%
  addRow(addFlowMult)

# Update foliage turnover rate with deciduous value
myData$Value[myData$FlowGroupId == "Biomass Turnover: Foliage -> AG Very Fast [Type]"] <- 0.95

# Update Net Growth by pool

netGrowth <- myData %>%
  filter(FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                            "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                            "Net Growth Forest: Atmosphere -> Foliage [Type]",
                            "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                            "Net Growth Forest: Atmosphere -> Other Wood [Type]"))

avgFoliageKrauss <- mean(c(3.492,2.838))
avgStemKrauss <- mean(c(1.922,2.035))
avgRootKrauss <- mean(c(2.870,1.370))
avgAboveKrauss <- avgFoliageKrauss+avgStemKrauss

netGrowthCBM66 <- 3.8157

flowMultFoliage <- netGrowth %>%
  filter(StateClassId == "Wetland: Palustrine Forested",
         AgeMin == 66,
         AgeMax == 66,
         FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Foliage [Type]")) %>%
  pull(Value)

flowMultStem <- netGrowth %>%
  filter(StateClassId == "Wetland: Palustrine Forested",
         AgeMin == 66,
         AgeMax == 66,
         FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Merchantable [Type]",
                            "Net Growth Forest: Atmosphere -> Other Wood [Type]")) %>%
  pull(Value)

flowMultRoot <- netGrowth %>%
  filter(StateClassId == "Wetland: Palustrine Forested",
         AgeMin == 66,
         AgeMax == 66,
         FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                            "Net Growth Forest: Atmosphere -> Coarse Roots [Type]")) %>%
  pull(Value)

avgAboveCBM <- netGrowthCBM66*sum(c(flowMultFoliage,flowMultStem))
avgRootCBM <- netGrowthCBM66*sum(flowMultRoot)

avgAboveI <- avgAboveKrauss/avgAboveCBM
avgRootI <- avgRootKrauss/avgRootCBM

addGrowth <- data.frame(FlowGroupId = c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                                        "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                                        "Net Growth Forest: Atmosphere -> Foliage [Type]",
                                        "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                                        "Net Growth Forest: Atmosphere -> Other Wood [Type]"),
                        multiplier = c(avgRootI,
                                       avgRootI,
                                       avgAboveI,
                                       avgAboveI,
                                       avgAboveI))

netGrowth2 <- netGrowth %>%
  left_join(addGrowth, by = join_by(FlowGroupId)) %>%
  mutate(Value = Value*multiplier) %>%
  select(-multiplier)

myData <- myData %>%
  filter(!(FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                              "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                              "Net Growth Forest: Atmosphere -> Foliage [Type]",
                              "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                              "Net Growth Forest: Atmosphere -> Other Wood [Type]"))) %>%
  addRow(netGrowth2)



saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

rm(myScenario,myData)


rm(massRemainingBGVF,emissionFlowMultBGVF,transferFlowMultBGVF,totalOutBGVF,
   transferFlowMultBGVFupdate,emissionFlowMultBGVFupdate,scalerBG,
   emissionFlowMultBGF,transferFlowMultBGF,
   totalOutBGF,transferFlowMultBGFupdate,emissionFlowMultBGFupdate)

# Values for S

# Mass Remaining for palustrine forested sites
massRemainingBGVF <- 0.6843146 # Savannah 0.6843146
# eqBGVFs <- 0.06819236
# eqFRs <- 1.733207
# eqCRs <- 13.37742
# eqBGFs <- 0.3862857

# Flow Multipliers Forested Wetland
myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower S]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "SF Flow Multipliers [Forested Wetland]")

myScenarioOld <- scenario(myProject, scenario = "SF Flow Multipliers [Forested Wetland]")

myData <- datasheet(myScenarioOld, "stsim_FlowMultiplier")

# Calculate new humification and emission rates for BGVF
emissionFlowMultBGVF <- myData %>%
  filter(FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultBGVF <- myData %>%
  filter(FlowGroupId == "Decay: BG Very Fast -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutBGVF <- emissionFlowMultBGVF + transferFlowMultBGVF

transferFlowMultBGVFupdate <- totalOutBGVF * (massRemainingBGVF)
emissionFlowMultBGVFupdate <- totalOutBGVF - transferFlowMultBGVFupdate

# Scale up BGF humification rate
scalerBG <- transferFlowMultBGVFupdate / transferFlowMultBGVF

emissionFlowMultBGF <- myData %>%
  filter(FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultBGF <- myData %>%
  filter(FlowGroupId == "Decay: BG Fast -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutBGF <- emissionFlowMultBGF + transferFlowMultBGF

transferFlowMultBGFupdate <- scalerBG * transferFlowMultBGF
emissionFlowMultBGFupdate <- totalOutBGF - transferFlowMultBGFupdate

# Update flow multiplier table

myData$Value[
  myData$FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultBGVFupdate
myData$Value[
  myData$FlowGroupId == "Decay: BG Very Fast -> BG Slow [Type]"
] <- transferFlowMultBGVFupdate

myData$Value[
  myData$FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultBGFupdate
myData$Value[
  myData$FlowGroupId == "Decay: BG Fast -> BG Slow [Type]"
] <- transferFlowMultBGFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Very Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGVFupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Very Fast -> BG Slow [Type]"
] <- transferFlowMultAGVFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGFupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Fast -> BG Slow [Type]"
] <- transferFlowMultAGFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Medium -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGMupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Medium -> BG Slow [Type]"
] <- transferFlowMultAGMupdate

# soilAge <- 1290
# soilPoolSize <- 628.4
# 
# poolTotal <- soilPoolSize - eqBGVFs - eqFRs - eqCRs - eqBGFs
# emissionsInOut <- 2.085988
# 
# burialRate <- soilPoolSize/soilAge
# 
# flowMultBGStoDeep <- burialRate/poolTotal
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm

addFlowMult <- tibble(StateClassId = c("Wetland: Estuarine Forested",
                                       "Wetland: Palustrine Forested"),
                      FlowGroupId = "Stabilization: BG Slow -> Deep Soil [Type]",
                      Value = 0)

myData <- myData %>%
  addRow(addFlowMult)

# Update foliage turnover rate with deciduous value
myData$Value[myData$FlowGroupId == "Biomass Turnover: Foliage -> AG Very Fast [Type]"] <- 0.95

# Update Net Growth

netGrowth <- myData %>%
  filter(FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                            "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                            "Net Growth Forest: Atmosphere -> Foliage [Type]",
                            "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                            "Net Growth Forest: Atmosphere -> Other Wood [Type]"))

avgFoliageKrauss <- 2.838
avgStemKrauss <- 2.035
avgRootKrauss <- 1.370
avgAboveKrauss <- avgFoliageKrauss+avgStemKrauss

avgAboveI <- avgAboveKrauss/avgAboveCBM
avgRootI <- avgRootKrauss/avgRootCBM

addGrowth <- data.frame(FlowGroupId = c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                                        "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                                        "Net Growth Forest: Atmosphere -> Foliage [Type]",
                                        "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                                        "Net Growth Forest: Atmosphere -> Other Wood [Type]"),
                        multiplier = c(avgRootI,
                                       avgRootI,
                                       avgAboveI,
                                       avgAboveI,
                                       avgAboveI))

netGrowth2 <- netGrowth %>%
  left_join(addGrowth, by = join_by(FlowGroupId)) %>%
  mutate(Value = Value*multiplier) %>%
  select(-multiplier)

myData <- myData %>%
  filter(!(FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                              "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                              "Net Growth Forest: Atmosphere -> Foliage [Type]",
                              "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                              "Net Growth Forest: Atmosphere -> Other Wood [Type]"))) %>%
  addRow(netGrowth2)

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

rm(myScenario,myData)

rm(massRemainingBGVF,emissionFlowMultBGVF,transferFlowMultBGVF,totalOutBGVF,
   transferFlowMultBGVFupdate,emissionFlowMultBGVFupdate,scalerBG,
   emissionFlowMultBGF,transferFlowMultBGF,
   totalOutBGF,transferFlowMultBGFupdate,emissionFlowMultBGFupdate)

# Values for W

# Mass Remaining for palustrine forested sites
massRemainingBGVF <- 0.7797051
# eqBGVFw <- 0.1428744
# eqFRw <- 3.630878
# eqCRw <- 28.02423
# eqBGFw <- 0.8091378

# Flow Multipliers Forested Wetland
myScenario <- scenario(myProject, scenario="SF Flow Multipliers [Forested Wetland BGS Slower W]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "SF Flow Multipliers [Forested Wetland]")

myScenarioOld <- scenario(myProject, scenario = "SF Flow Multipliers [Forested Wetland]")

myData <- datasheet(myScenarioOld, "stsim_FlowMultiplier")

# Calculate new humification and emission rates for BGVF
emissionFlowMultBGVF <- myData %>%
  filter(FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultBGVF <- myData %>%
  filter(FlowGroupId == "Decay: BG Very Fast -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutBGVF <- emissionFlowMultBGVF + transferFlowMultBGVF

transferFlowMultBGVFupdate <- totalOutBGVF * (massRemainingBGVF)
emissionFlowMultBGVFupdate <- totalOutBGVF - transferFlowMultBGVFupdate

# Scale up BGF humification rate
scalerBG <- transferFlowMultBGVFupdate / transferFlowMultBGVF

emissionFlowMultBGF <- myData %>%
  filter(FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

transferFlowMultBGF <- myData %>%
  filter(FlowGroupId == "Decay: BG Fast -> BG Slow [Type]") %>%
  select(Value) %>%
  distinct() %>%
  pull()

totalOutBGF <- emissionFlowMultBGF + transferFlowMultBGF

transferFlowMultBGFupdate <- scalerBG * transferFlowMultBGF
emissionFlowMultBGFupdate <- totalOutBGF - transferFlowMultBGFupdate

# Update flow multiplier table

myData$Value[
  myData$FlowGroupId == "Emission: BG Very Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultBGVFupdate
myData$Value[
  myData$FlowGroupId == "Decay: BG Very Fast -> BG Slow [Type]"
] <- transferFlowMultBGVFupdate

myData$Value[
  myData$FlowGroupId == "Emission: BG Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultBGFupdate
myData$Value[
  myData$FlowGroupId == "Decay: BG Fast -> BG Slow [Type]"
] <- transferFlowMultBGFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Very Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGVFupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Very Fast -> BG Slow [Type]"
] <- transferFlowMultAGVFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Fast -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGFupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Fast -> BG Slow [Type]"
] <- transferFlowMultAGFupdate

myData$Value[
  myData$FlowGroupId == "Emission: AG Medium -> Atmosphere Temp [Type]"
] <- emissionFlowMultAGMupdate
myData$Value[
  myData$FlowGroupId == "Decay: AG Medium -> BG Slow [Type]"
] <- transferFlowMultAGMupdate

# soilAge <- 1295
# soilPoolSize <- 127.1
# 
# poolTotal <- soilPoolSize - eqBGVFw - eqFRw - eqCRw - eqBGFw
# emissionsInOut <- 5.751005
# 
# burialRate <- soilPoolSize/soilAge
# 
# flowMultBGStoDeep <- burialRate/poolTotal
# flowMultBGStoAtm <-  emissionsInOut/poolTotal
# 
# myData$Value[myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"] <- flowMultBGStoAtm

# Add burial rate of 0 as placeholder
addFlowMult <- tibble(StateClassId = c("Wetland: Estuarine Forested",
                                       "Wetland: Palustrine Forested"),
                      FlowGroupId = "Stabilization: BG Slow -> Deep Soil [Type]",
                      Value = 0)

myData <- myData %>%
  addRow(addFlowMult)

# Update foliage turnover rate with deciduous value
myData$Value[myData$FlowGroupId == "Biomass Turnover: Foliage -> AG Very Fast [Type]"] <- 0.95

# Update Net Growth

netGrowth <- myData %>%
  filter(FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                            "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                            "Net Growth Forest: Atmosphere -> Foliage [Type]",
                            "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                            "Net Growth Forest: Atmosphere -> Other Wood [Type]"))

avgFoliageKrauss <- 3.492
avgStemKrauss <- 1.922
avgRootKrauss <- 2.870
avgAboveKrauss <- avgFoliageKrauss+avgStemKrauss

avgAboveI <- avgAboveKrauss/avgAboveCBM
avgRootI <- avgRootKrauss/avgRootCBM

addGrowth <- data.frame(FlowGroupId = c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                                        "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                                        "Net Growth Forest: Atmosphere -> Foliage [Type]",
                                        "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                                        "Net Growth Forest: Atmosphere -> Other Wood [Type]"),
                        multiplier = c(avgRootI,
                                       avgRootI,
                                       avgAboveI,
                                       avgAboveI,
                                       avgAboveI))

netGrowth2 <- netGrowth %>%
  left_join(addGrowth, by = join_by(FlowGroupId)) %>%
  mutate(Value = Value*multiplier) %>%
  select(-multiplier)

myData <- myData %>%
  filter(!(FlowGroupId %in% c("Net Growth Forest: Atmosphere -> Coarse Roots [Type]",
                              "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
                              "Net Growth Forest: Atmosphere -> Foliage [Type]",
                              "Net Growth Forest: Atmosphere -> Merchantable [Type]",
                              "Net Growth Forest: Atmosphere -> Other Wood [Type]"))) %>%
  addRow(netGrowth2)

saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

rm(myScenario,myData)

# Merge

myScenario <- scenario(myProject,
                       scenario="SF Flow Multipliers [Update Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Non Forest]",
                            "SF Flow Multipliers [Emergent Wetland]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")

rm(myScenario)
