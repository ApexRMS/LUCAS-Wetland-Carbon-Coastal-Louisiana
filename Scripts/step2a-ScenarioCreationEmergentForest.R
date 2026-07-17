# ApexRMS
# Updated 2025-03-06
# This script adds scenarios to the step1-LibraryCreation.R
# to update emergent and forested wetland parameters


library(rsyncrosim)
library(tidyverse)

source(paste0(rootPath, "Scripts/gwpConfig.R"))
gwpVariant <- gwpVariants[[activeGWP]]

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


if (activeGWP == "GWP100") {

# Add Flow Type
flowTypes <- read_csv(paste0(rootPathUpdatedTables,"stsimsf_FlowMultiplier/stsimsf_FlowMultiplier Wetland Emergent Mean IPCC.csv"))
flowTypes <- unique(flowTypes$FlowGroupID)

flowTypesEmergent <- grep("Emission",flowTypes, value = T)

flowTypesDecay <- c("Decay: AG Fast -> BG Slow",
                    "Decay: AG Medium -> BG Slow",
                    "Decay: AG Very Fast -> BG Slow",
                    "Decay: Snag Branch -> BG Slow",
                    "Decay: Snag Stem -> BG Slow")

flowTypesForest <- c("Emission: AG Fast -> Atmosphere",
                     "Emission: AG Medium -> Atmosphere",
                     "Emission: AG Slow -> Atmosphere",
                     "Emission: AG Very Fast -> Atmosphere",
                     "Emission: BG Fast -> Atmosphere",
                     "Emission: BG Slow -> Atmosphere",
                     "Emission: BG Very Fast -> Atmosphere",
                     "Emission: Snag Branch -> Atmosphere",
                     "Emission: Snag Stem -> Atmosphere")

flowTypesCH4 <- paste0("Emission: Atmosphere Temp -> Atmosphere: CH4")
flowTypesCO2 <- paste0(flowTypesForest," Temp")
flowTypesAtm <- paste0("Atmosphere Temp -> Atmosphere")

flowTypesUpdate <- c("Emission Emergent: BG Slow -> Atmosphere Temp",
                     "Stabilization Emergent: BG Slow -> Deep Soil",
                     "Lateral Transport Emergent: BG Slow -> Aquatic",
                     "Lateral Transport: AG Fast -> Aquatic",
                     "Lateral Transport: AG Medium -> Aquatic",
                     "Lateral Transport: BG Fast -> Aquatic")

flowTypesAdd <- c(flowTypesDecay,
                  flowTypesCH4,
                  flowTypesCO2,
                  flowTypesAtm,
                  flowTypesUpdate)

write.csv(tibble(Name = flowTypesAdd),paste0(outpathDatasheets,"stsim_FlowType.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_FlowType.csv"))

myData <- datasheet(myProject,"stsim_FlowType", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

myOrig <- datasheet(myProject,"stsim_FlowType")

dup <- myCSV$Name[myCSV$Name %in% myOrig$Name]

myData <- myData[-which(myData$Name %in% dup),]

myData$Name[duplicated(myData$Name)]

saveDatasheet(myProject, myData, "stsim_FlowType", append = TRUE)

rm(myData, myCSV)

# Add a new flow group

flowGroupsAdd <- c("Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)",
                   "Annual Net Ecosystem Carbon Balance (tons C per year)",
                   "Annual Emissions: CH4 (tons C per year)",
                   "Annual Emissions: CH4 (tons CO2-eq per year)",
                   "Annual Lateral Flux (tons C per year)",
                   "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
                   "Annual Emissions: CO2, CO, and CH4 (tons CO2-eq per year)",
                   "Annual Net Growth (tons CO2-eq per year)",
                   "Annual Emissions: CO2 and CH4 (tons C per year)",
                   "Annual Emissions: CO2, CO, and CH4 (tons C per year)",
                   "Annual Emissions: CO2 (tons C per year)",
                   "Annual Emissions: CO2 (tons CO2-eq per year)",
                   "Annual Net Growth (tons C per year)",
                   "Annual Lateral Flux (tons CO2-eq per year)",
                   "Annual Net Flux (tons C per year)",
                   "Annual Net Flux (tons CO2-eq per year)",
                   "Net Growth Wetland Emergent: Total")

write.csv(tibble(Name = flowGroupsAdd),paste0(outpathDatasheets,"stsim_FlowGroup.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_FlowGroup.csv"))

myData <- datasheet(myProject,"stsim_FlowGroup", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myProject, myData, "stsim_FlowGroup", append = TRUE)

rm(myData, myCSV)

# Add Stock Type

write.csv(tibble(Name = "Atmosphere Temp",
                 Description = "Temporary pool used for forested wetland model"),
          paste0(outpathDatasheets,"stsim_StockType.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StockType.csv"))

myData <- datasheet(myProject,"stsim_StockType", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myProject, myData, "stsim_StockType", append = TRUE)

# Add Stock Groups

stockGroupsAdd <- c("Ecosystem Carbon Storage (tons C)",
                    "Cumulative Emissions (tons C)",
                    "Cumulative Emissions (tons CO2-eq)",
                    "Cumulative Emissions: CH4 (tons CO2-eq)",
                    "Cumulative Emissions: CH4 (tons C)",
                    "Cumulative Emissions: CO2 (tons CO2-eq)",
                    "Cumulative Emissions: CO2 (tons C)",
                    "Cumulative Lateral Flux (tons C)",
                    "Cumulative Change in Net Carbon Sequestration (tons CO2-eq)",
                    "Cumulative Change in Net Carbon Sequestration (tons C)")

write.csv(tibble(Name = stockGroupsAdd),
          paste0(outpathDatasheets,"stsim_StockGroup.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StockGroup.csv"))

myData <- datasheet(myProject,"stsim_StockGroup", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myProject, myData, "stsim_StockGroup", append = TRUE)

rm(myCSV,myData)

# Add a new state attribute type

newStateAttributes <- c("Methane Emissions")

write.csv(tibble(Name = newStateAttributes),
          paste0(outpathDatasheets,"stsim_AttributeGroup.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_AttributeGroup.csv"))

myData <- datasheet(myProject,"stsim_AttributeGroup", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myProject, myData, "stsim_AttributeGroup", append = TRUE)

rm(myData,myCSV)

write.csv(tibble(Name = c(newStateAttributes,"Carbon Initial Conditions: Deep Soil"),
                 AttributeGroupId = c(newStateAttributes,"Carbon Initial Conditions")),
          paste0(outpathDatasheets,"stsim_StateAttributeType.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StateAttributeType.csv"))

saveDatasheet(myProject, myCSV, "stsim_StateAttributeType", append = TRUE)

rm(myCSV)

}

# Create sub-scenarios

# Initial Conditions Wetland: Estuarine Emergent
myScenario <- scenario(myProject, 
                       scenario = "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_InitialConditionsNonSpatial"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(TotalAmount = 1,
                    NumCells = 1,
                    CalcFromDist = TRUE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myData, sheetName)

sheetName <- "stsim_InitialConditionsNonSpatialDistribution"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(StratumId = "Study Area",
                    StateClassId = "Wetland: Estuarine Emergent",
                    RelativeAmount = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# Initial Conditions Wetland: Palustrine Emergent
myScenario <- scenario(myProject, 
                       scenario = "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_InitialConditionsNonSpatial"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(TotalAmount = 1,
                    NumCells = 1,
                    CalcFromDist = TRUE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myData, sheetName)

sheetName <- "stsim_InitialConditionsNonSpatialDistribution"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(StratumId = "Study Area",
                    StateClassId = "Wetland: Palustrine Emergent",
                    RelativeAmount = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# Initial Conditions Wetland: Palustrine Forested
myScenario <- scenario(myProject, 
                       scenario = "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 1]",
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
                    AgeMin = 1,
                    AgeMax = 1,
                    RelativeAmount = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# Initial conditions forest Oak Gum Cypress

myScenario <- scenario(myProject, 
                       scenario = "Initial Conditions: Single Cell - Forest: Oak Gum Cypress [Age 1]",
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
                    AgeMin = 1,
                    AgeMax = 1,
                    RelativeAmount = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# To add to model
# Make sure output options have transitions for tabular and spatial for KY-TN, only tabular for LA

# Output Options
myScenario <- scenario(myProject, 
                       scenario = "Output Options [Non-Spatial; Summary]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_OutputOptions"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(SummaryOutputSC = TRUE,
                    SummaryOutputSCTimesteps = 1,
                    SummaryOutputTR = TRUE,
                    SummaryOutputTRTimesteps = 1,
                    SummaryOutputEV = TRUE,
                    SummaryOutputEVTimesteps = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# To add to Model
# For KY/TN and LA will need to update the spatial layers to include a deep soil stock for the
# forested wetland pixels.
# As well as initial stocks for both emergent and forested wetland pixels

# SF Initial Stocks
myScenario <- scenario(myProject, 
                       scenario = "SF Initial Stocks",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_InitialStockNonSpatial"
myCSV <- read_csv(paste0(rootPath,dataPath,"Additional Spinups/Initial Stock - Non Spatial.csv"))
names(myCSV) <- gsub("ID","Id",names(myCSV))
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(myCSV) %>%
  addRow(tibble(StockTypeId = "Deep Soil",
                StateAttributeTypeId = "Carbon Initial Conditions: Deep Soil"))

saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName, myCSV)


# SF Flow Order
myScenario <- scenario(myProject, 
                       scenario = "SF Flow Order [Updated]",
                       folder = "Single-Cell Sub-Scenarios",
                       sourceScenario = "SF Flow Order")

sheetName <- "stsim_FlowOrder"
myData <- datasheet(myScenario, name = sheetName)

flowsToZero <- c("Decay: AG Fast -> AG Slow",
                 "Decay: AG Medium -> AG Slow",
                 "Decay: AG Very Fast -> AG Slow",
                 "Decay: Snag Branch -> AG Slow",
                 "Decay: Snag Stem -> AG Slow")

myDataAddForest <- myData %>%
  filter(FlowTypeId %in% flowsToZero) %>%
  mutate(FlowTypeId = gsub("AG Slow","BG Slow",FlowTypeId)) %>%
  filter(FlowTypeId != "Decay: AG Very Fast -> BG Slow")

myDataAddCO2 <- myData %>%
  filter(FlowTypeId %in% flowTypesForest) %>%
  mutate(FlowTypeId = gsub("Atmosphere","Atmosphere Temp",FlowTypeId))

myDataAddLatCH4 <- tibble(FlowTypeId = c("Emission: Atmosphere Temp -> Atmosphere: CH4"),
                          Order = 6.8)

myDataAddAtm <- tibble(FlowTypeId = c("Atmosphere Temp -> Atmosphere"),
                       Order = 6.9)

myDataAddLat2 <- tibble(FlowTypeId = c("Emission Emergent: BG Slow -> Atmosphere Temp",
                                       "Stabilization Emergent: BG Slow -> Deep Soil",
                                       "Lateral Transport Emergent: BG Slow -> Aquatic",
                                       "Lateral Transport: AG Fast -> Aquatic",
                                       "Lateral Transport: AG Medium -> Aquatic",
                                       "Lateral Transport: BG Fast -> Aquatic"),
                        Order = c(rep(6.6,3),6.5, 4, 6.5))

myData$Order[myData$FlowTypeId == "Stabilization: BG Slow -> Deep Soil"] <- 5
myData$Order[myData$FlowTypeId == "Lateral Transport: BG Slow -> Aquatic"] <- 5

myDataAll <- myData %>%
  bind_rows(myDataAddForest,
            myDataAddCO2,
            myDataAddLatCH4,
            myDataAddAtm,
            myDataAddLat2)

write.csv(myDataAll,
          paste0(outpathDatasheets,"stsim_FlowOrder.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_FlowOrder.csv"))

myData <- datasheet(myScenario,"stsim_FlowOrder", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_FlowOrder", append = FALSE)

rm(myData, sheetName, myCSV, myDataAddForest, myDataAddCO2,
   myDataAddLatCH4, myDataAddAtm, myDataAll,myDataAddLat2)

myData <- datasheet(myScenario, name = "stsim_FlowOrderOptions")

myData$ApplyBeforeTransitions <- FALSE

saveDatasheet(myScenario, myData, "stsim_FlowOrderOptions", append = FALSE)

rm(myScenario,myData)

# Turn off all transitions

myScenario <- scenario(myProject, scenario="Transition Multipliers",
                       folder = "Single-Cell Sub-Scenarios")

transitionTypeGroup <- datasheet(myProject,name = "stsim_TransitionTypeGroup")

transitionTypes <- unique(transitionTypeGroup$TransitionGroupId)
transitionTypes <- grep("Type",transitionTypes, value = T)

myData <- datasheet(myScenario, "stsim_TransitionMultiplierValue", optional = T, empty = T) %>%
  addRow(data.frame(Timestep = 2001,
                    TransitionGroupId = transitionTypes,
                    Amount = 0))

saveDatasheet(myScenario, myData, "stsim_TransitionMultiplierValue", append = FALSE)

rm(myScenario,myData,transitionTypeGroup,transitionTypes)


# Add additional pathways

myScenario <- scenario(myProject,
                       scenario=vTag("SF Flow Pathways [Base Flows, Add Methane]", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios",
                       sourceScenario = "SF Flow Pathways [Base Flows]")

write.csv(tibble(StockTypeId = "Atmosphere Temp",
                 Location = "F6"),
          paste0(outpathDatasheets,"stsim_FlowPathwayDiagram.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_FlowPathwayDiagram.csv"))

myData <- datasheet(myScenario, name = "stsim_FlowPathwayDiagram", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_FlowPathwayDiagram", append = TRUE)

rm(myData, myCSV)

myData <- datasheet(myScenario, name = "stsim_FlowPathway")

flowTypesEmergentEmission <- c("Emission Emergent: BG Slow -> Atmosphere Temp",
                               "Emission: BG Very Fast -> Atmosphere Temp",
                               "Emission: AG Very Fast -> Atmosphere Temp")

flowTypesEmergentDecay <- tibble(FromStateClassId = c("Wetland: Estuarine Emergent",
                                                              "Wetland: Palustrine Emergent"),
                                 FromStockTypeId = "DOM: Aboveground Very Fast",
                                 ToStockTypeId = "DOM: Belowground Slow",
                                 FlowTypeId = "Decay: AG Very Fast -> BG Slow",
                                 Multiplier = 1)
                                                            

flowTypesAddEmergent <- tibble(FromStateClassId = rep(c("Wetland: Estuarine Emergent",
                                                "Wetland: Palustrine Emergent"),
                                                length(flowTypesEmergentEmission)),
                               FlowTypeId = rep(flowTypesEmergentEmission, each = 2),
                               Multiplier = 1) %>%
  separate_wider_delim(FlowTypeId,
                       delim = " -> ",
                       names = c("FromStockTypeId",
                                 "ToStockTypeId"),
                       cols_remove = FALSE) %>%
  mutate(FromStockTypeId = case_when(FromStockTypeId == "Emission: AG Very Fast" ~ "DOM: Aboveground Very Fast",
                                     FromStockTypeId == "Emission Emergent: BG Slow" ~ "DOM: Belowground Slow",
                                     FromStockTypeId == "Emission: BG Very Fast" ~ "DOM: Belowground Very Fast"))


flowTypesAddForest1 <- tibble(FromStateClassId = rep(c("Wetland: Palustrine Forested",
                                                       "Wetland: Estuarine Forested"),
                                                     length(flowTypesDecay)),
                              FlowTypeId = rep(flowTypesDecay, each = 2),
                              Multiplier = 1) %>%
  separate_wider_delim(FlowTypeId, 
                       delim = " -> ", 
                       names = c("FromStockTypeId",
                                 "ToStockTypeId"),
                       cols_remove = FALSE) %>%
  mutate(ToStockTypeId = case_when(ToStockTypeId == "BG Slow" ~ "DOM: Belowground Slow")) %>%
  mutate(FromStockTypeId = case_when(FromStockTypeId == "Decay: AG Fast" ~ "DOM: Aboveground Fast",
                                     FromStockTypeId == "Decay: AG Medium" ~ "DOM: Aboveground Medium",
                                     FromStockTypeId == "Decay: AG Very Fast" ~ "DOM: Aboveground Very Fast",
                                     FromStockTypeId == "Decay: Snag Branch" ~ "DOM: Snag Branch",
                                     FromStockTypeId == "Decay: Snag Stem" ~ "DOM: Snag Stem"))

flowTypesAddForest2 <- myData %>%
  filter(FlowTypeId %in% grep("Stab",FlowTypeId, value = TRUE)) %>%
  mutate(FromStateClassId = gsub("Emergent","Forested",FromStateClassId)) %>%
  select(-StateAttributeTypeId)

flowTypesAddCO2 <- tibble(FromStateClassId = rep(c("Wetland: Palustrine Forested",
                                                   "Wetland: Estuarine Forested"),
                                                 length(flowTypesCO2)),
                          FlowTypeId = rep(flowTypesCO2, each = 2),
                          Multiplier = 1) %>%
  separate_wider_delim(FlowTypeId, 
                       delim = " -> ", 
                       names = c("FromStockTypeId",
                                 "ToStockTypeId"),
                       cols_remove = FALSE) %>%
  mutate(FromStockTypeId = case_when(FromStockTypeId == "Emission: AG Very Fast" ~ "DOM: Aboveground Very Fast",
                                     FromStockTypeId == "Emission: BG Slow" ~ "DOM: Belowground Slow",
                                     FromStockTypeId == "Emission: BG Very Fast" ~ "DOM: Belowground Very Fast",
                                     FromStockTypeId == "Emission: AG Fast" ~ "DOM: Aboveground Fast",
                                     FromStockTypeId == "Emission: AG Medium" ~ "DOM: Aboveground Medium",
                                     FromStockTypeId == "Emission: AG Slow" ~ "DOM: Aboveground Slow",
                                     FromStockTypeId == "Emission: BG Fast" ~ "DOM: Belowground Fast",
                                     FromStockTypeId == "Emission: Snag Branch" ~ "DOM: Snag Branch",
                                     FromStockTypeId == "Emission: Snag Stem" ~ "DOM: Snag Stem"))

flowTypesMethane <- tibble(FromStateClassId = c("Wetland: Palustrine Forested",
                                                "Wetland: Estuarine Forested",
                                                "Wetland: Estuarine Emergent",
                                                "Wetland: Palustrine Emergent"),
                           FromStockTypeId = "Atmosphere Temp",
                           ToStockTypeId = "Atmosphere: CH4",
                           FlowTypeId = "Emission: Atmosphere Temp -> Atmosphere: CH4",
                           StateAttributeTypeId = "Methane Emissions",
                           Multiplier = 1)

latForest <- c("Lateral Transport: AG Very Fast -> Aquatic",
               "Lateral Transport: BG Slow -> Aquatic",
               "Lateral Transport: BG Very Fast -> Aquatic",
               "Lateral Transport: AG Fast -> Aquatic",
               "Lateral Transport: AG Medium -> Aquatic",
               "Lateral Transport: BG Fast -> Aquatic")

flowTypesLateral <- tibble(FromStateClassId = rep(c("Wetland: Palustrine Forested",
                                                "Wetland: Estuarine Forested"),length(latForest)),
                           FromStockTypeId = rep(c("DOM: Aboveground Very Fast",
                                                   "DOM: Belowground Slow",
                                                   "DOM: Belowground Very Fast",
                                                   "DOM: Aboveground Fast",
                                                   "DOM: Aboveground Medium",
                                                   "DOM: Belowground Fast"), each = 2),
                           ToStockTypeId = "Aquatic",
                           FlowTypeId = rep(latForest, each = 2),
                           Multiplier = 1)

flowTypesAtm <- tibble(FromStateClassId = c("Wetland: Palustrine Forested",
                                            "Wetland: Estuarine Forested",
                                            "Wetland: Estuarine Emergent",
                                            "Wetland: Palustrine Emergent"),
                       FromStockTypeId = "Atmosphere Temp",
                       ToStockTypeId = "Atmosphere",
                       FlowTypeId = "Atmosphere Temp -> Atmosphere",
                       Multiplier = 1)

myData$FlowTypeId[myData$FlowTypeId == "Lateral Transport: BG Slow -> Aquatic" &
                    myData$FromStateClassId %in% c("Wetland: Estuarine Emergent",
                                                   "Wetland: Palustrine Emergent")] <- "Lateral Transport Emergent: BG Slow -> Aquatic"
myData$FlowTypeId[myData$FlowTypeId == "Stabilization: BG Slow -> Deep Soil" &
                  myData$FromStateClassId %in% c("Wetland: Estuarine Emergent",
                                                 "Wetland: Palustrine Emergent")] <- "Stabilization Emergent: BG Slow -> Deep Soil"

myDataState <- flowTypesMethane

myDataAll <- flowTypesAddEmergent %>%
  bind_rows(flowTypesEmergentDecay,
            flowTypesAddForest1,
            flowTypesAddForest2,
            flowTypesAddCO2,
            flowTypesLateral,
            flowTypesAtm)

write.csv(myDataAll,
          paste0(outpathDatasheets,"stsim_FlowPathway1.csv"), row.names = FALSE)
write.csv(myDataState,
          paste0(outpathDatasheets,"stsim_FlowPathway2.csv"), row.names = FALSE)

# Add to model
myCSV1 <- read.csv(paste0(outpathDatasheets,"stsim_FlowPathway1.csv"))
myCSV2 <- read.csv(paste0(outpathDatasheets,"stsim_FlowPathway2.csv"))

myData <- myData %>%
  addRow(myCSV1) %>%
  addRow(myCSV2)

saveDatasheet(myScenario, myData, "stsim_FlowPathway", append = FALSE)

rm(myScenario,flowTypesAddEmergent,myData,flowTypesAddForest1,flowTypesAddForest2,flowTypesAddCO2,
   flowTypesMethane,flowTypesLateral,flowTypesAtm,myDataState,myDataAll, myCSV1,myCSV2)

# Update Outputs
myScenario <- scenario(myProject,
                       scenario=vTag("SF Output Options and Filters [Add Methane]", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios",
                       sourceScenario = "SF Output Options and Filters")

myDataOrig <- datasheet(myScenario, name = "stsim_OutputFilterFlows")

myDataAdd <- tibble(FlowGroupId = paste0(flowTypesAdd," [Type]"),
                    Summary = TRUE,
                    Spatial = FALSE,
                    AvgSpatial = FALSE) %>%
  addRow(tibble(FlowGroupId = flowGroupsAdd,
                Summary = TRUE, 
                Spatial = FALSE,
                AvgSpatial = TRUE))

write.csv(myDataAdd,
          paste0(outpathDatasheets,"stsim_OutputFilterFlows.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_OutputFilterFlows.csv"))

myData <- datasheet(myScenario, name = "stsim_OutputFilterFlows", empty = TRUE, optional = TRUE, lookupsAsFactors = F) %>%
  addRow(myCSV) %>%
  filter(!(FlowGroupId %in% myDataOrig$FlowGroupId))

saveDatasheet(myScenario, myData, "stsim_OutputFilterFlows", append = TRUE)

rm(myData,myDataOrig, myDataAdd, myCSV)

myDataOrig <- datasheet(myScenario, name = "stsim_OutputFilterStocks")

myDataAdd <- tibble(StockGroupId = paste0("Atmosphere Temp"," [Type]"),
                    Summary = TRUE,
                    Spatial = FALSE,
                    AvgSpatial = FALSE) %>%
  addRow(tibble(StockGroupId = stockGroupsAdd,
                Summary = TRUE, 
                Spatial = FALSE,
                AvgSpatial = TRUE))

write.csv(myDataAdd,
          paste0(outpathDatasheets,"stsim_OutputFilterStocks.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_OutputFilterStocks.csv"))

myData <- datasheet(myScenario, name = "stsim_OutputFilterStocks", empty = TRUE, optional = TRUE, lookupsAsFactors = F) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_OutputFilterStocks", append = TRUE)

rm(myScenario, myData, myDataAdd, myDataOrig, myCSV)

# Update Outputs
myScenario <- scenario(myProject, 
                       scenario="SF Output Options [All]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_OutputOptionsStockFlow"

myData <- data.frame(SummaryOutputST	= "Yes",
                    SummaryOutputSTTimesteps = 1,
                    SummaryOutputFL	= "Yes",
                    SummaryOutputFLTimesteps = 1,
                    SpatialOutputST	= "Yes",
                    SpatialOutputSTTimesteps = 1, 
                    SpatialOutputFL	= "Yes",
                    SpatialOutputFLTimesteps = 1,
                    AvgSpatialOutputST = "Yes",
                    AvgSpatialOutputSTTimesteps = 1,	
                    AvgSpatialOutputFL = "Yes",
                    AvgSpatialOutputFLTimesteps = 1,
                    SummaryOutputFLOmitFromST = "Yes",
                    SummaryOutputFLOmitToST = "Yes")

saveDatasheet(myScenario, myData, sheetName)

# Calculate new GWP for methane
GWPmethane <- round((16.043/12.011)*gwpVariant$factor,2)

# Update stock flow group membership
myScenario <- scenario(myProject,
                       scenario=vTag("SF Stock and Flow Group Membership [Add Methane]", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios",
                       sourceScenario = "SF Stock and Flow Group Membership")

sheetName <- "stsim_FlowTypeGroupMembership"
myData <- datasheet(myScenario, name = sheetName)

myData[myData$Value %in% c(33.5,-33.5),]

myData$Value[myData$FlowTypeId %in% c("LULC: Emission DOM CH4",
                                      "LULC: Emission Live CH4") &
             myData$Value %in% c(33.5)] <- GWPmethane

myData$Value[myData$FlowTypeId %in% c("LULC: Emission DOM CH4",
                                      "LULC: Emission Live CH4") &
               myData$Value %in% c(-33.5)] <- -GWPmethane

saveDatasheet(myScenario, myData, "stsim_FlowTypeGroupMembership", append = FALSE)

myData <- datasheet(myScenario, name = sheetName)

myDataEmissionsCH4forest <- myData %>%
  filter(FlowTypeId %in% c("Emission: AG Very Fast -> Atmosphere")) %>%
  filter(!FlowGroupId %in% c("AG Very Fast ->","Q10 Fast Flows",
                             "Emission: Total Rh","Emission: Total Rh (CO2e)")) %>%
  mutate(FlowTypeId = case_when(FlowTypeId == "Emission: AG Very Fast -> Atmosphere" ~ "Emission: Atmosphere Temp -> Atmosphere: CH4")) %>%
  mutate(Value = case_when(Value == 1 ~ 1,
                           Value == 3.67 ~ GWPmethane,
                           Value == -3.67 ~ -GWPmethane,
                           Value == -1 ~ -1))

myDataEmissionsCO2forest <- myData %>%
  filter(FlowTypeId %in% c("Emission: AG Very Fast -> Atmosphere")) %>%
  filter(!FlowGroupId %in% c("AG Very Fast ->","Q10 Fast Flows")) %>%
  mutate(FlowTypeId = "Atmosphere Temp -> Atmosphere")

myFlowsForest <- myData %>%
  filter(FlowTypeId %in% flowsToZero) %>%
  mutate(FlowTypeId = gsub("AG Slow","BG Slow",FlowTypeId))

myDataLateralForest <- myData %>%
  filter(FlowTypeId %in% c("Lateral Transport: AG Very Fast -> Aquatic")) %>%
  filter(!FlowGroupId %in% c("AG Very Fast ->")) 

myDataLateralForest1 <- myDataLateralForest %>%
  mutate(FlowTypeId = "Lateral Transport: AG Fast -> Aquatic")

myDataLateralForest2 <- myDataLateralForest %>%
  mutate(FlowTypeId = "Lateral Transport: AG Medium -> Aquatic")

myDataLateralForest3 <- myDataLateralForest %>%
  mutate(FlowTypeId = "Lateral Transport: BG Fast -> Aquatic")

myDataLateralEmergent4 <- myDataLateralForest %>%
  mutate(FlowTypeId = "Lateral Transport Emergent: BG Slow -> Aquatic")
  
myDataEmissionsCO2b <- myData %>%
  filter(FlowTypeId %in% c(flowTypesForest)) %>%
  filter(FlowGroupId %in% c("Q10 Fast Flows","Q10 Slow Flows")) %>%
  mutate(FlowTypeId = paste0(FlowTypeId," Temp"))

myDataAll <- myDataEmissionsCH4forest %>%
  addRow(myDataEmissionsCO2forest) %>%
  addRow(myFlowsForest) %>%
  addRow(myDataLateralForest1) %>%
  addRow(myDataLateralForest2) %>%
  addRow(myDataLateralForest3) %>%
  addRow(myDataLateralEmergent4) %>%
  addRow(myDataEmissionsCO2b)

write.csv(myDataAll,
          paste0(outpathDatasheets,"stsim_FlowTypeGroupMembership1.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_FlowTypeGroupMembership1.csv"))

myData <- datasheet(myScenario, name = "stsim_FlowTypeGroupMembership", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData,"stsim_FlowTypeGroupMembership", append = TRUE)

rm(myData,myCSV,myDataAll, myDataEmissionsCH4forest, 
   myDataEmissionsCO2forest, myFlowsForest, myDataLateralForest,myDataLateralForest1,myDataLateralForest2,myDataLateralForest3,
   myDataLateralEmergent4,myDataEmissionsCO2b)

myData <- datasheet(myScenario, name = "stsim_FlowTypeGroupMembership")


renameStocks <- c("Net Biome Productivity (CO2e)",
                  "Net Biome Productivity",
                  "Lateral Flux",
                  "Lateral Flux (CO2e)",
                  "Emission: Total (CO2e)",
                  "Emission: Total",
                  "Net Growth: Total (CO2e)",
                  "Net Growth: Total")

myFlowsRename <- myData %>%
  filter(FlowGroupId %in% renameStocks) %>%
  mutate(FlowGroupId = case_when(FlowGroupId == "Net Biome Productivity (CO2e)" ~ "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)",
                                 FlowGroupId == "Net Biome Productivity" ~ "Annual Net Ecosystem Carbon Balance (tons C per year)",
                                 FlowGroupId == "Lateral Flux" ~ "Annual Lateral Flux (tons C per year)",
                                 FlowGroupId == "Lateral Flux (CO2e)" ~ "Annual Lateral Flux (tons CO2-eq per year)",
                                 FlowGroupId == "Emission: Total (CO2e)" ~ "Annual Emissions: CO2, CO, and CH4 (tons CO2-eq per year)",
                                 FlowGroupId == "Emission: Total" ~ "Annual Emissions: CO2, CO, and CH4 (tons C per year)",
                                 FlowGroupId == "Net Growth: Total (CO2e)" ~ "Annual Net Growth (tons CO2-eq per year)",
                                 FlowGroupId == "Net Growth: Total" ~ "Annual Net Growth (tons C per year)"))

myFlowsNetMethane <- myData %>%
  filter(FlowGroupId %in% c("Emission: Total (CO2e)",
                            "Emission: Total")) %>%
  filter(FlowTypeId %in% grep("CH4", FlowTypeId, value = TRUE)) %>%
  mutate(FlowGroupId = case_when(FlowGroupId == "Emission: Total (CO2e)" ~ "Annual Emissions: CH4 (tons CO2-eq per year)",
                                 FlowGroupId == "Emission: Total" ~ "Annual Emissions: CH4 (tons C per year)"))

myDataAdd1 <- myData %>% 
  filter(FlowGroupId == "Emission: Total") %>%
  mutate(FlowGroupId = "Annual Emissions: CO2 (tons C per year)") %>%
  filter(!(FlowTypeId %in% grep("CH4", FlowTypeId, value = TRUE))) %>%
  filter(!(FlowTypeId %in% grep("CO$", FlowTypeId, value = TRUE)))

myDataAdd2 <- myData %>% 
  filter(FlowGroupId == "Emission: Total (CO2e)") %>%
  mutate(FlowGroupId = "Annual Emissions: CO2 (tons CO2-eq per year)") %>%
  filter(!(FlowTypeId %in% grep("CH4", FlowTypeId, value = TRUE))) %>%
  filter(!(FlowTypeId %in% grep("CO$", FlowTypeId, value = TRUE)))

myDataAdd3 <- myData %>% 
  filter(FlowGroupId == "Net Biome Productivity") %>%
  mutate(FlowGroupId = "Annual Net Flux (tons C per year)") %>%
  mutate(Value = Value*-1)

myDataAdd4 <- myData %>% 
  filter(FlowGroupId == "Net Biome Productivity (CO2e)") %>%
  mutate(FlowGroupId = "Annual Net Flux (tons CO2-eq per year)") %>%
  mutate(Value = Value*-1)

myDataAdd5 <- data.frame(FlowTypeId = c("Net Growth Wetland Emergent: Atmosphere -> Fine Roots",
                                        "Net Growth Wetland Emergent: Atmosphere -> Foliage"),
                         FlowGroupId = "Net Growth Wetland Emergent: Total",
                         Value = 1)

myDataAdd6 <- myData %>% 
  filter(FlowGroupId == "Emission: Total") %>%
  mutate(FlowGroupId = "Annual Emissions: CO2 and CH4 (tons C per year)") %>%
  filter(!(FlowTypeId %in% grep("CO$", FlowTypeId, value = TRUE)))

myDataAdd7 <- myData %>% 
  filter(FlowGroupId == "Emission: Total (CO2e)") %>%
  mutate(FlowGroupId = "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)") %>%
  filter(!(FlowTypeId %in% grep("CO$", FlowTypeId, value = TRUE)))

Q10slow <- myData %>%
  filter(FlowGroupId == "Q10 Slow Flows")

Q10fast <- myData %>%
  filter(FlowGroupId == "Q10 Fast Flows")

myDataAdd8 <- myData %>%
  filter(FlowGroupId == "Lateral Flux") %>%
  filter(!(FlowTypeId %in% c("Lateral Transport Emergent: BG Slow -> Aquatic",
                              "Lateral Transport: BG Slow -> Aquatic"))) %>%
  mutate(FlowGroupId = case_when(FlowTypeId == "Lateral Transport: AG Fast -> Aquatic" ~ "Q10 Slow Flows",
                                 FlowTypeId == "Lateral Transport: AG Medium -> Aquatic" ~ "Q10 Slow Flows",
                                 FlowTypeId == "Lateral Transport: AG Very Fast -> Aquatic" ~ "Q10 Fast Flows",
                                 FlowTypeId == "Lateral Transport: BG Fast -> Aquatic" ~ "Q10 Slow Flows",
                                 FlowTypeId == "Lateral Transport: BG Very Fast -> Aquatic" ~ "Q10 Slow Flows"))

myDataAll <- myFlowsRename %>%
  addRow(myFlowsNetMethane) %>%
  addRow(myDataAdd1) %>%
  addRow(myDataAdd2) %>%
  addRow(myDataAdd3) %>%
  addRow(myDataAdd4) %>%
  addRow(myDataAdd5) %>%
  addRow(myDataAdd6) %>%
  addRow(myDataAdd7) %>%
  addRow(myDataAdd8)

write.csv(myDataAll,
          paste0(outpathDatasheets,"stsim_FlowTypeGroupMembership2.csv"), row.names = FALSE)

# Add to model
myCSV2 <- read.csv(paste0(outpathDatasheets,"stsim_FlowTypeGroupMembership2.csv"))

myData <- datasheet(myScenario, name = "stsim_FlowTypeGroupMembership", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV2)

saveDatasheet(myScenario, myData, "stsim_FlowTypeGroupMembership", append = TRUE)

rm(myData, sheetName, myFlowsRename, myFlowsNetMethane, myDataAll,
   myCSV2,myDataAdd1,myDataAdd2,myDataAdd3,myDataAdd4,myDataAdd5,myDataAdd6,myDataAdd7)

# Update stock group membership

myData <- datasheet(myScenario, name = "stsim_StockTypeGroupMembership")

myStocksRename <- myData %>%
  filter(StockGroupId == "Total Ecosystem") %>%
  mutate(StockGroupId = "Ecosystem Carbon Storage (tons C)")

myStocksLat <- tibble(StockTypeId = "Aquatic",
                      StockGroupId = "Cumulative Lateral Flux (tons C)")

myStocksCH4 <- tibble(StockTypeId = "Atmosphere: CH4",
                      StockGroupId = c("Cumulative Emissions: CH4 (tons C)",
                                       "Cumulative Emissions: CH4 (tons CO2-eq)"),
                      Value = c(1,GWPmethane))

myStocksCO2 <- tibble(StockTypeId = rep(c("Atmosphere: CO2",
                                          "Atmosphere"),each = 2),
                      StockGroupId = rep(c("Cumulative Emissions: CO2 (tons C)",
                                           "Cumulative Emissions: CO2 (tons CO2-eq)"),2),
                      Value = c(1,3.67,1,3.67))

myStocksAll <- tibble(StockTypeId = rep(c("Atmosphere",
                                          "Atmosphere: CH4",
                                          "Atmosphere: CO",
                                          "Atmosphere: CO2"),each = 2),
                      StockGroupId = rep(c("Cumulative Emissions (tons C)",
                                           "Cumulative Emissions (tons CO2-eq)"),4),
                      Value = c(1,3.67,1,GWPmethane,1,4.66,1,3.67))

myDataAll <- myStocksAll %>%
  addRow(myStocksLat) %>%
  addRow(myStocksCH4) %>%
  addRow(myStocksCO2) %>%
  addRow(myStocksRename)

myDataAdd1 <- myDataAll %>% 
  filter(StockGroupId == "Cumulative Emissions (tons C)") %>%
  mutate(StockGroupId = "Cumulative Change in Net Carbon Sequestration (tons C)") %>%
  addRow(tibble(StockTypeId = "Aquatic",
                StockGroupId = "Cumulative Change in Net Carbon Sequestration (tons C)",
                Value = 1)) %>%
  addRow(tibble(StockTypeId = "Forestry Sector",
                StockGroupId = "Cumulative Change in Net Carbon Sequestration (tons C)",
                Value = 1))

myDataAdd2 <- myDataAll %>% 
  filter(StockGroupId == "Cumulative Emissions (tons CO2-eq)") %>%
  mutate(StockGroupId = "Cumulative Change in Net Carbon Sequestration (tons CO2-eq)") %>%
  addRow(tibble(StockTypeId = "Aquatic",
                StockGroupId = "Cumulative Change in Net Carbon Sequestration (tons CO2-eq)",
                Value = 3.67)) %>%
  addRow(tibble(StockTypeId = "Forestry Sector",
                StockGroupId = "Cumulative Change in Net Carbon Sequestration (tons CO2-eq)",
                Value = 3.67))

myDataAll <- myDataAll %>%
  addRow(myDataAdd1) %>%
  addRow(myDataAdd2)

write.csv(myDataAll,
          paste0(outpathDatasheets,"stsim_StockTypeGroupMembership.csv"), row.names = FALSE)

# Add to model
myCSV <- read.csv(paste0(outpathDatasheets,"stsim_StockTypeGroupMembership.csv"))

myData <- datasheet(myScenario, name = "stsim_StockTypeGroupMembership", empty = TRUE, optional = TRUE) %>%
  addRow(myCSV)

saveDatasheet(myScenario, myData, "stsim_StockTypeGroupMembership", append = TRUE)

rm(myScenario, myData, myCSV, myStocksRename, myStocksLat, myStocksCH4, myStocksCO2, myStocksAll, myDataAll,myDataAdd1,myDataAdd2)


