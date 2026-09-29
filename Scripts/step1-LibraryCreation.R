# ApexRMS
# Updated 2025-03-06
# This script creates a LUCAS library using definitions from USGS model
# Adapted from Ben Sleeter's Build LUCAS Model.rmd script

library(rsyncrosim)
library(tidyverse)

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

primaryStrata <- "Study Area"
primaryStrataID <- 1

secondaryStrataOn <- FALSE

secondaryStrata <- NA
secondaryStrataID <- NA

modelPath <- "Models/"
dataPath <- "Data/"

definitionsPath <- paste0(rootPath, dataPath, "Definitions/")
stockFlowPath <- paste0(rootPath, dataPath, "Stock Flow/")

#modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName) 


## Create empty LUCAS library and project and enables the stock flow add-on

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"), 
                         package = "stsim", 
                         session = mySession)
myProject <- rsyncrosim::project(myLibrary, project="Definitions")


## Create Folder Structure within SyncroSim

# Create new folders
folderSF <- folder(ssimObject = myProject, folder = "1. Stock Flow")
folderSFSub <- folder(ssimObject = myProject, folder = "SF Sub-Scenarios", parentFolder = folderSF)

folderSTSM <- folder(ssimObject = myProject, folder = "2. STSM")
folderSTSMSub <- folder(ssimObject = myProject, folder = "STSM Sub-Scenarios", parentFolder = folderSTSM)

folderScenarios <- folder(myProject, folder = "3. Single-Cell Scenarios")

folderSpinupSub <- folder(ssimObject = myProject, folder = "Single-Cell Sub-Scenarios", parentFolder = folderScenarios)
folderSpinupSubMerged <- folder(ssimObject = myProject, folder = "Single-Cell Sub-Scenarios Merged", parentFolder = folderSpinupSub)

# Get a list of existing folders
folder(ssimObject = myLibrary)


## Define the terminology used in the model (both STSM and SF)
sheetName <- "stsim_Terminology"
csvName <- "Terminology.csv"
myData <- read.csv(paste0(definitionsPath, csvName))

myData$AmountUnits[myData$AmountUnits == "Hectares"] <- "hectares"

#saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_Terminology"
csvName <- "Terminology SF.csv"
myData2 <- read.csv(paste0(definitionsPath, csvName))

myData <- cbind(myData,myData2)

saveDatasheet(myProject, myData, sheetName)


## Define the Strata

# Primary Strata
sheetName <- "stsim_Stratum"
myData <- datasheet(myProject, sheetName, optional = T)

# ApexRMS changed to primaryStrata and primaryStrataID to generalize these column names
myData <- rsyncrosim::addRow(myData, data.frame(Name = unique(primaryStrata), Id = unique(primaryStrataID)))
saveDatasheet(myProject, myData, sheetName)

# ApexRMS added if else statement and updated column names to secondaryStrata and secondaryStrataID
if (secondaryStrataOn == TRUE){
  
  # Secondary Strata
  sheetName <- "stsim_SecondaryStratum"
  myData <- datasheet(myProject, sheetName, optional = T)
  myData <- rsyncrosim::addRow(myData, data.frame(Name = unique(secondaryStrata), Id = unique(secondaryStrataID)))
  saveDatasheet(myProject, myData, sheetName)
  
}


## Define the State Class Types

sheetName <- "stsim_StateLabelX"
csvName <- "LULC.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_StateLabelY"
csvName <- "Subclass.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_StateClass"
csvName <- "State Class.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))

myData <- myData %>%
  addRow(data.frame(Name = "Wetland: Unconsolidated Shore",
                    StateLabelXId = "Wetland",
                    StateLabelYId = "Unconsolidated Shore",
                    Id = 97,
                    Color = "255,0,242,242"))

saveDatasheet(myProject, myData, sheetName)

## Define the Transition types and groups

sheetName <- "stsim_TransitionType"
csvName <- "Transition Type.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
# # ApexRMS added a transition type "Dam installation: Wetland Palustrine Forested to Water"
# 
# myData <- myData %>% 
#             addRow(data.frame(Name = "Dam installation: Wetland Palustrine Forested to Water",
#                               ID = 1001))

saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_TransitionGroup"
csvName <- "Transition Group.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_TransitionTypeGroup"
csvName <- "Transition Types by Group.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_TransitionSimulationGroup"
csvName <- "Transition Simulation Groups.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myProject, myData, sheetName)


## Define Age types and groups
sheetName <- "stsim_AgeType"
csvName <- "Age Types.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_AgeGroup"
csvName <- "Age Groups.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)


## Define State Attribute types and groups, Distribution types, and External Variable types.

sheetName <- "stsim_AttributeGroup"
csvName <- "Attribute Group.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_StateAttributeType"
csvName <- "State Attribute Type.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "core_DistributionType"
csvName <- "Distributions.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "core_ExternalVariableType"
csvName <- "External Variables.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)


## Define Carbon Stock Flow types and groups

sheetName <- "stsim_StockType"
csvName <- "Stock Type.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_StockGroup"
csvName <- "Stock Group.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_FlowType"
csvName <- "Flow Type.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_FlowGroup"
csvName <- "Flow Group.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_FlowMultiplierType"
csvName <- "Flow Multiplier Type.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)


# Stock Flow Model

## Write Initial Stocks Sub-scenario


## Load flow pathways diagrams

# Base Flows
myScenario <- scenario(myProject, "SF Flow Pathways [Base Flows]")
folderId(myScenario) <- folderId(folderSFSub)

sheetName <- "stsim_FlowPathwayDiagram"
csvName <- "Flow Pathway Diagram.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_FlowPathway"
csvName <- "Flow Pathways - Base Flows.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Transition-triggered flows
myScenario <- scenario(myProject, "SF Flow Pathways [Event Flows]")
folderId(myScenario) <- folderId(folderSFSub)

sheetName <- "stsim_FlowPathwayDiagram"
csvName <- "Flow Pathway Diagram.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_FlowPathway"
csvName <- "Flow Pathways - Event Flows.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Merge SF pathway diagrams into single sub-scenario
myScenario <- scenario(myProject, "SF Flow Pathways")
folderId(myScenario) <- folderId(folderSF)
mergeDependencies(myScenario) <- TRUE
dependency(myScenario) <- c("SF Flow Pathways [Event Flows]",
                            "SF Flow Pathways [Base Flows]")


## Define Flow Multipliers sub-scenarios

myScenario <- scenario(myProject, "SF Flow Multipliers [Forest]")
folderId(myScenario) <- folderId(folderSFSub)
sheetName <- "stsim_FlowMultiplier"
csvName <- "Flow Multipliers - Forest.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

myScenario <- scenario(myProject, "SF Flow Multipliers [Grassland Shrubland]")
folderId(myScenario) <- folderId(folderSFSub)
sheetName <- "stsim_FlowMultiplier"
csvName <- "Flow Multipliers - Grassland Shrubland.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

myScenario <- scenario(myProject, "SF Flow Multipliers [Wetland]")
folderId(myScenario) <- folderId(folderSFSub)
sheetName <- "stsim_FlowMultiplier"
csvName <- "Flow Multipliers - Wetland.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

myScenario <- scenario(myProject, "SF Flow Multipliers [Non Forest]")
folderId(myScenario) <- folderId(folderSFSub)
sheetName <- "stsim_FlowMultiplier"
csvName <- "Flow Multipliers - Non Forest.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

myScenario <- scenario(myProject, "SF Flow Multipliers [Net Growth Uncertainty]")
folderId(myScenario) <- folderId(folderSFSub)
sheetName <- "stsim_FlowMultiplier"
csvName <- "Flow Multipliers - Net Growth Uncertainty.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Note: Only used when using spatial climate data as flow spatial multipliers. Spatial flow multipliers area stored as 8-bit integers and the scalar is used to convert back to floating point values. This scenario MUST be added as a dependency. For purposes of this script, it is assumed the user has Spatial Flow Multipliers. 
myScenario <- scenario(myProject, "SF Flow Multipliers [Scalar]")
folderId(myScenario) <- folderId(folderSFSub)
sheetName <- "stsim_FlowMultiplier"
csvName <- "Flow Multipliers - Scalar.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Merge SF pathway diagrams into single sub-scenario without net growth uncertainty.
myScenario <- scenario(myProject, "SF Flow Multipliers")
folderId(myScenario) <- folderId(folderSF)
mergeDependencies(myScenario) <- TRUE
dependency(myScenario) <- c("SF Flow Multipliers [Scalar]",
                            "SF Flow Multipliers [Net Growth Uncertainty]",
                            "SF Flow Multipliers [Non Forest]",
                            "SF Flow Multipliers [Wetland]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")

# Merge SF pathway diagrams into single sub-scenario without net growth uncertainty.
myScenario <- scenario(myProject, "SF Flow Multipliers [No Scalar]")
folderId(myScenario) <- folderId(folderSF)
mergeDependencies(myScenario) <- TRUE
dependency(myScenario) <- c("SF Flow Multipliers [Net Growth Uncertainty]",
                            "SF Flow Multipliers [Non Forest]",
                            "SF Flow Multipliers [Wetland]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")


## Define Flow Order

myScenario <- scenario(myProject, "SF Flow Order")
folderId(myScenario) <- folderId(folderSF)
sheetName <- "stsim_FlowOrder"
csvName <- "Flow Order.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_FlowOrderOptions"
myData <- datasheet(myScenario, sheetName)
myData <- rsyncrosim::addRow(myData, data.frame(ApplyBeforeTransitions = FALSE, ApplyEquallyRankedSimultaneously = TRUE))
saveDatasheet(myScenario, myData, sheetName)


## Define Flow Group and Stock Group Membership

myScenario <- scenario(myProject, "SF Stock and Flow Group Membership")
folderId(myScenario) <- folderId(folderSF)

sheetName <- "stsim_FlowTypeGroupMembership"
csvName <- "Flow Type-Group Membership.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_StockTypeGroupMembership"
csvName <- "Stock Type-Group Membership.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)


## Define Stock and Flow Output Options and Filters
myScenario <- scenario(myProject, "SF Output Options and Filters")
folderId(myScenario) <- folderId(folderSF)

sheetName <- "stsim_OutputOptionsStockFlow"
csvName <- "SF Output Options.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_OutputFilterStocks"
csvName <- "Filter Stock Output.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_OutputFilterFlows"
csvName <- "Filter Flow Output.csv"
myData <- read.csv(paste0(stockFlowPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)



# State and Transition Model

## Run Control

myScenario <- scenario(myProject, scenario = "STSM Run Control [2001-2021; 40MC; Spatial]")
folderId(myScenario) <- folderId(folderSTSM)
sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, sheetName)
myData <- data.frame(MaximumIteration = 40,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2021,
                    IsSpatial = TRUE)
saveDatasheet(myScenario, myData, sheetName)


## Output Options

outputOptionsPath <- paste0(rootPath, dataPath, "Output Options/")

myScenario <- scenario(myProject, scenario = "STSM Output Options")
folderId(myScenario) <- folderId(folderSTSM)

sheetName <- "stsim_OutputOptions"
csvName <- "Output Options.csv"
myData <- read.csv(paste0(outputOptionsPath, csvName))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_OutputFilterTransitionGroups"
csvName <- "Filter Transition Group Output.csv"
myData <- read.csv(paste0(outputOptionsPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_OutputOptionsSpatial"
csvName <- "Output Options Spatial.csv"
myData <- read.csv(paste0(outputOptionsPath, csvName))
saveDatasheet(myScenario, myData, sheetName)

sheetName <- "stsim_OutputOptionsSpatialAverage"
csvName <- "Output Options Spatial Averages.csv"
myData <- read.csv(paste0(outputOptionsPath, csvName))
saveDatasheet(myScenario, myData, sheetName)


## Transition Pathways
transitionPathwaysPath <- paste0(rootPath, dataPath, "Transition Pathways/")

# Urbanization
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways [Urbanization]")
folderId(myScenario) <- folderId(folderSTSMSub)

sheetName <- "stsim_DeterministicTransition"
csvName <- "States.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)
sheetName <- "stsim_Transition"
csvName <- "Transitions - Urbanization.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Intensification
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways [Intensification]")
folderId(myScenario) <- folderId(folderSTSMSub)

sheetName <- "stsim_DeterministicTransition"
csvName <- "States.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)
sheetName <- "stsim_Transition"
csvName <- "Transitions - Intensification.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Harvest
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways [Harvest]")
folderId(myScenario) <- folderId(folderSTSMSub)

sheetName <- "stsim_DeterministicTransition"
csvName <- "States.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)
sheetName <- "stsim_Transition"
csvName <- "Transitions - Harvest.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Fire
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways [Fire]")
folderId(myScenario) <- folderId(folderSTSMSub)

sheetName <- "stsim_DeterministicTransition"
csvName <- "States.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)
sheetName <- "stsim_Transition"
csvName <- "Transitions - Fire.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Ag Expansion
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways [Ag Expansion]")
folderId(myScenario) <- folderId(folderSTSMSub)

sheetName <- "stsim_DeterministicTransition"
csvName <- "States.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)
sheetName <- "stsim_Transition"
csvName <- "Transitions - Ag Expansion.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Ag Contraction
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways [Ag Contraction]")
folderId(myScenario) <- folderId(folderSTSMSub)

sheetName <- "stsim_DeterministicTransition"
csvName <- "States.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)
sheetName <- "stsim_Transition"
csvName <- "Transitions - Ag Contraction.csv"
myData <- read.csv(paste0(transitionPathwaysPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Merge Scenarios
myScenario <- scenario(myProject, scenario = "STSM Transition Pathways")
folderId(myScenario) <- folderId(folderSTSM)

mergeDependencies(myScenario) <- T
dependency(myScenario) <- c("STSM Transition Pathways [Ag Contraction]",
                            "STSM Transition Pathways [Ag Expansion]",
                            "STSM Transition Pathways [Fire]",
                            "STSM Transition Pathways [Harvest]",
                            "STSM Transition Pathways [Intensification]",
                            "STSM Transition Pathways [Urbanization]")



## Adjacency

adjacencyPath <- paste0(rootPath, dataPath, "Adjacency/")

myScenario <- scenario(myProject, scenario = "STSM Adjacency")
folderId(myScenario) <- folderId(folderSTSM)

# Adjacency Settings
sheetName <- "stsim_TransitionAdjacencySetting"
csvName <- "Transition Adjacency Setting.csv"
myData <- read.csv(paste0(adjacencyPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

# Adjacency Multipliers
sheetName <- "stsim_TransitionAdjacencyMultiplier"
csvName <- "Transition Adjacency Multipliers.csv"
myData <- read.csv(paste0(adjacencyPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)


## External Variables

extvarPath <- paste0(rootPath, dataPath, "External Variables/")

myScenario <- scenario(myProject, scenario = "STSM External Variables")
folderId(myScenario) <- folderId(folderSTSM)

sheetName <- "core_ExternalVariableValue"
csvName <- "External Variables.csv"
myData <- read.csv(paste0(extvarPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)


## State Attributes

stateAttributesPath <- paste0(rootPath, dataPath, "State Attributes/")

myScenario <- scenario(myProject, scenario = "STSM State Attributes [Net Growth]")
folderId(myScenario) <- folderId(folderSTSM)

sheetName <- "stsim_StateAttributeValue"

csvName <- "State Attribute Values - Net Growth Forest.csv"
myData1 <- read.csv(paste0(stateAttributesPath, csvName))

csvName <- "State Attribute Values - Net Growth Grassland Shrubland.csv"
myData2 <- read.csv(paste0(stateAttributesPath, csvName))

csvName <- "State Attribute Values - Net Growth Wetland.csv"
myData3 <- read.csv(paste0(stateAttributesPath, csvName))

myData <- bind_rows(myData1, myData2, myData3)
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myScenario, myData, sheetName)

