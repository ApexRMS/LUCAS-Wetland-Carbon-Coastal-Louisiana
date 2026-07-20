# ApexRMS
# Updated 2025-02-25
# Run after step3d-AddWaterPrevWetland.R
# This script creates final single-cell wetland scenarios with no transitions
# And with a transition from wetland to water

library(rsyncrosim)
library(tidyverse)

source(paste0(rootPath, "Scripts/gwpConfig.R"))
gwpVariant <- gwpVariants[[activeGWP]]

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

numberOfJobs <- 3

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

# sub-scenario: 1 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2100; 1 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2100,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 1 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2124; 1 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2124,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 1000 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2100; 1000 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1000,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2100,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 1000 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2124; 1000 MC]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1000,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2124,
                    IsSpatial = FALSE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# Merge

myScenario <- scenario(myProject,
                       scenario="SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Forested Wetland to Water]",
                            "SF Flow Multipliers [Emergent Wetland to Water]",
                            "SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")

rm(myScenario)

# Merge flow multipliers 

# Flow Multipliers No Climate
myScenario <- scenario(myProject, 
                       scenario="Flow Multipliers [No PRISM, Uncertainty, Add Prev Wetland]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Forested Wetland to Water]",
                            "SF Flow Multipliers [Emergent Wetland to Water]",
                            "SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland, Site]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat; Site]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")

rm(myScenario)

# Flow Multipliers No Climate
myScenario <- scenario(myProject, 
                       scenario="Flow Multipliers [No PRISM, Uncertainty, Add Prev Wetland, IPCC]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Forested Wetland to Water IPCC]",
                            "SF Flow Multipliers [Emergent Wetland to Water IPCC]",
                            "SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland, Site]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat; Site]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")

rm(myScenario)

myScenario <- scenario(myProject,
                       scenario="SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland, IPCC]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Forested Wetland to Water IPCC]",
                            "SF Flow Multipliers [Emergent Wetland to Water IPCC]",
                            "SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]")

rm(myScenario)

### Initialize grass and shrub
stateAttributesPath <- paste0(rootPath, "Data/State Attributes/")

stockTableForest <- read_csv(paste0(stateAttributesPath, "State Attributes - Initial Stocks Forest Harvest.csv"))
stockTableGrassShrub <- read_csv(paste0(stateAttributesPath, "State Attributes - Initial Stocks Grassland Shrubland.csv"))
names(stockTableForest) <- gsub("ID","Id",names(stockTableForest))
names(stockTableGrassShrub) <- gsub("ID","Id",names(stockTableGrassShrub))


stockTableForest <- stockTableForest %>%
  select(where(~!all(is.na(.x)))) %>%
  select(-TSTGroupId)

stockTableGrassShrub <- stockTableGrassShrub %>%
  select(where(~!all(is.na(.x))))

myScenario <- scenario(myProject,
                       scenario="Init C Stocks at Equilibrium [Forest, Shrub, Grass]",
                       folder = "Single-Cell Sub-Scenarios")

myData <- datasheet(myScenario,"stsim_StateAttributeValue", optional = T, empty = T) %>%
  addRow(stockTableForest)

saveDatasheet(myScenario, myData, "stsim_StateAttributeValue", append = FALSE)
saveDatasheet(myScenario, stockTableGrassShrub , "stsim_StateAttributeValue", append = TRUE)


# Update to initialize pasture in same way
myScenario <- scenario(myProject, 
                       scenario= "Init C Stocks at Equilibrium [Ag, Barren, Developed]",
                       source = "Init C Stocks at Equilibrium [Ag: Cropland; Prev Wetland Forest]",
                       folder = "Single-Cell Sub-Scenarios")

myData <- datasheet(myScenario,"stsim_StateAttributeValue")

myDataExisting <- myData

myDataAdd <- myData %>%
  mutate(StateClassId = "Agriculture: Pasture")

myDataAdd2 <- myData %>%
  filter(StateClassId == "Agriculture: Cropland") %>%
  mutate(StateClassId = "Barren: All")

myDataAdd3 <- myDataAdd2 %>%
  mutate(StateClassId = "Developed: High Intensity")

myDataAdd4 <- myDataAdd2 %>%
  mutate(StateClassId = "Developed: Low Intensity")

myDataAdd5 <- myDataAdd2 %>%
  mutate(StateClassId = "Developed: Medium Intensity")

myDataAdd6 <- myDataAdd2 %>%
  mutate(StateClassId = "Developed: Open Space")

myDataAdd <- myDataAdd %>%
  addRow(myDataAdd2) %>%
  addRow(myDataAdd3) %>%
  addRow(myDataAdd4) %>%
  addRow(myDataAdd5) %>%
  addRow(myDataAdd6) %>%
  anti_join(myDataExisting)

saveDatasheet(myScenario, myDataAdd, "stsim_StateAttributeValue", append = TRUE)

# SAVs
myScenario <- scenario(myProject, 
                       scenario="SAV: Uncertainty Update and Initial C",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("Init C Stocks at Equilibrium Water and Shore [Mean]",
                            "SAV: Agriculture [Net Growth]",
                            "SAV: Methane: Forested and Emergent [Site]",
                            "STSM State Attributes [Site Net Growth, Add Wetland]",
                            "Init C Stocks at Equilibrium [Ag, Barren, Developed]",
                            "Init C Stocks at Equilibrium [Forest, Shrub, Grass]",
                            "Init C Stocks at Equilibrium Forested Wetland [Site]",
                            "Init C Stocks at Equilibrium Emergent Wetland [Site]")

rm(myScenario)

# SAVs
myScenario <- scenario(myProject, 
                       scenario="SAV: Mean Update and Initial C",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("Init C Stocks at Equilibrium Water and Shore [Mean]",
                            "SAV: Agriculture [Net Growth]",
                            "SAV: Methane: Forested and Emergent",
                            "STSM State Attributes [Mean Net Growth, Add Wetland]",
                            "Init C Stocks at Equilibrium [Ag, Barren, Developed]",
                            "Init C Stocks at Equilibrium [Forest, Shrub, Grass]",
                            "Init C Stocks at Equilibrium [Wetland: Palustrine Forested Updated]",
                            "Init C Stocks at Equilibrium Emergent Wetland [Mean]")
  
rm(myScenario)

# Update so initial carbon stocks can't be negative

scen <- c("Init C Stocks at Equilibrium [Ag, Barren, Developed]",
          "Init C Stocks at Equilibrium [Forest, Shrub, Grass]",
          "Init C Stocks at Equilibrium [Wetland: Palustrine Forested Updated]",
          "Init C Stocks at Equilibrium Emergent Wetland [Mean]")

for (i in 1:length(scen)){
  
  myScenario <- scenario(myProject,scenario=scen[i])
  myData <- datasheet(myScenario, "stsim_StateAttributeValue")
  print(i)
  print(range(myData$Value, na.rm = T))
  myData$Value[myData$Value < 0 & !(is.na(myData$Value))] <- 0
  print(range(myData$Value, na.rm = T))
  saveDatasheet(myScenario, myData, "stsim_StateAttributeValue", append = FALSE)
  
  rm(myScenario, myData)
  
}


myScenario <- scenario(myProject,
                       scenario=vTag("SF Flow Pathways [Event, Base, Updated, Water]", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Pathways [Event Flows]",
                            vTag("SF Flow Pathways [Base Flows, Add Methane, Add Ag, Add Water]", gwpVariant, style="bracket"))

rm(myScenario)

# If running for longer than 200 years use
#"Flow Multiplier by Stock [Wetland to Water]",

# Merge carbon data sheets for mean model
myScenario <- scenario(myProject,
                       scenario=vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("SAV: Mean Update and Initial C",
                            "Stock Limit [All]",
                            "SF Initial Stocks",
                            vTag("SF Stock and Flow Group Membership [Add Methane]", gwpVariant, style="bracket"),
                            "SF Flow Order [Updated]",
                            vTag("SF Flow Pathways [Event, Base, Updated, Water]", gwpVariant, style="bracket"),
                            "Pipeline")

rm(myScenario)

# Merge carbon data sheets for uncertainty model
myScenario <- scenario(myProject,
                       scenario=vTag("Single Cell: Carbon and LULC: Uncertainty", gwpVariant, style="bracket"),
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("SAV: Uncertainty Update and Initial C",
                            "Distributions [Site, Lat, Forest]",
                            "External Variable: Site ID Lat Flux Estimate Set Seed Add Forest",
                            "Stock Limit [All]",
                            "SF Initial Stocks",
                            vTag("SF Stock and Flow Group Membership [Add Methane]", gwpVariant, style="bracket"),
                            "SF Flow Order [Updated]",
                            vTag("SF Flow Pathways [Event, Base, Updated, Water]", gwpVariant, style="bracket"),
                            "Pipeline")

rm(myScenario)


# Run Final Scenarios

# Wetland: Estuarine Emergent

myScenario <- scenario(myProject,
                       scenario=vTag("Estuarine Emergent Wetland: Mean", gwpVariant, style="bracket"),
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2124; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                            vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

rm(myScenario)

# Wetland: Palustrine Emergent

myScenario <- scenario(myProject,
                       scenario=vTag("Palustrine Emergent Wetland: Mean", gwpVariant, style="bracket"),
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2124; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                            vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))
rm(myScenario)

# Run Final Scenarios

# Wetland: Estuarine Emergent

myScenario <- scenario(myProject,
                       scenario=vTag("Estuarine Emergent Wetland: Add Uncertainty", gwpVariant, style="bracket"),
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2124; 1000 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "Flow Multipliers [No PRISM, Uncertainty, Add Prev Wetland]",
                            vTag("Single Cell: Carbon and LULC: Uncertainty", gwpVariant, style="bracket"))

rm(myScenario)

# Wetland: Palustrine Emergent

myScenario <- scenario(myProject,
                       scenario=vTag("Palustrine Emergent Wetland: Add Uncertainty", gwpVariant, style="bracket"),
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2124; 1000 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "Flow Multipliers [No PRISM, Uncertainty, Add Prev Wetland]",
                            vTag("Single Cell: Carbon and LULC: Uncertainty", gwpVariant, style="bracket"))

rm(myScenario)


# Oak gum cypress

myScenario <- scenario(myProject,
                       scenario=vTag("Original Oak Gum Cypress Forest", gwpVariant, style="bracket"),
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2124; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Forest: Oak Gum Cypress [Age 1]",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                            vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

rm(myScenario)

# Palustrine Forested Wetland

myScenario <- scenario(myProject,
                       scenario=vTag("Palustrine Forested Wetland: Mean", gwpVariant, style="bracket"),
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2124; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 1]",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                            vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

rm(myScenario)

# Palustrine Forested Wetland Add Uncertainty

myScenario <- scenario(myProject,
                       scenario=vTag("Palustrine Forested Wetland: Add Uncertainty", gwpVariant, style="bracket"),
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2124; 1000 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 1]",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "Flow Multipliers [No PRISM, Uncertainty, Add Prev Wetland]",
                            vTag("Single Cell: Carbon and LULC: Uncertainty", gwpVariant, style="bracket"))

rm(myScenario)


runIfNeeded(myProject, vTag("Estuarine Emergent Wetland: Mean", gwpVariant, style="bracket"))
runIfNeeded(myProject, vTag("Palustrine Emergent Wetland: Mean", gwpVariant, style="bracket"))
runIfNeeded(myProject, vTag("Estuarine Emergent Wetland: Add Uncertainty", gwpVariant, style="bracket"))
runIfNeeded(myProject, vTag("Palustrine Emergent Wetland: Add Uncertainty", gwpVariant, style="bracket"))
runIfNeeded(myProject, vTag("Original Oak Gum Cypress Forest", gwpVariant, style="bracket"))
runIfNeeded(myProject, vTag("Palustrine Forested Wetland: Mean", gwpVariant, style="bracket"))
runIfNeeded(myProject, vTag("Palustrine Forested Wetland: Add Uncertainty", gwpVariant, style="bracket"))

transitionTypes <- c("Emergent Wetland to Water",
                     "Emergent Wetland to Unvegetated",
                     "Forested Wetland to Water",
                     "Forested Wetland to Unvegetated")

for (i in 1:length(transitionTypes)){
  
  if (transitionTypes[i] %in% c("Emergent Wetland to Water",
                                "Forested Wetland to Water")){
    
    transitionPathways <- c("STSM Transition Pathways [Wetland to Water]")
    
  } else if (transitionTypes[i] %in% c("Emergent Wetland to Unvegetated",
                                       "Forested Wetland to Unvegetated")){
    
    transitionPathways <- c("STSM Transition Pathways [Wetland to Unvegetated]")
    
  }
  
  if (i %in% c(1,2)){

    myScenario <- scenario(myProject,
                           scenario=vTag(paste0("Transition: Estuarine ",transitionTypes[i]," S"), gwpVariant, style="bracket"),
                           folder = "3. Single-Cell Scenarios")

    mergeDependencies(myScenario) <- F

    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                                vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

    rm(myScenario)

    myScenario <- scenario(myProject,
                           scenario= vTag(paste0("Transition: Palustrine ",transitionTypes[i]," S"), gwpVariant, style="bracket"),
                           folder = "3. Single-Cell Scenarios")

    mergeDependencies(myScenario) <- F

    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                                vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

    rm(myScenario)

    myScenario <- scenario(myProject,
                           scenario=vTag(paste0("Transition: Estuarine ",transitionTypes[i]," IPCC"), gwpVariant, style="bracket"),
                           folder = "3. Single-Cell Scenarios")

    mergeDependencies(myScenario) <- F

    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland, IPCC]",
                                vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

    rm(myScenario)

    myScenario <- scenario(myProject,
                           scenario = vTag(paste0("Transition: Palustrine ",transitionTypes[i]," IPCC"), gwpVariant, style="bracket"),
                           folder = "3. Single-Cell Scenarios")

    mergeDependencies(myScenario) <- F

    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland, IPCC]",
                                vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

    rm(myScenario)

    runIfNeeded(myProject, vTag(paste0("Transition: Estuarine ",transitionTypes[i]," S"), gwpVariant, style="bracket"))
    runIfNeeded(myProject, vTag(paste0("Transition: Palustrine ",transitionTypes[i]," S"), gwpVariant, style="bracket"))
    runIfNeeded(myProject, vTag(paste0("Transition: Estuarine ",transitionTypes[i]," IPCC"), gwpVariant, style="bracket"))
    runIfNeeded(myProject, vTag(paste0("Transition: Palustrine ",transitionTypes[i]," IPCC"), gwpVariant, style="bracket"))

  } else if (i %in% c(3,4)){

    myScenario <- scenario(myProject,
                           scenario= vTag(paste0("Transition: Palustrine ",transitionTypes[i]," S"), gwpVariant, style="bracket"),
                           folder = "3. Single-Cell Scenarios")

    mergeDependencies(myScenario) <- F

    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 124]",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                                vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

    rm(myScenario)

    myScenario <- scenario(myProject,
                           scenario = vTag(paste0("Transition: Palustrine ",transitionTypes[i]," IPCC"), gwpVariant, style="bracket"),
                           folder = "3. Single-Cell Scenarios")

    mergeDependencies(myScenario) <- F

    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 124]",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland, IPCC]",
                                vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

    rm(myScenario)

    runIfNeeded(myProject, vTag(paste0("Transition: Palustrine ",transitionTypes[i]," S"), gwpVariant, style="bracket"))
    runIfNeeded(myProject, vTag(paste0("Transition: Palustrine ",transitionTypes[i]," IPCC"), gwpVariant, style="bracket"))

  }
  
}
