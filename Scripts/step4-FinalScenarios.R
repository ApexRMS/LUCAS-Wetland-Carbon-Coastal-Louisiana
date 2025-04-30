# Created by Amanda Schwantes, ApexRMS
# Updated 2025-02-25
# Run after step3d-AddWaterPrevWetland.R
# This script creates final single-cell wetland scenarios with no transitions
# And with a transition from wetland to water

library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

rootPath <- "E:/gitprojects/A329-LucasBarataria/"

outpathDatasheets <- paste0(rootpath,"Data/Datasheets Wetland/")

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
  addRow(myDataAdd6)

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
                       scenario="SF Flow Pathways [Event, Base, Updated, Water]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Pathways [Event Flows]",
                            "SF Flow Pathways [Base Flows, Add Methane, Add Ag, Add Water]")

rm(myScenario)

# If running for longer than 200 years use
#"Flow Multiplier by Stock [Wetland to Water]",

# Merge carbon data sheets for mean model
myScenario <- scenario(myProject, 
                       scenario="Single Cell: Carbon and LULC: Mean",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("SAV: Mean Update and Initial C",
                            "Stock Limit [All]",
                            "SF Initial Stocks",
                            "SF Stock and Flow Group Membership [Add Methane]",
                            "SF Flow Order [Updated]",
                            "SF Flow Pathways [Event, Base, Updated, Water]",
                            "Pipeline")

rm(myScenario)

# Merge carbon data sheets for uncertainty model
myScenario <- scenario(myProject, 
                       scenario="Single Cell: Carbon and LULC: Uncertainty",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("SAV: Uncertainty Update and Initial C",
                            "Distributions [Site, Lat, Forest]",
                            "External Variable: Site ID Lat Flux Estimate Set Seed Add Forest",
                            "Stock Limit [All]",
                            "SF Initial Stocks",
                            "SF Stock and Flow Group Membership [Add Methane]",
                            "SF Flow Order [Updated]",
                            "SF Flow Pathways [Event, Base, Updated, Water]",
                            "Pipeline")

rm(myScenario)


# Run Final Scenarios

# Wetland: Estuarine Emergent

myScenario <- scenario(myProject, 
                       scenario="Estuarine Emergent Wetland: Mean",
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2100; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                            "Single Cell: Carbon and LULC: Mean")

rm(myScenario)

# Wetland: Palustrine Emergent

myScenario <- scenario(myProject, 
                       scenario="Palustrine Emergent Wetland: Mean",
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2100; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                            "Single Cell: Carbon and LULC: Mean")
rm(myScenario)

# Run Final Scenarios

# Wetland: Estuarine Emergent

myScenario <- scenario(myProject, 
                       scenario="Estuarine Emergent Wetland: Add Uncertainty",
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2100; 1000 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "Flow Multipliers [No PRISM, Uncertainty, Add Prev Wetland]",
                            "Single Cell: Carbon and LULC: Uncertainty")

rm(myScenario)

# Wetland: Palustrine Emergent

myScenario <- scenario(myProject, 
                       scenario="Palustrine Emergent Wetland: Add Uncertainty",
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2100; 1000 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "Flow Multipliers [No PRISM, Uncertainty, Add Prev Wetland]",
                            "Single Cell: Carbon and LULC: Uncertainty")

rm(myScenario)


# Oak gum cypress

myScenario <- scenario(myProject,
                       scenario="Original Oak Gum Cypress Forest",
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2100; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Forest: Oak Gum Cypress [Age 1]",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                            "Single Cell: Carbon and LULC: Mean")

rm(myScenario)

# Palustrine Forested Wetland

myScenario <- scenario(myProject, 
                       scenario="Palustrine Forested Wetland: Mean",
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2100; 1 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 1]",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                            "Single Cell: Carbon and LULC: Mean")

rm(myScenario)

# Palustrine Forested Wetland Add Uncertainty

myScenario <- scenario(myProject, 
                       scenario="Palustrine Forested Wetland: Add Uncertainty",
                       folder = "3. Single-Cell Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Run Control [2001-2100; 1000 MC]",
                            "Output Options [Non-Spatial; Summary]",
                            "SF Output Options [All]",
                            "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 1]",
                            "Transition Multipliers",
                            "STSM Transition Pathways",
                            "Flow Multipliers [No PRISM, Uncertainty, Add Prev Wetland]",
                            "Single Cell: Carbon and LULC: Uncertainty")

rm(myScenario)


run(myProject, scenario="Estuarine Emergent Wetland: Mean")
run(myProject, scenario="Palustrine Emergent Wetland: Mean")
run(myProject, scenario="Estuarine Emergent Wetland: Add Uncertainty")
run(myProject, scenario="Palustrine Emergent Wetland: Add Uncertainty")
run(myProject, scenario="Original Oak Gum Cypress Forest")
run(myProject, scenario="Palustrine Forested Wetland: Mean")
run(myProject, scenario="Palustrine Forested Wetland: Add Uncertainty")

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
                           scenario=paste0("Transition: Estuarine ",transitionTypes[i]," S"),
                           folder = "3. Single-Cell Scenarios")
    
    mergeDependencies(myScenario) <- F
    
    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                                "Single Cell: Carbon and LULC: Mean")
    
    rm(myScenario)
    
    myScenario <- scenario(myProject, 
                           scenario= paste0("Transition: Palustrine ",transitionTypes[i]," S"),
                           folder = "3. Single-Cell Scenarios")
    
    mergeDependencies(myScenario) <- F
    
    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                                "Single Cell: Carbon and LULC: Mean")
    
    rm(myScenario)
    
    myScenario <- scenario(myProject, 
                           scenario=paste0("Transition: Estuarine ",transitionTypes[i]," IPCC"),
                           folder = "3. Single-Cell Scenarios")
    
    mergeDependencies(myScenario) <- F
    
    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Estuarine Emergent",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland, IPCC]",
                                "Single Cell: Carbon and LULC: Mean")
    
    rm(myScenario)
    
    myScenario <- scenario(myProject, 
                           scenario = paste0("Transition: Palustrine ",transitionTypes[i]," IPCC"),
                           folder = "3. Single-Cell Scenarios")
    
    mergeDependencies(myScenario) <- F
    
    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Palustrine Emergent",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland, IPCC]",
                                "Single Cell: Carbon and LULC: Mean")
    
    rm(myScenario)
    
    run(myProject, scenario=paste0("Transition: Estuarine ",transitionTypes[i]," S"))
    run(myProject, scenario=paste0("Transition: Palustrine ",transitionTypes[i]," S"))
    run(myProject, scenario=paste0("Transition: Estuarine ",transitionTypes[i]," IPCC"))
    run(myProject, scenario=paste0("Transition: Palustrine ",transitionTypes[i]," IPCC"))
    
  } else if (i %in% c(3,4)){
    
    myScenario <- scenario(myProject, 
                           scenario= paste0("Transition: Palustrine ",transitionTypes[i]," S"),
                           folder = "3. Single-Cell Scenarios")
    
    mergeDependencies(myScenario) <- F
    
    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 124]",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                                "Single Cell: Carbon and LULC: Mean")
    
    rm(myScenario)
    
    myScenario <- scenario(myProject, 
                           scenario = paste0("Transition: Palustrine ",transitionTypes[i]," IPCC"),
                           folder = "3. Single-Cell Scenarios")
    
    mergeDependencies(myScenario) <- F
    
    dependency(myScenario) <- c("Run Control [2001-2220; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                "Initial Conditions: Single Cell - Wetland: Palustrine Forested [Age 124]",
                                paste0("Transition Multipliers: ", transitionTypes[i]),
                                transitionPathways,
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland, IPCC]",
                                "Single Cell: Carbon and LULC: Mean")
    
    rm(myScenario)
    
    run(myProject, scenario=paste0("Transition: Palustrine ",transitionTypes[i]," S"))
    run(myProject, scenario=paste0("Transition: Palustrine ",transitionTypes[i]," IPCC"))
    
  }
  
}
