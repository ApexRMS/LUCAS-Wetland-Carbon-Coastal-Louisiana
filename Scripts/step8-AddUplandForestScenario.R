# ApexRMS
# Updated 2025-02-04
# Run after step7-AddSpatialExampleToLibrary.R
# This script creates a spatial scenario 
# where all palustrine forested wetland is converted to an oak gum cypress forest

library(rsyncrosim)
library(tidyverse)
library(terra)

source(paste0(rootPath, "Scripts/gwpConfig.R"))
gwpVariant <- gwpVariants[[activeGWP]]

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

## Initial Conditions
myScenario <- scenario(myProject, 
                       scenario = "STSM Initial Conditions [Spatial No Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_InitialConditionsSpatial"
myData <- datasheet(myScenario, sheetName)

rastSC <- rast(paste0(modelFullPath, "/Data/Initial Conditions/LUCAS/", "LUCAS_2001.tif"))
rastSC <- ifel(rastSC == 90,600,rastSC)
writeRaster(rastSC,paste0(modelFullPath, "/Data/Initial Conditions/", "StateClassNoForestedWetland.tif"), overwrite = T)

myData <- data.frame(StratumFileName = paste0(modelFullPath, "/Data/Initial Conditions/", "PrimaryStrata.tif"),
                     StateClassFileName = paste0(modelFullPath, "/Data/Initial Conditions/", "StateClassNoForestedWetland.tif"),
                     AgeFileName = paste0(modelFullPath, "/Data/Initial Conditions/", "Stand Age.tif"))

saveDatasheet(myScenario, myData, sheetName)

# Add in all possible transitions

cmpModelInputsDir <- paste0(modelFullPath, "/Data/Transition Spatial Multipliers No FW")

if(!dir.exists(cmpModelInputsDir)){
  dir.create(cmpModelInputsDir)
}

initialCondNoFW <- paste0(modelFullPath,"/Data/Initial Conditions/LUCAS No FW/")

if(!dir.exists(initialCondNoFW)){
  dir.create(initialCondNoFW)
}

# Get list of all possible LULC transitions
# Find all state classes in lucas raster stack of study area
# Don't allow forest to forest transitions, those don't happen
# Also reclass all forested wetland as oak gum cypress upland forest

years <- c("2001","2006","2010","2016")

stateClassIds <- c()

for (i in 1:length(years)){
  
  lucas1 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_",years[i],".tif"))
  lucas1 <- ifel(lucas1 == 90,600,lucas1)
  writeRaster(lucas1,paste0(initialCondNoFW, "LUCAS_",years[i],".tif"), overwrite = T)
  
  stateClassIds <- c(stateClassIds,freq(lucas1)$value)
  
  rm(lucas1)
  
}

stateClassIds <- unique(stateClassIds)

lulcTransitions <- expand.grid(stateClassIds, stateClassIds) %>% 
  filter(Var1 != Var2) %>%
  filter(!(Var1 >=100 & Var2 >=100)) %>%
  rename(FromStateClass = Var1,
         ToStateClass = Var2)

lulcTransitionsReview <- lulcTransitions
lulcTransitionsReview$TotalPixelTransitionsObserved <- 0

# List pairs of transition years
startYears <- c(2001,2006,2010)
endYears <- c(2006,2010,2016)

for(i in 1:length(startYears)){
  
  # Load state class rasters
  startYearRaster <- rast(paste0(modelFullPath,
                                 "/Data/Initial Conditions/LUCAS No FW/LUCAS_",
                                 startYears[i],".tif"))
  endYearRaster <- rast(paste0(modelFullPath,
                               "/Data/Initial Conditions/LUCAS No FW/LUCAS_",
                               endYears[i],".tif"))
  
  for(j in 1:nrow(lulcTransitions)){
    
    rowT <- lulcTransitions[j,]
    
    # Reclassify state class maps for a single state class
    transitionMultiplierRaster <- ifel((startYearRaster == rowT$FromStateClass & 
                                          endYearRaster == rowT$ToStateClass), 1, 0)
    
    lulcTransitionsReview$TotalPixelTransitionsObserved[j] <- sum(c(lulcTransitionsReview$TotalPixelTransitionsObserved[j],
                                                                    global(transitionMultiplierRaster, fun = "sum", na.rm = T)$sum))
    
    outputFilename <- str_c("tm-historical-", startYears[i], "-to-", endYears[i], "-sc-", rowT$FromStateClass, "-to-", rowT$ToStateClass, ".tif")
    
    # Save to disk
    transitionMultiplierRaster %>%
      writeRaster(filename = file.path(cmpModelInputsDir, "/", outputFilename),
                  datatype = "INT2S",
                  NAflag = -9999,
                  overwrite = T)
    
    
    rm(transitionMultiplierRaster,outputFilename,rowT)
    gc()
  }
  rm(startYearRaster, endYearRaster)
  gc()
}

table(lulcTransitionsReview$TotalPixelTransitionsObserved)

transitionsKeep <- lulcTransitionsReview[lulcTransitionsReview$TotalPixelTransitionsObserved>0,]

# Transition Spatial Multipliers -----------------------------------------------
# Load state class names and Ids
stateClassTable <- datasheet(myProject, name = "stsim_StateClass")

transitionTypes <- c()

tmFilepaths <- list.files(file.path(cmpModelInputsDir),
                          pattern = ".tif",
                          full.names = TRUE)

fileNamesTable <- data.frame(Timestep = NA,
                             TransitionGroupId = NA,
                             MultiplierFileName = NA)

for (i in 1:length(startYears)){
  
  for (j in 1:nrow(transitionsKeep)){
    
    rowT <- transitionsKeep[j,]
    
    file1 <- grep(paste0("tm-historical-",
                         startYears[i], 
                         "-to-", endYears[i], 
                         "-sc-", rowT$FromStateClass, 
                         "-to-", rowT$ToStateClass, ".tif"),tmFilepaths,value = T)
    
    fromStateClassName <- stateClassTable %>% 
      filter(Id == rowT$FromStateClass) %>% 
      pull(Name)
    
    toStateClassName <- stateClassTable %>% 
      filter(Id == rowT$ToStateClass) %>% 
      pull(Name)
    
    lulcTransitionType <- str_c("LULCC: ", fromStateClassName, " -> ", toStateClassName, " [Type]")
    
    transitionTypes <- append(transitionTypes, lulcTransitionType)
    
    fileNamesTable <- rbind(fileNamesTable,data.frame(Timestep = endYears[i],
                                                      TransitionGroupId = lulcTransitionType,
                                                      MultiplierFileName = file1))
    
  }
  
}

fileNamesTable <- fileNamesTable[-1,]

transitionTypes <- unique(transitionTypes)

# Add transition types to library properties
sheetName <- "stsim_TransitionType"
myData <- datasheet(myProject,"stsim_TransitionType")

maxId <- max(myData$Id)

transitionTypesUplandForest <- gsub(" [Type]", "",transitionTypes,fixed = T)

toAdd <- transitionTypesUplandForest[!(transitionTypesUplandForest %in% myData$Name)]

if (length(toAdd) == 1){
  myDataNew <- data.frame(Name = toAdd,
                          Id = maxId+1)
} else if (length(toAdd) > 1){
  myDataNew <- data.frame(Name = toAdd,
                          Id = seq((maxId+1),(maxId+length(toAdd)),1))
}

saveDatasheet(myProject, myDataNew, "stsim_TransitionType", append = T)

# Transition pathways No Forested Wetland
myScenario <- scenario(myProject, 
                       scenario = "STSM Transition Pathways [No Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_DeterministicTransition"

myData <- datasheet(myScenario, sheetName) %>%
  addRow(data.frame(StateClassIdSource = stateClassTable$Name,
                    Location = paste0("A",1:length(stateClassTable$Name))))
saveDatasheet(myScenario, myData, sheetName, append = F)

sheetName <- "stsim_Transition"

transitionTypesSub <- gsub(" [Type]","",gsub("LULCC: ","",transitionTypes),fixed = T)
stateClassStart <- unlist(lapply(strsplit(transitionTypesSub, " -> "), "[[", 1))
stateClassEnd <- unlist(lapply(strsplit(transitionTypesSub, " -> "), "[[", 2))

myData <- datasheet(myScenario, sheetName, optional = T) %>%
  addRow(data.frame(StateClassIdSource = stateClassStart,
                    StateClassIdDest = stateClassEnd,
                    TransitionTypeId = gsub(" [Type]","",transitionTypes,fixed = T),
                    Probability = 1))

saveDatasheet(myScenario, myData, sheetName, append = F)

# Turn off transition multipliers
myScenario <- scenario(myProject, 
                       scenario = "STSM Transition Multipliers [No Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_TransitionMultiplierValue"
myData <- datasheet(myScenario, sheetName, optional = T) %>%
  addRow(data.frame(Timestep = 2002,
                    TransitionGroupId = transitionTypes,
                    Amount = 0)) %>%
  addRow(data.frame(Timestep = 2006,
                    TransitionGroupId = transitionTypes,
                    Amount = 1)) %>%
  addRow(data.frame(Timestep = 2007,
                    TransitionGroupId = transitionTypes,
                    Amount = 0)) %>%
  addRow(data.frame(Timestep = 2010,
                    TransitionGroupId = transitionTypes,
                    Amount = 1)) %>%
  addRow(data.frame(Timestep = 2011,
                    TransitionGroupId = transitionTypes,
                    Amount = 0)) %>%
  addRow(data.frame(Timestep = 2016,
                    TransitionGroupId = transitionTypes,
                    Amount = 1)) %>%
  addRow(data.frame(Timestep = 2017,
                    TransitionGroupId = transitionTypes,
                    Amount = 0))

saveDatasheet(myScenario, myData, sheetName, append = F)

# Spatial multipliers table
myScenario <- scenario(myProject, 
                       scenario = "STSM Spatial Multipliers [No Forested Wetland]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_TransitionSpatialMultiplier"

saveDatasheet(myScenario, fileNamesTable, sheetName)


# LULC, Climate, No forested Wetland

# Update initial conditions

# Create new transition table
myScenario <- scenario(myProject,
                       scenario = vTag("Basin No Palustrine Forested Wetland", gwpVariant, style="suffix"),
                       folder = "4. Final Spatial Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Spatial Multiprocessing",
                            "Run Control [2001-2016; 1 MC; Spatial]",
                            "Output Options [Spatial; Summary]",
                            "STSM Initial Conditions [Spatial No Forested Wetland]",
                            "STSM Spatial Multipliers [No Forested Wetland]",
                            "STSM Transition Multipliers [No Forested Wetland]",
                            "STSM Transition Pathways [No Forested Wetland]",
                            "SF Flow Spatial Multipliers [PRISM Historical]",
                            vTag("SF Output Options and Filters [Only 2016]", gwpVariant, style="bracket"),
                            "SF Flow Multipliers [PRISM, Mean, Add Prev Wetland]",
                            "Stock Limit [All]",
                            vTag("Single Cell: Carbon and LULC: Mean", gwpVariant, style="bracket"))

rm(myScenario)

# run(myProject, 
#     scenario="4 Land Cover Change, Climate, and No Forested Wetland")
