# ApexRMS
# Updated 2025-02-04
# Run after step6-AddSpatialExample.R
# This script creates the four spatial scenarios

library(rsyncrosim)
library(tidyverse)
library(terra)

# Specify file paths, library, and project

## Spatial Multiprocessing----
turnOnSpatialMultiprocessing <- TRUE # Divide jobs by spatial tiles?
tileSize                     <- 650000 #1e4 # Approximate number of cells per tile
contig                       <- TRUE # Create contiguous tiles?
numberOfJobs <- 15

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

#Turn on multiprocessing
sheetName <- "core_Multiprocessing"
multiTab <- datasheet(myLibrary,name = sheetName)

multiTab$EnableMultiprocessing <- TRUE
multiTab$MaximumJobs <- numberOfJobs

saveDatasheet(myLibrary, multiTab, sheetName, append = FALSE)

rm(multiTab,sheetName)

### Write data into LUCAS subscenarios
inputRasterDir <- paste0(modelFullPath, "/", "Data/Flow Spatial Multipliers/")

# Forest Growth
myScenario <- scenario(myProject, 
                       scenario = "SF Flow Spatial Multipliers [PRISM Historical; Growth; Forest]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_FlowSpatialMultiplier"
myData <- data.frame(Timestep = seq(2000,2022),
                     FlowGroupId = "Net Growth Forest: Total",
                     MultiplierFileName = paste0(inputRasterDir, "Growth Forest/forestNppMult_", seq(2000,2022), ".tif"))
saveDatasheet(myScenario, myData, sheetName)

# Non Forest Growth
myScenario <- scenario(myProject, 
                       scenario = "SF Flow Spatial Multipliers [PRISM Historical; Growth; Non Forest]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_FlowSpatialMultiplier"
myData <- data.frame(Timestep = seq(2000,2022),
                    FlowGroupId = "Net Growth Non Forest: Total",
                    MultiplierFileName = paste0(inputRasterDir, "Growth Non Forest/nonForestNppMult_", seq(2000,2022), ".tif")) %>%
  addRow(data.frame(Timestep = seq(2000,2022),
                    FlowGroupId = "Net Growth Wetland Emergent: Total",
                    MultiplierFileName = paste0(inputRasterDir, "Growth Non Forest/nonForestNppMult_", seq(2000,2022), ".tif")))
saveDatasheet(myScenario, myData, sheetName)

# Q10 Fast
myScenario <- scenario(myProject, 
                       scenario = "SF Flow Spatial Multipliers [PRISM Historical; Q10 Fast]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_FlowSpatialMultiplier"
myData <- data.frame(Timestep = seq(2000,2022),
                    FlowGroupId = "Q10 Fast Flows",
                    MultiplierFileName = paste0(inputRasterDir, "Q10 Fast/q10Fast_", seq(2000,2022), ".tif"))
saveDatasheet(myScenario, myData, sheetName)

# Q10 Slow
myScenario <- scenario(myProject, 
                       scenario = "SF Flow Spatial Multipliers [PRISM Historical; Q10 Slow]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_FlowSpatialMultiplier"
myData <- data.frame(Timestep = seq(2000,2022),
                    FlowGroupId = "Q10 Slow Flows",
                    MultiplierFileName = paste0(inputRasterDir, "Q10 Slow/q10Slow_", seq(2000,2022), ".tif"))
saveDatasheet(myScenario, myData, sheetName)

# Merge Dependencies
myScenario <- scenario(myProject, 
                       scenario = "SF Flow Spatial Multipliers [PRISM Historical]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T
dependency(myScenario) <- c("SF Flow Spatial Multipliers [PRISM Historical; Q10 Slow]",
                            "SF Flow Spatial Multipliers [PRISM Historical; Q10 Fast]",
                            "SF Flow Spatial Multipliers [PRISM Historical; Growth; Non Forest]",
                            "SF Flow Spatial Multipliers [PRISM Historical; Growth; Forest]")

# Update Scalar
myScenario <- scenario(myProject, 
                       scenario = "SF Flow Multipliers [Scalar; Updated]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "SF Flow Multipliers [Scalar]")

sheetName <- "stsim_FlowMultiplier"
myData <- datasheet(myScenario, sheetName)

myDataNew <- data.frame(Timestep = 2002,
                        FlowGroupId = "Net Growth Wetland Emergent: Total",
                        Value = 0.01)

saveDatasheet(myScenario, myDataNew, sheetName, append = T)


# Add in all possible transitions

cmpModelInputsDir <- paste0(modelFullPath, "/Data/Transition Spatial Multipliers")

if(!dir.exists(cmpModelInputsDir)){
  dir.create(cmpModelInputsDir)
}

# update rasters so these state classes exist
# Water: Previously Emergent Wetland  13
# Water: Previously Forested Wetland  14
# Wetland: Unvegetated Emergent  98
# Wetland: Unvegetated Forested  99

# Wetland: Estuarine Emergent  96
# Wetland: Estuarine Forested  91
# Wetland: Palustrine Emergent  95
# Wetland: Palustrine Forested  90
# Wetland: Unconsolidated Shore  97

stateClassTable <- datasheet(myProject, name = "stsim_StateClass")

scenarios <- c("2001",
               "2006",
               "2010",
               "2016")

pathIn <- paste0(modelFullPath,
                   "/Data/Initial Conditions/LUCAS/")
  
for (i in 2:length(scenarios)){
  
  pattern1 <- paste0("LUCAS_",
                     scenarios[i],
                     ".tif")
  
  if (i == 2) {
      
    file1 <- list.files(pathIn,pattern = "LUCAS_2001.tif", full.names = T)
    r1 <- rast(file1)
      
    file2 <- list.files(pathIn,pattern = pattern1, full.names = T)
    r2 <- rast(file2)
    
    # if wetland then shore or water assign correct class
    r2 <- ifel((r1 %in% c(95,96) &
                  r2 == 11),13,r2)
    
    r2 <- ifel((r1 %in% c(90) &
                  r2 == 11),14,r2)
    
    r2 <- ifel((r1 %in% c(95,96) &
                  r2 == 97),98,r2)
    
    r2 <- ifel((r1 %in% c(90) &
                  r2 == 97),99,r2)
      
    writeRaster(r2,paste0(modelFullPath,
                          "/Data/Initial Conditions/LUCAS/LUCAS_",
                          scenarios[i],".tif"),
                overwrite = T)
      
    r1 <- r2
      
    rm(r2,file2,file1)
      
  } else if (i > 2){
      
    file2 <- list.files(pathIn,pattern = pattern1, full.names = T)
    r2 <- rast(file2)
      
    # if wetland then shore or water assign correct class
    r2 <- ifel((r1 %in% c(95,96) &
                  r2 == 11),13,r2)
    
    r2 <- ifel((r1 %in% c(90) &
                  r2 == 11),14,r2)
    
    r2 <- ifel((r1 %in% c(95,96) &
                  r2 == 97),98,r2)
    
    r2 <- ifel((r1 %in% c(90) &
                  r2 == 97),99,r2)
    
    # if water prev wetland and still water assign correct class
    r2 <- ifel(r1 == 13 &
                 r2 == 11,13,r2)
    
    r2 <- ifel(r1 == 14 &
                 r2 == 11,14,r2)
    
    r2 <- ifel(r1 == 98 &
                 r2 == 97,98,r2)
    
    r2 <- ifel(r1 == 99 &
                 r2 == 97,99,r2)
    
    # if water prev wetland and now shore assign correct class
    r2 <- ifel(r1 == 13 &
                 r2 == 97,98,r2)
    
    r2 <- ifel(r1 == 14 &
                 r2 == 97,99,r2)
    
    r2 <- ifel(r1 == 98 &
                 r2 == 11,13,r2)
    
    r2 <- ifel(r1 == 99 &
                 r2 == 11,14,r2)
      
    writeRaster(r2,paste0(modelFullPath,
                          "/Data/Initial Conditions/LUCAS/LUCAS_",
                          scenarios[i],".tif"),
                overwrite = T)
      
    r1 <- r2
      
    rm(file2,r2)
      
  }
  
}


# Consolidate state classes

for (i in 1:length(scenarios)){
  
  file1 <- list.files(paste0(modelFullPath,
                             "/Data/Initial Conditions/LUCAS/"),
                      pattern = paste0("LUCAS_",
                                       scenarios[i],
                                       ".tif"),
                      full.names = T)
  
  r1 <- rast(file1)
  
  r2 <- ifel(r1 %in% c(21,22,23,24,25),23,r1)
  r2 <- ifel(r2 %in% c(82,81,31),82,r2)
  
  fileOutName <- paste0(modelFullPath,
                        "/Data/Initial Conditions/LUCAS/LUCAS_",
                        scenarios[i],
                        ".tif")
  writeRaster(r2,fileOutName, overwrite = T)
  
  rm(file1,r1,r2,fileOutName)
  
}

## Initial Conditions
myScenario <- scenario(myProject, 
                       scenario = "STSM Initial Conditions [Spatial]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_InitialConditionsSpatial"
myData <- datasheet(myScenario, sheetName)

myData <- data.frame(StratumFileName = paste0(modelFullPath, "/Data/Initial Conditions/", "PrimaryStrata.tif"),
                     StateClassFileName = paste0(modelFullPath, "/Data/Initial Conditions/LUCAS/", "LUCAS_2001.tif"),
                     AgeFileName = paste0(modelFullPath, "/Data/Initial Conditions/", "Stand Age.tif"))

saveDatasheet(myScenario, myData, sheetName)

# Get list of all possible LULC transitions
# Find all state classes in lucas raster stack of study area
# Don't allow forest to forest transitions, those don't happen
lucas2001 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_2001.tif"))
lucas2006 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_2006.tif"))
lucas2010 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_2010.tif"))
lucas2016 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_2016.tif"))


stateClassIds <- unique(c(freq(lucas2001)$value,
                          freq(lucas2006)$value,
                          freq(lucas2010)$value,
                          freq(lucas2016)$value))

rm(lucas2001,lucas2006,lucas2010,lucas2016)

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
                                   "/Data/Initial Conditions/LUCAS/LUCAS_",
                                   startYears[i],".tif"))
    endYearRaster <- rast(paste0(modelFullPath,
                                 "/Data/Initial Conditions/LUCAS/LUCAS_",
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

transitionTypesSub0 <- gsub(" [Type]", "",transitionTypes,fixed = T)

transitionTypesSub0 <- transitionTypesSub0[!(transitionTypesSub0 %in% myData$Name)]

maxId <- max(myData$Id)

myDataNew <- data.frame(Name = transitionTypesSub0,
                        Id = c(1:length(transitionTypesSub0))+maxId)

saveDatasheet(myProject, myDataNew, "stsim_TransitionType", append = T)

# Transition pathways Empty
myScenario <- scenario(myProject, 
                       scenario = "STSM Transition Pathways [Turn Off]",
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
                    Probability = 0))

saveDatasheet(myScenario, myData, sheetName, append = F)

# Transition pathways
myScenario <- scenario(myProject, 
                       scenario = "STSM Transition Pathways [LA]",
                       folder = "Single-Cell Sub-Scenarios",
                       source = "STSM Transition Pathways [Turn Off]")

sheetName <- "stsim_Transition"

myData <- datasheet(myScenario, sheetName, optional = T) %>%
  mutate(Probability = 1)

saveDatasheet(myScenario, myData, sheetName, append = F)

# Turn off transition multipliers
myScenario <- scenario(myProject, 
                       scenario = "STSM Transition Multipliers [LA]",
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
                       scenario = "STSM Spatial Multipliers [LA]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_TransitionSpatialMultiplier"

saveDatasheet(myScenario, fileNamesTable, sheetName, append = F)

# sub-scenario: 1 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2016; 1 MC; Spatial]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 1,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2016,
                    IsSpatial = TRUE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# sub-scenario: 40 MC
myScenario <- scenario(myProject, 
                       scenario = "Run Control [2001-2016; 40 MC; Spatial]",
                       folder = "Single-Cell Sub-Scenarios")

sheetName <- "stsim_RunControl"
myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(MinimumIteration = 1,
                    MaximumIteration = 40,
                    MinimumTimestep = 2001,
                    MaximumTimestep = 2016,
                    IsSpatial = TRUE))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

# PRISM mean, schoolmaster
myScenario <- scenario(myProject,
                       scenario="SF Flow Multipliers [PRISM, Mean, Add Prev Wetland]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Forested Wetland to Water]",
                            "SF Flow Multipliers [Emergent Wetland to Water]",
                            "SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]",
                            "SF Flow Multipliers [Scalar; Updated]")

rm(myScenario)

# PRISM, IPCC
myScenario <- scenario(myProject,
                       scenario="SF Flow Multipliers [PRISM, Mean, Add Prev Wetland, IPCC]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Forested Wetland to Water]",
                            "SF Flow Multipliers [Emergent Wetland to Water IPCC]",
                            "SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Forest]",
                            "SF Flow Multipliers [Scalar; Updated]")

rm(myScenario)

#Flow Multipliers Climate
myScenario <- scenario(myProject, 
                       scenario="Flow Multipliers [PRISM, Uncertainty, Add Prev Wetland]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Forested Wetland to Water]",
                            "SF Flow Multipliers [Emergent Wetland to Water]",
                            "SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland, Site]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat; Site]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Scalar; Updated]",
                            "SF Flow Multipliers [Forest]")
                            
rm(myScenario)

#Flow Multipliers Climate
myScenario <- scenario(myProject, 
                       scenario="Flow Multipliers [PRISM, Uncertainty, Add Prev Wetland, IPCC]",
                       folder = "Single-Cell Sub-Scenarios Merged")

mergeDependencies(myScenario) <- T

dependency(myScenario) <- c("SF Flow Multipliers [Forested Wetland to Water IPCC]",
                            "SF Flow Multipliers [Emergent Wetland to Water IPCC]",
                            "SF Flow Multipliers [Non Forest; Updated]",
                            "SF Flow Multipliers [Emergent Wetland, Site]",
                            "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat; Site]",
                            "SF Flow Multipliers [Grassland Shrubland]",
                            "SF Flow Multipliers [Scalar; Updated]",
                            "SF Flow Multipliers [Forest]")
                            
rm(myScenario)

# Output Options
myScenario <- scenario(myProject, 
                       scenario = "Output Options [Spatial; Summary]",
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

sheetName <- "stsim_OutputOptionsSpatial"

myData <- datasheet(myScenario, name = sheetName, empty = TRUE) %>% 
  addRow(data.frame(RasterOutputSC = TRUE,
                    RasterOutputSCTimesteps = 1))
saveDatasheet(myScenario, myData, sheetName, append = FALSE)

rm(myScenario, myData, sheetName)

## Spatial Multiprocessing ----
if(turnOnSpatialMultiprocessing){
  # Load primary stratum raster
  # primaryStrataRaster <- rast(paste0(rootPath, "/", modelPath, "/", modelName, "/Data/Initial Conditions/", "PrimaryStrata", ".tif"))
  
  # Output filename of smp grid
  spatialMultiProcessingPath <- paste0(modelFullPath, "/Data/Spatial Multi Processing")
  if(!dir.exists(spatialMultiProcessingPath)) {
    dir.create(spatialMultiProcessingPath)}
  
  outputFilename <- paste0(spatialMultiProcessingPath, "/smpGrid.tif")
  
  initialScenario <- scenario(myProject,
                              "STSM Initial Conditions [Spatial]")
  
  classRaster <- rast(paste0(modelFullPath,"/",
                             modelName,
                             ".ssim.data/Scenario-",
                             scenarioId(initialScenario),
                             "/stsim_InitialConditionsSpatial/LUCAS_2001.tif"))
  
  # Calculate ncol and nrow for one tile 
  tileDimension <- classRaster %>% 
    ncell %>% 
    `/`(tileSize) %>% 
    sqrt %>% 
    ceiling
  
  # Generate smp grid
  smallGrid <- rast(ncols = tileDimension, nrows = tileDimension, vals = seq(tileDimension^2))
  crs(smallGrid) <- crs(classRaster)
  ext(smallGrid) <- ext(classRaster)
  
  bigGrid <- resample(smallGrid, classRaster, method = "near")
  maskedGrid <- mask(bigGrid, classRaster)
  
  # Takes a vector of sizes (input) and a maximum size per group (threshold) and
  # returns a vector of integers assigning the inputs to groups up to size threshold
  # - Used to consolidate tiling groups into more even groups
  consolidateGroups <- function(input, threshold) {
    # Initialized counters and output
    counter <- 1
    cumulator <- 0
    output <- integer(length(input))
    
    # For each input, decide whether or not to start a new group
    # Store that decision in output
    for(i in seq_along(input)) {
      cumulator <- cumulator + input[i]
      if(cumulator > threshold) {
        cumulator<- input[i]
        counter <- counter + 1
      }
      output[i] <- counter
    }
    
    return(output)
  }
  
  rcl <- freq(maskedGrid) %>% 
    as_tibble %>% 
    {if(!contig) arrange(.,count) else arrange(.,value)} %>% 
    mutate(newValue = consolidateGroups(count, tileSize)) %>% 
    select(value, newValue) %>% 
    as.matrix
  
  finalGrid <- classify(maskedGrid, rcl = rcl, filename = outputFilename, overwrite = TRUE, NAflag = -9999, datatype = "INT2S")
  
  # Create subscenario
  myScenario = scenario(myProject, 
                        scenario = "Spatial Multiprocessing",
                        folder = "Single-Cell Sub-Scenarios")
  
  datasheet(myScenario, name = "core_SpatialMultiprocessing", optional = TRUE, empty = T) %>% 
    addRow(data.frame(MaskFileName = outputFilename)) %>% 
    saveDatasheet(myScenario, data = ., name = "core_SpatialMultiprocessing")
  
}

# Update Outputs
myScenario <- scenario(myProject, 
                       scenario="SF Output Options and Filters [Only 2016]",
                       folder = "Single-Cell Sub-Scenarios",
                       sourceScenario = "SF Output Options and Filters [Add Methane]")

myDataOrig <- datasheet(myScenario, name = "stsim_OutputFilterFlows")

flowGroupsAdd1 <- c("Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)",
                    "Annual Net Ecosystem Carbon Balance (tons C per year)",
                    "Annual Emissions: CH4 (tons CO2-eq per year)",
                    "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
                    "Annual Net Growth (tons CO2-eq per year)",
                    "Annual Emissions: CO2 (tons CO2-eq per year)",
                    "Annual Lateral Flux (tons CO2-eq per year)",
                    "Annual Emissions: CH4 (tons C per year)",
                    "Annual Emissions: CO2 and CH4 (tons C per year)",
                    "Annual Net Growth (tons C per year)",
                    "Annual Emissions: CO2 (tons C per year)",
                    "Annual Lateral Flux (tons C per year)")

myDataOrig$Spatial <- FALSE
myDataOrig$AvgSpatial <- FALSE
myDataOrig$AvgSpatial[myDataOrig$FlowGroupId %in% flowGroupsAdd1] <- TRUE

saveDatasheet(myScenario, myDataOrig, "stsim_OutputFilterFlows", append = FALSE)

rm(myDataOrig,flowGroupsAdd1)

stockGroupsAdd1 <- c("Ecosystem Carbon Storage (tons C)",
                     "Biomass: Aboveground",
                     "Biomass: Belowground",
                     "DOM: Deadwood",
                     "DOM: Litter",
                     "DOM: Soil")

myDataOrig <- datasheet(myScenario, name = "stsim_OutputFilterStocks")

myDataOrig$Spatial <- FALSE
myDataOrig$AvgSpatial <- FALSE
myDataOrig$AvgSpatial[myDataOrig$StockGroupId %in% stockGroupsAdd1] <- TRUE

saveDatasheet(myScenario, myDataOrig, "stsim_OutputFilterStocks", append = FALSE)

rm(myDataOrig, stockGroupsAdd1)

myDataOrig <- datasheet(myScenario, name = "stsim_OutputOptionsStockFlow")

myDataOrig$SpatialOutputST <- "No"
myDataOrig$SpatialOutputFL <- "No"
myDataOrig$AvgSpatialOutputSTTimesteps <- 15
myDataOrig$AvgSpatialOutputFLTimesteps <- 1
myDataOrig$SummaryOutputFLOmitFromST <- TRUE
myDataOrig$SummaryOutputFLOmitToST <- TRUE

saveDatasheet(myScenario, myDataOrig, "stsim_OutputOptionsStockFlow", append = FALSE)

rm(myDataOrig, myScenario)

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

# Build Scenarios
# "SF Output Options [All]" keep for test area and remove when ready to run at scale
# Change to "SF Output Options and Filters [Only 2016]"

folder(ssimObject = myProject, 
       folder = "4. Final Spatial Scenarios")

# No LULC, Climate
myScenario <- scenario(myProject, 
                       scenario = "Basin Climate and No Land Cover Change",
                       folder = "4. Final Spatial Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Spatial Multiprocessing",
                            "Run Control [2001-2016; 1 MC; Spatial]",
                            "Output Options [Spatial; Summary]",
                            "STSM Initial Conditions [Spatial]",
                            "STSM Transition Pathways [Turn Off]",
                            "SF Flow Spatial Multipliers [PRISM Historical]",
                            "SF Output Options and Filters [Only 2016]",
                            "SF Flow Multipliers [PRISM, Mean, Add Prev Wetland]",
                            "Stock Limit [All]",
                            "Single Cell: Carbon and LULC: Mean")
                            
rm(myScenario)

# LULC, Climate
myScenario <- scenario(myProject, 
                       scenario = "Basin Baseline",
                       folder = "4. Final Spatial Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Spatial Multiprocessing",
                            "Run Control [2001-2016; 1 MC; Spatial]",
                            "Output Options [Spatial; Summary]",
                            "STSM Initial Conditions [Spatial]",
                            "STSM Spatial Multipliers [LA]",
                            "STSM Transition Multipliers [LA]",
                            "STSM Transition Pathways [LA]",
                            "SF Flow Spatial Multipliers [PRISM Historical]",
                            "SF Output Options and Filters [Only 2016]",
                            "SF Flow Multipliers [PRISM, Mean, Add Prev Wetland]",
                            "Stock Limit [All]",
                            "Single Cell: Carbon and LULC: Mean")

rm(myScenario)

# LULC, Climate
myScenario <- scenario(myProject, 
                       scenario = "Basin IPCC",
                       folder = "4. Final Spatial Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Spatial Multiprocessing",
                             "Run Control [2001-2016; 1 MC; Spatial]",
                             "Output Options [Spatial; Summary]",
                             "STSM Initial Conditions [Spatial]",
                             "STSM Spatial Multipliers [LA]",
                             "STSM Transition Multipliers [LA]",
                             "STSM Transition Pathways [LA]",
                             "SF Flow Spatial Multipliers [PRISM Historical]",
                             "SF Output Options and Filters [Only 2016]",
                             "SF Flow Multipliers [PRISM, Mean, Add Prev Wetland, IPCC]",
                             "Stock Limit [All]",
                             "Single Cell: Carbon and LULC: Mean")

rm(myScenario)

# LULC, Climate
myScenario <- scenario(myProject, 
                       scenario = "Basin Uncertainty Baseline",
                       folder = "4. Final Spatial Scenarios")

mergeDependencies(myScenario) <- F

dependency(myScenario) <- c("Spatial Multiprocessing",
                            "Run Control [2001-2016; 40 MC; Spatial]",
                            "Output Options [Spatial; Summary]",
                            "STSM Initial Conditions [Spatial]",
                            "STSM Spatial Multipliers [LA]",
                            "STSM Transition Multipliers [LA]",
                            "STSM Transition Pathways [LA]",
                            "SF Flow Spatial Multipliers [PRISM Historical]",
                            "SF Output Options and Filters [Only 2016]",
                            "Flow Multipliers [PRISM, Uncertainty, Add Prev Wetland]",
                            "Stock Limit [All]",
                            "Single Cell: Carbon and LULC: Uncertainty")

rm(myScenario)

# run(myProject, 
#     scenario="1 No Land Cover Change and Climate")
# 
# run(myProject, 
#     scenario="2 Land Cover Change and Climate")
# 
# run(myProject, 
#     scenario="3 Land Cover Change, Climate, Erosion")
# 
# run(myProject, 
#     scenario="5 Uncertainty: Land Cover Change and Climate")

