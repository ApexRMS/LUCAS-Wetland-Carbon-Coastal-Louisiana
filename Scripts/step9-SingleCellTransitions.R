# ApexRMS
# Updated 2025-02-04
# Run after step8-AddUplandForestScenario.R
# This script creates single-cell models for all transitions observed in the study area
# Saves Ecosystem Carbon Storage time-series figures
# In these figures, verify that all land cover types are initialized correctly
# And that carbon trajectories between land cover types are reasonable

library(rsyncrosim)
library(tidyverse)
library(terra)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

rootPath <- "E:/gitprojects/A329-LucasBarataria/"

dataPath = "Data/"
modelPath = "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")


folder(ssimObject = myProject, 
       folder = "Single-Cell Transitions",
       parentFolder = "3. Single-Cell Scenarios")

folder(ssimObject = myProject, 
       folder = "Single-Cell Transitions SubScenarios",
       parentFolder = "Single-Cell Transitions")

# Get list of current state classes in study area
stateClassTable <- datasheet(myProject, name = "stsim_StateClass")

lucas2001 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_2001.tif"))
lucas2006 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_2006.tif"))
lucas2010 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_2010.tif"))
lucas2016 <- rast(paste0(modelFullPath,"/Data/Initial Conditions/LUCAS/LUCAS_2016.tif"))

stateClassIds <- unique(c(freq(lucas2001)$value,
                          freq(lucas2006)$value,
                          freq(lucas2010)$value,
                          freq(lucas2016)$value))

stateClassIds <- c(stateClassIds,600)

rm(lucas2001,lucas2006,lucas2010,lucas2016)



stateClassList <- stateClassTable$Name[stateClassTable$Id %in% c(stateClassIds)]

for (i in 1:length(stateClassList)){
  
  # Initial Conditions Wetland: Estuarine Emergent
  myScenario <- scenario(myProject, 
                         scenario = paste0("Initial Conditions: Single Cell Transitions - ",stateClassList[i]),
                         folder = "Single-Cell Transitions SubScenarios")
  
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
                      StateClassId = stateClassList[i],
                      RelativeAmount = 1))
  saveDatasheet(myScenario, myData, sheetName, append = FALSE)
  
  rm(myScenario, myData, sheetName)
  
}

transitionTypeGroup <- datasheet(myProject,name = "stsim_TransitionTypeGroup")

transitionTypes <- unique(transitionTypeGroup$TransitionGroupId)
transitionTypes <- grep("Type",transitionTypes, value = T)

transitionTypesKeep <- grep("->",transitionTypes, value = T)

transitionsList <- transitionTypes[transitionTypes %in% transitionTypesKeep]

for (i in 1:length(transitionsList)){
  
  transitionName <- gsub(" [Type]","",gsub("LULCC: ","",transitionsList[i]),fixed = T)
  
  # Transition Multipliers: Forest and Wetland
  myScenario <- scenario(myProject, scenario=paste0("Transition Multipliers: ",transitionName),
                         folder = "Single-Cell Transitions SubScenarios")
  
  myData <- datasheet(myScenario, "stsim_TransitionMultiplierValue", optional = T, empty = T) %>%
    addRow(data.frame(Timestep = 0,
                      TransitionGroupId = transitionTypes,
                      Amount = 0)) %>%
    addRow(data.frame(Timestep = c(2020,2021),
                      TransitionGroupId = transitionsList[i],
                      Amount = c(1,0)))
  
  saveDatasheet(myScenario, myData, "stsim_TransitionMultiplierValue", append = FALSE)
  
  rm(myScenario,myData)
  
}

# Create all the scenarios and run them by either LA or No Forested Wetland Scenarios

myScen1 <- scenario(myProject,
                    scenario = "STSM Transition Pathways [No Forested Wetland]")
myPath1 <- datasheet(myScen1,"stsim_Transition")

myScen2 <- scenario(myProject,
                    scenario = "STSM Transition Pathways [LA]")
myPath2 <- datasheet(myScen2,"stsim_Transition")


for (i in 1:length(transitionsList)){
  
  transitionName <- gsub(" [Type]","",gsub("LULCC: ","",transitionsList[i]),fixed = T)
  
  myScenario <- scenario(myProject, 
                         scenario=paste0("Transition: ",transitionName),
                         folder = "Single-Cell Transitions")
  
  mergeDependencies(myScenario) <- F
  
  stateClassStart <- unlist(lapply(strsplit(transitionName, " -> "), "[[", 1))
  
  if (paste0("LULCC: ",transitionName) %in% myPath2$TransitionTypeId){
    
    dependency(myScenario) <- c("Run Control [2001-2100; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                paste0("Initial Conditions: Single Cell Transitions - ",stateClassStart),
                                paste0("Transition Multipliers: ",transitionName),
                                "STSM Transition Pathways [LA]",
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                                "Stock Limit [All]",
                                "Single Cell: Carbon and LULC: Mean")
    
    rm(myScenario)
    
  } else if (paste0("LULCC: ",transitionName) %in% myPath1$TransitionTypeId){
    
    dependency(myScenario) <- c("Run Control [2001-2100; 1 MC]",
                                "Output Options [Non-Spatial; Summary]",
                                "SF Output Options [All]",
                                paste0("Initial Conditions: Single Cell Transitions - ",stateClassStart),
                                paste0("Transition Multipliers: ",transitionName),
                                "STSM Transition Pathways [No Forested Wetland]",
                                "SF Flow Multipliers [No PRISM, Mean, Add Prev Wetland]",
                                "Stock Limit [All]",
                                "Single Cell: Carbon and LULC: Mean")
    
    rm(myScenario)
    
  }
  
  run(myProject, 
      scenario=paste0("Transition: ",transitionName))
  
  
}

pathOut <- paste0(rootPath,"Models/",modelName,"/OutputFigures/")

if(!dir.exists(pathOut)){
  dir.create(pathOut)
}

pathOutTransitions <- paste0(pathOut,"Transitions")

if(!dir.exists(pathOutTransitions)){
  dir.create(pathOutTransitions)
}

# remove / in forest type name

# Loop through each scenario and plot results
for (i in 1:length(transitionsList)){
  
  transitionName <- gsub(" [Type]","",gsub("LULCC: ","",transitionsList[i]),fixed = T)
  
  scenarioList <- scenario(myProject, summary = T, results = T)
  
  tId <- scenarioList$ScenarioId[grep(paste0("Transition: ",transitionName),scenarioList$Name)]
  
  myScenario <- scenario(myProject, scenario=max(tId))
  
  myDataStock <- datasheet(myScenario, "stsim_OutputStock")
  
  var1 <- "Ecosystem Carbon Storage (tons C)"
  plotNameVar <- "EcosystemCarbonStorage"
  
  plotName <- paste0(gsub("/","",gsub(":","",gsub("->","To",gsub(" ","",transitionName)))),
                     "_",
                     plotNameVar)
  
  myDataStockCarbonStorage <- myDataStock %>%
    filter(StockGroupId == var1) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC))
  
  p3 <- ggplot(myDataStockCarbonStorage, aes(x = Timestep, y = mean)) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.25, colour = NA) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nYear") + 
    ylab(paste0(gsub("(tons C)","",gsub(" [Type]","",var1, fixed = T), fixed = T),"\n(tons C per ha)\n"))+
    ylim(0,NA)
    #ggtitle(paste0(transitionName," in 2020"))
  
  p3
  
  ggsave(paste0(pathOutTransitions,"/",plotName,".png"), p3, width = 3.3, height = 3.4, dpi = 300)
  
  rm(myDataStockCarbonStorage,plotName)
  
}

 