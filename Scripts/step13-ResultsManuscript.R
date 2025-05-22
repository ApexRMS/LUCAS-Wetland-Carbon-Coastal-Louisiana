library(rsyncrosim)
library(tidyverse)
library(ggplot2)

options(scipen = 999)
old <- options(pillar.sigfig = 10)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

rootPath <- "E:/gitprojects/A329-LucasBarataria/"

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

pathOut <- paste0(rootPath,"Models/",modelName,"/OutputFigures/")

pathOutSpatial <- paste0(pathOut,"Spatial/")

if(!dir.exists(pathOutSpatial)){
  dir.create(pathOutSpatial)
}

pathOutManuscript <- paste0(pathOutSpatial,"Manuscript")

if(!dir.exists(pathOutManuscript)){
  dir.create(pathOutManuscript)
}

scenarioList <- scenario(myProject, summary = T, results = T)

id1 <- scenarioList$ScenarioId[grep("1 No Land Cover Change and Climate",scenarioList$Name)]
id2 <- scenarioList$ScenarioId[grep("2 Land Cover Change and Climate",scenarioList$Name)]
id3 <- scenarioList$ScenarioId[grep("3 Land Cover Change, Climate, Erosion",scenarioList$Name)]
id4 <- scenarioList$ScenarioId[grep("4 Land Cover Change, Climate, and No Forested Wetland",scenarioList$Name)]

myScenario1 <- scenario(myProject, scenario=max(id1))
myScenario2 <- scenario(myProject, scenario=max(id2))
myScenario3 <- scenario(myProject, scenario=max(id3))
myScenario4 <- scenario(myProject, scenario=max(id4))

# Summarize Total Ecosystem Carbon

myDataStock1 <- datasheet(myScenario1, "stsim_OutputStock")
myDataStock2 <- datasheet(myScenario2, "stsim_OutputStock")
myDataStock3 <- datasheet(myScenario3, "stsim_OutputStock")
myDataStock4 <- datasheet(myScenario4, "stsim_OutputStock")

stocksKeep <- c("DOM: Soil",
                "Ecosystem Carbon Storage (tons C)",
                "Atmosphere [Type]",
                "Atmosphere Temp [Type]",
                "Atmosphere: CH4 [Type]",
                "Atmosphere: CO [Type]",
                "Atmosphere: CO2 [Type]",
                "Aquatic [Type]")

stocksAtm <- c("Atmosphere [Type]",
                "Atmosphere Temp [Type]",
                "Atmosphere: CH4 [Type]",
                "Atmosphere: CO [Type]",
                "Atmosphere: CO2 [Type]")

myDataStock1s <- myDataStock1 %>%
  filter(StockGroupId %in% stocksKeep) %>%
  filter(Timestep == 2016) %>%
  mutate(StockGroupId2 = case_when(StockGroupId %in% stocksAtm ~ "Atmosphere",
                                   !(StockGroupId %in% stocksAtm)~StockGroupId)) %>%
  group_by(StockGroupId2,StateClassId) %>%
  summarize(AmountT = sum(Amount,na.rm = T)) %>%
  ungroup()



# For differences use KY/TN

# For changes in NECB 

# Loop over each time period
# Read in stack of NECB values for each time period
# Look to see which pixels transitioned in that time period and create a mask of 0's and 1's
# multiply by necb in each time period and sum over time
# Sum for just the one time period
# Sum for the whole time period

# 

