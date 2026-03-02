# ApexRMS
# Updated 2026-02-26
# This script extracts uncertainty estimates for the results section

library(rsyncrosim)
library(tidyverse)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(
  name = paste0(modelFullPath, "/", modelName, ".ssim"),
  session = mySession
)

myProject <- rsyncrosim::project(myLibrary, project = "Definitions")

scenarioListAll <- scenario(myProject, summary = T, results = T)

scenID <- scenarioListAll$ScenarioId[grep(
  "Basin Uncertainty Baseline",
  scenarioListAll$Name
)]

# Compare results with mean model

# scenID <- scenarioListAll$ScenarioId[grep(
#   "Basin Baseline",
#   scenarioListAll$Name
# )]

myScenario <- scenario(myProject, scenario = max(scenID))

# Load Flux Data
tabFlux <- datasheet(
  myScenario,
  "stsim_OutputFlow",
  filterColumn = "FlowGroupId",
  filterValue = "Annual Net Ecosystem Carbon Balance (tons C per year)"
)

# should be true
table(tabFlux$ToStateClassId == tabFlux$FromStateClassId)

names(tabFlux)

unique(tabFlux$ToStateClassId)

stateClassDev <- c(
  "Agriculture: Cropland",
  "Developed: Medium Intensity",
  "Water: All",
  "Wetland: Unconsolidated Shore",
  "Water: Previously Emergent Wetland",
  "Water: Previously Forested Wetland",
  "Wetland: Unvegetated Emergent",
  "Wetland: Unvegetated Forested"
)

tabFluxDev <- tabFlux %>%
  filter(ToStateClassId %in% stateClassDev) %>%
  group_by(Iteration) %>%
  summarize(totalC = (sum(Amount, na.rm = T) / 1000000)) %>%
  ungroup() %>%
  summarize(
    mean = mean(totalC, na.rm = T),
    low = quantile(totalC, 0.025, na.rm = T),
    high = quantile(totalC, 0.975, na.rm = T)
  )

tabFluxEco <- tabFlux %>%
  filter(!(ToStateClassId %in% stateClassDev)) %>%
  group_by(Iteration) %>%
  summarize(totalC = (sum(Amount, na.rm = T) / 1000000)) %>%
  ungroup() %>%
  summarize(
    mean = mean(totalC, na.rm = T),
    low = quantile(totalC, 0.025, na.rm = T),
    high = quantile(totalC, 0.975, na.rm = T)
  )

tabFluxNet <- tabFlux %>%
  group_by(Iteration) %>%
  summarize(totalC = (sum(Amount, na.rm = T) / 1000000)) %>%
  ungroup() %>%
  summarize(
    mean = mean(totalC, na.rm = T),
    low = quantile(totalC, 0.025, na.rm = T),
    high = quantile(totalC, 0.975, na.rm = T)
  )


tabFluxDev

tabFluxEco

tabFluxNet

as.data.frame(tabFluxDev)

as.data.frame(tabFluxEco)

as.data.frame(tabFluxNet)

table(tabFlux$Iteration)
length(unique(tabFlux$Iteration))
