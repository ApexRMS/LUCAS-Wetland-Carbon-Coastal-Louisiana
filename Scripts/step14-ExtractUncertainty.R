# ApexRMS
# Updated 2026-02-26
# This script extracts uncertainty estimates for the results section

library(rsyncrosim)
library(tidyverse)

source(paste0(rootPath, "Scripts/gwpConfig.R"))

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

uncertaintyResultsList <- list()

for (activeGWP in names(gwpVariants)) {
  gwpVariant <- gwpVariants[[activeGWP]]

  myScenario <- getScenarioExact(
    myProject,
    vTag("Basin Uncertainty Baseline", gwpVariant, style = "suffix")
  )

  # Compare results with mean model

  # scenID <- scenarioListAll$ScenarioId[grep(
  #   "Basin Baseline",
  #   scenarioListAll$Name
  # )]

  # Load Flux Data
  tabFlux <- datasheet(
    myScenario,
    "stsim_OutputFlow",
    filterColumn = "FlowGroupId",
    filterValue = "Annual Net Ecosystem Carbon Balance (tons C per year)"
  )

  # should be true
  print(table(tabFlux$ToStateClassId == tabFlux$FromStateClassId))

  print(names(tabFlux))

  print(unique(tabFlux$ToStateClassId))

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
    ) %>%
    mutate(Group = "Dev")

  tabFluxEco <- tabFlux %>%
    filter(!(ToStateClassId %in% stateClassDev)) %>%
    group_by(Iteration) %>%
    summarize(totalC = (sum(Amount, na.rm = T) / 1000000)) %>%
    ungroup() %>%
    summarize(
      mean = mean(totalC, na.rm = T),
      low = quantile(totalC, 0.025, na.rm = T),
      high = quantile(totalC, 0.975, na.rm = T)
    ) %>%
    mutate(Group = "Eco")

  tabFluxNet <- tabFlux %>%
    group_by(Iteration) %>%
    summarize(totalC = (sum(Amount, na.rm = T) / 1000000)) %>%
    ungroup() %>%
    summarize(
      mean = mean(totalC, na.rm = T),
      low = quantile(totalC, 0.025, na.rm = T),
      high = quantile(totalC, 0.975, na.rm = T)
    ) %>%
    mutate(Group = "Net")

  print(tabFluxDev)

  print(tabFluxEco)

  print(tabFluxNet)

  print(as.data.frame(tabFluxDev))

  print(as.data.frame(tabFluxEco))

  print(as.data.frame(tabFluxNet))

  print(table(tabFlux$Iteration))
  print(length(unique(tabFlux$Iteration)))

  # Combine this variant's three summaries and tag with the active GWP so the
  # final data frame (below) can distinguish GWP-100 vs GWP-20 rows.
  tabFluxCombined <- bind_rows(tabFluxDev, tabFluxEco, tabFluxNet) %>%
    mutate(GWP = gwpVariant$label)

  uncertaintyResultsList[[activeGWP]] <- tabFluxCombined
}

uncertaintyResultsAll <- bind_rows(uncertaintyResultsList)

uncertaintyResultsAll
