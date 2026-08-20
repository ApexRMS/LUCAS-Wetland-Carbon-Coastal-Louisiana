# ApexRMS
# Updated 2025-04-16
# Run after step11-SpatialMaps.R
# This script creates data release files

library(rsyncrosim)
library(tidyverse)
library(terra)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

dataPath <- "Data/"
modelPath <- "Models/"

modelFullPath <- paste0(rootPath, modelPath)

gwpModels

myLibrary_GWP100 <- ssimLibrary(file.path(
  modelFullPath,
  "Barataria LocalCH4 GWP-100",
  "Barataria LocalCH4 GWP-100.ssim"
))

myLibrary_GWP20 <- ssimLibrary(file.path(
  modelFullPath,
  "Barataria LocalCH4 GWP-20",
  "Barataria LocalCH4 GWP-20.ssim"
))


myProject_GWP100 <- rsyncrosim::project(
  myLibrary_GWP100,
  project = "Definitions"
)
myProject_GWP20 <- rsyncrosim::project(myLibrary_GWP20, project = "Definitions")

scenariosBasin <- c(
  "Uncertainty Baseline",
  "Uncertainty IPCC"
)

scenarioList_100 <- scenario(myProject_GWP100, summary = T, results = T)
scenarioList_20 <- scenario(myProject_GWP20, summary = T, results = T)

# Each Basin scenario/library combination to pull results from -
# 2 from GWP100 (Baseline, IPCC), 1 from GWP20 (Baseline only)

basinRuns <- list(
  list(
    project = myProject_GWP100,
    scenarioList = scenarioList_100,
    scenarioName = "Uncertainty Baseline",
    label = "GWP100",
    libraryName = "Barataria LocalCH4 GWP-100"
  ),
  list(
    project = myProject_GWP100,
    scenarioList = scenarioList_100,
    scenarioName = "Uncertainty IPCC",
    label = "IPCC Default",
    libraryName = "Barataria LocalCH4 GWP-100"
  ),
  list(
    project = myProject_GWP20,
    scenarioList = scenarioList_20,
    scenarioName = "Uncertainty Baseline",
    label = "GWP20",
    libraryName = "Barataria LocalCH4 GWP-20"
  )
)

basinRunLabels <- sapply(basinRuns, function(run) run$label)

pathOut <- paste0(modelFullPath, "JointOutputs", "/Output Data Release/")


# Create File Structure

if (!dir.exists(pathOut)) {
  dir.create(pathOut)
}

pathOutSpatial <- paste0(pathOut, "Spatial Data/")
if (!dir.exists(pathOutSpatial)) {
  dir.create(pathOutSpatial)
}

pathOutTabular <- paste0(pathOut, "Tabular Data/")
if (!dir.exists(pathOutTabular)) {
  dir.create(pathOutTabular)
}

pathOutSpatialSub <- paste0(pathOutSpatial, basinRunLabels)

for (i in 1:length(pathOutSpatialSub)) {
  if (!dir.exists(pathOutSpatialSub[i])) {
    dir.create(pathOutSpatialSub[i])
  }
}

# Extract ID names for Spatial Data

stockGroupIDs <- datasheet(
  myProject_GWP100,
  name = "stsim_StockGroup",
  includeKey = T
)
flowGroupIDs <- datasheet(
  myProject_GWP100,
  name = "stsim_FlowGroup",
  includeKey = T
)

keepStocks1 <- c(
  "Biomass: Aboveground",
  "Biomass: Belowground",
  "DOM: Deadwood",
  "DOM: Litter",
  "DOM: Soil"
)

keepStocks2 <- c(
  "Biomass: Coarse Root [Type]",
  "Biomass: Fine Root [Type]",
  "Biomass: Foliage [Type]",
  "Biomass: Merchantable [Type]",
  "Biomass: Other Wood [Type]",
  "Deep Soil [Type]",
  "DOM: Aboveground Fast [Type]",
  "DOM: Aboveground Medium [Type]",
  "DOM: Aboveground Slow [Type]",
  "DOM: Aboveground Very Fast [Type]",
  "DOM: Belowground Fast [Type]",
  "DOM: Belowground Slow [Type]",
  "DOM: Belowground Very Fast [Type]",
  "DOM: Snag Branch [Type]",
  "DOM: Snag Stem [Type]",
  "Ecosystem Carbon Storage (tons C)"
)

keepStocks2NoType <- gsub(" [Type]", "", keepStocks2, fixed = T)

keepFluxes <- c(
  "Annual Emissions: CO2 (tons C per year)",
  "Annual Emissions: CO2 (tons CO2-eq per year)",
  "Annual Emissions: CO2 and CH4 (tons C per year)",
  "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
  "Annual Net Growth (tons C per year)",
  "Annual Net Growth (tons CO2-eq per year)",
  "Annual Emissions: CH4 (tons C per year)",
  "Annual Emissions: CH4 (tons CO2-eq per year)",
  "Annual Lateral Flux (tons C per year)",
  "Annual Lateral Flux (tons CO2-eq per year)",
  "Annual Net Ecosystem Carbon Balance (tons C per year)",
  "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)"
)

lookupName <- data.frame(
  Name = c(
    "Ecosystem Carbon Storage (tons C)",
    "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)",
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
    "Annual Lateral Flux (tons C per year)"
  ),
  NameShort = c(
    "EcoStorage",
    "AnnNECBCO2e",
    "AnnNECBMgC",
    "AnnCH4CO2e",
    "AnnTotEmissCO2e",
    "AnnGrowCO2e",
    "AnnCO2CO2e",
    "AnnLatCO2e",
    "AnnCH4MgC",
    "AnnTotEmissMgC",
    "AnnGrowMgC",
    "AnnCO2MgC",
    "AnnLatMgC"
  )
)

landToChange <- c(
  "Agriculture: Cropland",
  "Developed: Medium Intensity",
  "Water: All",
  "Water: Previously Emergent Wetland",
  "Water: Previously Forested Wetland",
  "Wetland: Unvegetated Emergent",
  "Wetland: Unvegetated Forested",
  "Grassland: Annual",
  "Shrubland: Non-sage"
)

keepFluxesSpatial <- keepFluxes

keepStocksSpatial <- c("Ecosystem Carbon Storage (tons C)")

# Loop through all Basin Scenario/Library combinations
# (Tabular Data only - list-based so runs from both libraries combine at the end)

tabLandList <- list()
tabStockList <- list()
tabFluxList <- list()

for (i in 1:length(basinRuns)) {
  thisRun <- basinRuns[[i]]

  scenID <- thisRun$scenarioList$ScenarioId[grep(
    paste0("Basin ", thisRun$scenarioName),
    thisRun$scenarioList$Name
  )]

  myScenario <- scenario(thisRun$project, scenario = max(scenID))

  # Load Land Cover Data
  tabLand <- datasheet(myScenario, "stsim_OutputStratumState")

  tabLandList[[i]] <- tabLand %>%
    select(
      -c(
        StateLabelXId,
        StateLabelYId,
        AgeMin,
        AgeMax,
        AgeClass,
        StratumId,
        SecondaryStratumId,
        ResolutionId
      )
    ) %>%
    rename(Year = Timestep) %>%
    mutate(
      StateClass = case_when(
        StateClassId == "Agriculture: Cropland" ~ "Agriculture",
        StateClassId == "Developed: Medium Intensity" ~ "Developed",
        StateClassId == "Grassland: Annual" ~ "Grassland",
        StateClassId == "Shrubland: Non-sage" ~ "Shrubland",
        StateClassId == "Water: All" ~ "Water",
        StateClassId == "Water: Previously Emergent Wetland" ~ "Water",
        StateClassId == "Water: Previously Forested Wetland" ~ "Water",
        StateClassId ==
          "Wetland: Unvegetated Emergent" ~ "Wetland: Unconsolidated Shore",
        StateClassId ==
          "Wetland: Unvegetated Forested" ~ "Wetland: Unconsolidated Shore",
        !(StateClassId %in% landToChange) ~ StateClassId
      )
    ) %>%
    select(-StateClassId) %>%
    group_by(Iteration, Year, StateClass) %>%
    filter(Iteration == 1) %>% # data is identical for all iterations
    summarize(Area_ha = sum(Amount)) %>%
    ungroup() %>%
    mutate(Scenario = thisRun$label)

  rm(tabLand)

  # Load Stock Data
  tabStock <- datasheet(myScenario, "stsim_OutputStock")

  tabStockList[[i]] <- tabStock %>%
    select(-c(StratumId, SecondaryStratumId, ResolutionId)) %>%
    filter(StockGroupId %in% c(keepStocks1, keepStocks2)) %>%
    rename(Year = Timestep, StockGroup = StockGroupId) %>%
    mutate(
      StateClass = case_when(
        StateClassId == "Agriculture: Cropland" ~ "Agriculture",
        StateClassId == "Developed: Medium Intensity" ~ "Developed",
        StateClassId == "Grassland: Annual" ~ "Grassland",
        StateClassId == "Shrubland: Non-sage" ~ "Shrubland",
        StateClassId == "Water: All" ~ "Water",
        StateClassId == "Water: Previously Emergent Wetland" ~ "Water",
        StateClassId == "Water: Previously Forested Wetland" ~ "Water",
        StateClassId ==
          "Wetland: Unvegetated Emergent" ~ "Wetland: Unconsolidated Shore",
        StateClassId ==
          "Wetland: Unvegetated Forested" ~ "Wetland: Unconsolidated Shore",
        !(StateClassId %in% landToChange) ~ StateClassId
      )
    ) %>%
    select(-StateClassId) %>%
    mutate(StockGroup = gsub(" [Type]", "", StockGroup, fixed = T)) %>%
    group_by(Year, StateClass, StockGroup, Iteration) %>%
    summarize(total = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
    group_by(Year, StateClass, StockGroup) %>%
    summarize(
      Amount_MgC = mean(total, na.rm = TRUE),
      Low = quantile(total, 0.025, na.rm = TRUE),
      High = quantile(total, 0.975, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Scenario = thisRun$label)

  rm(tabStock)

  # Load Flux Data
  tabFlux <- datasheet(myScenario, "stsim_OutputFlow")

  # should be true
  table(tabFlux$ToStateClassId == tabFlux$FromStateClassId)

  tabFluxList[[i]] <- tabFlux %>%
    select(
      -c(
        FromStockTypeId,
        TransitionTypeId,
        ToStratumId,
        ToStateClassId,
        ToStockTypeId,
        EndStratumId,
        EndSecondaryStratumId,
        EndStateClassId,
        EndMinAge,
        FromSecondaryStratumId,
        FromStratumId,
        ResolutionId
      )
    ) %>%
    filter((FlowGroupId %in% keepFluxes)) %>%
    rename(Year = Timestep, FlowGroup = FlowGroupId, Amount1 = Amount) %>%
    mutate(
      StateClass = case_when(
        FromStateClassId == "Agriculture: Cropland" ~ "Agriculture",
        FromStateClassId == "Developed: Medium Intensity" ~ "Developed",
        FromStateClassId == "Grassland: Annual" ~ "Grassland",
        FromStateClassId == "Shrubland: Non-sage" ~ "Shrubland",
        FromStateClassId == "Water: All" ~ "Water",
        FromStateClassId == "Water: Previously Emergent Wetland" ~ "Water",
        FromStateClassId == "Water: Previously Forested Wetland" ~ "Water",
        FromStateClassId ==
          "Wetland: Unvegetated Emergent" ~ "Wetland: Unconsolidated Shore",
        FromStateClassId ==
          "Wetland: Unvegetated Forested" ~ "Wetland: Unconsolidated Shore",
        !(FromStateClassId %in% landToChange) ~ FromStateClassId
      )
    ) %>%
    select(-FromStateClassId) %>%
    group_by(Year, StateClass, FlowGroup, Iteration) %>%
    summarize(totalC = sum(Amount1, na.rm = T), .groups = "drop") %>%
    group_by(Year, StateClass, FlowGroup) %>%
    summarize(
      Amount = mean(totalC, na.rm = TRUE),
      Low = quantile(totalC, 0.025, na.rm = TRUE),
      High = quantile(totalC, 0.975, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Scenario = thisRun$label)

  rm(tabFlux, myScenario, scenID, thisRun)
}

tabLandSub <- bind_rows(tabLandList)
tabStockSub <- bind_rows(tabStockList)
tabFluxSub <- bind_rows(tabFluxList)

rm(tabLandList, tabStockList, tabFluxList)

# Loop through all Basin Scenario/Library combinations
# (Spatial Data only)

for (i in 1:length(basinRuns)) {
  thisRun <- basinRuns[[i]]

  scenID <- thisRun$scenarioList$ScenarioId[grep(
    paste0("Basin ", thisRun$scenarioName),
    thisRun$scenarioList$Name
  )]

  myScenario <- scenario(thisRun$project, scenario = max(scenID))

  stockGroupIDsRun <- datasheet(
    thisRun$project,
    name = "stsim_StockGroup",
    includeKey = T
  )
  flowGroupIDsRun <- datasheet(
    thisRun$project,
    name = "stsim_FlowGroup",
    includeKey = T
  )

  libraryDataPath <- file.path(
    modelFullPath,
    thisRun$libraryName,
    paste0(thisRun$libraryName, ".ssim.data"),
    paste0("Scenario-", scenarioId(myScenario))
  )

  # Spatial Data

  pathOutSpatialAll <- paste0(pathOutSpatial, thisRun$label)

  # Land Cover Area

  listLandCover <- list.files(
    file.path(libraryDataPath, "stsim_OutputSpatialState"),
    pattern = ".tif",
    full.names = T
  )

  file.copy(listLandCover, pathOutSpatialAll)

  rm(listLandCover)

  # Carbon Stocks

  listStocks <- list.files(
    file.path(libraryDataPath, "stsim_OutputAverageSpatialStockGroup"),
    pattern = ".tif",
    full.names = T
  )

  for (j in 1:length(keepStocksSpatial)) {
    outName <- lookupName$NameShort[lookupName$Name == keepStocksSpatial[j]]

    stockId <- stockGroupIDsRun %>%
      filter(Name == keepStocksSpatial[j]) %>%
      pull(StockGroupId)

    listStocksSub <- grep(stockId, listStocks, value = T)

    listStocksSubFiles <- gsub(
      paste0(
        file.path(libraryDataPath, "stsim_OutputAverageSpatialStockGroup"),
        "/"
      ),
      "",
      listStocksSub
    )

    listStocksSubFiles <- gsub(stockId, outName, listStocksSubFiles)

    file.copy(listStocksSub, paste0(pathOutSpatialAll, "/", listStocksSubFiles))

    rm(stockId, listStocksSub, outName, listStocksSubFiles)
  }

  rm(listStocks)

  # Carbon Fluxes

  listFluxes <- list.files(
    file.path(libraryDataPath, "stsim_OutputAverageSpatialFlowGroup"),
    pattern = ".tif",
    full.names = T
  )

  for (k in 1:length(keepFluxesSpatial)) {
    outName <- lookupName$NameShort[lookupName$Name == keepFluxesSpatial[k]]

    fluxId <- flowGroupIDsRun %>%
      filter(Name == keepFluxesSpatial[k]) %>%
      pull(FlowGroupId)

    listFluxesSub <- grep(fluxId, listFluxes, value = T)

    listFluxesSubFiles <- gsub(
      paste0(
        file.path(libraryDataPath, "stsim_OutputAverageSpatialFlowGroup"),
        "/"
      ),
      "",
      listFluxesSub
    )

    listFluxesSubFiles <- gsub(fluxId, outName, listFluxesSubFiles)

    file.copy(listFluxesSub, paste0(pathOutSpatialAll, "/", listFluxesSubFiles))

    rm(fluxId, listFluxesSub, outName, listFluxesSubFiles)
  }

  rm(
    listFluxes,
    stockGroupIDsRun,
    flowGroupIDsRun,
    libraryDataPath,
    myScenario,
    scenID,
    thisRun
  )
}


# save stock csvs
write.csv(
  tabLandSub,
  paste0(pathOutTabular, "LandCoverArea_Basin.csv"),
  row.names = F
)

tabStockSubIPCC <- tabStockSub %>%
  filter(StockGroup %in% c(keepStocks1)) %>%
  mutate(StockGroup = str_replace(StockGroup, "\\(tons", "\\(Mg")) %>%
  arrange(Year, StateClass, StockGroup)

tabStockSubLUCAS <- tabStockSub %>%
  filter(StockGroup %in% c(keepStocks2NoType)) %>%
  mutate(StockGroup = str_replace(StockGroup, "\\(tons", "\\(Mg")) %>%
  arrange(Year, StateClass, StockGroup)

write.csv(
  tabStockSubIPCC,
  paste0(pathOutTabular, "CarbonStocksIPCC_Basin.csv"),
  row.names = F
)

write.csv(
  tabStockSubLUCAS,
  paste0(pathOutTabular, "CarbonStocksLUCAS_Basin.csv"),
  row.names = F
)

write.csv(
  tabStockSub %>%
    arrange(Year, StateClass, StockGroup),
  paste0(pathOutTabular, "CarbonStocksAll_Basin.csv"),
  row.names = F
)

# save fluxe csvs
tabFluxSub <- tabFluxSub %>%
  mutate(FlowGroup = str_replace(FlowGroup, "\\(tons", "\\(Mg")) %>%
  arrange(Year, StateClass, FlowGroup)

write.csv(
  tabFluxSub,
  paste0(pathOutTabular, "CarbonFluxes_Basin.csv"),
  row.names = F
)

rm(tabFluxSub, tabStockSubLUCAS, tabStockSubIPCC, tabLandSub)

stockGroupIDs %>%
  filter(Name %in% keepStocksSpatial) %>%
  select(-Description)

flowGroupIDs %>%
  filter(Name %in% keepFluxesSpatial) %>%
  select(-Description)

# Extent, Project, Spatial Resolution

scenID <- max(scenarioListAll$ScenarioId[grep(
  paste0("Basin ", scenariosBasin[1]),
  scenarioListAll$Name
)])

r1 <- rast(paste0(
  rootPath,
  "Models/",
  modelName,
  "/",
  modelName,
  ".ssim.data/Scenario-",
  scenID,
  "/stsim_OutputSpatialState/sc.it1.ts2001.tif"
))

r1
