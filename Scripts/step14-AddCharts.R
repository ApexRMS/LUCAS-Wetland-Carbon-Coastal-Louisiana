# This script adds charts to the library
# ApexRMS
# Feb 2026

library(rsyncrosim)
library(tidyverse)

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

# Specify file paths, library, and project

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(
  name = paste0(modelFullPath, "/", modelName, ".ssim"),
  session = mySession
)

myProject <- rsyncrosim::project(myLibrary, project = "Definitions")

# Look at Chart Criteria for the project
chartCriteria(myProject)

## Carbon Fluxes: Growth, Emissions, Lateral Flux ----

myChart <- chart(
  myProject,
  chart = "1 ha Carbon Fluxes: Growth, Emissions, Lateral Flux"
)

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_FlowGroup",
    timesteps = c(2002, 2124),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_FlowGroup", addFilter = "FlowGroupId") %>%
  chartInclude(
    variable = "stsim_FlowGroup",
    filter = "FlowGroupId",
    addValue = c(
      "Annual Emissions: CH4 (tons C per year)",
      "Annual Emissions: CH4 (tons CO2-eq per year)",
      "Annual Emissions: CO2 (tons C per year)",
      "Annual Emissions: CO2 (tons CO2-eq per year)",
      "Annual Emissions: CO2 and CH4 (tons C per year)",
      "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
      "Annual Lateral Flux (tons C per year)",
      "Annual Lateral Flux (tons CO2-eq per year)",
      "Annual Net Growth (tons C per year)",
      "Annual Net Growth (tons CO2-eq per year)"
    )
  ) %>%
  chartOptionsXAxis(title = "Year")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Carbon Fluxes: Annual Net Ecosystem Carbon Balance ----
myChart <- chart(myProject, chart = "1 ha Carbon Fluxes: NECB")

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_FlowGroup",
    timesteps = c(2002, 2124),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_FlowGroup", addFilter = "FlowGroupId") %>%
  chartInclude(
    variable = "stsim_FlowGroup",
    filter = "FlowGroupId",
    addValue = c(
      "Annual Net Ecosystem Carbon Balance (tons C per year)",
      "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)"
    )
  ) %>%
  chartOptionsXAxis(title = "Year")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Carbon Stocks: Ecosystem Carbon Storage ----
myChart <- chart(
  myProject,
  chart = "1 ha Carbon Stocks: Ecosystem Carbon Storage"
)

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_StockGroup",
    timesteps = c(2001, 2124),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_StockGroup", addFilter = "StockGroupId") %>%
  chartInclude(
    variable = "stsim_StockGroup",
    filter = "StockGroupId",
    addValue = "Ecosystem Carbon Storage (tons C)"
  ) %>%
  chartOptionsXAxis(title = "Year")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)


## Carbon Stocks: IPCC ----

myChart <- chart(myProject, chart = "1 ha Carbon Stocks: IPCC")

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_StockGroup",
    timesteps = c(2001, 2124),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_StockGroup", addFilter = "StockGroupId") %>%
  chartInclude(
    variable = "stsim_StockGroup",
    filter = "StockGroupId",
    addValue = c(
      "Biomass: Aboveground",
      "Biomass: Belowground",
      "DOM: Deadwood",
      "DOM: Litter",
      "DOM: Soil"
    )
  ) %>%
  chartOptionsXAxis(title = "Year") %>%
  chartOptionsYAxis(title = "tons C")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Carbon Stocks: LUCAS ----
myChart <- chart(myProject, chart = "1 ha Carbon Stocks: LUCAS")

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_StockGroup",
    timesteps = c(2001, 2124),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_StockGroup", addFilter = "StockGroupId") %>%
  chartInclude(
    variable = "stsim_StockGroup",
    filter = "StockGroupId",
    addValue = c(
      "Biomass: Coarse Root",
      "Biomass: Fine Root",
      "Biomass: Foliage",
      "Biomass: Merchantable",
      "Biomass: Other Wood",
      "Deep Soil",
      "DOM: Aboveground Fast",
      "DOM: Aboveground Medium",
      "DOM: Aboveground Slow",
      "DOM: Aboveground Very Fast",
      "DOM: Belowground Fast",
      "DOM: Belowground Slow",
      "DOM: Belowground Very Fast",
      "DOM: Snag Branch",
      "DOM: Snag Stem"
    )
  ) %>%
  chartOptionsXAxis(title = "Year") %>%
  chartOptionsYAxis(title = "tons C")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Land Cover Area ----
myChart <- chart(myProject, chart = "1 ha Land Cover Area")

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_StateClass",
    timesteps = c(2001, 2124),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_StateClass", addFilter = "StateClassId") %>%
  chartInclude(
    variable = "stsim_StateClass",
    filter = "StateClassId",
    addValue = c(
      "Forest: Oak/Gum/Cypress Group",
      "Wetland: Palustrine Forested",
      "Wetland: Palustrine Emergent",
      "Wetland: Estuarine Emergent"
    )
  ) %>%
  chartOptionsXAxis(title = "Year") %>%
  chartOptionsYAxis(title = "Area (ha)")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)

chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Carbon Fluxes: Growth, Emissions, Lateral Flux ----
myChart <- chart(
  myProject,
  chart = "Basin Carbon Fluxes: Growth, Emissions, Lateral Flux"
)

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_FlowGroup",
    timesteps = c(2002, 2016),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_FlowGroup", addFilter = "FlowGroupId") %>%
  chartInclude(
    variable = "stsim_FlowGroup",
    filter = "FlowGroupId",
    addValue = c(
      "Annual Emissions: CH4 (tons C per year)",
      "Annual Emissions: CH4 (tons CO2-eq per year)",
      "Annual Emissions: CO2 (tons C per year)",
      "Annual Emissions: CO2 (tons CO2-eq per year)",
      "Annual Emissions: CO2 and CH4 (tons C per year)",
      "Annual Emissions: CO2 and CH4 (tons CO2-eq per year)",
      "Annual Lateral Flux (tons C per year)",
      "Annual Lateral Flux (tons CO2-eq per year)",
      "Annual Net Growth (tons C per year)",
      "Annual Net Growth (tons CO2-eq per year)"
    )
  ) %>%
  chartOptionsXAxis(title = "Year")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Carbon Fluxes: Annual Net Ecosystem Carbon Balance ----
myChart <- chart(myProject, chart = "Basin Carbon Fluxes: NECB")
myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_FlowGroup",
    timesteps = c(2002, 2016),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_FlowGroup", addFilter = "FlowGroupId") %>%
  chartInclude(
    variable = "stsim_FlowGroup",
    filter = "FlowGroupId",
    addValue = c(
      "Annual Net Ecosystem Carbon Balance (tons C per year)",
      "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)"
    )
  ) %>%
  chartOptionsXAxis(title = "Year")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Carbon Stocks: Ecosystem Carbon Storage ----
myChart <- chart(
  myProject,
  chart = "Basin Carbon Stocks: Ecosystem Carbon Storage"
)

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_StockGroup",
    timesteps = c(2001, 2016),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_StockGroup", addFilter = "StockGroupId") %>%
  chartInclude(
    variable = "stsim_StockGroup",
    filter = "StockGroupId",
    addValue = "Ecosystem Carbon Storage (tons C)"
  ) %>%
  chartOptionsXAxis(title = "Year")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Carbon Stocks: IPCC ----
myChart <- chart(myProject, chart = "Basin Carbon Stocks: IPCC")

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_StockGroup",
    timesteps = c(2001, 2016),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_StockGroup", addFilter = "StockGroupId") %>%
  chartInclude(
    variable = "stsim_StockGroup",
    filter = "StockGroupId",
    addValue = c(
      "Biomass: Aboveground",
      "Biomass: Belowground",
      "DOM: Deadwood",
      "DOM: Litter",
      "DOM: Soil"
    )
  ) %>%
  chartOptionsXAxis(title = "Year") %>%
  chartOptionsYAxis(title = "tons C")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Carbon Stocks: LUCAS ----
myChart <- chart(myProject, chart = "Basin Carbon Stocks: LUCAS")

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_StockGroup",
    timesteps = c(2001, 2124),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_StockGroup", addFilter = "StockGroupId") %>%
  chartInclude(
    variable = "stsim_StockGroup",
    filter = "StockGroupId",
    addValue = c(
      "Biomass: Coarse Root",
      "Biomass: Fine Root",
      "Biomass: Foliage",
      "Biomass: Merchantable",
      "Biomass: Other Wood",
      "Deep Soil",
      "DOM: Aboveground Fast",
      "DOM: Aboveground Medium",
      "DOM: Aboveground Slow",
      "DOM: Aboveground Very Fast",
      "DOM: Belowground Fast",
      "DOM: Belowground Slow",
      "DOM: Belowground Very Fast",
      "DOM: Snag Branch",
      "DOM: Snag Stem"
    )
  ) %>%
  chartOptionsXAxis(title = "Year") %>%
  chartOptionsYAxis(title = "tons C")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

## Land Cover Area ----

#### Select the land cover types within the area

# Something like this
# Load Land Cover Data

scenarioListAll <- scenario(myProject, summary = T, results = T)

myScenarioR <- scenario(
  myProject,
  scenario = max(scenarioListAll$ScenarioId[grep(
    "Basin Baseline",
    scenarioListAll$Name
  )])
)

tabLandR <- datasheet(myScenarioR, "stsim_OutputStratumState")

stateClassUnique <- unique(tabLandR$StateClassId)

myChart <- chart(myProject, chart = "Basin Land Cover Area")

myChart %>%
  chartData(
    type = "Line",
    addY = "stsim_StateClass",
    timesteps = c(2001, 2016),
    iterationType = "Mean"
  ) %>%
  chartDisagg(variable = "stsim_StateClass", addFilter = "StateClassId") %>%
  chartInclude(
    variable = "stsim_StateClass",
    filter = "StateClassId",
    addValue = stateClassUnique
  ) %>%
  chartOptionsXAxis(title = "Year") %>%
  chartOptionsYAxis(title = "Area (ha)")

chartOptionsLegend(myChart, showScenarioId = FALSE, showTimestamp = FALSE)
chartOptionsFormat(
  myChart,
  noDataAsZero = TRUE,
  showDataPoints = FALSE,
  showDataPointsOnly = FALSE,
  showPanelTitles = TRUE,
  showToolTips = TRUE,
  showNoDataPanels = TRUE,
  lineWidth = 1
)

rm(myChart)

# Maps ----

# Add Manually
# Flows: CO2 Emissions in tons CO2-eq
# Flows: Lateral Flux in tons CO2-eq
# Flows: Methane Emissions in tons CO2-eq
# Flows: Net Ecosystem Carbon Balance in tons C
# Flows: Net Ecosystem Carbon Balance in tons CO2-eq
# Flows: Net Growth in tons CO2-eq
# Land Cover
# Stocks: Ecosystem Carbon Storage in tons C
