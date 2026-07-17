# This script adds charts to the library
# ApexRMS
# Feb 2026

library(rsyncrosim)
library(tidyverse)

source(paste0(rootPath, "Scripts/gwpConfig.R"))

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
#chartCriteria(myProject)

# Chart objects are project-level (not scenario-level), so each GWP variant
# needs its own uniquely-named set of charts to avoid the two variants'
# charts colliding on the same shared object.
for (activeGWP in names(gwpVariants)) {
  gwpVariant <- gwpVariants[[activeGWP]]

  ## Carbon Fluxes: Growth, Emissions, Lateral Flux ----

  myChart <- chart(
    myProject,
    chart = paste0("1 ha Growth Emissions and Lateral Flux", " [", gwpVariant$label, "]")
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
        "Annual Lateral Flux (tons C per year)",
        "Annual Lateral Flux (tons CO2-eq per year)",
        "Annual Net Growth (tons C per year)",
        "Annual Net Growth (tons CO2-eq per year)"
      )
    )

  rm(myChart)

  ## Carbon Fluxes: Annual Net Ecosystem Carbon Balance ----
  myChart <- chart(myProject, chart = paste0("1 ha NECB", " [", gwpVariant$label, "]"))

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
    )

  rm(myChart)

  ## Carbon Stocks: Ecosystem Carbon Storage ----
  myChart <- chart(
    myProject,
    chart = paste0("1 ha Ecosystem Carbon Storage", " [", gwpVariant$label, "]")
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
    )

  rm(myChart)


  ## Carbon Stocks: IPCC ----

  myChart <- chart(myProject, chart = paste0("1 ha IPCC Carbon Stocks", " [", gwpVariant$label, "]"))

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
    )

  ## Carbon Stocks: LUCAS ----
  myChart <- chart(myProject, chart = paste0("1 ha LUCAS Carbon Stocks", " [", gwpVariant$label, "]"))

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
        "DOM: Snag Stem [Type]"
      )
    )

  rm(myChart)

  ## Land Cover Area ----
  myChart <- chart(myProject, chart = paste0("1 ha Land Cover Area", " [", gwpVariant$label, "]"))

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
    )

  rm(myChart)

  ## Carbon Fluxes: Growth, Emissions, Lateral Flux ----
  myChart <- chart(
    myProject,
    chart = paste0("Basin Growth Emissions and Lateral Flux", " [", gwpVariant$label, "]")
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
        "Annual Lateral Flux (tons C per year)",
        "Annual Lateral Flux (tons CO2-eq per year)",
        "Annual Net Growth (tons C per year)",
        "Annual Net Growth (tons CO2-eq per year)"
      )
    )

  rm(myChart)

  ## Carbon Fluxes: Annual Net Ecosystem Carbon Balance ----
  myChart <- chart(myProject, chart = paste0("Basin NECB", " [", gwpVariant$label, "]"))
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
    )

  rm(myChart)

  ## Carbon Stocks: Ecosystem Carbon Storage ----
  myChart <- chart(
    myProject,
    chart = paste0("Basin Ecosystem Carbon Storage", " [", gwpVariant$label, "]")
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
    )

  rm(myChart)

  ## Carbon Stocks: IPCC ----
  myChart <- chart(myProject, chart = paste0("Basin IPCC Carbon Stocks", " [", gwpVariant$label, "]"))

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
    )

  rm(myChart)

  ## Carbon Stocks: LUCAS ----
  myChart <- chart(myProject, chart = paste0("Basin LUCAS Carbon Stocks", " [", gwpVariant$label, "]"))

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
        "DOM: Snag Stem [Type]"
      )
    )

  rm(myChart)

  ## Land Cover Area ----

  #### Select the land cover types within the area

  # Something like this
  # Load Land Cover Data

  myScenarioR <- getScenarioExact(myProject, vTag("Basin Baseline", gwpVariant, style = "suffix"))

  tabLandR <- datasheet(myScenarioR, "stsim_OutputStratumState")

  stateClassUnique <- unique(tabLandR$StateClassId)

  myChart <- chart(myProject, chart = paste0("Basin Land Cover Area", " [", gwpVariant$label, "]"))

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
      addValue = as.character(stateClassUnique)
    )

  rm(myChart)
}

# Maps ----

# Add Manually
# Flows: CO2 Emissions in Mg C/ha/yr
# Flows: Lateral Flux in Mg C/ha/yr
# Flows: Methane Emissions in Mg C/ha/yr
# Flows: Net Ecosystem Carbon Balance in Mg C/ha/yr
# Flows: Net Ecosystem Carbon Balance in Mg CO2eq/ha/yr
# Flows: Net Growth in Mg C/ha/yr
# Land Cover
# Stocks: Ecosystem Carbon Storage in Mg C/ha
