# Creates the Barataria Basin model
# ApexRMS
# Nov 2025

rootPath <- "E:/gitprojects/LUCAS-Wetland-Carbon-Coastal-Louisiana/"
scriptsPath <- paste0(rootPath, "Scripts/")

varsKeep <- c("scriptsPath", "varsKeep", "rootPath", "activeGWP")

source(paste0(scriptsPath, "gwpConfig.R"))

# 0. Pre-process wetland data
source(paste0(scriptsPath, "step0-WetlandEmergentStockFlowParametersNoNee.R"))
rm(list = setdiff(ls(), varsKeep))
detach(package:tidyverse)

# 1. Build the base model
source(paste0(scriptsPath, "step1-LibraryCreation.R"))
rm(list = setdiff(ls(), varsKeep))

# 2a-9. Build the wetland sub-models, spinups, and spatial scenarios for both
# GWP variants. Steps that are fully GWP-agnostic (2b, 2c, 3c, 6) only need to
# run once, on the first pass.
for (activeGWP in names(gwpVariants)) {

  # 2a. Build the base wetland sub-models
  source(paste0(scriptsPath, "step2a-ScenarioCreationEmergentForest.R"))
  rm(list = setdiff(ls(), varsKeep))
  detach(package:tidyverse)

  if (activeGWP == "GWP100") {
    # 2b. Update the flow multipliers and state attributes for the wetland sub-models
    source(paste0(scriptsPath, "step2b-ScenarioCreationWetland.R"))
    rm(list = setdiff(ls(), varsKeep))

    # 2c. Add the emergent wetland uncertainty parameters
    source(paste0(scriptsPath, "step2c-AddUncertainty.R"))
    rm(list = setdiff(ls(), varsKeep))
  } else {
    message("Skipping step2b/step2c — GWP-agnostic, already built on the GWP100 pass")
  }

  # 2d. Spin-up the mean forested wetland model
  source(paste0(scriptsPath, "step2d-ForestSpinups.R"))
  rm(list = setdiff(ls(), varsKeep))

  # 3a. Add uncertainty to forested wetland model
  source(paste0(scriptsPath, "step3a-ForestSpinupsUncertainty.R"))
  rm(list = setdiff(ls(), varsKeep))

  # 3b. Spin-up the Ag model
  source(paste0(scriptsPath, "step3b-AgSpinups.R"))
  rm(list = setdiff(ls(), varsKeep))

  if (activeGWP == "GWP100") {
    # 3c. Add lateral flux uncertainty to forested wetland model
    source(paste0(scriptsPath, "step3c-AddUncertaintyForestedWetland.R"))
    rm(list = setdiff(ls(), varsKeep))
  } else {
    message("Skipping step3c — GWP-agnostic, already built on the GWP100 pass")
  }

  # 3d. Add parameters for water previously wetland classes
  source(paste0(scriptsPath, "step3d-AddWaterPrevWetland.R"))
  rm(list = setdiff(ls(), varsKeep))

  # 4. Run single-cell scenarios for validation
  source(paste0(scriptsPath, "step4-FinalScenarios.R"))
  rm(list = setdiff(ls(), varsKeep))

  # 5a. Create charts (png files) for single-cell scenarios
  source(paste0(scriptsPath, "step5a-SingleCellPlots.R"))
  rm(list = setdiff(ls(), varsKeep))

  # 5b. Create charts (png files) for single cell transition scenarios
  source(paste0(scriptsPath, "step5b-SingleCellPlots.R"))
  rm(list = setdiff(ls(), varsKeep))
  detach(package:rsyncrosim)
  detach(package:tidyverse)

  if (activeGWP == "GWP100") {
    # 6. Create spatial input data
    source(paste0(scriptsPath, "step6-AddSpatialExampleBasin.R"))
    rm(list = setdiff(ls(), varsKeep))
    detach(package:tidyverse)
    detach(package:sf)
    detach(package:terra)
  } else {
    message("Skipping step6 — GWP-agnostic, already built on the GWP100 pass")
  }

  # 7. Add spatial scenarios
  source(paste0(scriptsPath, "step7-AddSpatialExampleToLibrary.R"))
  rm(list = setdiff(ls(), varsKeep))

  # 8. Add spatial scenarios - no forested wetland
  source(paste0(scriptsPath, "step8-AddUplandForestScenario.R"))
  rm(list = setdiff(ls(), varsKeep))

  # 9. Add single-cell models for all transitions in spatial scenarios
  source(paste0(scriptsPath, "step9-SingleCellTransitions.R"))
  rm(list = setdiff(ls(), varsKeep))
  detach(package:rsyncrosim)
  detach(package:tidyverse)
  detach(package:terra)
}

# Increase the size of your instance and run spatial scenarios using SyncroSim Studio

# Steps 10-14 are resumed in a new R session (the activeGWP loop state above is
# gone), so each of these scripts loops internally over both GWP variants.

# 10. Create charts (png files) for spatial scenarios: carbon
source(paste0(scriptsPath, "step10-SpatialFigures.R"))
rm(list = setdiff(ls(), varsKeep))
detach(package:rsyncrosim)
detach(package:tidyverse)
detach(package:ggplot2)
detach(package:terra)

# 11. Create maps (png files) of spatial scenarios
source(paste0(scriptsPath, "step11-SpatialMaps.R"))
rm(list = setdiff(ls(), varsKeep))
detach(package:rsyncrosim)
detach(package:tidyverse)
detach(package:terra)
detach(package:viridis)

# 12. Create data release files
source(paste0(scriptsPath, "step12-DataRelease.R"))
rm(list = setdiff(ls(), varsKeep))
detach(package:rsyncrosim)
detach(package:tidyverse)
detach(package:terra)

# 13. Create charts (png files) for spatial scenarios: land cover
source(paste0(scriptsPath, "step13-FiguresManuscriptLand.R"))
rm(list = setdiff(ls(), varsKeep))
detach(package:rsyncrosim)
detach(package:tidyverse)
detach(package:ggplot2)
detach(package:terra)

# 13 Create figures for manuscript: single-cell charts
source(paste0(scriptsPath, "step13-FiguresManuscriptSingleCell.R"))
rm(list = setdiff(ls(), varsKeep))
detach(package:rsyncrosim)
detach(package:tidyverse)
detach(package:viridis)

# 13. Create figures for manuscript: spatial scenario charts
source(paste0(scriptsPath, "step13-FiguresManuscriptSpatial.R"))
rm(list = setdiff(ls(), varsKeep))
detach(package:rsyncrosim)
detach(package:tidyverse)
detach(package:ggplot2)

# 13. Extract data for results text
source(paste0(scriptsPath, "step13-ResultsManuscript.R"))
rm(list = setdiff(ls(), varsKeep))
detach(package:rsyncrosim)
detach(package:tidyverse)
detach(package:ggplot2)
detach(package:terra)

# 13. Extract data for supplement tables
source(paste0(scriptsPath, "step13-SupplementTables.R"))
rm(list = setdiff(ls(), varsKeep))

# 14. Add charts to syncrosim library
source(paste0(scriptsPath, "step14-AddCharts.R"))
rm(list = setdiff(ls(), varsKeep))
