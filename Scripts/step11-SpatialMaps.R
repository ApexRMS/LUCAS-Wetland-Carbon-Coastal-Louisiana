# ApexRMS
# Updated 2025-03-26
# Run after step10-SpatialFigures.R
# This script creates maps for spatial scenario results

library(rsyncrosim)
library(tidyverse)
library(terra)
library(viridis)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

dataPath <- "Data/"
modelPath <- "Models/"

#modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(
  name = paste0(modelFullPath, "/", modelName, ".ssim"),
  session = mySession
)

myProject <- rsyncrosim::project(myLibrary, project = "Definitions")

pathOut <- paste0(rootPath, "Models/", modelName, "/OutputFigures/")

pathOutSpatial <- paste0(pathOut, "Spatial/")

if (!dir.exists(pathOutSpatial)) {
  dir.create(pathOutSpatial)
}

pathOutMaps <- paste0(pathOutSpatial, "Maps/")

if (!dir.exists(pathOutMaps)) {
  dir.create(pathOutMaps)
}

stockGroupIds <- datasheet(myProject, name = "stsim_StockGroup", includeKey = T)
flowGroupIds <- datasheet(myProject, name = "stsim_FlowGroup", includeKey = T)
transitionGroupIds <- datasheet(
  myProject,
  name = "stsim_TransitionGroup",
  includeKey = T
)

scenarioList <- scenario(myProject, summary = T, results = T)

scenID <- scenarioList$ScenarioId[grep("Basin Baseline", scenarioList$Name)]

myScenario <- scenario(myProject, scenario = max(scenID))

# Carbon Stocks

listStocks <- list.files(
  paste0(
    rootPath,
    "/Models/",
    modelName,
    "/",
    modelName,
    ".ssim.data/Scenario-",
    scenarioId(myScenario),
    "/stsim_OutputAverageSpatialStockGroup"
  ),
  pattern = ".tif",
  full.names = T
)

keepStocksSpatial <- c(
  "Ecosystem Carbon Storage (tons C)",
  "Biomass: Aboveground",
  "Biomass: Belowground",
  "DOM: Deadwood",
  "DOM: Litter",
  "DOM: Soil"
)

for (j in 1:length(keepStocksSpatial)) {
  stockId <- stockGroupIds %>%
    filter(Name == keepStocksSpatial[j]) %>%
    pull(StockGroupId)

  listStocksSub <- grep(stockId, listStocks, value = T)
  listStocksSub <- grep("ts2016", listStocksSub, value = T)

  stockName <- gsub(
    " (tons C)",
    "",
    gsub(": ", " ", keepStocksSpatial[j]),
    fixed = T
  )

  r1 <- rast(listStocksSub)

  png(
    filename = paste0(pathOutMaps, gsub(" ", "", stockName), "_stock.png"),
    width = 3.5,
    height = 2.5,
    units = "in",
    res = 600
  )
  plot(
    r1,
    plg = list(title = as.expression(bquote("Mg C" ~ ha^-1)), title.cex = 0.65),
    type = "continuous",
    axes = FALSE,
    main = paste0("e. ", stockName),
    col = rev(mako(n = 100)),
    cex.main = 0.8,
    maxcell = 10000000
  )
  # north(type = 1, cex = 0.7, "bottomleft")
  sbar(
    40000,
    xy = c(500000, 694000),
    divs = 2,
    cex = 0.8,
    type = "bar",
    below = "km",
    label = c(0, 20, 40)
  )
  dev.off()

  rm(stockId, listStocksSub, stockName, r1)
}

# Carbon Fluxes

listFluxes <- list.files(
  paste0(
    rootPath,
    "/Models/",
    modelName,
    "/",
    modelName,
    ".ssim.data/Scenario-",
    scenarioId(myScenario),
    "/stsim_OutputAverageSpatialFlowGroup"
  ),
  pattern = ".tif",
  full.names = T
)

keepFluxesSpatial <- c(
  "Annual Net Growth (tons C per year)",
  "Annual Emissions: CO2 and CH4 (tons C per year)",
  "Annual Lateral Flux (tons C per year)",
  "Annual Emissions: CH4 (tons C per year)",
  "Annual Emissions: CO2 (tons C per year)"
)

fluxNameLetters <- c("a. ", "b. ", "c. ", "z. ", "z. ")

keepFluxesSpatialDiff <- "Annual Net Ecosystem Carbon Balance (tons C per year)"


for (k in 1:length(keepFluxesSpatial)) {
  fluxId <- flowGroupIds %>%
    filter(Name == keepFluxesSpatial[k]) %>%
    pull(FlowGroupId)

  listFluxesSub <- grep(fluxId, listFluxes, value = T)
  listFluxesSub <- grep("ts2016", listFluxesSub, value = T)

  fluxName <- gsub("(tons C per year)", "", keepFluxesSpatial[k], fixed = T)
  fluxName <- gsub(": ", " ", fluxName)
  fluxName <- gsub("-", " ", fluxName)
  fluxName <- gsub(
    "Annual Net Ecosystem Carbon Balance",
    "Annual NECB",
    fluxName
  )
  fluxName <- gsub(" CO2 and CH4", "", fluxName)

  r1 <- rast(listFluxesSub)

  png(
    filename = paste0(pathOutMaps, gsub(" ", "", fluxName), "_flux.png"),
    width = 3.5,
    height = 2.5,
    units = "in",
    res = 600
  )
  plot(
    r1,
    plg = list(
      title = as.expression(bquote("Mg C" ~ ha^-1 ~ y^-1)),
      title.cex = 0.65
    ),
    type = "continuous",
    axes = FALSE,
    main = paste0(fluxNameLetters[k], fluxName),
    col = rev(mako(n = 100)),
    cex.main = 0.8,
    maxcell = 10000000
  )
  # north(type =1,cex = 0.7, "bottomleft")
  sbar(
    40000,
    xy = c(500000, 694000),
    divs = 2,
    cex = 0.8,
    type = "bar",
    below = "km",
    label = c(0, 20, 40)
  )
  dev.off()

  rm(fluxId, listFluxesSub, fluxName, r1)
}

# Carbon Flux Differences

for (k in 1:length(keepFluxesSpatialDiff)) {
  fluxId <- flowGroupIds %>%
    filter(Name == keepFluxesSpatialDiff[k]) %>%
    pull(FlowGroupId)

  listFluxesSub <- grep(fluxId, listFluxes, value = T)
  listFluxesSub <- grep("ts2016", listFluxesSub, value = T)

  fluxName <- gsub("(tons C per year)", "", keepFluxesSpatialDiff[k], fixed = T)
  fluxName <- gsub(": ", " ", fluxName)
  fluxName <- gsub("-", " ", fluxName)
  fluxName <- gsub(
    "Annual Net Ecosystem Carbon Balance",
    "Annual NECB",
    fluxName
  )

  r1 <- rast(listFluxesSub)

  min1 <- global(r1, min, na.rm = T)$min
  max1 <- global(r1, max, na.rm = T)$max

  max2 <- max(c(abs(min1), abs(max1)))

  png(
    filename = paste0(pathOutMaps, gsub(" ", "", fluxName), "_flux.png"),
    width = 3.5,
    height = 2.5,
    units = "in",
    res = 600
  )
  plot(
    r1,
    plg = list(
      title = as.expression(bquote("Mg C" ~ ha^-1 ~ y^-1)),
      title.cex = 0.65
    ),
    type = "continuous",
    axes = FALSE,
    main = paste0("d. ", fluxName),
    range = c(-(max2 - 23), (max2 - 21)),
    col = c(
      "#997726",
      "#a9832a",
      "#b9902d",
      "#c99c31",
      "#d0a43e",
      "#d3ac4e",
      "#d7b35e",
      "#dbbb6d",
      "#dfc27d",
      "#f5f5f5",
      "#c7eae5",
      "#b9e5de",
      "#aadfd8",
      "#9cdad1",
      "#8ed5cb",
      "#80cfc4",
      "#71cabd",
      "#63c5b7",
      "#55bfb0",
      "#41ac9d",
      "#3b9e90",
      "#369083",
      "#318176",
      "#2b7369",
      "#26655c",
      "#20574f",
      "#1b4842"
    ), #rev(viridis(100)),
    cex.main = 0.8,
    fill_range = T,
    maxcell = 10000000
  )
  # north(type =1,cex = 0.7, "bottomleft")
  sbar(
    40000,
    xy = c(500000, 694000),
    divs = 2,
    cex = 0.8,
    type = "bar",
    below = "km",
    label = c(0, 20, 40)
  )
  dev.off()

  rm(fluxId, listFluxesSub, fluxName, r1)
}

# Land Cover Area 2016

listLandCover <- list.files(
  paste0(
    rootPath,
    "/Models/",
    modelName,
    "/",
    modelName,
    ".ssim.data/Scenario-",
    scenarioId(myScenario),
    "/stsim_OutputSpatialState"
  ),
  pattern = ".tif",
  full.names = T
)

listLandCoverSub <- grep("ts2016", listLandCover, value = T)

landCoverName <- "Land Cover"

r1 <- rast(listLandCoverSub)
freq(r1)

r1 <- ifel(r1 %in% c(98, 99), 97, r1)
r1 <- ifel(r1 %in% c(13, 14), 11, r1)
freq(r1)

col1 <- data.frame(
  value = c(82, 23, 71, 54, 96, 95, 90, 97, 11, 140, 400, 500, 700),
  col = c(
    "#BFA056",
    "#C3CB48",
    "#EFBA8B",
    "#6D6C14",
    "#A91EAC",
    "#EA2DEE",
    "#145A5A",
    "#46F1F1",
    "#000974",
    "#0A3905",
    "#2E9E40",
    "#9be2a6",
    "#48EF30"
  )
)

coltab(r1) <- col1

cls <- data.frame(
  id = c(140, 400, 500, 700, 90, 95, 96, 97, 11, 82, 23, 71, 54),
  cover = c(
    "Longleaf/Slash Pine Forest",
    "Oak/Pine Forest",
    "Oak/Hickory Forest",
    "Elm/Ash/Cottonwood Forest",
    "Palustrine Forested Wetland",
    "Palustrine Emergent Wetland",
    "Estuarine Emergent Wetland",
    "Unconsolidated Shore",
    "Water",
    "Agriculture",
    "Developed",
    "Grassland",
    "Shrubland"
  )
)


levels(r1) <- cls

clsCol <- cls %>%
  rename(value = id) %>%
  left_join(col1, by = join_by(value))

png(
  filename = paste0(pathOutMaps, "LandCover.png"),
  width = 3.5,
  height = 2.5,
  units = "in",
  res = 600
)
plot(
  r1,
  #plg=list(x = "bottomleft", cex = 0.51),
  axes = FALSE,
  main = "f. Land Cover",
  cex.main = 0.8,
  maxcell = 10000000,
  legend = F
)
add_legend(
  x = 472000,
  y = 755000,
  legend = clsCol$cover,
  fill = clsCol$col,
  cex = 0.3,
  bty = "n",
  y.intersp = 0.5
)
north(type = 1, cex = 0.6, xy = c(640000, 684000))
sbar(
  40000,
  xy = c(590000, 684000),
  divs = 2,
  cex = 0.6,
  type = "bar",
  below = "km",
  label = c(0, 20, 40)
)
dev.off()


## Map of Total Ecosystem Carbon difference between Baseline and IPCC

IPCCscenID <- scenarioList$ScenarioId[grep("Basin IPCC", scenarioList$Name)]

ipccScenario <- scenario(myProject, scenario = max(IPCCscenID))

totalC <- "Ecosystem Carbon Storage (tons C)"

stockId <- stockGroupIds %>%
  filter(Name == totalC) %>%
  pull(StockGroupId)

listStocksSub <- grep(stockId, listStocks, value = T)
listStocksSub <- grep("ts2016", listStocksSub, value = T)

ipccStockSub <- gsub(scenID, IPCCscenID, listStocksSub)

stockName <- gsub(" (tons C)", "", gsub(": ", " ", totalC), fixed = T)

baselineRast <- rast(listStocksSub)
ipccRast <- rast(ipccStockSub)

png(
  filename = paste0(pathOutMaps, "EcosystemCarbon_stock_difference.png"),
  width = 3.5,
  height = 2.5,
  units = "in",
  res = 600
)
plot(
  baselineRast - ipccRast,
  plg = list(title = as.expression(bquote("Mg C" ~ ha^-1)), title.cex = 0.65),
  type = "continuous",
  axes = FALSE,
  main = paste0(stockName, " Difference"),
  sub = "Empirically-Informed Carbon Fate Model - IPCC-Default Carbon Fate Model",
  col = rev(mako(n = 100)),
  cex.main = 0.6,
  maxcell = 10000000
)
# north(type = 1, cex = 0.7, "bottomleft")
sbar(
  40000,
  xy = c(500000, 694000),
  divs = 2,
  cex = 0.8,
  type = "bar",
  below = "km",
  label = c(0, 20, 40)
)
dev.off()
