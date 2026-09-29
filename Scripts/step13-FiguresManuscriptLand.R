# ApexRMS
# Updated 2025-07-10
# This script creates figures for the manuscript

library(rsyncrosim)
library(tidyverse)
library(ggplot2)
library(terra)

options(scipen = 999)
old <- options(pillar.sigfig = 10)

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

pathOutManuscript <- paste0(pathOutSpatial, "Manuscript")

if (!dir.exists(pathOutManuscript)) {
  dir.create(pathOutManuscript)
}

scenarioList <- scenario(myProject, summary = T, results = T)

# Summarize land cover change
stateClassTable <- datasheet(myProject, name = "stsim_StateClass")
#unique(stateClassTable$Name)

scOther <- stateClassTable$Name[
  !(stateClassTable$Name %in%
    c(
      "Wetland: Estuarine Emergent",
      "Wetland: Palustrine Emergent",
      "Wetland: Palustrine Forested",
      "Water: All",
      "Water: Previously Emergent Wetland",
      "Water: Previously Forested Wetland",
      "Wetland: Unconsolidated Shore",
      "Wetland: Unvegetated Emergent",
      "Wetland: Unvegetated Forested",
      "Agriculture: Cropland",
      "Agriculture: Pasture",
      "Barren: All",
      "Developed: High Intensity",
      "Developed: Low Intensity",
      "Developed: Medium Intensity",
      "Developed: Open Space",
      "Developed: Transportation"
    ))
]

#scOther

scForest <- grep("Forest:", scOther, value = T)
scOther <- scOther[!(scOther %in% scForest)]

scen <- c("Basin Baseline")

lookupLC <- data.frame(
  StateClassId = c(
    "Wetland: Estuarine Emergent",
    "Wetland: Palustrine Emergent",
    "Wetland: Palustrine Forested",
    "Water: All",
    "Water: Previously Emergent Wetland",
    "Water: Previously Forested Wetland",
    "Wetland: Unconsolidated Shore",
    "Wetland: Unvegetated Emergent",
    "Wetland: Unvegetated Forested",
    "Agriculture: Cropland",
    "Agriculture: Pasture",
    "Barren: All",
    "Developed: High Intensity",
    "Developed: Low Intensity",
    "Developed: Medium Intensity",
    "Developed: Open Space",
    "Developed: Transportation",
    scForest,
    scOther
  ),
  LandClass = c(
    "Estuarine Emergent Wetland",
    "Palustrine Emergent Wetland",
    "Palustrine Forested Wetland",
    rep("Water", 3),
    rep("Unconsolidated Shore", 3),
    rep("Agriculture and Developed", 8),
    rep("Upland Forest", length(scForest)),
    rep("Grassland and Shrubland", length(scOther))
  )
)

lookupLC

lookupChart <- data.frame(
  LandClass = c(
    "Estuarine Emergent Wetland",
    "Palustrine Emergent Wetland",
    "Palustrine Forested Wetland",
    "Water",
    "Unconsolidated Shore",
    "Agriculture and Developed",
    "Upland Forest",
    "Grassland and Shrubland"
  ),
  Color = c(
    "#A91EAC",
    "#EA2DEE",
    "#145A5A",
    "#000974",
    "#46F1F1",
    "#C3CB48",
    "#2E9E40",
    "#6D6C14"
  )
)

for (s in 1:length(scen)) {
  idS <- scenarioList$ScenarioId[grep(scen[s], scenarioList$Name)]

  myScenarioS <- scenario(myProject, scenario = max(idS))

  myDataLandS <- datasheet(myScenarioS, "stsim_OutputStratumState")

  landS <- myDataLandS %>%
    left_join(lookupLC, by = join_by(StateClassId)) %>%
    left_join(lookupChart, by = join_by(LandClass)) %>%
    group_by(Timestep, LandClass, Color) %>%
    summarize(Area = sum(Amount, na.rm = T)) %>%
    filter(Timestep %in% c(2001, 2006, 2010, 2016))

  landSdiff <- landS %>%
    select(LandClass, Timestep, Area) %>%
    pivot_wider(
      names_from = Timestep,
      values_from = Area,
      id_cols = LandClass
    ) %>%
    mutate(
      `2001-2006` = `2006` - `2001`,
      `2006-2010` = `2010` - `2006`,
      `2010-2016` = `2016` - `2010`
    ) %>%
    select(LandClass, `2001-2006`, `2006-2010`, `2010-2016`) %>%
    pivot_longer(!LandClass, names_to = "Timestep", values_to = "Area") %>%
    left_join(lookupChart, by = join_by(LandClass))

  landSdiff$Timestep <- as.factor(landSdiff$Timestep)
  landSdiff$LandClass2 <- factor(
    landSdiff$LandClass,
    levels = c(
      "Upland Forest",
      "Palustrine Forested Wetland",
      "Palustrine Emergent Wetland",
      "Estuarine Emergent Wetland",
      "Water",
      "Unconsolidated Shore",
      "Agriculture and Developed",
      "Grassland and Shrubland"
    )
  )

  col <- as.character(landSdiff$Color)
  names(col) <- as.character(landSdiff$LandClass2)

  p3 <- ggplot(
    landSdiff,
    aes(x = Timestep, y = Area, fill = LandClass2, group = LandClass2)
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_vline(xintercept = c(1.5, 2.5), linetype = "dotted") +
    scale_fill_manual(values = col) +
    geom_hline(aes(yintercept = 0), col = 'black', size = 0.7) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "right"
    ) +
    guides(
      fill = guide_legend(
        nrow = 8,
        keywidth = 0.2,
        keyheight = 0.2,
        default.unit = "inch",
        title = "Land Cover Class"
      )
    ) +
    xlab("\nYear") +
    ylab("Net Land Cover Change (ha)\n")

  p3

  ggsave(
    paste0(
      pathOutManuscript,
      "/LandCoverChangeWater_",
      gsub(" ", "", scen[s]),
      ".png"
    ),
    p3,
    width = 7,
    height = 3,
    dpi = 600
  )

  write.csv(
    landSdiff,
    paste0(
      pathOutManuscript,
      "/LandCoverChangeWater_",
      gsub(" ", "", scen[s]),
      ".csv"
    ),
    row.names = FALSE
  )
  write.csv(
    myDataLandS,
    paste0(pathOutManuscript, "/LandCover_", gsub(" ", "", scen[s]), ".csv"),
    row.names = FALSE
  )
}

id2 <- scenarioList$ScenarioId[grep("Basin Baseline", scenarioList$Name)]

myScenario2 <- scenario(myProject, scenario = max(id2))

lookupS <- lookupLC %>%
  rename(Start = StateClassId, LandClassStart = LandClass)

lookupE <- lookupLC %>%
  rename(End = StateClassId, LandClassEnd = LandClass)


tabTransition <- datasheet(myScenario2, "stsim_OutputStratumTransition")

years <- c(2001, 2006, 2010, 2016)

for (i in 2:length(years)) {
  subTabTransition <- tabTransition %>%
    filter(Timestep == years[i]) %>%
    select(Timestep, TransitionGroupId, Amount) %>%
    mutate(
      TransitionGroupId = gsub(
        " [Type]",
        "",
        gsub("LULCC: ", "", TransitionGroupId),
        fixed = T
      )
    )

  subTabTransition$Start <- unlist(lapply(
    strsplit(subTabTransition$TransitionGroupId, " -> "),
    "[[",
    1
  ))
  subTabTransition$End <- unlist(lapply(
    strsplit(subTabTransition$TransitionGroupId, " -> "),
    "[[",
    2
  ))

  subTabTransition <- subTabTransition %>%
    left_join(lookupS, by = join_by(Start)) %>%
    left_join(lookupE, by = join_by(End))

  allTransitions <- expand.grid(
    LandClassStart = unique(c(
      subTabTransition$LandClassStart,
      subTabTransition$LandClassEnd
    )),
    LandClassEnd = unique(c(
      subTabTransition$LandClassStart,
      subTabTransition$LandClassEnd
    ))
  )

  allTransitions <- allTransitions %>%
    mutate(Amount = 0, Timestep = years[i])

  subTabTransition <- subTabTransition %>%
    addRow(allTransitions) %>%
    group_by(Timestep, LandClassStart, LandClassEnd) %>%
    summarize(Area_ha = sum(Amount)) %>%
    ungroup()

  landClasses <- unique(lookupLC$LandClass)

  subTabTransitionBlank <- data.frame(
    LandClassStart = landClasses,
    LandClassEnd = landClasses,
    Area_ha = NA,
    Timestep = years[i]
  )

  subTabTransition <- subTabTransition %>%
    filter(!(LandClassStart == LandClassEnd)) %>%
    bind_rows(subTabTransitionBlank) %>%
    mutate(Area_haR = round(Area_ha, 0))

  print(max(subTabTransition$Area_haR, na.rm = T))

  write.csv(
    subTabTransition,
    paste0(pathOutManuscript, "/LandCoverT_", years[i], ".csv"),
    row.names = F
  )

  subTabTransition$LandClassEnd2 <- factor(
    subTabTransition$LandClassEnd,
    levels = rev(c(
      "Upland Forest",
      "Palustrine Forested Wetland",
      "Palustrine Emergent Wetland",
      "Estuarine Emergent Wetland",
      "Water",
      "Unconsolidated Shore",
      "Agriculture and Developed",
      "Grassland and Shrubland"
    ))
  )

  subTabTransition$LandClassStart2 <- factor(
    subTabTransition$LandClassStart,
    levels = rev(c(
      "Upland Forest",
      "Palustrine Forested Wetland",
      "Palustrine Emergent Wetland",
      "Estuarine Emergent Wetland",
      "Water",
      "Unconsolidated Shore",
      "Agriculture and Developed",
      "Grassland and Shrubland"
    ))
  )

  pT <- ggplot(
    data = subTabTransition,
    aes(x = LandClassEnd2, y = LandClassStart2, fill = Area_ha)
  ) +
    geom_tile() +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "right",
      #legend.title=element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_fill_gradient2(
      low = "#d6ebe4",
      high = "#297c60",
      limit = c(0, 4300), #min(subTabTransition$Area_ha) #max(subTabTransition$Area_ha)
      space = "Lab",
      name = "Area (ha)",
      na.value = "gray70"
    ) +
    xlab(paste0("\n", years[i])) +
    ylab(paste0(years[i - 1], "\n")) +
    geom_text(
      aes(x = LandClassEnd2, y = LandClassStart2, label = Area_haR),
      color = "black",
      size = 4
    )

  ggsave(
    paste0(pathOutManuscript, "/LandCoverT_", years[i], ".png"),
    pT,
    width = 6,
    height = 4,
    dpi = 600
  )
}


# Net Changes

timeStepPair <- c(2001, 2016)

stateClassTable <- datasheet(myProject, name = "stsim_StateClass") %>%
  select(Name, Id)

lookupLC <- lookupLC %>%
  rename(Name = StateClassId) %>%
  left_join(stateClassTable, by = join_by(Name))


lookupS <- lookupLC %>%
  rename(Start = Id, LandClassStart = LandClass) %>%
  select(Start, LandClassStart)

lookupE <- lookupLC %>%
  rename(End = Id, LandClassEnd = LandClass) %>%
  select(End, LandClassEnd)

scenList <- c("Basin Baseline")

for (i in 1:length(scenList)) {
  scenID <- scenarioList$ScenarioId[grep(scenList[i], scenarioList$Name)]

  myScenario <- scenario(myProject, scenario = max(scenID))

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

  sub1 <- grep(paste0("ts", timeStepPair[1]), listLandCover, value = T)
  sub2 <- grep(paste0("ts", timeStepPair[2]), listLandCover, value = T)

  r1 <- rast(sub1)
  r2 <- rast(sub2)

  t1 <- r1 * 1000 + r2

  lcF <- freq(t1)

  lcF$value6 <- formatC(lcF$value, width = 6, format = "d", flag = "0")

  lcF$Start <- as.numeric(substr(as.character(lcF$value6), 1, 3))
  lcF$End <- as.numeric(substr(as.character(lcF$value6), 4, 6))

  lcF <- lcF %>%
    left_join(lookupS, by = join_by(Start)) %>%
    left_join(lookupE, by = join_by(End))

  allTransitions <- expand.grid(
    LandClassStart = unique(c(lcF$LandClassStart, lcF$LandClassEnd)),
    LandClassEnd = unique(c(lcF$LandClassStart, lcF$LandClassEnd))
  )

  allTransitions <- allTransitions %>%
    mutate(count = 0)

  lcF <- lcF %>%
    addRow(allTransitions) %>%
    select(count, LandClassStart, LandClassEnd) %>%
    group_by(LandClassStart, LandClassEnd) %>%
    summarize(countT = sum(count, na.rm = T)) %>%
    mutate(Area_ha = countT * 30 * 30 / 10000)

  write.csv(
    lcF,
    paste0(pathOutManuscript, "/LandCoverT_2001_2016.csv"),
    row.names = F
  )

  landClasses <- unique(lookupLC$LandClass)

  subTabTransitionBlank <- data.frame(
    LandClassStart = landClasses,
    LandClassEnd = landClasses,
    Area_ha = NA
  )

  lcF <- lcF %>%
    filter(!(LandClassStart == LandClassEnd)) %>%
    bind_rows(subTabTransitionBlank) %>%
    mutate(Area_haR = round(Area_ha, 0))

  print(max(lcF$Area_haR, na.rm = T))

  lcF$LandClassEnd2 <- factor(
    lcF$LandClassEnd,
    levels = rev(c(
      "Upland Forest",
      "Palustrine Forested Wetland",
      "Palustrine Emergent Wetland",
      "Estuarine Emergent Wetland",
      "Water",
      "Unconsolidated Shore",
      "Agriculture and Developed",
      "Grassland and Shrubland"
    ))
  )

  lcF$LandClassStart2 <- factor(
    lcF$LandClassStart,
    levels = rev(c(
      "Upland Forest",
      "Palustrine Forested Wetland",
      "Palustrine Emergent Wetland",
      "Estuarine Emergent Wetland",
      "Water",
      "Unconsolidated Shore",
      "Agriculture and Developed",
      "Grassland and Shrubland"
    ))
  )

  pT <- ggplot(
    data = lcF,
    aes(x = LandClassEnd2, y = LandClassStart2, fill = Area_haR)
  ) +
    geom_tile() +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "right",
      #legend.title=element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_fill_gradient2(
      low = "#d6ebe4",
      high = "#297c60",
      limit = c(0, 4300), #c(min(lcF$Area_haR),max(lcF$Area_haR)),
      space = "Lab",
      name = "Area (ha)",
      na.value = "gray70"
    ) +
    xlab(paste0("\n", timeStepPair[2])) +
    ylab(paste0(timeStepPair[1], "\n")) +
    geom_text(
      aes(x = LandClassEnd2, y = LandClassStart2, label = Area_haR),
      color = "black",
      size = 4
    )

  ggsave(
    paste0(
      pathOutManuscript,
      "/LandCoverT_",
      gsub(" ", "", scenList[i]),
      ".png"
    ),
    pT,
    width = 6,
    height = 4,
    dpi = 600
  )

  write.csv(
    lcF,
    paste0(
      pathOutManuscript,
      "/LandCoverT_",
      gsub(" ", "", scenList[i]),
      ".csv"
    ),
    row.names = FALSE
  )
}
