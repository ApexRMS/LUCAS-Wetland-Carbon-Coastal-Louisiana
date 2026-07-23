# This script extracts parameter values for supplement
# ApexRMS
# Dec 2025

library(rsyncrosim)
library(tidyverse)

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

if (!dir.exists(pathOut)) {
  dir.create(pathOut)
}

pathOutTables <- paste0(pathOut, "Tables/")

if (!dir.exists(pathOutTables)) {
  dir.create(pathOutTables)
}

# Extract Proportional Rates from flow multiplier and flow pathway table

# Make sure all flow pathways are 1s

myScenario <- scenario(
  myProject,
  "SF Flow Pathways [Base Flows, Add Methane, Add Ag, Add Water]"
)

dataDict <- datasheet(myScenario, summary = T)

myData <- datasheet(myScenario, "stsim_FlowPathway")

unique(myData$Multiplier)

rm(myScenario, myData)

# Extract flow multipliers: Emergent: Mean

myScenario <- scenario(
  myProject,
  "SF Flow Multipliers [Emergent Wetland]"
)

myData <- datasheet(myScenario, "stsim_FlowMultiplier")

keepFlows <- c(
  "Net Growth Wetland Emergent: Atmosphere -> Foliage [Type]",
  "Net Growth Wetland Emergent: Atmosphere -> Fine Roots [Type]",
  "Biomass Turnover: Foliage -> AG Very Fast [Type]",
  "Biomass Turnover: Fine Roots -> BG Very Fast [Type]",
  "Stabilization Emergent: BG Slow -> Deep Soil [Type]",
  "Decay: AG Very Fast -> BG Slow [Type]",
  "Decay: BG Very Fast -> BG Slow [Type]",
  "Emission: AG Very Fast -> Atmosphere Temp [Type]",
  "Emission: BG Very Fast -> Atmosphere Temp [Type]",
  "Emission Emergent: BG Slow -> Atmosphere Temp [Type]",
  "Lateral Transport: AG Very Fast -> Aquatic [Type]",
  "Lateral Transport: BG Very Fast -> Aquatic [Type]",
  "Lateral Transport Emergent: BG Slow -> Aquatic [Type]"
)

orderIndex <- data.frame(order = 1:length(keepFlows), FlowGroupId = keepFlows)

myDataEst1 <- myData %>%
  filter(StateClassId == "Wetland: Estuarine Emergent") %>%
  filter(FlowGroupId %in% keepFlows) %>%
  left_join(orderIndex, by = "FlowGroupId") %>%
  arrange(order) %>%
  mutate(Mean = signif(Value, 3)) %>%
  select(-Value) %>%
  mutate(
    FlowGroupId = gsub(
      " Emergent: BG Slow",
      ": BG Slow",
      FlowGroupId,
      fixed = T
    )
  )

myDataPal1 <- myData %>%
  filter(StateClassId == "Wetland: Palustrine Emergent") %>%
  filter(FlowGroupId %in% keepFlows) %>%
  left_join(orderIndex, by = "FlowGroupId") %>%
  arrange(order) %>%
  mutate(Mean = signif(Value, 3)) %>%
  select(-Value) %>%
  mutate(
    FlowGroupId = gsub(
      " Emergent: BG Slow",
      ": BG Slow",
      FlowGroupId,
      fixed = T
    )
  )

rm(myScenario, myData)

# Extract NPP Mean
myScenarioGrowth <- scenario(
  myProject,
  "STSM State Attributes [Mean Net Growth, Add Wetland]"
)

myDataGrowth <- datasheet(myScenarioGrowth, "stsim_StateAttributeValue")

myDataGrowthEst <- myDataGrowth %>%
  filter(StateClassId == "Wetland: Estuarine Emergent") %>%
  select(Value) %>%
  pull()

myDataGrowthPal <- myDataGrowth %>%
  filter(StateClassId == "Wetland: Palustrine Emergent") %>%
  select(Value) %>%
  pull()

growthFlows <- c(
  "Net Growth Wetland Emergent: Atmosphere -> Foliage [Type]",
  "Net Growth Wetland Emergent: Atmosphere -> Fine Roots [Type]"
)

myDataEst1Npp <- myDataEst1 %>%
  filter(FlowGroupId %in% growthFlows) %>%
  mutate(Mean = Mean * myDataGrowthEst)

myDataPal1Npp <- myDataPal1 %>%
  filter(FlowGroupId %in% growthFlows) %>%
  mutate(Mean = Mean * myDataGrowthPal)

myDataEst1F <- myDataEst1 %>%
  filter(!(FlowGroupId %in% growthFlows)) %>%
  bind_rows(myDataEst1Npp)

myDataPal1F <- myDataPal1 %>%
  filter(!(FlowGroupId %in% growthFlows)) %>%
  bind_rows(myDataPal1Npp)

rm(
  myScenarioGrowth,
  myDataGrowth,
  myDataGrowthEst,
  myDataGrowthPal,
  growthFlows,
  myDataEst1Npp,
  myDataPal1Npp,
  myDataEst1,
  myDataPal1
)

# Extract Flow Multipliers: Emergent Wetland: Min and Max

myScenario <- scenario(
  myProject,
  "SF Flow Multipliers [Emergent Wetland, Site]"
)

myData <- datasheet(myScenario, "stsim_FlowMultiplier") %>%
  filter(FlowGroupId %in% keepFlows)

unique(myData$Value)

rm(myData, myScenario)

myScenario <- scenario(
  myProject,
  "Distributions [Flow Multipliers, Lat, Site]"
)

myScenarioGrowth <- scenario(myProject, "Distributions [NPP, Lat, Site]")

myDataGrowth <- datasheet(myScenarioGrowth, "stsim_DistributionValue")

myDataGrowthEst <- myDataGrowth %>%
  filter(DistributionTypeId == "Wetland: Estuarine Emergent Net Growth") %>%
  rename(NPP = Value) %>%
  select(-DistributionTypeId, -ValueDistributionRelativeFrequency)

myDataGrowthPal <- myDataGrowth %>%
  filter(DistributionTypeId == "Wetland: Palustrine Emergent Net Growth") %>%
  rename(NPP = Value) %>%
  select(-DistributionTypeId, -ValueDistributionRelativeFrequency)

myData <- datasheet(myScenario, "stsim_DistributionValue")

keepFlowsUnc <- gsub(" [Type]", "", unique(myDataEst1F$FlowGroupId), fixed = T)

keepFlowsEst <- paste0("Wetland: Estuarine Emergent ", keepFlowsUnc)

keepFlowsPal <- paste0("Wetland: Palustrine Emergent ", keepFlowsUnc)

myDataEst2NPP <- myData %>%
  filter(
    DistributionTypeId %in%
      c(
        "Wetland: Estuarine Emergent Net Growth Wetland Emergent: Atmosphere -> Foliage",
        "Wetland: Estuarine Emergent Net Growth Wetland Emergent: Atmosphere -> Fine Roots"
      )
  ) %>%
  left_join(
    myDataGrowthEst,
    by = c(
      "ExternalVariableTypeId",
      "ExternalVariableMin",
      "ExternalVariableMax"
    )
  ) %>%
  mutate(ValueNPP = Value * NPP) %>%
  group_by(DistributionTypeId) %>%
  summarize(
    Min = signif(min(ValueNPP), 3),
    Max = signif(max(ValueNPP), 3),
    .groups = "drop"
  ) %>%
  mutate(
    FlowGroupId = gsub("Wetland: Estuarine Emergent ", "", DistributionTypeId)
  ) %>%
  select(-DistributionTypeId)

myDataPal2NPP <- myData %>%
  filter(
    DistributionTypeId %in%
      c(
        "Wetland: Palustrine Emergent Net Growth Wetland Emergent: Atmosphere -> Foliage",
        "Wetland: Palustrine Emergent Net Growth Wetland Emergent: Atmosphere -> Fine Roots"
      )
  ) %>%
  left_join(
    myDataGrowthPal,
    by = c(
      "ExternalVariableTypeId",
      "ExternalVariableMin",
      "ExternalVariableMax"
    )
  ) %>%
  mutate(ValueNPP = Value * NPP) %>%
  group_by(DistributionTypeId) %>%
  summarize(
    Min = signif(min(ValueNPP), 3),
    Max = signif(max(ValueNPP), 3),
    .groups = "drop"
  ) %>%
  mutate(
    FlowGroupId = gsub("Wetland: Palustrine Emergent ", "", DistributionTypeId)
  ) %>%
  select(-DistributionTypeId)


keepFlowsEst <- keepFlowsEst[
  !(keepFlowsEst %in%
    c(
      "Wetland: Estuarine Emergent Net Growth Wetland Emergent: Atmosphere -> Foliage",
      "Wetland: Estuarine Emergent Net Growth Wetland Emergent: Atmosphere -> Fine Roots"
    ))
]

keepFlowsPal <- keepFlowsPal[
  !(keepFlowsPal %in%
    c(
      "Wetland: Palustrine Emergent Net Growth Wetland Emergent: Atmosphere -> Foliage",
      "Wetland: Palustrine Emergent Net Growth Wetland Emergent: Atmosphere -> Fine Roots"
    ))
]

myDataEst2 <- myData %>%
  filter(DistributionTypeId %in% keepFlowsEst) %>%
  group_by(DistributionTypeId) %>%
  summarize(
    Min = signif(min(Value), 3),
    Max = signif(max(Value), 3),
    .groups = "drop"
  ) %>%
  mutate(
    FlowGroupId = gsub("Wetland: Estuarine Emergent ", "", DistributionTypeId)
  ) %>%
  select(-DistributionTypeId) %>%
  bind_rows(myDataEst2NPP) %>%
  mutate(FlowGroupId = paste0(FlowGroupId, " [Type]"))

myDataPal2 <- myData %>%
  filter(DistributionTypeId %in% keepFlowsPal) %>%
  group_by(DistributionTypeId) %>%
  summarize(
    Min = signif(min(Value), 3),
    Max = signif(max(Value), 3),
    .groups = "drop"
  ) %>%
  mutate(
    FlowGroupId = gsub("Wetland: Palustrine Emergent ", "", DistributionTypeId)
  ) %>%
  select(-DistributionTypeId) %>%
  bind_rows(myDataPal2NPP) %>%
  mutate(FlowGroupId = paste0(FlowGroupId, " [Type]"))

# Combine together and save

myDataEst <- myDataEst1F %>%
  left_join(myDataEst2, by = "FlowGroupId") %>%
  arrange(order) %>%
  select(-order)

myDataPal <- myDataPal1F %>%
  left_join(myDataPal2, by = "FlowGroupId") %>%
  arrange(order) %>%
  select(-order)

write.csv(
  myDataEst,
  paste0(pathOutTables, "EstuarineEmergentParameters.csv"),
  row.names = F
)
write.csv(
  myDataPal,
  paste0(pathOutTables, "PalustrineEmergentParameters.csv"),
  row.names = F
)

# Extract flow multipliers: Forested: Mean

myScenario <- scenario(
  myProject,
  "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat]"
)

myData <- datasheet(myScenario, "stsim_FlowMultiplier")

keepFlowsGrowth <- c(
  "Net Growth Forest: Atmosphere -> Foliage [Type]",
  "Net Growth Forest: Atmosphere -> Other Wood [Type]",
  "Net Growth Forest: Atmosphere -> Merchantable [Type]",
  "Net Growth Forest: Atmosphere -> Fine Roots [Type]",
  "Net Growth Forest: Atmosphere -> Coarse Roots [Type]"
)

keepFlows <- c(
  "Biomass Turnover: Foliage -> AG Very Fast [Type]",
  "Biomass Turnover: Other Wood -> Snag Branches [Type]",
  "Biomass Turnover: Other Wood -> AG Fast [Type]",
  "Biomass Turnover: Merchantable -> Snag Stems [Type]",
  "Biomass Turnover: Fine Roots -> AG Very Fast [Type]",
  "Biomass Turnover: Fine Roots -> BG Very Fast [Type]",
  "Biomass Turnover: Coarse Roots -> AG Fast [Type]",
  "Biomass Turnover: Coarse Roots -> BG Fast [Type]",
  "Stabilization: BG Slow -> Deep Soil [Type]",
  "Decay: Snag Branch -> BG Slow [Type]",
  "Transfer: Snag Branch -> AG Fast [Type]",
  "Transfer: Snag Stem -> AG Medium [Type]",
  "Decay: Snag Stem -> BG Slow [Type]",
  "Decay: AG Very Fast -> BG Slow [Type]",
  "Decay: AG Fast -> BG Slow [Type]",
  "Decay: AG Medium -> BG Slow [Type]",
  "Decay: BG Very Fast -> BG Slow [Type]",
  "Decay: BG Fast -> BG Slow [Type]",
  "Lateral Transport: AG Very Fast -> Aquatic [Type]",
  "Lateral Transport: AG Fast -> Aquatic [Type]",
  "Lateral Transport: AG Medium -> Aquatic [Type]",
  "Lateral Transport: BG Very Fast -> Aquatic [Type]",
  "Lateral Transport: BG Fast -> Aquatic [Type]",
  "Lateral Transport: BG Slow -> Aquatic [Type]",
  "Emission: Snag Branch -> Atmosphere Temp [Type]",
  "Emission: Snag Stem -> Atmosphere Temp [Type]",
  "Emission: AG Very Fast -> Atmosphere Temp [Type]",
  "Emission: AG Fast -> Atmosphere Temp [Type]",
  "Emission: AG Medium -> Atmosphere Temp [Type]",
  "Emission: BG Very Fast -> Atmosphere Temp [Type]",
  "Emission: BG Fast -> Atmosphere Temp [Type]",
  "Emission: BG Slow -> Atmosphere Temp [Type]"
)

keepFlowsAll <- c(keepFlowsGrowth, keepFlows)

orderIndex <- data.frame(
  order = 1:length(keepFlowsAll),
  FlowGroupId = keepFlowsAll
)

myDataFor1 <- myData %>%
  filter(StateClassId == "Wetland: Palustrine Forested") %>%
  filter(FlowGroupId %in% keepFlowsAll) %>%
  filter(AgeMin == 66 | is.na(AgeMin)) %>%
  left_join(orderIndex, by = "FlowGroupId") %>%
  arrange(order) %>%
  mutate(Mean = signif(Value, 3)) %>%
  select(-Value)

rm(myScenario, myData)

# Extract NPP Mean
myScenarioGrowth <- scenario(
  myProject,
  "STSM State Attributes [Mean Net Growth, Add Wetland]"
)

myDataGrowth <- datasheet(myScenarioGrowth, "stsim_StateAttributeValue")

myDataGrowthFor <- myDataGrowth %>%
  filter(StateClassId == "Wetland: Palustrine Forested") %>%
  filter(AgeMin == 66) %>%
  select(Value) %>%
  pull()

myDataFor1Npp <- myDataFor1 %>%
  filter(FlowGroupId %in% keepFlowsGrowth) %>%
  mutate(Mean = Mean * myDataGrowthFor)

myDataFor1F <- myDataFor1 %>%
  filter(!(FlowGroupId %in% keepFlowsGrowth)) %>%
  bind_rows(myDataFor1Npp)

# Extract Flow Multipliers: Forested Wetland: Min and Max

myScenario <- scenario(
  myProject,
  "SF Flow Multipliers [Forested Wetland BGS Slower; Add Lat; Site]"
)

myDataFor2 <- datasheet(myScenario, "stsim_FlowMultiplier") %>%
  filter(StateClassId == "Wetland: Palustrine Forested") %>%
  filter(!is.na(Value)) %>%
  filter(FlowGroupId %in% keepFlowsAll) %>%
  filter(AgeMin == 66 | is.na(AgeMin)) %>%
  mutate(Min = signif(Value, 3), Max = signif(Value, 3)) %>%
  select(
    -Value,
    -DistributionType,
    -DistributionFrequencyId,
    -AgeMin,
    -AgeMax,
    -StateClassId
  )

rm(myScenario)

myScenario <- scenario(
  myProject,
  "Distributions [Forested Wetland; Flow Multipliers]"
)

myData <- datasheet(myScenario, "stsim_DistributionValue")

keepFlowsFor <- paste0(
  "Wetland: Palustrine Forested ",
  gsub(" [Type]", "", keepFlows, fixed = T)
)

keepFlowsGrowth66 <- paste0(
  "Wetland: Palustrine Forested ",
  gsub(" [Type]", "", keepFlowsGrowth, fixed = T),
  " 66"
)

myDataFor3NPP <- myData %>%
  filter(DistributionTypeId %in% c(keepFlowsGrowth66)) %>%
  group_by(DistributionTypeId) %>%
  summarize(
    Min = signif(min((Value * myDataGrowthFor)), 3),
    Max = signif(max((Value * myDataGrowthFor)), 3),
    .groups = "drop"
  ) %>%
  mutate(
    FlowGroupId = gsub("Wetland: Palustrine Forested ", "", DistributionTypeId)
  ) %>%
  mutate(
    FlowGroupId = gsub(" 66", "", FlowGroupId)
  ) %>%
  select(-DistributionTypeId)

myDataFor3 <- myData %>%
  filter(DistributionTypeId %in% c(keepFlowsFor)) %>%
  group_by(DistributionTypeId) %>%
  summarize(
    Min = signif(min(Value), 3),
    Max = signif(max(Value), 3),
    .groups = "drop"
  ) %>%
  mutate(
    FlowGroupId = gsub("Wetland: Palustrine Forested ", "", DistributionTypeId)
  ) %>%
  select(-DistributionTypeId) %>%
  bind_rows(myDataFor3NPP) %>%
  mutate(FlowGroupId = paste0(FlowGroupId, " [Type]"))


# Combine together and save

myDataFor2 <- myDataFor2 %>%
  bind_rows(myDataFor3)

myDataFor <- myDataFor1F %>%
  left_join(myDataFor2, by = "FlowGroupId") %>%
  arrange(order) %>%
  select(-order)

write.csv(
  myDataFor,
  paste0(pathOutTables, "PalustrineForestedParameters.csv"),
  row.names = F
)
