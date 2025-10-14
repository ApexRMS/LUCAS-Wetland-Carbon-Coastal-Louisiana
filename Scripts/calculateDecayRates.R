# Function to solve for correct decay parameters
# ApexRMS
# Sep 2025

calculateDecayRates <- function(
  projectName,
  scenarioName,
  targetValue,
  convergenceLevel,
  scenarioMult,
  emissionsStart,
  meanBurial
) {
  run(projectName, scenario = scenarioName)

  scenarioList <- scenario(projectName, summary = T, results = T)

  forestId <- scenarioList$ScenarioId[grep(scenarioName, scenarioList$Name)]

  myScenario <- scenario(projectName, scenario = max(forestId))

  myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

  eqBGVFm <- myData %>%
    filter(StockGroupId == "DOM: Belowground Very Fast [Type]") %>%
    filter(Timestep == 124) %>%
    pull(Amount)

  eqFRm <- myData %>%
    filter(StockGroupId == "Biomass: Fine Root [Type]") %>%
    filter(Timestep == 124) %>%
    pull(Amount)

  # Has equilibrium been reached, difference is less than 1%, (difference in peaks, year prior to disturbance)
  peaks <- seq(125, 3500, 125) - 1

  testE <- myData %>%
    filter(Timestep %in% peaks) %>%
    filter(StockGroupId == "DOM: Belowground Slow [Type]") %>%
    group_by(Timestep, StratumId, StateClassId, StockGroupId) %>%
    summarize(carbonMean = mean(Amount, na.rm = T)) %>%
    ungroup() %>%
    arrange(Timestep) %>%
    mutate(percentDiff = (carbonMean - lag(carbonMean)) / lag(carbonMean) * 100)

  carbonMean <- testE %>% filter(Timestep == 3499) %>% pull(carbonMean)

  targetValue <- targetValue - eqBGVFm - eqFRm

  diffPer <- (abs(carbonMean - targetValue) /
    mean(c(carbonMean, targetValue))) *
    100

  emissionsInOut <- emissionsStart

  while (diffPer > convergenceLevel) {
    diffPerSign <- ((carbonMean - targetValue) /
      mean(c(carbonMean, targetValue))) *
      100

    if (diffPerSign > 0) {
      emissionsInOut <- emissionsInOut * (100 + diffPer) / 100
    } else if (diffPerSign < 0) {
      emissionsInOut <- emissionsInOut * (100 - diffPer) / 100
    }

    # Flow Multipliers Forested Wetland
    myScenario <- scenario(
      projectName,
      scenario = scenarioMult,
      folder = "Single-Cell Sub-Scenarios"
    )

    myData <- datasheet(myScenario, "stsim_FlowMultiplier")

    poolTotal <- targetValue

    flowMultBGStoAtm <- emissionsInOut / poolTotal
    flowMultBGStoDeep <- meanBurial / poolTotal

    myData$Value[
      myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"
    ] <- flowMultBGStoAtm
    myData$Value[
      myData$FlowGroupId == "Stabilization: BG Slow -> Deep Soil [Type]"
    ] <- flowMultBGStoDeep

    saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE)

    run(projectName, scenario = scenarioName)

    scenarioList <- scenario(projectName, summary = T, results = T)

    forestId <- scenarioList$ScenarioId[grep(scenarioName, scenarioList$Name)]

    myScenario <- scenario(projectName, scenario = max(forestId))

    myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

    testE <- myData %>%
      filter(Timestep %in% peaks) %>%
      filter(StockGroupId == "DOM: Belowground Slow [Type]") %>%
      group_by(Timestep, StratumId, StateClassId, StockGroupId) %>%
      summarize(carbonMean = mean(Amount, na.rm = T)) %>%
      ungroup() %>%
      arrange(Timestep) %>%
      mutate(
        percentDiff = (carbonMean - lag(carbonMean)) / lag(carbonMean) * 100
      )

    carbonMean <- testE %>% filter(Timestep == 3499) %>% pull(carbonMean)

    diffPer <- (abs(carbonMean - targetValue) /
      mean(c(carbonMean, targetValue))) *
      100

    diffPerSign <- ((carbonMean - targetValue) /
      mean(c(carbonMean, targetValue))) *
      100

    print(diffPerSign)
    print(emissionsInOut)
  }

  print(testE, n = nrow(testE))
}
