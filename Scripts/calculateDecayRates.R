# Function to solve for correct decay parameters
# ApexRMS
# Sep 2025

# projectName: Project
# scenarioName: The spinup scenario
# targetValue: Target value is the sum of BGS, BGVF, FR, CR, and BGF. The sum of all belowground carbon.
# convergenceLevel: Convergence level is how different (% difference) can the BGS pool be from the calculated value. 
# scenarioMult: Flow multiplier values that should be updated to ensure convergence
# emissionsStart: Starting value for emissions flux
# meanBurial: Burial rate

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

  myScenario <- getScenarioExact(projectName, scenarioName)

  myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

  eqBGVFm <- myData %>%
    filter(StockGroupId == "DOM: Belowground Very Fast [Type]") %>%
    filter(Timestep == 3565) %>%
    pull(Amount)

  eqFRm <- myData %>%
    filter(StockGroupId == "Biomass: Fine Root [Type]") %>%
    filter(Timestep == 3565) %>%
    pull(Amount)
  
  eqCRm <- myData %>%
    filter(StockGroupId == "Biomass: Coarse Root [Type]") %>%
    filter(Timestep == 3565) %>%
    pull(Amount)
  
  eqBGFm <- myData %>%
    filter(StockGroupId == "DOM: Belowground Fast [Type]") %>%
    filter(Timestep == 3565) %>%
    pull(Amount)

  targetValue <- targetValue - eqBGVFm - eqFRm - eqCRm - eqBGFm
  
  emissionsInOut <- emissionsStart
  
  # Flow Multipliers Forested Wetland
  myScenario <- scenario(
    projectName,
    scenario = scenarioMult
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
  
  saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE, force = TRUE)
  
  rm(myScenario, myData)
  
  run(projectName, scenario = scenarioName)

  myScenario <- getScenarioExact(projectName, scenarioName)

  myData <- datasheet(myScenario, "stsim_OutputStock", optional = T)

  # Has equilibrium been reached, difference is less than 1%, (difference in peaks, year prior to disturbance)
  peaks <- seq(125, 3500, 125) - 1
  
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
  
  carbonMean <- myData %>% 
    filter(Timestep == 3565) %>% 
    filter(StockGroupId == "DOM: Belowground Slow [Type]") %>% 
    pull(Amount)
  
  diffPerSign <- ((carbonMean - targetValue) /
                    mean(c(carbonMean, targetValue))) * 100

  while (abs(diffPerSign) > convergenceLevel) {
    
    if (diffPerSign > 0) {
      emissionsInOut <- emissionsInOut * (100 + abs(diffPerSign)) / 100
    } else if (diffPerSign < 0) {
      emissionsInOut <- emissionsInOut * (100 - abs(diffPerSign)) / 100
    }

    # Flow Multipliers Forested Wetland
    myScenario <- scenario(
      projectName,
      scenario = scenarioMult
    )

    myData <- datasheet(myScenario, "stsim_FlowMultiplier")

    flowMultBGStoAtm <- emissionsInOut / poolTotal
    
    myData$Value[
      myData$FlowGroupId == "Emission: BG Slow -> Atmosphere Temp [Type]"
    ] <- flowMultBGStoAtm
    
    saveDatasheet(myScenario, myData, "stsim_FlowMultiplier", append = FALSE, force = TRUE)
    
    rm(myScenario,myData)
    
    run(projectName, scenario = scenarioName)

    myScenario <- getScenarioExact(projectName, scenarioName)

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

    carbonMean <- myData %>% 
      filter(Timestep == 3565) %>% 
      filter(StockGroupId == "DOM: Belowground Slow [Type]") %>% 
      pull(Amount)

    diffPerSign <- ((carbonMean - targetValue) /
      mean(c(carbonMean, targetValue))) * 100

    print(diffPerSign)
    print(emissionsInOut)
  }
  
  print(diffPerSign)
  print(testE, n = nrow(testE))
}
