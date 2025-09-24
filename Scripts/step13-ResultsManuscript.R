# ApexRMS
# Updated 2025-03-25
# Run after step13-FiguresManuscriptSpatial.R
# This script extracts values for the results section

library(rsyncrosim)
library(tidyverse)
library(ggplot2)
library(terra)

options(scipen = 999)
old <- options(pillar.sigfig = 10)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

rootPath <- "E:/gitprojects/A329-LucasBarataria/"

dataPath <- "Data/"
modelPath <- "Models/"

modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary <- ssimLibrary(name = paste0(modelFullPath, "/", modelName, ".ssim"),
                         session = mySession)

myProject <- rsyncrosim::project(myLibrary, project="Definitions")

pathOut <- paste0(rootPath,"Models/",modelName,"/OutputFigures/")

pathOutSpatial <- paste0(pathOut,"Spatial/")

if(!dir.exists(pathOutSpatial)){
  dir.create(pathOutSpatial)
}

pathOutManuscript <- paste0(pathOutSpatial,"Manuscript")

if(!dir.exists(pathOutManuscript)){
  dir.create(pathOutManuscript)
}

# Extract stocks 
scenarioList <- scenario(myProject, summary = T, results = T)

scenariosTable <- c("Original Oak Gum Cypress Forest",
                    "Palustrine Forested Wetland: Add Uncertainty",
                    "Palustrine Emergent Wetland: Add Uncertainty",
                    "Estuarine Emergent Wetland: Add Uncertainty",
                    "Basin Baseline",
                    "Basin No Palustrine Forested Wetland",
                    "Basin IPCC")

scenariosTimeStep <- c(2016,
                       2016,
                       2016,
                       2016,
                       2016,
                       2016,
                       2016)

stateClassTable <- datasheet(myProject, name = "stsim_StateClass")
scForest <- grep("Forest:",stateClassTable$Name,value = T)
scWater <- c("Water: All",
             "Water: Previously Emergent Wetland",
             "Water: Previously Forested Wetland")
scShore <- c("Wetland: Unconsolidated Shore",
             "Wetland: Unvegetated Emergent",
             "Wetland: Unvegetated Forested")
scOther <- stateClassTable$Name[!(stateClassTable$Name %in% c(scForest,
                                                              "Wetland: Palustrine Forested",
                                                              "Wetland: Palustrine Emergent",
                                                              "Wetland: Estuarine Emergent",
                                                              scWater,
                                                              scShore))]

# stateClassKeep <- c(scForest,
#                     "Wetland: Palustrine Forested",
#                     "Wetland: Palustrine Emergent",
#                     "Wetland: Estuarine Emergent",
#                     scOther)

# Summarize Total Ecosystem Carbon

stocksKeep <- c("DOM: Soil",
                "Ecosystem Carbon Storage (tons C)",
                "Atmosphere [Type]",
                "Atmosphere Temp [Type]",
                "Atmosphere: CH4 [Type]",
                "Atmosphere: CO [Type]",
                "Atmosphere: CO2 [Type]",
                "Aquatic [Type]")

stocksAtm <- c("Atmosphere [Type]",
                "Atmosphere Temp [Type]",
                "Atmosphere: CH4 [Type]",
                "Atmosphere: CO [Type]",
                "Atmosphere: CO2 [Type]")

stockTable <- data.frame(Scenario = NA,
                         StockGroup = NA,
                         StateClass = NA,
                         AmountT = NA)

for (i in 1:length(scenariosTable)){
  
  id1 <- scenarioList$ScenarioId[grep(scenariosTable[i],scenarioList$Name)]
  myScenario1 <- scenario(myProject, scenario=max(id1))
  myDataStock1 <- datasheet(myScenario1, "stsim_OutputStock")
  
  myDataStock1s <- myDataStock1 %>%
    #filter(StateClassId %in% stateClassKeep) %>%
    filter(StockGroupId %in% stocksKeep) %>%
    filter(Timestep == scenariosTimeStep[i]) %>%
    group_by(StockGroupId,StateClassId) %>%
    summarize(AmountI = mean(Amount,na.rm = T)) %>%
    ungroup() %>%
    mutate(StockGroup = case_when(StockGroupId %in% stocksAtm ~ "Atmosphere",
                                     StockGroupId == "DOM: Soil"~"Soil",
                                     StockGroupId == "Ecosystem Carbon Storage (tons C)"~"Ecosystem",
                                     StockGroupId == "Aquatic [Type]"~"Aquatic")) %>%
    mutate(StateClass = case_when(StateClassId %in% scForest ~ "Upland Forest",
                                  StateClassId %in% scOther ~ "Other Land",
                                  StateClassId %in% scWater ~ "Water",
                                  StateClassId %in% scShore ~ "Shore",
                                  !(StateClassId %in% c(scForest,scOther,scWater,scShore)) ~ StateClassId)) %>%
    group_by(StockGroup,StateClass) %>%
    summarize(AmountT = round(sum(AmountI,na.rm = T),2)) %>%
    ungroup() %>%
    mutate(Scenario = scenariosTable[i])
  
  stockTable <- stockTable %>%
    add_row(myDataStock1s)
  
  rm(id1,myScenario1,myDataStock1,myDataStock1s)
  

}

stockTable <- stockTable[-1,]

stockTable <- stockTable %>%
  rename(AmountTonsC = AmountT) %>%
  mutate(ScenarioName = case_when(Scenario == "Original Oak Gum Cypress Forest"~"Oak Gum Cypress Forest",
                                  Scenario == "Palustrine Forested Wetland: Add Uncertainty"~"Palustrine Forested Wetland",
                                  Scenario == "Palustrine Emergent Wetland: Add Uncertainty"~"Palustrine Emergent Wetland",
                                  Scenario == "Estuarine Emergent Wetland: Add Uncertainty"~"Estuarine Emergent Wetland",
                                  Scenario == "Basin Baseline"~"Baseline",
                                  Scenario == "Basin No Palustrine Forested Wetland"~"No Palustrine Forested Wetland",
                                  Scenario == "Basin IPCC"~"IPCC")) %>%
  select(ScenarioName,StateClass,StockGroup,AmountTonsC)

write.csv(stockTable,
          paste0(pathOutManuscript,"/stockTable.csv"),
          row.names = F)

stockTableSingleCell <- stockTable %>%
  filter(ScenarioName %in% c("Oak Gum Cypress Forest",
                            "Palustrine Forested Wetland",
                            "Palustrine Emergent Wetland",
                            "Estuarine Emergent Wetland"))

stockTableSingleCell

AtmValue <- abs(min(stockTableSingleCell$AmountTonsC[stockTableSingleCell$StockGroup == "Atmosphere"]))

ScenarioNames <- c("Oak Gum Cypress Forest",
                  "Palustrine Forested Wetland",
                  "Palustrine Emergent Wetland",
                  "Estuarine Emergent Wetland")

for (i in 1:length(ScenarioNames)){
  
  print(ScenarioNames[i])
  
  stockTableSingleCell1 <- stockTableSingleCell %>%
    filter(ScenarioName == ScenarioNames[i]) %>%
    filter(StockGroup %in% c("Atmosphere",
                            "Aquatic",
                            "Ecosystem"))
  
  stockTableSingleCell1$AmountTonsC[stockTableSingleCell1$StockGroup == "Atmosphere"] <- stockTableSingleCell1$AmountTonsC[stockTableSingleCell1$StockGroup == "Atmosphere"] + AtmValue
  
  totalC <- sum(stockTableSingleCell1$AmountTonsC)
  lateralC <- stockTableSingleCell1$AmountTonsC[stockTableSingleCell1$StockGroup == "Aquatic"]
  
  print(round((lateralC/totalC)*100,2))
  
  rm(stockTableSingleCell1,totalC,lateralC)
  
}


stockTableBasin <- stockTable %>%
  filter(ScenarioName == "Baseline") %>%
  filter(StockGroup %in% c("Atmosphere",
                          "Aquatic",
                          "Ecosystem"))

# need to add AtmValue*Area of Atm above each class

# id1 <- scenarioList$ScenarioId[grep("Basin Baseline",scenarioList$Name)]
# myScenario1 <- scenario(myProject, scenario=max(id1))
# myDataLandS <- datasheet(myScenario1, "stsim_OutputStratumState")
# 
# myDataLandS <- myDataLandS %>%
#   filter(Timestep == 2016)
# 
# totalHa <- sum(myDataLandS$Amount)
# AtmValue <- abs(min(stockTableSingleCell$AmountTonsC[stockTableSingleCell$StockGroup == "Atmosphere"]))
# AtmValueHa <- totalHa*AtmValue

totalBasin <- sum(stockTableBasin$AmountTonsC)

round((stockTableBasin$AmountTonsC[stockTableBasin$StateClass == "Wetland: Palustrine Emergent" &
                              stockTableBasin$StockGroup == "Aquatic"]/totalBasin)*100,2)

round((stockTableBasin$AmountTonsC[stockTableBasin$StateClass == "Wetland: Estuarine Emergent" &
                              stockTableBasin$StockGroup == "Aquatic"]/totalBasin)*100,2)

round((stockTableBasin$AmountTonsC[stockTableBasin$StateClass == "Wetland: Palustrine Forested" &
                              stockTableBasin$StockGroup == "Aquatic"]/totalBasin)*100,2)


# NECB

plotFlows<- c("Annual Net Ecosystem Carbon Balance (tons C per year)",
              "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)")

timePeriod <- c(2001,2016)
#timePeriod <- c(2006,2010)

# scenariosDiff <- c("Basin Baseline",
#                    "Basin No Palustrine Forested Wetland")

scenariosDiff <- c("Basin Baseline",
                   "Basin IPCC")

for (i in 1:length(plotFlows)){
  
  id2 <- scenarioList$ScenarioId[grep(scenariosDiff[1],scenarioList$Name)]
  myScenario2 <- scenario(myProject, scenario=max(id2))
  myDataFlux2 <- datasheet(myScenario2, "stsim_OutputFlow")
  
  id3 <- scenarioList$ScenarioId[grep(scenariosDiff[2],scenarioList$Name)]
  myScenario3 <- scenario(myProject, scenario=max(id3))
  myDataFlux3 <- datasheet(myScenario3, "stsim_OutputFlow")
  
  myDataFlux2necb <- myDataFlux2 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    filter(Timestep >= timePeriod[1]) %>%
    filter(Timestep <= timePeriod[2]) %>%
    group_by(Timestep) %>%
    summarize(necbB = sum(Amount, na.rm = T))
  
  myDataFlux3necb <- myDataFlux3 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    filter(Timestep >= timePeriod[1]) %>%
    filter(Timestep <= timePeriod[2]) %>%
    group_by(Timestep) %>%
    summarize(necbA = sum(Amount, na.rm = T))
  
  myDataFlux2necb <- myDataFlux2necb %>%
    left_join(myDataFlux3necb) %>%
    mutate(diff = necbB-necbA,
           cumulativeDiff = cumsum(diff))
  
  print(plotFlows[i])
  print(myDataFlux2necb[myDataFlux2necb$Timestep == timePeriod[2],])
  
  rm(id2,id3,
     myScenario2,myScenario3,
     myDataFlux2,myDataFlux3)
  
  rm(myDataFlux2necb,myDataFlux3necb)
}

# Difference stocks

plotStocks<- c("Ecosystem Carbon Storage (tons C)")

#timePeriod <- c(2001,2016)
timePeriod <- c(2006,2010)

# scenariosDiff <- c("Basin Baseline",
#                    "Basin No Palustrine Forested Wetland")

scenariosDiff <- c("Basin Baseline",
                   "Basin IPCC")

for (i in 1:length(plotStocks)){
  
  id2 <- scenarioList$ScenarioId[grep(scenariosDiff[1],scenarioList$Name)]
  myScenario2 <- scenario(myProject, scenario=max(id2))
  myDataStock2 <- datasheet(myScenario2, "stsim_OutputStock")
  
  id3 <- scenarioList$ScenarioId[grep(scenariosDiff[2],scenarioList$Name)]
  myScenario3 <- scenario(myProject, scenario=max(id3))
  myDataStock3 <- datasheet(myScenario3, "stsim_OutputStock")
  
  myDataStock2e <- myDataStock2 %>%
    filter(StockGroupId == plotStocks[i]) %>%
    filter(Timestep >= timePeriod[1]) %>%
    filter(Timestep <= timePeriod[2]) %>%
    group_by(Timestep) %>%
    summarize(eB = sum(Amount, na.rm = T))
  
  myDataStock3e <- myDataStock3 %>%
    filter(StockGroupId == plotStocks[i]) %>%
    filter(Timestep >= timePeriod[1]) %>%
    filter(Timestep <= timePeriod[2]) %>%
    group_by(Timestep) %>%
    summarize(eA = sum(Amount, na.rm = T))
  
  myDataStock2e <- myDataStock2e %>%
    left_join(myDataStock3e) %>%
    mutate(diff = eB-eA)
  
  print(plotStocks[i])
  print(myDataStock2e[myDataStock2e$Timestep == timePeriod[2],])
  
  rm(id2,id3,
     myScenario2,myScenario3,
     myDataStock2,myDataStock3)
  
  rm(myDataStock2e,myDataStock3e)
}


# For changes in NECB associated with transitions of wetland to water

id2 <- scenarioList$ScenarioId[grep("Basin Baseline",scenarioList$Name)]
myScenario2 <- scenario(myProject, scenario=max(id2))

start1 <- c("2001","2006","2010")
end1 <- c("2006","2010","2016")

stockGroupIds <- datasheet(myProject, name = "stsim_StockGroup", includeKey = T)
flowGroupIds <- datasheet(myProject, name = "stsim_FlowGroup", includeKey = T)

listLandCover <- list.files(paste0(rootPath,"/Models/",
                                   modelName,"/",
                                   modelName,".ssim.data/Scenario-",
                                   scenarioId(myScenario2),
                                   "/stsim_OutputSpatialState"),
                            pattern = ".tif",
                            full.names = T)

listFluxes <- list.files(paste0(rootPath,"/Models/",
                                modelName,"/",
                                modelName,".ssim.data/Scenario-",
                                scenarioId(myScenario2),
                                "/stsim_OutputAverageSpatialFlowGroup"),
                         pattern = ".tif",
                         full.names = T)

fluxId1 <- flowGroupIds %>%
  filter(Name == "Annual Net Ecosystem Carbon Balance (tons C per year)") %>%
  pull(FlowGroupId)

fluxId2 <- flowGroupIds %>%
  filter(Name == "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)") %>%
  pull(FlowGroupId)

listFluxesSub1 <- grep(fluxId1,listFluxes, value = T)
listFluxesSub2 <- grep(fluxId2,listFluxes, value = T)

for (i in 1:length(start1)){

    sub1 <- grep(paste0("ts",start1[i]),listLandCover, value = T)
    sub2 <- grep(paste0("ts",end1[i]),listLandCover, value = T)
    
    r1 <- rast(sub1)
    r2 <- rast(sub2)
    
    if (i == 1){
      
      mask1 <- ifel(r1 == 90 & r2 == 14,1,0)
      mask2 <- ifel(r1 == 96 & r2 == 13,1,0)
      mask3 <- ifel(r1 == 95 & r2 == 13,1,0)
      
      maskF <- mask1+mask2+mask3
      maskKeep <- maskF
      
    } else if (i > 1){
      
      mask0 <- ifel(maskF == 1 & r2 %in% c(13,14,11),1,0)
      rm(maskF)
      
      mask1 <- ifel(r1 == 90 & r2 == 14,1,0)
      mask2 <- ifel(r1 == 96 & r2 == 13,1,0)
      mask3 <- ifel(r1 == 95 & r2 == 13,1,0)
      
      maskF <- mask0+mask1+mask2+mask3
      maskKeep <- c(maskKeep,maskF)
      
      rm(mask0)
      
    }
 
    if (i %in% c(1,2)){
      
      timeAll <- c((as.numeric(end1[i])):(as.numeric(end1[i+1])-1))
      
    } else if (i == 3) {
      
      timeAll <- as.numeric(end1[i])
      
    }
    
    
    for (k in 1:length(timeAll)){
      
      listFluxesSub1R <- grep(paste0("ts",timeAll[k]),listFluxesSub1, value = T)
      listFluxesSub2R <- grep(paste0("ts",timeAll[k]),listFluxesSub2, value = T)
      
      if (i == 1 & k == 1){
        
        r3 <- rast(listFluxesSub1R)
        r4 <- rast(listFluxesSub2R)
        
        rT <- r3*maskF
        rE <- r4*maskF
        
      } else {
        
        r3 <- rast(listFluxesSub1R)
        r4 <- rast(listFluxesSub2R)
        
        rT <- c(rT,r3*maskF)
        rE <- c(rE,r4*maskF)
        
      }
      
      rm(r3,r4,listFluxesSub1R,listFluxesSub2R)
      
    }
    
    print(paste0("Area for ",timeAll[1]," to ",timeAll[length(timeAll)]," is ",
                 round((global(maskF, sum, na.rm = T)*(30*30/10000)),2),
                 " ha"))
    
    print(paste0("NECB for 2006 to ",timeAll[length(timeAll)]," is ",
                 round(global(rT,sum, na.rm = T),2),
                 " tons C"))
    
    print(paste0("NECB for 2006 to ",timeAll[length(timeAll)]," is ",
                 round(global(rE,sum, na.rm = T),2),
                 " tons CO2-eq"))
    
    rm(sub1,sub2,r1,r2,mask1,mask2,mask3,
       timeAll)
    gc()
  
}

print(paste0("NECB for all is ",
             round(global(app(rT,sum,na.rm = T),sum, na.rm = T),2),
             " tons C"))

print(paste0("NECB for all is ",
             round(global(app(rE,sum,na.rm = T),sum, na.rm = T),2),
             " tons CO2eq"))

years <- 2006:2016

NECBsummary <- data.frame(year = years,
                          area = NA,
                          NECBperHa = NA)

for (y in 1:length(years)){
  
  if (years[y] <= 2009){
    
    maskYear <- 1
    
  } else if (years[y] >= 2010 & years[y] < 2016){
    
    maskYear <- 2
    
  } else if (years[y] >= 2016){
    
    maskYear <- 3
    
  }
  
  rTsub <- mask(rT[[y]],maskKeep[[maskYear]], maskvalue = 1, inverse = T)
  NECBsummary$year[y] <- years[y]
  NECBsummary$NECBperHa[y] <- global(rTsub, mean, na.rm = T)$mean
  NECBsummary$area[y] <- global(maskKeep[[maskYear]], sum, na.rm = T)$sum*30*30/10000
  
  rm(maskYear,rTsub)
  
}

NECBsummary$NECBtotal <- NECBsummary$NECBperHa*NECBsummary$area
NECBsummary

sum(NECBsummary$NECBtotal, na.rm = T)
sum(NECBsummary$NECBtotal[NECBsummary$year %in% c(2006:2010)], na.rm = T)

sum(NECBsummary$NECBperHa, na.rm = T)
sum(NECBsummary$NECBperHa[NECBsummary$year %in% c(2006:2010)], na.rm = T)

mean(NECBsummary$NECBperHa, na.rm = T)
mean(NECBsummary$NECBperHa[NECBsummary$year %in% c(2006:2010)], na.rm = T)

rm(NECBsummary)

NECBsummary <- data.frame(year = years,
                          area = NA,
                          NECBperHa = NA)

for (y in 1:length(years)){
  
  if (years[y] <= 2009){
    
    maskYear <- 1
    
  } else if (years[y] >= 2010 & years[y] < 2016){
    
    maskYear <- 2
    
  } else if (years[y] >= 2016){
    
    maskYear <- 3
    
  }
  
  rEsub <- mask(rE[[y]],maskKeep[[maskYear]], maskvalue = 1, inverse = T)
  NECBsummary$year[y] <- years[y]
  NECBsummary$NECBperHa[y] <- global(rEsub, mean, na.rm = T)$mean
  NECBsummary$area[y] <- global(maskKeep[[maskYear]], sum, na.rm = T)$sum*30*30/10000
  
  rm(maskYear,rEsub)
  
}

NECBsummary$NECBtotal <- NECBsummary$NECBperHa*NECBsummary$area
NECBsummary

sum(NECBsummary$NECBtotal, na.rm = T)
sum(NECBsummary$NECBtotal[NECBsummary$year %in% c(2006:2010)], na.rm = T)

sum(NECBsummary$NECBperHa, na.rm = T)
sum(NECBsummary$NECBperHa[NECBsummary$year %in% c(2006:2010)], na.rm = T)

mean(NECBsummary$NECBperHa, na.rm = T)
mean(NECBsummary$NECBperHa[NECBsummary$year %in% c(2006:2010)], na.rm = T)

rm(NECBsummary)

rm(rT)
rm(rE)
rm(maskKeep)
gc()

for (i in 1:length(start1)){
  
  sub1 <- grep(paste0("ts",start1[i]),listLandCover, value = T)
  sub2 <- grep(paste0("ts",end1[i]),listLandCover, value = T)
  
  r1 <- rast(sub1)
  r2 <- rast(sub2)
  
  if (i == 1){
    
    mask1 <- ifel(r1 %in% c(13,14,11) & r2 == 90,1,0)
    mask2 <- ifel(r1 %in% c(13,14,11) & r2 == 96,1,0)
    mask3 <- ifel(r1 %in% c(13,14,11) & r2 == 95,1,0)
    
    maskF <- mask1+mask2+mask3
    
    maskKeep <- maskF
    
  } else if (i > 1){
    
    mask0 <- ifel(maskF == 1 & r2 %in% c(90,96,95),1,0)
    rm(maskF)
    
    mask1 <- ifel(r1 %in% c(13,14,11) & r2 == 90,1,0)
    mask2 <- ifel(r1 %in% c(13,14,11) & r2 == 96,1,0)
    mask3 <- ifel(r1 %in% c(13,14,11) & r2 == 95,1,0)
    
    maskF <- mask0+mask1+mask2+mask3
    
    maskKeep <- c(maskKeep,maskF)
    
    rm(mask0)
    
  }
  
  if (i %in% c(1,2)){
    
    timeAll <- c((as.numeric(end1[i])):(as.numeric(end1[i+1])-1))
    
  } else if (i == 3) {
    
    timeAll <- as.numeric(end1[i])
    
  }
  
  
  for (k in 1:length(timeAll)){
    
    listFluxesSub1R <- grep(paste0("ts",timeAll[k]),listFluxesSub1, value = T)
    listFluxesSub2R <- grep(paste0("ts",timeAll[k]),listFluxesSub2, value = T)
    
    if (i == 1 & k == 1){
      
      r3 <- rast(listFluxesSub1R)
      r4 <- rast(listFluxesSub2R)
      
      rT <- r3*maskF
      rE <- r4*maskF
      
    } else {
      
      r3 <- rast(listFluxesSub1R)
      r4 <- rast(listFluxesSub2R)
      
      rT <- c(rT,r3*maskF)
      rE <- c(rE,r4*maskF)
      
    }
    
    rm(r3,r4,listFluxesSub1R,listFluxesSub2R)
    
  }
  
  print(paste0("Area for ",timeAll[1]," to ",timeAll[length(timeAll)]," is ",
               round((global(maskF, sum, na.rm = T)*(30*30/10000)),2),
               " ha"))
  
  print(paste0("NECB for 2006 to ",timeAll[length(timeAll)]," is ",
               round(global(rT,sum, na.rm = T),2),
               " tons C"))
  
  print(paste0("NECB for 2006 to ",timeAll[length(timeAll)]," is ",
               round(global(rE,sum, na.rm = T),2),
               " tons CO2-eq"))
  
  rm(sub1,sub2,r1,r2,mask1,mask2,mask3,
     timeAll)
  gc()
  
}

print(paste0("NECB for all is ",
             round(global(app(rT,sum,na.rm = T),sum, na.rm = T),2),
             " tons C"))

print(paste0("NECB for all is ",
             round(global(app(rE,sum,na.rm = T),sum, na.rm = T),2),
             " tons CO2eq"))

years <- 2006:2016

NECBsummary <- data.frame(year = years,
                          area = NA,
                          NECBperHa = NA)

for (y in 1:length(years)){
  
  if (years[y] <= 2009){
    
    maskYear <- 1
    
  } else if (years[y] >= 2010 & years[y] < 2016){
    
    maskYear <- 2
    
  } else if (years[y] >= 2016){
    
    maskYear <- 3
    
  }
  
  rTsub <- mask(rT[[y]],maskKeep[[maskYear]], maskvalue = 1, inverse = T)
  NECBsummary$year[y] <- years[y]
  NECBsummary$NECBperHa[y] <- global(rTsub, mean, na.rm = T)$mean
  NECBsummary$area[y] <- global(maskKeep[[maskYear]], sum, na.rm = T)$sum*30*30/10000
  
  rm(maskYear,rTsub)
  
}

NECBsummary$NECBtotal <- NECBsummary$NECBperHa*NECBsummary$area
NECBsummary

sum(NECBsummary$NECBtotal, na.rm = T)
sum(NECBsummary$NECBtotal[NECBsummary$year %in% c(2006:2010)], na.rm = T)

sum(NECBsummary$NECBperHa, na.rm = T)
sum(NECBsummary$NECBperHa[NECBsummary$year %in% c(2006:2010)], na.rm = T)

mean(NECBsummary$NECBperHa, na.rm = T)
mean(NECBsummary$NECBperHa[NECBsummary$year %in% c(2006:2010)], na.rm = T)

rm(NECBsummary)

NECBsummary <- data.frame(year = years,
                          area = NA,
                          NECBperHa = NA)

for (y in 1:length(years)){
  
  if (years[y] <= 2009){
    
    maskYear <- 1
    
  } else if (years[y] >= 2010 & years[y] < 2016){
    
    maskYear <- 2
    
  } else if (years[y] >= 2016){
    
    maskYear <- 3
    
  }
  
  rEsub <- mask(rE[[y]],maskKeep[[maskYear]], maskvalue = 1, inverse = T)
  NECBsummary$year[y] <- years[y]
  NECBsummary$NECBperHa[y] <- global(rEsub, mean, na.rm = T)$mean
  NECBsummary$area[y] <- global(maskKeep[[maskYear]], sum, na.rm = T)$sum*30*30/10000
  
  rm(maskYear,rEsub)
  
}

NECBsummary$NECBtotal <- NECBsummary$NECBperHa*NECBsummary$area
NECBsummary

sum(NECBsummary$NECBtotal, na.rm = T)
sum(NECBsummary$NECBtotal[NECBsummary$year %in% c(2006:2010)], na.rm = T)

sum(NECBsummary$NECBperHa, na.rm = T)
sum(NECBsummary$NECBperHa[NECBsummary$year %in% c(2006:2010)], na.rm = T)

mean(NECBsummary$NECBperHa, na.rm = T)
mean(NECBsummary$NECBperHa[NECBsummary$year %in% c(2006:2010)], na.rm = T)

rm(NECBsummary)

rm(rT)
rm(rE)
gc()

# # Net Changes in Land Cover
# 
# timeStepPair <- c(2001,2016)
# 
# stateClassTable <- datasheet(myProject, name = "stsim_StateClass") %>%
#   select(Name,Id)
# 
# scenList <- c("Basin Baseline")
# 
# for (i in 1:length(scenList)){
#   
#   scenID <- scenarioList$ScenarioId[grep(scenList[i],scenarioList$Name)]
#   
#   myScenario <- scenario(myProject, scenario=max(scenID))
#   
#   listLandCover <- list.files(paste0(rootPath,"/Models/",
#                                      modelName,"/",
#                                      modelName,".ssim.data/Scenario-",
#                                      scenarioId(myScenario),
#                                      "/stsim_OutputSpatialState"),
#                               pattern = ".tif",
#                               full.names = T)
#   
#   sub1 <- grep(paste0("ts",timeStepPair[1]),listLandCover, value = T)
#   sub2 <- grep(paste0("ts",timeStepPair[2]),listLandCover, value = T)
#   
#   r1 <- rast(sub1)
#   r2 <- rast(sub2)
#   
#   rW1 <- ifel(r1 %in% c(90,96,95),1,0)
#   rW2 <- ifel(r2 %in% c(90,96,95),1,0)
#   
#   area1 <- global(rW1,sum, na.rm = T)*30*30/10000
#   area2 <- global(rW2,sum, na.rm = T)*30*30/10000
#     
#   print(area1-area2)
# 
# }



# Summarize land cover changes

# Net changes from 2001-2016. Would not include an area transitioning from wetland to water and back to wetland

tabT <- read.csv(paste0(pathOutManuscript,"/LandCoverT_2001_2016.csv"))

tabTwet <- tabT %>%
  filter(LandClassStart == "Water") %>%
  filter(LandClassEnd %in% c("Estuarine Emergent Wetland",
                               "Palustrine Emergent Wetland",
                               "Palustrine Forested Wetland")) %>%
  summarize(sum = sum(Area_ha, na.rm = T)) %>%
  pull(sum)

tabTwet

tabTwat <- tabT %>%
  filter(LandClassEnd == "Water") %>%
  filter(LandClassStart %in% c("Estuarine Emergent Wetland",
                             "Palustrine Emergent Wetland",
                             "Palustrine Forested Wetland")) %>%
  summarize(sum = sum(Area_ha, na.rm = T)) %>%
  pull(sum)

tabTwat

rm(tabT,tabTwet,tabTwat)

yearsL <- c("2006","2010","2016")

for (i in 1:length(yearsL)){
  
  tabT <- read.csv(paste0(pathOutManuscript,"/LandCoverT_",yearsL[i],".csv"))
  
  print(yearsL[i])
  
  tabTwet <- tabT %>%
    filter(LandClassStart == "Water") %>%
    filter(LandClassEnd %in% c("Estuarine Emergent Wetland",
                               "Palustrine Emergent Wetland",
                               "Palustrine Forested Wetland")) %>%
    summarize(sum = sum(Area_ha, na.rm = T)) %>%
    pull(sum)
  
  print(tabTwet)
  
  tabTwat <- tabT %>%
    filter(LandClassEnd == "Water") %>%
    filter(LandClassStart %in% c("Estuarine Emergent Wetland",
                                 "Palustrine Emergent Wetland",
                                 "Palustrine Forested Wetland")) %>%
    summarize(sum = sum(Area_ha, na.rm = T)) %>%
    pull(sum)
  
  print(tabTwat)
  
}


