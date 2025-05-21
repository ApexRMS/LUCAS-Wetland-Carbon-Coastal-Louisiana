# Created by Amanda Schwantes, ApexRMS
# Updated 2025-03-25
# Run after step9-SingleCellTransitions.R
# This script creates figures for spatial scenario results

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

pathOutLandCover <- paste0(pathOutSpatial,"Land Cover")

if(!dir.exists(pathOutLandCover)){
  dir.create(pathOutLandCover)
}

pathOutCarbonStocks <- paste0(pathOutSpatial,"Carbon Stocks")

if(!dir.exists(pathOutCarbonStocks)){
  dir.create(pathOutCarbonStocks)
}

pathOutCarbonFluxes <- paste0(pathOutSpatial,"Carbon Fluxes")

if(!dir.exists(pathOutCarbonFluxes)){
  dir.create(pathOutCarbonFluxes)
}

# Land Cover Change over time
scenarioList <- scenario(myProject, summary = T, results = T)

id1 <- scenarioList$ScenarioId[grep("1 No Land Cover Change and Climate",scenarioList$Name)]
id2 <- scenarioList$ScenarioId[grep("2 Land Cover Change and Climate",scenarioList$Name)]
id3 <- scenarioList$ScenarioId[grep("3 Land Cover Change, Climate, Erosion",scenarioList$Name)]
id4 <- scenarioList$ScenarioId[grep("4 Land Cover Change, Climate, and No Forested Wetland",scenarioList$Name)]

myScenario1 <- scenario(myProject, scenario=max(id1))
myScenario2 <- scenario(myProject, scenario=max(id2))
myScenario3 <- scenario(myProject, scenario=max(id3))
myScenario4 <- scenario(myProject, scenario=max(id4))

# Summarize Total Ecosystem Carbon

myDataStock1 <- datasheet(myScenario1, "stsim_OutputStock")
myDataStock2 <- datasheet(myScenario2, "stsim_OutputStock")
myDataStock3 <- datasheet(myScenario3, "stsim_OutputStock")
myDataStock4 <- datasheet(myScenario4, "stsim_OutputStock")

plotStocksChange <- c("Biomass: Aboveground",
                "Biomass: Belowground",
                "DOM: Litter",
                "Biomass: Fine Root [Type]",
                "Biomass: Foliage [Type]",
                "DOM: Aboveground Very Fast [Type]",
                "DOM: Belowground Slow [Type]",
                "DOM: Belowground Very Fast [Type]",
                "DOM: Soil",
                "Deep Soil [Type]",
                "Ecosystem Carbon Storage (tons C)")

for (i in 1:length(plotStocksChange)){
  
  plotName <- gsub("[Type]","",gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotStocksChange[i]), fixed = T),fixed = T)), fixed = T)
  
  myDataStock1c <- myDataStock1 %>%
    filter(StockGroupId == plotStocksChange[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "No LC change",
           Color = "#D4621F",
           Type = "solid")
  
  myDataStock2c <- myDataStock2 %>%
    filter(StockGroupId == plotStocksChange[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Schoolmaster",
           Color = "#349C78",
           Type = "solid")
  
  myDataStock3c <- myDataStock3 %>%
    filter(StockGroupId == plotStocksChange[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "IPCC",
           Color = "#7471B1",
           Type = "solid")
  
  myDataStock4c <- myDataStock4 %>%
    filter(StockGroupId == plotStocksChange[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "No palustrine forest",
           Color = "#A3762A",
           Type = "solid")
  
  myDataCarbonStorageA <- myDataStock2c %>%
    bind_rows(myDataStock3c)
  
  myDataCarbonStorageA$mean <- myDataCarbonStorageA$mean/1000000
  
  myDataStock2c$Scenario <- c("Historic observed LC change")
  
  myDataCarbonStorageB <- myDataStock2c %>%
    bind_rows(myDataStock4c)
  
  myDataCarbonStorageB$mean <- myDataCarbonStorageB$mean/1000000
  
  table(table(myDataCarbonStorageA$Timestep) == 2)
  table(table(myDataCarbonStorageB$Timestep) == 2)
  
  colB <- as.character(myDataCarbonStorageB$Color)
  names(colB) <- as.character(myDataCarbonStorageB$Scenario)
  
  lineTypeB <- as.character(myDataCarbonStorageB$Type)
  names(lineTypeB) <- as.character(myDataCarbonStorageB$Scenario)
  
  p3 <- ggplot(myDataCarbonStorageB, aes(x = Timestep, y = mean, color = Scenario, group = Scenario, linetype = Scenario)) +
    geom_line(linewidth = 0.8) +
    theme_bw() +
    scale_color_manual(values=colB, breaks = c("Historic observed LC change",
                                               "No palustrine forest")) +
    scale_linetype_manual(values=lineTypeB, breaks = c("Historic observed LC change",
                                                       "No palustrine forest")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nYear") + 
    ylab(paste0(gsub("(tons C)","",gsub(" [Type]","",plotStocksChange[i], fixed = T), fixed = T),"\n(millions of tons C)\n"))
    #ylim(0,NA)
  
  p3
  
  ggsave(paste0(pathOutCarbonStocks,"/",plotName,"_","NoForestWetland",".png"), p3, width = 3.8, height = 4.5, dpi = 600)
  
  colA <- as.character(myDataCarbonStorageA$Color)
  names(colA) <- as.character(myDataCarbonStorageA$Scenario)
  
  lineTypeA <- as.character(myDataCarbonStorageA$Type)
  names(lineTypeA) <- as.character(myDataCarbonStorageA$Scenario)
  
  p4 <- ggplot(myDataCarbonStorageA, aes(x = Timestep, y = mean, color = Scenario, group = Scenario, linetype = Scenario)) +
    geom_line(linewidth = 0.8) +
    theme_bw() +
    scale_color_manual(values=colA, breaks = c("Schoolmaster",
                                               "IPCC")) +
    scale_linetype_manual(values=lineTypeA, breaks = c("Schoolmaster",
                                                       "IPCC")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nYear") + 
    ylab(paste0(gsub("(tons C)","",gsub(" [Type]","",plotStocksChange[i], fixed = T), fixed = T),"\n(millions of tons C)\n"))
  #ylim(0,NA)
  
  p4
  
  ggsave(paste0(pathOutCarbonStocks,"/",plotName,"_","LandCover",".png"), p4, width = 3.8, height = 4.5, dpi = 600)
  
  
  rm(myDataStock1c,myDataStock2c,myDataStock3c,myDataStock4c,
     myDataCarbonStorageA,myDataCarbonStorageB,plotName)
  
}

# Summarize Flows

myDataFlux1 <- datasheet(myScenario1, "stsim_OutputFlow")
myDataFlux2 <- datasheet(myScenario2, "stsim_OutputFlow")
myDataFlux3 <- datasheet(myScenario3, "stsim_OutputFlow")
myDataFlux4 <- datasheet(myScenario4, "stsim_OutputFlow")

#unique(myDataFlux1$FlowGroupId)

plotFlows<- c("Annual Emissions: CO2 (tons C per year)",             
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
              "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)")

for (i in 1:length(plotFlows)){
  
  plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotFlows[i]), fixed = T),fixed = T))
  
  myDataFlux1c <- myDataFlux1 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "No LC change",
           Color = "#D4621F",
           Type = "solid")
  
  myDataFlux2c <- myDataFlux2 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "Schoolmaster",
           Color = "#349C78",
           Type = "solid")
  
  myDataFlux3c <- myDataFlux3 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "IPCC",
           Color = "#7471B1",
           Type = "solid")
  
  myDataFlux4c <- myDataFlux4 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "No palustrine forest",
           Color = "#A3762A",
           Type = "solid")
  
  myDataNECBA <- myDataFlux2c %>%
    bind_rows(myDataFlux3c)
  
  myDataNECBA$mean <- myDataNECBA$mean/1000000
  myDataNECBA$min <- myDataNECBA$min/1000000
  myDataNECBA$max <- myDataNECBA$max/1000000
  
  minVal <- min(myDataNECBA$min)
  maxVal <- max(myDataNECBA$max)
  
  if(minVal > 0 & maxVal >0){
    minVal = 0
  } else if (minVal < 0 & maxVal < 0){
    maxVal = 0
  } 
  
  col <- as.character(myDataNECBA$Color)
  names(col) <- as.character(myDataNECBA$Scenario)
  
  lineType <- as.character(myDataNECBA$Type)
  names(lineType) <- as.character(myDataNECBA$Scenario)
  
  p6 <- ggplot(myDataNECBA, aes(x = Timestep, y = mean, color = Scenario, group = Scenario, linetype = Scenario)) +
    geom_line(linewidth = 0.8) +
    theme_bw() +
    scale_color_manual(values=col, breaks = c("Schoolmaster",
                                              "IPCC")) +
    scale_linetype_manual(values=lineType, breaks = c("Schoolmaster",
                                                      "IPCC")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nYear") + 
    ylab(paste0(gsub("Annual Net Ecosystem Carbon Balance","Annual NECB",
                     gsub("(","\n(millions of ",plotFlows[i], fixed = T), fixed = T),"\n"))+
    ylim(minVal,maxVal)
  
  p6
  
  ggsave(paste0(pathOutCarbonFluxes,"/",plotName,"_","LandCover",".png"), p6, width = 3.8, height = 4.5, dpi = 600)
  
  rm(minVal,maxVal,col,lineType)
  
  myDataFlux2c$Scenario <- c("Historic observed LC change")
  
  myDataNECBB <- myDataFlux2c %>%
    bind_rows(myDataFlux4c)
  
  myDataNECBB$mean <- myDataNECBB$mean/1000000
  myDataNECBB$min <- myDataNECBB$min/1000000
  myDataNECBB$max <- myDataNECBB$max/1000000
  
  minVal <- min(myDataNECBB$min)
  maxVal <- max(myDataNECBB$max)
  
  if(minVal > 0 & maxVal >0){
    minVal = 0
  } else if (minVal < 0 & maxVal < 0){
    maxVal = 0
  } 
  
  col <- as.character(myDataNECBB$Color)
  names(col) <- as.character(myDataNECBB$Scenario)
  
  lineType <- as.character(myDataNECBB$Type)
  names(lineType) <- as.character(myDataNECBB$Scenario)
  
  p7 <- ggplot(myDataNECBB, aes(x = Timestep, y = mean, color = Scenario, group = Scenario, linetype = Scenario)) +
    geom_line(linewidth = 0.8) +
    theme_bw() +
    scale_color_manual(values=col, breaks = c("Historic observed LC change",
                                              "No palustrine forest")) +
    scale_linetype_manual(values=lineType, breaks = c("Historic observed LC change",
                                                      "No palustrine forest")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="bottom",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 2,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nYear") + 
    ylab(paste0(gsub("Annual Net Ecosystem Carbon Balance","Annual NECB",
                     gsub("(","\n(millions of ",plotFlows[i], fixed = T), fixed = T),"\n"))+
    ylim(minVal,maxVal)
  
  p7
  
  ggsave(paste0(pathOutCarbonFluxes,"/",plotName,"_","NoForestWetland",".png"), p7, width = 3.8, height = 4.5, dpi = 600)
  
  
  rm(myDataFlux1c,myDataFlux2c,myDataFlux3c,myDataFlux4c,
     myDataNECBA,myDataNECBB,plotName)
  
}

stockGroupIds <- datasheet(myProject, name = "stsim_StockGroup", includeKey = T)
flowGroupIds <- datasheet(myProject, name = "stsim_FlowGroup", includeKey = T)
transitionGroupIds <- datasheet(myProject, name = "stsim_TransitionGroup", includeKey = T)

# Summarize land cover change
stateClassTable <- datasheet(myProject, name = "stsim_StateClass")
#unique(stateClassTable$Name)

scOther <- stateClassTable$Name[!(stateClassTable$Name %in% c("Wetland: Estuarine Emergent",
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
                                                              "Developed: Transportation"))]

#scOther

scen <- c("2 Land Cover Change and Climate")

lookupLC <- data.frame(StateClassId = c("Wetland: Estuarine Emergent",
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
                                        scOther),
                       LandClass = c("Estuarine Emergent Wetland",
                                     "Palustrine Emergent Wetland",
                                     "Palustrine Forested Wetland",
                                     rep("Water & Shore",6),
                                     rep("Crop & Urban",8),
                                     rep("Forest, Grass, & Shrub",length(scOther))))

#lookupLC

lookupChart <- data.frame(LandClass = c("Estuarine Emergent Wetland",
                                        "Palustrine Emergent Wetland",
                                        "Palustrine Forested Wetland",
                                        "Water & Shore",
                                        "Crop & Urban",
                                        "Forest, Grass, & Shrub"),
                                        Color = c("#A91EAC",
                                                  "#EA2DEE",
                                                  "#145A5A",
                                                  "#6677D7",
                                                  "#BFA056",
                                                  "#2E9E40"))

for (s in 1:length(scen)){
  
  idS <- scenarioList$ScenarioId[grep(scen[s],scenarioList$Name)]
  
  myScenarioS <- scenario(myProject, scenario=max(idS))
  
  myDataLandS <- datasheet(myScenarioS, "stsim_OutputStratumState")
  
  landS <- myDataLandS %>%
    left_join(lookupLC, by = join_by(StateClassId)) %>%
    left_join(lookupChart, by = join_by(LandClass)) %>%
    group_by(Timestep, LandClass, Color) %>%
    summarize(Area = sum(Amount, na.rm = T)) %>%
    filter(Timestep %in% c(2001,2006,2010,2016))
  
  landSdiff <- landS %>%
    select(LandClass,Timestep,Area) %>%
    pivot_wider(names_from = Timestep, 
                values_from = Area,
                id_cols = LandClass) %>%
    mutate(`2001-2006` = `2006`-`2001`,
           `2006-2010` = `2010`-`2006`,
           `2010-2016` = `2016`-`2010`) %>%
    select(LandClass,`2001-2006`,`2006-2010`,`2010-2016`) %>%
    pivot_longer(!LandClass,
                 names_to = "Timestep", 
                 values_to = "Area") %>%
    left_join(lookupChart, by = join_by(LandClass))
  
  col <- as.character(landSdiff$Color)
  names(col) <- as.character(landSdiff$LandClass)
  
  landSdiff$Timestep <- as.factor(landSdiff$Timestep)
  
  p3 <- ggplot(landSdiff, aes(x = Timestep, y = Area, fill = LandClass, group = LandClass)) + 
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=col) +
    geom_hline(aes(yintercept=0),col = 'black', size = 0.7) +
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position="right",
          legend.title=element_blank()) +
    guides(colour = guide_legend(nrow = 5,
                                 keywidth=0.4,
                                 keyheight=0.1,
                                 default.unit="inch")) +
    xlab("\nYear") + 
    ylab("Land area change (hectares)\n")
  
  p3
  
  ggsave(paste0(pathOutLandCover,"/LandCoverChangeWater_",gsub(" ","",scen[s]),".png"), p3, width = 7.5, height = 4, dpi = 600)
  
}

lookupS <- lookupLC %>%
  rename(Start = StateClassId,
         LandClassStart = LandClass)

lookupE <- lookupLC %>%
  rename(End = StateClassId,
         LandClassEnd = LandClass)


tabTransition <- datasheet(myScenario2, "stsim_OutputStratumTransition")

years <- c(2001,2006,2010,2016)

for (i in 2:length(years)){
  
  subTabTransition <- tabTransition %>%
    filter(Timestep == years[i]) %>%
    select(Timestep,TransitionGroupId,Amount) %>%
    mutate(TransitionGroupId = gsub(" [Type]","",gsub("LULCC: ","",TransitionGroupId),fixed = T))
  
  subTabTransition$Start <- unlist(lapply(strsplit(subTabTransition$TransitionGroupId, " -> "), "[[", 1))
  subTabTransition$End <- unlist(lapply(strsplit(subTabTransition$TransitionGroupId, " -> "), "[[", 2))
  
  subTabTransition <- subTabTransition %>%
    left_join(lookupS, by = join_by(Start)) %>%
    left_join(lookupE, by = join_by(End)) %>%
    group_by(Timestep,LandClassStart,LandClassEnd) %>%
    summarize(Area_ha = sum(Amount)) %>%
    ungroup()
  
  landClasses <- c("Estuarine Emergent Wetland",
                   "Palustrine Emergent Wetland",
                   "Palustrine Forested Wetland",
                   "Water & Shore",
                   "Crop & Urban",
                   "Forest, Grass, & Shrub")
  
  subTabTransitionBlank <- data.frame(LandClassStart = landClasses,
                                      LandClassEnd = landClasses,
                                      Area_ha = NA,
                                      TimeStep = years[i])
  
  subTabTransition <- subTabTransition %>%
    filter(!(LandClassStart == LandClassEnd)) %>%
    bind_rows(subTabTransitionBlank) %>%
    mutate(Area_haR = round(Area_ha,0))

  pT <- ggplot(data = subTabTransition, aes(x=LandClassEnd, y=LandClassStart, fill=Area_ha)) + 
    geom_tile() +
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position="right",
          legend.title=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_gradient2(low = "#d6ebe4", high = "#297c60", 
                         limit = c(0,4300),#min(subTabTransition$Area_ha) #max(subTabTransition$Area_ha)
                         space = "Lab", 
                         name="Area (Ha)",
                         na.value="gray70") +
    xlab(paste0("\n",years[i])) + 
    ylab(paste0(years[i-1],"\n")) +
    geom_text(aes(x=LandClassEnd, y=LandClassStart, label = Area_haR), color = "black", size = 4)
  
  ggsave(paste0(pathOutLandCover,"/LandCoverT_",years[i],".png"), pT, width = 6, height = 4, dpi = 600)
    
}

