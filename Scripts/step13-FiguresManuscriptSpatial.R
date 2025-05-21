# Created by Amanda Schwantes, ApexRMS
# Updated 2025-03-25
# Run after step9-SingleCellTransitions.R
# This script creates figures for spatial scenario results

library(rsyncrosim)
library(tidyverse)
library(ggplot2)

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

scenarioList <- scenario(myProject, summary = T, results = T)

id1 <- scenarioList$ScenarioId[grep("1 No Land Cover Change and Climate",scenarioList$Name)]
id2 <- scenarioList$ScenarioId[grep("2 Land Cover Change and Climate",scenarioList$Name)]
id3 <- scenarioList$ScenarioId[grep("3 Land Cover Change, Climate, Erosion",scenarioList$Name)]
id4 <- scenarioList$ScenarioId[grep("4 Land Cover Change, Climate, and No Forested Wetland",scenarioList$Name)]

myScenario1 <- scenario(myProject, scenario=max(id1))
myScenario2 <- scenario(myProject, scenario=max(id2))
myScenario3 <- scenario(myProject, scenario=max(id3))
myScenario4 <- scenario(myProject, scenario=max(id4))

# Summarize Flows

myDataFlux1 <- datasheet(myScenario1, "stsim_OutputFlow")
myDataFlux2 <- datasheet(myScenario2, "stsim_OutputFlow")
myDataFlux3 <- datasheet(myScenario3, "stsim_OutputFlow")
myDataFlux4 <- datasheet(myScenario4, "stsim_OutputFlow")

#unique(myDataFlux1$FlowGroupId)

plotFlows<- c("Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)")

for (i in 1:length(plotFlows)){
  
  plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotFlows[i]), fixed = T),fixed = T))
  
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
           Color = "Black",
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
           Color = "gray40",
           Type = "dotted")
  
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
    scale_x_continuous(limits = c(2001, 2016),
                       breaks = c(2001,2006,2010,2016)) +
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
    ylab(as.expression(bquote(atop("Net Ecosystem Carbon Balance","million metric tons "~CO[2-eq]~y^-1))))+
    ylim(minVal,maxVal)
  
  p6
  
  ggsave(paste0(pathOutManuscript,"/",plotName,"_","LandCover",".png"), p6, width = 3.5, height = 3.5, dpi = 600)
  
  rm(minVal,maxVal,col,lineType)
  
}

plotFlows<- c("Annual Net Ecosystem Carbon Balance (tons C per year)",
              "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)")

plotFlowsLetters <- c("a. ","b. ")

for (i in 1:length(plotFlows)){
  
  plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotFlows[i]), fixed = T),fixed = T))
  
  myDataFlux2c <- myDataFlux2 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep,Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC),
              min = min(totalC),
              max = max(totalC)) %>%
    mutate(Scenario = "With Palustrine Forested Wetlands",
           Color = "Black",
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
    mutate(Scenario = "Without Palustrine Forested Wetlands",
           Color = "gray40",
           Type = "dotted")
  
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
  
  if (i == 1){
    
    p7 <- ggplot(myDataNECBB, aes(x = Timestep, y = mean, color = Scenario, group = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 0.8) +
      theme_bw() +
      scale_color_manual(values=col, breaks = c("With Palustrine Forested Wetlands",
                                                "Without Palustrine Forested Wetlands")) +
      scale_linetype_manual(values=lineType, breaks = c("With Palustrine Forested Wetlands",
                                                        "Without Palustrine Forested Wetlands")) +
      scale_x_continuous(limits = c(2001, 2016),
                         breaks = c(2001,2006,2010,2016)) +
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
      ylab(as.expression(bquote(atop("Net Ecosystem Carbon Balance","million metric tons C"~y^-1))))+
      ylim(-5,4) +
      ggtitle("a.")
    
  } else if (i == 2){
    
    p7 <- ggplot(myDataNECBB, aes(x = Timestep, y = mean, color = Scenario, group = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 0.8) +
      theme_bw() +
      scale_color_manual(values=col, breaks = c("With Palustrine Forested Wetlands",
                                                "Without Palustrine Forested Wetlands")) +
      scale_linetype_manual(values=lineType, breaks = c("With Palustrine Forested Wetlands",
                                                        "Without Palustrine Forested Wetlands")) +
      scale_x_continuous(limits = c(2001, 2016),
                         breaks = c(2001,2006,2010,2016)) +
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
      ylab(as.expression(bquote(atop("Net Ecosystem Carbon Balance","million metric tons "~CO[2-eq]~y^-1))))+
      ylim(-5,4)+
      ggtitle("b.")
  }
  
  
  
  p7
  
  ggsave(paste0(pathOutManuscript,"/",plotName,"_","NoForestWetland",".png"), p7, width = 3.5, height = 3.5, dpi = 600)
  
  
  rm(myDataFlux2c,myDataFlux4c,
     myDataNECBB,plotName)
  
}


