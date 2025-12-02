# ApexRMS
# Updated 2025-05-20
# This script creates figures for the manuscript

library(rsyncrosim)
library(tidyverse)
library(viridis)

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

if(!dir.exists(pathOut)){
  dir.create(pathOut)
}

pathOutSingleCell <- paste0(pathOut,"SingleCell")

if(!dir.exists(pathOutSingleCell)){
  dir.create(pathOutSingleCell)
}

scenarioList <- scenario(myProject, summary = T, results = T)

scenarioLetters <- c("a. Upland Forest",
                     "b. Palustrine Forested Wetland",
                     "c. Palustrine Emergent Wetland",
                     "d. Estuarine Emergent Wetland")

scenarios <- c("Original Oak Gum Cypress Forest",
               "Palustrine Forested Wetland: Add Uncertainty",
               "Palustrine Emergent Wetland: Add Uncertainty",
               "Estuarine Emergent Wetland: Add Uncertainty")

plotName <- "Soil"

for (i in 1:length(scenarios)){
  
  sId <- scenarioList$ScenarioId[grep(scenarios[i],scenarioList$Name)]
  
  myScenario1 <- scenario(myProject, scenario=max(sId))
  
  myDataStock1 <- datasheet(myScenario1, "stsim_OutputStock")
  
  domSoil <- myDataStock1 %>%
    filter(StockGroupId == "DOM: Soil") %>%
    filter(Timestep < 2101) %>%
    group_by(Timestep,StockGroupId) %>%
    summarize(meanTotal = mean(Amount, na.rm = T),
              lowTotal = quantile(Amount,0.025, na.rm = T),
              highTotal = quantile(Amount,0.975, na.rm = T)) %>%
    ungroup() %>%
    select(-StockGroupId)
  
  print(i)
  print(max(domSoil$meanTotal,na.rm = T))
  print(max(domSoil$highTotal,na.rm = T))
  
  stocksKeep <- c("Aboveground Very Fast",
                  "Aboveground Fast",
                  "Aboveground Medium",
                  "Aboveground Slow",
                  "Belowground Very Fast",
                  "Belowground Fast",
                  "Belowground Slow",
                  "Deep Soil")
  
  myDataStock2 <- myDataStock1 %>%
    filter(Timestep < 2101) %>%
    mutate(StockGroupId = gsub(" [Type]","",gsub("DOM: ","",StockGroupId),fixed = T)) %>%
    filter(StockGroupId %in% stocksKeep) %>%
    group_by(Timestep,StockGroupId) %>%
    summarize(mean = mean(Amount, na.rm = T),
              low = quantile(Amount,0.025, na.rm = T),
              high = quantile(Amount,0.975, na.rm = T)) %>%
    ungroup()
  
  myDataStock2$Pool <- factor(myDataStock2$StockGroupId, levels = stocksKeep)
  
  p1 <- ggplot(myDataStock2, aes(x=Timestep, y=mean, fill=Pool)) + 
        geom_area() + 
        scale_fill_viridis(discrete = T) +
        theme_bw() +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position="right",
              legend.title=element_blank()) +
        xlab("\nYear") +
        ylab(as.expression(bquote(atop("DOM: Soil","(Mg C"~ha^-1*")")))) +
        ggtitle(scenarioLetters[i]) +
        ylim(0,850)
  
  p1
  
  ggsave(paste0(pathOutSingleCell,"/",plotName,"_",substr(scenarioLetters[i],1,1),".png"), p1, width = 5, height = 3, dpi = 600)
  
  timestepKeep <- c(2001,2050,2100)
  
  myDataStock3 <- myDataStock2 %>%
    left_join(domSoil, by = join_by(Timestep)) %>%
    mutate(lowTotalSub = case_when(!(Timestep %in% timestepKeep) ~ NA,
                                   (Timestep %in% timestepKeep) ~ lowTotal),
           highTotalSub = case_when(!(Timestep %in% timestepKeep)~NA,
                                   (Timestep %in% timestepKeep) ~ highTotal))
  
  if (scenarios[i] == "Original Oak Gum Cypress Forest"){
    
    p2 <- ggplot(myDataStock3, aes(x=Timestep, y=mean, fill=Pool)) + 
      geom_area() + 
      #geom_errorbar(aes(x=Timestep, ymin=lowTotalSub, ymax=highTotalSub), width = 1.3)+
      scale_fill_viridis(discrete = T) +
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position="right",
            legend.title=element_blank()) +
      xlab("\nYear") + 
      ylab(as.expression(bquote(atop("DOM: Soil","(Mg C"~ha^-1*")")))) +
      ggtitle(scenarioLetters[i]) +
      ylim(0,1800)
    
  } else {
    
    p2 <- ggplot(myDataStock3, aes(x=Timestep, y=mean, fill=Pool)) + 
      geom_area() + 
      geom_errorbar(aes(x=Timestep, ymin=lowTotalSub, ymax=highTotalSub), width = 1.3)+
      scale_fill_viridis(discrete = T) +
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position="right",
            legend.title=element_blank()) +
      xlab("\nYear") + 
      ylab(as.expression(bquote(atop("DOM: Soil","(Mg C"~ha^-1*")")))) +
      ggtitle(scenarioLetters[i]) +
      ylim(0,1800)
    
  }
  
  p2
  
  ggsave(paste0(pathOutSingleCell,"/",plotName,"_Error_",substr(scenarioLetters[i],1,1),".png"), p2, width = 5, height = 3, dpi = 600)
  

  rm(p1,myDataStock3,myDataStock2,domSoil,myDataStock1,myScenario1,sId,p2)
  
}


# Loop through all flux tables and save data needed for plots

scenariosForest <- c("Original Oak Gum Cypress Forest",
                     "Palustrine Forested Wetland: Add Uncertainty")

for (i in 1:length(scenarios)){

  sId <- scenarioList$ScenarioId[grep(scenarios[i],scenarioList$Name)]

  myScenario1 <- scenario(myProject, scenario=max(sId))

  if (scenarios[i] %in% scenariosForest){

    yr <- 2124

  } else{

    yr <- 2124
  }

  myDataFlux1 <- datasheet(myScenario1, "stsim_OutputFlow",
                           filterColumn = "Timestep",
                           filterValue = yr)
  write.csv(myDataFlux1,
            paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[i]),fixed = T),".csv"),
            row.names = F)

  rm(sId,myScenario1,myDataFlux1,yr)
  gc()

}

# Summarize Flows

plotFlows <- c("Annual Lateral Flux (tons CO2-eq per year)",
              "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)",
              "Annual Net Growth (tons CO2-eq per year)",
              "Annual Emissions: CH4 (tons CO2-eq per year)")

for (i in 1:length(plotFlows)){
  
  plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotFlows[i]), fixed = T),fixed = T))
  
  plotFlowsName <- gsub("Annual Net Ecosystem Carbon Balance","Net Ecosystem Carbon Balance",
                    gsub(" (tons CO2-eq per year)","",plotFlows[i], fixed = T))
  plotFlowsName <- gsub(": CH4"," ",plotFlowsName, fixed = T)
  
  myDataFlux1 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[1]),fixed = T),".csv"))
  
  if (i %in% c(1,4)){
    
    myDataFlux1necb <- data.frame(mean = 0,
                                  low = NA,
                                  high = NA,
                                  Scenario = "Upland \nForest",
                                  Color = "#2E9E40")
    
  } else {
    
    myDataFlux1necb <- myDataFlux1 %>%
      filter(FlowGroupId == plotFlows[i]) %>%
      group_by(Iteration) %>%
      summarize(totalC = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      summarize(mean = mean(totalC, na.rm = T),
                low = NA,
                high = NA) %>%
      ungroup() %>%
      mutate(Scenario = "Upland \nForest",
             Color = "#2E9E40")
    
  }
  
  myDataFlux2 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[2]),fixed = T),".csv"))
  
  myDataFlux2necb <- myDataFlux2 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Palustrine \nForested \nWetland",
           Color = "#145A5A")
  
  myDataFlux3 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[3]),fixed = T),".csv"))
  
  myDataFlux3necb <- myDataFlux3 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Palustrine \nEmergent \nWetland",
           Color = "#EA2DEE")
  
  myDataFlux4 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[4]),fixed = T),".csv"))
  
  myDataFlux4necb <- myDataFlux4 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Estuarine \nEmergent \nWetland",
           Color = "#A91EAC")
  
  myDataNECB <- myDataFlux1necb %>%
    bind_rows(myDataFlux2necb) %>%
    bind_rows(myDataFlux3necb) %>%
    bind_rows(myDataFlux4necb)
  
  minVal <- min(myDataNECB$low, na.rm = T)
  maxVal <- max(myDataNECB$high, na.rm = T)
  
  if(minVal > 0 & maxVal >0){
    minVal = 0
  } else if (minVal < 0 & maxVal < 0){
    maxVal = 0
  }
  
  myDataNECB$ScenarioO <- factor(myDataNECB$Scenario, levels = c("Upland \nForest",
                                                                 "Palustrine \nForested \nWetland",
                                                                 "Palustrine \nEmergent \nWetland",
                                                                 "Estuarine \nEmergent \nWetland"))
  
  col <- as.character(myDataNECB$Color)
  names(col) <- as.character(myDataNECB$ScenarioO)
  
  p5 <- ggplot(myDataNECB, aes(x = ScenarioO, y = mean, colour = ScenarioO, fill = ScenarioO)) +
    geom_errorbar(aes(x=ScenarioO, ymin=low, ymax=high), colour = "black", width = 0.25)+
    geom_point(shape = 23, size = 3) +
    theme_bw() +
    scale_colour_manual(values=col,
                        aesthetics = c("colour", "fill")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none",
          legend.title=element_blank()) +
    xlab("\nLand Cover Class") + 
    ylab(as.expression(bquote(atop(.(plotFlowsName),"(Mg "~CO[2-eq]~ha^-1~y^-1*")")))) +
    ylim(minVal,maxVal)
  
  p5
  
  ggsave(paste0(pathOutSingleCell,"/",plotName,".png"), p5, width = 3.5, height = 3.5, dpi = 600)
  
  if (i == 4){
    
    p5 <- ggplot(myDataNECB, aes(x = ScenarioO, y = mean, colour = ScenarioO, fill = ScenarioO)) +
      geom_errorbar(aes(x=ScenarioO, ymin=low, ymax=high), colour = "black", width = 0.25)+
      geom_point(shape = 23, size = 3) +
      theme_bw() +
      scale_colour_manual(values=col,
                          aesthetics = c("colour", "fill")) +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position="none",
            legend.title=element_blank()) +
      xlab("\nLand Cover Class") + 
      ylab(as.expression(bquote(atop(.(plotFlowsName)~CH[4],"(Mg "~CO[2-eq]~ha^-1~y^-1*")")))) +
      ylim(minVal,maxVal)
    
    p5
    
    ggsave(paste0(pathOutSingleCell,"/",plotName,".png"), p5, width = 3.5, height = 3.5, dpi = 600)
    
    myDataNECB$FlowGroupId <- "Annual Emissions: CH4 (tons CO2-eq per year)"
    
    myDataNECB$GHG <- factor(myDataNECB$FlowGroupId, levels = c("Annual Emissions: CH4 (tons CO2-eq per year)"))
    
    p6 <- ggplot(myDataNECB, aes(x = ScenarioO, y = mean, fill = GHG)) +
      geom_bar(position="stack", stat="identity", alpha = 0.75) +
      geom_errorbar(aes(x=ScenarioO, ymin=low, ymax=high), colour = "black", width = 0.25)+
      theme_bw() + 
      scale_fill_manual(values = c("Annual Emissions: CH4 (tons CO2-eq per year)" = "#46337E"),
                        labels = c(expression("CH"[4]))) +
      #scale_fill_viridis(discrete = T, begin = 0.5, end = 0.8) +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position.inside=c(0.2, 0.8),
            legend.position = "inside") +
      xlab("\nLand Cover Class") + 
      ylab(as.expression(bquote(atop(.("Annual Emissions"),"(Mg "~CO[2-eq]~ha^-1~y^-1*")")))) +
      ylim(minVal,maxVal)
    
    p6
    
    ggsave(paste0(pathOutSingleCell,"/",plotName,"Option2.png"), p6, width = 3.5, height = 3.5, dpi = 600)
    
  }
  
  rm(plotName,plotFlowsName,myDataFlux1,myDataFlux1necb,
     myDataFlux2,myDataFlux2necb,
     myDataFlux3,myDataFlux3necb,
     myDataFlux4,myDataFlux4necb,
     minVal,maxVal,col,
     myDataNECB,p5)
  
}

# Summarize flows tons C

plotFlows <- c("Annual Lateral Flux (tons C per year)",
               "Annual Net Ecosystem Carbon Balance (tons C per year)",
               "Annual Net Growth (tons C per year)",
               "Annual Emissions: CH4 (tons C per year)")

for (i in 1:length(plotFlows)){
  
  plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotFlows[i]), fixed = T),fixed = T))
  
  plotFlowsName <- gsub("Annual Net Ecosystem Carbon Balance","Net Ecosystem Carbon Balance",
                        gsub(" (tons C per year)","",plotFlows[i], fixed = T))
  plotFlowsName <- gsub(": CH4"," ",plotFlowsName, fixed = T)
  
  myDataFlux1 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[1]),fixed = T),".csv"))
  
  if (plotFlows[i] %in% c("Annual Lateral Flux (tons C per year)",
                          "Annual Emissions: CH4 (tons C per year)")){
    
    myDataFlux1necb <- data.frame(mean = 0,
                                  low = NA,
                                  high = NA,
                                  Scenario = "Upland \nForest",
                                  Color = "#2E9E40")
    
  } else {
    
    myDataFlux1necb <- myDataFlux1 %>%
      filter(FlowGroupId == plotFlows[i]) %>%
      group_by(Iteration) %>%
      summarize(totalC = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      summarize(mean = mean(totalC, na.rm = T),
                low = NA,
                high = NA) %>%
      ungroup() %>%
      mutate(Scenario = "Upland \nForest",
             Color = "#2E9E40")
    
  }
  
  myDataFlux2 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[2]),fixed = T),".csv"))
  
  myDataFlux2necb <- myDataFlux2 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Palustrine \nForested \nWetland",
           Color = "#145A5A")
  
  myDataFlux3 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[3]),fixed = T),".csv"))
  
  myDataFlux3necb <- myDataFlux3 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Palustrine \nEmergent \nWetland",
           Color = "#EA2DEE")
  
  myDataFlux4 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[4]),fixed = T),".csv"))
  
  myDataFlux4necb <- myDataFlux4 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Estuarine \nEmergent \nWetland",
           Color = "#A91EAC")
  
  myDataNECB <- myDataFlux1necb %>%
    bind_rows(myDataFlux2necb) %>%
    bind_rows(myDataFlux3necb) %>%
    bind_rows(myDataFlux4necb)
  
  minVal <- min(myDataNECB$low, na.rm = T)
  maxVal <- max(myDataNECB$high, na.rm = T)
  
  if(minVal > 0 & maxVal >0){
    minVal = 0
  } else if (minVal < 0 & maxVal < 0){
    maxVal = 0
  }
  
  myDataNECB$ScenarioO <- factor(myDataNECB$Scenario, levels = c("Upland \nForest",
                                                                 "Palustrine \nForested \nWetland",
                                                                 "Palustrine \nEmergent \nWetland",
                                                                 "Estuarine \nEmergent \nWetland"))
  
  col <- as.character(myDataNECB$Color)
  names(col) <- as.character(myDataNECB$ScenarioO)
  
    
  if (plotFlows[i] == "Annual Emissions: CH4 (tons C per year)"){
    
    p5 <- ggplot(myDataNECB, aes(x = ScenarioO, y = mean, colour = ScenarioO, fill = ScenarioO)) +
      geom_errorbar(aes(x=ScenarioO, ymin=low, ymax=high), colour = "black", width = 0.25)+
      geom_point(shape = 23, size = 3) +
      theme_bw() +
      scale_colour_manual(values=col,
                          aesthetics = c("colour", "fill")) +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position="none",
            legend.title=element_blank()) +
      xlab("\nLand Cover Class") + 
      ylab(as.expression(bquote(atop(.(plotFlowsName)~CH[4],"(Mg "~C~ha^-1~y^-1*")")))) +
      ylim(minVal,maxVal)
    
    p5
    
    ggsave(paste0(pathOutSingleCell,"/",plotName,".png"), p5, width = 3.5, height = 3.5, dpi = 600)
    
  } else {
    
    p5 <- ggplot(myDataNECB, aes(x = ScenarioO, y = mean, colour = ScenarioO, fill = ScenarioO)) +
      geom_errorbar(aes(x=ScenarioO, ymin=low, ymax=high), colour = "black", width = 0.25)+
      geom_point(shape = 23, size = 3) +
      theme_bw() +
      scale_colour_manual(values=col,
                          aesthetics = c("colour", "fill")) +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position="none",
            legend.title=element_blank()) +
      xlab("\nLand Cover Class") + 
      ylab(as.expression(bquote(atop(.(plotFlowsName),"(Mg "~C~ha^-1~y^-1*")")))) +
      ylim(minVal,maxVal)
    
    p5
    
    ggsave(paste0(pathOutSingleCell,"/",plotName,".png"), p5, width = 3.5, height = 3.5, dpi = 600)
    
  }
  
  rm(plotName,plotFlowsName,myDataFlux1,myDataFlux1necb,
     myDataFlux2,myDataFlux2necb,
     myDataFlux3,myDataFlux3necb,
     myDataFlux4,myDataFlux4necb,
     minVal,maxVal,col,
     myDataNECB,p5)
  
}


# Summarize Emissions (Not used in paper)

plotE <- c("Annual Emissions: CO2 and CH4 (tons CO2-eq per year)")
plotEs <- c("Annual Emissions: CO2 (tons CO2-eq per year)",
            "Annual Emissions: CH4 (tons CO2-eq per year)")

plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotE), fixed = T),fixed = T))
  
plotEName <- gsub(": CO2 and CH4 (tons CO2-eq per year)","",plotE, fixed = T)
  
myDataFlux1 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[1]),fixed = T),".csv"))
  
myDataFlux1all <- myDataFlux1 %>%
      filter(FlowGroupId == plotE) %>%
      group_by(Iteration) %>%
      summarize(totalC = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      summarize(mean = mean(totalC, na.rm = T),
                low = NA,
                high = NA) %>%
      ungroup() %>%
      mutate(Scenario = "Upland \nForest",
             Color = "#2E9E40")
  
myDataFlux2 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[2]),fixed = T),".csv"))
  
myDataFlux2all <- myDataFlux2 %>%
    filter(FlowGroupId == plotE) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Palustrine \nForested \nWetland",
           Color = "#145A5A")
  
myDataFlux3 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[3]),fixed = T),".csv"))
  
myDataFlux3all <- myDataFlux3 %>%
    filter(FlowGroupId == plotE) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Palustrine \nEmergent \nWetland",
           Color = "#EA2DEE")
  
myDataFlux4 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[4]),fixed = T),".csv"))
  
myDataFlux4all <- myDataFlux4 %>%
    filter(FlowGroupId == plotE) %>%
    group_by(Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    summarize(mean = mean(totalC, na.rm = T),
              low = quantile(totalC,0.025, na.rm = T),
              high = quantile(totalC,0.975, na.rm = T)) %>%
    ungroup() %>%
    mutate(Scenario = "Estuarine \nEmergent \nWetland",
           Color = "#A91EAC")
  
myDataAll <- myDataFlux1all %>%
    bind_rows(myDataFlux2all) %>%
    bind_rows(myDataFlux3all) %>%
    bind_rows(myDataFlux4all)

myDataAll <- myDataAll %>%
  rename(lowT = low,
         highT = high) %>%
  select(Scenario,lowT,highT)
  
minVal <- min(myDataAll$lowT, na.rm = T)
maxVal <- max(myDataAll$highT, na.rm = T)
  
if(minVal > 0 & maxVal >0){
    minVal = 0
} else if (minVal < 0 & maxVal < 0){
    maxVal = 0
}

myDataFlux1s <- myDataFlux1 %>%
  filter(FlowGroupId %in% plotEs) %>%
  group_by(Iteration,FlowGroupId) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(FlowGroupId) %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = NA,
            high = NA) %>%
  ungroup() %>%
  mutate(Scenario = "Upland \nForest",
         Color = "#2E9E40") %>%
  add_row(data.frame(FlowGroupId = "Annual Emissions: CH4 (tons CO2-eq per year)",
                     mean = 0,
                     low = NA,
                     high = NA,
                     Scenario = "Upland \nForest",
                     Color = "#2E9E40"))

myDataFlux2s <- myDataFlux2 %>%
  filter(FlowGroupId %in% plotEs) %>%
  group_by(Iteration,FlowGroupId) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(FlowGroupId) %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Palustrine \nForested \nWetland",
         Color = "#145A5A")

myDataFlux3s <- myDataFlux3 %>%
  filter(FlowGroupId %in% plotEs) %>%
  group_by(Iteration,FlowGroupId) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(FlowGroupId) %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Palustrine \nEmergent \nWetland",
         Color = "#EA2DEE")

myDataFlux4s <- myDataFlux4 %>%
  filter(FlowGroupId %in% plotEs) %>%
  group_by(Iteration,FlowGroupId) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(FlowGroupId) %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Estuarine \nEmergent \nWetland",
         Color = "#A91EAC")

myDataS <- myDataFlux1s %>%
  bind_rows(myDataFlux2s) %>%
  bind_rows(myDataFlux3s) %>%
  bind_rows(myDataFlux4s) %>%
  left_join(myDataAll,by = join_by(Scenario))

  
# col <- as.character(myDataS$Color)
# names(col) <- as.character(myDataS$Scenario)
  
myDataS$ScenarioO <- factor(myDataS$Scenario, levels = c("Upland \nForest",
                                                                 "Palustrine \nForested \nWetland",
                                                                 "Palustrine \nEmergent \nWetland",
                                                                 "Estuarine \nEmergent \nWetland"))

myDataS$GHG <- factor(myDataS$FlowGroupId, levels = c("Annual Emissions: CH4 (tons CO2-eq per year)",
                                                      "Annual Emissions: CO2 (tons CO2-eq per year)"))


p6 <- ggplot(myDataS, aes(x = ScenarioO, y = mean, fill = GHG)) +
  geom_bar(position="stack", stat="identity", alpha = 0.75) +
  geom_errorbar(aes(x=ScenarioO, ymin=lowT, ymax=highT), colour = "black", width = 0.25)+
  theme_bw() + 
  scale_fill_manual(values = c("Annual Emissions: CH4 (tons CO2-eq per year)" = "#46337E",
                               "Annual Emissions: CO2 (tons CO2-eq per year)" = "#9FDA3A"),
                    labels = c(expression("CH"[4]),expression("CO"[2]))) +
  #scale_fill_viridis(discrete = T, begin = 0.5, end = 0.8) +
  theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position.inside=c(0.2, 0.8),
          legend.position = "inside") +
  xlab("\nLand Cover Class") + 
  ylab(as.expression(bquote(atop(.(plotEName),"(Mg "~CO[2-eq]~ha^-1~y^-1*")")))) +
  ylim(minVal,maxVal)
  
  p6
  
ggsave(paste0(pathOutSingleCell,"/",plotEName,".png"), p6, width = 3.5, height = 3.5, dpi = 600)

# Summarize methane emissions

plotE <- c("Annual Emissions: CH4 (tons CO2-eq per year)")
plotEs <- c("Annual Emissions: CH4 (tons CO2-eq per year)")

plotName <- gsub(" ","",gsub(")","",gsub("(","",gsub(": "," ",plotE), fixed = T),fixed = T))

plotEName <- gsub(": CO2 and CH4 (tons CO2-eq per year)","",plotE, fixed = T)

myDataFlux1 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[1]),fixed = T),".csv"))

myDataFlux1all <- myDataFlux1 %>%
  filter(FlowGroupId == plotE) %>%
  group_by(Iteration) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = NA,
            high = NA) %>%
  ungroup() %>%
  mutate(Scenario = "Upland \nForest",
         Color = "#2E9E40")

myDataFlux2 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[2]),fixed = T),".csv"))

myDataFlux2all <- myDataFlux2 %>%
  filter(FlowGroupId == plotE) %>%
  group_by(Iteration) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Palustrine \nForested \nWetland",
         Color = "#145A5A")

myDataFlux3 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[3]),fixed = T),".csv"))

myDataFlux3all <- myDataFlux3 %>%
  filter(FlowGroupId == plotE) %>%
  group_by(Iteration) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Palustrine \nEmergent \nWetland",
         Color = "#EA2DEE")

myDataFlux4 <- read.csv(paste0(pathOutSingleCell,"/",gsub(":","",gsub(" ","",scenarios[4]),fixed = T),".csv"))

myDataFlux4all <- myDataFlux4 %>%
  filter(FlowGroupId == plotE) %>%
  group_by(Iteration) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Estuarine \nEmergent \nWetland",
         Color = "#A91EAC")

myDataAll <- myDataFlux1all %>%
  bind_rows(myDataFlux2all) %>%
  bind_rows(myDataFlux3all) %>%
  bind_rows(myDataFlux4all)

myDataAll <- myDataAll %>%
  rename(lowT = low,
         highT = high) %>%
  select(Scenario,lowT,highT)

minVal <- min(myDataAll$lowT, na.rm = T)
maxVal <- max(myDataAll$highT, na.rm = T)

if(minVal > 0 & maxVal >0){
  minVal = 0
} else if (minVal < 0 & maxVal < 0){
  maxVal = 0
}

myDataFlux1s <- myDataFlux1 %>%
  filter(FlowGroupId %in% plotEs) %>%
  group_by(Iteration,FlowGroupId) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(FlowGroupId) %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = NA,
            high = NA) %>%
  ungroup() %>%
  mutate(Scenario = "Upland \nForest",
         Color = "#2E9E40") %>%
  add_row(data.frame(FlowGroupId = "Annual Emissions: CH4 (tons CO2-eq per year)",
                     mean = 0,
                     low = NA,
                     high = NA,
                     Scenario = "Upland \nForest",
                     Color = "#2E9E40"))

myDataFlux2s <- myDataFlux2 %>%
  filter(FlowGroupId %in% plotEs) %>%
  group_by(Iteration,FlowGroupId) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(FlowGroupId) %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Palustrine \nForested \nWetland",
         Color = "#145A5A")

myDataFlux3s <- myDataFlux3 %>%
  filter(FlowGroupId %in% plotEs) %>%
  group_by(Iteration,FlowGroupId) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(FlowGroupId) %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Palustrine \nEmergent \nWetland",
         Color = "#EA2DEE")

myDataFlux4s <- myDataFlux4 %>%
  filter(FlowGroupId %in% plotEs) %>%
  group_by(Iteration,FlowGroupId) %>%
  summarize(totalC = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(FlowGroupId) %>%
  summarize(mean = mean(totalC, na.rm = T),
            low = quantile(totalC,0.025, na.rm = T),
            high = quantile(totalC,0.975, na.rm = T)) %>%
  ungroup() %>%
  mutate(Scenario = "Estuarine \nEmergent \nWetland",
         Color = "#A91EAC")

myDataS <- myDataFlux1s %>%
  bind_rows(myDataFlux2s) %>%
  bind_rows(myDataFlux3s) %>%
  bind_rows(myDataFlux4s) %>%
  left_join(myDataAll,by = join_by(Scenario))


# col <- as.character(myDataS$Color)
# names(col) <- as.character(myDataS$Scenario)

myDataS$ScenarioO <- factor(myDataS$Scenario, levels = c("Upland \nForest",
                                                         "Palustrine \nForested \nWetland",
                                                         "Palustrine \nEmergent \nWetland",
                                                         "Estuarine \nEmergent \nWetland"))

myDataS$GHG <- factor(myDataS$FlowGroupId, levels = c("Annual Emissions: CH4 (tons CO2-eq per year)",
                                                      "Annual Emissions: CO2 (tons CO2-eq per year)"))


p6 <- ggplot(myDataS, aes(x = ScenarioO, y = mean, fill = GHG)) +
  geom_bar(position="stack", stat="identity", alpha = 0.75) +
  geom_errorbar(aes(x=ScenarioO, ymin=lowT, ymax=highT), colour = "black", width = 0.25)+
  theme_bw() + 
  scale_fill_manual(values = c("Annual Emissions: CH4 (tons CO2-eq per year)" = "#46337E",
                               "Annual Emissions: CO2 (tons CO2-eq per year)" = "#9FDA3A"),
                    labels = c(expression("CH"[4]),expression("CO"[2]))) +
  #scale_fill_viridis(discrete = T, begin = 0.5, end = 0.8) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position.inside=c(0.2, 0.8),
        legend.position = "inside") +
  xlab("\nLand Cover Class") + 
  ylab(as.expression(bquote(atop(.(plotEName),"(Mg "~CO[2-eq]~ha^-1~y^-1*")")))) +
  ylim(minVal,maxVal)

p6

ggsave(paste0(pathOutSingleCell,"/",plotEName,".png"), p6, width = 3.5, height = 3.5, dpi = 600)

