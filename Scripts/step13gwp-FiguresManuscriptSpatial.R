# ApexRMS
# Updated 2025-03-25
# Run after step13-FiguresManuscriptSingleCell.R
# This script creates figures for spatial scenario results

library(rsyncrosim)
library(tidyverse)
library(ggplot2)

options(scipen = 999)
old <- options(pillar.sigfig = 10)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")
signIn(mySession)

dataPath <- "Data/"
modelPath <- "Models/"

#modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

myLibrary_GWP100 <- ssimLibrary(file.path(
  path.expand("~"),
  "A379",
  "Barataria_LocalCH4/Models",
  "Barataria LocalCH4 GWP-100.ssim"
))

myLibrary_GWP20 <- ssimLibrary(file.path(
  path.expand("~"),
  "A379",
  "Barataria_LocalCH4/Models",
  "Barataria LocalCH4 GWP-20.ssim"
))


myProject_GWP100 <- rsyncrosim::project(
  myLibrary_GWP100,
  project = "Definitions"
)
myProject_GWP20 <- rsyncrosim::project(myLibrary_GWP20, project = "Definitions")

pathOut <- paste0(rootPath, "Models/", "OutputFigures/")

pathOutSpatial <- paste0(pathOut, "Spatial/")

if (!dir.exists(pathOutSpatial)) {
  dir.create(pathOutSpatial, recursive = TRUE)
}

pathOutManuscript <- paste0(pathOutSpatial, "Manuscript")

if (!dir.exists(pathOutManuscript)) {
  dir.create(pathOutManuscript)
}

scenarioList_100 <- scenario(myProject_GWP100, summary = TRUE, results = TRUE)
scenarioList_20 <- scenario(myProject_GWP20, summary = TRUE, results = TRUE)

idBsl20 <- scenarioList_20$ScenarioId[grep(
  "Basin Baseline",
  scenarioList_20$Name
)]
idBsl100 <- scenarioList_100$ScenarioId[grep(
  "Basin Baseline",
  scenarioList_100$Name
)]
idIpcc100 <- scenarioList_100$ScenarioId[grep(
  "Basin IPCC",
  scenarioList_100$Name
)]

baseline20 <- scenario(myProject_GWP20, scenario = max(idBsl20))
baseline100 <- scenario(myProject_GWP100, scenario = max(idBsl100))
ipcc100 <- scenario(myProject_GWP100, scenario = max(idIpcc100))

# Summarize Flows

myDataFluxB20 <- datasheet(baseline20, "stsim_OutputFlow")
myDataFluxB100 <- datasheet(baseline100, "stsim_OutputFlow")
myDataFluxI100 <- datasheet(ipcc100, "stsim_OutputFlow")


plotFlows <- c("Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)")

for (i in 1:length(plotFlows)) {
  plotName <- gsub(
    " ",
    "",
    gsub(
      ")",
      "",
      gsub("(", "", gsub(": ", " ", plotFlows[i]), fixed = T),
      fixed = T
    )
  )

  myDataFlux20b <- myDataFluxB20 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC), min = min(totalC), max = max(totalC)) %>%
    mutate(
      Scenario = "GWP-20", #Schoolmaster
      Color = "gray40",
      Type = "dotted"
    )

  myDataFlux100b <- myDataFluxB100 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC), min = min(totalC), max = max(totalC)) %>%
    mutate(
      Scenario = "GWP-100", #IPCC
      Color = "black",
      Type = "solid"
    )

  myDataNECBA <- myDataFlux20b %>%
    bind_rows(myDataFlux100b)

  myDataNECBA$mean <- myDataNECBA$mean / 1000000
  myDataNECBA$min <- myDataNECBA$min / 1000000
  myDataNECBA$max <- myDataNECBA$max / 1000000

  minVal <- min(myDataNECBA$min)
  maxVal <- max(myDataNECBA$max)

  if (minVal > 0 & maxVal > 0) {
    minVal = 0
  } else if (minVal < 0 & maxVal < 0) {
    maxVal = 0
  }

  col <- as.character(myDataNECBA$Color)
  names(col) <- as.character(myDataNECBA$Scenario)

  lineType <- as.character(myDataNECBA$Type)
  names(lineType) <- as.character(myDataNECBA$Scenario)

  p6 <- ggplot(
    myDataNECBA,
    aes(
      x = Timestep,
      y = mean,
      color = Scenario,
      group = Scenario,
      linetype = Scenario
    )
  ) +
    geom_line(linewidth = 0.8) +
    theme_bw() +
    # scale_color_manual(values=col, breaks = c("Schoolmaster",
    #                                           "IPCC")) +
    # scale_linetype_manual(values=lineType, breaks = c("Schoolmaster",
    #                                                   "IPCC")) +
    scale_color_manual(values = col, breaks = c("GWP-100", "GWP-20")) +
    scale_linetype_manual(values = lineType, breaks = c("GWP-100", "GWP-20")) +
    scale_x_continuous(
      limits = c(2001, 2016),
      breaks = c(2001, 2006, 2010, 2016)
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    guides(
      colour = guide_legend(
        nrow = 2,
        keywidth = 0.4,
        keyheight = 0.1,
        default.unit = "inch"
      )
    ) +
    xlab("\nYear") +
    ylab(as.expression(bquote(atop(
      "Net Radiative Balance",
      "(Tg " ~ CO[2 - eq] ~ yr^-1 * ")"
    )))) +
    ylim(minVal, maxVal)

  p6

  ggsave(
    paste0(pathOutManuscript, "/", plotName, "_", "LandCover", ".png"),
    p6,
    width = 3.5,
    height = 3.5,
    dpi = 600
  )

  rm(minVal, maxVal, col, lineType)
}

plotFlows <- c(
  "Annual Net Ecosystem Carbon Balance (tons C per year)",
  "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)"
)

plotFlowsLetters <- c("a. ", "b. ")

for (i in 1:length(plotFlows)) {
  plotName <- gsub(
    " ",
    "",
    gsub(
      ")",
      "",
      gsub("(", "", gsub(": ", " ", plotFlows[i]), fixed = T),
      fixed = T
    )
  )

  myDataFlux20b <- myDataFluxB20 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC), min = min(totalC), max = max(totalC)) %>%
    mutate(
      Scenario = "GWP-20", #Schoolmaster
      Color = "gray40",
      Type = "dotted"
    )

  myDataFlux100b <- myDataFluxB100 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC), min = min(totalC), max = max(totalC)) %>%
    mutate(
      Scenario = "GWP-100", #IPCC
      Color = "black",
      Type = "solid"
    )

  # myDataFlux100i <- myDataFluxI100 %>%
  #   filter(FlowGroupId == plotFlows[i]) %>%
  #   group_by(Timestep,Iteration) %>%
  #   summarize(totalC = sum(Amount, na.rm = T)) %>%
  #   ungroup() %>%
  #   group_by(Timestep) %>%
  #   summarize(mean = mean(totalC),
  #             min = min(totalC),
  #             max = max(totalC)) %>%
  #   mutate(Scenario = "IPCC", #IPCC
  #          Color = "gray40",
  #          Type = "dotted")

  myDataNECBB <- myDataFlux100b %>%
    #bind_rows(myDataFlux100i)
    bind_rows(myDataFlux20b)

  myDataNECBB$mean <- myDataNECBB$mean / 1000000
  myDataNECBB$min <- myDataNECBB$min / 1000000
  myDataNECBB$max <- myDataNECBB$max / 1000000

  minVal <- min(myDataNECBB$min)
  maxVal <- max(myDataNECBB$max)

  if (minVal > 0 & maxVal > 0) {
    minVal = 0
  } else if (minVal < 0 & maxVal < 0) {
    maxVal = 0
  }

  col <- as.character(myDataNECBB$Color)
  names(col) <- as.character(myDataNECBB$Scenario)

  lineType <- as.character(myDataNECBB$Type)
  names(lineType) <- as.character(myDataNECBB$Scenario)

  if (plotFlows[i] == "Annual Net Ecosystem Carbon Balance (tons C per year)") {
    p7 <- ggplot(
      myDataNECBB,
      aes(
        x = Timestep,
        y = mean,
        color = Scenario,
        group = Scenario,
        linetype = Scenario
      )
    ) +
      geom_line(linewidth = 0.8) +
      theme_bw() +
      scale_color_manual(values = col, breaks = c("GWP-100", "GWP-20")) +
      scale_linetype_manual(
        values = lineType,
        breaks = c("GWP-100", "GWP-20")
      ) +
      scale_x_continuous(
        limits = c(2001, 2016),
        breaks = c(2001, 2006, 2010, 2016)
      ) +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      guides(
        colour = guide_legend(
          nrow = 2,
          keywidth = 0.4,
          keyheight = 0.1,
          default.unit = "inch"
        )
      ) +
      xlab("\nYear") +
      ylab(as.expression(bquote(atop(
        "Net Ecosystem Carbon Balance",
        "(Tg C" ~ yr^-1 * ")"
      )))) +
      ylim(-5.1, 4) +
      ggtitle("a.")
  } else if (
    plotFlows[i] == "Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)"
  ) {
    p7 <- ggplot(
      myDataNECBB,
      aes(
        x = Timestep,
        y = mean,
        color = Scenario,
        group = Scenario,
        linetype = Scenario
      )
    ) +
      geom_line(linewidth = 0.8) +
      theme_bw() +
      scale_color_manual(values = col, breaks = c("GWP-100", "GWP-20")) +
      scale_linetype_manual(
        values = lineType,
        breaks = c("GWP-100", "GWP-20")
      ) +
      scale_x_continuous(
        limits = c(2001, 2016),
        breaks = c(2001, 2006, 2010, 2016)
      ) +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      guides(
        colour = guide_legend(
          nrow = 2,
          keywidth = 0.4,
          keyheight = 0.1,
          default.unit = "inch"
        )
      ) +
      xlab("\nYear") +
      ylab(as.expression(bquote(atop(
        "Net Radiative Balance",
        "(Tg " ~ CO[2 - eq] ~ yr^-1 * ")"
      )))) +
      ylim(floor(min(myDataNECBB$mean)), 4) +
      ggtitle("b.")
  }

  p7

  ggsave(
    paste0(pathOutManuscript, "/", plotName, "_", "NoForestWetland", ".png"),
    p7,
    width = 3.5,
    height = 3.5,
    dpi = 600
  )

  rm(myDataFlux2c, myDataFlux4c, myDataNECBB, plotName, p7, col, lineType)
}

plotFlows <- c("Annual Net Ecosystem Carbon Balance (tons C per year)")

for (i in 1:length(plotFlows)) {
  plotName <- gsub(
    " ",
    "",
    gsub(
      ")",
      "",
      gsub("(", "", gsub(": ", " ", plotFlows[i]), fixed = T),
      fixed = T
    )
  )

  myDataFlux2c <- myDataFlux2 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC), min = min(totalC), max = max(totalC)) %>%
    mutate(Scenario = "Schoolmaster", Color = "Black", Type = "solid")

  myDataFlux3c <- myDataFlux3 %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(Timestep) %>%
    summarize(mean = mean(totalC), min = min(totalC), max = max(totalC)) %>%
    mutate(Scenario = "IPCC", Color = "gray40", Type = "dotted")

  myDataNECBA <- myDataFlux2c %>%
    bind_rows(myDataFlux3c)

  myDataNECBA$mean <- myDataNECBA$mean / 1000000
  myDataNECBA$min <- myDataNECBA$min / 1000000
  myDataNECBA$max <- myDataNECBA$max / 1000000

  minVal <- min(myDataNECBA$min)
  maxVal <- max(myDataNECBA$max)

  if (minVal > 0 & maxVal > 0) {
    minVal = 0
  } else if (minVal < 0 & maxVal < 0) {
    maxVal = 0
  }

  col <- as.character(myDataNECBA$Color)
  names(col) <- as.character(myDataNECBA$Scenario)

  lineType <- as.character(myDataNECBA$Type)
  names(lineType) <- as.character(myDataNECBA$Scenario)

  p6 <- ggplot(
    myDataNECBA,
    aes(
      x = Timestep,
      y = mean,
      color = Scenario,
      group = Scenario,
      linetype = Scenario
    )
  ) +
    geom_line(linewidth = 0.8) +
    theme_bw() +
    scale_color_manual(values = col, breaks = c("Schoolmaster", "IPCC")) +
    scale_linetype_manual(
      values = lineType,
      breaks = c("Schoolmaster", "IPCC")
    ) +
    scale_x_continuous(
      limits = c(2001, 2016),
      breaks = c(2001, 2006, 2010, 2016)
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    guides(
      colour = guide_legend(
        nrow = 2,
        keywidth = 0.4,
        keyheight = 0.1,
        default.unit = "inch"
      )
    ) +
    xlab("\nYear") +
    ylab(as.expression(bquote(atop(
      "Net Ecosystem Carbon Balance",
      "(Tg " ~ C ~ yr^-1 * ")"
    )))) +
    ylim(minVal, maxVal)

  p6

  ggsave(
    paste0(pathOutManuscript, "/", plotName, "_", "LandCover", ".png"),
    p6,
    width = 3.5,
    height = 3.5,
    dpi = 600
  )

  rm(minVal, maxVal, col, lineType)

  # Create table of cumulative NECB differences

  myDataNECBBwide <- myDataNECBA %>%
    select(Scenario, Timestep, mean) %>%
    pivot_wider(names_from = Scenario, values_from = mean) %>%
    arrange(Timestep) %>%
    mutate(
      ScenDiff = Schoolmaster - IPCC
    )

  nameTabular <- "TgC_yr"

  write.csv(
    myDataNECBBwide,
    paste0(pathOut, "NECB_Comparison_2026-02-18_", nameTabular, ".csv"),
    row.names = F
  )
}
