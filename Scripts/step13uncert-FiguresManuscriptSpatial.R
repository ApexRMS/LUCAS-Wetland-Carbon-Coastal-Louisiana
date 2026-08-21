# ApexRMS
# Updated 2026-08-20
# Run after step13-FiguresManuscriptSingleCell.R
# This script creates figures for spatial scenario results

library(rsyncrosim)
library(tidyverse)
library(ggplot2)

options(scipen = 999)
old <- options(pillar.sigfig = 10)

# Specify file paths, library, and project

mySession <- session("C:/Program Files/SyncroSim/")

dataPath <- "Data/"
modelPath <- "Models/"

#modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

# myLibrary_Avg <- ssimLibrary(file.path(
#   path.expand("~"),
#   "A379",
#   "Barataria_LocalCH4/Models",
#   "Barataria LocalCH4 GWP-100.ssim"
# ))
#
# myLibrary_Uncert <- ssimLibrary(file.path(
#   path.expand("~"),
#   "A379",
#   "Barataria_LocalCH4/Models",
#   "Uncertainty",
#   "Barataria LocalCH4 GWP-100.ssim"
# ))

myLibrary <- ssimLibrary(
  name = paste0(modelFullPath, "/", modelName, ".ssim"),
  session = mySession
)

myProject <- rsyncrosim::project(myLibrary, project = "Definitions")
# myProject_Avg <- rsyncrosim::project(
#   myLibrary_Avg,
#   project = "Definitions"
# )
# myProject_Uncert <- rsyncrosim::project(
#   myLibrary_Uncert,
#   project = "Definitions"
# )

pathOut <- paste0(rootPath, "Models/", modelName, "/OutputFigures/")
pathOut <- paste0(pathOut, "Uncertainty/")

pathOutSpatial <- paste0(pathOut, "Spatial/")

if (!dir.exists(pathOutSpatial)) {
  dir.create(pathOutSpatial, recursive = TRUE)
}

pathOutManuscript <- paste0(pathOutSpatial, "Manuscript")

if (!dir.exists(pathOutManuscript)) {
  dir.create(pathOutManuscript)
}

#scenarioList_Avg <- scenario(myProject_Avg, summary = TRUE, results = TRUE)
#scenarioList_Unc <- scenario(myProject_Uncert, summary = TRUE, results = TRUE)

scenarioList <- scenario(myProject, summary = TRUE, results = TRUE)

idBslUnc <- scenarioList$ScenarioId[grep(
  "Basin Uncertainty Baseline",
  scenarioList$Name
)]
idBslAvg <- scenarioList$ScenarioId[grep(
  "Basin Baseline",
  scenarioList$Name
)]

uncertaintyScn <- scenario(myProject, scenario = max(idBslUnc))
averageScn <- scenario(myProject, scenario = max(idBslAvg))

# Summarize Flows

myDataFluxUncert <- datasheet(uncertaintyScn, "stsim_OutputFlow")
myDataFluxAvg <- datasheet(averageScn, "stsim_OutputFlow")

# plotFlows <- c("Annual Net Ecosystem Carbon Balance (tons CO2-eq per year)")
#
# for (i in 1:length(plotFlows)) {
#   plotName <- gsub(
#     " ",
#     "",
#     gsub(
#       ")",
#       "",
#       gsub("(", "", gsub(": ", " ", plotFlows[i]), fixed = T),
#       fixed = T
#     )
#   )
#
#   myDataFlux_Uncert <- myDataFluxUncert %>%
#     filter(FlowGroupId == plotFlows[i]) %>%
#     group_by(Timestep, Iteration) %>%
#     summarize(totalC = sum(Amount, na.rm = T), .groups = "drop") %>%
#     group_by(Timestep) %>%
#     summarize(
#       mean = mean(totalC),
#       min = min(totalC),
#       max = max(totalC),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       Scenario = "Uncertainty",
#       Color = "gray40",
#       Type = "dotted"
#     )
#
#   myDataFlux_Avg <- myDataFluxAvg %>%
#     filter(FlowGroupId == plotFlows[i]) %>%
#     group_by(Timestep, Iteration) %>%
#     summarize(totalC = sum(Amount, na.rm = T), .groups = "drop") %>%
#     group_by(Timestep) %>%
#     summarize(
#       mean = mean(totalC),
#       min = min(totalC),
#       max = max(totalC),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       Scenario = "GWP-100",
#       Color = "black",
#       Type = "solid"
#     )
#
#   myDataNECBA <- myDataFlux_Uncert %>%
#     bind_rows(myDataFlux_Avg)
#
#   myDataNECBA$mean <- myDataNECBA$mean / 1000000
#   myDataNECBA$min <- myDataNECBA$min / 1000000
#   myDataNECBA$max <- myDataNECBA$max / 1000000
#
#   minVal <- min(myDataNECBA$min)
#   maxVal <- max(myDataNECBA$max)
#
#   if (minVal > 0 & maxVal > 0) {
#     minVal = 0
#   } else if (minVal < 0 & maxVal < 0) {
#     maxVal = 0
#   }
#
#   col <- as.character(myDataNECBA$Color)
#   names(col) <- as.character(myDataNECBA$Scenario)
#
#   lineType <- as.character(myDataNECBA$Type)
#   names(lineType) <- as.character(myDataNECBA$Scenario)
#
#   p6 <- ggplot(
#     myDataNECBA,
#     aes(
#       x = Timestep,
#       y = mean,
#       color = Scenario,
#       group = Scenario,
#       linetype = Scenario
#     )
#   ) +
#     geom_ribbon(
#       data = myDataNECBA %>% filter(Scenario == "Uncertainty"),
#       mapping = aes(x = Timestep, ymin = min, ymax = max),
#       inherit.aes = FALSE,
#       fill = "gray40",
#       alpha = 0.3
#     ) +
#     geom_line(linewidth = 0.8) +
#     theme_bw() +
#     # scale_color_manual(values=col, breaks = c("Schoolmaster",
#     #                                           "IPCC")) +
#     # scale_linetype_manual(values=lineType, breaks = c("Schoolmaster",
#     #                                                   "IPCC")) +
#     scale_color_manual(values = col, breaks = c("GWP-100", "Uncertainty")) +
#     scale_linetype_manual(
#       values = lineType,
#       breaks = c("GWP-100", "Uncertainty")
#     ) +
#     scale_x_continuous(
#       limits = c(2001, 2016),
#       breaks = c(2001, 2006, 2010, 2016)
#     ) +
#     theme(
#       panel.border = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.line = element_line(colour = "black"),
#       legend.position = "bottom",
#       legend.title = element_blank()
#     ) +
#     guides(
#       colour = guide_legend(
#         nrow = 2,
#         keywidth = 0.4,
#         keyheight = 0.1,
#         default.unit = "inch"
#       )
#     ) +
#     xlab("\nYear") +
#     ylab(as.expression(bquote(atop(
#       "Net Radiative Balance",
#       "(Tg " ~ CO[2 - eq] ~ yr^-1 * ")"
#     )))) +
#     ylim(minVal, maxVal)
#
#   p6
#
#   ggsave(
#     paste0(pathOutManuscript, "/", plotName, "_", "LandCover", ".png"),
#     p6,
#     width = 3.5,
#     height = 3.5,
#     dpi = 600
#   )
#
#   rm(minVal, maxVal, col, lineType)
# }

gwpLegend <- str_extract(modelName, "GWP-\\d+")

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

  myDataFlux_Uncert <- myDataFluxUncert %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T), .groups = "drop") %>%
    group_by(Timestep) %>%
    summarize(
      mean = mean(totalC),
      min = min(totalC),
      max = max(totalC),
      .groups = "drop"
    ) %>%
    mutate(
      Scenario = "Uncertainty",
      Color = "gray40",
      Type = "dotted"
    )

  myDataFlux_Avg <- myDataFluxAvg %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T), .groups = "drop") %>%
    group_by(Timestep) %>%
    summarize(
      mean = mean(totalC),
      min = min(totalC),
      max = max(totalC),
      .groups = "drop"
    ) %>%
    mutate(
      Scenario = gwpLegend,
      Color = "black",
      Type = "solid"
    )

  myDataNECBB <- myDataFlux_Avg %>%
    bind_rows(myDataFlux_Uncert)

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
      geom_ribbon(
        data = myDataNECBB %>% filter(Scenario == "Uncertainty"),
        mapping = aes(x = Timestep, ymin = min, ymax = max),
        inherit.aes = FALSE,
        fill = "gray40",
        alpha = 0.3
      ) +
      geom_line(linewidth = 0.8) +
      theme_bw() +
      scale_color_manual(values = col, breaks = c(gwpLegend, "Uncertainty")) +
      scale_linetype_manual(
        values = lineType,
        breaks = c(gwpLegend, "Uncertainty")
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
      ylim(floor(minVal), ceiling(maxVal)) +
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
      geom_ribbon(
        data = myDataNECBB %>% filter(Scenario == "Uncertainty"),
        mapping = aes(x = Timestep, ymin = min, ymax = max),
        inherit.aes = FALSE,
        fill = "gray40",
        alpha = 0.3
      ) +
      geom_line(linewidth = 0.8) +
      theme_bw() +
      scale_color_manual(values = col, breaks = c(gwpLegend, "Uncertainty")) +
      scale_linetype_manual(
        values = lineType,
        breaks = c(gwpLegend, "Uncertainty")
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
      ylim(floor(minVal), ceiling(maxVal)) +
      ggtitle("b.")
  }

  p7

  ggsave(
    paste0(
      pathOutManuscript,
      "/",
      "Figure8",
      plotFlowsLetters[i],
      "_",
      plotName,
      ".png"
    ),
    p7,
    width = 3.5,
    height = 3.5,
    dpi = 600
  )

  rm(
    myDataNECBB,
    plotName,
    p7,
    col,
    lineType
  )
}


## Only Uncertainty Scenario ------------------
gwpLegend <- str_extract(modelName, "GWP-\\d+")

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

  myDataFlux_Uncert <- myDataFluxUncert %>%
    filter(FlowGroupId == plotFlows[i]) %>%
    group_by(Timestep, Iteration) %>%
    summarize(totalC = sum(Amount, na.rm = T), .groups = "drop") %>%
    group_by(Timestep) %>%
    summarize(
      mean = mean(totalC),
      min = min(totalC),
      max = max(totalC),
      .groups = "drop"
    ) %>%
    mutate(
      Scenario = gwpLegend,
      Color = "gray40",
      Type = "solid"
    )

  myDataNECBB <- myDataFlux_Uncert

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
      geom_ribbon(
        data = myDataNECBB %>% filter(Scenario == gwpLegend),
        mapping = aes(x = Timestep, ymin = min, ymax = max),
        inherit.aes = FALSE,
        fill = "gray40",
        alpha = 0.3
      ) +
      geom_line(linewidth = 0.8) +
      theme_bw() +
      scale_color_manual(values = "black", breaks = gwpLegend) +
      scale_linetype_manual(
        values = lineType,
        breaks = c(gwpLegend)
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
      ylim(floor(minVal), ceiling(maxVal)) +
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
      geom_ribbon(
        data = myDataNECBB %>% filter(Scenario == gwpLegend),
        mapping = aes(x = Timestep, ymin = min, ymax = max),
        inherit.aes = FALSE,
        fill = "gray40",
        alpha = 0.3
      ) +
      geom_line(linewidth = 0.8) +
      theme_bw() +
      scale_color_manual(values = "black", breaks = gwpLegend) +
      scale_linetype_manual(
        values = lineType,
        breaks = gwpLegend
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
      ylim(floor(minVal), ceiling(maxVal)) +
      ggtitle("b.")
  }

  p7

  ggsave(
    paste0(
      pathOutManuscript,
      "/",
      "Figure8",
      "_",
      plotName,
      " Uncertainty Scenario",
      ".png"
    ),
    p7,
    width = 3.5,
    height = 3.5,
    dpi = 600
  )

  rm(
    myDataFlux_Uncert,
    myDataNECBB,
    plotName,
    p7,
    col,
    lineType
  )
}

# plotFlows <- c("Annual Net Ecosystem Carbon Balance (tons C per year)")
#
# for (i in 1:length(plotFlows)) {
#   plotName <- gsub(
#     " ",
#     "",
#     gsub(
#       ")",
#       "",
#       gsub("(", "", gsub(": ", " ", plotFlows[i]), fixed = T),
#       fixed = T
#     )
#   )
#
#   myDataFlux_Uncert <- myDataFluxUncert %>%
#     filter(FlowGroupId == plotFlows[i]) %>%
#     group_by(Timestep, Iteration) %>%
#     summarize(totalC = sum(Amount, na.rm = T), .groups = "drop") %>%
#     group_by(Timestep) %>%
#     summarize(
#       mean = mean(totalC),
#       min = min(totalC),
#       max = max(totalC),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       Scenario = "Uncertainty",
#       Color = "gray40",
#       Type = "dotted"
#     )
#
#   myDataFlux_Avg <- myDataFluxAvg %>%
#     filter(FlowGroupId == plotFlows[i]) %>%
#     group_by(Timestep, Iteration) %>%
#     summarize(totalC = sum(Amount, na.rm = T), .groups = "drop") %>%
#     group_by(Timestep) %>%
#     summarize(
#       mean = mean(totalC),
#       min = min(totalC),
#       max = max(totalC),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       Scenario = "GWP-100",
#       Color = "black",
#       Type = "solid"
#     )
#
#   myDataNECBA <- myDataFlux_Avg %>%
#     bind_rows(myDataFlux_Uncert)
#
#   myDataNECBA$mean <- myDataNECBA$mean / 1000000
#   myDataNECBA$min <- myDataNECBA$min / 1000000
#   myDataNECBA$max <- myDataNECBA$max / 1000000
#
#   minVal <- min(myDataNECBA$min)
#   maxVal <- max(myDataNECBA$max)
#
#   if (minVal > 0 & maxVal > 0) {
#     minVal = 0
#   } else if (minVal < 0 & maxVal < 0) {
#     maxVal = 0
#   }
#
#   col <- as.character(myDataNECBA$Color)
#   names(col) <- as.character(myDataNECBA$Scenario)
#
#   lineType <- as.character(myDataNECBA$Type)
#   names(lineType) <- as.character(myDataNECBA$Scenario)
#
#   p6 <- ggplot(
#     myDataNECBA,
#     aes(
#       x = Timestep,
#       y = mean,
#       color = Scenario,
#       group = Scenario,
#       linetype = Scenario
#     )
#   ) +
#     geom_ribbon(
#       data = myDataNECBA %>% filter(Scenario == "Uncertainty"),
#       mapping = aes(x = Timestep, ymin = min, ymax = max),
#       inherit.aes = FALSE,
#       fill = "gray40",
#       alpha = 0.3
#     ) +
#     geom_line(linewidth = 0.8) +
#     theme_bw() +
#     scale_color_manual(values = col, breaks = c("GWP-100", "Uncertainty")) +
#     scale_linetype_manual(
#       values = lineType,
#       breaks = c("GWP-100", "Uncertainty")
#     ) +
#     scale_x_continuous(
#       limits = c(2001, 2016),
#       breaks = c(2001, 2006, 2010, 2016)
#     ) +
#     theme(
#       panel.border = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.line = element_line(colour = "black"),
#       legend.position = "bottom",
#       legend.title = element_blank()
#     ) +
#     guides(
#       colour = guide_legend(
#         nrow = 2,
#         keywidth = 0.4,
#         keyheight = 0.1,
#         default.unit = "inch"
#       )
#     ) +
#     xlab("\nYear") +
#     ylab(as.expression(bquote(atop(
#       "Net Ecosystem Carbon Balance",
#       "(Tg " ~ C ~ yr^-1 * ")"
#     )))) +
#     ylim(floor(minVal), ceiling(maxVal))
#
#   p6
#
#   ggsave(
#     paste0(pathOutManuscript, "/", plotName, "_", "LandCover", ".png"),
#     p6,
#     width = 3.5,
#     height = 3.5,
#     dpi = 600
#   )
#
#   rm(minVal, maxVal, col, lineType)
#
#   # Create table of cumulative NECB differences
#
#   myDataNECBBwide <- myDataNECBA %>%
#     select(Scenario, Timestep, mean) %>%
#     pivot_wider(names_from = Scenario, values_from = mean) %>%
#     arrange(Timestep) %>%
#     mutate(
#       ScenDiff = `GWP-100` - Uncertainty
#     )
#
#   nameTabular <- "TgC_yr"
#
#   write.csv(
#     myDataNECBBwide,
#     paste0(pathOut, "NECB_Comparison_2026-02-18_", nameTabular, ".csv"),
#     row.names = F
#   )
# }
