for (s in 1:length(scen)){
  
  idS <- scenarioList$ScenarioId[grep(scen[s],scenarioList$Name)]
  
  myScenarioS <- scenario(myProject, scenario=max(idS))
  
  myDataLandS <- datasheet(myScenarioS, "stsim_OutputStratumState")
  
  landS <- myDataLandS %>%
    left_join(lookupLC, by = join_by(StateClassId)) %>%
    left_join(lookupChart, by = join_by(LandClass)) %>%
    group_by(Timestep, LandClass, Color) %>%
    summarize(Area = sum(Amount, na.rm = T)) %>%
    filter(!(LandClass == "Water & Shore")) %>%
    filter(Timestep %in% c(2001,2006,2010,2016))
  
  table(table(landS$Timestep) == 5)
  
  col <- as.character(landS$Color)
  names(col) <- as.character(landS$LandClass)
  
  landS$Timestep <- as.factor(landS$Timestep)
  
  p2 <- ggplot(landS, aes(x = Timestep, y = Area, fill = LandClass, group = LandClass)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values=col) +
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
    ylab("Land area (hectares)\n")
  
  p2
  
  ggsave(paste0(pathOutLandCover,"/LandCover_",gsub(" ","",scen[s]),".png"), p2, width = 7.5, height = 4, dpi = 600)
  
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
  
  ggsave(paste0(pathOutLandCover,"/LandCoverChange_",gsub(" ","",scen[s]),".png"), p3, width = 7.5, height = 4, dpi = 600)
  
}