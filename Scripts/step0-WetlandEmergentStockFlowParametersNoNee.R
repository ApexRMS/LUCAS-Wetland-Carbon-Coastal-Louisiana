##############################################################################
# Script to estimate carbon stocks and flows for emergent wetland model      #
#                                                                            #
# Written by: Bronwyn Rayfield, Katie Birchard, & Amanda Schwantes, ApexRMS  #
# bronwyn.rayfield@apexrms.com                                               #
# katie.birchard@apexrms.com                                                 #
# amanda.schwantes@apexrms.com                                               #
##############################################################################

library(tidyverse)
options(tibble.width = Inf, tibble.print_min = Inf)
dateStamp <- "2025_05_16"


# Set constants ----------------------------------------------------------------

# Set directory paths
#baseDir <- 'C:/gitprojects/a275/'
baseDir <- rootPath
cleanDataDir <- paste0(baseDir, "Data/Datasheets Wetland/FieldData/")
outDir <- paste0(baseDir, "Data/Datasheets Wetland/")

subscenariosDir <- paste0(baseDir, "Data/Datasheets Wetland/Emergent/")
# Create directories for initial conditions and transition targets
if (!dir.exists(file.path(subscenariosDir, "stsimsf_FlowMultiplier"))) {
  dir.create(
    file.path(subscenariosDir, "stsimsf_FlowMultiplier"),
    showWarnings = F
  )
}
if (!dir.exists(file.path(subscenariosDir, "stsim_StateAttributeValue"))) {
  dir.create(
    file.path(subscenariosDir, "stsim_StateAttributeValue"),
    showWarnings = F
  )
}
if (!dir.exists(file.path(subscenariosDir, "stsim_DistributionValue"))) {
  dir.create(
    file.path(subscenariosDir, "stsim_DistributionValue"),
    showWarnings = F
  )
}


# Set parameters
# Number significant digits
sig_figs <- 4
# Download data?
download_data <- F

# soil depth is used as a proxy for C flux rates
# BGVF depth is the maximum depth of soil in cm included in the BG-VF stock
BGVFdepth <- 30
# BGS depth is the maximum depth of soil in cm included in the BG-S stock (NB minimum depth for the BG-S stock is BGVFdepth)
BGSdepth <- 100

# Conversion factors
# Conversion factor from %Organic Matter to Total Carbon (Baustian et al., 2017)
# Baustian, M.M., Stagg, C.L., Perry, C.L., Moss, L.C., Carruthers, T.J. and Allison, M., 2017. Relationships between salinity and short-term soil carbon accumulation rates from marsh types across a landscape in the Mississippi River Delta. Wetlands, 37, pp.313-324.
conversionOMtoTC <- 0.47
## Add area conversion parameter
conversionG.cm3toG.m2.cm = 10^4

## Base URL for data download
baseUrl <- "https://www.sciencebase.gov"


# Read Data --------------------------------------------------------------------

# Data provided directly from Camille Stagg, USGS

# Above and belowground biomass data
# Primary publication citation: Stagg, C.L., Schoolmaster, D.R., Jr., Piazza, S.C., Snedden, G., Steyer, G.D., Fischenich, C.J., and McComas, R.W., 2017, A landscape-scale assessment of above- and belowground primary production in coastal wetlands: implications for climate change-induced community shifts: Estuaries and Coasts, v. 40, no. 3, p. 856-879, http://dx.doi.org/10.1007/s12237-016-0177-y.
# Data release citation: Stagg, C.L., Schoolmaster, D.R., Piazza, S.C., Snedden, G., Steyer, G.D., Fischenich, C.J., and McComas, R.W., 2017, Primary production across a coastal wetland landscape in Louisiana, U.S.A. (2012-2014): U.S. Geological Survey data release, https://doi.org/10.5066/F7G44NFJ.
# Full data release landing page: https://www.sciencebase.gov/catalog/item/581a3445e4b0bb36a4ca2f29
# Above and belowground primary production: https://www.sciencebase.gov/catalog/item/581a3567e4b0bb36a4ca2f37
# Data release only includes averages for the full year but we needed the values for specific sampling periods to use the stock values in winter only and the NPP calculated to start and end with winter
abovegroundPlot <- read_csv(paste0(
  cleanDataDir,
  'LUCAS_AbovegroundPlot2012-2013_calcs_ER73_Stagg_2019-08.csv'
))
belowgroundPlot <- read_csv(paste0(
  cleanDataDir,
  'LUCAS_BelowgroundPlot2012-2013_calcs_ER73_Stagg_2019-08.csv'
))


# Download data from USGS data releases

# Litter bag data
# Primary publication citation: Stagg, C.L., Baustian, M.M., Perry, C.L., Carruthers, T.J.B., and Hall, C.T., 2017, Direct and indirect controls on organic matter decomposition in four coastal wetland communities along a landscape salinity gradient: Journal of Ecology, Early View, https://doi.org/10.1111/1365-2745.12901.
# Data release citation: Stagg, C.L., Baustian, M.M., Perry, C.L., Carruthers, T.J.B., and Hall, C.T., 2017, Organic matter decomposition across a coastal wetland landscape in Louisiana, U.S.A. (2014-2015): U.S. Geological Survey data release, https://doi.org/10.5066/F7639MVK.
# Full data release landing page: https://www.sciencebase.gov/catalog/item/57fd1019e4b0824b2d130ed5
# Above and belowground decomposition landing page: https://www.sciencebase.gov/catalog/item/57fd3d60e4b0824b2d13106b
# "FSP_Stagg_Decomp_BelowgroundLitterDecomposition.csv"
litterBagFile <- "catalog/file/get/57fd3d60e4b0824b2d13106b?f=__disk__37%2F03%2F0d%2F37030d41a011b22bd712a7b1694102b8fd933ffa"
litterBagUrl <- file.path(baseUrl, litterBagFile)
tempFilepath <- file.path(cleanDataDir, "litterbag.csv")
if (download_data) {
  download.file(litterBagUrl, tempFilepath)
}
litterbagPlot <- read_csv(tempFilepath)

litterbagAbovePlot <- read_csv(paste0(
  cleanDataDir,
  "FSP_Stagg_Decomp_AbovegroundLitterDecomposition.csv"
))
litterbagForestPlot <- read_csv(paste0(
  cleanDataDir,
  "FSP_Stagg_GCC_Decomp_Decomposition.csv"
))

# Soil C (0-10cm) data
# Primary publication citation: Baustian, M.M., Stagg, C.L., Perry, C.L. et al. Correction to: “Relationships Between Salinity and Short-Term Soil Carbon Accumulation Rates from Marsh Types Across a Landscape in the Mississippi River Delta”. Wetlands 40, 917–923 (2020). https://doi.org/10.1007/s13157-020-01283-8
# Data release citation: Baustian, M.M., Stagg, C.L., Perry, C.L., Moss, L.C., Carruthers, T.J.B., Allison, M., and Hall, C.T., 2020, Short term soil carbon data and accretion rates from four marsh types in Mississippi River Delta collected in 2015: U.S. Geological Survey data release, https://doi.org/10.5066/P9M5YSOL.
# Full data release landing page: https://www.sciencebase.gov/catalog/item/5b329df5e4b040769c159be2
# "Baustian_Short Term Carbon_Soil Core Data.csv"
soilCoreShortFile <- "catalog/file/get/5b329df5e4b040769c159be2?f=__disk__9f%2Ff1%2F5f%2F9ff15faf22ff5fdb491d3c2d7ecddef5ca64cb86"
soilCoreShortUrl <- file.path(baseUrl, soilCoreShortFile)
tempFilepath <- file.path(cleanDataDir, "soilCoreShort.csv")
if (download_data) {
  download.file(soilCoreShortUrl, tempFilepath, cacheOK = F)
}
soilCorePlotShort <- read.csv(tempFilepath) #for some reason crashes w/ readr::read_csv()

# Soil C (12-100cm) data
# Primary publication citation: Baustian, M.M., Stagg, C.L., Perry, C.L., Moss, L.C., & Carruthers, T.J.B., 2021, Long-term carbon sinks in marsh soils of coastal Louisiana are at risk to wetland loss: Journal of Geophysical Research: Biogeosciences, v. 126, no. 3, art. e2020JG005832, https://doi.org/10.1029/2020JG005832.
# Data release citation: Baustian, M.M., Stagg, C.L., Perry, C.L., Moss, L.C., Carruthers, T.J.B., Allison, M.A., and Hall, C.T., 2021, Long-term soil carbon data and accretion from four marsh types in Mississippi River Delta in 2015: U.S. Geological Survey data release, https://doi.org/10.5066/P93U3B3E.
# Full data release landing page: https://www.sciencebase.gov/catalog/item/5b3299d4e4b040769c159bb0
# "Baustian_Long_Term_Carbon_Soil.csv"
soilCoreLongFile <- "catalog/file/get/5b3299d4e4b040769c159bb0?f=__disk__e8%2Fc2%2Ff2%2Fe8c2f247af3665315c87420869603da90ea4c9a2"
soilCoreLongUrl <- file.path(baseUrl, soilCoreLongFile)
tempFilepath <- file.path(cleanDataDir, "soilCoreLong.csv")
if (download_data) {
  download.file(soilCoreLongUrl, tempFilepath, cacheOK = F)
}
soilCorePlotLong <- read_csv(tempFilepath)

# Data retrieved from Table 2 of Baustian et al. 2021 publication
# Baustian, M.M., Stagg, C.L., Perry, C.L., Moss, L.C., & Carruthers, T.J.B., 2021, Long-term carbon sinks in marsh soils of coastal Louisiana are at risk to wetland loss: Journal of Geophysical Research: Biogeosciences, v. 126, no. 3, art. e2020JG005832, https://doi.org/10.1029/2020JG005832.
burialSiteRaw <- read_csv(paste0(
  cleanDataDir,
  'Baustian_Long_term_Accretion_Table2_Updated_03-09-2023.csv'
))


# Data provided directly from Camille Stagg, USGS
# *****BR****** Need to check if there is a data release for these data
soilShoreWaterRaw <- read_csv(paste0(
  cleanDataDir,
  "Dataset_02_macroclimate_soil_data_2_22_2016.csv"
))
vegShoreWaterRaw <- read_csv(paste0(
  cleanDataDir,
  "Dataset_01_macroclimate_vegetation_data_all_2_24_2016.csv"
))

# Wetland crosswalk
LUCASWetlandSubclassRaw <- read_csv(paste0(
  cleanDataDir,
  "LUCAS_Wetland_Subclass.csv"
))

LUCASWetlandSubclassRaw <- LUCASWetlandSubclassRaw %>%
  rename(
    `Wetland Subclass LUCAS Older` = `Wetland Subclass LUCAS`,
    ExternalVariableValueOlder = ExternalVariableValue
  ) %>%
  mutate(
    `Wetland Subclass LUCAS` = case_when(
      (`Habitat Type 2014` %in%
        c("Saline", "Brackish", "Intermediate")) ~ "Estuarine Herbaceous",
      (`Habitat Type 2014` %in% c("Fresh")) ~ "Palustrine Herbaceous"
    )
  ) %>%
  group_by(`Wetland Subclass LUCAS`) %>%
  mutate(ExternalVariableValue = row_number()) %>%
  ungroup()


# Biomass to Carbon conversion factors
# Data provided directly from Camille Stagg, USGS
# biomass-to-carbon-conversion-factors.R pre-processes the original data
biomassToCarbonRaw <- read_csv(paste0(
  cleanDataDir,
  "biomass-to-carbon-site.csv"
))

# Literature values for lateral flux
lateralFluxRaw <- read_csv(paste0(
  cleanDataDir,
  "wetlandHerbaceousSummary_StockBasedEquilibrium_2023-01-16.csv"
))

# Methane Flux Data

# methaneRaw <- tibble(
#   `Wetland Subclass LUCAS` = c("Estuarine Herbaceous",
#                                "Palustrine Herbaceous"),
#   MethaneFlux = c(11,47)
# )

## Salinity based classification substituted on 2026-08-06
# methaneRaw <- LUCASWetlandSubclassRaw %>%
#   select(`Site.ID`, `Habitat Type 2014`,`Wetland Subclass LUCAS`) %>%
#   mutate(MethaneFlux = case_when(`Habitat Type 2014` == "Saline"~ 11.1,
#                                   `Habitat Type 2014` != "Saline" ~ 47.1)) %>%
#   select(Site.ID,`Wetland Subclass LUCAS`,MethaneFlux)

methaneRaw <- LUCASWetlandSubclassRaw %>%
  select(`Site.ID`, `Habitat Type 2014`, `Wetland Subclass LUCAS`) %>%
  mutate(
    MethaneFlux = case_when(
      `Wetland Subclass LUCAS` == "Estuarine Herbaceous" ~ 11.1,
      `Wetland Subclass LUCAS` == "Palustrine Herbaceous" ~ 47.1
    )
  ) %>%
  select(Site.ID, `Wetland Subclass LUCAS`, MethaneFlux)

# methaneRaw <- tibble(
#   `Wetland Subclass LUCAS` = c("Estuarine Herbaceous",
#                                "Palustrine Herbaceous"),
#   MethaneFlux = c(14.49,14.49)
# )

# Wrangle raw data -------------------------------------------------------------

## site and pool specific biomass to carbon conversion factors

biomassToCarbonAbove <- biomassToCarbonRaw %>%
  select(Site.ID, Pool, Status, mean) %>%
  filter(Pool == "above") %>%
  pivot_wider(names_from = Status, values_from = mean) %>%
  rename(btcDead = dead, btcLive = live) %>%
  select(!c(Pool))

biomassToCarbonBelow <- biomassToCarbonRaw %>%
  select(Site.ID, Pool, Status, mean) %>%
  filter(Pool == "below") %>%
  pivot_wider(names_from = Status, values_from = mean) %>%
  rename(btcDead = dead, btcLive = live) %>%
  select(!c(Pool))

## Lateral flux
lateralFluxRaw <- lateralFluxRaw %>%
  filter(Source != "Mean") %>%
  mutate(
    `Wetland Subclass LUCAS` = case_when(
      (Salinity %in% c("Salt", "Brackish")) ~ "Estuarine Herbaceous",
      (Salinity %in% c("Fresh")) ~ "Palustrine Herbaceous"
    )
  )

lateralFluxType <- lateralFluxRaw %>%
  group_by(`Wetland Subclass LUCAS`) %>%
  summarize(
    TotalLatTrans_Amount = round(mean(Flux.g.C.per.m2.per.y, na.rm = T), 1)
  )


## Litter bag data
litterbagPlot <- litterbagPlot %>%
  select(where(function(x) all(!is.na(x)))) %>%
  filter(`Sampling Event` == "T4", `Mass Remaining (%)` != ".") %>%
  mutate(
    `BGVF.annual.percent.loss` = 100 - as.numeric(`Mass Remaining (%)`),
    Site.ID = case_when(
      `Site ID` == "BA0104" ~ "BA-01-04",
      nchar(`Site ID`) == 3 ~ paste0("CRMS0", `Site ID`),
      nchar(`Site ID`) == 4 ~ paste0("CRMS", `Site ID`),
      nchar(`Site ID`) > 4 ~ `Site ID`
    )
  ) %>%
  select(Site.ID, BGVF.annual.percent.loss)

## Litter bag forest data
litterbagForestPlotSub <- litterbagForestPlot %>%
  filter(
    `Sampling Event` == "T6",
    `Percent Mass Remaining` != ".",
    `River ID` == 1,
    `Site ID` == 1
  )
mean(as.numeric(litterbagForestPlotSub$`Percent Mass Remaining`))

## Litter bag data
litterbagAbovePlot <- litterbagAbovePlot %>%
  select(where(function(x) all(!is.na(x)))) %>%
  filter(`Sampling Event` == "T4", `Mass Remaining (%)` != ".") %>%
  mutate(
    `AGVF.annual.percent.loss` = 100 - as.numeric(`Mass Remaining (%)`),
    Site.ID = case_when(
      `Site ID` == "BA0104" ~ "BA-01-04",
      nchar(`Site ID`) == 3 ~ paste0("CRMS0", `Site ID`),
      nchar(`Site ID`) == 4 ~ paste0("CRMS", `Site ID`),
      nchar(`Site ID`) > 4 ~ `Site ID`
    )
  ) %>%
  select(Site.ID, AGVF.annual.percent.loss)

# LUCASWetlandSubclass <- LUCASWetlandSubclassRaw %>% select(`Site.ID`, `Wetland Subclass LUCAS`)
#
# litterbagAbovePlotSummary <- litterbagAbovePlot %>%
#   group_by(Site.ID) %>%
#   summarize(meanMassRemaining = 100 - mean(BGVF.annual.percent.loss)) %>%
#   ungroup() %>%
#   left_join(LUCASWetlandSubclass, by = 'Site.ID') %>%
#   group_by(`Wetland Subclass LUCAS`) %>%
#   summarize(mean = mean(meanMassRemaining),
#             min = min(meanMassRemaining),
#             max = max(meanMassRemaining)) %>%
#   ungroup()

# litterbagBelowPlotSummary <- litterbagPlot %>%
#   group_by(Site.ID) %>%
#   summarize(meanMassRemaining = 100 - mean(BGVF.annual.percent.loss)) %>%
#   ungroup() %>%
#   summarize(mean = mean(meanMassRemaining),
#             min = min(meanMassRemaining),
#             max = max(meanMassRemaining))

## Soil core data
# Comes from 2 separate data releases: short term (0-10cm) and long term (12-100cm) *NB there is a data gap between 10-12cm but also other gaps (e.g. 14-16)
soilIncrementDict <- data.frame(
  Core_Increment = c(1, 2, 3, 4, 5),
  Core_Increment_New = c("0-2", "2-4", "4-6", "6-8", "8-10")
)

soilCorePlotShort <- soilCorePlotShort %>%
  left_join(soilIncrementDict, by = "Core_Increment") %>%
  mutate(
    `Core Increment (cm)` = Core_Increment_New,
    `Organic matter (percent)` = Organic_Matter,
    `Bulk Density (g/cm^3)` = Bulk_Density
  ) %>%
  select(
    Site,
    `Core Increment (cm)`,
    `Organic matter (percent)`,
    `Bulk Density (g/cm^3)`
  )

soilCorePlotLong <- soilCorePlotLong %>%
  select(
    Site,
    `Core Increment (cm)`,
    `Organic matter (percent)`,
    `Bulk Density (g/cm^3)`
  )

# Fix error in column "Core Increment (cm)"
# This returns "12-Oct" but it should be "10-12"
# soilCorePlotLong$`Core Increment (cm)`[151]
soilCorePlotLong$`Core Increment (cm)`[151] <- "10-12"

soilCorePlot <- rbind(soilCorePlotShort, soilCorePlotLong)

soilCorePlot <- soilCorePlot %>%
  mutate(
    Site.ID = case_when(
      nchar(Site) == 3 ~ paste0("CRMS0", Site),
      nchar(Site) == 4 ~ paste0("CRMS", Site),
      nchar(Site) > 4 ~ Site
    )
  )

# Burial site raw
burialSiteRaw <- burialSiteRaw %>%
  mutate(`Habitat Type 2014` = `Most Freq. Occ. Habitat from 1949 to 1978`)

# Add LUCAS Wetland Subclass to raw data
LUCASWetlandSubclass <- LUCASWetlandSubclassRaw %>%
  select(`Site.ID`, `Wetland Subclass LUCAS`)
abovegroundPlot <- abovegroundPlot %>%
  left_join(LUCASWetlandSubclass, by = 'Site.ID')
belowgroundPlot <- belowgroundPlot %>%
  left_join(LUCASWetlandSubclass, by = 'Site.ID')
litterbagPlot <- litterbagPlot %>%
  left_join(LUCASWetlandSubclass, by = 'Site.ID')
soilCorePlot <- soilCorePlot %>% left_join(LUCASWetlandSubclass, by = 'Site.ID')
burialSiteRaw <- burialSiteRaw %>%
  left_join(LUCASWetlandSubclass, by = 'Site.ID')
litterbagAbovePlot <- litterbagAbovePlot %>%
  left_join(LUCASWetlandSubclass, by = 'Site.ID')

# Prepare data to summarize carbon stocks and flows by site by year -------------------------------------
# Summarise Foliage, Aboveground Very Fast (AGVF), Root and Belowground Very Fast (BGVF) stocks at the transect and site level
# Use stock amounts reported in February (i.e. annual minimum)
# Note that for abovegroundPlot and belowgroundPlot data Events T3 and T7 correspond to February in 2013 and 2014
# Note that belowground has a depth field. To summarize at the transect level, I sum across all depths (A 0-7.5, B 7.5-15, C 15-30).
abovegroundStocksSite <- abovegroundPlot %>%
  filter(Event %in% c("T3", "T7")) %>%
  group_by(Site.ID, `Wetland Subclass LUCAS`, Year) %>%
  summarize(
    foliageStockB = mean(`Live Biomass.g` / Plot.Area.m2, na.rm = TRUE),
    AGVFStockB = mean(`Dead Biomass.g` / Plot.Area.m2, na.rm = TRUE)
  ) %>%
  left_join(biomassToCarbonAbove, by = "Site.ID") %>%
  mutate(
    foliageStock = foliageStockB * (btcLive / 100),
    AGVFStock = AGVFStockB * (btcDead / 100)
  ) %>%
  select(!c(foliageStockB, AGVFStockB, btcDead, btcLive))

belowgroundStocksDepth <- belowgroundPlot %>%
  filter(Event %in% c("T3", "T7")) %>%
  group_by(Site.ID, Depth, `Wetland Subclass LUCAS`, Year) %>%
  mutate(
    rootStock = Live.Roots.g / Core.Area.m2,
    BGVFStock = Dead.Roots.g / Core.Area.m2
  ) %>%
  summarize(
    rootStockDepth = mean(rootStock, na.rm = T),
    BGVFStockDepth = mean(BGVFStock, na.rm = T)
  )

belowgroundStocksSite <- belowgroundStocksDepth %>%
  group_by(Site.ID, `Wetland Subclass LUCAS`, Year) %>%
  summarise(
    rootStockB = sum(rootStockDepth),
    BGVFStockB = sum(BGVFStockDepth)
  ) %>%
  left_join(biomassToCarbonBelow, by = "Site.ID") %>%
  mutate(
    rootStock = rootStockB * (btcLive / 100),
    BGVFStock = BGVFStockB * (btcDead / 100)
  ) %>%
  select(!c(rootStockB, BGVFStockB, btcDead, btcLive))


# Create a separate table for each stock with a separate column for each year
# This will help with faster computations of difference equations later
foliageStockSite <- abovegroundStocksSite %>%
  select(-AGVFStock) %>%
  spread(Year, foliageStock) %>%
  rename('2013_FoliageStock' = '2013', '2014_FoliageStock' = '2014') %>%
  mutate(FoliageStock = mean(c(`2013_FoliageStock`, `2014_FoliageStock`)))

AGVFStockSite <- abovegroundStocksSite %>%
  select(-foliageStock) %>%
  spread(Year, AGVFStock) %>%
  rename('2013_AGVFStock' = '2013', '2014_AGVFStock' = '2014') %>%
  mutate(AGVFStock = mean(c(`2013_AGVFStock`, `2014_AGVFStock`)))

rootStockSite <- belowgroundStocksSite %>%
  select(-BGVFStock) %>%
  spread(Year, rootStock) %>%
  rename('2013_RootStock' = '2013', '2014_RootStock' = '2014') %>%
  mutate(RootStock = mean(c(`2013_RootStock`, `2014_RootStock`)))

BGVFStockSite <- belowgroundStocksSite %>%
  select(-rootStock) %>%
  spread(Year, BGVFStock) %>%
  rename('2013_BGVFStock' = '2013', '2014_BGVFStock' = '2014') %>%
  mutate(BGVFStock = mean(c(`2013_BGVFStock`, `2014_BGVFStock`)))


# Summarize Belowground Slow (BG-S) stock at the site level
# Convert g cm-3 to g m-2 ground area per 1 cm depth interval
# The BG-S stock includes all soil in the 1m soil core
# BGSStockTop = total carbon (TC) in e.g. 0-30 cm minus the live and dead roots (aka Root stock and BG-VF stock)
# BGSStockBottom = TC in e.g. 30-100 cm
# BGSStock = BGSStockTop + BGSStockBottom

# Calculate total carbon density
# set NA values and conver to numeric format
soilCorePlot$`Organic matter (percent)`[
  soilCorePlot$`Organic matter (percent)` == "."
] <- NA
soilCorePlot$`Organic matter (percent)` <- as.numeric(
  soilCorePlot$`Organic matter (percent)`
)
# total carbon density = bulk density * organic matter % * conversion factor OM to TC
soilCorePlot <- soilCorePlot %>%
  mutate(
    `Total Carbon (percent)` = `Organic matter (percent)` * conversionOMtoTC,
    `TC g cm-3` = (`Bulk Density (g/cm^3)` * `Total Carbon (percent)`) / 100
  )

# Convert total carbon to g per m2 per cm
soilCorePlot <- soilCorePlot %>%
  mutate(TC.g.m.2.per.cm = `TC g cm-3` * conversionG.cm3toG.m2.cm)

# Isolate min and max depth from "Core Increment"
soilCorePlot <- soilCorePlot %>%
  separate(
    col = `Core Increment (cm)`,
    into = c("min.depth.cm", "max.depth.cm"),
    sep = "-"
  ) %>%
  mutate(
    min.depth.cm = as.numeric(min.depth.cm),
    max.depth.cm = as.numeric(max.depth.cm)
  )

# BGS C in 0-30cm
BGSStockSiteTop <- soilCorePlot %>%
  filter(max.depth.cm < BGVFdepth) %>%
  mutate(
    depth.interval = max.depth.cm - min.depth.cm,
    weighted.TC.g.m.2.per.cm = (depth.interval) * TC.g.m.2.per.cm
  ) %>%
  group_by(Site.ID) %>%
  drop_na() %>%
  # Weighted mean calculation
  summarize(
    MeanCDensityBGSTopwithRootBGVF = sum(
      weighted.TC.g.m.2.per.cm,
      na.rm = TRUE
    ) /
      sum(depth.interval, na.rm = TRUE)
  ) %>%
  mutate(TCTop = MeanCDensityBGSTopwithRootBGVF * BGVFdepth) %>%
  full_join(rootStockSite, by = 'Site.ID') %>%
  full_join(BGVFStockSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  mutate(
    BGSStockTop = TCTop - BGVFStock - RootStock,
    MeanCDensityBGSTop = BGSStockTop / BGVFdepth
  ) %>%
  select(Site.ID, `Wetland Subclass LUCAS`, MeanCDensityBGSTop, BGSStockTop)

# BGS C in 30-100cm
BGSStockSiteBottom <- soilCorePlot %>%
  filter(max.depth.cm >= BGVFdepth) %>%
  mutate(
    depth.interval = max.depth.cm - min.depth.cm,
    weighted.TC.g.m.2.per.cm = (depth.interval) * TC.g.m.2.per.cm
  ) %>%
  group_by(Site.ID) %>%
  drop_na() %>%
  #use weighted mean rather than unweighted mean but for these data they are equivalent
  #summarize('MeanCDensityBGSBottom' = mean(TC.g.m.2.per.cm, na.rm = TRUE)) %>%
  summarize(
    MeanCDensityBGSBottom = sum(weighted.TC.g.m.2.per.cm, na.rm = TRUE) /
      sum(depth.interval, na.rm = TRUE),
    BGSStockBottom = MeanCDensityBGSBottom * (BGSdepth - BGVFdepth)
  )

# BGS C in 0-100cm
BGSStockSite <- BGSStockSiteBottom %>%
  full_join(BGSStockSiteTop, by = 'Site.ID') %>%
  mutate(
    BGSStock = BGSStockTop + BGSStockBottom,
    'Mean BGSCDensity TC.g.m.2.per.cm' = (BGSStockTop + BGSStockBottom) /
      BGSdepth
  ) %>%
  select(
    Site.ID,
    `Wetland Subclass LUCAS`,
    BGSStock,
    `Mean BGSCDensity TC.g.m.2.per.cm`,
    MeanCDensityBGSBottom,
    MeanCDensityBGSTop
  )

write_csv(BGSStockSite, paste0(outDir, "BGSCheck_", dateStamp, ".csv"))

BGSStockSummary <- BGSStockSite %>%
  group_by(`Wetland Subclass LUCAS`) %>%
  summarize(mean(BGSStock), mean(`Mean BGSCDensity TC.g.m.2.per.cm`))


# Calculate NPP at the transect and site level per year per m2 for 2013
# To start and end with winter (min living biomass), use Production listed under Events T3-T6
# Camille already has calculated, so simply need to sum interval productions at transect level and then take mean at site level
# Note that belowground has a depth field. To summarize at the transect level, sum across all sampling periods (T3, T4, T5, T6) and depths (A 0-7.5, B 7.5-15, C 15-30).
foliageNPPEvent <- abovegroundPlot %>%
  filter(Event %in% c("T3", "T4", "T5", "T6")) %>%
  group_by(Site.ID, Event, `Wetland Subclass LUCAS`) %>%
  summarize(
    FoliageNPPEvent = mean(Interval.Production.g / Plot.Area.m2, na.rm = TRUE)
  )

foliageNPPSite <- foliageNPPEvent %>%
  group_by(Site.ID, `Wetland Subclass LUCAS`) %>%
  summarize(FoliageNPPB = sum(FoliageNPPEvent)) %>%
  left_join(biomassToCarbonAbove, by = "Site.ID") %>%
  mutate(FoliageNPP = FoliageNPPB * (btcLive / 100)) %>%
  select(!c(FoliageNPPB, btcDead, btcLive))

rootNPPEvent <- belowgroundPlot %>%
  filter(Event %in% c("T3", "T4", "T5", "T6")) %>%
  group_by(Site.ID, Event, Depth, `Wetland Subclass LUCAS`) %>%
  summarise(
    RootNPPEvent = mean(Interval.Production.g / Core.Area.m2, na.rm = TRUE)
  ) %>%
  group_by(Event, Depth, `Wetland Subclass LUCAS`) %>%
  mutate(
    MeanRootNPPEvent = mean(RootNPPEvent, na.rm = T),
    RootNPPEvent = ifelse(is.na(RootNPPEvent), MeanRootNPPEvent, RootNPPEvent)
  ) %>%
  select(Site.ID, Event, Depth, `Wetland Subclass LUCAS`, RootNPPEvent)

rootNPPSite <- rootNPPEvent %>%
  group_by(Site.ID, `Wetland Subclass LUCAS`) %>%
  summarize(RootNPPB = sum(RootNPPEvent)) %>%
  left_join(biomassToCarbonBelow, by = "Site.ID") %>%
  mutate(RootNPP = RootNPPB * (btcLive / 100)) %>%
  select(!c(RootNPPB, btcDead, btcLive))

# Calculate outputs from AG-VF and BG-VF at site-level based on litter bag data (percent annual loss)
# Note that we are not using the AG-VF litter bag data
litterbagSite <- litterbagPlot %>%
  group_by(Site.ID, `Wetland Subclass LUCAS`) %>%
  summarize(
    BGVFPercentLoss = mean(BGVF.annual.percent.loss, na.rm = TRUE) / 100
  ) #AGVFPercentLoss = mean(AGVF.annual.percent.loss, na.rm = TRUE) / 100,

litterbagAboveSite <- litterbagAbovePlot %>%
  group_by(Site.ID, `Wetland Subclass LUCAS`) %>%
  summarize(
    AGVFPercentLoss = mean(AGVF.annual.percent.loss, na.rm = TRUE) / 100
  ) #AGVFPercentLoss = mean(AGVF.annual.percent.loss, na.rm = TRUE) / 100,


# Calculate burial rates
burialSiteDensity <- burialSiteRaw %>%
  mutate(
    'Mean Uncorr-210Pb Accr. (cm  yr-1)' = mean(
      `Uncorr-210Pb Accr. (cm  yr-1)`,
      na.rm = T
    ),
    'Uncorr-210Pb Accr. (cm  yr-1)' = ifelse(
      is.na(`Uncorr-210Pb Accr. (cm  yr-1)`),
      `Mean Uncorr-210Pb Accr. (cm  yr-1)`,
      `Uncorr-210Pb Accr. (cm  yr-1)`
    )
  ) %>%
  mutate(
    densitySampleDepth = (2 * ceiling(`Uncorr-210Pb Accr. (cm  yr-1)` / 2))
  ) %>%
  full_join(
    soilCorePlot,
    by = c('Site.ID', 'Wetland Subclass LUCAS'),
    multiple = "all"
  ) %>%
  drop_na(TC.g.m.2.per.cm) %>%
  group_by(Site.ID) %>%
  mutate(max.depth.cm.by.site = max(max.depth.cm, na.rm = T)) %>%
  filter(min.depth.cm >= (max.depth.cm.by.site - densitySampleDepth)) %>%
  mutate(
    depth.interval = max.depth.cm - min.depth.cm,
    weighted.TC.g.m.2.per.cm = (depth.interval) * TC.g.m.2.per.cm
  ) %>%
  summarize(
    `Accretion Interval CDensity TC.g.m.2.per.cm` = sum(
      weighted.TC.g.m.2.per.cm,
      na.rm = TRUE
    ) /
      sum(depth.interval, na.rm = TRUE)
  )

burialSiteDensity %>%
  full_join(BGSStockSite, by = "Site.ID") %>%
  mutate(
    `BGSCDensity Mean TC.g.m.2.per.cm` = `Mean BGSCDensity TC.g.m.2.per.cm`,
    `BGSCDensity Bottom TC.g.m.2.per.cm` = MeanCDensityBGSBottom,
    `BGSCDensity Top TC.g.m.2.per.cm` = MeanCDensityBGSTop
  ) %>%
  select(
    Site.ID,
    `Accretion Interval CDensity TC.g.m.2.per.cm`,
    `BGSCDensity Mean TC.g.m.2.per.cm`,
    `BGSCDensity Bottom TC.g.m.2.per.cm`,
    `BGSCDensity Top TC.g.m.2.per.cm`
  ) %>%
  mutate_if(is.numeric, round, digits = sig_figs) %>%
  write_csv(paste0(outDir, "intermediate_CDensity_", dateStamp, ".csv"))

burialSite <- burialSiteRaw %>%
  full_join(burialSiteDensity, by = c('Site.ID')) %>%
  full_join(BGSStockSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  group_by(`Wetland Subclass LUCAS`) %>%
  mutate(
    'Mean Uncorr-210Pb Accr. (cm  yr-1)' = mean(
      `Uncorr-210Pb Accr. (cm  yr-1)`,
      na.rm = T
    ),
    'Uncorr-210Pb Accr. (cm  yr-1)' = ifelse(
      is.na(`Uncorr-210Pb Accr. (cm  yr-1)`),
      `Mean Uncorr-210Pb Accr. (cm  yr-1)`,
      `Uncorr-210Pb Accr. (cm  yr-1)`
    )
  ) %>%
  mutate(
    'Burial210Pb TC.g.m.2' = `Accretion Interval CDensity TC.g.m.2.per.cm` *
      `Uncorr-210Pb Accr. (cm  yr-1)`
  ) %>%
  select(Site.ID, `Wetland Subclass LUCAS`, `Burial210Pb TC.g.m.2`)

# TODO: Create intermediate output to compare different density values: top, bottom, mean, accretion interval -> save

burialSummary <- burialSite %>%
  group_by(`Wetland Subclass LUCAS`) %>%
  summarize(mean(`Burial210Pb TC.g.m.2`, na.rm = T))

# Stock estimates by site
stockSite <- foliageStockSite %>%
  full_join(AGVFStockSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(rootStockSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(BGVFStockSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(BGSStockSite, by = c('Site.ID', 'Wetland Subclass LUCAS'))

externalVariable <- LUCASWetlandSubclassRaw %>%
  select(`Site.ID`, `ExternalVariableValue`)

# Site-level summary ---------------------------------------------------------------------------------
# Flow estimates based on NPP and stocks
siteSummary0 <- foliageNPPSite %>%
  full_join(rootNPPSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(litterbagSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(litterbagAboveSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(burialSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(stockSite, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(methaneRaw, by = c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  #  full_join(neeSite, by=c('Site.ID', 'Wetland Subclass LUCAS')) %>%
  full_join(externalVariable, by = 'Site.ID') %>%
  full_join(lateralFluxType, by = 'Wetland Subclass LUCAS') %>%
  mutate(
    'Wetland Subclass LUCAS' = recode(
      `Wetland Subclass LUCAS`,
      'Estuarine Herbaceous' = 'Wetland: Estuarine Emergent',
      'Palustrine Herbaceous' = 'Wetland: Palustrine Emergent'
    )
  )

# Create function to calculate proportion emission vs. lateral flux
# From Bronwyn, might be useful to re-write at some point...

funFluxCalc <- function(SummaryTab) {
  SummaryTab <- SummaryTab %>%
    rowwise() %>%
    mutate(
      'TotalNPP_Amount' = FoliageNPP + RootNPP,
      'SedimentTransport_Amount' = 0,
      'FoliageGrowth_PropTotalNPP' = FoliageNPP / TotalNPP_Amount,
      'RootGrowth_PropTotalNPP' = RootNPP / TotalNPP_Amount,
      'FoliageMaxStock' = FoliageStock + FoliageNPP,
      'FoliageMortality_PropMaxStock' = FoliageNPP / FoliageMaxStock,
      'FoliageMortality_Amount' = FoliageMortality_PropMaxStock *
        FoliageMaxStock,
      'RootMaxStock' = RootStock + RootNPP,
      'RootMortality_PropMaxStock' = RootNPP / RootMaxStock,
      'RootMortality_Amount' = RootMortality_PropMaxStock * RootMaxStock,
      'AGVFMaxStock' = AGVFStock + FoliageMortality_Amount,
      'AGVFOut_PropMaxStock' = FoliageMortality_Amount / AGVFMaxStock,
      'AGVFOut_Amount' = AGVFOut_PropMaxStock * AGVFMaxStock,
      'AGVFStabilization_PropOut' = 1 - AGVFPercentLoss,
      'AGVFStabilization_Amount' = AGVFStabilization_PropOut * AGVFOut_Amount,
      'AGVFStabilization_PropMaxStock' = AGVFStabilization_Amount /
        AGVFMaxStock,
      'BGVFMaxStock' = BGVFStock + RootMortality_Amount,
      'BGVFOut_PropMaxStock' = RootMortality_Amount / BGVFMaxStock,
      'BGVFOut_Amount' = BGVFOut_PropMaxStock * BGVFMaxStock,
      'BGVFStabilization_PropOut' = 1 - BGVFPercentLoss,
      'BGVFStabilization_Amount' = max(c(
        BGVFStabilization_PropOut * BGVFOut_Amount,
        `Burial210Pb TC.g.m.2` - AGVFStabilization_Amount
      )),
      #'BGVFStabilization_Amount' = BGVFStabilization_PropOut * BGVFOut_Amount,
      'BGVFStabilization_PropMaxStock' = BGVFStabilization_Amount /
        BGVFMaxStock,
      'BGSDeposition' = SedimentTransport_Amount,
      'BGSIn_Amount' = (BGVFStabilization_Amount +
        AGVFStabilization_Amount +
        BGSDeposition)
    ) %>%
    mutate(
      'BGSMaxStock' = BGSStock + BGSIn_Amount,
      'BGSOut_PropMaxStock' = BGSIn_Amount / BGSMaxStock,
      'BGSOut_Amount' = BGSOut_PropMaxStock * BGSMaxStock,
      'TotalOut_Amount' = AGVFOut_Amount + BGVFOut_Amount + BGSOut_Amount,
      'AGVFOut_PropOut' = AGVFOut_Amount / TotalOut_Amount,
      'BGVFOut_PropOut' = BGVFOut_Amount / TotalOut_Amount,
      'BGSOut_PropOut' = BGSOut_Amount / TotalOut_Amount,
      'TotalMaxStock' = AGVFMaxStock + BGVFMaxStock + BGSMaxStock,
      'AGVFMaxStock_PropMaxStock' = AGVFMaxStock / TotalMaxStock,
      'BGVFMaxStock_PropMaxStock' = BGVFMaxStock / TotalMaxStock,
      'BGSMaxStock_PropMaxStock' = BGSMaxStock / TotalMaxStock,
      'BGSStabilization_PropOut' = `Burial210Pb TC.g.m.2` / BGSOut_Amount,
      'BGSStabilization_Amount' = BGSStabilization_PropOut * BGSOut_Amount,
      'BGSStabilization_PropMaxStock' = `Burial210Pb TC.g.m.2` / BGSMaxStock,
      'TotalStabilization_Amount' = AGVFStabilization_Amount +
        BGVFStabilization_Amount +
        BGSStabilization_Amount,
      'AGVFEmissionLatTrans_Amount' = AGVFOut_Amount - AGVFStabilization_Amount,
      'BGVFEmissionLatTrans_Amount' = BGVFOut_Amount - BGVFStabilization_Amount,
      'BGSEmissionLatTrans_Amount' = BGSOut_Amount - BGSStabilization_Amount,
      'TotalEmissionLatTrans_Amount' = AGVFEmissionLatTrans_Amount +
        BGVFEmissionLatTrans_Amount +
        BGSEmissionLatTrans_Amount,
      #  'BGSDepositionEffective' = TotalNPP_Amount - TotalEmissionLatTrans_Amount,
      'TotalLatTrans_PropEmissionLatTrans' = TotalLatTrans_Amount /
        TotalEmissionLatTrans_Amount,
      # 'AGVFEmissionLatTrans_PropEmissionLatTrans' = AGVFEmissionLatTrans_Amount / TotalEmissionLatTrans_Amount,
      # 'BGVFEmissionLatTrans_PropEmissionLatTrans' = BGVFEmissionLatTrans_Amount / TotalEmissionLatTrans_Amount,
      # 'BGSEmissionLatTrans_PropEmissionLatTrans' = BGSEmissionLatTrans_Amount / TotalEmissionLatTrans_Amount,
      # 'AGVFEmissionLatTrans_PropOut' = AGVFEmissionLatTrans_Amount / AGVFOut_Amount,
      # 'BGVFEmissionLatTrans_PropOut' = BGVFEmissionLatTrans_Amount / BGVFOut_Amount,
      # 'BGSEmissionLatTrans_PropOut' = BGSEmissionLatTrans_Amount / BGSOut_Amount,
      # 'TotalNEE_Amount' = TotalNPP_Amount * NEENPPRatio,
      # 'TotalEmission_Amount' = TotalNEE_Amount + TotalNPP_Amount,
      # 'TotalLatTrans_Amount' = TotalEmissionLatTrans_Amount - TotalEmission_Amount,
      # 'TotalEmission_PropEmissionLatTrans' = TotalEmission_Amount / TotalEmissionLatTrans_Amount,
      # 'TotalLatTrans_PropEmissionLatTrans' = TotalLatTrans_Amount / TotalEmissionLatTrans_Amount,
      'AGVFLatTrans_Amount' = TotalLatTrans_PropEmissionLatTrans *
        AGVFEmissionLatTrans_Amount,
      'BGVFLatTrans_Amount' = TotalLatTrans_PropEmissionLatTrans *
        BGVFEmissionLatTrans_Amount,
      'BGSLatTrans_Amount' = TotalLatTrans_PropEmissionLatTrans *
        BGSEmissionLatTrans_Amount,
      'AGVFLatTrans_PropMaxStock' = AGVFLatTrans_Amount / AGVFMaxStock,
      'BGVFLatTrans_PropMaxStock' = BGVFLatTrans_Amount / BGVFMaxStock,
      'BGSLatTrans_PropMaxStock' = BGSLatTrans_Amount / BGSMaxStock,
      'AGVFLatTrans_PropOut' = AGVFLatTrans_Amount / AGVFOut_Amount,
      'BGVFLatTrans_PropOut' = BGVFLatTrans_Amount / BGVFOut_Amount,
      'BGSLatTrans_PropOut' = BGSLatTrans_Amount / BGSOut_Amount,
      'AGVFEmission_Amount' = AGVFEmissionLatTrans_Amount - AGVFLatTrans_Amount,
      'BGVFEmission_Amount' = BGVFEmissionLatTrans_Amount - BGVFLatTrans_Amount,
      'BGSEmission_Amount' = BGSEmissionLatTrans_Amount - BGSLatTrans_Amount,
      'AGVFEmission_PropMaxStock' = AGVFEmission_Amount / AGVFMaxStock,
      'BGVFEmission_PropMaxStock' = BGVFEmission_Amount / BGVFMaxStock,
      'BGSEmission_PropMaxStock' = BGSEmission_Amount / BGSMaxStock,
      'AGVFEmission_PropOut' = AGVFEmission_Amount / AGVFOut_Amount,
      'BGVFEmission_PropOut' = BGVFEmission_Amount / BGVFOut_Amount,
      'BGSEmission_PropOut' = BGSEmission_Amount / BGSOut_Amount,
      'outAGVFCheck' = FoliageMortality_Amount -
        (AGVFMaxStock *
          (AGVFEmission_PropMaxStock +
            AGVFLatTrans_PropMaxStock +
            AGVFStabilization_PropMaxStock)),
      'outBGVFCheck' = RootMortality_Amount -
        (BGVFMaxStock *
          (BGVFEmission_PropMaxStock +
            BGVFLatTrans_PropMaxStock +
            BGVFStabilization_PropMaxStock)),
      'outBGSCheck' = BGSIn_Amount -
        (BGSMaxStock *
          (BGSEmission_PropMaxStock +
            BGSLatTrans_PropMaxStock +
            BGSStabilization_PropMaxStock)),
      # 'TotalLatTrans_Amount' = AGVFLatTrans_Amount + BGVFLatTrans_Amount + BGSLatTrans_Amount,
      'massBalanceCheck' = TotalNPP_Amount -
        (AGVFOut_Amount -
          AGVFStabilization_Amount +
          BGVFOut_Amount -
          BGVFStabilization_Amount +
          BGSOut_Amount),
      'checkAGVFratio' = AGVFEmission_Amount / AGVFLatTrans_Amount,
      'checkBGVFratio' = BGVFEmission_Amount / BGVFLatTrans_Amount,
      'checkBGSratio' = BGSEmission_Amount / BGSLatTrans_Amount,
      #'check' = (TotalNEE_Amount + TotalNPP_Amount) / (TotalNPP_Amount - BGSStabilization_Amount),
      'checkpropAGVF' = AGVFOut_Amount /
        (TotalNPP_Amount + BGVFStabilization_Amount) -
        AGVFOut_PropOut
    )

  return(SummaryTab)
}


# Add Lateral Flux uncertainty

siteSummaryLatFlux <- siteSummary0 %>%
  select(
    Site.ID,
    `Wetland Subclass LUCAS`,
    FoliageStock,
    RootStock,
    AGVFStock,
    BGVFStock,
    BGSStock,
    FoliageNPP,
    RootNPP,
    BGVFPercentLoss,
    `Burial210Pb TC.g.m.2`,
    MethaneFlux,
    ExternalVariableValue,
    AGVFPercentLoss
  )

palSiteIDs <- siteSummaryLatFlux %>%
  filter(`Wetland Subclass LUCAS` == "Wetland: Palustrine Emergent") %>%
  select(Site.ID)

lateralFluxPal <- lateralFluxRaw %>%
  filter(`Wetland Subclass LUCAS` == "Palustrine Herbaceous") %>%
  select(Flux.g.C.per.m2.per.y) %>%
  rename(TotalLatTrans_Amount = Flux.g.C.per.m2.per.y)

lateralFluxPal <- lateralFluxPal %>%
  mutate(externalVariableLat = 1:nrow(lateralFluxPal)) %>%
  crossing(palSiteIDs)

lateralFluxPal <- lateralFluxPal %>%
  mutate(externalVariableLatSite = 1:nrow(lateralFluxPal))

estSiteIDs <- siteSummaryLatFlux %>%
  filter(`Wetland Subclass LUCAS` == "Wetland: Estuarine Emergent") %>%
  select(Site.ID)

lateralFluxEst <- lateralFluxRaw %>%
  filter(`Wetland Subclass LUCAS` == "Estuarine Herbaceous") %>%
  select(Flux.g.C.per.m2.per.y) %>%
  rename(TotalLatTrans_Amount = Flux.g.C.per.m2.per.y)

lateralFluxEst <- lateralFluxEst %>%
  mutate(externalVariableLat = 1:nrow(lateralFluxEst)) %>%
  crossing(estSiteIDs)

lateralFluxEst <- lateralFluxEst %>%
  mutate(externalVariableLatSite = 1:nrow(lateralFluxEst))

allCrossing <- rbind(lateralFluxPal, lateralFluxEst)

siteSummaryLatFlux <- siteSummaryLatFlux %>%
  full_join(
    allCrossing,
    by = "Site.ID",
    multiple = "all",
    relationship = "one-to-many"
  )

siteSummaryLatFlux <- funFluxCalc(SummaryTab = siteSummaryLatFlux)

#siteSummaryLatFlux %>% group_by(`Wetland Subclass LUCAS`) %>% summarise_all(list(min, max))

# Save site summary raw
siteSummaryLatFlux %>%
  mutate_if(is.numeric, round, digits = sig_figs) %>%
  write_csv(paste0(
    outDir,
    "siteSummaryLatFlux_StockBasedEquilibrium_",
    dateStamp,
    ".csv"
  ))

# Format site-level carbon parameters for STSim-SF
# Distributions for state attributes. Rename to match state attribute type. Separate initial conditions, npp, and sediment transport state attributes
# Initial Carbon - with external variable
initialCarbonExternalDistnLatFlux <- siteSummaryLatFlux %>%
  select(
    Site.ID,
    'StateClassID' = `Wetland Subclass LUCAS`,
    'ExternalVariableID' = externalVariableLatSite,
    'Carbon Initial Conditions: Foliage' = FoliageStock,
    'Carbon Initial Conditions: Fine Roots' = RootStock,
    'Carbon Initial Conditions: Aboveground Very Fast' = AGVFStock,
    'Carbon Initial Conditions: Belowground Very Fast' = BGVFStock,
    'Carbon Initial Conditions: Belowground Slow' = BGSStock
  ) %>%
  gather('Distribution', 'Value', 4:8) %>%
  mutate(
    DistributionTypeID = paste(StateClassID, Distribution),
    ExternalVariableTypeID = paste("Site ID", StateClassID),
    ExternalVariableMin = ExternalVariableID,
    ExternalVariableMax = ExternalVariableID,
    ValueDistributionRelativeFrequency = 1
  ) %>%
  select(
    DistributionTypeID,
    ExternalVariableTypeID,
    ExternalVariableMin,
    ExternalVariableMax,
    Value,
    ValueDistributionRelativeFrequency,
    StateClassID,
    Distribution
  )

# State Attribute Value Distributions
initialCarbonSite <- initialCarbonExternalDistnLatFlux %>%
  select(StateClassID, Distribution, DistributionTypeID) %>%
  rename(
    StateAttributeTypeID = Distribution,
    DistributionType = DistributionTypeID
  ) %>%
  distinct()

initialCarbonExternalDistnLatFlux <- initialCarbonExternalDistnLatFlux %>%
  select(
    DistributionTypeID,
    ExternalVariableTypeID,
    ExternalVariableMin,
    ExternalVariableMax,
    Value,
    ValueDistributionRelativeFrequency
  )

# Save csv
datasheetName <- "stsim_DistributionValue"

initialCarbonExternalDistnLatFlux %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " Initial C Wetland Emergent Site Lat.csv")
  ))

# NPP - with external variable
NPPExternalDistnLatFlux <- siteSummaryLatFlux %>%
  select(
    Site.ID,
    'StateClassID' = `Wetland Subclass LUCAS`,
    'ExternalVariableID' = externalVariableLatSite,
    'Value' = TotalNPP_Amount
  ) %>%
  mutate(
    DistributionTypeID = paste(StateClassID, "Net Growth"),
    ExternalVariableTypeID = paste("Site ID", StateClassID),
    ExternalVariableMin = ExternalVariableID,
    ExternalVariableMax = ExternalVariableID,
    ValueDistributionRelativeFrequency = 1
  ) %>%
  select(
    Site.ID,
    DistributionTypeID,
    ExternalVariableTypeID,
    ExternalVariableMin,
    ExternalVariableMax,
    Value,
    ValueDistributionRelativeFrequency,
    StateClassID
  )

# State Attribute Value Distributions
NPPSite <- ungroup(NPPExternalDistnLatFlux) %>%
  select(StateClassID, DistributionTypeID) %>%
  rename(DistributionType = DistributionTypeID) %>%
  mutate(StateAttributeTypeID = "Net Growth") %>%
  distinct()

NPPExternalDistnLatFlux <- NPPExternalDistnLatFlux %>%
  select(
    Site.ID,
    DistributionTypeID,
    ExternalVariableTypeID,
    ExternalVariableMin,
    ExternalVariableMax,
    Value,
    ValueDistributionRelativeFrequency
  )

# Save csv
datasheetName <- "stsim_DistributionValue"
NPPExternalDistnLatFlux %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " NPP Wetland Emergent Site Lat", ".csv")
  ))

# CH4 - with external variable
CH4ExternalDistnLatFlux <- siteSummaryLatFlux %>%
  select(
    Site.ID,
    'StateClassID' = `Wetland Subclass LUCAS`,
    'ExternalVariableID' = externalVariableLatSite,
    'Value' = MethaneFlux
  ) %>%
  mutate(
    DistributionTypeID = paste(StateClassID, "Methane Emissions"),
    ExternalVariableTypeID = paste("Site ID", StateClassID),
    ExternalVariableMin = ExternalVariableID,
    ExternalVariableMax = ExternalVariableID,
    ValueDistributionRelativeFrequency = 1
  ) %>%
  select(
    Site.ID,
    DistributionTypeID,
    ExternalVariableTypeID,
    ExternalVariableMin,
    ExternalVariableMax,
    Value,
    ValueDistributionRelativeFrequency
  )

# Save csv
datasheetName <- "stsim_DistributionValue"
CH4ExternalDistnLatFlux %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " CH4 Wetland Emergent Site Lat", ".csv")
  ))

# Distributions for flow multiplier values
# Rename to match flow type/group.
flowExternalDistnLatFlux <- siteSummaryLatFlux %>%
  select(
    Site.ID,
    'StateClassID' = `Wetland Subclass LUCAS`,
    'ExternalVariableID' = externalVariableLatSite,
    'Net Growth Wetland Emergent: Atmosphere -> Foliage' = FoliageGrowth_PropTotalNPP,
    'Net Growth Wetland Emergent: Atmosphere -> Fine Roots' = RootGrowth_PropTotalNPP,
    'Biomass Turnover: Foliage -> AG Very Fast' = FoliageMortality_PropMaxStock,
    'Biomass Turnover: Fine Roots -> BG Very Fast' = RootMortality_PropMaxStock,
    'Emission: AG Very Fast -> Atmosphere Temp' = AGVFEmission_PropMaxStock,
    'Emission: BG Very Fast -> Atmosphere Temp' = BGVFEmission_PropMaxStock,
    'Emission: BG Slow -> Atmosphere Temp' = BGSEmission_PropMaxStock,
    'Lateral Transport: AG Very Fast -> Aquatic' = AGVFLatTrans_PropMaxStock,
    'Lateral Transport: BG Very Fast -> Aquatic' = BGVFLatTrans_PropMaxStock,
    'Lateral Transport: BG Slow -> Aquatic' = BGSLatTrans_PropMaxStock,
    'Decay: BG Very Fast -> BG Slow' = BGVFStabilization_PropMaxStock,
    'Decay: AG Very Fast -> BG Slow' = AGVFStabilization_PropMaxStock,
    'Stabilization: BG Slow -> Deep Soil' = BGSStabilization_PropMaxStock
  ) %>%
  gather('Distribution', 'Value', 4:16) %>%
  mutate(
    DistributionTypeID = paste(StateClassID, Distribution),
    ExternalVariableTypeID = paste("Site ID", StateClassID),
    ExternalVariableMin = ExternalVariableID,
    ExternalVariableMax = ExternalVariableID,
    ValueDistributionRelativeFrequency = 1
  ) %>%
  select(
    DistributionTypeID,
    ExternalVariableTypeID,
    ExternalVariableMin,
    ExternalVariableMax,
    Value,
    ValueDistributionRelativeFrequency,
    StateClassID,
    Distribution
  )

# Flow Multiplier Value Distributions
flowMultiplierValueSite <- flowExternalDistnLatFlux %>%
  select(StateClassID, Distribution, DistributionTypeID) %>%
  rename(FlowGroupID = Distribution, DistributionType = DistributionTypeID) %>%
  distinct()

flowExternalDistnLatFlux <- flowExternalDistnLatFlux %>%
  select(
    DistributionTypeID,
    ExternalVariableTypeID,
    ExternalVariableMin,
    ExternalVariableMax,
    Value,
    ValueDistributionRelativeFrequency
  )

# Save csv
datasheetName <- "stsim_DistributionValue"
flowExternalDistnLatFlux %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(
      datasheetName,
      " Flow Multipliers Wetland Emergent Site Lat IPCC.csv"
    )
  ))

# Save csv
datasheetName <- "stsim_StateAttributeValue"
initialCarbonSite %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " Initial C Wetland Emergent Site.csv")
  ))

NPPSite %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " NPP Wetland Emergent Site.csv")
  ))

# Save csv
datasheetName <- "stsimsf_FlowMultiplier"
flowMultiplierValueSite %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " Wetland Emergent Site.csv")
  ))

# Wetland subclass level summary -------------------------------------------------------------------
#Mean Lateral Transport Est and Pal g C m-2 yr-1

subclassSummary <- ungroup(siteSummary0) %>%
  select(
    `Wetland Subclass LUCAS`,
    FoliageStock,
    RootStock,
    AGVFStock,
    BGVFStock,
    BGSStock,
    FoliageNPP,
    RootNPP,
    BGVFPercentLoss,
    `Burial210Pb TC.g.m.2`,
    TotalLatTrans_Amount,
    MethaneFlux,
    AGVFPercentLoss
  ) %>%
  group_by(`Wetland Subclass LUCAS`) %>%
  summarize_all(mean, na.rm = TRUE)

subclassSummary <- funFluxCalc(SummaryTab = subclassSummary)

# Save wetland subclass summaries
subclassSummary %>%
  mutate_if(is.numeric, round, digits = sig_figs) %>%
  write_csv(paste0(
    outDir,
    "subclassSummary_StockBasedEquilibrium_",
    dateStamp,
    ".csv"
  ))

# State attribute values Wetland Herbaceous Mean
# Rename to match state attribute type. Separate initial conditions, npp, and sediment transport state attributes
initialCarbonMean <- subclassSummary %>%
  select(
    'StateClassID' = `Wetland Subclass LUCAS`,
    'Carbon Initial Conditions: Foliage' = FoliageStock,
    'Carbon Initial Conditions: Fine Roots' = RootStock,
    'Carbon Initial Conditions: Aboveground Very Fast' = AGVFStock,
    'Carbon Initial Conditions: Belowground Very Fast' = BGVFStock,
    'Carbon Initial Conditions: Belowground Slow' = BGSStock
  ) %>%
  gather('StateAttributeTypeID', 'Value', 2:6)

NPPMean <- subclassSummary %>%
  select(
    'StateClassID' = `Wetland Subclass LUCAS`,
    'Net Growth' = TotalNPP_Amount
  ) %>%
  gather('StateAttributeTypeID', 'Value', 2)

MethaneMean <- subclassSummary %>%
  select(
    'StateClassID' = `Wetland Subclass LUCAS`,
    'Methane Emissions' = MethaneFlux
  ) %>%
  gather('StateAttributeTypeID', 'Value', 2)

# Save csv
datasheetName <- "stsim_StateAttributeValue"
initialCarbonMean %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " Initial C Wetland Emergent Mean.csv")
  ))

NPPMean %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " NPP Wetland Emergent Mean.csv")
  ))

MethaneMean %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " CH4 Wetland Emergent Mean.csv")
  ))

# Flow multiplier values Wetland Herbaceous Mean Based on Max Stock and not Prop Out
# Rename to match flow type/group.
flowMultiplierValueMeanMaxStock <- subclassSummary %>%
  select(
    'StateClassID' = `Wetland Subclass LUCAS`,
    'Net Growth Wetland Emergent: Atmosphere -> Foliage' = FoliageGrowth_PropTotalNPP,
    'Net Growth Wetland Emergent: Atmosphere -> Fine Roots' = RootGrowth_PropTotalNPP,
    'Biomass Turnover: Foliage -> AG Very Fast' = FoliageMortality_PropMaxStock,
    'Biomass Turnover: Fine Roots -> BG Very Fast' = RootMortality_PropMaxStock,
    'Emission: AG Very Fast -> Atmosphere Temp' = AGVFEmission_PropMaxStock,
    'Emission: BG Very Fast -> Atmosphere Temp' = BGVFEmission_PropMaxStock,
    'Emission: BG Slow -> Atmosphere Temp' = BGSEmission_PropMaxStock,
    'Lateral Transport: AG Very Fast -> Aquatic' = AGVFLatTrans_PropMaxStock,
    'Lateral Transport: BG Very Fast -> Aquatic' = BGVFLatTrans_PropMaxStock,
    'Lateral Transport: BG Slow -> Aquatic' = BGSLatTrans_PropMaxStock,
    'Decay: BG Very Fast -> BG Slow' = BGVFStabilization_PropMaxStock,
    'Decay: AG Very Fast -> BG Slow' = AGVFStabilization_PropMaxStock,
    'Stabilization: BG Slow -> Deep Soil' = BGSStabilization_PropMaxStock
  ) %>%
  gather('FlowGroupID', 'Value', 2:14)

# Save csv
datasheetName <- "stsimsf_FlowMultiplier"
flowMultiplierValueMeanMaxStock %>%
  #mutate_if(is.numeric, round, digits=sig_figs) %>%
  write_csv(file.path(
    paste(subscenariosDir, datasheetName, sep = "/"),
    paste0(datasheetName, " Wetland Emergent Mean IPCC.csv")
  ))
