# ApexRMS
# Updated 2025-02-04
# Run after step5-SingleCellPlots.R
# This script pre-processes spatial data
# Download spatial data and update file paths
# CCAP (2001-2016)
# LandFire EVT (2016 & 2001)
# NASA CMS stand age (https://doi.org/10.3334/ORNLDAAC/1829)
# PRISM climate data (2000-2022)
# Adapted from Ben Sleeter's Build LUCAS Model.rmd script

library(tidyverse)
library(sf)
library(terra)

# Specify file paths
#sourceDataPath <- "D:/Barataria/Data Sources/"
sourceDataPath <- "~/A379/Data Sources/"
studyAreaFullPath <- paste0(rootPath, "StudyArea/BasinsCoastal.shp")

applyCoarserGrid <- FALSE
spatialResMult <- 3 #10 # Only used when applyCoarserGrid == TRUE

dataPath <- "Data/"
modelPath <- "Models/"

#modelName <- "Barataria"

modelFullPath <- paste0(rootPath, modelPath, modelName)

# Full path name to study area shapefile
studyAreaShape <- read_sf(studyAreaFullPath)

studyAreaShape <- studyAreaShape[studyAreaShape$UNIT == "Barataria", ]

#studyAreaShape <- studyAreaShape[studyAreaShape$UNIT == "Breton Sound",]

# Name of column in shapefile of study area that specifies a primary strata.
studyAreaShape$primaryStrata <- "Study Area"
studyAreaShape$primaryStrataId <- 1

# Specify the spatial reference
projectionCRS <- crs("EPSG:5070")

## Creates a raster from the analysis area shapefile specified in the InputParameters section

# This reprojects the shapefile back to the projection specified in the inputParameters section.
studyAreaShape <- st_transform(studyAreaShape, crs = projectionCRS)

# ApexRMS changed to primaryStrata to generalize these column names
primaryStrataName <- unique(studyAreaShape$primaryStrata)

# Creates the strata rasters used in the model
s <- studyAreaShape
v <- vect(s)
r <- rast(v, res = 30)

# ApexRMS changed to primaryStrataId
primaryStrataRaster <- rasterize(v, r, "primaryStrataId")

filePaths <- c(
  paste0(modelFullPath, "/Data"),
  paste0(modelFullPath, "/Data/Boundary"),
  paste0(modelFullPath, "/Data/Initial Conditions"),
  paste0(modelFullPath, "/Data/Initial Stocks"),
  paste0(modelFullPath, "/Data/Transition Spatial Multipliers"),
  paste0(modelFullPath, "/Data/Flow Spatial Multipliers"),
  paste0(modelFullPath, "/Data/Flow Spatial Multipliers/Growth Forest"),
  paste0(modelFullPath, "/Data/Flow Spatial Multipliers/Growth Non Forest"),
  paste0(modelFullPath, "/Data/Flow Spatial Multipliers/Q10 Fast"),
  paste0(modelFullPath, "/Data/Flow Spatial Multipliers/Q10 Slow"),
  paste0(modelFullPath, "/Data/Initial Conditions/CCAP"),
  paste0(modelFullPath, "/Data/Initial Conditions/LUCAS"),
  paste0(modelFullPath, "/Images"),
  paste0(modelFullPath, "/Data/State Attributes"),
  paste0(modelFullPath, "/Data/Initial Stocks/Harvest")
)

for (i in 1:length(filePaths)) {
  if (!dir.exists(filePaths[i])) {
    dir.create(filePaths[i])
  }
}

## Write study area rasters to disk
#writeRaster(primaryStrataRaster, paste0(modelFullPath, "/Data/Initial Conditions/PrimaryStrata.tif"), overwrite=T)

write_sf(s, paste0(modelFullPath, "/Data/Boundary/analysis-area.shp"))

## CCAP Time series
ccapPath <- paste0(sourceDataPath, "CCAP All/")

ccap2001 <- rast(paste0(ccapPath, "conus_2001_ccap_landcover_20200311.tif")) %>%
  crop(primaryStrataRaster) %>%
  project(primaryStrataRaster, "near") %>%
  mask(primaryStrataRaster)
#activeCat(ccap2001) <- "value"

# Update primaryStrataRaster so if CCAP is background 0, then value is NA
primaryStrataRaster <- ifel(ccap2001 == 0, NA, primaryStrataRaster)

if (applyCoarserGrid == TRUE) {
  # scale up to 900m for testing
  studyAreaMask <- aggregate(
    primaryStrataRaster,
    fact = spatialResMult,
    fun = "modal",
    na.rm = TRUE
  )
} else {
  studyAreaMask <- primaryStrataRaster
}

## Write study area rasters to disk
writeRaster(
  studyAreaMask,
  paste0(modelFullPath, "/Data/Initial Conditions/PrimaryStrata.tif"),
  overwrite = T
)

ccap2001 <- rast(paste0(ccapPath, "conus_2001_ccap_landcover_20200311.tif")) %>%
  crop(primaryStrataRaster) %>%
  project(primaryStrataRaster, "near") %>%
  mask(primaryStrataRaster)

if (applyCoarserGrid == TRUE) {
  ccap2001 <- aggregate(
    ccap2001,
    fact = spatialResMult,
    fun = "modal",
    na.rm = TRUE
  )

  ccap2001 <- ifel(is.na(studyAreaMask), NA, ccap2001)
}

ccap2006 <- rast(paste0(ccapPath, "conus_2006_ccap_landcover_20200311.tif")) %>%
  crop(primaryStrataRaster) %>%
  project(primaryStrataRaster, "near") %>%
  mask(primaryStrataRaster)

if (applyCoarserGrid == TRUE) {
  ccap2006 <- aggregate(
    ccap2006,
    fact = spatialResMult,
    fun = "modal",
    na.rm = TRUE
  )

  ccap2006 <- ifel(is.na(studyAreaMask), NA, ccap2006)
}

ccap2010 <- rast(paste0(ccapPath, "conus_2010_ccap_landcover_20200311.tif")) %>%
  crop(primaryStrataRaster) %>%
  project(primaryStrataRaster, "near") %>%
  mask(primaryStrataRaster)

if (applyCoarserGrid == TRUE) {
  ccap2010 <- aggregate(
    ccap2010,
    fact = spatialResMult,
    fun = "modal",
    na.rm = TRUE
  )

  ccap2010 <- ifel(is.na(studyAreaMask), NA, ccap2010)
}

ccap2016 <- rast(paste0(ccapPath, "conus_2016_ccap_landcover_20200311.tif")) %>%
  crop(primaryStrataRaster) %>%
  project(primaryStrataRaster, "near") %>%
  mask(primaryStrataRaster)

if (applyCoarserGrid == TRUE) {
  ccap2016 <- aggregate(
    ccap2016,
    fact = spatialResMult,
    fun = "modal",
    na.rm = TRUE
  )

  ccap2016 <- ifel(is.na(studyAreaMask), NA, ccap2016)
}

# CCAP Stack
ccapStack <- c(ccap2001, ccap2006, ccap2010, ccap2016)
names(ccapStack) <- c(paste0("CCAP_", c(2001, 2006, 2010, 2016)))

# Plot CCAP figure to disk
png(
  file = paste0(modelFullPath, "/Images", "/CCAP.png"),
  width = 800,
  height = 800
)
plot(ccapStack)
dev.off()

# Write NLCD data to disk
writeRaster(
  ccapStack,
  paste0(
    modelFullPath,
    "/Data/Initial Conditions/CCAP/",
    names(ccapStack),
    ".tif"
  ),
  overwrite = T
)

fTable <- freq(ccapStack)
sum(fTable$count)
sum(fTable$count[fTable$value == 16])
sum(fTable$count[fTable$value == 17])


## Download LandFire EVT 2016 and 2001
# Note that the 2001 EVT will be used where 2016 EVT was classified as Recently Disturbed, Recently Logged, and Recently Burned. Landfire EVT will be used to assign a forest type-group to each of the NLCD forest cells. This script assumes 2016 EVT is used except in areas classified as recently disturbed, where 2001 EVT is used.

evtPath <- paste0(sourceDataPath, "EVT")

# Read in crosswalk tables
evt2016Crosswalk <- read_csv(paste0(evtPath, "/evt2016-crosswalk-CONUS-v2.csv"))
evt2001Crosswalk <- read_csv(paste0(evtPath, "/evt2001-crosswalk-CONUS-v2.csv"))

# Read in Spatial Data
evt2001CONUS <- rast(paste0(
  evtPath,
  "/US_105_EVT/US_105_EVT/Tif/us_105evt.tif"
))
evt2016CONUS <- rast(paste0(
  evtPath,
  "/LF2016_EVT_200_CONUS/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif"
))

# Crop EVT rasters to test extent
evt2001Crop <- evt2001CONUS %>%
  crop(y = primaryStrataRaster) %>%
  project(primaryStrataRaster, "near") %>%
  mask(primaryStrataRaster)

evt2016Crop <- evt2016CONUS %>%
  crop(y = primaryStrataRaster) %>%
  project(primaryStrataRaster, "near") %>%
  mask(primaryStrataRaster)

# Identify recently disturbed EVT values
recentlyDisturbedEvt <- evt2016Crosswalk %>%
  filter(str_detect(EVT_NAME, "Recently")) %>%
  pull(VALUE) %>%
  as.integer()

# Construct joint reclassification lookup
rcl <-
  bind_rows(
    evt2016Crosswalk %>% dplyr::select(from = VALUE, to = ID),
    evt2001Crosswalk %>% dplyr::select(from = Value, to = ID)
  ) %>%
  filter(
    !from %in% recentlyDisturbedEvt,
    from != -9999
  ) %>% # This value is represented in both, must be re-added manually
  bind_rows(
    # Manually add NA behavior
    tibble(from = c(-9999), to = c(NA))
  ) %>%
  mutate_all(as.integer)

# Set recently disturbed cell values in EVT 2016 to NA
evt2016Crop <- evt2016Crop %>%
  values() %>%
  setValues(evt2016Crop, .) %>%
  subst(from = recentlyDisturbedEvt, to = NA)

# Fill NA cells with EVT 2001 values
evtCombined <- terra::merge(evt2016Crop, evt2001Crop)

# Reclassify EVT 2016
forestTypeGroup <- evtCombined %>%
  classify(rcl = rcl)

if (applyCoarserGrid == TRUE) {
  forestTypeGroup <- aggregate(
    forestTypeGroup,
    fact = spatialResMult,
    fun = "modal",
    na.rm = TRUE
  )

  forestTypeGroup <- ifel(is.na(studyAreaMask), NA, forestTypeGroup)
}

# Fill non-forest cells with nearest forest type group
# Only in areas where forests exist across the study area

forestStack <- sum(ifel(ccapStack %in% c(9, 10, 11), 1, 0))
forestStack <- ifel(forestStack > 0, 1, 0)

forestTypeGroupFilled <- ifel(forestTypeGroup < 100, NA, forestTypeGroup)

forestTypeGroupNAs <- ifel(
  (is.na(forestTypeGroupFilled) &
    forestStack == 1),
  NA,
  1
)

naCount <- freq(forestTypeGroupNAs, value = NA)

# Added by ApexRMS to avoid infinite while loop, forestTypeGroupFilled set to most frequent EVT across CONUS: 500 Forest: Oak/Hickory Group
# Only in cases where forestTypeGroup is always non-forest (less than 100)
if (naCount$count == ncell(forestTypeGroupFilled)) {
  forestTypeGroupFilled <- forestTypeGroupFilled %>% setValues(values = 500)
} else {
  while (naCount$count > 0) {
    forestTypeGroupFilled <- terra::focal(
      forestTypeGroupFilled,
      9,
      "modal",
      na.policy = "only"
    )
    forestTypeGroupNAs <- ifel(
      (is.na(forestTypeGroupFilled) &
        forestStack == 1),
      NA,
      1
    )
    naCount = terra::freq(forestTypeGroupNAs, value = NA)
  }
}

naCount <- freq(forestTypeGroupNAs, value = NA)

forestTypeGroupFilled <- forestTypeGroupFilled %>%
  mask(studyAreaMask)

plot(forestTypeGroupFilled)

## Build All State Class Rasters

#verify no 0's or 1's if so set to NA (same NA structure for all, check?)
freq(ccap2001)
freq(ccap2006)
freq(ccap2010)
freq(ccap2016)

# Check reclass table from LA model, align?

reclassTable <- data.frame(
  from = c(0:25),
  to = c(
    NA,
    NA,
    24,
    23,
    22,
    21,
    82,
    81,
    71,
    600,
    600,
    600,
    54,
    90,
    90,
    95,
    96,
    96,
    96,
    97,
    31,
    11,
    11,
    11,
    31,
    12
  )
)

lucas2001 <- classify(ccap2001, reclassTable)
lucas2001 <- ifel(lucas2001 == 600, forestTypeGroupFilled, lucas2001)

lucas2006 <- classify(ccap2006, reclassTable)
lucas2006 <- ifel(lucas2006 == 600, forestTypeGroupFilled, lucas2006)

lucas2010 <- classify(ccap2010, reclassTable)
lucas2010 <- ifel(lucas2010 == 600, forestTypeGroupFilled, lucas2010)

lucas2016 <- classify(ccap2016, reclassTable)
lucas2016 <- ifel(lucas2016 == 600, forestTypeGroupFilled, lucas2016)

# LUCAS Stack
lucasStack <- c(lucas2001, lucas2006, lucas2010, lucas2016)
names(lucasStack) <- c(paste0("LUCAS_", c(2001, 2006, 2010, 2016)))


# Plot LUCAS figure to disk
png(
  file = paste0(modelFullPath, "/Images", "/LUCAS.png"),
  width = 800,
  height = 800
)
plot(lucasStack)
dev.off()

# Write NLCD data to disk
writeRaster(
  lucasStack,
  paste0(
    modelFullPath,
    "/Data/Initial Conditions/LUCAS/",
    names(lucasStack),
    ".tif"
  ),
  overwrite = T
)

freq(lucas2001)

## NASA CMS Stand Age
# NASA CMS stand age is available from the following report and can be downloaded through the Oak Ridge National Lab DAAC.
# Williams, C.A., N. Hasler, H. Gu, and Y. Zhou. 2020. Forest Carbon Stocks and Fluxes from the NFCMS, Conterminous USA, 1990-2010. ORNL DAAC, Oak Ridge, Tennessee, USA. <https://doi.org/10.3334/ORNLDAAC/1829>
#  Stand Age data are available in regional tiles at 30-meter spatial resolution. Files correspond to the years 1990, 2000, and 2010. For this work, we utilize the 2000 date to initialize stand age.

standAge <- rast(paste0(sourceDataPath, "Stand Age/cms_age_2000.tif")) # ApexRMS removed rootpath
standAge <- crop(standAge, primaryStrataRaster)
standAge <- terra::project(standAge, primaryStrataRaster, method = "near")
standAge <- mask(standAge, primaryStrataRaster)
plot(standAge)

if (applyCoarserGrid == TRUE) {
  standAge <- aggregate(
    standAge,
    fact = spatialResMult,
    fun = "mean",
    na.rm = TRUE
  )

  standAge <- ifel(is.na(studyAreaMask), NA, standAge)
}

# Fill non-forest cells with nearest stand age
standAgeFilled <- ifel(standAge == 0, NA, standAge)

forestWetlandStack <- ifel(ccap2001 %in% c(9, 10, 11, 13, 14), 1, 0)

standAgeNAs <- ifel(
  (is.na(standAgeFilled) &
    forestWetlandStack == 1),
  NA,
  1
)

naCount <- freq(standAgeNAs, value = NA)

# Added by ApexRMS to avoid infinite while loop
# standAgeFilled is set to 0, if standAgeFilled is 0 everywhere and there is no forest or forested wetland in the study area in 2001
# standAgeFilled is set to mean value across conus 63, if standAgeFilled is 0 everywhere and there is forest or forested wetland in the study area in 2001. Consider using median, max is very high: 17653.
# This may slow down our script, if run across conus, and would not be necessary at large scales?

if (
  all(
    naCount$count == ncell(standAgeFilled),
    minmax(forestWetlandStack)[2] == 1
  )
) {
  standAgeFilled <- standAgeFilled %>% setValues(values = 63)
} else if (
  all(
    naCount$count == ncell(standAgeFilled),
    minmax(forestWetlandStack)[2] == 0
  )
) {
  standAgeFilled <- standAgeFilled %>% setValues(values = 0)
} else {
  while (naCount$count > 0) {
    standAgeFilled <- terra::focal(
      standAgeFilled,
      9,
      "modal",
      na.policy = "only"
    )
    standAgeNAs <- ifel(
      (is.na(standAgeFilled) &
        forestWetlandStack == 1),
      NA,
      1
    )
    naCount = terra::freq(standAgeNAs, value = NA)
  }
}

naCount <- freq(standAgeNAs, value = NA)
plot(standAgeFilled)
freq(standAgeFilled)
hist(standAgeFilled)

# Create Stand Age based on forest and forested wetland cells
standAgeRaster <- ifel(
  lucas2001 >= 100 | lucas2001 == 90 | lucas2001 == 91,
  standAgeFilled,
  0
)

plot(standAgeRaster)

standAgeRaster <- standAgeRaster %>%
  mask(studyAreaMask)

plot(standAgeRaster)

## Write State Class and Stand Age Initial Conditions to Disk
terra::writeRaster(
  lucas2001,
  paste0(modelFullPath, "/Data/Initial Conditions/", "State Class", ".tif"),
  overwrite = T
)
terra::writeRaster(
  standAgeRaster,
  paste0(modelFullPath, "/Data/Initial Conditions/", "Stand Age", ".tif"),
  overwrite = T
)

## Flow Spatial Multipliers
#This section requires downloading or acquiring historical climate data which is processed to create spatial flow multipliers for NPP and DOM respiration. For this script we use PRISM historical climate data for mean annual temperature and total annual precipitation.

#PRISM data can be downloaded here: <https://prism.oregonstate.edu/recent/>

#studyAreaMask <- primaryStrataRaster

### Define folders and directories
inputClimDataPrism <- paste0(sourceDataPath, "Climate/prism/annual/")
inputClimDataPrismNormals <- paste0(sourceDataPath, "Climate/prism/normals/")

### Get list of mean annual temperature and total annual precip rasters and then create raster stacks for each
tempList <- list.files(
  paste0(inputClimDataPrism, "tmean/"),
  pattern = "*bil.bil$",
  recursive = T
)
precipList <- list.files(
  paste0(inputClimDataPrism, "precip/"),
  pattern = "*bil.bil$",
  recursive = T
)

tempStack <- rast(paste0(inputClimDataPrism, "tmean/", tempList))
precipStack <- rast(paste0(inputClimDataPrism, "precip/", precipList))

### Calculate fMAP and fMAT for the raster stacks for forest
fMatForest <- 2540 / (1 + exp(1.584 - 0.0622 * tempStack))
fMapForest <- (0.551 * precipStack^1.055) / exp(0.000306 * precipStack)

### Calculate Forest and Non-Forest NPP
# Forest NPP
forestNpp <- ifel(fMapForest < fMatForest, fMapForest, fMatForest)
names(forestNpp) <- paste0("forestNpp_", seq(2000, 2022))

# Non Forest NPP
nonForestNpp <- 6116 * (1 - exp(-6.05 * (10^-5) * precipStack))
names(nonForestNpp) <- paste0("nonForestNpp_", seq(2000, 2022))

### Read in the 30-year climate normals and calculate NPP for froest and Non-Forest
tempNormal <- rast(paste0(
  inputClimDataPrismNormals,
  "/PRISM_tmean_30yr_normal_4kmM4_annual_bil/PRISM_tmean_30yr_normal_4kmM4_annual_bil.bil"
))
precipNormal <- rast(paste0(
  inputClimDataPrismNormals,
  "/PRISM_ppt_30yr_normal_4kmM4_annual_bil/PRISM_ppt_30yr_normal_4kmM4_annual_bil.bil"
))

fMatForestNormal <- 2540 / (1 + exp(1.584 - 0.0622 * tempNormal))
fMapForestNormal <- (0.551 * precipNormal^1.055) / exp(0.000306 * precipNormal)

forestNppNormal <- ifel(
  fMapForestNormal < fMatForestNormal,
  fMapForestNormal,
  fMatForestNormal
)
nonForestNppNormal <- 6116 * (1 - exp(-6.05 * (10^-5) * precipNormal))

### Calculate the NPP Anomoly
forestNppAnomoly <- forestNpp / forestNppNormal
names(forestNppAnomoly) <- paste0("forestNppAnom_", seq(2000, 2022))

nonForestAnomoly <- nonForestNpp / nonForestNppNormal
names(nonForestAnomoly) <- paste0("nonForestNppAnom_", seq(2000, 2022))

### Clip and reproject to Study Area mask and extent
# Forest
forestNppMult <- terra::project(
  forestNppAnomoly,
  studyAreaMask,
  method = "near"
)
forestNppMult <- mask(forestNppMult, studyAreaMask)
names(forestNppMult) <- paste0("forestNppMult_", seq(2000, 2022))

# Non Forest
nonforestNppMult <- terra::project(
  nonForestAnomoly,
  studyAreaMask,
  method = "near"
)
nonforestNppMult <- mask(nonforestNppMult, studyAreaMask)
names(nonforestNppMult) <- paste0("nonForestNppMult_", seq(2000, 2022))

# Plot the 2020 Anomoly
plot(forestNppMult$forestNppMult_2022)
plot(nonforestNppMult$nonForestNppMult_2022)

### Calculate the Q10 decomposition multipliers

q10FastRate <- 2.65
q10SlowRate <- 2.00

# Calculate the decomposition multipliers
q10Fast <- (1 * q10FastRate^((tempStack - tempNormal) / 10))
q10Slow <- (1 * q10SlowRate^((tempStack - tempNormal) / 10))

# Project and mask to study area
q10Fast <- terra::project(q10Fast, studyAreaMask, method = "near")
q10Fast <- mask(q10Fast, studyAreaMask)
names(q10Fast) <- paste0("q10Fast_", seq(2000, 2022))

q10Slow <- terra::project(q10Slow, studyAreaMask, method = "near")
q10Slow <- mask(q10Slow, studyAreaMask)
names(q10Slow) <- paste0("q10Slow_", seq(2000, 2022))

plot(q10Fast$q10Fast_2022)

### Convert multipliers to Integers and Write to Disk
forestNppMultInt <- forestNppMult * 100
nonforestNppMultInt <- nonforestNppMult * 100
q10FastInt <- q10Fast * 100
q10SlowInt <- q10Slow * 100

### Write ratser multipliers to disk
outDir <- paste0(modelFullPath, "/Data/Flow Spatial Multipliers/")
writeRaster(
  forestNppMultInt,
  filename = paste0(outDir, "Growth Forest/", names(forestNppMultInt), ".tif"),
  datatype = "INT2U",
  overwrite = T
)
writeRaster(
  nonforestNppMultInt,
  filename = paste0(
    outDir,
    "Growth Non Forest/",
    names(nonforestNppMult),
    ".tif"
  ),
  datatype = "INT2U",
  overwrite = T
)
writeRaster(
  q10FastInt,
  filename = paste0(outDir, "Q10 Fast/", names(q10FastInt), ".tif"),
  datatype = "INT2U",
  overwrite = T
)
writeRaster(
  q10SlowInt,
  filename = paste0(outDir, "Q10 Slow/", names(q10SlowInt), ".tif"),
  datatype = "INT2U",
  overwrite = T
)
