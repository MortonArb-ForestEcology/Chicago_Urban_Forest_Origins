# --------------------------------------------------------------
# Purpose: Extracting tree cover data by census tract to make comparisons easier
# Author: Christy Rollinson, crollinson@mortonarb.org
# Date: 16 May 2017
# --------------------------------------------------------------
#
# Steps
# 1. Read in datalayers of interest & transform into common projection
# 2. Calculate percent tree cover by census tract:
#    A. pre-settlement forest
#    B. Remnant Forest 1939
#    C. Remnant Forest 2010
#    B. Modern land cover (will be slow & tricky; see bottom of exploratory.R)
#       - inside of loop:
#         1. crop landcover to census tract
#         2. mask landcover to census tract
#         3. calculate the fraction of cells in each LU class (using ncell seems fastest)
#         4. delete temporary files that keep you from having to store everything in memory
# 3. Save census tract SpatialPolygonDataFrame as shapefile that can be shared (then do analysis in other script)
#
# --------------------------------------------------------------
rm(list=ls())

# --------------------------------------------------------------
# Loading libraries, setting directories
# --------------------------------------------------------------
library(rgeos); library(rgdal)
library(raster)
library(parallel); library(tictoc)
library(ggplot2)

# Need to be in the data folder for the geodatabase to read properly
setwd("~/Desktop/Research/Chicago_Urban_Forest_Origins/data/")
# --------------------------------------------------------------

# --------------------------------------------------------------
# Loading in data layers, moving to a common projection
# Note: Needs to be the projection of land cover because that's a raster & harder to transform
# --------------------------------------------------------------
fgdb <- "OriginsData.gdb" # Note: file paths do NOT work --> must be in the directory you're trying to pull it from

# List all feature classes in a file geodatabase
ogrListLayers(fgdb)[1:7]

# Read the feature classes from the geodatabase
forest.1850  <- readOGR(fgdb, "PresettlementForest")
remnant.1939 <- readOGR(fgdb, "Forests1939")
remnant.2010 <- readOGR(fgdb, "Forests2010")
income       <- readOGR(fgdb, "HistoricIncome")
census       <- readOGR(fgdb, "CurrentCensus")

# Opening a connection to the Land Cover raster
landcover <- raster("landcover_2010_chicagoregion_illinois/landcover_2010_chicagoregion_illinois.img")
landcover

projection(landcover)
forest.1850  <- spTransform(forest.1850 , projection(landcover))
remnant.1939 <- spTransform(remnant.1939, projection(landcover))
remnant.2010 <- spTransform(remnant.2010, projection(landcover))
income       <- spTransform(income      , projection(landcover))
census       <- spTransform(census      , projection(landcover))
# --------------------------------------------------------------

# --------------------------------------------------------------
# 2. Extracting variables by census tract
# --------------------------------------------------------------
# -----------------------------------
# 2.A. Extracting Landcover by census tract
# Assuming Land Cover categories are in order:
# 1 = tree canopy
# 2 = grass + shrubs
# 3 = bare soil
# 4 = water
# 5 = buildings
# 6 = transportation (roads/rail)
# 7 = other paved
# -----------------------------------
# A function to extract landcover data for an individual tract; has been optimized to avoid certain slow processes
extract.lc <- function(tract){
  # tract = spatial polygon data frame for single census tract
  # census = data frame or spatial polygon data frame for all census data
  
  # Extracting landcover
  lc.temp <- crop(landcover, tract, filename=paste0("landcover_temp_", tract$OBJECTID), overwrite=T)
  lc.mask <- rasterize(tract, lc.temp, mask=T, filename=paste0("landcover_temp_", tract$OBJECTID, "_mask"), overwrite=T)
  
  lc.temp <- getValues(lc.mask)
  lc.temp <- lc.temp[!is.na(lc.temp)]
  # summary(lc.temp)
  
  tot.cell         <- length(lc.temp) # make sure to skip empty cells
  tract$tree       <- length(lc.temp[lc.temp==1])/tot.cell
  tract$veg_other  <- length(lc.temp[lc.temp==2])/tot.cell
  tract$soil_bare  <- length(lc.temp[lc.temp==3])/tot.cell
  tract$water      <- length(lc.temp[lc.temp==4])/tot.cell
  tract$building   <- length(lc.temp[lc.temp==5])/tot.cell
  tract$transport  <- length(lc.temp[lc.temp==6])/tot.cell
  tract$paved_misc <- length(lc.temp[lc.temp==7])/tot.cell
  
  files.rm <- paste0("rm -f landcover_temp_", tract$OBJECTID, "*")
  system(files.rm)
  
  tract.area <- area(tract)
  tract.1850 <- intersect(forest.1850 , tract)
  tract.1939 <- intersect(remnant.1939, tract)
  tract.2010 <- intersect(remnant.2010, tract)
  
  tract$forest_1850  <- ifelse(!is.null(tract.1850), sum(area(tract.1850))/tract.area, 0)
  tract$remnant_1939 <- ifelse(!is.null(tract.1939), sum(area(tract.1939))/tract.area, 0)
  tract$remnant_2010 <- ifelse(!is.null(tract.2010), sum(area(tract.2010))/tract.area, 0)

  return(tract)
  # return(census)
}

# Setting up some new columns in census
census$tree       <- NA
census$veg_other  <- NA
census$soil_bare  <- NA
census$water      <- NA
census$building   <- NA
census$transport  <- NA
census$paved_misc <- NA
census$forest_1850  <- NA
census$remnant_1939 <- NA
census$remnant_2010 <- NA


# Setting up a list of census tracts
tract.list <- list()
for(i in 1:nrow(census)){
  tract.list[[i]] <- census[i,]
}

tic()
tract.list <- mclapply(tract.list, extract.lc, mc.cores=6)
toc()


for(i in 1:length(tract.list)){
  census[i,] <- data.frame(tract.list[[i]])
}
summary(census)

# Save the data as a spatial polygon data frame
out.folder <- "ChicagoRegion_Census_Forests"
dir.create(out.folder, recursive=T)

# Note: writeOGR will abreviate variable names, so also saving a .csv for various reasons 
writeOGR(census, "ChicagoRegion_Census_Forests", "ChicagoRegion_Census_Forests", driver="ESRI Shapefile", overwrite_layer=T)
write.csv(data.frame(census), "ChicagoRegion_Census_Forests.csv", row.names=F)
# --------------------------------------------------------------


