# --------------------------------------------------------------
# Purpose: 
# Author: Christy Rollinson, crollinson@mortonarb.org
# Date: 10 October 2017
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


census <- readOGR("Census_Block_Canopy/CensusBlockGroupPIncCanopyRemnant.shp")

summary(census)

census$id <- as.factor(0:(nrow(census)-1))
census.fort <- fortify(census)
census.fort$id <- as.factor(census.fort$id)
census.fort <- merge(census.fort, census)
summary(census.fort)

ggplot(data=census.fort[census.fort$HOUSINGDEN>0,]) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=PCANCURREN)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover") + coord_equal()

ggplot(data=census.fort[census.fort$PINCORP>.9 & census.fort$HOUSINGDEN>0,]) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=PCANCURREN)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover") + coord_equal()

ggplot(data=census.fort[,]) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=PINCORP))  + coord_equal()

hist(census$PINCORP)

length(census[census$PINCORP>=.9,"PINCORP"])/length(census$PINCORP)


ggplot(data=census.fort[census.fort$HOUSINGDEN>0,]) +
  geom_polygon(data=census.fort, aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=PFOR1830*100)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover", limits=c(0,100)) + 
  ggtitle("Pre-Settlement Forest Cover") +
  coord_equal()

ggplot(data=census.fort[census.fort$PINCORP>=0.9 & census.fort$HOUSINGDEN>0,]) +
  geom_polygon(data=census.fort, aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) +
  geom_polygon(data=census.fort[census.fort$PFOR1830>=0.9,], aes(x=long, y=lat, group=group), fill="darkgoldenrod2", color="black", size=0.1) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=PFOR1830*100)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover", limits=c(0,100)) + 
  ggtitle("Pre-Settlement Forest Cover") +
  coord_equal()

ggplot(data=census.fort[census.fort$PINCORP>=0.9 & census.fort$HOUSINGDEN>0,]) +
  geom_polygon(data=census.fort, aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) +
  geom_polygon(data=census.fort[census.fort$PFOR1830>=0.9,], aes(x=long, y=lat, group=group), fill="darkgoldenrod2", color="black", size=0.1) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=PCANCURREN*100)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover", limits=c(0,100)) + 
  ggtitle("Modern Tree Cover") +
  coord_equal()


