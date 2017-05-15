library(rgeos); library(rgdal)
library(raster)
library(ggplot2)

setwd("~/Desktop/Research/Chicago_Urban_Forest_Origins/data/")
fgdb <- "OriginsData.gdb" # Note: file paths do NOT work --> must be in the directory you're trying to pull it from

# List all feature classes in a file geodatabase
ogrListLayers(fgdb)[1:7]

# Read the feature class
forest.1850 <- readOGR(fgdb, "PresettlementForest")
forest.1939 <- readOGR(fgdb, "Forests1939")
forest.2010 <- readOGR(fgdb, "Forests2010")
dim(forest.1850); dim(forest.1939); dim(forest.2010)
summary(forest.1850)
summary(forest.1939)
summary(forest.2010)

# Making life easier by putting everything in a common projection
projection(forest.1850)
projection(forest.1939)
projection(forest.2010)
forest.1850 <- spTransform(forest.1850, projection(forest.2010))

# land.use <- readOGR(fgdb, "LandUse")
income   <- readOGR(fgdb, "HistoricIncome")
income$id <- as.factor(row.names(income)) # need an "id" column to make the merge we want later to work
projection(income)

# Transforming income projection
income <- spTransform(income, projection(forest.2010))

# Makign income a data frame to make life easier
income.df <- fortify(income)
income.df$id <- as.factor(income.df$id)
income.df <- merge(income.df, income)
summary(income.df)

# 
# png("../figures/2010IncomeOnly.png", height=8, width=5, units="in", res=220)
# ggplot() +
#   ggtitle("Income Only") +
#   # geom_polygon(data=forest.1850, aes(x=long, y=lat, group=group), fill="green") +
#   geom_polygon(data=income.df, aes(x=long, y=lat, group=group, fill=log(F2000Income))) +
#   coord_equal() +
#   theme_bw()
# dev.off()
# 
# png("../figures/2010Income_Forest1850.png", height=8, width=5, units="in", res=220)
# ggplot() +
#   ggtitle("Pre-Settlement Forest") +
#   geom_polygon(data=forest.1850, aes(x=long, y=lat, group=group), fill="green") +
#   geom_polygon(data=income.df, aes(x=long, y=lat, group=group, fill=log(F2000Income)), alpha=0.8) +
#   coord_equal() +
#   theme_bw()
# dev.off()
# 
# png("../figures/2010Income_Forest1939.png", height=8, width=5, units="in", res=220)
# ggplot() +
#   ggtitle("1939 Forest") +
#   geom_polygon(data=forest.1939, aes(x=long, y=lat, group=group), fill="green") +
#   geom_polygon(data=income.df, aes(x=long, y=lat, group=group, fill=log(F2000Income)), alpha=0.8) +
#   coord_equal() +
#   theme_bw()
# dev.off()
# 
# png("../figures/2010Income_Forest2010.png", height=8, width=5, units="in", res=220)
# ggplot() +
#   ggtitle("2010 Forest") +
#   geom_polygon(data=forest.2010, aes(x=long, y=lat, group=group), fill="green") +
#   geom_polygon(data=income.df, aes(x=long, y=lat, group=group, fill=log(F2000Income)), alpha=0.8) +
#   coord_equal() +
#   theme_bw()
# dev.off()

# 
# canopy.grid <- readOGR(fgdb, "CanopyForestGrid")
# class(canopy.grid)
# summary(canopy.grid)
# # canopy.grid <- spTransform(canopy.grid, proj4string(forest.1850))
# dim(canopy.grid)
# 


# png("../figures/TreeCover_1850.png", height=8, width=5, units="in", res=220)
# plot(forest.1850, col="green3", lwd=0.1, border="green3")
# plot(spTransform(canopy.grid, proj4string(forest.1850)), add=T, lwd=0.25)
# dev.off()
# 
# 
# png("../figures/TreeCover_1939.png", height=8, width=5, units="in", res=220)
# plot(forest.1939, col="green3", lwd=0.1, border="green3")
# plot(spTransform(canopy.grid, proj4string(forest.1939)), add=T, lwd=0.25)
# dev.off()
# 
# png("../figures/TreeCover_2010.png", height=8, width=5, units="in", res=220)
# plot(forest.2010, col="green3", lwd=0.1, border="green3")
# plot(spTransform(canopy.grid, proj4string(forest.2010)), add=T, lwd=0.25)
# dev.off()


# canopy.grid2 <- SpatialGrid(canopy.grid,)
# 
# # Determine the FC extent, projection, and attribute information
# summary(fc)
# test <- readOGR("~/Desktop/OriginsData.gdb/")
# 
# ogrDrivers()
# dsn <- system.file("vectors", package = "rgdal")[1]
# ogrListLayers(dsn)
# 
# -----------------------------------

# -----------------------------------
# Loading in land cover:
#This raster file maps seven land cover types (tree canopy, grass and shrubs, bare soil, water, buildings, roads and rail, and other paved surfaces) on a < 1 m resolution. These data already exist for Cook County (https://datahub.cmap.illinois.gov/dataset/high-resolution-land-cover-cook-county-2010), and the remaining counties will be available in October, 2015. See http://www.nrs.fs.fed.us/urban/utc for more information on methods.

# Assuming categories are in order:
# 1 = tree canopy
# 2 = grass + shrubs
# 3 = bare soil
# 4 = water
# 5 = buildings
# 6 = transportation (roads/rail)
# 7 = other paved
# -----------------------------------
landcover <- raster("landcover_2010_chicagoregion_illinois/landcover_2010_chicagoregion_illinois.img")
landcover
plot(landcover)
plot(income, add=T)

# forest <- landcover
# forest[forest!=1] <- NA

county.IL <- readOGR("IL_BNDY_County/IL_BNDY_County_Py.shp")
county.IL <- spTransform(county.IL, projection(landcover))
summary(county.IL)
plot(county.IL)
dim(county.IL)

counties.chicago <- c("LAKE", "COOK", "DUPAGE", "WILL", "MCHENRY", "KANE", "KENDALL")
plot(county.IL[county.IL$COUNTY_NAM %in% counties.chicago,])
plot(county.IL[county.IL$COUNTY_NAM=="DUPAGE",])

projection(county.chicago)

# lc.dupage.crop <- crop(landcover, extent(county.IL[county.IL$COUNTY_NAM=="DUPAGE",]), filename="landcover_2010_dupage", overwrite=T)

test <- crop(landcover, extent(spTransform(income[income$id==1,], projection(landcover))), filename="landcover_2010_CT1", overwrite=T)
plot(test)

# crop(landcover, extent(spTransform(income[income$id==1,], projection(landcover))))

summary(income)
plot(income)
plot(income[income$id==1,], add=T, border="red", lwd=2)
plot(spTransform(county.IL[county.IL$COUNTY_NAM=="DUPAGE",], projection(income)), add=T, border="blue", lwd=1.5)

# dupage <- county.IL[county.IL$COUNTY_NAM=="DUPAGE",]

# writeRaster(landcover, filename="LandCover_Forest")
test2 <- test
test2[test!=1] <- NA
plot(test2)
plot(test)

ncell(test)
ncell(test2)

ncell(test[test==1])/ncell(test)
test.summary <- ncells(test)

test2 <- extract(test2, income[income$id==1,], weights=T)
plot(landcover[landcover==1])
