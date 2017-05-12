library(rgeos); library(rgdal)


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


canopy.grid <- readOGR(fgdb, "CanopyForestGrid")
class(canopy.grid)
summary(canopy.grid)
plot(canopy.grid)
dim(canopy.grid)

test <- coordinates(canopy.grid)
class(test); dim(test)
summary(test)
plot(test[,2] ~ test[,1], pch=19, cex=0.5)

# canopy.grid2 <- SpatialGrid(canopy.grid,)

# Determine the FC extent, projection, and attribute information
summary(fc)
test <- readOGR("~/Desktop/OriginsData.gdb/")

ogrDrivers()
dsn <- system.file("vectors", package = "rgdal")[1]
ogrListLayers(dsn)




