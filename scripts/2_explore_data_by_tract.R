# --------------------------------------------------------------
# Purpose:Exploring PCanopy_1 Cover and Demogrpahic information by Census Tract
# Author: Christy Rollinson, crollinson@mortonarb.org
# Date: 22 May 2017
# --------------------------------------------------------------
#
# Steps
# 1. Read in census polygons & csv; write csv file into data frame because big values got weird
# 2. Exporatory Graphing
#     Changes in Distribution of PCanopy_1 cover
#     Income versus pre-settlement and modern PCanopy_1 cover
# 3. Very Basic Exploratory Analyses
#
# --------------------------------------------------------------
rm(list=ls())

# --------------------------------------------------------------
# Loading libraries, setting directories
# --------------------------------------------------------------
library(rgeos); library(rgdal)
# library(raster)
library(ggplot2); library(grid); library(scales); 
library(gridExtra)
library(car)

# No longer need to be in the data folder 
setwd("~/Desktop/Research/Chicago_Urban_Forest_Origins/")
fig.out <- "~/Google Drive/OriginsSources-CRTI/analyses/figures/"

# --------------------------------------------------------------

# --------------------------------------------------------------
# 1. Loading in & Formatting Data

# From Lindsay's email 31 Oct 2017
# "STATE_FIPS" Identification number for census data
# "CNTY_FIPS"  Identification number for census data
# "STCOFIPS"   Identification number for census data
# "TRACT"      Identification number for census data
# "BLKGRP"     Identification number for census data
# "FIPS"       All of the previous number concatenated together
# "Shape_Leng" Automatic measure that ArcMap adds I have no idea what the units are or what it really measures
# "Shape_Area" Automatic measure that ArcMap adds
# "PERBACHELO" Percent of population over 18 that is not married
# "HOUSINGUNI" Number of houses in the block group
# "ESTMEDINC" Median income in block group
# "MEDAGE"    Median age of building
# "OWNER"    number of house owners
# "RENTER"   number of renters
# "NOTMINORIT" Percent of pop that are not minority (probably white)
# "BLACK"    Percent of pop that's black  
# "ASIAN"     Percent of pop that's asian
# "OTHER"    Percent of pop that's other (mostly hispanic)
# "PerOwner"  Percent of population that owns a home
# "HousingDen" # houses per square mile
# "SQMIBlock" Square miles in each block
# "OID_"      Meaningless. Remnant from the join
# "FIPS_1"    Meaningless. Remnant from the join
# "PINCORP"  Percentage of the block group that's incorporated
# "PCanopy_1"  Canopy percentage from the land cover dataset
# "PPlantab_1"  Sum of percentages of bare soil, turf and other paved surface layers from land cover dataset
# "PImpervio"  Sum of percentages of buildings, roads/rail and other paved surface layers from land cover dataset
# "PRoads"  percentage of Roads/rail layer from land cover dataset
# "PFORESTED1" Percent forested in PLSS layer
# "PForeste_1" Percent forested from 1939 layer. It doesn't seem useful
# "PercentPov"Percent of pop that has been below poverty level in the last 12 months (from 2015)
# "PerUnemplo" Percent of population that's been unemployed in the last 12 months
# "POPULATION" Population in block group
# “PopDensity" Population divided by area (sq miles)
# --------------------------------------------------------------
# census <- readOGR("data/Census_Block_Canopy/CensusBlockGroupPIncCanopyRemnant.shp")
census <- readOGR("data/Census_Block/BlockGroupData1.shp")

# Putting some categorical factors as continuous
census$HOUSINGUNI <- as.numeric(paste(census$HOUSINGUNI))
census$ESTMEDINC <- as.numeric(paste(census$ESTMEDINC))
census$MEDAGE <- as.numeric(paste(census$MEDAGE))
census$OWNER <- as.numeric(paste(census$OWNER))
census$RENTER <- as.numeric(paste(census$RENTER))
census$POPULATION <- as.numeric(paste(census$POPULATION))
summary(census)

# Making Past Canopy cover on a scale of 0-100 like the other modern landover types
census$PForeste_1 <- census$PForeste_1*100
census$PFORESTED1 <- census$PFORESTED1*100

# Doing the fortify stuff necessary to make ggplot plot polygons correctly
census$id <- as.factor(0:(nrow(census)-1))
census.fort <- fortify(census)
census.fort$id <- as.factor(census.fort$id)
census.fort <- merge(census.fort, census)
summary(census.fort)

census.df <- data.frame(census)
census.df[,c("lon", "lat")] <- coordinates(census)
summary(census.df)
# --------------------------------------------------------------

# --------------------------------------------------------------
# 2. Some diagnostic & exploratory figures
# --------------------------------------------------------------

ggplot(data=census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) + 
  geom_polygon(data=census.fort[census.fort$PINCORP>=0.9 & census.fort$HousingDen>0 ,],
               aes(x=long, y=lat, group=group), fill="dodgerblue", color="black", size=0.1) + 
  geom_polygon(data=census.fort[census.fort$PINCORP>=0.9 & census.fort$HousingDen>0 & census.fort$ESTMEDINC==0,], 
               aes(x=long, y=lat, group=group), fill="red", color="black", size=0.1) + 
  coord_equal()

summary(census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0,])
summary(census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])


png(file.path(fig.out, "tree_cover_histogram.png"), height=8, width=8, unit="in", res=220)
par(mfrow=c(2,1))
hist(census.df[census.df$PINCORP>=.90 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,"PFORESTED1"], main="Pre-Settlement Forest Cover; (Incorporated)", xlim=c(0,100), xlab="% Cover Oak Ecosystem")
hist(census.df[census.df$PINCORP>=0.90 & census.df$HousingDen>0 & census.df$ESTMEDINC>0, "PCanopy_1"], main="PCanopy_1 Cover 2010; (Incorporated)", xlim=c(0,100), xlab="% PCanopy_1 Cover")
par(mfrow=c(1,1))
dev.off()

tree.poly <- stack(census.fort[,c("PCanopy_1", "PFORESTED1")])
names(tree.poly) <- c("cover.tree", "cover.period")
tree.poly$cover.period <- recode(tree.poly$cover.period, "'PFORESTED1'='pre-settlement'; 'PCanopy_1'='modern'")
tree.poly$cover.period <- factor(tree.poly$cover.period, levels=c("pre-settlement", "modern"))
tree.poly[,c("id", "long", "lat", "order", "hole", "piece", "group", "PINCORP", "HousingDen", "ESTMEDINC")] <- census.fort[,c("id", "long", "lat", "order", "hole", "piece", "group", "PINCORP", "HousingDen", "ESTMEDINC")]
summary(tree.poly)

png(file.path(fig.out, "tree_cover_comparison_byBlock.png"), height=8, width=12, unit="in", res=220)
ggplot(data=tree.poly) +
  facet_grid(.~cover.period) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) +
  geom_polygon(data=tree.poly[tree.poly$PINCORP>=0.9 & tree.poly$HousingDen>0 & tree.poly$ESTMEDINC>=0,],aes(x=long, y=lat, group=group, fill=cover.tree)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover", limits=c(0,100)) +
  coord_equal() +
  theme_bw() +
  theme(strip.text = element_text(size=rel(2), face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())
dev.off()


census.predictors <- names(census.df)[10:37]



tree.analy <- stack(census.df[,c("PCanopy_1", "PFORESTED1")])
names(tree.analy) <- c("cover.tree", "cover.period")
tree.analy$cover.period <- recode(tree.analy$cover.period, "'PFORESTED1'='pre-settlement'; 'PCanopy_1'='modern'")
tree.analy$cover.period <- factor(tree.analy$cover.period, levels=c("pre-settlement", "modern"))
tree.analy[,census.predictors] <- census.df[,census.predictors]
summary(tree.analy)

png(file.path(fig.out, "tree_cover_comparison_histogram.png"), height=8, width=12, unit="in", res=220)
ggplot(data=tree.analy[tree.analy$PINCORP>=0.9 & tree.analy$HousingDen>0 & tree.analy$ESTMEDINC>0,]) +
  facet_grid(.~cover.period) +
  geom_histogram(aes(x=cover.tree*100), fill="darkgreen", bins=20) +
  labs(x="PCanopy_1/Forest Cover (%)") +
  theme_bw() +
  theme(strip.text = element_text(size=rel(2), face="bold"))
dev.off()

png(file.path(fig.out, "trees_vs_income.png"), height=8, width=12, unit="in", res=220)
ggplot(data=tree.analy[tree.analy$PINCORP>=0.9 & tree.analy$HousingDen>0 & tree.analy$ESTMEDINC>0,]) +
  # ggplot(data=tree.analy[,]) +
  facet_grid(.~cover.period) +
  geom_point(aes(x=cover.tree*100, y=ESTMEDINC/1000), size=0.8, color="gray30") +
  geom_smooth(aes(x=cover.tree*100, y=ESTMEDINC/1000), method="lm", color="darkgreen", fill="darkgreen") +
  labs(x="Tree or Forest Cover (%)", y="Median Household Income (thousand USD)") +
  theme_bw() +
  theme(strip.text = element_text(size=rel(2), face="bold"))
dev.off()

census.fort$Dens.rel <- census.fort$HousingDen/max(census.fort$HousingDen)
census.fort$Earn.rel <- census.fort$ESTMEDINC/max(census.fort$ESTMEDINC)
summary(census.fort)

maps.demo <- stack(census.fort[,c("PFORESTED1", "PCanopy_1", "Dens.rel", "Earn.rel")])
names(maps.demo) <- c("values", "layer")
maps.demo$layer <- recode(maps.demo$layer, "'PFORESTED1'='Forest, 1830 (%)'; 'PCanopy_1'='Trees, 2010 (%)'; 'Earn.rel'='Rel Income (%)'; 'Dens.rel'='Rel Density (%)'")
maps.demo$layer <- factor(maps.demo$layer, levels=c("Forest, 1830 (%)", "Trees, 2010 (%)", "Rel Income (%)", "Rel Density (%)"))
maps.demo[,c("id", "long", "lat", "order", "hole", "piece", "group", "PINCORP", "HousingDen", "ESTMEDINC")] <- census.fort[,c("id", "long", "lat", "order", "hole", "piece", "group", "PINCORP", "HousingDen", "ESTMEDINC")]
summary(maps.demo)

map.tree.1830 <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) + 
  geom_polygon(data=census.fort[census.fort$PINCORP>=0.9 & census.fort$HousingDen>0 & census.fort$ESTMEDINC>0 ,],
               aes(x=long, y=lat, group=group, fill=PFORESTED1)) +
  scale_fill_gradient(high="green3", name="% Forest\nCover", limits=c(0,100)) +
  coord_equal() +
  theme_bw() +
  ggtitle("Pre-Settlement\nForest Cover") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.tree.2010 <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) + 
  geom_polygon(data=census.fort[census.fort$PINCORP>=0.9 & census.fort$HousingDen>0 & census.fort$ESTMEDINC>0 ,],
               aes(x=long, y=lat, group=group, fill=PCanopy_1)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover", limits=c(0,100)) +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nModern Tree Cover") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.density <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) + 
  geom_polygon(data=census.fort[census.fort$PINCORP>=0.9 & census.fort$HousingDen>0 & census.fort$ESTMEDINC>0 ,],
               aes(x=long, y=lat, group=group, fill=log10(HousingDen))) +
  scale_fill_gradient(high="red2", name="Log Pop.\nDensity") +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nPopulation Density (Log)") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.income <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) + 
  geom_polygon(data=census.fort[census.fort$PINCORP>=0.9 & census.fort$HousingDen>0 & census.fort$ESTMEDINC>0 ,],
               aes(x=long, y=lat, group=group, fill=ESTMEDINC/1000)) +
  scale_fill_gradient(name="Household\nIncome\n($ x 1,000)") +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nHousehold Income") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

png(file.path(fig.out, "trees_vs_demography2.png"), width=6, height=8, units="in", res=320)
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2)))
print(map.tree.1830 , vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(map.tree.2010 , vp = viewport(layout.pos.row = 1, layout.pos.col=2))
print(map.density, vp = viewport(layout.pos.row = 2, layout.pos.col=1))
print(map.income    , vp = viewport(layout.pos.row = 2, layout.pos.col=2))
dev.off()



ggplot(data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,]) +
  geom_point(aes(x=log(HousingDen), y=ESTMEDINC), size=0.8, color="gray30") +
  geom_smooth(aes(x=log(HousingDen), y=ESTMEDINC), method="lm", color="darkgreen", fill="darkgreen") +
  theme_bw()

ggplot(data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,]) +
  geom_point(aes(x=PERBACHELO, y=ESTMEDINC), size=0.8, color="gray30") +
  geom_smooth(aes(x=PERBACHELO, y=ESTMEDINC), method="lm", color="darkgreen", fill="darkgreen") +
  theme_bw()


ggplot(data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,]) +
  geom_point(aes(x=PFORESTED1, y=PCanopy_1), size=0.8, color="gray30") +
  geom_smooth(aes(x=PFORESTED1, y=PCanopy_1), method="lm", color="darkgreen", fill="darkgreen") +
  theme_bw()


census.df$PCanopy_1 <- census.df$PCanopy_1*100
census.df$PFORESTED1 <- census.df$PFORESTED1*100

lm.tree <- lm(PCanopy_1 ~ PFORESTED1, data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
summary(lm.tree)

income.density <- lm(ESTMEDINC ~ log(HousingDen), data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
summary(income.density)

income.preset <- lm(ESTMEDINC ~ PFORESTED1, data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
summary(income.preset)

income.trees <- lm(ESTMEDINC ~ PCanopy_1, data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
summary(income.trees)

income.college <- lm(ESTMEDINC ~ PERBACHELO, data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
summary(income.college)

college.tree <- lm(ESTMEDINC ~ PERBACHELO*PCanopy_1, data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
summary(college.tree)

college.preset <- lm(ESTMEDINC ~ PERBACHELO*PFORESTED1, data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
summary(college.preset)


mod.income <- lm(ESTMEDINC ~ PFORESTED1*PERBACHELO*log(HousingDen),data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
mod.income2 <- lm(ESTMEDINC ~ PCanopy_1*PERBACHELO*log(HousingDen),data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])

summary(mod.income)
summary(mod.income2)


# mod.tree <- lm(PCanopy_1 ~ PFORESTED1 + ESTMEDINC*PERBACHELO + log(HousingDen)*Shape_Area,data=census.df[census.df$HousingDen<0.3 & census.df$HousingDen>0,])
# summary(mod.tree)


mod.income2 <- lm(ESTMEDINC ~ PCanopy_1*PFORESTED1*log10(HousingDen)*PERBACHELO,data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])
summary(mod.income2)
# mod.income2 <- lm(ESTMEDINC ~ PCanopy_1*PFORESTED1*log10(HousingDen)*PERBACHELO,data=census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,])

plot(resid(mod.income2) ~ predict(mod.income2)); abline(h=0, col="red")

obs <- census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,"ESTMEDINC"]
pred <- predict(mod.income2)
obs.pred <- lm(pred~obs)
summary(obs.pred)
plot(pred ~ obs); abline(b=1, a=0, col="red"); abline(obs.pred, col="red", lty="dashed")

library(nlme)

hist(resid(income.density))
# --------------------------------------------------------------



# --------------------------------------------------------------
# Playing around with spatial autocorrelation
# http://rspatial.org/analysis/rst/3-spauto.html
# --------------------------------------------------------------
library(spdep)

# Go ahead and do the subset so I don't have to keep typing it all out
census.df.all <- census.df
census.df <- census.df[census.df$PINCORP>=0.9 & census.df$HousingDen>0 & census.df$ESTMEDINC>0,]

census.all <- census
census <- census[census$PINCORP>=0.9 & census$HousingDen>0 & census$ESTMEDINC>0,]

# sw <- poly2nb(census, row.names=census$id)
# sw2 <- nb2listw(sw, style="B")
# 
# 
# moran(census$ESTMEDINC, sw2, n=length(sw2$neighbours), S0=Szero(sw2))
# 
# moran.test(census$ESTMEDINC, sw2, randomisation = F)
# moran.mc(census$ESTMEDINC, sw2, nsim=1000)


library(mgcv)
library(nlme)
mod.1850 <- lm(ESTMEDINC ~ PFORESTED1*PERBACHELO*log(HousingDen), data=census.df)
summary(mod.1850)

mod.1850.sp <- gls(ESTMEDINC ~ PFORESTED1*PERBACHELO*log(HousingDen), data=census.df, correlation = corSpher(form=~lon + lat, nugget=T))
summary(mod.1850.sp)

mod.2010 <- lm(ESTMEDINC ~ PCanopy_1*PERBACHELO*log(HousingDen),data=census.df)
summary(mod.2010)

mod.2010.sp <- gls(ESTMEDINC ~ PCanopy_1*PERBACHELO*log(HousingDen), data=census.df, correlation = corSpher(form=~lon + lat, nugget=T))
summary(mod.2010.sp)

# --------------------------------------------------------------
