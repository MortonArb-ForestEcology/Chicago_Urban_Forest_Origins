# --------------------------------------------------------------
# Purpose:Exploring Tree Cover and Demogrpahic information by Census Tract
# Author: Christy Rollinson, crollinson@mortonarb.org
# Date: 22 May 2017
# --------------------------------------------------------------
#
# Steps
# 1. Read in census polygons & csv; write csv file into data frame because big values got weird
# 2. Exporatory Graphing
#     Changes in Distribution of tree cover
#     Income versus pre-settlement and modern tree cover
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
# --------------------------------------------------------------

# --------------------------------------------------------------
# 1. Loading in & Formatting Data
# --------------------------------------------------------------
census <- readOGR("data/ChicagoRegion_Census_Forests/ChicagoRegion_Census_Forests.shp")
census.df <- read.csv("data/ChicagoRegion_Census_Forests.csv")
names(census) <- names(census.df)
summary(census)
summary(census.df)

# Putting population on an area basis
census$Population.m2 <- census$POPULATION/(census$Shape_Area*0.093)
census.df$Population.m2 <- census.df$POPULATION/(census.df$Shape_Area*0.093)
summary(census)

# Doing the fortify stuff necessary to make ggplot plot polygons correctly
census$id <- as.factor(1:nrow(census))
census.fort <- fortify(census)
census.fort$id <- as.factor(census.fort$id)
census.fort <- merge(census.fort, census)
summary(census.fort)
# --------------------------------------------------------------

# --------------------------------------------------------------
# 2. Some diagnostic & exploratory figures
# --------------------------------------------------------------
png("figures/tree_cover_histogram.png", height=8, width=8, unit="in", res=220)
par(mfrow=c(2,1))
hist(census$forest_1850, main="Pre-Settlement Forest Cover (c. 1850)", xlim=c(0,1), xlab="% Cover Oak Ecosystem")
hist(census$tree, main="Tree Cover 2010", xlim=c(0,1), xlab="% Tree Cover")
par(mfrow=c(1,1))
dev.off()

tree.poly <- stack(census.fort[,c("tree", "forest_1850")])
names(tree.poly) <- c("cover.tree", "cover.period")
tree.poly$cover.period <- recode(tree.poly$cover.period, "'forest_1850'='pre-settlement'; 'tree'='modern'")
tree.poly$cover.period <- factor(tree.poly$cover.period, levels=c("pre-settlement", "modern"))
tree.poly[,c("id", "long", "lat", "order", "hole", "piece", "group")] <- census.fort[,c("id", "long", "lat", "order", "hole", "piece", "group")]
summary(tree.poly)

png("figures/tree_cover_comparison_byTract.png", height=8, width=12, unit="in", res=220)
ggplot(data=tree.poly) +
  facet_grid(.~cover.period) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cover.tree*100)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover") +
  coord_equal() +
  theme_bw() +
  theme(strip.text = element_text(size=rel(2), face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())
dev.off()


census.predictors <- names(census.df)[11:42]

tree.analy <- stack(census.df[,c("tree", "forest_1850")])
names(tree.analy) <- c("cover.tree", "cover.period")
tree.analy$cover.period <- recode(tree.analy$cover.period, "'forest_1850'='pre-settlement'; 'tree'='modern'")
tree.analy$cover.period <- factor(tree.analy$cover.period, levels=c("pre-settlement", "modern"))
tree.analy[,census.predictors] <- census.df[,census.predictors]
summary(tree.analy)

png("figures/tree_cover_comparison_histogram.png", height=8, width=12, unit="in", res=220)
ggplot(data=tree.analy) +
  facet_grid(.~cover.period) +
  geom_histogram(aes(x=cover.tree*100), fill="darkgreen", bins=20) +
  labs(x="Tree/Forest Cover (%)") +
  theme_bw() +
  theme(strip.text = element_text(size=rel(2), face="bold"))
dev.off()

png("figures/trees_vs_income.png", height=8, width=12, unit="in", res=220)
ggplot(data=tree.analy) +
  facet_grid(.~cover.period) +
  geom_point(aes(x=cover.tree*100, y=MEDIANEARN/1000), size=0.8, color="gray30") +
  geom_smooth(aes(x=cover.tree*100, y=MEDIANEARN/1000), method="lm", color="darkgreen", fill="darkgreen") +
  labs(x="Tree/Forest Cover (%)", y="Median Household Income (thousand USD)") +
  theme_bw() +
  theme(strip.text = element_text(size=rel(2), face="bold"))
dev.off()

census.fort$Pop.rel <- census.fort$POPULATION/max(census.fort$POPULATION)
census.fort$Earn.rel <- census.fort$MEDIANEARN/max(census.fort$MEDIANEARN)
summary(census.fort)

maps.demo <- stack(census.fort[,c("forest_1850", "tree", "Pop.rel", "Earn.rel")])
names(maps.demo) <- c("values", "layer")
maps.demo$layer <- recode(maps.demo$layer, "'forest_1850'='Forest, 1850 (%)'; 'tree'='Tree, 2010 (%)'; 'Earn.rel'='Rel Income (%)'; 'Pop.rel'='Rel Population (%)'")
maps.demo$layer <- factor(maps.demo$layer, levels=c("Forest, 1850 (%)", "Tree, 2010 (%)", "Rel Income (%)", "Rel Population (%)"))
maps.demo[,c("id", "long", "lat", "order", "hole", "piece", "group")] <- census.fort[,c("id", "long", "lat", "order", "hole", "piece", "group")]
summary(maps.demo)

map.tree.1850 <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=forest_1850*100)) +
  scale_fill_gradient(high="green3", name="% Forest\nCover", limits=c(0,100)) +
  coord_equal() +
  theme_bw() +
  ggtitle("Pre-Settlement\nForest Cover") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.tree.2010 <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=tree*100)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover", limits=c(0,100)) +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nModern Tree Cover") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.population <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=POPULATION/1000)) +
  scale_fill_gradient(high="red2", name="Population\n(x 1,000)") +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nPopulation") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.income <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=MEDIANEARN/1000)) +
  scale_fill_gradient(name="Household\nIncome\n($ x 1,000)") +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nHousehold Income") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

png("figures/trees_vs_demography.png", width=6, height=8, units="in", res=320)
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2)))
print(map.tree.1850 , vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(map.tree.2010 , vp = viewport(layout.pos.row = 1, layout.pos.col=2))
print(map.population, vp = viewport(layout.pos.row = 2, layout.pos.col=1))
print(map.income    , vp = viewport(layout.pos.row = 2, layout.pos.col=2))
dev.off()


# Lookign at tree cover and population density
ggplot(census.fort[census.fort$Population.m2<0.1,]) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Population.m2)) +
  scale_fill_gradient(high="red2", name="Population\n(ppl m-2)") +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nPopulation") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

# png("figures/trees_vs_income.png", height=8, width=12, unit="in", res=220)
ggplot(data=tree.analy[tree.analy$Population.m2<0.3,]) +
  facet_grid(.~cover.period) +
  geom_point(aes(x=cover.tree*100, y=log(Population.m2)), size=0.8, color="gray30") +
  geom_smooth(aes(x=cover.tree*100, y=log(Population.m2)), method="lm", color="darkgreen", fill="darkgreen") +
  # labs(x="Tree/Forest Cover (%)", y="Median Household Income (thousand USD)") +
  theme_bw() +
  theme(strip.text = element_text(size=rel(2), face="bold"))
# dev.off()


ggplot(data=census.df[census.df$Population.m2<0.3,]) +
  geom_point(aes(x=log(Population.m2), y=MEDIANEARN), size=0.8, color="gray30") +
  geom_smooth(aes(x=log(Population.m2), y=MEDIANEARN), method="lm", color="darkgreen", fill="darkgreen") +
  theme_bw()

ggplot(data=census.df[census.df$Population.m2<0.3,]) +
  geom_point(aes(x=COLLEGE, y=MEDIANEARN), size=0.8, color="gray30") +
  geom_smooth(aes(x=COLLEGE, y=MEDIANEARN), method="lm", color="darkgreen", fill="darkgreen") +
  theme_bw()


ggplot(data=census.df[census.df$Population.m2<0.3,]) +
  geom_point(aes(x=forest_1850, y=tree), size=0.8, color="gray30") +
  geom_smooth(aes(x=forest_1850, y=tree), method="lm", color="darkgreen", fill="darkgreen") +
  theme_bw()


census.df$tree <- census.df$tree*100
census.df$forest_1850 <- census.df$forest_1850*100

lm.tree <- lm(tree ~ forest_1850, data=census.df)
summary(lm.tree)

income.density <- lm(MEDIANEARN ~ log(Population.m2), data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
summary(income.density)

income.preset <- lm(MEDIANEARN ~ forest_1850, data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
summary(income.preset)

income.trees <- lm(MEDIANEARN ~ tree, data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
summary(income.trees)

income.college <- lm(MEDIANEARN ~ COLLEGE, data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
summary(income.college)

college.tree <- lm(MEDIANEARN ~ COLLEGE*tree, data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
summary(college.tree)

college.preset <- lm(MEDIANEARN ~ COLLEGE*forest_1850, data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
summary(college.preset)


mod.income <- lm(MEDIANEARN ~ tree*forest_1850*COLLEGE + log(Population.m2)*Shape_Area,data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
summary(mod.income)

mod.tree <- lm(tree ~ forest_1850 + MEDIANEARN*COLLEGE + log(Population.m2)*Shape_Area,data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
summary(mod.tree)


# mod.income2 <- lm(MEDIANEARN ~ forest_1850 + Population.m2 + COLLEGE,data=census.df[census.df$Population.m2<0.3 & census.df$Population.m2>0,])
# summary(mod.income2)

library(nlme)

hist(resid(income.density))
# --------------------------------------------------------------
