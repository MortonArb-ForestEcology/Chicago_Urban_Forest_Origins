# --------------------------------------------------------------
# Purpose:Exploring Tree Cover and Demogrpahic information by Census Tract
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
# --------------------------------------------------------------
# census <- readOGR("data/Census_Block_Canopy/CensusBlockGroupPIncCanopyRemnant.shp")
census <- readOGR("data/Census_Block/BlockGroupData2.shp")

# Putting some categorical factors as continuous
census$HOUSINGUNI <- as.numeric(paste(census$HOUSINGUNI))
census$ESTMEDINC <- as.numeric(paste(census$ESTMEDINC))
census$MEDAGE <- as.numeric(paste(census$MEDAGE))
census$OWNER <- as.numeric(paste(census$OWNER))
census$RENTER <- as.numeric(paste(census$RENTER))
census$POPULATION <- as.numeric(paste(census$POPULATION))
summary(census)

# Making Past Canopy cover on a scale of 0-100 like the other modern landover types
census$PFORESTED1 <- census$PFORESTED1*100
census$dForest <- census$PCanopy_1 - census$PFORESTED1
summary(census$dForest)

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
# Playing around with an ordination and then doing an env fit to correlate PCanopy_1 cover
# --------------------------------------------------------------
library(vegan); library(labdsv)

# SOme of our variables seem way out of wack, so lets trim it a bit
demog.use <- census.df$PINCORP>=0.9 & census.df$POPULATION>0 & census.df$HousingDen>0 & census.df$ESTMEDINC>0 
summary(census.df[demog.use,])

# pc.demog <- princomp(~ PERBACHELO + ESTMEDINC + BELOWPOVER + NOTMINORIT + UNEMPLOYME + HOUSEHOLD2 + HousingDen,
#                      data=census.df[demog.use,], cor=T, scores=T)
set.seed(154003)
# # Original list
# pc.demog <- princomp(~  ESTMEDINC + NOTMINORIT + HousingDen + PopDensity + PerOwner + PercentPov + PerUnemplo + PNOHIGH + PHIGH + PSOMECOLLE + PCOLLEGE + PADVANCED,
#                      data=census.df[demog.use,], cor=T, scores=T)
# Closely Correlated Variables:
#  - Population & housing density  
#  - college & advanced dgrees closely related
#  - Whiteness (not minority) & income
#  - Percent Unemplyment & Percent High school
#  - Percent No Highschool and percent poverty
pc.demog <- princomp(~  ESTMEDINC + PopDensity + PerOwner + PercentPov + PerUnemplo + PSOMECOLLE + PCOLLEGE,
                     data=census.df[demog.use,], cor=T, scores=T)

summary(pc.demog)

# summary(census.df[demog.use,])

census.df[demog.use,"pc1"] <- census[demog.use,"pc1"] <- pc.demog$scores[,1]
census.df[demog.use,"pc2"] <- census[demog.use,"pc2"] <- pc.demog$scores[,2]
pc.POV <- pc.demog$sdev^2/sum(pc.demog$sdev^2)


pc.loadings <- data.frame(var=row.names(pc.demog$loadings), pc.demog$loadings[,1:4])
mult <- min( (max(census.df$pc2, na.rm=T)-min(census.df$pc2, na.rm=T))/(max(pc.loadings$Comp.2, na.rm=T)-min(pc.loadings$Comp.2, na.rm=T)),
             (max(census.df$pc1, na.rm=T)-min(census.df$pc1, na.rm=T))/(max(pc.loadings$Comp.1, na.rm=T)-min(pc.loadings$Comp.1, na.rm=T))
)

write.csv(pc.loadings, file.path(fig.out, "../PCA_Loadings.csv"), row.names=F)

pc.loadings$load.PC1 <- 0.7*mult*pc.loadings$Comp.1
pc.loadings$load.PC2 <- 0.7*mult*pc.loadings$Comp.2
# pc.loadings$var <- c("Water Cap", "AWC (top)", "Tair (yr)", "Tair (JJA)", "Precip (yr)", "Precip (JJA)")
pc.all.cutoff <- 0
png(file.path(fig.out, "DemogPCA_Biplot.png"), height=10, width=8, units="in", res=320)
ggplot() +
  theme_bw() +# coord_equal() +
  geom_hline(yintercept=0, size=0.3, color="gray50") + geom_vline(xintercept = 0, size=0.3, color="gray50") +
  geom_point(data=census.df, aes(x=pc1, y=pc2), size=0.2, color="black") +
  geom_segment(data=pc.loadings[abs(pc.loadings$Comp.1)>pc.all.cutoff | abs(pc.loadings$Comp.2)>pc.all.cutoff,], aes(x=0, y=0, xend=load.PC1, yend=load.PC2), 
               arrow=arrow(length=unit(0.3,"cm")), size=1, alpha=0.9, color="red") +
  geom_text(data=pc.loadings[abs(pc.loadings$Comp.1)>pc.all.cutoff | abs(pc.loadings$Comp.2)>pc.all.cutoff,], 
            aes(x=load.PC1, y=load.PC2, label=var), size = 4, vjust=2, color="red", fontface="bold") +
  # coord_cartesian(xlim=quantile(census.df$pc1, c(0.001, 0.999)), ylim=quantile(census.df$pc2, c(0.0001, 0.9999)))
  
  scale_x_continuous(name=paste0("PC1 (", round(pc.POV["Comp.1"]*100,1),"%)"), expand=c(0.1,0.1)) +
  scale_y_continuous(name=paste0("PC2 (", round(pc.POV["Comp.2"]*100,1),"%)"), expand=c(0.1,0.1)) +
  # ggtitle("Region, All") +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=rel(1.25)))
dev.off()


# Doing the fortify stuff necessary to make ggplot plot polygons correctly
census$id <- as.factor(0:(nrow(census)-1))
census.fort <- fortify(census)
census.fort$id <- as.factor(census.fort$id)
census.fort <- merge(census.fort, census)
summary(census.fort)

demog.use2 <- census.fort$PINCORP>=0.9 & census.fort$HousingDen>0 & census.fort$ESTMEDINC>0

map.pc1 <- ggplot(data=census.fort) + theme_bw() + coord_equal(expand=0) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) +
  geom_polygon(data=census.fort[demog.use2,],aes(x=long, y=lat, group=group, fill=pc1)) +
  scale_fill_gradient2(low="blue2", high="red3", mid="gray80", midpoint=0, name="PC1") +
  ggtitle(paste0("PCA, PC1 (", round(pc.POV["Comp.1"]*100,1),"%)")) +
  theme(plot.title=element_text(face="bold", hjust=0.5, size=rel(1)))+
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.pc2 <- ggplot(data=census.fort) + theme_bw() + coord_equal(expand=0) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) +
  geom_polygon(data=census.fort[demog.use2,],aes(x=long, y=lat, group=group, fill=pc2)) +
  scale_fill_gradient2(low="blue2", high="red3", mid="gray80", midpoint=0, name="PC2") +
  ggtitle(paste0("PCA, PC2 (", round(pc.POV["Comp.2"]*100,1),"%)")) +
  theme(plot.title=element_text(face="bold", hjust=0.5, size=rel(1)))+
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.tree.1850 <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) +
  geom_polygon(data=census.fort[demog.use2,],aes(x=long, y=lat, group=group, fill=PFORESTED1)) +
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
  geom_polygon(data=census.fort[demog.use2,],aes(x=long, y=lat, group=group, fill=PCanopy_1)) +
  scale_fill_gradient(high="green3", name="% Tree\nCover", limits=c(0,100)) +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nModern Tree Cover") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

map.tree.diff <- ggplot(census.fort) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="gray50", color="black", size=0.1) +
  geom_polygon(data=census.fort[demog.use2,],aes(x=long, y=lat, group=group, fill=dForest)) +
  scale_fill_gradient2(high="green3", low="red", mid="black", name="% Tree\nCover") +
  coord_equal() +
  theme_bw() +
  ggtitle(" \nChange in Tree Cover") +
  # theme(legend.position=c(0.87, 0.85)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text  = element_blank(),
        axis.title = element_blank())

png(file.path(fig.out, "DemogPCA.png"), height=10, width=8, units="in", res=320)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(map.pc1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(map.pc2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(map.tree.1850, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(map.tree.2010, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()

png(file.path(fig.out, "TreeCover_Change.png"), height=10, width=8, units="in", res=320)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 2)))
print(map.tree.diff)
dev.off()

# --------------------------------------------------------------



# --------------------------------------------------------------
# Correlating current & past PCanopy_1 cover with our demographic PCA
# --------------------------------------------------------------
lm.hist.pc1 <- lm(pc1 ~ PFORESTED1, data=census.df[demog.use,])
lm.hist.pc2 <- lm(pc2 ~ PFORESTED1, data=census.df[demog.use,])
lm.mod.pc1  <- lm(pc1 ~ PCanopy_1, data=census.df[demog.use,])
lm.mod.pc2  <- lm(pc2 ~ PCanopy_1, data=census.df[demog.use,])

lm.diff.pc1 <- lm(pc1 ~ dForest, data=census.df[demog.use,])
lm.diff.pc2 <- lm(pc2 ~ dForest, data=census.df[demog.use,])
summary(lm.diff.pc1)
summary(lm.diff.pc2)

d.pc1 <- ggplot(data=census.df[demog.use,]) +
  geom_point(aes(x=dForest, y=pc1)) +
  stat_smooth(aes(x=dForest, y=pc1), method="lm", color="blue", fill="blue", alpha=0.5) +
  ggtitle(paste0("PC1 vs. Change in Forests (R2=", round(summary(lm.diff.pc1)$r.squared, 2), ")")) +
  theme_bw()

d.pc2 <- ggplot(data=census.df[demog.use,]) +
  geom_point(aes(x=dForest, y=pc2)) +
  stat_smooth(aes(x=dForest, y=pc2), method="lm", color="blue", fill="blue", alpha=0.5) +
  ggtitle(paste0("PC2 vs. Change in Forests (R2=", round(summary(lm.diff.pc2)$r.squared, 2), ")")) +
  theme_bw()


png(file.path(fig.out, "PCA_ForestChange.png"), height=10, width=8, units="in", res=320)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(d.pc1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(d.pc2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()



hist.pc1 <- ggplot(data=census.df[demog.use,]) +
  geom_point(aes(x=PFORESTED1, y=pc1)) +
  stat_smooth(aes(x=PFORESTED1, y=pc1), method="lm", color="blue", fill="blue", alpha=0.5) +
  ggtitle(paste0("PC1 vs. Settlement Forest (R2=", round(summary(lm.hist.pc1)$r.squared, 2), ")")) +
  theme_bw()
hist.pc2 <- ggplot(data=census.df[demog.use,]) +
  geom_point(aes(x=PFORESTED1, y=pc2)) +
  stat_smooth(aes(x=PFORESTED1, y=pc2), method="lm", color="blue", fill="blue", alpha=0.5) +
  ggtitle(paste0("PC2 vs. Settlement Forest (R2=", round(summary(lm.hist.pc2)$r.squared, 2), ")")) +
  theme_bw()

mod.pc1 <- ggplot(data=census.df[demog.use,]) +
  geom_point(aes(x=PCanopy_1, y=pc1)) +
  stat_smooth(aes(x=PCanopy_1, y=pc1), method="lm", color="green3", fill="green3", alpha=0.5) +
  ggtitle(paste0("PC1 vs. Modern Forest (R2=", round(summary(lm.mod.pc1)$r.squared, 2), ")")) +
  theme_bw()
mod.pc2 <- ggplot(data=census.df[demog.use,]) +
  geom_point(aes(x=PCanopy_1, y=pc2))+
  stat_smooth(aes(x=PCanopy_1, y=pc2), method="lm", color="green3", fill="green3", alpha=0.5) +
  ggtitle(paste0("PC2 vs. Modern Forest (R2=", round(summary(lm.mod.pc2)$r.squared, 2), ")")) +
  theme_bw()

png(file.path(fig.out, "DemogPCA_tree_corr.png"), height=10, width=8, units="in", res=320)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(hist.pc1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(hist.pc2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(mod.pc1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(mod.pc2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()


# Looking at the effect of PCanopy_1 cover on the ordination 
# 1. calculate the distnace matrix
# library(vegan)
demog.dist <- dist(census.df[!is.na(census.df$pc1),c("pc1", "pc2")], diag=T)
demog.tree <- adonis(demog.dist ~ PCanopy_1 + PFORESTED1, data=census.df[demog.use,])
demog.tree

env.demog <- envfit(pc.demog ~ PCanopy_1 + PFORESTED1, data=census.df[demog.use,], perm=2e4, na.rm=TRUE)
env.demog

tree.load <- data.frame(env.demog$vectors$arrows)
tree.load$var <- row.names(tree.load)
tree.load

# pc.loadings <- data.frame(var=row.names(pc.demog$loadings), pc.demog$loadings[,1:4])
# pc.loadings <- data.frame(var=row.names(pc.demog$loadings), pc.demog$loadings[,1:4])
# mult <- min( (max(census.df$pc2, na.rm=T)-min(census.df$pc2, na.rm=T))/(max(pc.loadings$Comp.2, na.rm=T)-min(pc.loadings$Comp.2, na.rm=T)),
             # (max(census.df$pc1, na.rm=T)-min(census.df$pc1, na.rm=T))/(max(pc.loadings$Comp.1, na.rm=T)-min(pc.loadings$Comp.1, na.rm=T))
# )

mult <- min( (max(census.df$pc2, na.rm=T)-min(census.df$pc2, na.rm=T))/(max(c(pc.loadings$Comp.2, env.demog$vectors$arrows[,2]), na.rm=T)-min(c(pc.loadings$Comp.2, env.demog$vectors$arrows[,2]), na.rm=T)),
             (max(census.df$pc1, na.rm=T)-min(census.df$pc1, na.rm=T))/(max(c(pc.loadings$Comp.1, env.demog$vectors$arrows[,1]), na.rm=T)-min(c(pc.loadings$Comp.1, env.demog$vectors$arrows[,1]), na.rm=T))
)
pc.loadings$load.PC1B <- 0.7*mult*pc.loadings$Comp.1
pc.loadings$load.PC2B <- 0.7*mult*pc.loadings$Comp.2
tree.load$load.PC1B <- 0.7*mult*tree.load$Comp.1
tree.load$load.PC2B <- 0.7*mult*tree.load$Comp.2

png(file.path(fig.out, "DemogPCA_Biplot_TreeFit.png"), height=10, width=8, units="in", res=320)
ggplot() +
  theme_bw() +# coord_equal() +
  geom_hline(yintercept=0, size=0.3, color="gray50") + geom_vline(xintercept = 0, size=0.3, color="gray50") +
  geom_point(data=census.df, aes(x=pc1, y=pc2), size=0.2, color="black") +
  geom_segment(data=pc.loadings[,], aes(x=0, y=0, xend=load.PC1B, yend=load.PC2B), 
               arrow=arrow(length=unit(0.3,"cm")), size=1, alpha=0.9, color="red") +
  geom_text(data=pc.loadings[,], 
            aes(x=load.PC1B, y=load.PC2B, label=var), size = 4, vjust=2, color="red", fontface="bold") +
  geom_segment(data=tree.load[,], aes(x=0, y=0, xend=load.PC1B, yend=load.PC2B), 
               arrow=arrow(length=unit(0.3,"cm")), size=1, alpha=0.9, color="blue2") +
  geom_text(data=tree.load[,], 
            aes(x=load.PC1B, y=load.PC2B, label=var), size = 4, vjust=2, color="blue2", fontface="bold") +
  
  scale_x_continuous(name=paste0("PC1 (", round(pc.POV["Comp.1"]*100,1),"%)"), expand=c(0.1,0.1)) +
  scale_y_continuous(name=paste0("PC2 (", round(pc.POV["Comp.2"]*100,1),"%)"), expand=c(0.1,0.1)) +
  # ggtitle("Region, All") +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=rel(1.25)))
dev.off()




# re-running the PCA with PCanopy_1 cover
# demog.use <- census.df$POPULATION>0 & census.df$POPULATION<max(census.df$POPULATION) & census.df$Population.m2<max(census.df$Population.m2) 
pc.demog2 <- princomp(~ ESTMEDINC + PopDensity + PerOwner + PercentPov + PerUnemplo + PSOMECOLLE + PCOLLEGE + PCanopy_1 + PFORESTED1, 
                      data=census.df[demog.use,], cor=T, scores=T)
summary(pc.demog2)

summary(census.df)

census.df[demog.use,"pc1.tree"] <- census[demog.use,"pc1.tree"] <- pc.demog2$scores[,1]
census.df[demog.use,"pc2.tree"] <- census[demog.use,"pc2.tree"] <- pc.demog2$scores[,2]
pc.POV2 <- pc.demog2$sdev^2/sum(pc.demog2$sdev^2)


pc.loadings2 <- data.frame(var=row.names(pc.demog2$loadings), pc.demog2$loadings[,1:4])
mult <- min( (max(census.df$pc2.tree, na.rm=T)-min(census.df$pc2.tree, na.rm=T))/(max(pc.loadings2$Comp.2, na.rm=T)-min(pc.loadings2$Comp.2, na.rm=T)),
             (max(census.df$pc1.tree, na.rm=T)-min(census.df$pc1.tree, na.rm=T))/(max(pc.loadings2$Comp.1, na.rm=T)-min(pc.loadings2$Comp.1, na.rm=T))
)
pc.loadings2$load.PC1 <- 0.7*mult*pc.loadings2$Comp.1
pc.loadings2$load.PC2 <- 0.7*mult*pc.loadings2$Comp.2
# pc.loadings$var <- c("Water Cap", "AWC (top)", "Tair (yr)", "Tair (JJA)", "Precip (yr)", "Precip (JJA)")
pc.all.cutoff <- 0


png(file.path(fig.out, "DemogPCA_Biplot_withTree.png"), height=10, width=8, units="in", res=320)
ggplot() +
  theme_bw() +# coord_equal() +
  geom_hline(yintercept=0, size=0.3, color="gray50") + geom_vline(xintercept = 0, size=0.3, color="gray50") +
  geom_point(data=census.df, aes(x=pc1.tree, y=pc2.tree), size=0.2, color="black") +
  geom_segment(data=pc.loadings2[(abs(pc.loadings2$Comp.1)>pc.all.cutoff | abs(pc.loadings2$Comp.2)>pc.all.cutoff) & !pc.loadings2$var %in% c("PCanopy_1", "PFORESTED1"),], 
               aes(x=0, y=0, xend=load.PC1, yend=load.PC2), 
               arrow=arrow(length=unit(0.3,"cm")), size=1, alpha=0.9, color="red") +
  geom_text(data=pc.loadings2[(abs(pc.loadings2$Comp.1)>pc.all.cutoff | abs(pc.loadings2$Comp.2)>pc.all.cutoff)  & !pc.loadings2$var %in% c("PCanopy_1", "PFORESTED1"),], 
            aes(x=load.PC1, y=load.PC2, label=var), size = 4, vjust=2, color="red", fontface="bold") +
  geom_segment(data=pc.loadings2[(abs(pc.loadings2$Comp.1)>pc.all.cutoff | abs(pc.loadings2$Comp.2)>pc.all.cutoff) & pc.loadings2$var %in% c("PCanopy_1", "PFORESTED1"),], 
               aes(x=0, y=0, xend=load.PC1, yend=load.PC2), 
               arrow=arrow(length=unit(0.3,"cm")), size=1.5, alpha=0.9, color="blue3") +
  geom_text(data=pc.loadings2[(abs(pc.loadings2$Comp.1)>pc.all.cutoff | abs(pc.loadings2$Comp.2)>pc.all.cutoff)  & pc.loadings2$var %in% c("PCanopy_1", "PFORESTED1"),], 
            aes(x=load.PC1, y=load.PC2, label=var), size = 5, vjust=2, color="blue3", fontface="bold") +
  
  scale_x_continuous(name=paste0("PC1 (", round(pc.POV2["Comp.1"]*100,1),"%)"), expand=c(0.1,0.1)) +
  scale_y_continuous(name=paste0("PC2 (", round(pc.POV2["Comp.2"]*100,1),"%)"), expand=c(0.1,0.1)) +
  # ggtitle("Region, All") +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=rel(1.25)))
dev.off()

# --------------------------------------------------------------
