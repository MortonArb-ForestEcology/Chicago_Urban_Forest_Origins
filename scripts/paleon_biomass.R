# Convert PalEON Biomass into Arc-Friendly spatial grids

path.drive <- "/Volumes/GoogleDrive/My Drive/OriginsSources-CRTI"
dat.nc <- ncdf4::nc_open(file.path(path.drive, "analyses/data/PalEON_Biomass/PLS_biomass_western_point_v0.999.nc"))

dat.nc$var$Total$longname

bm.total <- ncdf4::ncvar_get(dat.nc, "Total")
paleon.x <- ncdf4::ncvar_get(dat.nc, "x")
paleon.y <- ncdf4::ncvar_get(dat.nc, "y")

bm.df <- data.frame(x=rep(paleon.x, length(paleon.y)), 
                    y=rep(paleon.y, each=length(paleon.x)), 
                    Biomass=stack(data.frame(bm.total))[,1])

library(ggplot2)
png("/Volumes/GoogleDrive/My Drive/OriginsSources-CRTI/analyses/figures/Biomass_PLS.png", height=8, width=8, unit="in", res=320)
ggplot(data=bm.df) + coord_equal(expand=0) +
  geom_raster(aes(x=x, y=y, fill=Biomass))
dev.off()

library(sp); library(rgdal); library(raster)
bm.sp <- SpatialPixelsDataFrame(points=bm.df[,c("x", "y")], data=data.frame(bm.df[,c("Biomass")]), proj4string = CRS("+init=epsg:3175"))


bm.sp <- raster(bm.sp)
names(bm.sp) <- "Biomass-Total"

png(file.path(path.drive, "analyses/figures/Biomass_PLS2.png"), height=8, width=8, unit="in", res=320)
plot(bm.sp)
dev.off()

writeRaster(bm.sp, filename=file.path(path.drive, "analyses/data/PalEON_Biomass/PLS_biomass_western_point_v0.999_TOTAL.tif"), overwrite=T, format="GTiff")

test <- raster(file.path(path.drive, "analyses/data/PalEON_Biomass/PLS_biomass_western_point_v0.999_TOTAL.tif"))
test
# projection(test) <- CRS("+init=epsg:3175")
plot(test)
