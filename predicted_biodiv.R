# load prediction maps from https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/NWGCBY

lulc_2015 <- rast(raster("raw_data/predicted_land_system/ssp1_cov0_eu_updated.tif"),crs="EPSG:3035")
lulc_ssp1 <- rast(raster("raw_data/predicted_land_system/SSP1_Europe.tif"),crs="EPSG:3035")
lulc_ssp3 <- rast(raster("raw_data/predicted_land_system/SSP3_Europe.tif"),crs="EPSG:3035")
lulc_nac <- rast(raster("raw_data/predicted_land_system/nac_2050_eu.tif"),crs="EPSG:3035")
lulc_nfn <- rast(raster("raw_data/predicted_land_system/nfn_2050_eu.tif"),crs="EPSG:3035")
lulc_nfs <- rast(raster("raw_data/predicted_land_system/nfs_2050_eu.tif"),crs="EPSG:3035")

grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")

ggplot(grid_eu_mainland_biogeo) + tidyterra::geom_spatraster(data=lulc_2015, aes(fill=ssp1_cov0_eu_updated)) +
  scale_fill_viridis_b(na.value = NA) +
  geom_sf(fill=NA, col="white") + coord_sf(xlim=c(3500000,4000000),ylim=c(2500000,3000000))

### control compare to https://doi.org/10.1016/j.gloenvcha.2023.102766

lulc_eu_2015 <- exact_extract(lulc_2015,grid_eu_mainland_outline, fun="frac",default_value=0)
lulc_eu_ssp1 <- exact_extract(lulc_ssp1,grid_eu_mainland_outline, fun="frac",default_value=0)
lulc_eu_ssp3 <- exact_extract(lulc_ssp3,grid_eu_mainland_outline, fun="frac",default_value=0)
lulc_eu_nac <- exact_extract(lulc_nac,grid_eu_mainland_outline, fun="frac",default_value=30)
lulc_eu_nfn <- exact_extract(lulc_nfn,grid_eu_mainland_outline, fun="frac",default_value=30)
lulc_eu_nfs <- exact_extract(lulc_nfs,grid_eu_mainland_outline, fun="frac",default_value=30)

lulc_eu_2015$urban <- lulc_eu_2015$frac_1 + lulc_eu_2015$frac_2 + lulc_eu_2015$frac_3
lulc_eu_2015$farmland_low <- lulc_eu_2015$frac_4 + lulc_eu_2015$frac_9 + lulc_eu_2015$frac_12 + lulc_eu_2015$frac_17
lulc_eu_2015$farmland_medium <- lulc_eu_2015$frac_10 + lulc_eu_2015$frac_13 + lulc_eu_2015$frac_18
lulc_eu_2015$farmland_high <- lulc_eu_2015$frac_5 + lulc_eu_2015$frac_11 + lulc_eu_2015$frac_14 + lulc_eu_2015$frac_19
lulc_eu_2015$forest_lowmedium <- lulc_eu_2015$frac_6 + lulc_eu_2015$frac_7
lulc_eu_2015$forest_high <- lulc_eu_2015$frac_8
lulc_eu_2015$landscape_div <- lulc_eu_2015$frac_15 + lulc_eu_2015$frac_16 + lulc_eu_2015$frac_17 + lulc_eu_2015$frac_18 + lulc_eu_2015$frac_19
lulc_eu_2015 <- lulc_eu_2015[,c("urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_eu_2015$scenario <- "initial"

lulc_eu_ssp1$urban <- lulc_eu_ssp1$frac_1 + lulc_eu_ssp1$frac_2 + lulc_eu_ssp1$frac_3
lulc_eu_ssp1$farmland_low <- lulc_eu_ssp1$frac_4 + lulc_eu_ssp1$frac_9 + lulc_eu_ssp1$frac_12 + lulc_eu_ssp1$frac_17
lulc_eu_ssp1$farmland_medium <- lulc_eu_ssp1$frac_10 + lulc_eu_ssp1$frac_13 + lulc_eu_ssp1$frac_18
lulc_eu_ssp1$farmland_high <- lulc_eu_ssp1$frac_5 + lulc_eu_ssp1$frac_11 + lulc_eu_ssp1$frac_14 + lulc_eu_ssp1$frac_19
lulc_eu_ssp1$forest_lowmedium <- lulc_eu_ssp1$frac_6 + lulc_eu_ssp1$frac_7
lulc_eu_ssp1$forest_high <- lulc_eu_ssp1$frac_8
lulc_eu_ssp1$landscape_div <- lulc_eu_ssp1$frac_15 + lulc_eu_ssp1$frac_16 + lulc_eu_ssp1$frac_17 + lulc_eu_ssp1$frac_18 + lulc_eu_ssp1$frac_19
lulc_eu_ssp1 <- lulc_eu_ssp1[,c("urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_eu_ssp1$scenario <- "ssp1"

lulc_eu_ssp3$urban <- lulc_eu_ssp3$frac_1 + lulc_eu_ssp3$frac_2 + lulc_eu_ssp3$frac_3
lulc_eu_ssp3$farmland_low <- lulc_eu_ssp3$frac_4 + lulc_eu_ssp3$frac_9 + lulc_eu_ssp3$frac_12 + lulc_eu_ssp3$frac_17
lulc_eu_ssp3$farmland_medium <- lulc_eu_ssp3$frac_10 + lulc_eu_ssp3$frac_13 + lulc_eu_ssp3$frac_18
lulc_eu_ssp3$farmland_high <- lulc_eu_ssp3$frac_5 + lulc_eu_ssp3$frac_11 + lulc_eu_ssp3$frac_14 + lulc_eu_ssp3$frac_19
lulc_eu_ssp3$forest_lowmedium <- lulc_eu_ssp3$frac_6 + lulc_eu_ssp3$frac_7
lulc_eu_ssp3$forest_high <- lulc_eu_ssp3$frac_8
lulc_eu_ssp3$landscape_div <- lulc_eu_ssp3$frac_15 + lulc_eu_ssp3$frac_16 + lulc_eu_ssp3$frac_17 + lulc_eu_ssp3$frac_18 + lulc_eu_ssp3$frac_19
lulc_eu_ssp3 <- lulc_eu_ssp3[,c("urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_eu_ssp3$scenario <- "ssp3"

lulc_eu_nac$urban <- lulc_eu_nac$frac_0 + lulc_eu_nac$frac_1 + lulc_eu_nac$frac_2
lulc_eu_nac$farmland_low <- lulc_eu_nac$frac_3 + lulc_eu_nac$frac_8 + lulc_eu_nac$frac_11 + lulc_eu_nac$frac_16
lulc_eu_nac$farmland_medium <- lulc_eu_nac$frac_9 + lulc_eu_nac$frac_12 + lulc_eu_nac$frac_17
lulc_eu_nac$farmland_high <- lulc_eu_nac$frac_4 + lulc_eu_nac$frac_10 + lulc_eu_nac$frac_13 + lulc_eu_nac$frac_18
lulc_eu_nac$forest_lowmedium <- lulc_eu_nac$frac_5 + lulc_eu_nac$frac_6
lulc_eu_nac$forest_high <- lulc_eu_nac$frac_7
lulc_eu_nac$landscape_div <- lulc_eu_nac$frac_14 + lulc_eu_nac$frac_15 + lulc_eu_nac$frac_16 + lulc_eu_nac$frac_17 + lulc_eu_nac$frac_18
lulc_eu_nac <- lulc_eu_nac[,c("urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_eu_nac$scenario <- "nac"

lulc_eu_nfn$urban <- lulc_eu_nfn$frac_0 + lulc_eu_nfn$frac_1 + lulc_eu_nfn$frac_2
lulc_eu_nfn$farmland_low <- lulc_eu_nfn$frac_3 + lulc_eu_nfn$frac_8 + lulc_eu_nfn$frac_11 + lulc_eu_nfn$frac_16
lulc_eu_nfn$farmland_medium <- lulc_eu_nfn$frac_9 + lulc_eu_nfn$frac_12 + lulc_eu_nfn$frac_17
lulc_eu_nfn$farmland_high <- lulc_eu_nfn$frac_4 + lulc_eu_nfn$frac_10 + lulc_eu_nfn$frac_13 + lulc_eu_nfn$frac_18
lulc_eu_nfn$forest_lowmedium <- lulc_eu_nfn$frac_5 + lulc_eu_nfn$frac_6
lulc_eu_nfn$forest_high <- lulc_eu_nfn$frac_7
lulc_eu_nfn$landscape_div <- lulc_eu_nfn$frac_14 + lulc_eu_nfn$frac_15 + lulc_eu_nfn$frac_16 + lulc_eu_nfn$frac_17 + lulc_eu_nfn$frac_18
lulc_eu_nfn <- lulc_eu_nfn[,c("urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_eu_nfn$scenario <- "nfn"

lulc_eu_nfs$urban <- lulc_eu_nfs$frac_0 + lulc_eu_nfs$frac_1 + lulc_eu_nfs$frac_2
lulc_eu_nfs$farmland_low <- lulc_eu_nfs$frac_3 + lulc_eu_nfs$frac_8 + lulc_eu_nfs$frac_11 + lulc_eu_nfs$frac_16
lulc_eu_nfs$farmland_medium <- lulc_eu_nfs$frac_9 + lulc_eu_nfs$frac_12 + lulc_eu_nfs$frac_17
lulc_eu_nfs$farmland_high <- lulc_eu_nfs$frac_4 + lulc_eu_nfs$frac_10 + lulc_eu_nfs$frac_13 + lulc_eu_nfs$frac_18
lulc_eu_nfs$forest_lowmedium <- lulc_eu_nfs$frac_5 + lulc_eu_nfs$frac_6
lulc_eu_nfs$forest_high <- lulc_eu_nfs$frac_7
lulc_eu_nfs$landscape_div <- lulc_eu_nfs$frac_14 + lulc_eu_nfs$frac_15 + lulc_eu_nfs$frac_16 + lulc_eu_nfs$frac_17 + lulc_eu_nfs$frac_18
lulc_eu_nfs <- lulc_eu_nfs[,c("urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_eu_nfs$scenario <- "nfs"

lulc_eu <- rbind(lulc_eu_2015,lulc_eu_ssp1,lulc_eu_ssp3,lulc_eu_nac,lulc_eu_nfn,lulc_eu_nfs)

### apply to PLS

lulc_pls_2015 <- exact_extract(lulc_2015,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac",default_value=0)
lulc_pls_ssp1 <- exact_extract(lulc_ssp1,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac",default_value=0)
lulc_pls_ssp3 <- exact_extract(lulc_ssp3,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac",default_value=0)
lulc_pls_nac <- exact_extract(lulc_nac,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac",default_value=30)
lulc_pls_nfn <- exact_extract(lulc_nfn,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac",default_value=0)
lulc_pls_nfs <- exact_extract(lulc_nfs,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac",default_value=0)

lulc_pls_2015$urban <- lulc_pls_2015$frac_2 + lulc_pls_2015$frac_3
lulc_pls_2015$farmland_low <- lulc_pls_2015$frac_4 + lulc_pls_2015$frac_9 + lulc_pls_2015$frac_12 + lulc_pls_2015$frac_17
lulc_pls_2015$farmland_medium <- lulc_pls_2015$frac_10 + lulc_pls_2015$frac_13 + lulc_pls_2015$frac_18
lulc_pls_2015$farmland_high <- lulc_pls_2015$frac_5 + lulc_pls_2015$frac_11 + lulc_pls_2015$frac_14 + lulc_pls_2015$frac_19
lulc_pls_2015$forest_lowmedium <- lulc_pls_2015$frac_6 + lulc_pls_2015$frac_7
lulc_pls_2015$forest_high <- lulc_pls_2015$frac_8
lulc_pls_2015$landscape_div <- lulc_pls_2015$frac_15 + lulc_pls_2015$frac_16 + lulc_pls_2015$frac_17 + lulc_pls_2015$frac_18 + lulc_pls_2015$frac_19
lulc_pls_2015$PLS <- c(1:19,21:25)

plot_lulc_pls <- merge(grid_eu_mainland_biogeo,lulc_pls_2015, by="PLS")

ggplot(plot_lulc_pls) + geom_sf(aes(fill=urban),col=NA) +
  scale_fill_gradientn(colors = paletteer_c("ggthemes::Classic Area Red", 30))

lulc_pls_2015 <- lulc_pls_2015[,c("PLS","urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_pls_2015 <- rbind(lulc_pls_2015,data.frame(PLS="europe",lulc_eu_2015[,1:7]))
lulc_pls_2015 <- reshape2::melt(lulc_pls_2015,id.vars="PLS")
lulc_pls_2015$scenario <- "initial"

lulc_pls_ssp1$urban <- lulc_pls_ssp1$frac_2 + lulc_pls_ssp1$frac_3
lulc_pls_ssp1$farmland_low <- lulc_pls_ssp1$frac_4 + lulc_pls_ssp1$frac_9 + lulc_pls_ssp1$frac_12 + lulc_pls_ssp1$frac_17
lulc_pls_ssp1$farmland_medium <- lulc_pls_ssp1$frac_10 + lulc_pls_ssp1$frac_13 + lulc_pls_ssp1$frac_18
lulc_pls_ssp1$farmland_high <- lulc_pls_ssp1$frac_5 + lulc_pls_ssp1$frac_11 + lulc_pls_ssp1$frac_14 + lulc_pls_ssp1$frac_19
lulc_pls_ssp1$forest_lowmedium <- lulc_pls_ssp1$frac_6 + lulc_pls_ssp1$frac_7
lulc_pls_ssp1$forest_high <- lulc_pls_ssp1$frac_8
lulc_pls_ssp1$landscape_div <- lulc_pls_ssp1$frac_15 + lulc_pls_ssp1$frac_16 + lulc_pls_ssp1$frac_17 + lulc_pls_ssp1$frac_18 + lulc_pls_ssp1$frac_19
lulc_pls_ssp1$PLS <- c(1:19,21:25)
lulc_pls_ssp1 <- lulc_pls_ssp1[,c("PLS","urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_pls_ssp1 <- rbind(lulc_pls_ssp1,data.frame(PLS="europe",lulc_eu_ssp1[,1:7]))
lulc_pls_ssp1 <- reshape2::melt(lulc_pls_ssp1,id.vars="PLS")
lulc_pls_ssp1$scenario <- "ssp1"

lulc_pls_ssp3$urban <- lulc_pls_ssp3$frac_2 + lulc_pls_ssp3$frac_3
lulc_pls_ssp3$farmland_low <- lulc_pls_ssp3$frac_4 + lulc_pls_ssp3$frac_9 + lulc_pls_ssp3$frac_12 + lulc_pls_ssp3$frac_17
lulc_pls_ssp3$farmland_medium <- lulc_pls_ssp3$frac_10 + lulc_pls_ssp3$frac_13 + lulc_pls_ssp3$frac_18
lulc_pls_ssp3$farmland_high <- lulc_pls_ssp3$frac_5 + lulc_pls_ssp3$frac_11 + lulc_pls_ssp3$frac_14 + lulc_pls_ssp3$frac_19
lulc_pls_ssp3$forest_lowmedium <- lulc_pls_ssp3$frac_6 + lulc_pls_ssp3$frac_7
lulc_pls_ssp3$forest_high <- lulc_pls_ssp3$frac_8
lulc_pls_ssp3$landscape_div <- lulc_pls_ssp3$frac_15 + lulc_pls_ssp3$frac_16 + lulc_pls_ssp3$frac_17 + lulc_pls_ssp3$frac_18 + lulc_pls_ssp3$frac_19
lulc_pls_ssp3$PLS <- c(1:19,21:25)
lulc_pls_ssp3 <- lulc_pls_ssp3[,c("PLS","urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_pls_ssp3 <- rbind(lulc_pls_ssp3,data.frame(PLS="europe",lulc_eu_ssp3[,1:7]))
lulc_pls_ssp3 <- reshape2::melt(lulc_pls_ssp3,id.vars="PLS")
lulc_pls_ssp3$scenario <- "ssp3"

lulc_pls_nac$urban <- lulc_pls_nac$frac_1 + lulc_pls_nac$frac_2
lulc_pls_nac$farmland_low <- lulc_pls_nac$frac_3 + lulc_pls_nac$frac_8 + lulc_pls_nac$frac_11 + lulc_pls_nac$frac_16
lulc_pls_nac$farmland_medium <- lulc_pls_nac$frac_9 + lulc_pls_nac$frac_12 + lulc_pls_nac$frac_17
lulc_pls_nac$farmland_high <- lulc_pls_nac$frac_4 + lulc_pls_nac$frac_10 + lulc_pls_nac$frac_13 + lulc_pls_nac$frac_18
lulc_pls_nac$forest_lowmedium <- lulc_pls_nac$frac_5 + lulc_pls_nac$frac_6
lulc_pls_nac$forest_high <- lulc_pls_nac$frac_7
lulc_pls_nac$landscape_div <- lulc_pls_nac$frac_14 + lulc_pls_nac$frac_15 + lulc_pls_nac$frac_16 + lulc_pls_nac$frac_17 + lulc_pls_nac$frac_18
lulc_pls_nac$PLS <- c(1:19,21:25)
lulc_pls_nac <- lulc_pls_nac[,c("PLS","urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_pls_nac <- rbind(lulc_pls_nac,data.frame(PLS="europe",lulc_eu_nac[,1:7]))
lulc_pls_nac <- reshape2::melt(lulc_pls_nac,id.vars="PLS")
lulc_pls_nac$scenario <- "nac"

lulc_pls_nfn$urban <- lulc_pls_nfn$frac_1 + lulc_pls_nfn$frac_2
lulc_pls_nfn$farmland_low <- lulc_pls_nfn$frac_3 + lulc_pls_nfn$frac_8 + lulc_pls_nfn$frac_11 + lulc_pls_nfn$frac_16
lulc_pls_nfn$farmland_medium <- lulc_pls_nfn$frac_9 + lulc_pls_nfn$frac_12 + lulc_pls_nfn$frac_17
lulc_pls_nfn$farmland_high <- lulc_pls_nfn$frac_4 + lulc_pls_nfn$frac_10 + lulc_pls_nfn$frac_13 + lulc_pls_nfn$frac_18
lulc_pls_nfn$forest_lowmedium <- lulc_pls_nfn$frac_5 + lulc_pls_nfn$frac_6
lulc_pls_nfn$forest_high <- lulc_pls_nfn$frac_7
lulc_pls_nfn$landscape_div <- lulc_pls_nfn$frac_14 + lulc_pls_nfn$frac_15 + lulc_pls_nfn$frac_16 + lulc_pls_nfn$frac_17 + lulc_pls_nfn$frac_18
lulc_pls_nfn$PLS <- c(1:19,21:25)
lulc_pls_nfn <- lulc_pls_nfn[,c("PLS","urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_pls_nfn <- rbind(lulc_pls_nfn,data.frame(PLS="europe",lulc_eu_nfn[,1:7]))
lulc_pls_nfn <- reshape2::melt(lulc_pls_nfn,id.vars="PLS")
lulc_pls_nfn$scenario <- "nfn"

lulc_pls_nfs$urban <- lulc_pls_nfs$frac_1 + lulc_pls_nfs$frac_2
lulc_pls_nfs$farmland_low <- lulc_pls_nfs$frac_3 + lulc_pls_nfs$frac_8 + lulc_pls_nfs$frac_11 + lulc_pls_nfs$frac_16
lulc_pls_nfs$farmland_medium <- lulc_pls_nfs$frac_9 + lulc_pls_nfs$frac_12 + lulc_pls_nfs$frac_17
lulc_pls_nfs$farmland_high <- lulc_pls_nfs$frac_4 + lulc_pls_nfs$frac_10 + lulc_pls_nfs$frac_13 + lulc_pls_nfs$frac_18
lulc_pls_nfs$forest_lowmedium <- lulc_pls_nfs$frac_5 + lulc_pls_nfs$frac_6
lulc_pls_nfs$forest_high <- lulc_pls_nfs$frac_7
lulc_pls_nfs$landscape_div <- lulc_pls_nfs$frac_14 + lulc_pls_nfs$frac_15 + lulc_pls_nfs$frac_16 + lulc_pls_nfs$frac_17 + lulc_pls_nfs$frac_18
lulc_pls_nfs$PLS <- c(1:19,21:25)
lulc_pls_nfs <- lulc_pls_nfs[,c("PLS","urban","farmland_low","farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div")]
lulc_pls_nfs <- rbind(lulc_pls_nfs,data.frame(PLS="europe",lulc_eu_nfs[,1:7]))
lulc_pls_nfs <- reshape2::melt(lulc_pls_nfs,id.vars="PLS")
lulc_pls_nfs$scenario <- "nfs"

lulc_pls <- rbind(lulc_pls_2015,lulc_pls_ssp1,lulc_pls_ssp3,lulc_pls_nac,lulc_pls_nfn,lulc_pls_nfs)

lulc_pls_short <- reshape2::dcast(lulc_pls, PLS + variable ~ scenario, value.var = "value")

saveRDS(lulc_pls_short,"output/lulc_pls_short.rds")


# load change in protected areas from https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/TCNKPJ

#pa_2050 <- rast(raster("raw_data/predicted_protected_areas/FIG1_3values_optimal_1k_top10.tif"))
pa_2050 <- rast(raster("raw_data/predicted_protected_areas/FIG2_3values_expand_top5.tif"))

# current PA
protected_area_rast_reproj2 <- rast(raster(x = "output/protected_area_rast2.tif"))

ggplot(grid_eu_mainland_biogeo) + tidyterra::geom_spatraster(data=pa_2050, aes(fill=FIG2_3values_expand_top5)) +
  scale_fill_viridis_b(na.value = NA) +
  geom_sf(fill=NA, col="white")

ggplot(grid_eu_mainland_biogeo) + tidyterra::geom_spatraster(data=protected_area_rast_reproj2, aes(fill=protected_area_rast2)) +
  scale_fill_viridis_b(na.value = NA) +
  geom_sf(fill=NA, col="white")

pa_pls <- exact_extract(pa_2050,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac")
pa_pls_eu <- exact_extract(pa_2050,grid_eu_mainland_outline, fun="frac")
pa_pls <- rbind(pa_pls,pa_pls_eu)
pa_pls$nfn <- pa_pls$frac_1 + pa_pls$frac_2 + pa_pls$frac_3 + pa_pls$frac_7 + pa_pls$frac_8 + pa_pls$frac_12
pa_pls$nac <- pa_pls$frac_1 + pa_pls$frac_2 + pa_pls$frac_4 + pa_pls$frac_7 + pa_pls$frac_9 + pa_pls$frac_12
pa_pls$nfs <- pa_pls$frac_1 + pa_pls$frac_2 + pa_pls$frac_5 + pa_pls$frac_8 + pa_pls$frac_9 + pa_pls$frac_12
pa_pls$initial <- pa_pls$frac_1 + pa_pls$frac_2
pa_pls$PLS <- c(as.character(c(1:19,21:25)),"europe")

pa_pls_short <- pa_pls[,c("PLS","nfn","nfs","nac","initial")]
pa_pls_short$ssp1 <- pa_pls_short$ssp3 <- pa_pls_short$initial
pa_pls_short <- reshape2::melt(pa_pls_short,id.vars="PLS")
names(pa_pls_short)[2] <- "scenario"
pa_pls_short$variable <- "protected area"
pa_pls_short <- reshape2::dcast(pa_pls_short, PLS + variable ~ scenario, value.var = "value")

#pa_pls_current <- exact_extract(protected_area_rast_reproj2,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac", default_value=0)
#pa_pls_current$protected_perc <- pa_pls_current$frac_1 + pa_pls_current$frac_2

saveRDS(pa_pls_short,"output/pa_pls_short.rds")


# load change in climate https://cds.climate.copernicus.eu/

#36584 01/01/1950 - 01/03/2050
#36705 01/01/1950 - 30/06/2050

mean_t_4_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
mean_t_4_5_cclm_2016 <- (mean_t_4_5_cclm$X2016.03.01 + mean_t_4_5_cclm$X2016.04.01 + mean_t_4_5_cclm$X2016.05.01 + mean_t_4_5_cclm$X2016.06.01 +
                           mean_t_4_5_cclm$X2017.03.01 + mean_t_4_5_cclm$X2017.04.01 + mean_t_4_5_cclm$X2017.05.01 + mean_t_4_5_cclm$X2017.06.01 +
                           mean_t_4_5_cclm$X2018.03.01 + mean_t_4_5_cclm$X2018.04.01 + mean_t_4_5_cclm$X2018.05.01 + mean_t_4_5_cclm$X2018.06.01 +
                           mean_t_4_5_cclm$X2019.03.01 + mean_t_4_5_cclm$X2019.04.01 + mean_t_4_5_cclm$X2019.05.01 + mean_t_4_5_cclm$X2019.06.01 +
                           mean_t_4_5_cclm$X2020.03.01 + mean_t_4_5_cclm$X2020.04.01 + mean_t_4_5_cclm$X2020.05.01 + mean_t_4_5_cclm$X2020.06.01 +
                           mean_t_4_5_cclm$X2021.03.01 + mean_t_4_5_cclm$X2021.04.01 + mean_t_4_5_cclm$X2021.05.01 + mean_t_4_5_cclm$X2021.06.01)/24
mean_t_4_5_cclm <- (mean_t_4_5_cclm$X2050.03.01 + mean_t_4_5_cclm$X2050.04.01 + mean_t_4_5_cclm$X2050.05.01 + mean_t_4_5_cclm$X2050.06.01)/4
mean_t_4_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
mean_t_4_5_hirham_2016 <- (mean_t_4_5_hirham$X2016.03.01 + mean_t_4_5_hirham$X2016.04.01 + mean_t_4_5_hirham$X2016.05.01 + mean_t_4_5_hirham$X2016.06.01 +
                           mean_t_4_5_hirham$X2017.03.01 + mean_t_4_5_hirham$X2017.04.01 + mean_t_4_5_hirham$X2017.05.01 + mean_t_4_5_hirham$X2017.06.01 +
                           mean_t_4_5_hirham$X2018.03.01 + mean_t_4_5_hirham$X2018.04.01 + mean_t_4_5_hirham$X2018.05.01 + mean_t_4_5_hirham$X2018.06.01 +
                           mean_t_4_5_hirham$X2019.03.01 + mean_t_4_5_hirham$X2019.04.01 + mean_t_4_5_hirham$X2019.05.01 + mean_t_4_5_hirham$X2019.06.01 +
                           mean_t_4_5_hirham$X2020.03.01 + mean_t_4_5_hirham$X2020.04.01 + mean_t_4_5_hirham$X2020.05.01 + mean_t_4_5_hirham$X2020.06.01 +
                           mean_t_4_5_hirham$X2021.03.01 + mean_t_4_5_hirham$X2021.04.01 + mean_t_4_5_hirham$X2021.05.01 + mean_t_4_5_hirham$X2021.06.01)/24
mean_t_4_5_hirham <- (mean_t_4_5_hirham$X2050.03.01 + mean_t_4_5_hirham$X2050.04.01 + mean_t_4_5_hirham$X2050.05.01 + mean_t_4_5_hirham$X2050.06.01)/4
mean_t_4_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
mean_t_4_5_racmo_ecearth_2016 <- (mean_t_4_5_racmo_ecearth$X2016.03.01 + mean_t_4_5_racmo_ecearth$X2016.04.01 + mean_t_4_5_racmo_ecearth$X2016.05.01 + mean_t_4_5_racmo_ecearth$X2016.06.01 +
                           mean_t_4_5_racmo_ecearth$X2017.03.01 + mean_t_4_5_racmo_ecearth$X2017.04.01 + mean_t_4_5_racmo_ecearth$X2017.05.01 + mean_t_4_5_racmo_ecearth$X2017.06.01 +
                           mean_t_4_5_racmo_ecearth$X2018.03.01 + mean_t_4_5_racmo_ecearth$X2018.04.01 + mean_t_4_5_racmo_ecearth$X2018.05.01 + mean_t_4_5_racmo_ecearth$X2018.06.01 +
                           mean_t_4_5_racmo_ecearth$X2019.03.01 + mean_t_4_5_racmo_ecearth$X2019.04.01 + mean_t_4_5_racmo_ecearth$X2019.05.01 + mean_t_4_5_racmo_ecearth$X2019.06.01 +
                           mean_t_4_5_racmo_ecearth$X2020.03.01 + mean_t_4_5_racmo_ecearth$X2020.04.01 + mean_t_4_5_racmo_ecearth$X2020.05.01 + mean_t_4_5_racmo_ecearth$X2020.06.01 +
                           mean_t_4_5_racmo_ecearth$X2021.03.01 + mean_t_4_5_racmo_ecearth$X2021.04.01 + mean_t_4_5_racmo_ecearth$X2021.05.01 + mean_t_4_5_racmo_ecearth$X2021.06.01)/24
mean_t_4_5_racmo_ecearth <- (mean_t_4_5_racmo_ecearth$X2050.03.01 + mean_t_4_5_racmo_ecearth$X2050.04.01 + mean_t_4_5_racmo_ecearth$X2050.05.01 + mean_t_4_5_racmo_ecearth$X2050.06.01)/4
mean_t_4_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
mean_t_4_5_racmo_hadgem_2016 <- (mean_t_4_5_racmo_hadgem$X2016.03.01 + mean_t_4_5_racmo_hadgem$X2016.04.01 + mean_t_4_5_racmo_hadgem$X2016.05.01 + mean_t_4_5_racmo_hadgem$X2016.06.01 +
                           mean_t_4_5_racmo_hadgem$X2017.03.01 + mean_t_4_5_racmo_hadgem$X2017.04.01 + mean_t_4_5_racmo_hadgem$X2017.05.01 + mean_t_4_5_racmo_hadgem$X2017.06.01 +
                           mean_t_4_5_racmo_hadgem$X2018.03.01 + mean_t_4_5_racmo_hadgem$X2018.04.01 + mean_t_4_5_racmo_hadgem$X2018.05.01 + mean_t_4_5_racmo_hadgem$X2018.06.01 +
                           mean_t_4_5_racmo_hadgem$X2019.03.01 + mean_t_4_5_racmo_hadgem$X2019.04.01 + mean_t_4_5_racmo_hadgem$X2019.05.01 + mean_t_4_5_racmo_hadgem$X2019.06.01 +
                           mean_t_4_5_racmo_hadgem$X2020.03.01 + mean_t_4_5_racmo_hadgem$X2020.04.01 + mean_t_4_5_racmo_hadgem$X2020.05.01 + mean_t_4_5_racmo_hadgem$X2020.06.01 +
                           mean_t_4_5_racmo_hadgem$X2021.03.01 + mean_t_4_5_racmo_hadgem$X2021.04.01 + mean_t_4_5_racmo_hadgem$X2021.05.01 + mean_t_4_5_racmo_hadgem$X2021.06.01)/24
mean_t_4_5_racmo_hadgem <- (mean_t_4_5_racmo_hadgem$X2050.03.01 + mean_t_4_5_racmo_hadgem$X2050.04.01 + mean_t_4_5_racmo_hadgem$X2050.05.01 + mean_t_4_5_racmo_hadgem$X2050.06.01)/4
mean_t_4_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
mean_t_4_5_rca_hadgem_2016 <- (mean_t_4_5_rca_hadgem$X2016.03.01 + mean_t_4_5_rca_hadgem$X2016.04.01 + mean_t_4_5_rca_hadgem$X2016.05.01 + mean_t_4_5_rca_hadgem$X2016.06.01 +
                           mean_t_4_5_rca_hadgem$X2017.03.01 + mean_t_4_5_rca_hadgem$X2017.04.01 + mean_t_4_5_rca_hadgem$X2017.05.01 + mean_t_4_5_rca_hadgem$X2017.06.01 +
                           mean_t_4_5_rca_hadgem$X2018.03.01 + mean_t_4_5_rca_hadgem$X2018.04.01 + mean_t_4_5_rca_hadgem$X2018.05.01 + mean_t_4_5_rca_hadgem$X2018.06.01 +
                           mean_t_4_5_rca_hadgem$X2019.03.01 + mean_t_4_5_rca_hadgem$X2019.04.01 + mean_t_4_5_rca_hadgem$X2019.05.01 + mean_t_4_5_rca_hadgem$X2019.06.01 +
                           mean_t_4_5_rca_hadgem$X2020.03.01 + mean_t_4_5_rca_hadgem$X2020.04.01 + mean_t_4_5_rca_hadgem$X2020.05.01 + mean_t_4_5_rca_hadgem$X2020.06.01 +
                           mean_t_4_5_rca_hadgem$X2021.03.01 + mean_t_4_5_rca_hadgem$X2021.04.01 + mean_t_4_5_rca_hadgem$X2021.05.01 + mean_t_4_5_rca_hadgem$X2021.06.01)/24
mean_t_4_5_rca_hadgem <- (mean_t_4_5_rca_hadgem$X2050.03.01 + mean_t_4_5_rca_hadgem$X2050.04.01 + mean_t_4_5_rca_hadgem$X2050.05.01 + mean_t_4_5_rca_hadgem$X2050.06.01)/4
mean_t_4_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
mean_t_4_5_rca_mpi_2016 <- (mean_t_4_5_rca_mpi$X2016.03.01 + mean_t_4_5_rca_mpi$X2016.04.01 + mean_t_4_5_rca_mpi$X2016.05.01 + mean_t_4_5_rca_mpi$X2016.06.01 +
                           mean_t_4_5_rca_mpi$X2017.03.01 + mean_t_4_5_rca_mpi$X2017.04.01 + mean_t_4_5_rca_mpi$X2017.05.01 + mean_t_4_5_rca_mpi$X2017.06.01 +
                           mean_t_4_5_rca_mpi$X2018.03.01 + mean_t_4_5_rca_mpi$X2018.04.01 + mean_t_4_5_rca_mpi$X2018.05.01 + mean_t_4_5_rca_mpi$X2018.06.01 +
                           mean_t_4_5_rca_mpi$X2019.03.01 + mean_t_4_5_rca_mpi$X2019.04.01 + mean_t_4_5_rca_mpi$X2019.05.01 + mean_t_4_5_rca_mpi$X2019.06.01 +
                           mean_t_4_5_rca_mpi$X2020.03.01 + mean_t_4_5_rca_mpi$X2020.04.01 + mean_t_4_5_rca_mpi$X2020.05.01 + mean_t_4_5_rca_mpi$X2020.06.01 +
                           mean_t_4_5_rca_mpi$X2021.03.01 + mean_t_4_5_rca_mpi$X2021.04.01 + mean_t_4_5_rca_mpi$X2021.05.01 + mean_t_4_5_rca_mpi$X2021.06.01)/24
mean_t_4_5_rca_mpi <- (mean_t_4_5_rca_mpi$X2050.03.01 + mean_t_4_5_rca_mpi$X2050.04.01 + mean_t_4_5_rca_mpi$X2050.05.01 + mean_t_4_5_rca_mpi$X2050.06.01)/4
mean_t_4_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
mean_t_4_5_wrf_2016 <- (mean_t_4_5_wrf$X2016.03.01 + mean_t_4_5_wrf$X2016.04.01 + mean_t_4_5_wrf$X2016.05.01 + mean_t_4_5_wrf$X2016.06.01 +
                           mean_t_4_5_wrf$X2017.03.01 + mean_t_4_5_wrf$X2017.04.01 + mean_t_4_5_wrf$X2017.05.01 + mean_t_4_5_wrf$X2017.06.01 +
                           mean_t_4_5_wrf$X2018.03.01 + mean_t_4_5_wrf$X2018.04.01 + mean_t_4_5_wrf$X2018.05.01 + mean_t_4_5_wrf$X2018.06.01 +
                           mean_t_4_5_wrf$X2019.03.01 + mean_t_4_5_wrf$X2019.04.01 + mean_t_4_5_wrf$X2019.05.01 + mean_t_4_5_wrf$X2019.06.01 +
                           mean_t_4_5_wrf$X2020.03.01 + mean_t_4_5_wrf$X2020.04.01 + mean_t_4_5_wrf$X2020.05.01 + mean_t_4_5_wrf$X2020.06.01 +
                           mean_t_4_5_wrf$X2021.03.01 + mean_t_4_5_wrf$X2021.04.01 + mean_t_4_5_wrf$X2021.05.01 + mean_t_4_5_wrf$X2021.06.01)/24
mean_t_4_5_wrf <- (mean_t_4_5_wrf$X2050.03.01 + mean_t_4_5_wrf$X2050.04.01 + mean_t_4_5_wrf$X2050.05.01 + mean_t_4_5_wrf$X2050.06.01)/4

mean_t_4_5 <- (mean_t_4_5_cclm + mean_t_4_5_hirham + mean_t_4_5_racmo_ecearth + mean_t_4_5_racmo_hadgem + mean_t_4_5_rca_hadgem + mean_t_4_5_rca_mpi + mean_t_4_5_wrf)/7 - 272.15
mean_t_2016 <- (mean_t_4_5_cclm_2016 + mean_t_4_5_hirham_2016 + mean_t_4_5_racmo_ecearth_2016 + mean_t_4_5_racmo_hadgem_2016 + mean_t_4_5_rca_hadgem_2016 + mean_t_4_5_rca_mpi_2016 + mean_t_4_5_wrf_2016)/7 - 272.15


mean_t_8_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_8_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
mean_t_8_5_cclm <- (mean_t_8_5_cclm$X2050.03.01 + mean_t_8_5_cclm$X2050.04.01 + mean_t_8_5_cclm$X2050.05.01 + mean_t_8_5_cclm$X2050.06.01)/4
mean_t_8_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_8_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
mean_t_8_5_hirham <- (mean_t_8_5_hirham$X2050.03.01 + mean_t_8_5_hirham$X2050.04.01 + mean_t_8_5_hirham$X2050.05.01 + mean_t_8_5_hirham$X2050.06.01)/4
mean_t_8_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_8_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
mean_t_8_5_racmo_ecearth <- (mean_t_8_5_racmo_ecearth$X2050.03.01 + mean_t_8_5_racmo_ecearth$X2050.04.01 + mean_t_8_5_racmo_ecearth$X2050.05.01 + mean_t_8_5_racmo_ecearth$X2050.06.01)/4
mean_t_8_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_8_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
mean_t_8_5_racmo_hadgem <- (mean_t_8_5_racmo_hadgem$X2050.03.01 + mean_t_8_5_racmo_hadgem$X2050.04.01 + mean_t_8_5_racmo_hadgem$X2050.05.01 + mean_t_8_5_racmo_hadgem$X2050.06.01)/4
mean_t_8_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_8_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
mean_t_8_5_rca_hadgem <- (mean_t_8_5_rca_hadgem$X2050.03.01 + mean_t_8_5_rca_hadgem$X2050.04.01 + mean_t_8_5_rca_hadgem$X2050.05.01 + mean_t_8_5_rca_hadgem$X2050.06.01)/4
mean_t_8_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_8_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
mean_t_8_5_rca_mpi <- (mean_t_8_5_rca_mpi$X2050.03.01 + mean_t_8_5_rca_mpi$X2050.04.01 + mean_t_8_5_rca_mpi$X2050.05.01 + mean_t_8_5_rca_mpi$X2050.06.01)/4
mean_t_8_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_8_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
mean_t_8_5_wrf <- (mean_t_8_5_wrf$X2050.03.01 + mean_t_8_5_wrf$X2050.04.01 + mean_t_8_5_wrf$X2050.05.01 + mean_t_8_5_wrf$X2050.06.01)/4

mean_t_8_5 <- (mean_t_8_5_cclm + mean_t_8_5_hirham + mean_t_8_5_racmo_ecearth + mean_t_8_5_racmo_hadgem + mean_t_8_5_rca_hadgem + mean_t_8_5_rca_mpi + mean_t_8_5_wrf)/7 - 272.15

sum_p_4_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
sum_p_4_5_cclm_2016 <- (sum_p_4_5_cclm$X2016.03.01 + sum_p_4_5_cclm$X2016.04.01 + sum_p_4_5_cclm$X2016.05.01 + sum_p_4_5_cclm$X2016.06.01 +
                           sum_p_4_5_cclm$X2017.03.01 + sum_p_4_5_cclm$X2017.04.01 + sum_p_4_5_cclm$X2017.05.01 + sum_p_4_5_cclm$X2017.06.01 +
                           sum_p_4_5_cclm$X2018.03.01 + sum_p_4_5_cclm$X2018.04.01 + sum_p_4_5_cclm$X2018.05.01 + sum_p_4_5_cclm$X2018.06.01 +
                           sum_p_4_5_cclm$X2019.03.01 + sum_p_4_5_cclm$X2019.04.01 + sum_p_4_5_cclm$X2019.05.01 + sum_p_4_5_cclm$X2019.06.01 +
                           sum_p_4_5_cclm$X2020.03.01 + sum_p_4_5_cclm$X2020.04.01 + sum_p_4_5_cclm$X2020.05.01 + sum_p_4_5_cclm$X2020.06.01 +
                           sum_p_4_5_cclm$X2021.03.01 + sum_p_4_5_cclm$X2021.04.01 + sum_p_4_5_cclm$X2021.05.01 + sum_p_4_5_cclm$X2021.06.01)/6
sum_p_4_5_cclm <- sum_p_4_5_cclm$X2050.03.01 + sum_p_4_5_cclm$X2050.04.01 + sum_p_4_5_cclm$X2050.05.01 + sum_p_4_5_cclm$X2050.06.01
sum_p_4_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
sum_p_4_5_hirham_2016 <- (sum_p_4_5_hirham$X2016.03.01 + sum_p_4_5_hirham$X2016.04.01 + sum_p_4_5_hirham$X2016.05.01 + sum_p_4_5_hirham$X2016.06.01 +
                          sum_p_4_5_hirham$X2017.03.01 + sum_p_4_5_hirham$X2017.04.01 + sum_p_4_5_hirham$X2017.05.01 + sum_p_4_5_hirham$X2017.06.01 +
                          sum_p_4_5_hirham$X2018.03.01 + sum_p_4_5_hirham$X2018.04.01 + sum_p_4_5_hirham$X2018.05.01 + sum_p_4_5_hirham$X2018.06.01 +
                          sum_p_4_5_hirham$X2019.03.01 + sum_p_4_5_hirham$X2019.04.01 + sum_p_4_5_hirham$X2019.05.01 + sum_p_4_5_hirham$X2019.06.01 +
                          sum_p_4_5_hirham$X2020.03.01 + sum_p_4_5_hirham$X2020.04.01 + sum_p_4_5_hirham$X2020.05.01 + sum_p_4_5_hirham$X2020.06.01 +
                          sum_p_4_5_hirham$X2021.03.01 + sum_p_4_5_hirham$X2021.04.01 + sum_p_4_5_hirham$X2021.05.01 + sum_p_4_5_hirham$X2021.06.01)/6
sum_p_4_5_hirham <- sum_p_4_5_hirham$X2050.03.01 + sum_p_4_5_hirham$X2050.04.01 + sum_p_4_5_hirham$X2050.05.01 + sum_p_4_5_hirham$X2050.06.01
sum_p_4_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
sum_p_4_5_racmo_ecearth_2016 <- (sum_p_4_5_racmo_ecearth$X2016.03.01 + sum_p_4_5_racmo_ecearth$X2016.04.01 + sum_p_4_5_racmo_ecearth$X2016.05.01 + sum_p_4_5_racmo_ecearth$X2016.06.01 +
                          sum_p_4_5_racmo_ecearth$X2017.03.01 + sum_p_4_5_racmo_ecearth$X2017.04.01 + sum_p_4_5_racmo_ecearth$X2017.05.01 + sum_p_4_5_racmo_ecearth$X2017.06.01 +
                          sum_p_4_5_racmo_ecearth$X2018.03.01 + sum_p_4_5_racmo_ecearth$X2018.04.01 + sum_p_4_5_racmo_ecearth$X2018.05.01 + sum_p_4_5_racmo_ecearth$X2018.06.01 +
                          sum_p_4_5_racmo_ecearth$X2019.03.01 + sum_p_4_5_racmo_ecearth$X2019.04.01 + sum_p_4_5_racmo_ecearth$X2019.05.01 + sum_p_4_5_racmo_ecearth$X2019.06.01 +
                          sum_p_4_5_racmo_ecearth$X2020.03.01 + sum_p_4_5_racmo_ecearth$X2020.04.01 + sum_p_4_5_racmo_ecearth$X2020.05.01 + sum_p_4_5_racmo_ecearth$X2020.06.01 +
                          sum_p_4_5_racmo_ecearth$X2021.03.01 + sum_p_4_5_racmo_ecearth$X2021.04.01 + sum_p_4_5_racmo_ecearth$X2021.05.01 + sum_p_4_5_racmo_ecearth$X2021.06.01)/6
sum_p_4_5_racmo_ecearth <- sum_p_4_5_racmo_ecearth$X2050.03.01 + sum_p_4_5_racmo_ecearth$X2050.04.01 + sum_p_4_5_racmo_ecearth$X2050.05.01 + sum_p_4_5_racmo_ecearth$X2050.06.01
sum_p_4_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
sum_p_4_5_racmo_hadgem_2016 <- (sum_p_4_5_racmo_hadgem$X2016.03.01 + sum_p_4_5_racmo_hadgem$X2016.04.01 + sum_p_4_5_racmo_hadgem$X2016.05.01 + sum_p_4_5_racmo_hadgem$X2016.06.01 +
                          sum_p_4_5_racmo_hadgem$X2017.03.01 + sum_p_4_5_racmo_hadgem$X2017.04.01 + sum_p_4_5_racmo_hadgem$X2017.05.01 + sum_p_4_5_racmo_hadgem$X2017.06.01 +
                          sum_p_4_5_racmo_hadgem$X2018.03.01 + sum_p_4_5_racmo_hadgem$X2018.04.01 + sum_p_4_5_racmo_hadgem$X2018.05.01 + sum_p_4_5_racmo_hadgem$X2018.06.01 +
                          sum_p_4_5_racmo_hadgem$X2019.03.01 + sum_p_4_5_racmo_hadgem$X2019.04.01 + sum_p_4_5_racmo_hadgem$X2019.05.01 + sum_p_4_5_racmo_hadgem$X2019.06.01 +
                          sum_p_4_5_racmo_hadgem$X2020.03.01 + sum_p_4_5_racmo_hadgem$X2020.04.01 + sum_p_4_5_racmo_hadgem$X2020.05.01 + sum_p_4_5_racmo_hadgem$X2020.06.01 +
                          sum_p_4_5_racmo_hadgem$X2021.03.01 + sum_p_4_5_racmo_hadgem$X2021.04.01 + sum_p_4_5_racmo_hadgem$X2021.05.01 + sum_p_4_5_racmo_hadgem$X2021.06.01)/6
sum_p_4_5_racmo_hadgem <- sum_p_4_5_racmo_hadgem$X2050.03.01 + sum_p_4_5_racmo_hadgem$X2050.04.01 + sum_p_4_5_racmo_hadgem$X2050.05.01 + sum_p_4_5_racmo_hadgem$X2050.06.01
sum_p_4_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
sum_p_4_5_rca_hadgem_2016 <- (sum_p_4_5_rca_hadgem$X2016.03.01 + sum_p_4_5_rca_hadgem$X2016.04.01 + sum_p_4_5_rca_hadgem$X2016.05.01 + sum_p_4_5_rca_hadgem$X2016.06.01 +
                          sum_p_4_5_rca_hadgem$X2017.03.01 + sum_p_4_5_rca_hadgem$X2017.04.01 + sum_p_4_5_rca_hadgem$X2017.05.01 + sum_p_4_5_rca_hadgem$X2017.06.01 +
                          sum_p_4_5_rca_hadgem$X2018.03.01 + sum_p_4_5_rca_hadgem$X2018.04.01 + sum_p_4_5_rca_hadgem$X2018.05.01 + sum_p_4_5_rca_hadgem$X2018.06.01 +
                          sum_p_4_5_rca_hadgem$X2019.03.01 + sum_p_4_5_rca_hadgem$X2019.04.01 + sum_p_4_5_rca_hadgem$X2019.05.01 + sum_p_4_5_rca_hadgem$X2019.06.01 +
                          sum_p_4_5_rca_hadgem$X2020.03.01 + sum_p_4_5_rca_hadgem$X2020.04.01 + sum_p_4_5_rca_hadgem$X2020.05.01 + sum_p_4_5_rca_hadgem$X2020.06.01 +
                          sum_p_4_5_rca_hadgem$X2021.03.01 + sum_p_4_5_rca_hadgem$X2021.04.01 + sum_p_4_5_rca_hadgem$X2021.05.01 + sum_p_4_5_rca_hadgem$X2021.06.01)/6
sum_p_4_5_rca_hadgem <- sum_p_4_5_rca_hadgem$X2050.03.01 + sum_p_4_5_rca_hadgem$X2050.04.01 + sum_p_4_5_rca_hadgem$X2050.05.01 + sum_p_4_5_rca_hadgem$X2050.06.01
sum_p_4_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
sum_p_4_5_rca_mpi_2016 <- (sum_p_4_5_rca_mpi$X2016.03.01 + sum_p_4_5_rca_mpi$X2016.04.01 + sum_p_4_5_rca_mpi$X2016.05.01 + sum_p_4_5_rca_mpi$X2016.06.01 +
                          sum_p_4_5_rca_mpi$X2017.03.01 + sum_p_4_5_rca_mpi$X2017.04.01 + sum_p_4_5_rca_mpi$X2017.05.01 + sum_p_4_5_rca_mpi$X2017.06.01 +
                          sum_p_4_5_rca_mpi$X2018.03.01 + sum_p_4_5_rca_mpi$X2018.04.01 + sum_p_4_5_rca_mpi$X2018.05.01 + sum_p_4_5_rca_mpi$X2018.06.01 +
                          sum_p_4_5_rca_mpi$X2019.03.01 + sum_p_4_5_rca_mpi$X2019.04.01 + sum_p_4_5_rca_mpi$X2019.05.01 + sum_p_4_5_rca_mpi$X2019.06.01 +
                          sum_p_4_5_rca_mpi$X2020.03.01 + sum_p_4_5_rca_mpi$X2020.04.01 + sum_p_4_5_rca_mpi$X2020.05.01 + sum_p_4_5_rca_mpi$X2020.06.01 +
                          sum_p_4_5_rca_mpi$X2021.03.01 + sum_p_4_5_rca_mpi$X2021.04.01 + sum_p_4_5_rca_mpi$X2021.05.01 + sum_p_4_5_rca_mpi$X2021.06.01)/6
sum_p_4_5_rca_mpi <- sum_p_4_5_rca_mpi$X2050.03.01 + sum_p_4_5_rca_mpi$X2050.04.01 + sum_p_4_5_rca_mpi$X2050.05.01 + sum_p_4_5_rca_mpi$X2050.06.01
sum_p_4_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
sum_p_4_5_wrf_2016 <- (sum_p_4_5_wrf$X2016.03.01 + sum_p_4_5_wrf$X2016.04.01 + sum_p_4_5_wrf$X2016.05.01 + sum_p_4_5_wrf$X2016.06.01 +
                          sum_p_4_5_wrf$X2017.03.01 + sum_p_4_5_wrf$X2017.04.01 + sum_p_4_5_wrf$X2017.05.01 + sum_p_4_5_wrf$X2017.06.01 +
                          sum_p_4_5_wrf$X2018.03.01 + sum_p_4_5_wrf$X2018.04.01 + sum_p_4_5_wrf$X2018.05.01 + sum_p_4_5_wrf$X2018.06.01 +
                          sum_p_4_5_wrf$X2019.03.01 + sum_p_4_5_wrf$X2019.04.01 + sum_p_4_5_wrf$X2019.05.01 + sum_p_4_5_wrf$X2019.06.01 +
                          sum_p_4_5_wrf$X2020.03.01 + sum_p_4_5_wrf$X2020.04.01 + sum_p_4_5_wrf$X2020.05.01 + sum_p_4_5_wrf$X2020.06.01 +
                          sum_p_4_5_wrf$X2021.03.01 + sum_p_4_5_wrf$X2021.04.01 + sum_p_4_5_wrf$X2021.05.01 + sum_p_4_5_wrf$X2021.06.01)/6
sum_p_4_5_wrf <- sum_p_4_5_wrf$X2050.03.01 + sum_p_4_5_wrf$X2050.04.01 + sum_p_4_5_wrf$X2050.05.01 + sum_p_4_5_wrf$X2050.06.01

sum_p_4_5 <- (sum_p_4_5_cclm + sum_p_4_5_hirham + sum_p_4_5_racmo_ecearth + sum_p_4_5_racmo_hadgem + sum_p_4_5_rca_hadgem + sum_p_4_5_rca_mpi + sum_p_4_5_wrf)/7
sum_p_2016 <- (sum_p_4_5_cclm_2016 + sum_p_4_5_hirham_2016 + sum_p_4_5_racmo_ecearth_2016 + sum_p_4_5_racmo_hadgem_2016 + sum_p_4_5_rca_hadgem_2016 + sum_p_4_5_rca_mpi_2016 + sum_p_4_5_wrf_2016)/7

sum_p_8_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_8_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
sum_p_8_5_cclm <- sum_p_8_5_cclm$X2050.03.01 + sum_p_8_5_cclm$X2050.04.01 + sum_p_8_5_cclm$X2050.05.01 + sum_p_8_5_cclm$X2050.06.01
sum_p_8_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_8_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
sum_p_8_5_hirham <- sum_p_8_5_hirham$X2050.03.01 + sum_p_8_5_hirham$X2050.04.01 + sum_p_8_5_hirham$X2050.05.01 + sum_p_8_5_hirham$X2050.06.01
sum_p_8_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_8_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
sum_p_8_5_racmo_ecearth <- sum_p_8_5_racmo_ecearth$X2050.03.01 + sum_p_8_5_racmo_ecearth$X2050.04.01 + sum_p_8_5_racmo_ecearth$X2050.05.01 + sum_p_8_5_racmo_ecearth$X2050.06.01
sum_p_8_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_8_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
sum_p_8_5_racmo_hadgem <- sum_p_8_5_racmo_hadgem$X2050.03.01 + sum_p_8_5_racmo_hadgem$X2050.04.01 + sum_p_8_5_racmo_hadgem$X2050.05.01 + sum_p_8_5_racmo_hadgem$X2050.06.01
sum_p_8_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_8_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
sum_p_8_5_rca_hadgem <- sum_p_8_5_rca_hadgem$X2050.03.01 + sum_p_8_5_rca_hadgem$X2050.04.01 + sum_p_8_5_rca_hadgem$X2050.05.01 + sum_p_8_5_rca_hadgem$X2050.06.01
sum_p_8_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_8_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
sum_p_8_5_rca_mpi <- sum_p_8_5_rca_mpi$X2050.03.01 + sum_p_8_5_rca_mpi$X2050.04.01 + sum_p_8_5_rca_mpi$X2050.05.01 + sum_p_8_5_rca_mpi$X2050.06.01
sum_p_8_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_8_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
sum_p_8_5_wrf <- sum_p_8_5_wrf$X2050.03.01 + sum_p_8_5_wrf$X2050.04.01 + sum_p_8_5_wrf$X2050.05.01 + sum_p_8_5_wrf$X2050.06.01

sum_p_8_5 <- (sum_p_8_5_cclm + sum_p_8_5_hirham + sum_p_8_5_racmo_ecearth + sum_p_8_5_racmo_hadgem + sum_p_8_5_rca_hadgem + sum_p_8_5_rca_mpi + sum_p_8_5_wrf)/7


max_t_4_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
max_t_4_5_cclm_2016 <- (max_t_4_5_cclm$X2016.03.01 + max_t_4_5_cclm$X2016.04.01 + max_t_4_5_cclm$X2016.05.01 + max_t_4_5_cclm$X2016.06.01 +
                           max_t_4_5_cclm$X2017.03.01 + max_t_4_5_cclm$X2017.04.01 + max_t_4_5_cclm$X2017.05.01 + max_t_4_5_cclm$X2017.06.01 +
                           max_t_4_5_cclm$X2018.03.01 + max_t_4_5_cclm$X2018.04.01 + max_t_4_5_cclm$X2018.05.01 + max_t_4_5_cclm$X2018.06.01 +
                           max_t_4_5_cclm$X2019.03.01 + max_t_4_5_cclm$X2019.04.01 + max_t_4_5_cclm$X2019.05.01 + max_t_4_5_cclm$X2019.06.01 +
                           max_t_4_5_cclm$X2020.03.01 + max_t_4_5_cclm$X2020.04.01 + max_t_4_5_cclm$X2020.05.01 + max_t_4_5_cclm$X2020.06.01 +
                           max_t_4_5_cclm$X2021.03.01 + max_t_4_5_cclm$X2021.04.01 + max_t_4_5_cclm$X2021.05.01 + max_t_4_5_cclm$X2021.06.01)/24
max_t_4_5_cclm <- (max_t_4_5_cclm$X2050.03.01 + max_t_4_5_cclm$X2050.04.01 + max_t_4_5_cclm$X2050.05.01 + max_t_4_5_cclm$X2050.06.01)/4
max_t_4_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
max_t_4_5_hirham_2016 <- (max_t_4_5_hirham$X2016.03.01 + max_t_4_5_hirham$X2016.04.01 + max_t_4_5_hirham$X2016.05.01 + max_t_4_5_hirham$X2016.06.01 +
                             max_t_4_5_hirham$X2017.03.01 + max_t_4_5_hirham$X2017.04.01 + max_t_4_5_hirham$X2017.05.01 + max_t_4_5_hirham$X2017.06.01 +
                             max_t_4_5_hirham$X2018.03.01 + max_t_4_5_hirham$X2018.04.01 + max_t_4_5_hirham$X2018.05.01 + max_t_4_5_hirham$X2018.06.01 +
                             max_t_4_5_hirham$X2019.03.01 + max_t_4_5_hirham$X2019.04.01 + max_t_4_5_hirham$X2019.05.01 + max_t_4_5_hirham$X2019.06.01 +
                             max_t_4_5_hirham$X2020.03.01 + max_t_4_5_hirham$X2020.04.01 + max_t_4_5_hirham$X2020.05.01 + max_t_4_5_hirham$X2020.06.01 +
                             max_t_4_5_hirham$X2021.03.01 + max_t_4_5_hirham$X2021.04.01 + max_t_4_5_hirham$X2021.05.01 + max_t_4_5_hirham$X2021.06.01)/24
max_t_4_5_hirham <- (max_t_4_5_hirham$X2050.03.01 + max_t_4_5_hirham$X2050.04.01 + max_t_4_5_hirham$X2050.05.01 + max_t_4_5_hirham$X2050.06.01)/4
max_t_4_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
max_t_4_5_racmo_ecearth_2016 <- (max_t_4_5_racmo_ecearth$X2016.03.01 + max_t_4_5_racmo_ecearth$X2016.04.01 + max_t_4_5_racmo_ecearth$X2016.05.01 + max_t_4_5_racmo_ecearth$X2016.06.01 +
                                    max_t_4_5_racmo_ecearth$X2017.03.01 + max_t_4_5_racmo_ecearth$X2017.04.01 + max_t_4_5_racmo_ecearth$X2017.05.01 + max_t_4_5_racmo_ecearth$X2017.06.01 +
                                    max_t_4_5_racmo_ecearth$X2018.03.01 + max_t_4_5_racmo_ecearth$X2018.04.01 + max_t_4_5_racmo_ecearth$X2018.05.01 + max_t_4_5_racmo_ecearth$X2018.06.01 +
                                    max_t_4_5_racmo_ecearth$X2019.03.01 + max_t_4_5_racmo_ecearth$X2019.04.01 + max_t_4_5_racmo_ecearth$X2019.05.01 + max_t_4_5_racmo_ecearth$X2019.06.01 +
                                    max_t_4_5_racmo_ecearth$X2020.03.01 + max_t_4_5_racmo_ecearth$X2020.04.01 + max_t_4_5_racmo_ecearth$X2020.05.01 + max_t_4_5_racmo_ecearth$X2020.06.01 +
                                    max_t_4_5_racmo_ecearth$X2021.03.01 + max_t_4_5_racmo_ecearth$X2021.04.01 + max_t_4_5_racmo_ecearth$X2021.05.01 + max_t_4_5_racmo_ecearth$X2021.06.01)/24
max_t_4_5_racmo_ecearth <- (max_t_4_5_racmo_ecearth$X2050.03.01 + max_t_4_5_racmo_ecearth$X2050.04.01 + max_t_4_5_racmo_ecearth$X2050.05.01 + max_t_4_5_racmo_ecearth$X2050.06.01)/4
max_t_4_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
max_t_4_5_racmo_hadgem_2016 <- (max_t_4_5_racmo_hadgem$X2016.03.01 + max_t_4_5_racmo_hadgem$X2016.04.01 + max_t_4_5_racmo_hadgem$X2016.05.01 + max_t_4_5_racmo_hadgem$X2016.06.01 +
                                   max_t_4_5_racmo_hadgem$X2017.03.01 + max_t_4_5_racmo_hadgem$X2017.04.01 + max_t_4_5_racmo_hadgem$X2017.05.01 + max_t_4_5_racmo_hadgem$X2017.06.01 +
                                   max_t_4_5_racmo_hadgem$X2018.03.01 + max_t_4_5_racmo_hadgem$X2018.04.01 + max_t_4_5_racmo_hadgem$X2018.05.01 + max_t_4_5_racmo_hadgem$X2018.06.01 +
                                   max_t_4_5_racmo_hadgem$X2019.03.01 + max_t_4_5_racmo_hadgem$X2019.04.01 + max_t_4_5_racmo_hadgem$X2019.05.01 + max_t_4_5_racmo_hadgem$X2019.06.01 +
                                   max_t_4_5_racmo_hadgem$X2020.03.01 + max_t_4_5_racmo_hadgem$X2020.04.01 + max_t_4_5_racmo_hadgem$X2020.05.01 + max_t_4_5_racmo_hadgem$X2020.06.01 +
                                   max_t_4_5_racmo_hadgem$X2021.03.01 + max_t_4_5_racmo_hadgem$X2021.04.01 + max_t_4_5_racmo_hadgem$X2021.05.01 + max_t_4_5_racmo_hadgem$X2021.06.01)/24
max_t_4_5_racmo_hadgem <- (max_t_4_5_racmo_hadgem$X2050.03.01 + max_t_4_5_racmo_hadgem$X2050.04.01 + max_t_4_5_racmo_hadgem$X2050.05.01 + max_t_4_5_racmo_hadgem$X2050.06.01)/4
max_t_4_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
max_t_4_5_rca_hadgem_2016 <- (max_t_4_5_rca_hadgem$X2016.03.01 + max_t_4_5_rca_hadgem$X2016.04.01 + max_t_4_5_rca_hadgem$X2016.05.01 + max_t_4_5_rca_hadgem$X2016.06.01 +
                                 max_t_4_5_rca_hadgem$X2017.03.01 + max_t_4_5_rca_hadgem$X2017.04.01 + max_t_4_5_rca_hadgem$X2017.05.01 + max_t_4_5_rca_hadgem$X2017.06.01 +
                                 max_t_4_5_rca_hadgem$X2018.03.01 + max_t_4_5_rca_hadgem$X2018.04.01 + max_t_4_5_rca_hadgem$X2018.05.01 + max_t_4_5_rca_hadgem$X2018.06.01 +
                                 max_t_4_5_rca_hadgem$X2019.03.01 + max_t_4_5_rca_hadgem$X2019.04.01 + max_t_4_5_rca_hadgem$X2019.05.01 + max_t_4_5_rca_hadgem$X2019.06.01 +
                                 max_t_4_5_rca_hadgem$X2020.03.01 + max_t_4_5_rca_hadgem$X2020.04.01 + max_t_4_5_rca_hadgem$X2020.05.01 + max_t_4_5_rca_hadgem$X2020.06.01 +
                                 max_t_4_5_rca_hadgem$X2021.03.01 + max_t_4_5_rca_hadgem$X2021.04.01 + max_t_4_5_rca_hadgem$X2021.05.01 + max_t_4_5_rca_hadgem$X2021.06.01)/24
max_t_4_5_rca_hadgem <- (max_t_4_5_rca_hadgem$X2050.03.01 + max_t_4_5_rca_hadgem$X2050.04.01 + max_t_4_5_rca_hadgem$X2050.05.01 + max_t_4_5_rca_hadgem$X2050.06.01)/4
max_t_4_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
max_t_4_5_rca_mpi_2016 <- (max_t_4_5_rca_mpi$X2016.03.01 + max_t_4_5_rca_mpi$X2016.04.01 + max_t_4_5_rca_mpi$X2016.05.01 + max_t_4_5_rca_mpi$X2016.06.01 +
                              max_t_4_5_rca_mpi$X2017.03.01 + max_t_4_5_rca_mpi$X2017.04.01 + max_t_4_5_rca_mpi$X2017.05.01 + max_t_4_5_rca_mpi$X2017.06.01 +
                              max_t_4_5_rca_mpi$X2018.03.01 + max_t_4_5_rca_mpi$X2018.04.01 + max_t_4_5_rca_mpi$X2018.05.01 + max_t_4_5_rca_mpi$X2018.06.01 +
                              max_t_4_5_rca_mpi$X2019.03.01 + max_t_4_5_rca_mpi$X2019.04.01 + max_t_4_5_rca_mpi$X2019.05.01 + max_t_4_5_rca_mpi$X2019.06.01 +
                              max_t_4_5_rca_mpi$X2020.03.01 + max_t_4_5_rca_mpi$X2020.04.01 + max_t_4_5_rca_mpi$X2020.05.01 + max_t_4_5_rca_mpi$X2020.06.01 +
                              max_t_4_5_rca_mpi$X2021.03.01 + max_t_4_5_rca_mpi$X2021.04.01 + max_t_4_5_rca_mpi$X2021.05.01 + max_t_4_5_rca_mpi$X2021.06.01)/24
max_t_4_5_rca_mpi <- (max_t_4_5_rca_mpi$X2050.03.01 + max_t_4_5_rca_mpi$X2050.04.01 + max_t_4_5_rca_mpi$X2050.05.01 + max_t_4_5_rca_mpi$X2050.06.01)/4
max_t_4_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
max_t_4_5_wrf_2016 <- (max_t_4_5_wrf$X2016.03.01 + max_t_4_5_wrf$X2016.04.01 + max_t_4_5_wrf$X2016.05.01 + max_t_4_5_wrf$X2016.06.01 +
                          max_t_4_5_wrf$X2017.03.01 + max_t_4_5_wrf$X2017.04.01 + max_t_4_5_wrf$X2017.05.01 + max_t_4_5_wrf$X2017.06.01 +
                          max_t_4_5_wrf$X2018.03.01 + max_t_4_5_wrf$X2018.04.01 + max_t_4_5_wrf$X2018.05.01 + max_t_4_5_wrf$X2018.06.01 +
                          max_t_4_5_wrf$X2019.03.01 + max_t_4_5_wrf$X2019.04.01 + max_t_4_5_wrf$X2019.05.01 + max_t_4_5_wrf$X2019.06.01 +
                          max_t_4_5_wrf$X2020.03.01 + max_t_4_5_wrf$X2020.04.01 + max_t_4_5_wrf$X2020.05.01 + max_t_4_5_wrf$X2020.06.01 +
                          max_t_4_5_wrf$X2021.03.01 + max_t_4_5_wrf$X2021.04.01 + max_t_4_5_wrf$X2021.05.01 + max_t_4_5_wrf$X2021.06.01)/24
max_t_4_5_wrf <- (max_t_4_5_wrf$X2050.03.01 + max_t_4_5_wrf$X2050.04.01 + max_t_4_5_wrf$X2050.05.01 + max_t_4_5_wrf$X2050.06.01)/4

max_t_4_5 <- (max_t_4_5_cclm + max_t_4_5_hirham + max_t_4_5_racmo_ecearth + max_t_4_5_racmo_hadgem + max_t_4_5_rca_hadgem + max_t_4_5_rca_mpi + max_t_4_5_wrf)/7 - 272.15
max_t_2016 <- (max_t_4_5_cclm_2016 + max_t_4_5_hirham_2016 + max_t_4_5_racmo_ecearth_2016 + max_t_4_5_racmo_hadgem_2016 + max_t_4_5_rca_hadgem_2016 + max_t_4_5_rca_mpi_2016 + max_t_4_5_wrf_2016)/7 - 272.15

min_t_4_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
min_t_4_5_cclm_2016 <- (min_t_4_5_cclm$X2016.03.01 + min_t_4_5_cclm$X2016.04.01 + min_t_4_5_cclm$X2016.05.01 + min_t_4_5_cclm$X2016.06.01 +
                          min_t_4_5_cclm$X2017.03.01 + min_t_4_5_cclm$X2017.04.01 + min_t_4_5_cclm$X2017.05.01 + min_t_4_5_cclm$X2017.06.01 +
                          min_t_4_5_cclm$X2018.03.01 + min_t_4_5_cclm$X2018.04.01 + min_t_4_5_cclm$X2018.05.01 + min_t_4_5_cclm$X2018.06.01 +
                          min_t_4_5_cclm$X2019.03.01 + min_t_4_5_cclm$X2019.04.01 + min_t_4_5_cclm$X2019.05.01 + min_t_4_5_cclm$X2019.06.01 +
                          min_t_4_5_cclm$X2020.03.01 + min_t_4_5_cclm$X2020.04.01 + min_t_4_5_cclm$X2020.05.01 + min_t_4_5_cclm$X2020.06.01 +
                          min_t_4_5_cclm$X2021.03.01 + min_t_4_5_cclm$X2021.04.01 + min_t_4_5_cclm$X2021.05.01 + min_t_4_5_cclm$X2021.06.01)/24
min_t_4_5_cclm <- (min_t_4_5_cclm$X2050.03.01 + min_t_4_5_cclm$X2050.04.01 + min_t_4_5_cclm$X2050.05.01 + min_t_4_5_cclm$X2050.06.01)/4
min_t_4_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
min_t_4_5_hirham_2016 <- (min_t_4_5_hirham$X2016.03.01 + min_t_4_5_hirham$X2016.04.01 + min_t_4_5_hirham$X2016.05.01 + min_t_4_5_hirham$X2016.06.01 +
                            min_t_4_5_hirham$X2017.03.01 + min_t_4_5_hirham$X2017.04.01 + min_t_4_5_hirham$X2017.05.01 + min_t_4_5_hirham$X2017.06.01 +
                            min_t_4_5_hirham$X2018.03.01 + min_t_4_5_hirham$X2018.04.01 + min_t_4_5_hirham$X2018.05.01 + min_t_4_5_hirham$X2018.06.01 +
                            min_t_4_5_hirham$X2019.03.01 + min_t_4_5_hirham$X2019.04.01 + min_t_4_5_hirham$X2019.05.01 + min_t_4_5_hirham$X2019.06.01 +
                            min_t_4_5_hirham$X2020.03.01 + min_t_4_5_hirham$X2020.04.01 + min_t_4_5_hirham$X2020.05.01 + min_t_4_5_hirham$X2020.06.01 +
                            min_t_4_5_hirham$X2021.03.01 + min_t_4_5_hirham$X2021.04.01 + min_t_4_5_hirham$X2021.05.01 + min_t_4_5_hirham$X2021.06.01)/24
min_t_4_5_hirham <- (min_t_4_5_hirham$X2050.03.01 + min_t_4_5_hirham$X2050.04.01 + min_t_4_5_hirham$X2050.05.01 + min_t_4_5_hirham$X2050.06.01)/4
min_t_4_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
min_t_4_5_racmo_ecearth_2016 <- (min_t_4_5_racmo_ecearth$X2016.03.01 + min_t_4_5_racmo_ecearth$X2016.04.01 + min_t_4_5_racmo_ecearth$X2016.05.01 + min_t_4_5_racmo_ecearth$X2016.06.01 +
                                   min_t_4_5_racmo_ecearth$X2017.03.01 + min_t_4_5_racmo_ecearth$X2017.04.01 + min_t_4_5_racmo_ecearth$X2017.05.01 + min_t_4_5_racmo_ecearth$X2017.06.01 +
                                   min_t_4_5_racmo_ecearth$X2018.03.01 + min_t_4_5_racmo_ecearth$X2018.04.01 + min_t_4_5_racmo_ecearth$X2018.05.01 + min_t_4_5_racmo_ecearth$X2018.06.01 +
                                   min_t_4_5_racmo_ecearth$X2019.03.01 + min_t_4_5_racmo_ecearth$X2019.04.01 + min_t_4_5_racmo_ecearth$X2019.05.01 + min_t_4_5_racmo_ecearth$X2019.06.01 +
                                   min_t_4_5_racmo_ecearth$X2020.03.01 + min_t_4_5_racmo_ecearth$X2020.04.01 + min_t_4_5_racmo_ecearth$X2020.05.01 + min_t_4_5_racmo_ecearth$X2020.06.01 +
                                   min_t_4_5_racmo_ecearth$X2021.03.01 + min_t_4_5_racmo_ecearth$X2021.04.01 + min_t_4_5_racmo_ecearth$X2021.05.01 + min_t_4_5_racmo_ecearth$X2021.06.01)/24
min_t_4_5_racmo_ecearth <- (min_t_4_5_racmo_ecearth$X2050.03.01 + min_t_4_5_racmo_ecearth$X2050.04.01 + min_t_4_5_racmo_ecearth$X2050.05.01 + min_t_4_5_racmo_ecearth$X2050.06.01)/4
min_t_4_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
min_t_4_5_racmo_hadgem_2016 <- (min_t_4_5_racmo_hadgem$X2016.03.01 + min_t_4_5_racmo_hadgem$X2016.04.01 + min_t_4_5_racmo_hadgem$X2016.05.01 + min_t_4_5_racmo_hadgem$X2016.06.01 +
                                  min_t_4_5_racmo_hadgem$X2017.03.01 + min_t_4_5_racmo_hadgem$X2017.04.01 + min_t_4_5_racmo_hadgem$X2017.05.01 + min_t_4_5_racmo_hadgem$X2017.06.01 +
                                  min_t_4_5_racmo_hadgem$X2018.03.01 + min_t_4_5_racmo_hadgem$X2018.04.01 + min_t_4_5_racmo_hadgem$X2018.05.01 + min_t_4_5_racmo_hadgem$X2018.06.01 +
                                  min_t_4_5_racmo_hadgem$X2019.03.01 + min_t_4_5_racmo_hadgem$X2019.04.01 + min_t_4_5_racmo_hadgem$X2019.05.01 + min_t_4_5_racmo_hadgem$X2019.06.01 +
                                  min_t_4_5_racmo_hadgem$X2020.03.01 + min_t_4_5_racmo_hadgem$X2020.04.01 + min_t_4_5_racmo_hadgem$X2020.05.01 + min_t_4_5_racmo_hadgem$X2020.06.01 +
                                  min_t_4_5_racmo_hadgem$X2021.03.01 + min_t_4_5_racmo_hadgem$X2021.04.01 + min_t_4_5_racmo_hadgem$X2021.05.01 + min_t_4_5_racmo_hadgem$X2021.06.01)/24
min_t_4_5_racmo_hadgem <- (min_t_4_5_racmo_hadgem$X2050.03.01 + min_t_4_5_racmo_hadgem$X2050.04.01 + min_t_4_5_racmo_hadgem$X2050.05.01 + min_t_4_5_racmo_hadgem$X2050.06.01)/4
min_t_4_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
min_t_4_5_rca_hadgem_2016 <- (min_t_4_5_rca_hadgem$X2016.03.01 + min_t_4_5_rca_hadgem$X2016.04.01 + min_t_4_5_rca_hadgem$X2016.05.01 + min_t_4_5_rca_hadgem$X2016.06.01 +
                                min_t_4_5_rca_hadgem$X2017.03.01 + min_t_4_5_rca_hadgem$X2017.04.01 + min_t_4_5_rca_hadgem$X2017.05.01 + min_t_4_5_rca_hadgem$X2017.06.01 +
                                min_t_4_5_rca_hadgem$X2018.03.01 + min_t_4_5_rca_hadgem$X2018.04.01 + min_t_4_5_rca_hadgem$X2018.05.01 + min_t_4_5_rca_hadgem$X2018.06.01 +
                                min_t_4_5_rca_hadgem$X2019.03.01 + min_t_4_5_rca_hadgem$X2019.04.01 + min_t_4_5_rca_hadgem$X2019.05.01 + min_t_4_5_rca_hadgem$X2019.06.01 +
                                min_t_4_5_rca_hadgem$X2020.03.01 + min_t_4_5_rca_hadgem$X2020.04.01 + min_t_4_5_rca_hadgem$X2020.05.01 + min_t_4_5_rca_hadgem$X2020.06.01 +
                                min_t_4_5_rca_hadgem$X2021.03.01 + min_t_4_5_rca_hadgem$X2021.04.01 + min_t_4_5_rca_hadgem$X2021.05.01 + min_t_4_5_rca_hadgem$X2021.06.01)/24
min_t_4_5_rca_hadgem <- (min_t_4_5_rca_hadgem$X2050.03.01 + min_t_4_5_rca_hadgem$X2050.04.01 + min_t_4_5_rca_hadgem$X2050.05.01 + min_t_4_5_rca_hadgem$X2050.06.01)/4
min_t_4_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
min_t_4_5_rca_mpi_2016 <- (min_t_4_5_rca_mpi$X2016.03.01 + min_t_4_5_rca_mpi$X2016.04.01 + min_t_4_5_rca_mpi$X2016.05.01 + min_t_4_5_rca_mpi$X2016.06.01 +
                             min_t_4_5_rca_mpi$X2017.03.01 + min_t_4_5_rca_mpi$X2017.04.01 + min_t_4_5_rca_mpi$X2017.05.01 + min_t_4_5_rca_mpi$X2017.06.01 +
                             min_t_4_5_rca_mpi$X2018.03.01 + min_t_4_5_rca_mpi$X2018.04.01 + min_t_4_5_rca_mpi$X2018.05.01 + min_t_4_5_rca_mpi$X2018.06.01 +
                             min_t_4_5_rca_mpi$X2019.03.01 + min_t_4_5_rca_mpi$X2019.04.01 + min_t_4_5_rca_mpi$X2019.05.01 + min_t_4_5_rca_mpi$X2019.06.01 +
                             min_t_4_5_rca_mpi$X2020.03.01 + min_t_4_5_rca_mpi$X2020.04.01 + min_t_4_5_rca_mpi$X2020.05.01 + min_t_4_5_rca_mpi$X2020.06.01 +
                             min_t_4_5_rca_mpi$X2021.03.01 + min_t_4_5_rca_mpi$X2021.04.01 + min_t_4_5_rca_mpi$X2021.05.01 + min_t_4_5_rca_mpi$X2021.06.01)/24
min_t_4_5_rca_mpi <- (min_t_4_5_rca_mpi$X2050.03.01 + min_t_4_5_rca_mpi$X2050.04.01 + min_t_4_5_rca_mpi$X2050.05.01 + min_t_4_5_rca_mpi$X2050.06.01)/4
min_t_4_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
min_t_4_5_wrf_2016 <- (min_t_4_5_wrf$X2016.03.01 + min_t_4_5_wrf$X2016.04.01 + min_t_4_5_wrf$X2016.05.01 + min_t_4_5_wrf$X2016.06.01 +
                         min_t_4_5_wrf$X2017.03.01 + min_t_4_5_wrf$X2017.04.01 + min_t_4_5_wrf$X2017.05.01 + min_t_4_5_wrf$X2017.06.01 +
                         min_t_4_5_wrf$X2018.03.01 + min_t_4_5_wrf$X2018.04.01 + min_t_4_5_wrf$X2018.05.01 + min_t_4_5_wrf$X2018.06.01 +
                         min_t_4_5_wrf$X2019.03.01 + min_t_4_5_wrf$X2019.04.01 + min_t_4_5_wrf$X2019.05.01 + min_t_4_5_wrf$X2019.06.01 +
                         min_t_4_5_wrf$X2020.03.01 + min_t_4_5_wrf$X2020.04.01 + min_t_4_5_wrf$X2020.05.01 + min_t_4_5_wrf$X2020.06.01 +
                         min_t_4_5_wrf$X2021.03.01 + min_t_4_5_wrf$X2021.04.01 + min_t_4_5_wrf$X2021.05.01 + min_t_4_5_wrf$X2021.06.01)/24
min_t_4_5_wrf <- (min_t_4_5_wrf$X2050.03.01 + min_t_4_5_wrf$X2050.04.01 + min_t_4_5_wrf$X2050.05.01 + min_t_4_5_wrf$X2050.06.01)/4

min_t_4_5 <- (min_t_4_5_cclm + min_t_4_5_hirham + min_t_4_5_racmo_ecearth + min_t_4_5_racmo_hadgem + min_t_4_5_rca_hadgem + min_t_4_5_rca_mpi + min_t_4_5_wrf)/7 - 272.15
min_t_2016 <- (min_t_4_5_cclm_2016 + min_t_4_5_hirham_2016 + min_t_4_5_racmo_ecearth_2016 + min_t_4_5_racmo_hadgem_2016 + min_t_4_5_rca_hadgem_2016 + min_t_4_5_rca_mpi_2016 + min_t_4_5_wrf_2016)/7 - 272.15

var_t_4_5 <- max_t_4_5-min_t_4_5
var_t_2016 <- max_t_2016-min_t_2016

max_t_8_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_8_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
max_t_8_5_cclm <- (max_t_8_5_cclm$X2050.03.01 + max_t_8_5_cclm$X2050.04.01 + max_t_8_5_cclm$X2050.05.01 + max_t_8_5_cclm$X2050.06.01)/4
max_t_8_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_8_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
max_t_8_5_hirham <- (max_t_8_5_hirham$X2050.03.01 + max_t_8_5_hirham$X2050.04.01 + max_t_8_5_hirham$X2050.05.01 + max_t_8_5_hirham$X2050.06.01)/4
max_t_8_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_8_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
max_t_8_5_racmo_ecearth <- (max_t_8_5_racmo_ecearth$X2050.03.01 + max_t_8_5_racmo_ecearth$X2050.04.01 + max_t_8_5_racmo_ecearth$X2050.05.01 + max_t_8_5_racmo_ecearth$X2050.06.01)/4
max_t_8_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_8_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
max_t_8_5_racmo_hadgem <- (max_t_8_5_racmo_hadgem$X2050.03.01 + max_t_8_5_racmo_hadgem$X2050.04.01 + max_t_8_5_racmo_hadgem$X2050.05.01 + max_t_8_5_racmo_hadgem$X2050.06.01)/4
max_t_8_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_8_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
max_t_8_5_rca_hadgem <- (max_t_8_5_rca_hadgem$X2050.03.01 + max_t_8_5_rca_hadgem$X2050.04.01 + max_t_8_5_rca_hadgem$X2050.05.01 + max_t_8_5_rca_hadgem$X2050.06.01)/4
max_t_8_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_8_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
max_t_8_5_rca_mpi <- (max_t_8_5_rca_mpi$X2050.03.01 + max_t_8_5_rca_mpi$X2050.04.01 + max_t_8_5_rca_mpi$X2050.05.01 + max_t_8_5_rca_mpi$X2050.06.01)/4
max_t_8_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_8_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
max_t_8_5_wrf <- (max_t_8_5_wrf$X2050.03.01 + max_t_8_5_wrf$X2050.04.01 + max_t_8_5_wrf$X2050.05.01 + max_t_8_5_wrf$X2050.06.01)/4

max_t_8_5 <- (max_t_8_5_cclm + max_t_8_5_hirham + max_t_8_5_racmo_ecearth + max_t_8_5_racmo_hadgem + max_t_8_5_rca_hadgem + max_t_8_5_rca_mpi + max_t_8_5_wrf)/7 - 272.15

min_t_8_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_8_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
min_t_8_5_cclm <- (min_t_8_5_cclm$X2050.03.01 + min_t_8_5_cclm$X2050.04.01 + min_t_8_5_cclm$X2050.05.01 + min_t_8_5_cclm$X2050.06.01)/4
min_t_8_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_8_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
min_t_8_5_hirham <- (min_t_8_5_hirham$X2050.03.01 + min_t_8_5_hirham$X2050.04.01 + min_t_8_5_hirham$X2050.05.01 + min_t_8_5_hirham$X2050.06.01)/4
min_t_8_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_8_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
min_t_8_5_racmo_ecearth <- (min_t_8_5_racmo_ecearth$X2050.03.01 + min_t_8_5_racmo_ecearth$X2050.04.01 + min_t_8_5_racmo_ecearth$X2050.05.01 + min_t_8_5_racmo_ecearth$X2050.06.01)/4
min_t_8_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_8_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
min_t_8_5_racmo_hadgem <- (min_t_8_5_racmo_hadgem$X2050.03.01 + min_t_8_5_racmo_hadgem$X2050.04.01 + min_t_8_5_racmo_hadgem$X2050.05.01 + min_t_8_5_racmo_hadgem$X2050.06.01)/4
min_t_8_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_8_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
min_t_8_5_rca_hadgem <- (min_t_8_5_rca_hadgem$X2050.03.01 + min_t_8_5_rca_hadgem$X2050.04.01 + min_t_8_5_rca_hadgem$X2050.05.01 + min_t_8_5_rca_hadgem$X2050.06.01)/4
min_t_8_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_8_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
min_t_8_5_rca_mpi <- (min_t_8_5_rca_mpi$X2050.03.01 + min_t_8_5_rca_mpi$X2050.04.01 + min_t_8_5_rca_mpi$X2050.05.01 + min_t_8_5_rca_mpi$X2050.06.01)/4
min_t_8_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_8_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
min_t_8_5_wrf <- (min_t_8_5_wrf$X2050.03.01 + min_t_8_5_wrf$X2050.04.01 + min_t_8_5_wrf$X2050.05.01 + min_t_8_5_wrf$X2050.06.01)/4

min_t_8_5 <- (min_t_8_5_cclm + min_t_8_5_hirham + min_t_8_5_racmo_ecearth + min_t_8_5_racmo_hadgem + min_t_8_5_rca_hadgem + min_t_8_5_rca_mpi + min_t_8_5_wrf)/7 - 272.15

var_t_8_5 <- max_t_8_5-min_t_8_5


climate_pls <- data.frame(PLS = c(as.character(c(1:19,21:25)),"europe"),
                          mean_t_2016=c(exact_extract(mean_t_2016,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(mean_t_2016,grid_eu_mainland_outline, fun="mean")),
                          mean_t_4_5=c(exact_extract(mean_t_4_5,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(mean_t_4_5,grid_eu_mainland_outline, fun="mean")),
                          mean_t_8_5=c(exact_extract(mean_t_8_5,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(mean_t_8_5,grid_eu_mainland_outline, fun="mean")),
                          sum_p_2016=c(exact_extract(sum_p_2016,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(sum_p_2016,grid_eu_mainland_outline, fun="mean")),
                          sum_p_4_5=c(exact_extract(sum_p_4_5,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(sum_p_4_5,grid_eu_mainland_outline, fun="mean")),
                          sum_p_8_5=c(exact_extract(sum_p_8_5,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(sum_p_8_5,grid_eu_mainland_outline, fun="mean")),
                          var_t_2016=c(exact_extract(var_t_2016,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(var_t_2016,grid_eu_mainland_outline, fun="mean")),
                          var_t_4_5=c(exact_extract(var_t_4_5,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(var_t_4_5,grid_eu_mainland_outline, fun="mean")),
                          var_t_8_5=c(exact_extract(var_t_8_5,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="mean"),exact_extract(var_t_8_5,grid_eu_mainland_outline, fun="mean")))

saveRDS(climate_pls,"output/climate_pls.rds")

# represent changes

plot_scenario <- reshape2::melt(climate_pls[which(climate_pls$PLS=="europe"),c("PLS","mean_t_2016","mean_t_4_5","sum_p_2016","sum_p_4_5","var_t_2016","var_t_4_5")],id.vars="PLS")
plot_scenario <- data.frame(PLS="europe", variable=c("Temperature","Precipitation","Temperature variance"), initial=plot_scenario$value[which(plot_scenario$variable %in% c("mean_t_2016","var_t_2016","sum_p_2016"))],
                            nac=plot_scenario$value[which(plot_scenario$variable %in% c("mean_t_4_5","var_t_4_5","sum_p_4_5"))])
plot_scenario$nfn <- plot_scenario$nfs <- plot_scenario$ssp1 <- plot_scenario$ssp3 <- plot_scenario$nac
plot_scenario <- rbind(lulc_pls_short[which(lulc_pls_short$PLS=="europe"),],pa_pls_short[which(pa_pls_short$PLS=="europe"),],plot_scenario)
plot_scenario$PLS <- plot_scenario$ssp3 <- NULL
plot_scenario <- rbind(plot_scenario,data.frame(variable = "farmland",t(apply(plot_scenario[which(plot_scenario$variable %in% c("farmland_low","farmland_medium","farmland_high")),-1],2,sum))),
                       data.frame(variable = "forest",t(apply(plot_scenario[which(plot_scenario$variable %in% c("forest_lowmedium","forest_high")),-1],2,sum))))
plot_scenario$nac <- plot_scenario$nac - plot_scenario$initial
plot_scenario$nfn <- plot_scenario$nfn - plot_scenario$initial
plot_scenario$nfs <- plot_scenario$nfs - plot_scenario$initial
plot_scenario$ssp1 <- plot_scenario$ssp1 - plot_scenario$initial
plot_scenario$initial <- NULL
plot_scenario <- reshape2::melt(plot_scenario, id.vars="variable")
names(plot_scenario)[2] <- "scenario"


plot_scenario <- data.frame(scenario = c("BAU","SSP1","NAC","NFN","NFS"),
                            d_impervious = c(mean(press_mainland_trend$d_impervious, na.rm=TRUE),
                                             mean(press_mainland_trend$impervious_2018)*(lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018),
                                             mean(press_mainland_trend$impervious_2018)*(lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018),
                                             mean(press_mainland_trend$impervious_2018)*(lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018),
                                             mean(press_mainland_trend$impervious_2018)*(lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018)),
                            d_shannon = c(mean(press_mainland_trend$d_shannon, na.rm=TRUE),
                                          mean(press_mainland_trend$shannon_2018)*(lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018),
                                          mean(press_mainland_trend$shannon_2018)*(lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018),
                                          mean(press_mainland_trend$shannon_2018)*(lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018),
                                          mean(press_mainland_trend$shannon_2018)*(lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018)),
                            protectedarea_perc = c(mean(press_mainland_trend$protectedarea_perc),
                                                   mean(press_mainland_trend$protectedarea_perc)*pa_pls_short$ssp1[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")],
                                                   mean(press_mainland_trend$protectedarea_perc)*pa_pls_short$nac[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")],
                                                   mean(press_mainland_trend$protectedarea_perc)*pa_pls_short$nfn[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")],
                                                   mean(press_mainland_trend$protectedarea_perc)*pa_pls_short$nfs[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")]),
                            d_agri = c(mean(press_mainland_trend$d_agri, na.rm=TRUE),
                                       mean(press_mainland_trend$agri_2018)*(sum(lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018),
                                       mean(press_mainland_trend$agri_2018)*(sum(lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018),
                                       mean(press_mainland_trend$agri_2018)*(sum(lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018),
                                       mean(press_mainland_trend$agri_2018)*(sum(lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)),
                            agri_low = c(mean(press_mainland_trend$eulandsystem_farmland_low),
                                         mean(press_mainland_trend$eulandsystem_farmland_low)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))],
                                         mean(press_mainland_trend$eulandsystem_farmland_low)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))],
                                         mean(press_mainland_trend$eulandsystem_farmland_low)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))],
                                         mean(press_mainland_trend$eulandsystem_farmland_low)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]),
                            agri_medium = c(mean(press_mainland_trend$eulandsystem_farmland_medium),
                                            mean(press_mainland_trend$eulandsystem_farmland_medium)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))],
                                            mean(press_mainland_trend$eulandsystem_farmland_medium)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))],
                                            mean(press_mainland_trend$eulandsystem_farmland_medium)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))],
                                            mean(press_mainland_trend$eulandsystem_farmland_medium)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]),
                            agri_high = c(mean(press_mainland_trend$eulandsystem_farmland_high),
                                          mean(press_mainland_trend$eulandsystem_farmland_high)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))],
                                          mean(press_mainland_trend$eulandsystem_farmland_high)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))],
                                          mean(press_mainland_trend$eulandsystem_farmland_high)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))],
                                          mean(press_mainland_trend$eulandsystem_farmland_high)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]),
                            d_treedensity = c(mean(press_mainland_trend$d_treedensity, na.rm=TRUE),
                                              mean(press_mainland_trend$treedensity_2018)*(sum(lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018),
                                              mean(press_mainland_trend$treedensity_2018)*(sum(lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018),
                                              mean(press_mainland_trend$treedensity_2018)*(sum(lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018),
                                              mean(press_mainland_trend$treedensity_2018)*(sum(lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)),
                            forest_lowmedium = c(mean(press_mainland_trend$eulandsystem_forest_lowmedium),
                                                 mean(press_mainland_trend$eulandsystem_forest_lowmedium)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))],
                                                 mean(press_mainland_trend$eulandsystem_forest_lowmedium)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))],
                                                 mean(press_mainland_trend$eulandsystem_forest_lowmedium)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))],
                                                 mean(press_mainland_trend$eulandsystem_forest_lowmedium)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]),
                            forest_high = c(mean(press_mainland_trend$eulandsystem_forest_high),
                                            mean(press_mainland_trend$eulandsystem_forest_high)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))],
                                            mean(press_mainland_trend$eulandsystem_forest_high)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))],
                                            mean(press_mainland_trend$eulandsystem_forest_high)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))],
                                            mean(press_mainland_trend$eulandsystem_forest_high)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]),
                            d_tempspring = c(rep(mean(press_mainland_trend$tempspring_2020, na.rm =TRUE)*(climate_pls$mean_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$mean_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018),5)),
                            d_tempspringvar = c(rep(mean(press_mainland_trend$tempspringvar_2020, na.rm =TRUE)*(climate_pls$var_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$var_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018),5)),
                            d_precspring = c(rep(mean(press_mainland_trend$precspring_2020, na.rm =TRUE)*(climate_pls$sum_p_4_5[which(climate_pls$PLS=="europe")]/climate_pls$sum_p_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018),5))
                            )
plot_scenario <- reshape2::melt(plot_scenario, id.vars="scenario")
plot_scenario$scenario <- factor(plot_scenario$scenario, levels = c("BAU","SSP1","NFN","NFS","NAC"))
plot_scenario$variable <- factor(plot_scenario$variable, levels = c("d_impervious","d_agri","agri_high","agri_medium","agri_low",
                                                                    "d_treedensity","forest_high","forest_lowmedium",
                                                                    "d_shannon","protectedarea_perc","d_tempspring","d_tempspringvar","d_precspring"))
plot_scenario$variable <- recode(plot_scenario$variable, d_impervious = "D urbanisation",d_agri="D agricultural surface",agri_high = "High intensive farmland",
                                 agri_medium = "Medium intensive farmland", agri_low = "Low intensive farmland", d_treedensity = "D tree density",
                                 forest_high = "High intensive forest", forest_lowmedium = "Low/medium intensive forest", d_shannon = "D landscape diversity",
                                 protectedarea_perc = "Protected area percent", d_tempspring ="D temperature", d_tempspringvar = "D temperature variation", d_precspring= "D precipitation")

ggplot(plot_scenario[which(plot_scenario$variable %in% c("D urbanisation","D agricultural surface","D tree density","D landscape diversity")),], aes(fill=scenario, y=value, x=scenario)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  facet_wrap(~variable, scales="free_y", nrow=1) +
  theme_minimal() +
  theme(legend.position="none") + xlab("") + ylab("")

ggsave("output/scenario_detail1.png",
       width = 10,
       height = 4,
       dpi = 300
)

ggplot(plot_scenario[which(plot_scenario$variable %in% c("Low intensive farmland","Medium intensive farmland","High intensive farmland")),], aes(fill=scenario, y=value, x=scenario)) + 
  geom_bar(position="fill", stat="identity", aes(alpha=variable)) +
  scale_fill_manual(values = c("BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_manual (values = c("Low intensive farmland"=1,"Medium intensive farmland"=0.7,"High intensive farmland"=0.5)) +
  theme_minimal() +
  theme(legend.position="none") + xlab("") + ylab("")

ggsave("output/scenario_detail_agri.png",
       width = 2.5,
       height = 4,
       dpi = 300
)

ggplot(plot_scenario[which(plot_scenario$variable %in% c("Low/medium intensive forest","High intensive forest")),], aes(fill=scenario, y=value, x=scenario)) + 
  geom_bar(position="fill", stat="identity", aes(alpha=variable)) +
  scale_fill_manual(values = c("BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_manual (values = c("Low/medium intensive forest" = 1,"High intensive forest" = 0.5)) +
  theme_minimal() +
  theme(legend.position="none") + xlab("") + ylab("")

ggsave("output/scenario_detail_foret.png",
       width = 2.5,
       height = 4,
       dpi = 300
)

ggplot(plot_scenario[which(plot_scenario$variable %in% c("Protected area percent")),], aes(fill=scenario, y=value, x=scenario)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  theme_minimal() +
  theme(legend.position="none") + xlab("") + ylab("")

ggsave("output/scenario_detail_pa.png",
       width = 2.5,
       height = 4,
       dpi = 300
)

ggplot(plot_scenario[which(plot_scenario$variable %in% c("D temperature", "D temperature variation", "D precipitation")),], aes(y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity") + theme_minimal() +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("") + ylab("")

ggsave("output/scenario_detail_climat.png",
       width = 2.5,
       height = 4,
       dpi = 300
)


pls_scenario <- climate_pls
pls_scenario$temp_change <- pls_scenario$mean_t_4_5-pls_scenario$mean_t_2016
pls_scenario$tempvar_change <- pls_scenario$var_t_4_5-pls_scenario$var_t_2016
pls_scenario$prec_change <- pls_scenario$sum_p_4_5-pls_scenario$sum_p_2016
pls_scenario <- pls_scenario[,c("PLS","temp_change","tempvar_change","prec_change")]

pls_scenario2 <- lulc_pls_short_farm_for
pls_scenario2$change <- pls_scenario2$nfn - pls_scenario2$initial
pls_scenario2 <- pls_scenario2[,c("PLS","variable","change")]
pls_scenario2 <- reshape2::dcast(pls_scenario2, PLS~variable, value.var="change")

pls_scenario3 <- pa_pls_short
pls_scenario3$pa_change <- pls_scenario3$nfn - pls_scenario3$initial

pls_scenario_all <- merge(pls_scenario,pls_scenario2,by="PLS")
pls_scenario_all <- merge(pls_scenario_all,pls_scenario3[,c("PLS","pa_change")],by="PLS")

value_pls <- merge(pls_scenario_all,overall_trend_farmland[,c("PLS","mu_nfn_signif")],by="PLS", all.x=TRUE)

ACP <- rda(pls_scenario_all[-1], scale=TRUE)
MVA.synt(ACP)
MVA.plot(ACP)
MVA.plot(ACP, fac = sign(log(value_pls$mu_nfn_signif)), col=c("red","blue"))
MVA.plot(ACP,"corr")

## lda
exp_impact_FaB <- c("green","green","red","red","green","red","green","red","red","red","red","grey","green")
exp_impact_FoB <- c("green","green","red","red","red","green","red","red","red","red","red","red","green")
exp_impact_GB <- c("red","grey","red","green","green","grey","green","green","red","grey","green","green","green")
exp_impact_WB <- c("green","green","grey","green","grey","grey","green","grey","grey","grey","red","red","red")

lda_data <- pls_scenario_all[which(!is.na(value_pls$mu_nfn_signif)),c("temp_change","tempvar_change","prec_change","urban","farmland","forest","farmland_low",
                                                                      "farmland_medium","farmland_high","forest_lowmedium","forest_high","landscape_div","pa_change")]
names(lda_data) <- c("Temperature change","Temperature variance change","Precipitation change","Urbanisation","Agricultural surface change","Forest cover change","Low intensive farmland change",
                     "Medium intensive farmland change","High intensive farmland change","Low/medium intensive forest change","High intensive forest change","Landscape diversity change","Protected area change")
#anova(betadisper(dist(pls_scenario_all[-1]),sign(log(value_pls$mu_nfn_signif))))
LDA <- lda(lda_data,sign(log(value_pls$mu_nfn_signif[which(!is.na(value_pls$mu_nfn_signif))])))
MVA.plot(LDA,fac=sign(log(value_pls$mu_nfn_signif[which(!is.na(value_pls$mu_nfn_signif))])))
MVA.plot(LDA,"corr", col=exp_impact_WB)




# load pressure estimate for 2050

lulc_pls_short <- readRDS("output/lulc_pls_short.rds")
pa_pls_short <- readRDS("output/pa_pls_short.rds")
climate_pls <- readRDS("output/climate_pls.rds")

# load bird data

bird_data_mainland <- readRDS("output/bird_data_mainland.rds")
grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")
press_mainland_trend_scale <- readRDS("output/press_mainland_trend_scale.rds")
press_mainland_trend <- readRDS("output/press_mainland_trend.rds")
site_mainland_sf_reproj <- readRDS("output/site_mainland_sf_reproj.rds")
subsite_data_mainland_trend <- readRDS("output/subsite_data_mainland_trend.rds")


# ex with ALAARV

bird_data <- droplevels(subsite_data_mainland_trend[which(subsite_data_mainland_trend$sci_name_out == "Alauda arvensis"),])
pressure_data <- press_mainland_trend_scale
pressure_data_unscale <- press_mainland_trend
site_data <- site_mainland_sf_reproj
min_site_number_per_species <- 60
min_occurence_species <- 200
family <- "quasipoisson"
pressure_name <- c("d_impervious","d_treedensity","d_agri",
                  "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                  "d_shannon","shannon","drymatter","protectedarea_perc",
                  "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                  "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat")

species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)

poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                 pressure_name,"PLS")])

species_press_data_year_unscale <- merge(bird_data, pressure_data_unscale[which(pressure_data_unscale$siteID %in% unique(bird_data$siteID) & pressure_data_unscale$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)

poisson_df_unscale <- na.omit(species_press_data_year_unscale[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                 pressure_name,"tempspring_2020","tempspringvar_2020","precspring_2020","agri_2018","shannon_2018","impervious_2018","treedensity_2018","PLS")])

poisson_df$year <- scale(poisson_df$year)#poisson_df$year - 2000

if(length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
  one_scheme_time_area <- 0 
  poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
}else{
  one_scheme_time_area <- 1
}

poisson_df$count_scale_all <- poisson_df$count#scales::rescale(poisson_df$count)

formula_gam <- "count_scale_all ~ year + year:d_impervious + year:d_treedensity +
    year:eulandsystem_forest_lowmedium + year:eulandsystem_forest_high +
    year:d_agri + year:eulandsystem_farmland_low + year:eulandsystem_farmland_medium + year:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"

col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
               "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
               "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
               "year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium","year:eulandsystem_farmland_high",
               "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")


global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                  family=family, data=poisson_df)


# unscale pressure estimates https://stackoverflow.com/questions/23642111/how-to-unscale-the-coefficients-from-an-lmer-model-fitted-with-a-scaled-respon

global_mod_coef <- summary(global_mod)$p.table[grep("year",row.names(summary(global_mod)$p.table)),]

year_si <- sd(na.omit(press_mainland_trend$year))
d_impervious_si <- sd(na.omit(press_mainland_trend$d_impervious))
d_tempspring_si <- sd(na.omit(press_mainland_trend$d_tempsrping))
d_tempspringvar_si <- sd(na.omit(press_mainland_trend$d_tempsrpingvar))
d_precspring_si <- sd(na.omit(press_mainland_trend$d_precspring))
d_shannon_si <- sd(na.omit(press_mainland_trend$d_shannon))
protectedarea_perc_si <- sd(na.omit(press_mainland_trend$protectedarea_perc))
d_treedensity_si <- sd(na.omit(press_mainland_trend$d_treedensity))
d_agri_si <- sd(na.omit(press_mainland_trend$d_agri))
eulandsystem_farmland_low_si <- sd(na.omit(press_mainland_trend$eulandsystem_farmland_low))
eulandsystem_farmland_medium_si <- sd(na.omit(press_mainland_trend$eulandsystem_farmland_medium))
eulandsystem_farmland_high_si <- sd(na.omit(press_mainland_trend$eulandsystem_farmland_high))
eulandsystem_forest_lowmedium_si <- sd(na.omit(press_mainland_trend$eulandsystem_forest_lowmedium))
eulandsystem_forest_high_si <- sd(na.omit(press_mainland_trend$eulandsystem_forest_high))

year_mu <- mean(na.omit(press_mainland_trend$year))
d_impervious_mu <- mean(na.omit(press_mainland_trend$d_impervious))
d_tempspring_mu <- mean(na.omit(press_mainland_trend$d_tempsrping))
d_tempspringvar_mu <- mean(na.omit(press_mainland_trend$d_tempsrpingvar))
d_precspring_mu <- mean(na.omit(press_mainland_trend$d_precspring))
d_shannon_mu <- mean(na.omit(press_mainland_trend$d_shannon))
protectedarea_perc_mu <- mean(na.omit(press_mainland_trend$protectedarea_perc))
d_treedensity_mu <- mean(na.omit(press_mainland_trend$d_treedensity))
d_agri_mu <- mean(na.omit(press_mainland_trend$d_agri))
eulandsystem_farmland_low_mu <- mean(na.omit(press_mainland_trend$eulandsystem_farmland_low))
eulandsystem_farmland_medium_mu <- mean(na.omit(press_mainland_trend$eulandsystem_farmland_medium))
eulandsystem_farmland_high_mu <- mean(na.omit(press_mainland_trend$eulandsystem_farmland_high))
eulandsystem_forest_lowmedium_mu <- mean(na.omit(press_mainland_trend$eulandsystem_forest_lowmedium))
eulandsystem_forest_high_mu <- mean(na.omit(press_mainland_trend$eulandsystem_forest_high))

nb_rep <- 1000

beta1_BAU <- global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"] +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"]*mean(poisson_df$d_impervious) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"]*mean(poisson_df$d_tempsrping) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*mean(poisson_df$d_tempsrpingvar) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"]*mean(poisson_df$d_precspring) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"]*mean(poisson_df$d_shannon) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"]*mean(poisson_df$protectedarea_perc) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"]*mean(poisson_df$d_treedensity) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*mean(poisson_df$eulandsystem_forest_lowmedium) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*mean(poisson_df$eulandsystem_forest_high) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"]*mean(poisson_df$d_agri) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_low) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_medium) +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_high)

beta1_BAU_sample <- rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Std. Error"])*mean(poisson_df$d_impervious) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Std. Error"])*mean(poisson_df$d_tempsrping) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*mean(poisson_df$d_tempsrpingvar) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Std. Error"])*mean(poisson_df$d_precspring) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Std. Error"])*mean(poisson_df$d_shannon) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Std. Error"])*mean(poisson_df$protectedarea_perc) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Std. Error"])*mean(poisson_df$d_treedensity) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_lowmedium) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_high) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Std. Error"])*mean(poisson_df$d_agri) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_low) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_medium) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_high)


d_impervious_ssp1 <- mean(poisson_df_unscale$impervious_2018)*
  (lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
d_shannon_ssp1 <- mean(poisson_df_unscale$shannon_2018)*
  (lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
d_tempspring_ssp1 <- mean(poisson_df_unscale$tempspring_2020)*
  (climate_pls$mean_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$mean_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_tempspringvar_ssp1 <- mean(poisson_df_unscale$tempspringvar_2020)*
  (climate_pls$var_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$var_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_precspring_ssp1 <- mean(poisson_df_unscale$precspring_2020)*
  (climate_pls$sum_p_4_5[which(climate_pls$PLS=="europe")]/climate_pls$sum_p_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_agri_ssp1 <- mean(poisson_df_unscale$agri_2018)*
  (sum(lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
agri_low_ssp1 <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]
agri_medium_ssp1 <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]
agri_high_ssp1 <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]
d_treedensity_ssp1 <- mean(poisson_df_unscale$treedensity_2018)*
  (sum(lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
forest_lowmedium_ssp1 <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]
forest_high_ssp1 <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]
protectedarea_perc_ssp1 <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$ssp1[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")]

beta1_SSP1 <- global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"] +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_ssp1 - d_impervious_mu)/d_impervious_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_ssp1 - d_shannon_mu)/d_shannon_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_ssp1 - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_ssp1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_ssp1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_ssp1 - d_agri_mu)/d_agri_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_ssp1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_ssp1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_ssp1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_SSP1_sample <- rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_ssp1 - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_ssp1 - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_ssp1 - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_ssp1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_ssp1 +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_ssp1 - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_ssp1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_ssp1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_ssp1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

d_impervious_ssp3 <- mean(poisson_df_unscale$impervious_2018)*
  (lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
d_shannon_ssp3 <- mean(poisson_df_unscale$shannon_2018)*
  (lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
d_tempspring_ssp3 <- mean(poisson_df_unscale$tempspring_2020)*
  (climate_pls$mean_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$mean_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_tempspringvar_ssp3 <- mean(poisson_df_unscale$tempspringvar_2020)*
  (climate_pls$var_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$var_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_precspring_ssp3 <- mean(poisson_df_unscale$precspring_2020)*
  (climate_pls$sum_p_4_5[which(climate_pls$PLS=="europe")]/climate_pls$sum_p_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_agri_ssp3 <- mean(poisson_df_unscale$agri_2018)*
  (sum(lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
agri_low_ssp3 <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]
agri_medium_ssp3 <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]
agri_high_ssp3 <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]
d_treedensity_ssp3 <- mean(poisson_df_unscale$treedensity_2018)*
  (sum(lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
forest_lowmedium_ssp3 <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]
forest_high_ssp3 <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]
protectedarea_perc_ssp3 <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$ssp3[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")]

beta1_SSP3 <- global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"] +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_ssp3 - d_impervious_mu)/d_impervious_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp3 - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp3 - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp3 - d_precspring_mu)/d_precspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_ssp3 - d_shannon_mu)/d_shannon_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp3 - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_ssp3 - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_ssp3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_ssp3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_ssp3 - d_agri_mu)/d_agri_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_ssp3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_ssp3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_ssp3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_SSP3_sample <- rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_ssp3 - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp3 - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp3 - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp3 - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_ssp3 - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp3 - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_ssp3 - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_ssp3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_ssp3 +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_ssp3 - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_ssp3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_ssp3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_ssp3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

d_impervious_nac <- mean(poisson_df_unscale$impervious_2018)*
  (lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
d_shannon_nac <- mean(poisson_df_unscale$shannon_2018)*
  (lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
d_tempspring_nac <- mean(poisson_df_unscale$tempspring_2020)*
  (climate_pls$mean_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$mean_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_tempspringvar_nac <- mean(poisson_df_unscale$tempspringvar_2020)*
  (climate_pls$var_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$var_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_precspring_nac <- mean(poisson_df_unscale$precspring_2020)*
  (climate_pls$sum_p_4_5[which(climate_pls$PLS=="europe")]/climate_pls$sum_p_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_agri_nac <- mean(poisson_df_unscale$agri_2018)*
  (sum(lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
agri_low_nac <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]
agri_medium_nac <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]
agri_high_nac <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]
d_treedensity_nac <- mean(poisson_df_unscale$treedensity_2018)*
  (sum(lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
forest_lowmedium_nac <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]
forest_high_nac <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$nac[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]
protectedarea_perc_nac <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$nac[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")]

beta1_nac <- global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"] +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_nac - d_impervious_mu)/d_impervious_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nac - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nac - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_nac - d_precspring_mu)/d_precspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_nac - d_shannon_mu)/d_shannon_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nac - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nac - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nac - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nac - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_nac - d_agri_mu)/d_agri_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nac - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nac - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nac - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nac_sample <- rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_nac - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nac - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nac - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_nac - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_nac - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nac - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nac - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nac - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nac +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_nac - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nac - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nac - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nac - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

d_impervious_nfn <- mean(poisson_df_unscale$impervious_2018)*
  (lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
d_shannon_nfn <- mean(poisson_df_unscale$shannon_2018)*
  (lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
d_tempspring_nfn <- mean(poisson_df_unscale$tempspring_2020)*
  (climate_pls$mean_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$mean_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_tempspringvar_nfn <- mean(poisson_df_unscale$tempspringvar_2020)*
  (climate_pls$var_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$var_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_precspring_nfn <- mean(poisson_df_unscale$precspring_2020)*
  (climate_pls$sum_p_4_5[which(climate_pls$PLS=="europe")]/climate_pls$sum_p_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_agri_nfn <- mean(poisson_df_unscale$agri_2018)*
  (sum(lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
agri_low_nfn <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]
agri_medium_nfn <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]
agri_high_nfn <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]
d_treedensity_nfn <- mean(poisson_df_unscale$treedensity_2018)*
  (sum(lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
forest_lowmedium_nfn <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]
forest_high_nfn <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$nfn[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]
protectedarea_perc_nfn <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$nfn[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")]

beta1_nfn <- global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"] +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_nfn - d_impervious_mu)/d_impervious_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nfn - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nfn - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_nfn - d_precspring_mu)/d_precspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_nfn - d_shannon_mu)/d_shannon_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nfn - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nfn - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nfn - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nfn - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_nfn - d_agri_mu)/d_agri_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nfn - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nfn - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nfn - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nfn_sample <- rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_nfn - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nfn - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nfn - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_nfn - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_nfn - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nfn - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nfn - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nfn - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nfn +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_nfn - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nfn - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nfn - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nfn - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

d_impervious_nfs <- mean(poisson_df_unscale$impervious_2018)*
  (lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
d_shannon_nfs <- mean(poisson_df_unscale$shannon_2018)*
  (lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
d_tempspring_nfs <- mean(poisson_df_unscale$tempspring_2020)*
  (climate_pls$mean_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$mean_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_tempspringvar_nfs <- mean(poisson_df_unscale$tempspringvar_2020)*
  (climate_pls$var_t_4_5[which(climate_pls$PLS=="europe")]/climate_pls$var_t_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_precspring_nfs <- mean(poisson_df_unscale$precspring_2020)*
  (climate_pls$sum_p_4_5[which(climate_pls$PLS=="europe")]/climate_pls$sum_p_2016[which(climate_pls$PLS=="europe")]-1)/(2050-2018)
d_agri_nfs <- mean(poisson_df_unscale$agri_2018)*
  (sum(lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
agri_low_nfs <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_low"))]
agri_medium_nfs <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_medium"))]
agri_high_nfs <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("farmland_high"))]
d_treedensity_nfs <- mean(poisson_df_unscale$treedensity_2018)*
  (sum(lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
forest_lowmedium_nfs <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_lowmedium"))]
forest_high_nfs <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$nfs[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS=="europe" & lulc_pls_short$variable %in% c("forest_high"))]
protectedarea_perc_nfs <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$nfs[which(pa_pls_short$PLS=="europe")]/pa_pls_short$initial[which(pa_pls_short$PLS=="europe")]

beta1_nfs <- global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"] +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_nfs - d_impervious_mu)/d_impervious_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nfs - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nfs - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_nfs - d_precspring_mu)/d_precspring_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_nfs - d_shannon_mu)/d_shannon_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nfs - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nfs - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nfs - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nfs - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_nfs - d_agri_mu)/d_agri_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nfs - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nfs - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nfs - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nfs_sample <- rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_nfs - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nfs - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nfs - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_nfs - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_nfs - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nfs - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nfs - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nfs - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nfs +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_nfs - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nfs - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nfs - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef[which(row.names(global_mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nfs - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si


global_mod_coef_signif <- global_mod_coef
global_mod_coef_signif[which(global_mod_coef_signif[,c("Pr(>|t|)")] > 0.05),c("Estimate","Std. Error")] <- 0

beta1_BAU_signif <- global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"] +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"]*mean(poisson_df$d_impervious) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*mean(poisson_df$d_tempsrping) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*mean(poisson_df$d_tempsrpingvar) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"]*mean(poisson_df$d_precspring) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"]*mean(poisson_df$d_shannon) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*mean(poisson_df$protectedarea_perc) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"]*mean(poisson_df$d_treedensity) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*mean(poisson_df$eulandsystem_forest_lowmedium) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*mean(poisson_df$eulandsystem_forest_high) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"]*mean(poisson_df$d_agri) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_low) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_medium) +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_high)

beta1_BAU_sample_signif <- rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Std. Error"])*mean(poisson_df$d_impervious) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*mean(poisson_df$d_tempsrping) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*mean(poisson_df$d_tempsrpingvar) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Std. Error"])*mean(poisson_df$d_precspring) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Std. Error"])*mean(poisson_df$d_shannon) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*mean(poisson_df$protectedarea_perc) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*mean(poisson_df$d_treedensity) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_lowmedium) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_high) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Std. Error"])*mean(poisson_df$d_agri) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_low) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_medium) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_high)

beta1_SSP1_signif <- global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"] +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_ssp1 - d_impervious_mu)/d_impervious_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_ssp1 - d_shannon_mu)/d_shannon_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_ssp1 - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_ssp1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_ssp1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_ssp1 - d_agri_mu)/d_agri_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_ssp1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_ssp1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_ssp1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_SSP1_sample_signif <- rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_ssp1 - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_ssp1 - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_ssp1 - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_ssp1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_ssp1 +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_ssp1 - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_ssp1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_ssp1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_ssp1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_SSP3_signif <- global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"] +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_ssp3 - d_impervious_mu)/d_impervious_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp3 - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp3 - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp3 - d_precspring_mu)/d_precspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_ssp3 - d_shannon_mu)/d_shannon_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp3 - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_ssp3 - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_ssp3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_ssp3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_ssp3 - d_agri_mu)/d_agri_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_ssp3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_ssp3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_ssp3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_SSP3_sample_signif <- rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_ssp3 - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp3 - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp3 - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp3 - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_ssp3 - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp3 - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_ssp3 - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_ssp3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_ssp3 +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_ssp3 - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_ssp3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_ssp3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_ssp3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nac_signif <- global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"] +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_nac - d_impervious_mu)/d_impervious_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nac - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nac - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_nac - d_precspring_mu)/d_precspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_nac - d_shannon_mu)/d_shannon_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nac - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nac - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nac - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nac - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_nac - d_agri_mu)/d_agri_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nac - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nac - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nac - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nac_sample_signif <- rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_nac - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nac - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nac - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_nac - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_nac - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nac - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nac - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nac - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nac +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_nac - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nac - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nac - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nac - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nfn_signif <- global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"] +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_nfn - d_impervious_mu)/d_impervious_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nfn - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nfn - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_nfn - d_precspring_mu)/d_precspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_nfn - d_shannon_mu)/d_shannon_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nfn - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nfn - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nfn - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nfn - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_nfn - d_agri_mu)/d_agri_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nfn - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nfn - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nfn - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nfn_sample_signif <- rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_nfn - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nfn - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nfn - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_nfn - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_nfn - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nfn - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nfn - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nfn - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nfn +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_nfn - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nfn - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nfn - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nfn - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nfs_signif <- global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"] +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_nfs - d_impervious_mu)/d_impervious_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nfs - d_tempspring_mu)/d_tempspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nfs - d_tempspringvar_mu)/d_tempspringvar_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_nfs - d_precspring_mu)/d_precspring_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_nfs - d_shannon_mu)/d_shannon_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nfs - protectedarea_perc_mu)/protectedarea_perc_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nfs - d_treedensity_mu)/d_treedensity_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nfs - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nfs - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_nfs - d_agri_mu)/d_agri_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nfs - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nfs - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nfs - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si

beta1_nfs_sample_signif <- rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year"),"Std. Error"]) +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_nfs - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nfs - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nfs - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_nfs - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_nfs - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nfs - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nfs - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nfs - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nfs +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_nfs - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nfs - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nfs - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=global_mod_coef_signif[which(row.names(global_mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nfs - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si



### apply to all species and PLS 

source("functions.R")

# load pressure estimate for 2050

lulc_pls_short <- readRDS("output/lulc_pls_short.rds")
pa_pls_short <- readRDS("output/pa_pls_short.rds")
climate_pls <- readRDS("output/climate_pls.rds")


# load bird data

bird_data_mainland <- readRDS("output/bird_data_mainland.rds")
grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")
press_mainland_trend_scale <- readRDS("output/press_mainland_trend_scale.rds")
press_mainland_trend <- readRDS("output/press_mainland_trend.rds")
site_mainland_sf_reproj <- readRDS("output/site_mainland_sf_reproj.rds")
subsite_data_mainland_trend <- readRDS("output/subsite_data_mainland_trend.rds")

# load butterfly data

butterfly_data_mainland <- readRDS("output/butterfly_data_mainland.rds")
grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")
press_mainland_trend_butterfly_scale <- readRDS("output/press_mainland_trend_butterfly_scale.rds")
press_mainland_trend_butterfly <- readRDS("output/press_mainland_trend_butterfly.rds")
site_mainland_sf_reproj_butterfly <- readRDS("output/site_mainland_sf_reproj_butterfly.rds")
subsite_data_mainland_trend_butterfly <- readRDS("output/subsite_data_mainland_trend_butterfly.rds")



predict_trend_all_bird <- ddply(subsite_data_mainland_trend,
                      .(sci_name_out),.fun=predict_trend_bird,
                      pressure_data=press_mainland_trend_scale,site_data=site_mainland_sf_reproj,
                      pressure_data_unscale=press_mainland_trend,
                      lulc_pls_short=lulc_pls_short,climate_pls=climate_pls,pa_pls_short=pa_pls_short,
                      .progress = "text")
predict_trend_all_bird <- predict_trend_all_bird[which(!is.na(predict_trend_all_bird$PLS)),]

#saveRDS(predict_trend_all_bird,"output/predict_trend_all_bird.rds")
#predict_trend_all_bird <- readRDS("output/predict_trend_all_birdnew_past.rds")


predict_trend_all_butterfly <- ddply(subsite_data_mainland_trend_butterfly,
                                .(species_name),.fun=predict_trend_butterfly,
                                pressure_data=press_mainland_trend_butterfly_scale,site_data=site_mainland_sf_reproj_butterfly,
                                pressure_data_unscale=press_mainland_trend_butterfly,
                                lulc_pls_short=lulc_pls_short,climate_pls=climate_pls,pa_pls_short=pa_pls_short,
                                .progress = "text")
predict_trend_all_butterfly <- predict_trend_all_butterfly[which(!is.na(predict_trend_all_butterfly$PLS)),]

#saveRDS(predict_trend_all_butterfly,"output/predict_trend_all_butterfly.rds")
#predict_trend_all_butterfly <- readRDS("output/predict_trend_all_butterflynew_past.rds")



predict_trend_all_bird_correct <- merge(res_gamm_bird_correct,predict_trend_all_bird, by=c("sci_name_out","PLS"),all.x=TRUE)

#weight_species <- bird_data_mainland %>% group_by(sci_name_out) %>% summarise(ab_tot = sum(count), nb_monit_site = n(), nb_monit_sp = length(which(count>0)))
#weight_species$ab_moy <- weight_species$ab_tot/weight_species$nb_monit_site
#weight_species$oc_moy <- weight_species$nb_monit_sp/weight_species$nb_monit_site
#weight_species <- bird_data_mainland[which(bird_data_mainland$year < 2005),] %>% group_by(sci_name_out) %>% summarise(ab_tot = sum(count)/5)

#predict_trend_all_bird_correct <- merge(predict_trend_all_bird_correct,weight_species, by=c("sci_name_out"),all.x=TRUE)


predict_trend_all_bird_correct <- ddply(predict_trend_all_bird_correct,
                                             .(PLS),.fun=function(x){
                                               for(i in c("trend_past","trend_BAU","trend_SSP1","trend_SSP3","trend_nac","trend_nfn","trend_nfs")){
                                                 #x[which(abs(x[,i]) < abs(x[,(which(names(x)==i)+1)])),i] <- 0
                                                 value_max <- max(abs(quantile(predict_trend_all_bird_eu$trend_past_signif,0.25)),abs(quantile(predict_trend_all_bird_eu$trend_past_signif,0.75)))
                                                 x[which(x[,i]>value_max),i] <- value_max
                                                 x[which(x[,i]<(-value_max)),i] <- -value_max
                                               }
                                               for(i in c("trend_past_signif","trend_BAU_signif","trend_SSP1_signif","trend_SSP3_signif","trend_nac_signif","trend_nfn_signif","trend_nfs_signif")){
                                                 #x[which(x[,i]>max(x$trend_past_signif)),i] <- max(x$trend_past_signif)
                                                 #x[which(x[,i]<min(x$trend_past_signif)),i] <- min(x$trend_past_signif)
                                                 #x[which(abs(x[,i]) < abs(x[,(which(names(x)==i)+1)])),i] <- 0
                                                 x[which(x[,i]>value_max),i] <- value_max
                                                 x[which(x[,i]<(-value_max)),i] <- -value_max
                                               }
                                               return(x)
                                             },
                                             .progress = "text")

predict_trend_all_bird_eu <- predict_trend_all_bird_correct[which(predict_trend_all_bird_correct$PLS=="europe" & predict_trend_all_bird_correct$pressure_removed=="none"),]


species_habitat <- read.csv("raw_data/Habitat_class_PECBMS.csv")
predict_trend_farmland <- predict_trend_all_bird_correct[which(predict_trend_all_bird_correct$sci_name_out %in% unique(species_habitat$Species_new[which(species_habitat$Habitat=="Farmland")])),]
predict_trend_forest <- predict_trend_all_bird_correct[which(predict_trend_all_bird_correct$sci_name_out %in% unique(species_habitat$Species_new[which(species_habitat$Habitat=="Forest")])),]

predict_trend_all_bird_correct_pecbms <- predict_trend_all_bird_correct[which(predict_trend_all_bird_correct$sci_name_out %in% species_habitat$Species_new),]


overall_trend_all <- ddply(predict_trend_all_bird_correct,
                                .(PLS,pressure_removed),.fun=overall_mean_sd_trend,
                                .progress = "text")
overall_trend_all_sf <- merge(grid_eu_mainland_biogeo,overall_trend_all,by="PLS",all.x=TRUE)

ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_all_sf, aes(fill=mu_bau_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(midpoint = 1, name = NULL) + theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_all_sf, aes(fill=mu_ssp1_signif-mu_bau_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.09, 0.035),midpoint = 0, name = NULL) + theme_void()

ggsave("output/map_pred_all_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

overall_trend_all_eu <- overall_mean_sd_trend(predict_trend_all_bird_correct[which(predict_trend_all_bird_correct$PLS=="europe" & predict_trend_all_bird_correct$pressure_removed =="none"),])



overall_trend_farmland <- ddply(predict_trend_farmland,
                                .(PLS,pressure_removed),.fun=overall_mean_sd_trend,
                                .progress = "text")

overall_trend_farmland_sf <- merge(grid_eu_mainland_biogeo,overall_trend_farmland,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_farmland_sf, aes(fill=mu_past_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(midpoint = 1, name = NULL) + theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_farmland_sf, aes(fill=mu_ssp1_signif-mu_bau_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.1, 0.05),midpoint = 0, name = NULL) + theme_void()

ggsave("output/map_pred_farm_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

overall_trend_forest <- ddply(predict_trend_forest,
                                .(PLS,pressure_removed),.fun=overall_mean_sd_trend,
                                .progress = "text")

overall_trend_forest_sf <- merge(grid_eu_mainland_biogeo,overall_trend_forest,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_forest_sf, aes(fill=mu_past_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(midpoint = 1, name = NULL) + theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_forest_sf, aes(fill=mu_ssp1_signif-mu_bau_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.03, 0.03),midpoint = 0, name = NULL) + theme_void()

ggsave("output/map_pred_forest_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)


pressure_removed <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                      "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                      "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                      "eulandsystem_farmland_high")[1]

europe_all <- data.frame(value = c(overall_trend_all$mu_past[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_bau[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_ssp1[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_ssp3[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_nac[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_nfn[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_nfs[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)]),
                         sd = c(overall_trend_all$sd_past[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_bau[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_ssp1[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_ssp3[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_nac[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_nfn[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_nfs[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)]),
                         se = c(overall_trend_all$se_past[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_bau[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_ssp1[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_ssp3[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_nac[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_nfn[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_nfs[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)]),
                         variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_all2 <- europe_all[which(europe_all$variable %in% c("past","bau","ssp1","nfn","nfs","nac")),]
europe_all2$variable <- factor(europe_all2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_all2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_bird_eu_all_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="past")]^x/europe_all$value[which(europe_all$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="bau")]^x/europe_all$value[which(europe_all$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="ssp1")]^x/europe_all$value[which(europe_all$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="nfn")]^x/europe_all$value[which(europe_all$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="nfs")]^x/europe_all$value[which(europe_all$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="nac")]^x/europe_all$value[which(europe_all$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_bird_eu_all.png",
       width = 5,
       height = 3,
       dpi = 300
)

comb_var <- combn(europe_all$variable,2)
test_diff_var_europe_all <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_all[i,3] <- tsum.test(mean.x=europe_all$value[which(europe_all$variable==comb_var[1,i])],   s.x=europe_all$se[which(europe_all$variable==comb_var[1,i])], n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe")],
                                             mean.y=europe_all$value[which(europe_all$variable==comb_var[2,i])],   s.y=europe_all$se[which(europe_all$variable==comb_var[2,i])], n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe")])$p.value
  
}


boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_all$pressure_removed[which(overall_trend_all$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_all$mu_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1[which(overall_trend_all$PLS=="europe")],
                                       overall_trend_all$mu_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs[which(overall_trend_all$PLS=="europe")]), 
                 boxCILow = c(overall_trend_all$mu_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1[which(overall_trend_all$PLS=="europe")],
                              overall_trend_all$mu_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs[which(overall_trend_all$PLS=="europe")])-1.96*c(overall_trend_all$se_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_ssp1[which(overall_trend_all$PLS=="europe")],
                                                     overall_trend_all$se_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfs[which(overall_trend_all$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_all$mu_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1[which(overall_trend_all$PLS=="europe")],
                               overall_trend_all$mu_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs[which(overall_trend_all$PLS=="europe")])+1.96*c(overall_trend_all$se_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_ssp1[which(overall_trend_all$PLS=="europe")],
                                                      overall_trend_all$se_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfs[which(overall_trend_all$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_bird_eu_all_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)









europe_all_signif <- data.frame(value = c(overall_trend_all$mu_past_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_bau_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_ssp1_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_ssp3_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_nac_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_nfn_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_nfs_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)]),
                                sd = c(overall_trend_all$sd_past_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_bau_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_ssp1_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_ssp3_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_nac_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_nfn_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_nfs_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)]),
                                se = c(overall_trend_all$se_past_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_bau_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_ssp1_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_ssp3_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_nac_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_nfn_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_nfs_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)]),
                                variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_all_signif2 <- europe_all_signif[which(europe_all_signif$variable %in% c("past","bau","ssp1","nfn","nfs","nac")),]
europe_all_signif2$variable <- factor(europe_all_signif2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_all_signif2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_bird_eu_all_signif_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="past")]^x/europe_all_signif$value[which(europe_all_signif$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="bau")]^x/europe_all_signif$value[which(europe_all_signif$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="ssp1")]^x/europe_all_signif$value[which(europe_all_signif$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="nfn")]^x/europe_all_signif$value[which(europe_all_signif$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="nfs")]^x/europe_all_signif$value[which(europe_all_signif$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="nac")]^x/europe_all_signif$value[which(europe_all_signif$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_bird_eu_all_signif.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_all_signif2$variable <- as.character(europe_all_signif2$variable)
comb_var <- combn(europe_all_signif2$variable,2)
test_diff_var_europe_all_signif <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_all_signif[i,3] <- tsum.test(mean.x=europe_all_signif2$value[which(europe_all_signif2$variable==comb_var[1,i])],   s.x=europe_all_signif2$se[which(europe_all_signif2$variable==comb_var[1,i])], n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe")],
                                                    mean.y=europe_all_signif2$value[which(europe_all_signif2$variable==comb_var[2,i])],   s.y=europe_all_signif2$se[which(europe_all_signif2$variable==comb_var[2,i])], n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe")])$p.value
  
}


boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_all$pressure_removed[which(overall_trend_all$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_all$mu_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                                       overall_trend_all$mu_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs_signif[which(overall_trend_all$PLS=="europe")]), 
                 boxCILow = c(overall_trend_all$mu_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                              overall_trend_all$mu_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs_signif[which(overall_trend_all$PLS=="europe")])-1.96*c(overall_trend_all$se_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                                                                                                                                                                                                                                         overall_trend_all$se_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfs_signif[which(overall_trend_all$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_all$mu_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                               overall_trend_all$mu_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs_signif[which(overall_trend_all$PLS=="europe")])+1.96*c(overall_trend_all$se_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                                                                                                                                                                                                                                          overall_trend_all$se_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfs_signif[which(overall_trend_all$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_bird_eu_all_signif_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)



europe_pecbms <- data.frame(value = c(overall_trend_all_eu$mu_past[which(overall_trend_all_eu$PLS=="europe")],
                                   overall_trend_all_eu$mu_bau[which(overall_trend_all_eu$PLS=="europe")],
                                   overall_trend_all_eu$mu_ssp1[which(overall_trend_all_eu$PLS=="europe")],
                                   overall_trend_all_eu$mu_ssp3[which(overall_trend_all_eu$PLS=="europe")],
                                   overall_trend_all_eu$mu_nac[which(overall_trend_all_eu$PLS=="europe")],
                                   overall_trend_all_eu$mu_nfn[which(overall_trend_all_eu$PLS=="europe")],
                                   overall_trend_all_eu$mu_nfs[which(overall_trend_all_eu$PLS=="europe")]),
                         sd = c(overall_trend_all_eu$sd_past[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$sd_bau[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$sd_ssp1[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$sd_ssp3[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$sd_nac[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$sd_nfn[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$sd_nfs[which(overall_trend_all_eu$PLS=="europe")]),
                         se = c(overall_trend_all_eu$se_past[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$se_bau[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$se_ssp1[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$se_ssp3[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$se_nac[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$se_nfn[which(overall_trend_all_eu$PLS=="europe")],
                                overall_trend_all_eu$se_nfs[which(overall_trend_all_eu$PLS=="europe")]),
                         variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_pecbms2 <- europe_pecbms[which(europe_pecbms$variable != "ssp3"),]
europe_pecbms2$variable <- factor(europe_pecbms2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_pecbms2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_bird_eu_pecbms_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_pecbms$value[which(europe_pecbms$variable=="past")]^x/europe_pecbms$value[which(europe_pecbms$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_pecbms$value[which(europe_pecbms$variable=="bau")]^x/europe_pecbms$value[which(europe_pecbms$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_pecbms$value[which(europe_pecbms$variable=="ssp1")]^x/europe_pecbms$value[which(europe_pecbms$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_pecbms$value[which(europe_pecbms$variable=="nfn")]^x/europe_pecbms$value[which(europe_pecbms$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_pecbms$value[which(europe_pecbms$variable=="nfs")]^x/europe_pecbms$value[which(europe_pecbms$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_pecbms$value[which(europe_pecbms$variable=="nac")]^x/europe_pecbms$value[which(europe_pecbms$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_bird_eu_pecbms.png",
       width = 5,
       height = 3,
       dpi = 300
)

comb_var <- combn(europe_pecbms$variable,2)
test_diff_var_europe_pecbms <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_pecbms[i,3] <- tsum.test(mean.x=europe_pecbms$value[which(europe_pecbms$variable==comb_var[1,i])],   s.x=europe_pecbms$se[which(europe_pecbms$variable==comb_var[1,i])], n.x= overall_trend_all_eu$n[which(overall_trend_all_eu$PLS=="europe")],
                                             mean.y=europe_pecbms$value[which(europe_pecbms$variable==comb_var[2,i])],   s.y=europe_pecbms$se[which(europe_pecbms$variable==comb_var[2,i])], n.y= overall_trend_all_eu$n[which(overall_trend_all_eu$PLS=="europe")])$p.value
  
}


europe_pecbms_signif <- data.frame(value = c(overall_trend_all_eu$mu_past_signif[which(overall_trend_all_eu$PLS=="europe")],
                                          overall_trend_all_eu$mu_bau_signif[which(overall_trend_all_eu$PLS=="europe")],
                                          overall_trend_all_eu$mu_ssp1_signif[which(overall_trend_all_eu$PLS=="europe")],
                                          overall_trend_all_eu$mu_ssp3_signif[which(overall_trend_all_eu$PLS=="europe")],
                                          overall_trend_all_eu$mu_nac_signif[which(overall_trend_all_eu$PLS=="europe")],
                                          overall_trend_all_eu$mu_nfn_signif[which(overall_trend_all_eu$PLS=="europe")],
                                          overall_trend_all_eu$mu_nfs_signif[which(overall_trend_all_eu$PLS=="europe")]),
                                sd = c(overall_trend_all_eu$sd_past_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$sd_bau_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$sd_ssp1_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$sd_ssp3_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$sd_nac_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$sd_nfn_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$sd_nfs_signif[which(overall_trend_all_eu$PLS=="europe")]),
                                se = c(overall_trend_all_eu$se_past_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$se_bau_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$se_ssp1_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$se_ssp3_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$se_nac_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$se_nfn_signif[which(overall_trend_all_eu$PLS=="europe")],
                                       overall_trend_all_eu$se_nfs_signif[which(overall_trend_all_eu$PLS=="europe")]),
                                variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_pecbms_signif2 <- europe_pecbms_signif[which(europe_pecbms_signif$variable != "ssp3"),]
europe_pecbms_signif2$variable <- factor(europe_pecbms_signif2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_pecbms_signif2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_bird_eu_pecbms_signif_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="past")]^x/europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="bau")]^x/europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="ssp1")]^x/europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="nfn")]^x/europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="nfs")]^x/europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="nac")]^x/europe_pecbms_signif$value[which(europe_pecbms_signif$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_bird_eu_pecbms_signif.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_pecbms_signif2$variable <- as.character(europe_pecbms_signif2$variable)
comb_var <- combn(europe_pecbms_signif2$variable,2)
test_diff_var_europe_pecbms_signif <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_pecbms_signif[i,3] <- tsum.test(mean.x=europe_pecbms_signif2$value[which(europe_pecbms_signif2$variable==comb_var[1,i])],   s.x=europe_pecbms_signif2$se[which(europe_pecbms_signif2$variable==comb_var[1,i])], n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe")],
                                                    mean.y=europe_pecbms_signif2$value[which(europe_pecbms_signif2$variable==comb_var[2,i])],   s.y=europe_pecbms_signif2$se[which(europe_pecbms_signif2$variable==comb_var[2,i])], n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe")])$p.value
  
}


europe_farmland <- data.frame(value = c(overall_trend_farmland$mu_past[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_bau[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_ssp1[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_ssp3[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_nac[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_nfn[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_nfs[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                              sd = c(overall_trend_farmland$sd_past[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_bau[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_ssp1[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_ssp3[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_nac[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_nfn[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_nfs[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                              se = c(overall_trend_farmland$se_past[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_bau[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_ssp1[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_ssp3[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_nac[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_nfn[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_nfs[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                              variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_farmland2 <- europe_farmland[which(europe_farmland$variable != "ssp3"),]
europe_farmland2$variable <- factor(europe_farmland2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_farmland2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_bird_farm_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="past")]^x/europe_farmland$value[which(europe_farmland$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="bau")]^x/europe_farmland$value[which(europe_farmland$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="ssp1")]^x/europe_farmland$value[which(europe_farmland$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="nfn")]^x/europe_farmland$value[which(europe_farmland$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="nfs")]^x/europe_farmland$value[which(europe_farmland$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="nac")]^x/europe_farmland$value[which(europe_farmland$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_bird_farm.png",
       width = 5,
       height = 3,
       dpi = 300
)

boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_farmland$pressure_removed[which(overall_trend_farmland$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_farmland$mu_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1[which(overall_trend_farmland$PLS=="europe")],
                                       overall_trend_farmland$mu_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs[which(overall_trend_farmland$PLS=="europe")]), 
                 boxCILow = c(overall_trend_farmland$mu_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1[which(overall_trend_farmland$PLS=="europe")],
                              overall_trend_farmland$mu_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs[which(overall_trend_farmland$PLS=="europe")])-1.96*c(overall_trend_farmland$se_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_ssp1[which(overall_trend_farmland$PLS=="europe")],
                                                                                                                                                                                                                                         overall_trend_farmland$se_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfs[which(overall_trend_farmland$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_farmland$mu_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1[which(overall_trend_farmland$PLS=="europe")],
                               overall_trend_farmland$mu_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs[which(overall_trend_farmland$PLS=="europe")])+1.96*c(overall_trend_farmland$se_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_ssp1[which(overall_trend_farmland$PLS=="europe")],
                                                                                                                                                                                                                                          overall_trend_farmland$se_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfs[which(overall_trend_farmland$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_bird_eu_farmland_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)







europe_farmland_signif <- data.frame(value = c(overall_trend_farmland$mu_past_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_bau_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_ssp1_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_ssp3_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_nac_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_nfn_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_nfs_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                                     sd = c(overall_trend_farmland$sd_past_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_bau_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_ssp1_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_ssp3_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_nac_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_nfn_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_nfs_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                                     se = c(overall_trend_farmland$se_past_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_bau_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_ssp1_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_ssp3_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_nac_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_nfn_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_nfs_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                                     variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_farmland_signif2 <- europe_farmland_signif[which(europe_farmland_signif$variable != "ssp3"),]
europe_farmland_signif2$variable <- factor(europe_farmland_signif2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_farmland_signif2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_bird_farm_signif_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="past")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="bau")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="ssp1")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="nfn")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="nfs")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="nac")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_bird_farm_signif.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_farmland_signif2$variable <- as.character(europe_farmland_signif2$variable)
comb_var <- combn(europe_farmland_signif2$variable,2)
test_diff_var_europe_farmland_signif <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_farmland_signif[i,3] <- tsum.test(mean.x=europe_farmland_signif2$value[which(europe_farmland_signif2$variable==comb_var[1,i])],   s.x=europe_farmland_signif2$se[which(europe_farmland_signif2$variable==comb_var[1,i])], n.x= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe")],
                                                         mean.y=europe_farmland_signif2$value[which(europe_farmland_signif2$variable==comb_var[2,i])],   s.y=europe_farmland_signif2$se[which(europe_farmland_signif2$variable==comb_var[2,i])], n.y= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe")])$p.value
  
}

boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_farmland$pressure_removed[which(overall_trend_farmland$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_farmland$mu_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                                       overall_trend_farmland$mu_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs_signif[which(overall_trend_farmland$PLS=="europe")]), 
                 boxCILow = c(overall_trend_farmland$mu_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                              overall_trend_farmland$mu_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs_signif[which(overall_trend_farmland$PLS=="europe")])-1.96*c(overall_trend_farmland$se_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                                                                                                                                                                                                                                                              overall_trend_farmland$se_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfs_signif[which(overall_trend_farmland$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_farmland$mu_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                               overall_trend_farmland$mu_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs_signif[which(overall_trend_farmland$PLS=="europe")])+1.96*c(overall_trend_farmland$se_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                                                                                                                                                                                                                                                               overall_trend_farmland$se_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfs_signif[which(overall_trend_farmland$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_bird_eu_farmland_signif_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)







europe_forest <- data.frame(value = c(overall_trend_forest$mu_past[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_bau[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_ssp1[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_ssp3[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_nac[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_nfn[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_nfs[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                            sd = c(overall_trend_forest$sd_past[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_bau[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_ssp1[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_ssp3[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_nac[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_nfn[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_nfs[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                            se = c(overall_trend_forest$se_past[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_bau[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_ssp1[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_ssp3[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_nac[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_nfn[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_nfs[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                            variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_forest2 <- europe_forest[which(europe_forest$variable != "ssp3"),]
europe_forest2$variable <- factor(europe_forest2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_forest2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_bird_forest_error.png",
       width = 3,
       height = 5,
       dpi = 300
)


ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="past")]^x/europe_forest$value[which(europe_forest$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="bau")]^x/europe_forest$value[which(europe_forest$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="ssp1")]^x/europe_forest$value[which(europe_forest$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="nfn")]^x/europe_forest$value[which(europe_forest$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="nfs")]^x/europe_forest$value[which(europe_forest$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="nac")]^x/europe_forest$value[which(europe_forest$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_bird_forest.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_forest2$variable <- as.character(europe_forest2$variable)
comb_var <- combn(europe_forest2$variable,2)
test_diff_var_europe_forest <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_forest[i,3] <- tsum.test(mean.x=europe_forest2$value[which(europe_forest2$variable==comb_var[1,i])],   s.x=europe_forest2$se[which(europe_forest2$variable==comb_var[1,i])], n.x= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe")],
                                                mean.y=europe_forest2$value[which(europe_forest2$variable==comb_var[2,i])],   s.y=europe_forest2$se[which(europe_forest2$variable==comb_var[2,i])], n.y= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe")])$p.value
  
}


boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_forest$pressure_removed[which(overall_trend_forest$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_forest$mu_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1[which(overall_trend_forest$PLS=="europe")],
                                       overall_trend_forest$mu_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs[which(overall_trend_forest$PLS=="europe")]), 
                 boxCILow = c(overall_trend_forest$mu_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1[which(overall_trend_forest$PLS=="europe")],
                              overall_trend_forest$mu_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs[which(overall_trend_forest$PLS=="europe")])-1.96*c(overall_trend_forest$se_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_ssp1[which(overall_trend_forest$PLS=="europe")],
                                                                                                                                                                                                                                         overall_trend_forest$se_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfs[which(overall_trend_forest$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_forest$mu_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1[which(overall_trend_forest$PLS=="europe")],
                               overall_trend_forest$mu_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs[which(overall_trend_forest$PLS=="europe")])+1.96*c(overall_trend_forest$se_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_ssp1[which(overall_trend_forest$PLS=="europe")],
                                                                                                                                                                                                                                          overall_trend_forest$se_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfs[which(overall_trend_forest$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_bird_eu_forest_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)






europe_forest_signif <- data.frame(value = c(overall_trend_forest$mu_past_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_bau_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_ssp1_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_ssp3_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_nac_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_nfn_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_nfs_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                                   sd = c(overall_trend_forest$sd_past_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_bau_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_ssp1_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_ssp3_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_nac_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_nfn_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_nfs_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                                   se = c(overall_trend_forest$se_past_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_bau_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_ssp1_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_ssp3_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_nac_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_nfn_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_nfs_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                                   variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_forest_signif2 <- europe_forest_signif[which(europe_forest_signif$variable != "ssp3"),]
europe_forest_signif2$variable <- factor(europe_forest_signif2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_forest_signif2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_bird_forest_signif_error.png",
       width = 3,
       height = 5,
       dpi = 300
)


ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="past")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="bau")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="ssp1")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="nfn")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="nfs")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="nac")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_bird_forest_signif.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_forest_signif2$variable <- as.character(europe_forest_signif2$variable)
comb_var <- combn(europe_forest_signif2$variable,2)
test_diff_var_europe_forest_signif <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_forest_signif[i,3] <- tsum.test(mean.x=europe_forest_signif2$value[which(europe_forest_signif2$variable==comb_var[1,i])],   s.x=europe_forest_signif2$se[which(europe_forest_signif2$variable==comb_var[1,i])], n.x= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe")],
                                                       mean.y=europe_forest_signif2$value[which(europe_forest_signif2$variable==comb_var[2,i])],   s.y=europe_forest_signif2$se[which(europe_forest_signif2$variable==comb_var[2,i])], n.y= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe")])$p.value
  
}


boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_forest$pressure_removed[which(overall_trend_forest$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_forest$mu_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                                       overall_trend_forest$mu_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs_signif[which(overall_trend_forest$PLS=="europe")]), 
                 boxCILow = c(overall_trend_forest$mu_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                              overall_trend_forest$mu_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs_signif[which(overall_trend_forest$PLS=="europe")])-1.96*c(overall_trend_forest$se_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                                                                                                                                                                                                                                                              overall_trend_forest$se_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfs_signif[which(overall_trend_forest$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_forest$mu_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                               overall_trend_forest$mu_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs_signif[which(overall_trend_forest$PLS=="europe")])+1.96*c(overall_trend_forest$se_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                                                                                                                                                                                                                                                               overall_trend_forest$se_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfs_signif[which(overall_trend_forest$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_bird_eu_forest_signif_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)




#butterfly


predict_trend_all_butterfly_correct <- merge(res_gamm_butterfly_correct,predict_trend_all_butterfly, by=c("species_name","PLS"),all.x=TRUE)


predict_trend_all_butterfly_correct <- ddply(predict_trend_all_butterfly_correct,
                                        .(PLS),.fun=function(x){
                                          for(i in c("trend_past","trend_BAU","trend_SSP1","trend_SSP3","trend_nac","trend_nfn","trend_nfs")){
                                            #x[which(abs(x[,i]) < abs(x[,(which(names(x)==i)+1)])),i] <- 0
                                            value_max <- max(abs(quantile(predict_trend_all_butterfly_eu$trend_past_signif,0.25)),abs(quantile(predict_trend_all_butterfly_eu$trend_past_signif,0.75)))
                                            x[which(x[,i]>value_max),i] <- value_max #quantile(predict_trend_all_butterfly_eu$trend_past_signif)
                                            x[which(x[,i]<(-value_max)),i] <- -value_max
                                          }
                                          for(i in c("trend_past_signif","trend_BAU_signif","trend_SSP1_signif","trend_SSP3_signif","trend_nac_signif","trend_nfn_signif","trend_nfs_signif")){
                                            #x[which(x[,i]>max(x$trend_past_signif)),i] <- max(x$trend_past_signif)
                                            #x[which(x[,i]<min(x$trend_past_signif)),i] <- min(x$trend_past_signif)
                                            #x[which(abs(x[,i]) < abs(x[,(which(names(x)==i)+1)])),i] <- 0
                                            x[which(x[,i]>value_max),i] <- value_max
                                            x[which(x[,i]<(-value_max)),i] <- -value_max
                                          }
                                          return(x)
                                        },
                                        .progress = "text")


predict_trend_all_butterfly_eu <- predict_trend_all_butterfly_correct[which(predict_trend_all_butterfly_correct$PLS=="europe"),]
predict_trend_farmland <- predict_trend_all_butterfly_correct[which(predict_trend_all_butterfly_correct$species_name %in% grassland_species),]
predict_trend_forest <- predict_trend_all_butterfly_correct[which(predict_trend_all_butterfly_correct$species_name %in% woodland_ind_species),]



overall_trend_all <- ddply(predict_trend_all_butterfly_correct,
                           .(PLS,pressure_removed),.fun=overall_mean_sd_trend,
                           .progress = "text")
overall_trend_all_sf <- merge(grid_eu_mainland_biogeo,overall_trend_all,by="PLS",all.x=TRUE)

ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_all_sf, aes(fill=mu_past_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(midpoint = 1, name = NULL) + theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_all_sf, aes(fill=mu_ssp1_signif-mu_bau_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.35, 0.15),midpoint = 0, name = NULL) + theme_void()

ggsave("output/map_pred_butterfly_all_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

overall_trend_all_eu <- ddply(predict_trend_all_butterfly_eu,
                              .(PLS,pressure_removed),.fun=overall_mean_sd_trend,
                              .progress = "text")

overall_trend_farmland <- ddply(predict_trend_farmland,
                                .(PLS,pressure_removed),.fun=overall_mean_sd_trend,
                                .progress = "text")

overall_trend_farmland_sf <- merge(grid_eu_mainland_biogeo,overall_trend_farmland,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_farmland_sf, aes(fill=mu_past_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(midpoint = 1, name = NULL) + theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_farmland_sf, aes(fill=mu_ssp1_signif-mu_bau_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.10, 0.06),midpoint = 0, name = NULL) + theme_void()

ggsave("output/map_pred_butterfly_farm_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

overall_trend_forest <- ddply(predict_trend_forest,
                              .(PLS,pressure_removed),.fun=overall_mean_sd_trend,
                              .progress = "text")

overall_trend_forest_sf <- merge(grid_eu_mainland_biogeo,overall_trend_forest,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_forest_sf, aes(fill=mu_past_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(midpoint = 1, name = NULL) + theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=overall_trend_forest_sf, aes(fill=mu_ssp1_signif-mu_bau_signif), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.10, 0.08),midpoint = 0, name = NULL) + theme_void()

ggsave("output/map_pred_butterfly_forest_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)



pressure_removed <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                      "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                      "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                      "eulandsystem_farmland_high")[1]

europe_all <- data.frame(value = c(overall_trend_all$mu_past[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_bau[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_ssp1[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_ssp3[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_nac[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_nfn[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$mu_nfs[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)]),
                         sd = c(overall_trend_all$sd_past[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_bau[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_ssp1[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_ssp3[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_nac[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_nfn[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$sd_nfs[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)]),
                         se = c(overall_trend_all$se_past[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_bau[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_ssp1[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_ssp3[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_nac[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_nfn[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)],
                                overall_trend_all$se_nfs[which(overall_trend_all$PLS=="europe"  & overall_trend_all$pressure_removed == pressure_removed)]),
                         variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_all2 <- europe_all[which(europe_all$variable %in% c("past","bau","ssp1","nfn","nfs","nac")),]
europe_all2$variable <- factor(europe_all2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_all2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_butterfly_eu_all_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="past")]^x/europe_all$value[which(europe_all$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="bau")]^x/europe_all$value[which(europe_all$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="ssp1")]^x/europe_all$value[which(europe_all$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="nfn")]^x/europe_all$value[which(europe_all$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="nfs")]^x/europe_all$value[which(europe_all$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all$value[which(europe_all$variable=="nac")]^x/europe_all$value[which(europe_all$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_butterfly_eu_all.png",
       width = 5,
       height = 3,
       dpi = 300
)

comb_var <- combn(europe_all$variable,2)
test_diff_var_europe_all <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_all[i,3] <- tsum.test(mean.x=europe_all$value[which(europe_all$variable==comb_var[1,i])],   s.x=europe_all$se[which(europe_all$variable==comb_var[1,i])], n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe")],
                                             mean.y=europe_all$value[which(europe_all$variable==comb_var[2,i])],   s.y=europe_all$se[which(europe_all$variable==comb_var[2,i])], n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe")])$p.value
  
}


boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_all$pressure_removed[which(overall_trend_all$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_all$mu_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1[which(overall_trend_all$PLS=="europe")],
                                       overall_trend_all$mu_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs[which(overall_trend_all$PLS=="europe")]), 
                 boxCILow = c(overall_trend_all$mu_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1[which(overall_trend_all$PLS=="europe")],
                              overall_trend_all$mu_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs[which(overall_trend_all$PLS=="europe")])-1.96*c(overall_trend_all$se_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_ssp1[which(overall_trend_all$PLS=="europe")],
                                                                                                                                                                                                                                         overall_trend_all$se_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfs[which(overall_trend_all$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_all$mu_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1[which(overall_trend_all$PLS=="europe")],
                               overall_trend_all$mu_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs[which(overall_trend_all$PLS=="europe")])+1.96*c(overall_trend_all$se_past[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_bau[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_ssp1[which(overall_trend_all$PLS=="europe")],
                                                                                                                                                                                                                                          overall_trend_all$se_nac[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfn[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfs[which(overall_trend_all$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_butterfly_eu_all_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)









europe_all_signif <- data.frame(value = c(overall_trend_all$mu_past_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_bau_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_ssp1_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_ssp3_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_nac_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_nfn_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_nfs_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)]),
                                sd = c(overall_trend_all$sd_past_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_bau_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_ssp1_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_ssp3_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_nac_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_nfn_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_nfs_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)]),
                                se = c(overall_trend_all$se_past_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_bau_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_ssp1_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_ssp3_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_nac_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_nfn_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_nfs_signif[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == pressure_removed)]),
                                variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_all_signif2 <- europe_all_signif[which(europe_all_signif$variable %in% c("past","bau","ssp1","nfn","nfs","nac")),]
europe_all_signif2$variable <- factor(europe_all_signif2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_all_signif2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_butterfly_eu_all_signif_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="past")]^x/europe_all_signif$value[which(europe_all_signif$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="bau")]^x/europe_all_signif$value[which(europe_all_signif$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="ssp1")]^x/europe_all_signif$value[which(europe_all_signif$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="nfn")]^x/europe_all_signif$value[which(europe_all_signif$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="nfs")]^x/europe_all_signif$value[which(europe_all_signif$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_all_signif$value[which(europe_all_signif$variable=="nac")]^x/europe_all_signif$value[which(europe_all_signif$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_butterfly_eu_all_signif.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_all_signif2$variable <- as.character(europe_all_signif2$variable)
comb_var <- combn(europe_all_signif2$variable,2)
test_diff_var_europe_all_signif <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_all_signif[i,3] <- tsum.test(mean.x=europe_all_signif2$value[which(europe_all_signif2$variable==comb_var[1,i])],   s.x=europe_all_signif2$se[which(europe_all_signif2$variable==comb_var[1,i])], n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe")],
                                                    mean.y=europe_all_signif2$value[which(europe_all_signif2$variable==comb_var[2,i])],   s.y=europe_all_signif2$se[which(europe_all_signif2$variable==comb_var[2,i])], n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe")])$p.value
  
}


boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_all$pressure_removed[which(overall_trend_all$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_all$mu_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                                       overall_trend_all$mu_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs_signif[which(overall_trend_all$PLS=="europe")]), 
                 boxCILow = c(overall_trend_all$mu_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                              overall_trend_all$mu_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs_signif[which(overall_trend_all$PLS=="europe")])-1.96*c(overall_trend_all$se_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                                                                                                                                                                                                                                                              overall_trend_all$se_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfs_signif[which(overall_trend_all$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_all$mu_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                               overall_trend_all$mu_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$mu_nfs_signif[which(overall_trend_all$PLS=="europe")])+1.96*c(overall_trend_all$se_past_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_bau_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_ssp1_signif[which(overall_trend_all$PLS=="europe")],
                                                                                                                                                                                                                                                               overall_trend_all$se_nac_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfn_signif[which(overall_trend_all$PLS=="europe")],overall_trend_all$se_nfs_signif[which(overall_trend_all$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_all$n[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_all$n[which(overall_trend_all$PLS=="europe" & overall_trend_all$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_butterfly_eu_all_signif_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)


europe_farmland <- data.frame(value = c(overall_trend_farmland$mu_past[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_bau[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_ssp1[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_ssp3[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_nac[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_nfn[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                        overall_trend_farmland$mu_nfs[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                              sd = c(overall_trend_farmland$sd_past[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_bau[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_ssp1[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_ssp3[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_nac[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_nfn[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$sd_nfs[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                              se = c(overall_trend_farmland$se_past[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_bau[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_ssp1[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_ssp3[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_nac[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_nfn[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                     overall_trend_farmland$se_nfs[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                              variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_farmland2 <- europe_farmland[which(europe_farmland$variable != "ssp3"),]
europe_farmland2$variable <- factor(europe_farmland2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_farmland2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_butterfly_farm_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="past")]^x/europe_farmland$value[which(europe_farmland$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="bau")]^x/europe_farmland$value[which(europe_farmland$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="ssp1")]^x/europe_farmland$value[which(europe_farmland$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="nfn")]^x/europe_farmland$value[which(europe_farmland$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="nfs")]^x/europe_farmland$value[which(europe_farmland$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland$value[which(europe_farmland$variable=="nac")]^x/europe_farmland$value[which(europe_farmland$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_butterfly_farm.png",
       width = 5,
       height = 3,
       dpi = 300
)

boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_farmland$pressure_removed[which(overall_trend_farmland$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_farmland$mu_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1[which(overall_trend_farmland$PLS=="europe")],
                                       overall_trend_farmland$mu_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs[which(overall_trend_farmland$PLS=="europe")]), 
                 boxCILow = c(overall_trend_farmland$mu_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1[which(overall_trend_farmland$PLS=="europe")],
                              overall_trend_farmland$mu_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs[which(overall_trend_farmland$PLS=="europe")])-1.96*c(overall_trend_farmland$se_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_ssp1[which(overall_trend_farmland$PLS=="europe")],
                                                                                                                                                                                                                                                                       overall_trend_farmland$se_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfs[which(overall_trend_farmland$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_farmland$mu_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1[which(overall_trend_farmland$PLS=="europe")],
                               overall_trend_farmland$mu_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs[which(overall_trend_farmland$PLS=="europe")])+1.96*c(overall_trend_farmland$se_past[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_bau[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_ssp1[which(overall_trend_farmland$PLS=="europe")],
                                                                                                                                                                                                                                                                        overall_trend_farmland$se_nac[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfn[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfs[which(overall_trend_farmland$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_butterfly_eu_farmland_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)







europe_farmland_signif <- data.frame(value = c(overall_trend_farmland$mu_past_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_bau_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_ssp1_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_ssp3_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_nac_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_nfn_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                               overall_trend_farmland$mu_nfs_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                                     sd = c(overall_trend_farmland$sd_past_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_bau_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_ssp1_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_ssp3_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_nac_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_nfn_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$sd_nfs_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                                     se = c(overall_trend_farmland$se_past_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_bau_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_ssp1_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_ssp3_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_nac_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_nfn_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)],
                                            overall_trend_farmland$se_nfs_signif[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == pressure_removed)]),
                                     variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_farmland_signif2 <- europe_farmland_signif[which(europe_farmland_signif$variable != "ssp3"),]
europe_farmland_signif2$variable <- factor(europe_farmland_signif2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_farmland_signif2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_butterfly_farm_signif_error.png",
       width = 3,
       height = 5,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="past")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="bau")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="ssp1")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="nfn")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="nfs")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_farmland_signif$value[which(europe_farmland_signif$variable=="nac")]^x/europe_farmland_signif$value[which(europe_farmland_signif$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_butterfly_farm_signif.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_farmland_signif2$variable <- as.character(europe_farmland_signif2$variable)
comb_var <- combn(europe_farmland_signif2$variable,2)
test_diff_var_europe_farmland_signif <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_farmland_signif[i,3] <- tsum.test(mean.x=europe_farmland_signif2$value[which(europe_farmland_signif2$variable==comb_var[1,i])],   s.x=europe_farmland_signif2$se[which(europe_farmland_signif2$variable==comb_var[1,i])], n.x= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe")],
                                                         mean.y=europe_farmland_signif2$value[which(europe_farmland_signif2$variable==comb_var[2,i])],   s.y=europe_farmland_signif2$se[which(europe_farmland_signif2$variable==comb_var[2,i])], n.y= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe")])$p.value
  
}

boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_farmland$pressure_removed[which(overall_trend_farmland$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_farmland$mu_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                                       overall_trend_farmland$mu_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs_signif[which(overall_trend_farmland$PLS=="europe")]), 
                 boxCILow = c(overall_trend_farmland$mu_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                              overall_trend_farmland$mu_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs_signif[which(overall_trend_farmland$PLS=="europe")])-1.96*c(overall_trend_farmland$se_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                                                                                                                                                                                                                                                                                            overall_trend_farmland$se_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfs_signif[which(overall_trend_farmland$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_farmland$mu_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                               overall_trend_farmland$mu_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$mu_nfs_signif[which(overall_trend_farmland$PLS=="europe")])+1.96*c(overall_trend_farmland$se_past_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_bau_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_ssp1_signif[which(overall_trend_farmland$PLS=="europe")],
                                                                                                                                                                                                                                                                                             overall_trend_farmland$se_nac_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfn_signif[which(overall_trend_farmland$PLS=="europe")],overall_trend_farmland$se_nfs_signif[which(overall_trend_farmland$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_farmland$n[which(overall_trend_farmland$PLS=="europe" & overall_trend_farmland$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_butterfly_eu_farmland_signif_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)







europe_forest <- data.frame(value = c(overall_trend_forest$mu_past[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_bau[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_ssp1[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_ssp3[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_nac[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_nfn[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                      overall_trend_forest$mu_nfs[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                            sd = c(overall_trend_forest$sd_past[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_bau[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_ssp1[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_ssp3[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_nac[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_nfn[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$sd_nfs[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                            se = c(overall_trend_forest$se_past[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_bau[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_ssp1[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_ssp3[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_nac[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_nfn[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                   overall_trend_forest$se_nfs[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                            variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_forest2 <- europe_forest[which(europe_forest$variable != "ssp3"),]
europe_forest2$variable <- factor(europe_forest2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_forest2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_butterfly_forest_error.png",
       width = 3,
       height = 5,
       dpi = 300
)


ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="past")]^x/europe_forest$value[which(europe_forest$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="bau")]^x/europe_forest$value[which(europe_forest$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="ssp1")]^x/europe_forest$value[which(europe_forest$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="nfn")]^x/europe_forest$value[which(europe_forest$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="nfs")]^x/europe_forest$value[which(europe_forest$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest$value[which(europe_forest$variable=="nac")]^x/europe_forest$value[which(europe_forest$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_butterfly_forest.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_forest2$variable <- as.character(europe_forest2$variable)
comb_var <- combn(europe_forest2$variable,2)
test_diff_var_europe_forest <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_forest[i,3] <- tsum.test(mean.x=europe_forest2$value[which(europe_forest2$variable==comb_var[1,i])],   s.x=europe_forest2$se[which(europe_forest2$variable==comb_var[1,i])], n.x= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe")],
                                                mean.y=europe_forest2$value[which(europe_forest2$variable==comb_var[2,i])],   s.y=europe_forest2$se[which(europe_forest2$variable==comb_var[2,i])], n.y= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe")])$p.value
  
}


boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_forest$pressure_removed[which(overall_trend_forest$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_forest$mu_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1[which(overall_trend_forest$PLS=="europe")],
                                       overall_trend_forest$mu_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs[which(overall_trend_forest$PLS=="europe")]), 
                 boxCILow = c(overall_trend_forest$mu_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1[which(overall_trend_forest$PLS=="europe")],
                              overall_trend_forest$mu_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs[which(overall_trend_forest$PLS=="europe")])-1.96*c(overall_trend_forest$se_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_ssp1[which(overall_trend_forest$PLS=="europe")],
                                                                                                                                                                                                                                                           overall_trend_forest$se_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfs[which(overall_trend_forest$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_forest$mu_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1[which(overall_trend_forest$PLS=="europe")],
                               overall_trend_forest$mu_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs[which(overall_trend_forest$PLS=="europe")])+1.96*c(overall_trend_forest$se_past[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_bau[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_ssp1[which(overall_trend_forest$PLS=="europe")],
                                                                                                                                                                                                                                                            overall_trend_forest$se_nac[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfn[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfs[which(overall_trend_forest$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_butterfly_eu_forest_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)






europe_forest_signif <- data.frame(value = c(overall_trend_forest$mu_past_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_bau_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_ssp1_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_ssp3_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_nac_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_nfn_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                             overall_trend_forest$mu_nfs_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                                   sd = c(overall_trend_forest$sd_past_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_bau_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_ssp1_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_ssp3_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_nac_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_nfn_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$sd_nfs_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                                   se = c(overall_trend_forest$se_past_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_bau_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_ssp1_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_ssp3_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_nac_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_nfn_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)],
                                          overall_trend_forest$se_nfs_signif[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == pressure_removed)]),
                                   variable = c("past","bau","ssp1","ssp3","nac","nfn","nfs"))

europe_forest_signif2 <- europe_forest_signif[which(europe_forest_signif$variable != "ssp3"),]
europe_forest_signif2$variable <- factor(europe_forest_signif2$variable, levels = c("past","bau","ssp1","nfn","nfs","nac"))
ggplot(europe_forest_signif2, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("past"="black","bau"="red","ssp1"="blue","nfn"="darkgreen","nfs"="green","nac"="lightgreen")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave("output/trend_butterfly_forest_signif_error.png",
       width = 3,
       height = 5,
       dpi = 300
)


ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="past")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="past")]^2021*100}, colour = "black", linetype=2, xlim=c(2000,2021)) +
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="bau")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="bau")]^2021*100}, colour = "red", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="ssp1")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="ssp1")]^2021*100}, colour = "blue", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="nfn")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="nfn")]^2021*100}, colour = "darkgreen", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="nfs")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="nfs")]^2021*100}, colour = "green", xlim=c(2021,2050)) + 
  geom_function(fun = function(x){europe_forest_signif$value[which(europe_forest_signif$variable=="nac")]^x/europe_forest_signif$value[which(europe_forest_signif$variable=="nac")]^2021*100}, colour = "lightgreen", xlim=c(2021,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave("output/trend_butterfly_forest_signif.png",
       width = 5,
       height = 3,
       dpi = 300
)

europe_forest_signif2$variable <- as.character(europe_forest_signif2$variable)
comb_var <- combn(europe_forest_signif2$variable,2)
test_diff_var_europe_forest_signif <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_europe_forest_signif[i,3] <- tsum.test(mean.x=europe_forest_signif2$value[which(europe_forest_signif2$variable==comb_var[1,i])],   s.x=europe_forest_signif2$se[which(europe_forest_signif2$variable==comb_var[1,i])], n.x= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe")],
                                                       mean.y=europe_forest_signif2$value[which(europe_forest_signif2$variable==comb_var[2,i])],   s.y=europe_forest_signif2$se[which(europe_forest_signif2$variable==comb_var[2,i])], n.y= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe")])$p.value
  
}


boxLabels <- c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
               "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
               "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
               "eulandsystem_farmland_high")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("SSP1",length(boxLabels)),rep("NAC",length(boxLabels)),rep("NFN",length(boxLabels)),rep("NFS",length(boxLabels))),
                 Variable = rep(overall_trend_forest$pressure_removed[which(overall_trend_forest$PLS=="europe")],6),
                 box_estimate_main = c(overall_trend_forest$mu_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                                       overall_trend_forest$mu_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs_signif[which(overall_trend_forest$PLS=="europe")]), 
                 boxCILow = c(overall_trend_forest$mu_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                              overall_trend_forest$mu_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs_signif[which(overall_trend_forest$PLS=="europe")])-1.96*c(overall_trend_forest$se_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                                                                                                                                                                                                                                                                                overall_trend_forest$se_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfs_signif[which(overall_trend_forest$PLS=="europe")]),
                 boxCIHigh = c(overall_trend_forest$mu_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                               overall_trend_forest$mu_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$mu_nfs_signif[which(overall_trend_forest$PLS=="europe")])+1.96*c(overall_trend_forest$se_past_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_bau_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_ssp1_signif[which(overall_trend_forest$PLS=="europe")],
                                                                                                                                                                                                                                                                                 overall_trend_forest$se_nac_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfn_signif[which(overall_trend_forest$PLS=="europe")],overall_trend_forest$se_nfs_signif[which(overall_trend_forest$PLS=="europe")]))


df$Attribute <- factor(df$Attribute, levels = c("Past", "BAU","SSP1","NAC","NFN","NFS"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
                                              "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium",
                                              "eulandsystem_farmland_high"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% mutate(pvalue = tsum.test(mean.x=box_estimate_main,   s.x=((boxCIHigh - mean_y)/1.96), n.x= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == "none")],
                                                                           mean.y=mean_y,s.y=se_y, n.y= overall_trend_forest$n[which(overall_trend_forest$PLS=="europe" & overall_trend_forest$pressure_removed == "none")],)$p.value)))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  


ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","eulandsystem_farmland_low" = "\u2205 Low intensive farmland",
                            "eulandsystem_farmland_medium" = "\u2205 Medium intensive farmland", "eulandsystem_farmland_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","BAU"="red","SSP1"="blue","NFN"="darkgreen","NFS"="green","NAC"="lightgreen")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave("output/trend_butterfly_eu_forest_signif_effect.png",
       width = 9,
       height = 6,
       dpi = 300
)


# plot pressure change

lulc_sf <- merge(grid_eu_mainland_biogeo,lulc_pls_short,by="PLS",all.x=TRUE)

lulc_pls_short_farm_for <- reshape2::melt(lulc_pls_short, id.vars=c("PLS","variable"))
names(lulc_pls_short_farm_for)[3] <- "scenario"
lulc_pls_short_farm_for <- reshape2::dcast(lulc_pls_short_farm_for, PLS + scenario ~ variable, value.var = "value")
lulc_pls_short_farm_for$farmland <- lulc_pls_short_farm_for$farmland_low + lulc_pls_short_farm_for$farmland_medium + lulc_pls_short_farm_for$farmland_high
lulc_pls_short_farm_for$forest <- lulc_pls_short_farm_for$forest_lowmedium + lulc_pls_short_farm_for$forest_high
lulc_pls_short_farm_for <- reshape2::melt(lulc_pls_short_farm_for, id.vars=c("PLS","scenario"))
lulc_pls_short_farm_for <- reshape2::dcast(lulc_pls_short_farm_for, PLS + variable ~ scenario, value.var = "value")
lulc_sf_farm_for <- merge(grid_eu_mainland_biogeo,lulc_pls_short_farm_for,by="PLS",all.x=TRUE)

lulc_pls_compare <- reshape2::melt(lulc_pls_short_farm_for[,c("PLS","variable","initial")], id.vars=c("PLS","variable"))
lulc_pls_compare <- reshape2::dcast(lulc_pls_compare[,-3], PLS ~ variable, value.var = "value")


ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="urban"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="urban"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="urban"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), high = "#a80000ff",name = NULL) + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="urban"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(0,0.06), name = NULL,high = "#a80000ff") + theme_void()

ggsave("output/map_urban_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="farmland_low"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="farmland_low"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="farmland_low"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL, high = "#e6a600ff") +
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="farmland_low"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.3,0.2), name = NULL,low = "#888888ff", high = "#e6a600ff") + theme_void()

ggsave("output/map_farmland_low_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="farmland_medium"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="farmland_medium"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="farmland_medium"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL,high = "#e68e00ff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="farmland_medium"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.3,0.05), name = NULL,low = "#888888ff", high = "#e68e00ff") + theme_void()

ggsave("output/map_farmland_medium_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="farmland_high"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="farmland_high"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="farmland_high"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL,high = "#e67500ff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="farmland_high"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.05,0.2), name = NULL,low = "#888888ff", high = "#e67500ff") + theme_void()

ggsave("output/map_farmland_high_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf_farm_for[which(lulc_sf_farm_for$variable=="farmland"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf_farm_for[which(lulc_sf_farm_for$variable=="farmland"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf_farm_for[which(lulc_sf_farm_for$variable=="farmland"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL,high = "yellow") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf_farm_for[which(lulc_sf_farm_for$variable=="farmland"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.32,0.2), name = NULL,low = "#888888ff", high = "yellow") + theme_void()

ggsave("output/map_farmland_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)


ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="forest_lowmedium"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="forest_lowmedium"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="forest_lowmedium"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL,high = "#00c800ff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="forest_lowmedium"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.2,0.1), name = NULL,low = "#888888ff", high = "#00c800ff") + theme_void()

ggsave("output/map_forest_low_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="forest_high"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="forest_high"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="forest_high"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL,high = "#82a600ff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="forest_high"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.05,0.2), name = NULL,low = "#888888ff", high = "#82a600ff") + theme_void()

ggsave("output/map_forest_high_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)


ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf_farm_for[which(lulc_sf_farm_for$variable=="forest"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf_farm_for[which(lulc_sf_farm_for$variable=="forest"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf_farm_for[which(lulc_sf_farm_for$variable=="forest"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL,high = "green") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf_farm_for[which(lulc_sf_farm_for$variable=="forest"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.2,0.2), name = NULL,low = "#888888ff", high = "green") + theme_void()

ggsave("output/map_forest_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)


ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="landscape_div"),], aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="landscape_div"),c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(lulc_sf[which(lulc_sf$variable=="landscape_div"),c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL,high = "#66cdabff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=lulc_sf[which(lulc_sf$variable=="landscape_div"),], aes(fill=ssp1-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(-0.11,0.32), name = NULL,low = "#888888ff", high = "#66cdabff") + theme_void()

ggsave("output/map_landscape_div_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

pa_sf <- merge(grid_eu_mainland_biogeo,pa_pls_short,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=pa_sf, aes(fill=initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(pa_sf[,c("initial","ssp1","ssp3","nac","nfn","nfs")]))),
                                max(na.omit(st_drop_geometry(pa_sf[,c("initial","ssp1","ssp3","nac","nfn","nfs")])))), name = NULL,high = "#66bdcdff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=pa_sf, aes(fill=nfn-initial), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(0,0.25), name = NULL,low = "#888888ff", high = "#66bdcdff") + theme_void()

ggsave("output/map_pa_bau.png",
       width = 8,
       height = 8,
       dpi = 300
)

climate_sf <- merge(grid_eu_mainland_biogeo,climate_pls,by="PLS",all.x=TRUE)
climate_sf$d_mean <- climate_sf$mean_t_4_5 - climate_sf$mean_t_2016
climate_sf$d_sum <- climate_sf$sum_p_4_5 - climate_sf$sum_p_2016
climate_sf$d_var <- climate_sf$var_t_4_5 - climate_sf$var_t_2016
ggplot() + geom_sf() +  
  geom_sf(data=climate_sf, aes(fill=mean_t_2016), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(climate_sf[,c("mean_t_2016","mean_t_4_5")]))),
                                max(na.omit(st_drop_geometry(climate_sf[,c("mean_t_2016","mean_t_4_5")])))), name = NULL,high = "#cd66a6ff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=climate_sf, aes(fill=d_mean), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(name = NULL,high = "#cd66a6ff") + theme_void()

ggsave("output/map_temp_2016.png",
       width = 8,
       height = 8,
       dpi = 300
)


ggplot() + geom_sf() +  
  geom_sf(data=climate_sf, aes(fill=sum_p_2016), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(climate_sf[,c("sum_p_2016","sum_p_4_5")]))),
                                max(na.omit(st_drop_geometry(climate_sf[,c("sum_p_2016","sum_p_4_5")])))), name = NULL,high = "#6f66cdff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=climate_sf, aes(fill=d_sum), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(name = NULL,low = "#888888ff",high = "#6f66cdff") + theme_void()

ggsave("output/map_prec_2016.png",
       width = 8,
       height = 8,
       dpi = 300
)

ggplot() + geom_sf() +  
  geom_sf(data=climate_sf, aes(fill=var_t_2016), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(limits=c(min(na.omit(st_drop_geometry(climate_sf[,c("var_t_2016","var_t_4_5")]))),
                                max(na.omit(st_drop_geometry(climate_sf[,c("var_t_2016","var_t_4_5")])))), name = NULL,high = "#9966cdff") + 
  theme_void()
ggplot() + geom_sf() +  
  geom_sf(data=climate_sf, aes(fill=d_var), col = NA) + 
  geom_sf(data=grid_eu_mainland_outline, fill=NA) +
  scale_fill_gradient2(name = NULL,low = "#888888ff",high = "#9966cdff") + theme_void()

ggsave("output/map_var_2016.png",
       width = 8,
       height = 8,
       dpi = 300
)












#make this example reproducible

set.seed(0)

#split the dataset into a training set (80%) and test set (20%).

training_obs <- poisson_df$count_scale_all %>% createDataPartition(p = 0.8, list = FALSE)

train <- poisson_df[training_obs, ]
test <- poisson_df[-training_obs, ]

# Build the regression model on the training set

global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                      family=family, data=train)

# Use the model to make predictions on the test set

predictions <- global_mod %>% predict(test)

#Examine R-squared, RMSE, and MAE of predictions

data.frame(R_squared = R2(predictions, test$count_scale_all),
           RMSE = RMSE(predictions, test$count_scale_all),
           MAE = MAE(predictions, test$count_scale_all))

cvgam <- CVgam(formula=as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
      data = poisson_df, nfold = 10, debug.level = 0, method = "GCV.Cp",
      printit = TRUE, cvparts = NULL, gamma = 1, seed = 29)

