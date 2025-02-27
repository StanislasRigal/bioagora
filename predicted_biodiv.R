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
lulc_pls_2015 <- melt(lulc_pls_2015,id.vars="PLS")
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
lulc_pls_ssp1 <- melt(lulc_pls_ssp1,id.vars="PLS")
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
lulc_pls_ssp3 <- melt(lulc_pls_ssp3,id.vars="PLS")
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
lulc_pls_nac <- melt(lulc_pls_nac,id.vars="PLS")
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
lulc_pls_nfn <- melt(lulc_pls_nfn,id.vars="PLS")
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
lulc_pls_nfs <- melt(lulc_pls_nfs,id.vars="PLS")
lulc_pls_nfs$scenario <- "nfs"

lulc_pls <- rbind(lulc_pls_2015,lulc_pls_ssp1,lulc_pls_ssp3,lulc_pls_nac,lulc_pls_nfn,lulc_pls_nfs)

lulc_pls_short <- dcast(lulc_pls, PLS + variable ~ scenario, value.var = "value")

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
pa_pls$nfn <- pa_pls$frac_1 + pa_pls$frac_2 + pa_pls$frac_3 + pa_pls$frac_7 + pa_pls$frac_8 + pa_pls$frac_12
pa_pls$nac <- pa_pls$frac_1 + pa_pls$frac_2 + pa_pls$frac_4 + pa_pls$frac_7 + pa_pls$frac_9 + pa_pls$frac_12
pa_pls$nfs <- pa_pls$frac_1 + pa_pls$frac_2 + pa_pls$frac_5 + pa_pls$frac_8 + pa_pls$frac_9 + pa_pls$frac_12
pa_pls$initial <- pa_pls$frac_1 + pa_pls$frac_2
pa_pls$PLS <- c(1:19,21:25)

pa_pls_short <- pa_pls[,c("PLS","nfn","nfs","nac","initial")]
pa_pls_short$ssp1 <- pa_pls_short$ssp3 <- pa_pls_short$initial
pa_pls_short <- melt(pa_pls_short,id.vars="PLS")
names(pa_pls_short)[2] <- "scenario"
pa_pls_short$variable <- "protected area"
pa_pls_short <- dcast(pa_pls_short, PLS + variable ~ scenario, value.var = "value")

#pa_pls_current <- exact_extract(protected_area_rast_reproj2,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac", default_value=0)
#pa_pls_current$protected_perc <- pa_pls_current$frac_1 + pa_pls_current$frac_2

saveRDS(pa_pls_short,"output/lulc_pls_short.rds")


# load change in climate https://cds.climate.copernicus.eu/

#36584 01/01/1950 - 01/03/2050
#36705 01/01/1950 - 30/06/2050


mean_t_4_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
mean_t_4_5_cclm <- (mean_t_4_5_cclm$X2050.03.01 + mean_t_4_5_cclm$X2050.04.01 + mean_t_4_5_cclm$X2050.05.01 + mean_t_4_5_cclm$X2050.06.01)/4
mean_t_4_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
mean_t_4_5_hirham <- (mean_t_4_5_hirham$X2050.03.01 + mean_t_4_5_hirham$X2050.04.01 + mean_t_4_5_hirham$X2050.05.01 + mean_t_4_5_hirham$X2050.06.01)/4
mean_t_4_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
mean_t_4_5_racmo_ecearth <- (mean_t_4_5_racmo_ecearth$X2050.03.01 + mean_t_4_5_racmo_ecearth$X2050.04.01 + mean_t_4_5_racmo_ecearth$X2050.05.01 + mean_t_4_5_racmo_ecearth$X2050.06.01)/4
mean_t_4_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
mean_t_4_5_racmo_hadgem <- (mean_t_4_5_racmo_hadgem$X2050.03.01 + mean_t_4_5_racmo_hadgem$X2050.04.01 + mean_t_4_5_racmo_hadgem$X2050.05.01 + mean_t_4_5_racmo_hadgem$X2050.06.01)/4
mean_t_4_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
mean_t_4_5_rca_hadgem <- (mean_t_4_5_rca_hadgem$X2050.03.01 + mean_t_4_5_rca_hadgem$X2050.04.01 + mean_t_4_5_rca_hadgem$X2050.05.01 + mean_t_4_5_rca_hadgem$X2050.06.01)/4
mean_t_4_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
mean_t_4_5_rca_mpi <- (mean_t_4_5_rca_mpi$X2050.03.01 + mean_t_4_5_rca_mpi$X2050.04.01 + mean_t_4_5_rca_mpi$X2050.05.01 + mean_t_4_5_rca_mpi$X2050.06.01)/4
mean_t_4_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/01_mean_temperature-projections-monthly-rcp_4_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
mean_t_4_5_wrf <- (mean_t_4_5_wrf$X2050.03.01 + mean_t_4_5_wrf$X2050.04.01 + mean_t_4_5_wrf$X2050.05.01 + mean_t_4_5_wrf$X2050.06.01)/4

mean_t_4_5 <- (mean_t_4_5_cclm + mean_t_4_5_hirham + mean_t_4_5_racmo_ecearth + mean_t_4_5_racmo_hadgem + mean_t_4_5_rca_hadgem + mean_t_4_5_rca_mpi + mean_t_4_5_wrf)/7 - 272.15


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
sum_p_4_5_cclm <- sum_p_4_5_cclm$X2050.03.01 + sum_p_4_5_cclm$X2050.04.01 + sum_p_4_5_cclm$X2050.05.01 + sum_p_4_5_cclm$X2050.06.01
sum_p_4_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
sum_p_4_5_hirham <- sum_p_4_5_hirham$X2050.03.01 + sum_p_4_5_hirham$X2050.04.01 + sum_p_4_5_hirham$X2050.05.01 + sum_p_4_5_hirham$X2050.06.01
sum_p_4_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
sum_p_4_5_racmo_ecearth <- sum_p_4_5_racmo_ecearth$X2050.03.01 + sum_p_4_5_racmo_ecearth$X2050.04.01 + sum_p_4_5_racmo_ecearth$X2050.05.01 + sum_p_4_5_racmo_ecearth$X2050.06.01
sum_p_4_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
sum_p_4_5_racmo_hadgem <- sum_p_4_5_racmo_hadgem$X2050.03.01 + sum_p_4_5_racmo_hadgem$X2050.04.01 + sum_p_4_5_racmo_hadgem$X2050.05.01 + sum_p_4_5_racmo_hadgem$X2050.06.01
sum_p_4_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
sum_p_4_5_rca_hadgem <- sum_p_4_5_rca_hadgem$X2050.03.01 + sum_p_4_5_rca_hadgem$X2050.04.01 + sum_p_4_5_rca_hadgem$X2050.05.01 + sum_p_4_5_rca_hadgem$X2050.06.01
sum_p_4_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
sum_p_4_5_rca_mpi <- sum_p_4_5_rca_mpi$X2050.03.01 + sum_p_4_5_rca_mpi$X2050.04.01 + sum_p_4_5_rca_mpi$X2050.05.01 + sum_p_4_5_rca_mpi$X2050.06.01
sum_p_4_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/12_total_precipitation-projections-monthly-rcp_4_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
sum_p_4_5_wrf <- sum_p_4_5_wrf$X2050.03.01 + sum_p_4_5_wrf$X2050.04.01 + sum_p_4_5_wrf$X2050.05.01 + sum_p_4_5_wrf$X2050.06.01

sum_p_4_5 <- (sum_p_4_5_cclm + sum_p_4_5_hirham + sum_p_4_5_racmo_ecearth + sum_p_4_5_racmo_hadgem + sum_p_4_5_rca_hadgem + sum_p_4_5_rca_mpi + sum_p_4_5_wrf)/7

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
max_t_4_5_cclm <- (max_t_4_5_cclm$X2050.03.01 + max_t_4_5_cclm$X2050.04.01 + max_t_4_5_cclm$X2050.05.01 + max_t_4_5_cclm$X2050.06.01)/4
max_t_4_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
max_t_4_5_hirham <- (max_t_4_5_hirham$X2050.03.01 + max_t_4_5_hirham$X2050.04.01 + max_t_4_5_hirham$X2050.05.01 + max_t_4_5_hirham$X2050.06.01)/4
max_t_4_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
max_t_4_5_racmo_ecearth <- (max_t_4_5_racmo_ecearth$X2050.03.01 + max_t_4_5_racmo_ecearth$X2050.04.01 + max_t_4_5_racmo_ecearth$X2050.05.01 + max_t_4_5_racmo_ecearth$X2050.06.01)/4
max_t_4_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
max_t_4_5_racmo_hadgem <- (max_t_4_5_racmo_hadgem$X2050.03.01 + max_t_4_5_racmo_hadgem$X2050.04.01 + max_t_4_5_racmo_hadgem$X2050.05.01 + max_t_4_5_racmo_hadgem$X2050.06.01)/4
max_t_4_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
max_t_4_5_rca_hadgem <- (max_t_4_5_rca_hadgem$X2050.03.01 + max_t_4_5_rca_hadgem$X2050.04.01 + max_t_4_5_rca_hadgem$X2050.05.01 + max_t_4_5_rca_hadgem$X2050.06.01)/4
max_t_4_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
max_t_4_5_rca_mpi <- (max_t_4_5_rca_mpi$X2050.03.01 + max_t_4_5_rca_mpi$X2050.04.01 + max_t_4_5_rca_mpi$X2050.05.01 + max_t_4_5_rca_mpi$X2050.06.01)/4
max_t_4_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l1_daily_maximum_temperature-projections-monthly-mean-rcp_4_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
max_t_4_5_wrf <- (max_t_4_5_wrf$X2050.03.01 + max_t_4_5_wrf$X2050.04.01 + max_t_4_5_wrf$X2050.05.01 + max_t_4_5_wrf$X2050.06.01)/4

max_t_4_5 <- (max_t_4_5_cclm + max_t_4_5_hirham + max_t_4_5_racmo_ecearth + max_t_4_5_racmo_hadgem + max_t_4_5_rca_hadgem + max_t_4_5_rca_mpi + max_t_4_5_wrf)/7 - 272.15

min_t_4_5_cclm <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-cclm4_8_17-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
min_t_4_5_cclm <- (min_t_4_5_cclm$X2050.03.01 + min_t_4_5_cclm$X2050.04.01 + min_t_4_5_cclm$X2050.05.01 + min_t_4_5_cclm$X2050.06.01)/4
min_t_4_5_hirham <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-hirham5-noresm1_m-r1i1p1-grid-v1.0.nc")
min_t_4_5_hirham <- (min_t_4_5_hirham$X2050.03.01 + min_t_4_5_hirham$X2050.04.01 + min_t_4_5_hirham$X2050.05.01 + min_t_4_5_hirham$X2050.06.01)/4
min_t_4_5_racmo_ecearth <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-racmo22e-ec_earth-r1i1p1-grid-v1.0.nc")
min_t_4_5_racmo_ecearth <- (min_t_4_5_racmo_ecearth$X2050.03.01 + min_t_4_5_racmo_ecearth$X2050.04.01 + min_t_4_5_racmo_ecearth$X2050.05.01 + min_t_4_5_racmo_ecearth$X2050.06.01)/4
min_t_4_5_racmo_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-racmo22e-hadgem2_es-r1i1p1-grid-v1.0.nc")
min_t_4_5_racmo_hadgem <- (min_t_4_5_racmo_hadgem$X2050.03.01 + min_t_4_5_racmo_hadgem$X2050.04.01 + min_t_4_5_racmo_hadgem$X2050.05.01 + min_t_4_5_racmo_hadgem$X2050.06.01)/4
min_t_4_5_rca_hadgem <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-rca4-hadgem2_es-r1i1p1-grid-v1.0.nc")
min_t_4_5_rca_hadgem <- (min_t_4_5_rca_hadgem$X2050.03.01 + min_t_4_5_rca_hadgem$X2050.04.01 + min_t_4_5_rca_hadgem$X2050.05.01 + min_t_4_5_rca_hadgem$X2050.06.01)/4
min_t_4_5_rca_mpi <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-rca4-mpi_esm_lr-r1i1p1-grid-v1.0.nc")
min_t_4_5_rca_mpi <- (min_t_4_5_rca_mpi$X2050.03.01 + min_t_4_5_rca_mpi$X2050.04.01 + min_t_4_5_rca_mpi$X2050.05.01 + min_t_4_5_rca_mpi$X2050.06.01)/4
min_t_4_5_wrf <- brick("raw_data/predicted_climate/bdbb154c789566ea9935529937de5e88/l2_daily_minimum_temperature-projections-monthly-mean-rcp_4_5-wrf381p-ipsl_cm5a_mr-r1i1p1-grid-v1.0.nc")
min_t_4_5_wrf <- (min_t_4_5_wrf$X2050.03.01 + min_t_4_5_wrf$X2050.04.01 + min_t_4_5_wrf$X2050.05.01 + min_t_4_5_wrf$X2050.06.01)/4

min_t_4_5 <- (min_t_4_5_cclm + min_t_4_5_hirham + min_t_4_5_racmo_ecearth + min_t_4_5_racmo_hadgem + min_t_4_5_rca_hadgem + min_t_4_5_rca_mpi + min_t_4_5_wrf)/7 - 272.15

var_t_4_5 <- max_t_4_5-min_t_4_5

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
