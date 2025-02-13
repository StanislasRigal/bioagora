# load prediction maps from https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/NWGCBY

lulc_2015 <- rast(raster("raw_data/predicted_land_system/ssp1_cov0_eu_updated.tif"),crs="EPSG:3035")
lulc_ssp1 <- rast(raster("raw_data/predicted_land_system/SSP1_Europe.tif"),crs="EPSG:3035")
lulc_ssp3 <- rast(raster("raw_data/predicted_land_system/SSP3_Europe.tif"),crs="EPSG:3035")
lulc_nac <- rast(raster("raw_data/predicted_land_system/nac_2050_eu.tif"),crs="EPSG:3035")
lulc_nfn <- rast(raster("raw_data/predicted_land_system/nfn_2050_eu.tif"),crs="EPSG:3035")
lulc_nfs <- rast(raster("raw_data/predicted_land_system/nfs_2050_eu.tif"),crs="EPSG:3035")

grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")

ggplot(grid_eu_mainland_biogeo) + tidyterra::geom_spatraster(data=lulc_2015, aes(fill=ssp1_cov0_eu_updated)) +
  scale_fill_viridis_b(na.value = NA) +
  geom_sf(fill=NA, col="white")


lulc_pls_2015 <- exact_extract(lulc_2015,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac")
lulc_pls_ssp1 <- exact_extract(lulc_ssp1,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac")
lulc_pls_ssp3 <- exact_extract(lulc_ssp3,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac")
lulc_pls_nac <- exact_extract(lulc_nac,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac")
lulc_pls_nfn <- exact_extract(lulc_nfn,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac")
lulc_pls_nfs <- exact_extract(lulc_nfs,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac")

lulc_pls_2015$PLS <- row.names(lulc_pls_2015)
lulc_pls_2015 <- melt(lulc_pls_2015,id.vars="PLS")
lulc_pls_2015$scenario <- "initial"

lulc_pls_ssp1$PLS <- row.names(lulc_pls_ssp1)
lulc_pls_ssp1 <- melt(lulc_pls_ssp1,id.vars="PLS")
lulc_pls_ssp1$scenario <- "ssp1"

lulc_pls_ssp3$PLS <- row.names(lulc_pls_ssp3)
lulc_pls_ssp3 <- melt(lulc_pls_ssp3,id.vars="PLS")
lulc_pls_ssp3$scenario <- "ssp3"

lulc_pls_nac$PLS <- row.names(lulc_pls_nac)
lulc_pls_nac <- melt(lulc_pls_nac,id.vars="PLS")
lulc_pls_nac$scenario <- "nac"

lulc_pls_nfn$PLS <- row.names(lulc_pls_nfn)
lulc_pls_nfn <- melt(lulc_pls_nfn,id.vars="PLS")
lulc_pls_nfn$scenario <- "nfn"

lulc_pls_nfs$PLS <- row.names(lulc_pls_nfs)
lulc_pls_nfs <- melt(lulc_pls_nfs,id.vars="PLS")
lulc_pls_nfs$scenario <- "nfs"

lulc_pls <- rbind(lulc_pls_2015,lulc_pls_ssp1,lulc_pls_ssp3,lulc_pls_nac,lulc_pls_nfn,lulc_pls_nfs)

# load change in protected areas from https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/TCNKPJ

#pa_2050 <- rast(raster("raw_data/predicted_protected_areas/FIG1_3values_optimal_1k_top10.tif"))
pa_2050 <- rast(raster("raw_data/predicted_protected_areas/FIG2_3values_expand_top5.tif"))

ggplot(grid_eu_mainland_biogeo) + tidyterra::geom_spatraster(data=pa_2050, aes(fill=FIG2_3values_expand_top5)) +
  scale_fill_viridis_b(na.value = NA) +
  geom_sf(fill=NA, col="white")

pa_pls <- exact_extract(pa_2050,grid_eu_mainland_biogeo[which(!is.na(grid_eu_mainland_biogeo$PLS)),], fun="frac")


# load change in climate

