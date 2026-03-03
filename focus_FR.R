grid_fr <- st_read("output/grid_fr.gpkg")

prec_spring_2000_average_FR <- rast(raster(x='raw_data/climate/prec_spring_2000_average_FR.tif')) #2000-2002
prec_spring_2020_average_FR <- rast(raster(x='raw_data/climate/prec_spring_2020_average_FR.tif')) #2018-2020

temp_spring_var_2000_average_FR <- rast(raster(x='raw_data/climate/temp_spring_var_2000_average_FR.tif'))
temp_spring_var_2020_average_FR <- rast(raster(x='raw_data/climate/temp_spring_var_2020_average_FR.tif'))

temp_spring_2000_average_FR <- rast(raster(x='raw_data/climate/temp_spring_2000_average_FR.tif'))
temp_spring_2020_average_FR <- rast(raster(x='raw_data/climate/temp_spring_2020_average_FR.tif'))

prec_spring_2000_average_FR <- project(prec_spring_2000_average_FR, crs(grid_fr))
prec_spring_2000_average_FR <- crop(prec_spring_2000_average_FR,ext(grid_fr))
prec_2000 <- exact_extract(prec_spring_2000_average_FR,grid_fr, fun=c("sum","count"))
prec_2000$mean <- prec_2000$sum/prec_2000$count

prec_spring_2020_average_FR <- project(prec_spring_2020_average_FR, crs(grid_fr))
prec_spring_2020_average_FR <- crop(prec_spring_2020_average_FR,ext(grid_fr))
prec_2020 <- exact_extract(prec_spring_2020_average_FR,grid_fr, fun=c("sum","count"))
prec_2020$mean <- prec_2020$sum/prec_2020$count

temp_spring_2000_average_FR <- project(temp_spring_2000_average_FR, crs(grid_fr))
temp_spring_2000_average_FR <- crop(temp_spring_2000_average_FR,ext(grid_fr))
temp_2000 <- exact_extract(temp_spring_2000_average_FR,grid_fr, fun=c("sum","count"))
temp_2000$mean <- temp_2000$sum/temp_2000$count

temp_spring_2020_average_FR <- project(temp_spring_2020_average_FR, crs(grid_fr))
temp_spring_2020_average_FR <- crop(temp_spring_2020_average_FR,ext(grid_fr))
temp_2020 <- exact_extract(temp_spring_2020_average_FR,grid_fr, fun=c("sum","count"))
temp_2020$mean <- temp_2020$sum/temp_2020$count

temp_spring_var_2000_average_FR <- project(temp_spring_var_2000_average_FR, crs(grid_fr))
temp_spring_var_2000_average_FR <- crop(temp_spring_var_2000_average_FR,ext(grid_fr))
temp_var_2000 <- exact_extract(temp_spring_var_2000_average_FR,grid_fr, fun=c("sum","count"))
temp_var_2000$mean <- temp_var_2000$sum/temp_var_2000$count

temp_spring_var_2020_average_FR <- project(temp_spring_var_2020_average_FR, crs(grid_fr))
temp_spring_var_2020_average_FR <- crop(temp_spring_var_2020_average_FR,ext(grid_fr))
temp_var_2020 <- exact_extract(temp_spring_var_2020_average_FR,grid_fr, fun=c("sum","count"))
temp_var_2020$mean <- temp_var_2020$sum/temp_var_2020$count

grid_fr$tempspring2000 <- temp_2000$mean
grid_fr$tempspring2020 <- temp_2020$mean
grid_fr$tempspringvar2000 <- temp_var_2000$mean
grid_fr$tempspringvar2020 <- temp_var_2020$mean
grid_fr$precspring2000 <- prec_2000$mean
grid_fr$precspring2020 <- prec_2020$mean

st_write(grid_fr,"output/grid_fr_temp.gpkg")

### Load previously produced datasets

grid_fr <- st_read("output/grid_fr_temp.gpkg")
site_mainland_sf_reproj <- readRDS("output/site_mainland_sf_reproj.rds")
site_mainland_sf_reproj_fr <- site_mainland_sf_reproj[which(site_mainland_sf_reproj$scheme_code=="FR"),]
grid_fr_outline <- grid_fr[,1] %>% summarise(id="europe")
st_write(grid_fr_outline,"output/grid_fr_outline.gpkg")
grid_fr_biogeo <- grid_fr %>% group_by(PLS) %>% summarise(id="PLS_region")#grid_eu_mainland %>% group_by(biogeo_area) %>% summarise(id="biogeo") 
st_write(grid_fr_biogeo,"output/grid_fr_biogeo.gpkg")
grid_fr_outline <- st_read("output/grid_fr_outline.gpkg")

ss_centroids <- data.frame(st_coordinates(st_centroid(grid_fr_biogeo)),PLS=grid_fr_biogeo$PLS)

ggplot(grid_fr_biogeo) + geom_sf(aes(fill=as.character(PLS)),col=NA) + 
  scale_fill_viridis_d() + theme_minimal() + theme(legend.position = "none") +
  geom_text(data=ss_centroids,aes(x=X,y=Y,label=PLS)) +
  theme(text = element_text(colour = "white"),
        panel.grid = element_line(colour = "white"),
        axis.text = element_text(colour = "white"))

ggsave("output/biogeo_area_fr.png",
       width = 8,
       height = 8,
       dpi = 300
)

grid_fr_outline_plot <- st_transform(grid_fr_outline, crs="EPSG:27572")
site_mainland_sf_reproj_fr_plot <- st_transform(site_mainland_sf_reproj[which(site_mainland_sf_reproj$siteID %in% unique(bird_data_fr$siteID)),], crs="EPSG:27572")

ggplot(grid_fr_outline_plot) +
  geom_sf() +
  geom_sf(data=site_mainland_sf_reproj_fr_plot, size=1) +
  theme_minimal()

ggsave("output/grid_fr_outline_plot.png",
       width = 8,
       height = 8,
       dpi = 300
)



grid_fr_outline_crop <- st_crop(grid_fr_outline, xmin = 4, xmax = 5,ymin = 48, ymax = 49)

site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 1128.379) # sqrt(4000000/pi) small
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 1414.214) # 2000*sqrt(2)/2 small2
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 2500) # medium
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 5000) # high

area_site_mainland <-  st_intersection(site_mainland_buffer, grid_fr)
area_site_mainland$area <- as.numeric(st_area(area_site_mainland))

grid_fr_outline_crop <- st_crop(grid_fr_outline, xmin = 4050000, xmax = 4100000,ymin = 2750000, ymax = 2800000)
area_site_mainland_crop <- st_crop(area_site_mainland, xmin = 4050000, xmax = 4100000,ymin = 2750000, ymax = 2800000)

ggplot(grid_fr_outline) +
  geom_sf() +
  geom_sf(data=area_site_mainland) +
  theme_minimal()

ggplot(grid_fr_outline_crop) +
  geom_sf() +
  geom_sf(data=area_site_mainland_crop) +
  theme_minimal()


## summarize external variable for each site and format them
area_site_mainland_df <- area_site_mainland
st_geometry(area_site_mainland_df) <- NULL

value_site_mainland <- ddply(area_site_mainland_df,.(siteID),
                             .fun = function(x){
                               
                               impervious2006 = weighted.mean(x$impervious2006,x$area); impervious2018 = weighted.mean(x$impervious2018,x$area)
                               treedensity2012 = weighted.mean(x$treedensity2012,x$area); treedensity2018 = weighted.mean(x$treedensity2018,x$area)
                               drymatter2000 = weighted.mean(x$drymatter2000,x$area); drymatter2018 = weighted.mean(x$drymatter2018,x$area)
                               smallwoodyfeatures = weighted.mean(x$smallwoodyfeatures,x$area); fragmentation = weighted.mean(x$fragmentation,x$area)
                               temp2000 = weighted.mean(x$temp2000,x$area); temp2020 = weighted.mean(x$temp2020,x$area); tempspring2000 = weighted.mean(x$tempspring2000,x$area); tempspring2020 = weighted.mean(x$tempspring2020,x$area)
                               tempspringvar2000 = weighted.mean(x$tempspringvar2000,x$area); tempspringvar2020 = weighted.mean(x$tempspringvar2020,x$area)
                               prec2000 = weighted.mean(x$prec2000,x$area); prec2020 = weighted.mean(x$prec2020,x$area); precspring2000 = weighted.mean(x$precspring2000,x$area); precspring2020 = weighted.mean(x$precspring2020,x$area)
                               precspringvar2000 = weighted.mean(x$precspringvar2000); precspringvar2020 = weighted.mean(x$precspringvar2020,x$area)
                               humidity2000 = weighted.mean(x$humidity2000,x$area); humidity2020 = weighted.mean(x$humidity2020,x$area); humidityspring2000 = weighted.mean(x$humidityspring2000,x$area)
                               humidityspring2020 = weighted.mean(x$humidityspring2020,x$area); humidityspringvar2000 = weighted.mean(x$humidityspringvar2000,x$area); humidityspringvar2020 = weighted.mean(x$humidityspringvar2020,x$area)
                               shannon2000 = weighted.mean(x$shannon_2000,x$area); shannon2018 = weighted.mean(x$shannon_2018,x$area)
                               agri2000 = weighted.mean(x$agri_2000,x$area); agri2018 = weighted.mean(x$agri_2018,x$area)
 
                               PLS = data.frame(x %>% group_by(PLS) %>% summarise(biogeo_surface=sum(area)))
                               PLS = PLS$PLS[which.max(PLS$biogeo_surface)]
                               
                               eulandsystem = data.frame(x %>% group_by(eulandsystem) %>% summarise(biogeo_surface=sum(area)))
                               eulandsystem_max = eulandsystem$eulandsystem[which.max(eulandsystem$biogeo_surface)]
                               
                               eulandsystem_forest = eulandsystem[which(eulandsystem$eulandsystem %in% c(41:43)),]
                               eulandsystem_max_forest = eulandsystem_forest$eulandsystem[which.max(eulandsystem_forest$biogeo_surface)]
                               eulandsystem_cat_forest <- "no_forest"
                               if(length(eulandsystem_max_forest) > 0){
                                 eulandsystem_cat_forest <-"low_intensity"
                                 if(eulandsystem_max_forest == 42){
                                   eulandsystem_cat_forest <- "medium_intensity"
                                 }
                                 if(eulandsystem_max_forest == 43){
                                   eulandsystem_cat_forest <- "high_intensity"
                                 }
                               }
                               
                               if(length(eulandsystem_max_forest) > 0){
                                 eulandsystem_forest_lowmedium = sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(41:42))])/sum(eulandsystem_forest$biogeo_surface)
                                 eulandsystem_forest_high = sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(43))])/sum(eulandsystem_forest$biogeo_surface)
                               }else{
                                 eulandsystem_forest_lowmedium <- eulandsystem_forest_high <- 0
                               }
                               
                               
                               eulandsystem_urban = eulandsystem[which(eulandsystem$eulandsystem %in% c(21:23)),]
                               eulandsystem_max_urban = eulandsystem_urban$eulandsystem[which.max(eulandsystem_urban$biogeo_surface)]
                               eulandsystem_cat_urban <- "no_urban"
                               if(length(eulandsystem_max_urban) > 0){
                                 eulandsystem_cat_urban <- "low_intensity"
                                 if(eulandsystem_max_urban == 22){
                                   eulandsystem_cat_urban <- "medium_intensity"
                                 }
                                 if(eulandsystem_max_urban == 23){
                                   eulandsystem_cat_urban <- "high_intensity"
                                 }
                               }
                               
                               eulandsystem_farmland = eulandsystem[which(eulandsystem$eulandsystem %in% c(51,52,53,61,62,63,31,32,731,732,733)),]
                               eulandsystem_farmland_low = sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(51,61,31,731))])
                               eulandsystem_farmland_medium = sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(52,62,732))])
                               eulandsystem_farmland_high = sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(53,63,32,733))])
                               
                               eulandsystem_cat_farmland <- "no_farmland"
                               if(nrow(eulandsystem_farmland) > 0){
                                 eulandsystem_cat_farmland <- "low_intensity"
                                 if(eulandsystem_farmland_medium > eulandsystem_farmland_low & eulandsystem_farmland_medium > eulandsystem_farmland_high){
                                   eulandsystem_cat_farmland <- "medium_intensity"
                                 }
                                 if(eulandsystem_farmland_high > eulandsystem_farmland_medium & eulandsystem_farmland_high > eulandsystem_farmland_low){
                                   eulandsystem_cat_farmland <- "high_intensity"
                                 }
                               }
                               
                               if(nrow(eulandsystem_farmland) > 0){
                                 eulandsystem_farmland_low <- eulandsystem_farmland_low/sum(eulandsystem_farmland$biogeo_surface)
                                 eulandsystem_farmland_medium <- eulandsystem_farmland_medium/sum(eulandsystem_farmland$biogeo_surface)
                                 eulandsystem_farmland_high <- eulandsystem_farmland_high/sum(eulandsystem_farmland$biogeo_surface)
                               }else{
                                 eulandsystem_farmland_low <- eulandsystem_farmland_medium <- eulandsystem_farmland_high <- 0
                               }
                               
                               grassland = sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(51,52,53,72))]) /  sum(eulandsystem$biogeo_surface)
                               farmland = sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(51,52,53,61,62,63,31,32,731,732,733))]) /  sum(eulandsystem$biogeo_surface)
                               if(farmland > 0){
                                 high_farmland <- sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(53,63,32,733))]) / sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(51,52,53,61,62,63,31,32,731,732,733))])
                                 low_farmland <- sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(51,61,31,731))]) / sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(51,52,53,61,62,63,31,32,731,732,733))])
                               }else{
                                 low_farmland <- high_farmland <- NA
                               }
                               high_farmland_tot <- sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(53,63,32,733))]) / sum(eulandsystem$biogeo_surface)
                               low_farmland_tot <- sum(eulandsystem$biogeo_surface[which(eulandsystem$eulandsystem %in% c(51,61,31,731))]) / sum(eulandsystem$biogeo_surface)
                               
                               protectedarea = data.frame(x %>% group_by(protectedarea) %>% summarise(biogeo_surface=sum(area)))
                               if(nrow(protectedarea[!is.na(protectedarea$protectedarea),]) > 0){
                                 protectedarea_perc <- sum(protectedarea$biogeo_surface[!is.na(protectedarea$protectedarea)]) / sum(protectedarea$biogeo_surface)
                                 if(protectedarea_perc > 0.5){
                                   protectedarea_cat <- 1
                                 }else{
                                   protectedarea_cat <- 0
                                 }
                                 protectedarea_type = na.omit(data.frame(x %>% group_by(protectedarea_type) %>% summarise(biogeo_surface=sum(area))))
                                 protectedarea_type <- protectedarea_type$protectedarea_type[which.max(protectedarea_type$biogeo_surface)]
                                 protectedarea_size = na.omit(data.frame(x %>% group_by(protectedarea_size) %>% summarise(biogeo_surface=sum(area))))
                                 protectedarea_size <- sum(protectedarea_size$protectedarea_size)
                               }else{
                                 protectedarea_perc <- protectedarea_type <- protectedarea_size <- protectedarea_cat <- 0
                               }
                               
                               return(data.frame(impervious2006,impervious2018,treedensity2012,treedensity2018,
                                                 drymatter2000,drymatter2018,
                                                 smallwoodyfeatures,temp2000,temp2020,tempspring2000,tempspring2020,
                                                 tempspringvar2000,tempspringvar2020,prec2000,prec2020,precspring2000,precspring2020,
                                                 precspringvar2000,precspringvar2020,humidity2000,humidity2020,humidityspring2000,humidityspring2020,
                                                 humidityspringvar2000,humidityspringvar2020,shannon2000,shannon2018,agri2000,agri2018,
                                                 PLS,eulandsystem_max,grassland,farmland,
                                                 low_farmland,high_farmland,low_farmland_tot,high_farmland_tot,protectedarea_cat,
                                                 protectedarea_perc,protectedarea_type,protectedarea_size,eulandsystem_cat_forest,eulandsystem_cat_urban,eulandsystem_cat_farmland,
                                                 eulandsystem_farmland_low,eulandsystem_farmland_medium,eulandsystem_farmland_high,
                                                 eulandsystem_forest_lowmedium,eulandsystem_forest_high))
                               
                             },.progress = "text")




### add pesticide data

site_mainland_sf_reproj_fr <- site_mainland_sf_reproj[which(site_mainland_sf_reproj$scheme_code=="FR"),]
pesticide_fr <- st_read("raw_data/pesticide_fr/Yearly_exposure_to_active_substance_in_use_air_and_water.gpkg")
site_mainland_sf_reproj_fr <- st_transform(site_mainland_sf_reproj_fr,st_crs(pesticide_fr))


site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 1128.379)
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 1414.214)
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 2500)
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 5000)
area_site_pesticide <- st_intersection(site_mainland_buffer, pesticide_fr)
area_site_pesticide$area <- as.numeric(st_area(area_site_pesticide))


## summarize external variable for each site and format them
area_site_pesticide_df <- area_site_pesticide
st_geometry(area_site_pesticide_df) <- NULL

value_site_pesticide <- ddply(area_site_pesticide_df,.(siteID,id,area),
                             .fun = function(x){
                               
                               CPE_2013 = mean(x$all_pesticide_exposure[which(x$year %in% c(2013:2015))],na.omit=TRUE)
                               CPE_2020 = mean(x$all_pesticide_exposure[which(x$year %in% c(2018:2020))],na.omit=TRUE)
                               CPE_mean = mean(x$all_pesticide_exposure[which(x$year %in% c(2013:2019))],na.omit=TRUE)
                               
                               return(data.frame(CPE_2013,CPE_2020,CPE_mean))
                               
                             },.progress = "text")


value_site_pesticide <- ddply(value_site_pesticide,.(siteID),
                              .fun = function(x){
                                
                                CPE2013 = weighted.mean(x$CPE_2013,x$area)
                                CPE2020 = weighted.mean(x$CPE_2020,x$area)
                                CPEmean = weighted.mean(x$CPE_mean,x$area)
                                
                                return(data.frame(CPE2013,CPE2020,CPEmean))
                                
                              },.progress = "text")

value_site_mainland <- merge(value_site_mainland, value_site_pesticide, by="siteID", all.x=TRUE)

### add agi intensity

#site_mainland_sf_reproj_fr <- site_mainland_sf_reproj[which(site_mainland_sf_reproj$scheme_code=="FR"),]
#abs_intensity_fr <- raster("raw_data/nest/Absolute_intensity_5_clas_Fig3A.tif")
#site_mainland_sf_reproj_fr <- st_transform(site_mainland_sf_reproj_fr,st_crs(abs_intensity_fr))
#site_mainland_buffer_fr <- st_buffer(site_mainland_sf_reproj_fr, dist = 2500)
#area_site_mainland_fr <- exact_extract(abs_intensity_fr,site_mainland_buffer_fr, fun=c("frac"))
#area_site_mainland_fr_mean <- apply(area_site_mainland_fr,1,function(x){x %*% c(2500,7500,12500,17500,22500)})
#area_site_mainland_fr_cat <- cut(area_site_mainland_fr_mean,
#                                 breaks=c(-1, 5000, 10000, 15000, 20000, 25000),
#                                 labels=c('<= 5000', '> 5000 - 10000', '> 10000 - 15000', '> 15000 - 20000', '> 20000'))

site_mainland_sf_reproj_fr <- site_mainland_sf_reproj[which(site_mainland_sf_reproj$scheme_code=="FR"),]
abs_intensity_fr <- raster("raw_data/nest/Crop_management_systems_dom50_def.tif")
site_mainland_sf_reproj_fr <- st_transform(site_mainland_sf_reproj_fr,st_crs(abs_intensity_fr))

site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 1128.379)
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 1414.214) 
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 2500)
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_fr, dist = 5000)
area_site_agi <- exact_extract(abs_intensity_fr,site_mainland_buffer, fun=c("frac"))

area_site_agi$low <- area_site_agi$frac_3 + area_site_agi$frac_5 + area_site_agi$frac_16 +
  area_site_agi$frac_18 + area_site_agi$frac_20 + area_site_agi$frac_21 + area_site_agi$frac_22 +
  area_site_agi$frac_23 + area_site_agi$frac_24 + area_site_agi$frac_25 + area_site_agi$frac_29 +
  area_site_agi$frac_31 + area_site_agi$frac_34
area_site_agi$medium <- area_site_agi$frac_4 + area_site_agi$frac_7 + area_site_agi$frac_11 +
  area_site_agi$frac_12 + area_site_agi$frac_13 + area_site_agi$frac_15 + area_site_agi$frac_17 +
  area_site_agi$frac_27 + area_site_agi$frac_30 + area_site_agi$frac_35
area_site_agi$high <- area_site_agi$frac_2 + area_site_agi$frac_6 + area_site_agi$frac_8 +
  area_site_agi$frac_9 +  area_site_agi$frac_14 + 
  area_site_agi$frac_26 + area_site_agi$frac_28 + area_site_agi$frac_32 + area_site_agi$frac_33

site_mainland_sf_reproj_fr <- site_mainland_sf_reproj[which(site_mainland_sf_reproj$scheme_code=="FR"),]

value_site_intensity <- site_mainland_sf_reproj_fr
st_geometry(value_site_intensity) <- NULL
value_site_intensity$low <- area_site_agi$low
value_site_intensity$medium <- area_site_agi$medium
value_site_intensity$high <- area_site_agi$high

value_site_mainland <- merge(value_site_mainland, value_site_intensity, by="siteID", all.x=TRUE)


#saveRDS(value_site_mainland, "output/value_site_mainland_buffersmall.rds")
#saveRDS(value_site_mainland, "output/value_site_mainland_buffersmall2.rds")
#saveRDS(value_site_mainland, "output/value_site_mainland_buffermedium.rds")
#saveRDS(value_site_mainland, "output/value_site_mainland_bufferhigh.rds")

value_site_mainland_fr <- readRDS("output/value_site_mainland_buffersmall2.rds")

#subsite_data_mainland_trend <- readRDS("output/subsite_data_mainland_trend.rds")
#subsite_data_mainland_trend_fr <- subsite_data_mainland_trend[which(subsite_data_mainland_trend$scheme_code=="FR"),]
#saveRDS(subsite_data_mainland_trend_fr,"output/subsite_data_mainland_trend_fr.rds")
subsite_data_mainland_trend_fr <- readRDS("output/subsite_data_mainland_trend_fr.rds")


## get value per year per pressure

press_mainland_trend_fr <- ddply(distinct(subsite_data_mainland_trend_fr,siteID,year,.keep_all=TRUE), .(siteID,year),
                              .fun = function(x,pressure_data){
                                
                                pressure_subdata <- pressure_data[which(pressure_data$siteID == x$siteID),]
                                
                                impervious_2018 <- pressure_subdata$impervious2018
                                treedensity_2018 <- pressure_subdata$treedensity2018
                                agri_2018 <- pressure_subdata$agri2018
                                tempspring_2020 <- pressure_subdata$tempspring2020
                                tempspringvar_2020 <- pressure_subdata$tempspringvar2020
                                precspring_2020 <- pressure_subdata$precspring2020
                                shannon_2018 <- pressure_subdata$shannon2018
                                CPE_2020 <- pressure_subdata$CPE2020

                                d_impervious <- (pressure_subdata$impervious2018-pressure_subdata$impervious2006)/13
                                d_treedensity <- (pressure_subdata$treedensity2018-pressure_subdata$treedensity2012)/7
                                d_agri <- (pressure_subdata$agri2018-pressure_subdata$agri2000)/19
                                d_tempsrping <- (pressure_subdata$tempspring2020-pressure_subdata$tempspring2000)/21
                                tempsrping <- pressure_subdata$tempspring2000
                                d_tempsrpingvar <- (pressure_subdata$tempspringvar2020-pressure_subdata$tempspringvar2000)/21
                                d_precspring <- (pressure_subdata$precspring2020-pressure_subdata$precspring2000)/21
                                precspring <- pressure_subdata$precspring2000
                                d_shannon <- (pressure_subdata$shannon2018-pressure_subdata$shannon2000)/19
                                shannon <- pressure_subdata$shannon2000
                                d_CPE <- (pressure_subdata$CPE2020-pressure_subdata$CPE2013)/8
                                CPE <- pressure_subdata$CPEmean
                                
                                milieu <- pressure_subdata$eulandsystem_max
                                
                                drymatter <- sum(pressure_subdata$drymatter2000,pressure_subdata$drymatter2018, na.rm = TRUE)/2
                                
                                protectedarea_perc <- pressure_subdata$protectedarea_perc
                                protectedarea_type <- pressure_subdata$protectedarea_type
                                
                                eulandsystem_cat <- pressure_subdata$eulandsystem_cat
                                eulandsystem_farmland_low <- pressure_subdata$eulandsystem_farmland_low
                                eulandsystem_farmland_medium <- pressure_subdata$eulandsystem_farmland_medium
                                eulandsystem_farmland_high <- pressure_subdata$eulandsystem_farmland_high
                                eulandsystem_forest_lowmedium <- pressure_subdata$eulandsystem_forest_lowmedium
                                eulandsystem_forest_high <- pressure_subdata$eulandsystem_forest_high
                                
                                agi_low <- pressure_subdata$low
                                agi_medium <- pressure_subdata$medium
                                agi_high <- pressure_subdata$high
                                
                                PLS <- pressure_subdata$PLS
                                
                                trend_result <- data.frame(impervious_2018,treedensity_2018,agri_2018,tempspring_2020,tempspringvar_2020,precspring_2020,shannon_2018,CPE_2020,
                                                           d_impervious,d_treedensity,d_agri,d_tempsrping,tempsrping,d_tempsrpingvar,d_precspring,precspring,d_CPE,CPE,
                                                           d_shannon,shannon,milieu,drymatter,protectedarea_perc,protectedarea_type,
                                                           eulandsystem_farmland_low,eulandsystem_farmland_medium,eulandsystem_farmland_high,
                                                           eulandsystem_forest_lowmedium,eulandsystem_forest_high,agi_low,agi_medium,agi_high,PLS)
                                return(trend_result)
                              },pressure_data = value_site_mainland_fr,
                              .progress = "text")

press_mainland_trend_fr$milieu_cat <- NA
press_mainland_trend_fr$milieu_cat[which(press_mainland_trend_fr$milieu %in% c(21,22,23))] <- "urban"
press_mainland_trend_fr$milieu_cat[which(press_mainland_trend_fr$milieu %in% c(41,42,43,71,72,74,75))] <- "forest and shrub"
press_mainland_trend_fr$milieu_cat[which(press_mainland_trend_fr$milieu %in% c(31,32,51,52,53,61,62,63,731,732,733))] <- "openland"
press_mainland_trend_fr$milieu_cat[which(press_mainland_trend_fr$milieu %in% c(0,11,12,13,80,90))] <- "others"




###


press_mainland_trend_scale_fr <- press_mainland_trend_fr
press_mainland_trend_scale_fr[,c("d_impervious","d_treedensity","d_agri",
                              "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                              "d_shannon","shannon","drymatter","protectedarea_perc",
                              "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",   
                              "eulandsystem_forest_lowmedium","eulandsystem_forest_high","CPE","d_CPE","agi_high","agi_medium","agi_low")] <- scale(press_mainland_trend_scale_fr[,c("d_impervious","d_treedensity","d_agri",
                                                                                                                                                                                     "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                                                                                                                                                                     "d_shannon","shannon","drymatter","protectedarea_perc",
                                                                                                                                                                                     "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",   
                                                                                                                                                                                     "eulandsystem_forest_lowmedium","eulandsystem_forest_high","CPE","d_CPE","agi_high","agi_medium","agi_low")])


#saveRDS(press_mainland_trend_fr,"output/press_mainland_trend_fr_buffersmall.rds") 
#saveRDS(press_mainland_trend_scale_fr,"output/press_mainland_trend_scale_fr_buffersmall.rds")
#saveRDS(press_mainland_trend_fr,"output/press_mainland_trend_fr_buffersmall2.rds") 
#saveRDS(press_mainland_trend_scale_fr,"output/press_mainland_trend_scale_fr_buffersmall2.rds")
#saveRDS(press_mainland_trend_fr,"output/press_mainland_trend_fr_buffermedium.rds") 
#saveRDS(press_mainland_trend_scale_fr,"output/press_mainland_trend_scale_fr_buffermedium.rds")
#saveRDS(press_mainland_trend_fr,"output/press_mainland_trend_fr_bufferhigh.rds") 
#saveRDS(press_mainland_trend_scale_fr,"output/press_mainland_trend_scale_fr_bufferhigh.rds")

ggplot(press_mainland_trend_fr) +
  geom_point(aes(x=agi_high,y=CPE))

ggplot(press_mainland_trend_fr) +
  geom_point(aes(x=agi_high,y=d_CPE))


###

#bird_data_mainland <- readRDS("output/bird_data_mainland.rds")
#subsite_data_mainland_trend_fr <- readRDS("output/subsite_data_mainland_trend_fr.rds")
#bird_data_fr <- bird_data_mainland[which(bird_data_mainland$siteID %in% unique(subsite_data_mainland_trend_fr$siteID)),]
#saveRDS(bird_data_fr,"output/bird_data_fr.rds")

bird_data_fr <- readRDS("output/bird_data_fr.rds")
grid_fr_outline <- st_read("output/grid_fr_outline.gpkg")
#press_trend_scale <- readRDS("output/press_mainland_trend_scale_fr_buffersmall.rds")
#press_trend <- readRDS("output/press_mainland_trend_fr_buffersmall.rds")
press_trend_scale <- readRDS("output/press_mainland_trend_scale_fr_buffersmall2.rds")
press_trend <- readRDS("output/press_mainland_trend_fr_buffersmall2.rds")
#press_trend_scale <- readRDS("output/press_mainland_trend_scale_fr_buffermedium.rds")
#press_trend <- readRDS("output/press_mainland_trend_fr_buffermedium.rds")
#press_trend_scale <- readRDS("output/press_mainland_trend_scale_fr_bufferhigh.rds")
#press_trend <- readRDS("output/press_mainland_trend_fr_bufferhigh.rds")
subsite_data_trend_fr <- readRDS("output/subsite_data_mainland_trend_fr.rds")
site_mainland_sf_reproj <- readRDS("output/site_mainland_sf_reproj.rds")

test_multicor <- press_trend[which(press_trend$year==2010),c("d_impervious","d_treedensity","d_agri",
                                                             "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                                             "d_shannon","shannon","drymatter","protectedarea_perc","protectedarea_type",
                                                             "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                                             "CPE","d_CPE","agi_high","agi_medium","agi_low",
                                                             "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat")]
test_multicor$milieu_cat <- as.numeric(as.factor(test_multicor$milieu_cat))
test_multicor <- round(cor(na.omit(test_multicor)),2)
get_upper_tri <- function(test_multicor){
  test_multicor[lower.tri(test_multicor)]<- NA
  return(test_multicor)
}
test_multicor <- get_upper_tri(test_multicor)
test_multicor <- reshape2::melt(test_multicor, na.rm = TRUE)
ggplot(data = test_multicor, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.title = element_blank())+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  coord_fixed()



source("functions.R")

col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
               "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
               "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
               "year:agi_low","year:agi_high",
               "year:d_CPE",
               "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")

if_fail <- data.frame(t(rep(NA,(length(col_names)+28))))
names(if_fail) <- c(col_names,"dev_exp","n_obs","PLS","trend_past","sd_past",
                           "trend_tend","sd_tend","trend_s1","sd_s1",
                           "trend_s2","sd_s2","trend_s3","sd_s3",
                           "trend_s4","sd_s4",
                           "trend_past_signif","sd_past_signif","trend_tend_signif","sd_tend_signif","trend_s1_signif","sd_s1_signif",
                           "trend_s2_signif","sd_s2_signif","trend_s3_signif","sd_s3_signif",
                           "trend_s4_signif","sd_s4_signif","pressure_removed")

col_names_simple <- c("(Intercept)","year","protectedarea_perc","milieu_catopenland","milieu_catothers","milieu_caturban",
                      "tempsrping","precspring","shannon","drymatter",
                      "year:d_impervious","year:d_treedensity",
                      "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
                      "year:agi_low","year:agi_high",
                      "year:d_CPE",
                      "year:d_tempsrping","year:d_shannon")

if_fail_simple <- data.frame(t(rep(NA,(length(col_names_simple)+28))))
names(if_fail_simple) <- c(col_names_simple,"dev_exp","n_obs","PLS","trend_past","sd_past",
                    "trend_tend","sd_tend","trend_s1","sd_s1",
                    "trend_s2","sd_s2","trend_s3","sd_s3",
                    "trend_s4","sd_s4",
                    "trend_past_signif","sd_past_signif","trend_tend_signif","sd_tend_signif","trend_s1_signif","sd_s1_signif",
                    "trend_s2_signif","sd_s2_signif","trend_s3_signif","sd_s3_signif",
                    "trend_s4_signif","sd_s4_signif","pressure_removed")

site_mainland_sf_reproj_fr <- site_mainland_sf_reproj[which(site_mainland_sf_reproj$scheme_code=="FR"),]

press_trend$d_CPE <- press_trend$CPE #CPE2
press_trend_scale$d_CPE <- press_trend_scale$CPE

res_gam_bird_FR <- ddply(subsite_data_trend_fr,
                      .(sci_name_out),.fun=purrr::possibly(otherwise=if_fail,
                                                                .f=gam_species_FR),
                      pressure_data=press_trend_scale,
                      pressure_data_unscale=press_trend,
                      pressure_change=pressure_change,
                      site_data=site_mainland_sf_reproj_fr,
                      .progress = "text")

res_gam_bird_FR_simple <- ddply(subsite_data_trend_fr,
                         .(sci_name_out),.fun=purrr::possibly(otherwise=if_fail_simple,
                                                              .f=gam_species_FR_simple),
                         pressure_data=press_trend_scale,
                         pressure_data_unscale=press_trend,
                         pressure_change=pressure_change,
                         site_data=site_mainland_sf_reproj_fr,
                         .progress = "text")

#saveRDS(res_gam_bird_FR,"output/res_gam_bird_fr_final.rds")
#saveRDS(res_gam_bird_FR_nosignif,"output/res_gam_bird_fr_nosignif_final.rds")

saveRDS(res_gam_bird_FR_simple,"output/res_gam_bird_fr_simple.rds")
saveRDS(res_gam_bird_FR_simple,"output/res_gam_bird_fr_simple_nosignif.rds")

#saveRDS(res_gam_bird_FR,"output/res_gam_bird_fr_agi_CPE.rds")
#saveRDS(res_gam_bird_FR,"output/res_gam_bird_fr_agi_CPE_nosignif.rds")
#res_gam_bird_FR <- readRDS("output/res_gam_bird_fr_agi_CPE2.rds")
#res_gam_bird_FR_nosignif <- readRDS("output/res_gam_bird_fr_agi_CPE_nosignif2.rds")

res_gam_bird_FR <- readRDS("output/res_gam_bird_fr_simple.rds") # readRDS("output/res_gam_bird_fr_agi_CPE2small2.rds")
res_gam_bird_FR_nosignif <- readRDS("output/res_gam_bird_fr_simple_nosignif.rds") # readRDS("output/res_gam_bird_fr_agi_CPE_nosignif2small2.rds")

#res_gam_bird_FR <- readRDS("output/res_gam_bird_fr_agi_CPE2medium.rds")
#res_gam_bird_FR_nosignif <- readRDS("output/res_gam_bird_fr_agi_CPE_nosignif2medium.rds")

#res_gam_bird_FR <- readRDS("output/res_gam_bird_fr_agi_CPE2high.rds")
#res_gam_bird_FR_nosignif <- readRDS("output/res_gam_bird_fr_agi_CPE_nosignif2high.rds")

### select good model fit and compare with PECBMS trends

pecbms_trend_class <- read.csv("output/pecbms_trend_class.csv", header=TRUE)
pecbms_trend_class <- read.csv("output/pecbms_test2.csv", header=TRUE)
pecbms_trend_class <- read.csv("output/pecbms_test_fr.csv", header=TRUE)
pecbms_trend_class$PECBMS_slope_long <- as.numeric(substr(pecbms_trend_class$PECBMS_slope_long,1,6))
pecbms_trend_class$PECBMS_slope_short <- as.numeric(substr(pecbms_trend_class$PECBMS_slope_short,1,6))
pecbms_trend_class$PECBMS_slope_long[which(pecbms_trend_class$sci_name_out=="Sturnus vulgaris")] <- 0.98
pecbms_trend_class$PECBMS_slope_short[which(pecbms_trend_class$sci_name_out=="Chloris chloris")] <- 0.98

res_gam_bird_FR2 <- merge(res_gam_bird_FR[which(res_gam_bird_FR$dev_exp > 0.1 & res_gam_bird_FR$pressure_removed=="none"),],pecbms_trend_class,by.x="sci_name_out", by.y="sci_name_out_new")
res_gam_bird_FR2 <- merge(res_gam_bird_FR_correct_trend[which(res_gam_bird_FR_correct_trend$pressure_removed=="none"),],pecbms_trend_class,by.x="sci_name_out", by.y="sci_name_out_new")

res_gam_bird_FR2$PECBMS_slope_mid <- (res_gam_bird_FR2$PECBMS_slope_long + res_gam_bird_FR2$PECBMS_slope_short)/2

plot(exp(year)~PECBMS_slope_long,res_gam_bird_FR2)


data_plot <- res_gam_bird_FR2
data_plot <- reshape2::melt(data_plot[,c("sci_name_out","trend_past","sd_past","trend_past_signif","sd_past_signif","PECBMS_slope_long","PECBMS_slope_short","STOC")], id.var=c("sci_name_out","trend_past","sd_past","trend_past_signif","sd_past_signif"))

ggplot(data_plot, aes(y=exp(trend_past))) + 
  geom_vline(xintercept = 1, linetype = 2) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_point(aes(x=value, col=variable), alpha = 0.5) +
  geom_smooth(aes(x=value, col=variable),method = "lm", se = FALSE) +
  xlab("Slope from PECBMS") + ylab("Observed slope (2000-2021)") +
  #geom_abline(intercept = 0, slope = 1) +
  #xlim(c(0.86,1.07)) + ylim(c(0.65,1.2)) +
  scale_color_discrete(labels= c("PECBMS_slope_long" = "Long-term slope (1980-2023)", "PECBMS_slope_short" = "Ten-year slope (2014-2023)")) +
  theme_minimal() + theme(legend.title = element_blank(), legend.position = c(0.3, 0.8))

ggsave("output/bird_trend_FR_pecbms.png",
       width = 6,
       height = 6,
       dpi = 300)

lm_eqn <- function(df){
  y <- exp(df$trend_past)
  x <- df$value
  m <- lm(y ~ x);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot(data_plot[which(data_plot$variable=="STOC"),], aes(y=exp(trend_past))) + 
  geom_vline(xintercept = 1, linetype = 2) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_point(aes(x=value, col=variable), alpha = 0.5) +
  #geom_errorbar(aes(x=value, ymin = exp(trend_past) - 1.96*exp(sd_past), ymax = exp(trend_past) + 1.96*exp(sd_past),col=variable), alpha = 0.5) +
  geom_smooth(aes(x=value, col=variable),method = "lm", se = FALSE) +
  xlab("Slope from STOC 2001-2019") + ylab("Observed slope") +
  #geom_abline(intercept = 0, slope = 1) +
  #xlim(c(0.86,1.07)) + ylim(c(0.65,1.2)) +
  geom_label_repel(aes(x = value, label = sci_name_out)) +
  geom_text(x = 0.98, y = 1.3, label = lm_eqn(data_plot[which(data_plot$variable=="STOC"),]), parse = TRUE) +
  scale_color_discrete(labels= c("PECBMS_slope_long" = "Long-term slope (1980-2023)", "PECBMS_slope_short" = "Ten-year slope (2014-2023)")) +
  theme_minimal() + theme(legend.position = "none")

ggsave("output/bird_trend_FR_stoc.png",
       width = 15,
       height = 15,
       dpi = 300)

STOC_species <- pecbms_trend_class$sci_name_out_new[which(!is.na(pecbms_trend_class$STOC))]

res_gam_bird_FR_correct <- res_gam_bird_FR[which(res_gam_bird_FR$dev_exp > 0.1 & res_gam_bird_FR$sci_name_out %in% STOC_species),]
res_gam_bird_FR_correct_nosignif <- res_gam_bird_FR_nosignif[which(res_gam_bird_FR_nosignif$dev_exp > 0.1 & res_gam_bird_FR_nosignif$sci_name_out %in% STOC_species),]
#res_gam_bird_FR_correct <- res_gam_bird_FR[which(res_gam_bird_FR$dev_exp > 0.2),]


### check with Benoit a priori expectation

expected_effect <- read.csv2("raw_data/pressions_oiseaux.csv")
#expected_effect <- read.csv("raw_data/pressions_oiseaux_gl.csv",sep = "\t")
expected_effect2 <- read.csv("raw_data/pressions_oiseaux_gl.csv",sep = "\t")

expected_effect[expected_effect == "--"] <- "-"
expected_effect[expected_effect == "++"] <- "+"
expected_effect2[expected_effect2 == "--"] <- "-"
expected_effect2[expected_effect2 == "++"] <- "+"

expected_effect_all <- expected_effect
expected_effect_all[,c(2:7)] <- 0
expected_effect_all$Augmentation.des.températures[which(expected_effect$Augmentation.des.températures == "-" & expected_effect2$Augmentation.des.températures == "-" )] <- "-"
expected_effect_all$Augmentation.des.températures[which(expected_effect$Augmentation.des.températures == "+" & expected_effect2$Augmentation.des.températures == "+" )] <- "+"
expected_effect_all$Augmentation.de.l.artificialisation[which(expected_effect$Augmentation.de.l.artificialisation == "-" & expected_effect2$Augmentation.de.l.artificialisation == "-" )] <- "-"
expected_effect_all$Augmentation.de.l.artificialisation[which(expected_effect$Augmentation.de.l.artificialisation == "+" & expected_effect2$Augmentation.de.l.artificialisation == "+" )] <- "+"
expected_effect_all$Augmentation.du.couvert.forestier[which(expected_effect$Augmentation.du.couvert.forestier == "-" & expected_effect2$Augmentation.du.couvert.forestier == "-" )] <- "-"
expected_effect_all$Augmentation.du.couvert.forestier[which(expected_effect$Augmentation.du.couvert.forestier == "+" & expected_effect2$Augmentation.du.couvert.forestier == "+" )] <- "+"
expected_effect_all$Intensification.de.l.agriculture[which(expected_effect$Intensification.de.l.agriculture == "-" & expected_effect2$Intensification.de.l.agriculture == "-" )] <- "-"
expected_effect_all$Intensification.de.l.agriculture[which(expected_effect$Intensification.de.l.agriculture == "+" & expected_effect2$Intensification.de.l.agriculture == "+" )] <- "+"
expected_effect_all$Diversité.des.paysages[which(expected_effect$Diversité.des.paysages == "-" & expected_effect2$Diversité.des.paysages == "-" )] <- "-"
expected_effect_all$Diversité.des.paysages[which(expected_effect$Diversité.des.paysages == "+" & expected_effect2$Diversité.des.paysages == "+" )] <- "+"
expected_effect_all$Aires.protégées[which(expected_effect$Aires.protégées == "-" & expected_effect2$Aires.protégées == "-" )] <- "-"
expected_effect_all$Aires.protégées[which(expected_effect$Aires.protégées == "+" & expected_effect2$Aires.protégées == "+" )] <- "+"


obs_vs_expected <- res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$pressure_removed=="none"),]
obs_vs_expected[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                    "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","protectedarea_perc","year:d_shannon",
                    "year:d_impervious","year:d_tempsrping")][obs_vs_expected[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                                                                                                                             "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","protectedarea_perc","year:d_shannon",
                                                                                                                             "year:d_impervious","year:d_tempsrping")] > value_max] <- value_max
obs_vs_expected[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                    "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","protectedarea_perc","year:d_shannon",
                    "year:d_impervious","year:d_tempsrping")][obs_vs_expected[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                                                                                                                             "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","protectedarea_perc","year:d_shannon",
                                                                                                                             "year:d_impervious","year:d_tempsrping")] < -value_max] <- -value_max


obs_vs_expected <- merge(obs_vs_expected,expected_effect_all, by.x = "sci_name_out", by.y="Species", all.y=TRUE)

obs_vs_expected$Augmentation.des.températures <- factor(obs_vs_expected$Augmentation.des.températures, levels = c("--","-","0","+","++"))
obs_vs_expected$Augmentation.de.l.artificialisation <- factor(obs_vs_expected$Augmentation.de.l.artificialisation, levels = c("--","-","0","+","++"))
obs_vs_expected$Augmentation.du.couvert.forestier <- factor(obs_vs_expected$Augmentation.du.couvert.forestier, levels = c("--","-","0","+","++"))
obs_vs_expected$Intensification.de.l.agriculture <- factor(obs_vs_expected$Intensification.de.l.agriculture, levels = c("--","-","0","+","++"))
obs_vs_expected$Diversité.des.paysages <- factor(obs_vs_expected$Diversité.des.paysages, levels = c("--","-","0","+","++"))
obs_vs_expected$Aires.protégées <- factor(obs_vs_expected$Aires.protégées, levels = c("--","-","0","+","++"))

ggplot(obs_vs_expected, aes(x=Augmentation.des.températures, y=tempsrping)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of temperature increase", y = "Observed effect of temperature on abundance") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.des.températures, y=`year:d_tempsrping`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of temperature increase", y = "Observed effect of temperature increase on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()

ggsave("output/exp_obs_temp.png",
       width = 10,
       height = 6,
       dpi = 300)


ggplot(obs_vs_expected, aes(x=Augmentation.de.l.artificialisation, y=milieu_caturban)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of urbanisation", y = "Observed effect of urban areas on abundance") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.de.l.artificialisation, y=`year:d_impervious`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of urbanisation", y = "Observed effect of urbanisation on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()

ggsave("output/exp_obs_urban.png",
       width = 10,
       height = 6,
       dpi = 300)

ggplot(obs_vs_expected, aes(x=Augmentation.du.couvert.forestier, y=milieu_catopenland)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of forest cover increase", y = "Observed effect of openland on abundance") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.du.couvert.forestier, y=`year:d_treedensity`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of forest cover increase", y = "Observed effect of tree density increase on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.du.couvert.forestier, y=`year:eulandsystem_forest_lowmedium`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of forest cover increase", y = "Observed effect of low/medium managed forest cover on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.du.couvert.forestier, y=`year:eulandsystem_forest_high`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of forest cover increase", y = "Observed effect of high managed forest cover on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()

ggsave("output/exp_obs_forest.png",
       width = 10,
       height = 6,
       dpi = 300)

ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=milieu_catopenland)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of agricultural intensification", y = "Observed effect of openland on abundance") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=milieu_catothers)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of agricultural intensification", y = "Observed effect of other natural land on abundance") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:d_agri`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of agricultural intensification", y = "Observed effect of agricultural cover increase on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:CPE_mean`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  geom_point(color="black", size=1, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:d_CPE`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of agricultural intensification", y = "Observed effect of pesticide increase on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:eulandsystem_farmland_low`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  geom_point(color="black", size=1, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:eulandsystem_farmland_medium`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  geom_point(color="black", size=1, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:agi_high`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of agricultural intensification", y = "Observed effect of high input cover on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:agi_low`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of agricultural intensification", y = "Observed effect of low input cover on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()

ggsave("output/exp_obs_agri.png",
       width = 10,
       height = 6,
       dpi = 300)

ggplot(obs_vs_expected, aes(x=Diversité.des.paysages, y=shannon)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of landscape diversity", y = "Observed effect of landscape diversity on abundance") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Diversité.des.paysages, y=`year:d_shannon`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of landscape diversity", y = "Observed effect of landscape diversity increase on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()

ggsave("output/exp_obs_shannon.png",
       width = 10,
       height = 6,
       dpi = 300)

ggplot(obs_vs_expected, aes(x=Aires.protégées, y=`protectedarea_perc`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,,outliers =FALSE) +
  labs(x = "Expected effect of protected areas", y = "Observed effect of protected areas on trend") +
  geom_label_repel(aes(label = sci_name_out)) +
  geom_point(color="black", size=2, alpha=0.9) + theme_minimal()

ggsave("output/exp_obs_PA.png",
       width = 10,
       height = 6,
       dpi = 300)


# pressure change

pressure_change <- data.frame(variable = c("Low intensity farmland","Medium intensity farmland","High intensity farmland",
                                           "Forest cover","Urban cover",
                                           "Agricultural cover","Temperature","Wood production","Hedge","NODU"),
                              initial = c(0.05,0.02,0.93,16914,5263,28.77855,14.1,51.8,500,14.6),
                              tend = c(0.20,0.10,0.70,18606,7369,25.1017,15,60.7,547,9.7),
                              s1 = c(0.70,0.30,0,19769,5061,25.63628,15,51.9,935,1.7),
                              s2 = c(0.50,0.50,0,17515,5567,27.71214,15,60.7,939,3),
                              s3 = c(0.20,0.50,0.30,17564,6073,27.16419,15,70.7,619,5.7),
                              s4 = c(0.10,0.20,0.70,17264,7085,26.69525,15,71.1,549,9.7))

pressure_change_b <- data.frame(variable = c("Low intensity farmland","Medium intensity farmland","High intensity farmland",
                                           "Forest cover","Urban cover",
                                           "Agricultural cover","Temperature","Wood production","Hedge","NODU"),
                              initial = c(0.0666,0.0333,0.9001,17489,5137,28.651,14.1,51.33,985.972,73.32),
                              tend = c(0.1915,0.0958,0.7127,18364,5858,27.657,15,80.80,461.062,57.02),
                              s1 = c(0.6834,0.2929,0.0237,20442,5437,25.100,15,80.80,1632.107,10.14),
                              s2 = c(0.4893,0.4893,0.0214,18111,5554,27.486,15,80.80,1821.986,17.91),
                              s3 = c(0.1952,0.4880,0.3168,18162,5856,27.043,15,80.80,1123.128,33.07),
                              s4 = c(0.0978,0.1955,0.7067,17852,6011,27.498,15,80.80,798.128,56.18))

pressure_change_long <- pressure_change
pressure_change_long[1:3,2:7] <- pressure_change_long[1:3,2:7]*100
pressure_change_long[4:5,2:7] <- pressure_change_long[4:5,2:7]/55150000*1000*100
pressure_change_long[6,2:7] <- pressure_change_long[6,2:7]/55150000*1000000*100
pressure_change_long[10,2:7] <- pressure_change_long[10,2:7]/pressure_change_long[10,2]*100
pressure_change_long$tend <- pressure_change_long$tend - pressure_change_long$initial
pressure_change_long$s1 <- pressure_change_long$s1 - pressure_change_long$initial
pressure_change_long$s2 <- pressure_change_long$s2 - pressure_change_long$initial
pressure_change_long$s3 <- pressure_change_long$s3 - pressure_change_long$initial
pressure_change_long$s4 <- pressure_change_long$s4 - pressure_change_long$initial
pressure_change_long <- reshape::melt(pressure_change_long, id.vars = "variable")
names(pressure_change_long)[2] <- "scenario"



pressure_change_long$scenario <- factor(pressure_change_long$scenario, levels = c("initial","tend","s1","s2","s3","s4"), labels = c("Current", "BAU", "S1", "S2", "S3", "S4"))
pressure_change_long$variable <- factor(pressure_change_long$variable, levels = c("Temperature","Urban cover","Forest cover","Wood production","Agricultural cover",
                                                                                  "Low intensity farmland","Medium intensity farmland","High intensity farmland",
                                                                                  "NODU","Hedge"),
                                        labels = c("Temperature","Urban cover","Forest cover","Wood production","Agricultural cover",
                                                   "Low input farmland","Integrated production farmland","Reasoned conventional farmland",
                                                   "Pesticides","Hedge"))

ggplot(droplevels(pressure_change_long[which(pressure_change_long$scenario != "Current"),]), aes(fill=scenario, y=value, x=scenario)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("Current" = "black", "BAU"="grey","S1"="#6293c9ff","S2"="#a8338fff","S3"="#85ba4cff","S4"="#ef7132ff")) + 
  facet_wrap(~variable, scales="free_y", nrow=2) +
  theme_minimal() +
  theme(legend.position="none") + xlab("") + ylab("")

ggsave("output/ademe_scenarios.png",
       width = 13,
       height = 7,
       dpi = 300)





# by species type:

farmland_species <- c("Alauda arvensis","Alectoris rufa","Anthus campestris","Corvus frugilegus",
                      "Emberiza calandra","Emberiza cirlus","Galerida cristata","Lanius collurio",
                      "Motacilla flava","Perdix perdix","Upupa epops","Buteo buteo","Coturnix coturnix",
                      "Sylvia communis","Falco tinnunculus","Linaria cannabina","Lullula arborea",
                      "Oenanthe oenanthe","Saxicola torquatus","Anthus pratensis","Emberiza citrinella",
                      "Emberiza hortulana","Saxicola rubetra", "Vanellus vanellus")
forest_species <- c("Certhia familiaris","Coccothraustes coccothraustes","Periparus ater","Phylloscopus bonelli",
                    "Phylloscopus sibilatrix","Phylloscopus trochilus","Pyrrhula pyrrhula","Dendrocopos major",
                    "Leiopicus medius","Poecile montanus","Poecile palustris","Sitta europaea",
                    "Certhia brachydactyla","Sylvia melanocephala","Dryocopus martius","Erithacus rubecula",
                    "Lophophanes cristatus","Phylloscopus collybita","Regulus ignicapilla","Regulus regulus",
                    "Troglodytes troglodytes","Turdus philomelos","Turdus viscivorus","Picus canus")
urban_species <- c("Carduelis carduelis","Corvus monedula","Delichon urbicum","Hirundo rustica","Apus apus",
                   "Passer domesticus","Passer montanus","Pica pica","Phoenicurus ochruros","Phoenicurus phoenicurus",
                   "Serinus serinus","Streptopelia decaocto","Chloris chloris")
generalist_species <- c("Columba palumbus", "Cuculus canorus", "Picus viridis / Picus sharpei", "Sylvia atricapilla", "Hippolais polyglotta",
                        "Luscinia megarhynchos", "Turdus merula", "Prunella modularis","Oriolus oriolus","Parus major",
                        "Cyanistes caeruleus", "Corvus corone","Garrulus glandarius", "Fringilla coelebs")

smap_sp_mean <- read.csv("raw_data/smap_sp_mean.csv")
species_affected <- smap_sp_mean$Species[which(!is.na(smap_sp_mean$temp) | !is.na(smap_sp_mean$urb) | !is.na(smap_sp_mean$hico) | !is.na(smap_sp_mean$forest))]


pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
value_max <- max(abs(quantile(pressure_FR_bird_long$value[which(pressure_FR_bird_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                                                      "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                                      "year:d_agri","year:CPE_mean","year:CPE_trend","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                                      "year:eulandsystem_farmland_high"))],0.1, na.rm=TRUE)),abs(quantile(pressure_FR_bird_long$value[which(pressure_FR_bird_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                                                                                                                                                                                                  "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                                                                                                                                                                                  "year:d_agri","year:CPE_mean","year:CPE_trend","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                                                                                                                                                                                  "year:eulandsystem_farmland_high"))],0.9, na.rm=TRUE)))

species_high <- unique(pressure_FR_bird_long$sci_name_out[which((pressure_FR_bird_long$value > value_max | pressure_FR_bird_long$value < - value_max) & pressure_FR_bird_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                                                                                                                 "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                                                                                                 "year:d_agri","year:CPE_mean","year:CPE_trend","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                                                                                                 "year:eulandsystem_farmland_high"))])


pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% farmland_species & res_gam_bird_FR_correct$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% forest_species & res_gam_bird_FR_correct$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% urban_species & res_gam_bird_FR_correct$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% generalist_species & res_gam_bird_FR_correct$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% species_affected & res_gam_bird_FR_correct$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long <- pressure_FR_bird_long[which(!pressure_FR_bird_long$variable %in% c("(Intercept)","PLS","dev_exp","n_obs","pressure_removed")),]

pressure_FR_bird_long_nosignif <- reshape2::melt(res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long_nosignif <- reshape2::melt(res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$sci_name_out %in% farmland_species & res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long_nosignif <- reshape2::melt(res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$sci_name_out %in% forest_species & res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long_nosignif <- reshape2::melt(res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$sci_name_out %in% urban_species & res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long_nosignif <- reshape2::melt(res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$sci_name_out %in% generalist_species & res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long_nosignif <- reshape2::melt(res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$sci_name_out %in% species_affected & res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),], id.vars=c("sci_name_out","PLS","pressure_removed"))
pressure_FR_bird_long_nosignif <- pressure_FR_bird_long_nosignif[which(!pressure_FR_bird_long_nosignif$variable %in% c("(Intercept)","PLS","dev_exp","n_obs","pressure_removed")),]

pressure_FR_bird_long$value_nosignif <- pressure_FR_bird_long_nosignif$value
pressure_FR_bird_long$signif <- as.factor(ifelse(is.na(pressure_FR_bird_long$value), 0,1))
pressure_FR_bird_long$variable_nosignif <- pressure_FR_bird_long$variable 
pressure_FR_bird_long$variable_nosignif[which(pressure_FR_bird_long$signif==0)] <- NA

pressure_FR_bird_long_d <- pressure_FR_bird_long[which(pressure_FR_bird_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                                             "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                             "year:d_agri","year:d_CPE","year:agi_low","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                             "year:agi_high")),]

pressure_FR_bird_long_d$variable <- factor(pressure_FR_bird_long_d$variable , levels = c("year:protectedarea_perc","year:d_shannon","year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                                                                                         "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity",
                                                                                         "year:d_impervious","year:d_precspring","year:d_tempsrpingvar","year:d_tempsrping"
                                                                                         ))

ggplot(pressure_FR_bird_long_d, aes(x = value_nosignif)) +
  geom_histogram(aes(fill = variable_nosignif),col="lightgrey",
                 bins = 30) + 
  xlim(c(-0.21,0.21)) +
  scale_y_discrete(labels=c("year:d_impervious" = "Urbanisation (\u03B4Urb)","year:d_tempsrping" = "Temperature (\u03B4T)", "year:d_tempsrpingvar" = "Temperature variation (\u03B4Tva)", "year:d_precspring" = "Rainfall (\u03B4R)", "year:d_shannon" = "Landscape diversity (\u03B4H)",              
                            "year:protectedarea_perc" = "Protected area (P)", "year:d_treedensity" = "Tree density (\u03B4TD)","year:eulandsystem_forest_lowmedium" = "Low/medium intensive forests (Folw)", "year:eulandsystem_forest_high" = "High intensive forests on trend (Foh)",
                            "year:d_agri" = "Agricultural surface (\u03B4Fa)","year:d_CPE" = "Pesticide exposure (\u03B4CPE)","year:agi_low" = "Low intensive farmland (Fal)","year:eulandsystem_farmland_low" = "Low intensive farmland (Fal)",
                            "year:eulandsystem_farmland_medium" = "Medium intensive farmland (Fam)", "year:agi_high" = "High intensive farmland (Fah)"
  )) + 
  scale_fill_manual(values = c("year:d_impervious"="#33a02c","year:d_tempsrping"="#1f78b4","year:d_tempsrpingvar"="#1f78b4","year:d_precspring"="#1f78b4",
                               "year:d_shannon"="#33a02c","year:protectedarea_perc"="#b2df8a","year:d_treedensity"="#33a02c","year:eulandsystem_forest_lowmedium"="#b2df8a","year:eulandsystem_forest_high"="#b2df8a",
                               "year:d_agri"="#33a02c","year:d_CPE"="#b2df8a","year:agi_low"="#b2df8a","year:eulandsystem_farmland_low"="#b2df8a","year:eulandsystem_farmland_medium"="#b2df8a",
                               "year:agi_high"="#b2df8a"), na.value = "#f5f5f5ff") +
  geom_vline(aes(xintercept = 0), lty=2) +
  facet_grid(factor(variable, levels = c("year:d_tempsrping", "year:d_tempsrpingvar", "year:d_precspring", "year:d_impervious", "year:d_shannon",              
                                         "year:protectedarea_perc", "year:d_treedensity","year:eulandsystem_forest_lowmedium", "year:eulandsystem_forest_high",
                                         "year:d_agri",
                                         "year:agi_low", "year:agi_high","year:d_CPE"),
                    labels = c("Temperature","Temperature variation","Rainfall","Urbanisation","Landscape diversity",              
                               "Protected area","Tree density","Low/medium intensive forests","High intensive forests",
                               "Agricultural surface",
                               "Low intensive farmland","High intensive farmland","Pesticide exposure"))~., switch = "y") + theme_ridges() + 
  theme(legend.position = "none", 
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.background.y = element_rect(fill = NA),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        axis.title = element_blank())

ggsave("output/pressure_trend_bird_FR_hist.png",
       width = 6,
       height = 6,
       dpi = 300
)

pressure_FR_bird_long_d$signif2 <- NA
pressure_FR_bird_long_d$signif2[which(pressure_FR_bird_long_d$signif==0 & pressure_FR_bird_long_d$value_nosignif > 0)] <- "Positive, p-value > 0.05"
pressure_FR_bird_long_d$signif2[which(pressure_FR_bird_long_d$signif==0 & pressure_FR_bird_long_d$value_nosignif < 0)] <- "Negative, p-value > 0.05"
pressure_FR_bird_long_d$signif2[which(pressure_FR_bird_long_d$signif==1 & pressure_FR_bird_long_d$value_nosignif > 0)] <- "Positive, p-value < 0.05"
pressure_FR_bird_long_d$signif2[which(pressure_FR_bird_long_d$signif==1 & pressure_FR_bird_long_d$value_nosignif < 0)] <- "Negative, p-value < 0.05"
pressure_FR_bird_long_d$signif2 <- factor(pressure_FR_bird_long_d$signif2, levels = c("Negative, p-value > 0.05","Negative, p-value < 0.05","Positive, p-value < 0.05","Positive, p-value > 0.05"))

pressure_FR_bird_likert <- reshape2::dcast(pressure_FR_bird_long_d, sci_name_out ~ variable, value.var = "signif2")

for(i in 2:14){
  pressure_FR_bird_likert[,i] <- factor(pressure_FR_bird_likert[,i], levels = c("Negative, p-value > 0.05","Negative, p-value < 0.05","Positive, p-value < 0.05","Positive, p-value > 0.05"))
}

pressure_FR_bird_likert <- pressure_FR_bird_likert[,c("year:d_tempsrping", "year:d_tempsrpingvar", "year:d_precspring", "year:d_impervious", "year:d_treedensity","year:eulandsystem_forest_lowmedium", "year:eulandsystem_forest_high",
                                                      "year:d_agri","year:agi_low", "year:agi_high","year:d_CPE", "year:d_shannon","year:protectedarea_perc")]

for(i in 2:11){
  pressure_FR_bird_likert[,i] <- factor(pressure_FR_bird_likert[,i], levels = c("Negative, p-value > 0.05","Negative, p-value < 0.05","Positive, p-value < 0.05","Positive, p-value > 0.05"))
}

pressure_FR_bird_likert <- pressure_FR_bird_likert[,c("year:d_tempsrping", "year:d_impervious", "year:d_treedensity","year:eulandsystem_forest_lowmedium", "year:eulandsystem_forest_high",
                                                      "year:d_agri","year:agi_low", "year:agi_high","year:d_CPE", "year:d_shannon")]


gglikert(pressure_FR_bird_likert,add_totals = FALSE, labels_hide_below = 0.01) +
  scale_y_discrete(labels=c("year:d_impervious" = "Urbanisation (\u03B4Urb)","year:d_tempsrping" = "Temperature (\u03B4T)", "year:d_tempsrpingvar" = "Temperature variation (\u03B4Tva)", "year:d_precspring" = "Rainfall (\u03B4R)", "year:d_shannon" = "Landscape diversity (\u03B4H)",              
                            "year:protectedarea_perc" = "Protected area (P)", "year:d_treedensity" = "Tree density (\u03B4TD)","year:eulandsystem_forest_lowmedium" = "Low/medium intensive forests (Folw)", "year:eulandsystem_forest_high" = "High intensive forests on trend (Foh)",
                            "year:d_agri" = "Agricultural surface (\u03B4Fa)","year:d_CPE" = "Pesticide exposure (CPE)","year:agi_low" = "Low intensive farmland (Fal)","year:eulandsystem_farmland_low" = "Low intensive farmland (Fal)",
                            "year:eulandsystem_farmland_medium" = "Medium intensive farmland (Fam)", "year:agi_high" = "High intensive farmland (Fah)"
  )) + scale_fill_manual(values = c("Negative, p-value > 0.05" = "#fdae61ff","Negative, p-value < 0.05" = "#d7191cff" ,"Positive, p-value < 0.05" = "#2c7bb6ff", "Positive, p-value > 0.05" = "#abd9e9ff")) +
  theme(legend.position = "none", 
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.background.y = element_rect(fill = NA),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        axis.title = element_blank())

ggsave("output/pressure_trend_bird_FR_likert.png",
       width = 6,
       height = 6,
       dpi = 300
)



pressure_FR_bird <- res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$pressure_removed =="none"),]
pressure_FR_bird <- res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% farmland_species & res_gam_bird_FR_correct$pressure_removed =="none"),]
pressure_FR_bird <- res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% forest_species & res_gam_bird_FR_correct$pressure_removed =="none"),]
pressure_FR_bird <- res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% urban_species & res_gam_bird_FR_correct$pressure_removed =="none"),]

pressure_FR_bird <- res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),]
pressure_FR_bird <- res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$sci_name_out %in% farmland_species & res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),]
pressure_FR_bird <- res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$sci_name_out %in% forest_species & res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),]
pressure_FR_bird <- res_gam_bird_FR_correct_nosignif[which(res_gam_bird_FR_correct_nosignif$sci_name_out %in% urban_species & res_gam_bird_FR_correct_nosignif$pressure_removed =="none"),]

#pressure_FR_bird <- pressure_FR_bird[which(!(pressure_FR_bird$sci_name_out %in% species_high)),]
pressure_FR_bird[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                    "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","year:protectedarea_perc","year:d_shannon",
                    "year:d_impervious","year:d_precspring","year:d_tempsrpingvar","year:d_tempsrping")][pressure_FR_bird[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                    "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","year:protectedarea_perc","year:d_shannon",
                    "year:d_impervious","year:d_precspring","year:d_tempsrpingvar","year:d_tempsrping")] > value_max] <- value_max
pressure_FR_bird[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                    "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","year:protectedarea_perc","year:d_shannon",
                    "year:d_impervious","year:d_precspring","year:d_tempsrpingvar","year:d_tempsrping")][pressure_FR_bird[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                                                                                                                             "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","year:protectedarea_perc","year:d_shannon",
                                                                                                                             "year:d_impervious","year:d_precspring","year:d_tempsrpingvar","year:d_tempsrping")] < -value_max] <- -value_max

mean_pressure_FR_bird <- mean_pressure_FR(pressure_FR_bird[,c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                                                                  "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","year:protectedarea_perc","year:d_shannon",
                                                                  "year:d_impervious","year:d_precspring","year:d_tempsrpingvar","year:d_tempsrping")])
mean_pressure_FR_bird$variable <- factor(mean_pressure_FR_bird$variable , levels = c("year:d_CPE","year:agi_high","year:agi_low","year:d_agri",
                                                                                         "year:eulandsystem_forest_high","year:eulandsystem_forest_lowmedium","year:d_treedensity","year:protectedarea_perc","year:d_shannon",
                                                                                         "year:d_impervious","year:d_precspring","year:d_tempsrpingvar","year:d_tempsrping"))



ggplot(mean_pressure_FR_bird, aes(x = mean_value, y = variable, fill = variable)) +
  geom_errorbarh(aes(xmax = mean_value+1.96*se_value, xmin = mean_value-1.96*se_value), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_y_discrete(labels=c("year:d_impervious" = "Urbanisation (\u03B4Urb)","year:d_tempsrping" = "Temperature (\u03B4T)", "year:d_tempsrpingvar" = "Temperature variation (\u03B4Tva)", "year:d_precspring" = "Rainfall (\u03B4R)", "year:d_shannon" = "Landscape diversity (\u03B4H)",              
                            "year:protectedarea_perc" = "Protected area (P)", "year:d_treedensity" = "Tree density (\u03B4TD)","year:eulandsystem_forest_lowmedium" = "Low/medium intensive forests (Folw)", "year:eulandsystem_forest_high" = "High intensive forests (Foh)",
                            "year:d_agri" = "Agricultural surface (\u03B4Fa)","year:agi_low" = "Low intensive farmland (Fal)",
                            "year:agi_high" = "High intensive farmland (Fah)","year:d_CPE" = "Pesticide exposure (\u03B4CPE)")) +
  scale_color_manual(values = c("year:d_impervious"="#33a02c","year:d_tempsrping"="#1f78b4","year:d_tempsrpingvar"="#1f78b4","year:d_precspring"="#1f78b4",
                                "year:d_shannon"="#33a02c","year:protectedarea_perc"="#b2df8a","year:d_treedensity"="#33a02c","year:eulandsystem_forest_lowmedium"="#b2df8a","year:eulandsystem_forest_high"="#b2df8a",
                                "year:d_agri"="#33a02c","year:d_CPE"="#b2df8a","year:agi_low"="#b2df8a","year:agi_high"="#b2df8a")) +
  theme_ridges() + geom_vline(aes(xintercept = 1), lty=2) +
  theme(legend.position = "none", axis.title = element_blank())

ggsave("output/pressure_mean_bird_FR_hist_signif.png",
       width = 6,
       height = 6,
       dpi = 300
)


pressure_FR_bird_long_s <- pressure_FR_bird_long[which(pressure_FR_bird_long$variable %in% c("protectedarea_perc","milieu_catopenland","milieu_caturban","tempsrping",
                                                                                             "precspring","shannon","drymatter")),]

pressure_FR_bird_long_s$variable <- factor(pressure_FR_bird_long_s$variable , levels = c("protectedarea_perc","tempsrping","precspring","milieu_catopenland",
                                                                                         "milieu_caturban","shannon","drymatter"))

ggplot(pressure_FR_bird_long_s, aes(x = value, y = variable, fill = variable)) +
  scale_y_discrete(labels=c("tempsrping" = "Temperature on abundance","precspring"= "Precipitation on abundance","milieu_catopenland" = "Openland vs forest on abundance",
                            "milieu_caturban" = "Urban vs forest on abundance","shannon" = "Landscape diversity on abundance","drymatter" = "Productivity on abundance")) + 
  geom_density_ridges(stat = "binline", col=NA,scale = 0.9,
                      bins = 60, draw_baseline = FALSE) + xlim(c(-3,3))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.2, scale = 0.9,
                      quantiles = 2) +
  scale_fill_manual(values = c("tempsrping"="#1f78b4","precspring"="#1f78b4","milieu_catopenland"="#33a02c","milieu_catothers"="#33a02c",
                               "milieu_caturban"="#33a02c","shannon"="#33a02c","drymatter"="#33a02c")) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  xlab("Pressures") + ylab("Estimate") +
  theme(legend.position = "none")

pressure_FR_bird_long_s$signif2 <- NA
pressure_FR_bird_long_s$signif2[which(pressure_FR_bird_long_s$signif==0 & pressure_FR_bird_long_s$value_nosignif > 0)] <- "Positive, p-value > 0.05"
pressure_FR_bird_long_s$signif2[which(pressure_FR_bird_long_s$signif==0 & pressure_FR_bird_long_s$value_nosignif < 0)] <- "Negative, p-value > 0.05"
pressure_FR_bird_long_s$signif2[which(pressure_FR_bird_long_s$signif==1 & pressure_FR_bird_long_s$value_nosignif > 0)] <- "Positive, p-value < 0.05"
pressure_FR_bird_long_s$signif2[which(pressure_FR_bird_long_s$signif==1 & pressure_FR_bird_long_s$value_nosignif < 0)] <- "Negative, p-value < 0.05"
pressure_FR_bird_long_s$signif2 <- factor(pressure_FR_bird_long_s$signif2, levels = c("Negative, p-value > 0.05","Negative, p-value < 0.05","Positive, p-value < 0.05","Positive, p-value > 0.05"))

pressure_FR_bird_likert_s <- reshape2::dcast(pressure_FR_bird_long_s, sci_name_out ~ variable, value.var = "signif2")

for(i in 2:8){
  pressure_FR_bird_likert_s[,i] <- factor(pressure_FR_bird_likert_s[,i], levels = c("Negative, p-value > 0.05","Negative, p-value < 0.05","Positive, p-value < 0.05","Positive, p-value > 0.05"))
}

pressure_FR_bird_likert_s <- pressure_FR_bird_likert_s[,c("protectedarea_perc","milieu_catopenland","milieu_caturban","tempsrping",
                                                          "precspring","shannon","drymatter")]


gglikert(pressure_FR_bird_likert_s,add_totals = FALSE, labels_hide_below = 0.01) +
  scale_y_discrete(labels=c("milieu_catopenland" = "Open land vs. forest area (LU)","milieu_caturban" = "Urbanised area vs. forest area (LU)", "tempsrping" = "Temperature (Tstart)", "precspring" = "Rainfall (Rstart)", "shannon" = "Landscape diversity (Hstart)",              
                            "protectedarea_perc" = "Protected area (P)", "drymatter" = "Dry matter productivity (D)")) +
  scale_fill_manual(values = c("Negative, p-value > 0.05" = "#fdae61ff","Negative, p-value < 0.05" = "#d7191cff" ,"Positive, p-value < 0.05" = "#2c7bb6ff", "Positive, p-value > 0.05" = "#abd9e9ff")) +
  theme(legend.position = "none", 
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.background.y = element_rect(fill = NA),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        axis.title = element_blank())

ggsave("output/pressure_trend_bird_FR_likert_start.png",
       width = 6,
       height = 6,
       dpi = 300
)






ggplot(pressure_FR_bird_long[which(pressure_FR_bird_long$variable %in% c("trend_tend","trend_s1","trend_s2","trend_s3","trend_s4")),], aes(x = value, y = variable, fill = variable)) +
  geom_density_ridges(stat = "binline",scale = 0.9,
                      bins = 60, draw_baseline = FALSE) + xlim(c(-2,2))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75, scale = 0.9,
                      quantiles = 2) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  xlab("Pressures") + ylab("Estimate") +
  theme(legend.position = "none")

res_gam_bird_FR_correct_trend <- ddply(res_gam_bird_FR_correct,
                                        .(PLS),.fun=function(x){
                                          for(i in c("trend_past","trend_tend","trend_s1","trend_s2","trend_s3","trend_s4")){
                                            #x[which(abs(x[,i]) < abs(x[,(which(names(x)==i)+1)])),i] <- 0
                                            value_max <- max(abs(quantile(res_gam_bird_FR_correct$trend_past[which(res_gam_bird_FR_correct$pressure_removed=="none")],0.1)),abs(quantile(res_gam_bird_FR_correct$trend_past[which(res_gam_bird_FR_correct$pressure_removed=="none")],0.9)))
                                            x[which(x[,i]>value_max),i] <- value_max
                                            x[which(x[,i]<(-value_max)),i] <- -value_max
                                          }
                                          for(i in c("trend_past_signif","trend_tend_signif","trend_s1_signif","trend_s2_signif","trend_s3_signif","trend_s4_signif")){
                                            #x[which(x[,i]>max(x$trend_past_signif)),i] <- max(x$trend_past_signif)
                                            #x[which(x[,i]<min(x$trend_past_signif)),i] <- min(x$trend_past_signif)
                                            #x[which(abs(x[,i]) < abs(x[,(which(names(x)==i)+1)])),i] <- 0
                                            x[which(x[,i]>value_max),i] <- value_max
                                            x[which(x[,i]<(-value_max)),i] <- -value_max
                                          }
                                          return(x)
                                        },
                                        .progress = "text")

#res_gam_bird_FR_correct_trend <- res_gam_bird_FR_correct_trend[which(!(res_gam_bird_FR_correct_trend$sci_name_out %in% species_high)),]


overall_trend_all <- ddply(res_gam_bird_FR_correct_trend,
                           .(pressure_removed),.fun=overall_mean_sd_trend_FR,
                           .progress = "text"); addon <- ".png"
overall_trend_all <- ddply(res_gam_bird_FR_correct_trend[which(res_gam_bird_FR_correct_trend$sci_name_out %in% farmland_species),],
                           .(pressure_removed),.fun=overall_mean_sd_trend_FR,
                           .progress = "text"); addon <- "_farm.png"
overall_trend_all <- ddply(res_gam_bird_FR_correct_trend[which(res_gam_bird_FR_correct_trend$sci_name_out %in% forest_species),],
                           .(pressure_removed),.fun=overall_mean_sd_trend_FR,
                           .progress = "text"); addon <- "_forest.png"
overall_trend_all <- ddply(res_gam_bird_FR_correct_trend[which(res_gam_bird_FR_correct_trend$sci_name_out %in% urban_species),],
                           .(pressure_removed),.fun=overall_mean_sd_trend_FR,
                           .progress = "text")
overall_trend_all <- ddply(res_gam_bird_FR_correct_trend[which(res_gam_bird_FR_correct_trend$sci_name_out %in% generalist_species),],
                           .(pressure_removed),.fun=overall_mean_sd_trend_FR,
                           .progress = "text"); addon <- "_gene.png"

pressure_removed <- c("none","year","d_impervious","d_treedensity","d_agri",
                      "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                      "d_shannon","shannon","drymatter","protectedarea_perc",
                      "eulandsystem_farmland_high",
                      "CPE_mean","CPE_trend",
                      "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat")[1]


FR_all <- data.frame(value = c(overall_trend_all$mu_past[which(overall_trend_all$pressure_removed == pressure_removed)],
                               overall_trend_all$mu_tend[which(overall_trend_all$pressure_removed == pressure_removed)],
                               overall_trend_all$mu_s1[which(overall_trend_all$pressure_removed == pressure_removed)],
                               overall_trend_all$mu_s2[which(overall_trend_all$pressure_removed == pressure_removed)],
                               overall_trend_all$mu_s3[which(overall_trend_all$pressure_removed == pressure_removed)],
                               overall_trend_all$mu_s4[which(overall_trend_all$pressure_removed == pressure_removed)]),
                            sd = c(overall_trend_all$sd_past[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$sd_tend[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$sd_s1[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$sd_s2[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$sd_s3[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$sd_s4[which(overall_trend_all$pressure_removed == pressure_removed)]),
                            se = c(overall_trend_all$se_past[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$se_tend[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$se_s1[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$se_s2[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$se_s3[which(overall_trend_all$pressure_removed == pressure_removed)],
                                   overall_trend_all$se_s4[which(overall_trend_all$pressure_removed == pressure_removed)]),
                            variable = c("Past","BAU","S1","S2","S3","S4"))

FR_all$variable <- factor(FR_all$variable, levels = c("Past","BAU","S1","S2","S3","S4"))
ggplot(FR_all, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("Past"="black","BAU"="grey","S1"="#6293c9ff","S2"="#a8338fff","S3"="#85ba4cff","S4"="#ef7132ff")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave(paste0("output/trend_bird_FR_error",addon),
       width = 5,
       height = 3,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="Past")]^x/FR_all$value[which(FR_all$variable=="Past")]^2019*100}, colour = "black", linetype=2, xlim=c(2000,2019)) +
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="BAU")]^x/FR_all$value[which(FR_all$variable=="BAU")]^2019*100}, colour = "grey", xlim=c(2019,2050)) + 
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="S1")]^x/FR_all$value[which(FR_all$variable=="S1")]^2019*100}, colour = "#6293c9ff", xlim=c(2019,2050)) + 
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="S2")]^x/FR_all$value[which(FR_all$variable=="S2")]^2019*100}, colour = "#a8338fff", xlim=c(2019,2050)) + 
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="S3")]^x/FR_all$value[which(FR_all$variable=="S3")]^2019*100}, colour = "#85ba4cff", xlim=c(2019,2050)) + 
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="S4")]^x/FR_all$value[which(FR_all$variable=="S4")]^2019*100}, colour = "#ef7132ff", xlim=c(2019,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave(paste0("output/trend_bird_FR",addon),
       width = 5,
       height = 3,
       dpi = 300
)

FR_all$variable <- as.character(FR_all$variable)
comb_var <- combn(FR_all$variable,2)
test_diff_var_FR_all <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_FR_all[i,3] <- tsum.test(mean.x=FR_all$value[which(FR_all$variable==comb_var[1,i])],   s.x=FR_all$se[which(FR_all$variable==comb_var[1,i])], n.x= overall_trend_all$n[which(overall_trend_all$pressure_removed == pressure_removed)],
                                                mean.y=FR_all$value[which(FR_all$variable==comb_var[2,i])],   s.y=FR_all$se[which(FR_all$variable==comb_var[2,i])], n.y= overall_trend_all$n[which(overall_trend_all$pressure_removed == pressure_removed)])$p.value
  
}

boxLabels <- c("agi_high","agi_low","d_agri",
               "d_CPE","d_impervious",
               "d_shannon","d_tempsrping",
               "d_treedensity","eulandsystem_forest_high","eulandsystem_forest_lowmedium",
               "none","year")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("BAU",length(boxLabels)),rep("S1",length(boxLabels)),rep("S2",length(boxLabels)),rep("S3",length(boxLabels)),rep("S4",length(boxLabels))),
                 Variable = rep(overall_trend_all$pressure_removed,6),
                 box_estimate_main = c(overall_trend_all$mu_past,overall_trend_all$mu_tend,overall_trend_all$mu_s1,
                                       overall_trend_all$mu_s2,overall_trend_all$mu_s3,overall_trend_all$mu_s4), 
                 boxCILow = c(overall_trend_all$mu_past,overall_trend_all$mu_tend,overall_trend_all$mu_s1,
                              overall_trend_all$mu_s2,overall_trend_all$mu_s3,overall_trend_all$mu_s4)-1.96*c(overall_trend_all$se_past,overall_trend_all$se_tend,overall_trend_all$se_s1,
                                                                                                                                   overall_trend_all$se_s2,overall_trend_all$se_s3,overall_trend_all$se_s4),
                 boxCIHigh = c(overall_trend_all$mu_past,overall_trend_all$mu_tend,overall_trend_all$mu_s1,
                               overall_trend_all$mu_s2,overall_trend_all$mu_s3,overall_trend_all$mu_s4)+1.96*c(overall_trend_all$se_past,overall_trend_all$se_tend,overall_trend_all$se_s1,
                                                                                                                                    overall_trend_all$se_s2,overall_trend_all$se_s3,overall_trend_all$se_s4))


df$Attribute <- factor(df$Attribute, levels = c("Past","BAU","S1","S2","S3","S4"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping",
                                              "d_shannon","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","agi_low","agi_high","d_CPE"))

df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% 
                      mutate(pvalue = tsum.test(mean.x=box_estimate_main,
                                                s.x=((boxCIHigh - mean_y)/1.96),
                                                n.x= overall_trend_all$n[which(overall_trend_all$pressure_removed == "none")],
                                                mean.y=mean_y,
                                                s.y=se_y,
                                                n.y= overall_trend_all$n[which(overall_trend_all$pressure_removed == "none")],)$p.value,
                             relative_estimate = mean_y - box_estimate_main,
                             relative_se = se_y + ((boxCIHigh - mean_y)/1.96))))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  
df_signif$relativehighCI <- df_signif$relative_estimate + 1.96*df_signif$relative_se
df_signif$relativelowCI <- df_signif$relative_estimate - 1.96*df_signif$relative_se

ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","agi_low" = "\u2205 Low intensive farmland",
                            "d_CPE" = "\u2205 Pesticide exposure", "agi_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","Tend"="grey","S1"="#6293c9ff","S2"="#a8338fff","S3"="#85ba4cff","S4"="#ef7132ff")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggplot(df_signif[which(df_signif$Variable!="none"),], aes(x=relative_estimate,y = Variable, group=Attribute)) +
  geom_vline(xintercept = 0, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = relativehighCI, xmin = relativelowCI), linewidth = .5, height =.2, color = "gray50") +
  geom_point(size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("year" = "Trend", "d_impervious" = "Urbanisation","d_tempsrping" = "Temperature", "d_tempsrpingvar" = "Temperature variation", "d_precspring" = "Rainfall", "d_shannon" = "Landscape diversity",              
                            "protectedarea_perc" = "Protected area", "d_treedensity" = "Tree density","eulandsystem_forest_lowmedium" = "Low/medium intensive forests", "eulandsystem_forest_high" = "High intensive forests",
                            "d_agri" = "Agricultural surface","agi_low" = "Low intensive farmland",
                            "d_CPE" = "Pesticide exposure", "agi_high" = "High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","Tend"="grey","S1"="#6293c9ff","S2"="#a8338fff","S3"="#85ba4cff","S4"="#ef7132ff")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')



ggsave(paste0("output/trend_bird_FR_all_effect",addon),
       width = 9,
       height = 4,
       dpi = 300
)






FR_all_signif <- data.frame(value = c(overall_trend_all$mu_past_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                      overall_trend_all$mu_tend_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_s1_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_s2_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_s3_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                          overall_trend_all$mu_s4_signif[which(overall_trend_all$pressure_removed == pressure_removed)]),
                                sd = c(overall_trend_all$sd_past_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_tend_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_s1_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_s2_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_s3_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$sd_s4_signif[which(overall_trend_all$pressure_removed == pressure_removed)]),
                                se = c(overall_trend_all$se_past_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_tend_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_s1_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_s2_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_s3_signif[which(overall_trend_all$pressure_removed == pressure_removed)],
                                       overall_trend_all$se_s4_signif[which(overall_trend_all$pressure_removed == pressure_removed)]),
                                variable = c("Past","BAU","S1","S2","S3","S4"))

FR_all_signif$variable <- factor(FR_all_signif$variable, levels = c("Past","BAU","S1","S2","S3","S4"))
ggplot(FR_all_signif, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("Past"="black","BAU"="grey","S1"="#6293c9ff","S2"="#a8338fff","S3"="#85ba4cff","S4"="#ef7132ff")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")

ggsave(paste0("output/trend_bird_FR_signif_error",addon),
       width = 5,
       height = 3,
       dpi = 300
)

ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="Past")]^x/FR_all_signif$value[which(FR_all_signif$variable=="Past")]^2019*100}, colour = "black", linetype=2, xlim=c(2000,2019)) +
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="BAU")]^x/FR_all_signif$value[which(FR_all_signif$variable=="BAU")]^2019*100}, colour = "grey", xlim=c(2019,2050)) + 
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="S1")]^x/FR_all_signif$value[which(FR_all_signif$variable=="S1")]^2019*100}, colour = "#6293c9ff", xlim=c(2019,2050)) + 
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="S2")]^x/FR_all_signif$value[which(FR_all_signif$variable=="S2")]^2019*100}, colour = "#a8338fff", xlim=c(2019,2050)) + 
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="S3")]^x/FR_all_signif$value[which(FR_all_signif$variable=="S3")]^2019*100}, colour = "#85ba4cff", xlim=c(2019,2050)) + 
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="S4")]^x/FR_all_signif$value[which(FR_all_signif$variable=="S4")]^2019*100}, colour = "#ef7132ff", xlim=c(2019,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")

ggsave(paste0("output/trend_bird_FR_signif",addon),
       width = 5,
       height = 3,
       dpi = 300
)


FR_all_signif$variable <- as.character(FR_all_signif$variable)
comb_var <- combn(FR_all_signif$variable,2)
test_diff_var_FR_all_signif <- data.frame(cbind(t(comb_var),NA))
for(i in 1:dim(comb_var)[2]){
  test_diff_var_FR_all_signif[i,3] <- tsum.test(mean.x=FR_all_signif$value[which(FR_all_signif$variable==comb_var[1,i])],   s.x=FR_all_signif$se[which(FR_all_signif$variable==comb_var[1,i])], n.x= overall_trend_all$n[which(overall_trend_all$pressure_removed == "none")],
                                                       mean.y=FR_all_signif$value[which(FR_all_signif$variable==comb_var[2,i])],   s.y=FR_all_signif$se[which(FR_all_signif$variable==comb_var[2,i])], n.y= overall_trend_all$n[which(overall_trend_all$pressure_removed == "none")])$p.value
  
}


boxLabels <- c("agi_high","agi_low","d_agri",
               "d_CPE","d_impervious",
               "d_shannon","d_tempsrping",
               "d_treedensity","eulandsystem_forest_high","eulandsystem_forest_lowmedium",
               "none","year")

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c(rep("Past",length(boxLabels)),rep("Tend",length(boxLabels)),rep("S1",length(boxLabels)),rep("S2",length(boxLabels)),rep("S3",length(boxLabels)),rep("S4",length(boxLabels))),
                 Variable = rep(overall_trend_all$pressure_removed,6),
                 box_estimate_main = c(overall_trend_all$mu_past_signif,overall_trend_all$mu_tend_signif,overall_trend_all$mu_s1_signif,
                                       overall_trend_all$mu_s2_signif,overall_trend_all$mu_s3_signif,overall_trend_all$mu_s4_signif), 
                 boxCILow = c(overall_trend_all$mu_past_signif,overall_trend_all$mu_tend_signif,overall_trend_all$mu_s1_signif,
                              overall_trend_all$mu_s2_signif,overall_trend_all$mu_s3_signif,overall_trend_all$mu_s4_signif)-1.96*c(overall_trend_all$se_past_signif,overall_trend_all$se_tend_signif,overall_trend_all$se_s1_signif,
                                                                                                                                   overall_trend_all$se_s2_signif,overall_trend_all$se_s3_signif,overall_trend_all$se_s4_signif),
                 boxCIHigh = c(overall_trend_all$mu_past_signif,overall_trend_all$mu_tend_signif,overall_trend_all$mu_s1_signif,
                               overall_trend_all$mu_s2_signif,overall_trend_all$mu_s3_signif,overall_trend_all$mu_s4_signif)+1.96*c(overall_trend_all$se_past_signif,overall_trend_all$se_tend_signif,overall_trend_all$se_s1_signif,
                                                                                                                                    overall_trend_all$se_s2_signif,overall_trend_all$se_s3_signif,overall_trend_all$se_s4_signif))


df$Attribute <- factor(df$Attribute, levels = c("Past", "Tend","S1","S2","S3","S4"))
df$Variable <- factor(df$Variable, levels = c("none","year","d_impervious","d_tempsrping",
                                              "d_shannon","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
                                              "d_agri","agi_low","agi_high","d_CPE"))
df_signif <- ddply(df, .(Attribute), .fun = function(x){
  mean_y <- x$box_estimate_main[which(x$Variable == "none")]
  se_y <- (x$boxCIHigh[which(x$Variable == "none")] - mean_y)/1.96
  return(data.frame(x %>% group_by(Variable) %>% 
                      mutate(pvalue = tsum.test(mean.x=box_estimate_main,
                                                s.x=((boxCIHigh - mean_y)/1.96),
                                                n.x= overall_trend_all$n[which(overall_trend_all$pressure_removed == "none")],
                                                mean.y=mean_y,
                                                s.y=se_y,
                                                n.y= overall_trend_all$n[which(overall_trend_all$pressure_removed == "none")],)$p.value,
                             relative_estimate = mean_y - box_estimate_main,
                             relative_se = se_y + ((boxCIHigh - mean_y)/1.96))))})

df_signif$signif <- ifelse(df_signif$pvalue < 0.05,"yes","no")  
df_signif$relativehighCI <- df_signif$relative_estimate + 1.96*df_signif$relative_se
df_signif$relativelowCI <- df_signif$relative_estimate - 1.96*df_signif$relative_se

ggplot(df_signif, aes(x=box_estimate_main,y = Variable, group=Attribute)) + 
  geom_vline(data=df_signif[which(df_signif$Variable=="none"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(data=df_signif[which(df_signif$Variable=="none"),],size = 3.5, aes(color = Attribute)) + 
  geom_point(data=df_signif[which(df_signif$Variable!="none"),],size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("none" = "All covariates", "year" = "\u2205 Trend", "d_impervious" = "\u2205 Urbanisation","d_tempsrping" = "\u2205 Temperature", "d_tempsrpingvar" = "\u2205 Temperature variation", "d_precspring" = "\u2205 Rainfall", "d_shannon" = "\u2205 Landscape diversity",              
                            "protectedarea_perc" = "\u2205 Protected area", "d_treedensity" = "\u2205 Tree density","eulandsystem_forest_lowmedium" = "\u2205 Low/medium intensive forests", "eulandsystem_forest_high" = "\u2205 High intensive forests",
                            "d_agri" = "\u2205 Agricultural surface","agi_low" = "\u2205 Low intensive farmland",
                            "d_CPE" = "\u2205 Pesticide exposure", "agi_high" = "\u2205 High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","Tend"="grey","S1"="#6293c9ff","S2"="#a8338fff","S3"="#85ba4cff","S4"="#ef7132ff")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggplot(df_signif[which(df_signif$Variable!="none"),], aes(x=relative_estimate,y = Variable, group=Attribute)) +
  geom_vline(xintercept = 0, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = relativehighCI, xmin = relativelowCI), linewidth = .5, height =.2, color = "gray50") +
  geom_point(size = 3.5, aes(color = Attribute, alpha=signif)) + 
  scale_y_discrete(labels=c("year" = "Trend", "d_impervious" = "Urbanisation","d_tempsrping" = "Temperature", "d_tempsrpingvar" = "Temperature variation", "d_precspring" = "Rainfall", "d_shannon" = "Landscape diversity",              
                            "protectedarea_perc" = "Protected area", "d_treedensity" = "Tree density","eulandsystem_forest_lowmedium" = "Low/medium intensive forests", "eulandsystem_forest_high" = "High intensive forests",
                            "d_agri" = "Agricultural surface","agi_low" = "Low intensive farmland",
                            "d_CPE" = "Pesticide exposure", "agi_high" = "High intensive farmland")) + 
  scale_color_manual(values = c("Past"="black","Tend"="grey","S1"="#6293c9ff","S2"="#a8338fff","S3"="#85ba4cff","S4"="#ef7132ff")) + 
  scale_alpha_discrete(range = c(0.4, 1)) +
  theme_modern() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("") +
  xlab("Slope") + facet_grid(. ~ Attribute, scales='free')


ggsave(paste0("output/trend_bird_FR_all_signif_effect",addon),
       width = 9,
       height = 4,
       dpi = 300
)

### species list

species_list_fr <- data.frame(Species = STOC_species)
species_list_fr$Index <- NA
species_list_fr$Index[which(species_list_fr$Species %in% farmland_species)] <- "FaBI"
species_list_fr$Index[which(species_list_fr$Species %in% forest_species)] <- "FoBI"
species_list_fr$ABI <- NA
species_list_fr$ABI[which(species_list_fr$Species %in% unique(res_gam_bird_FR_correct$sci_name_out))] <- "ABI"

species_list_fr <- merge(species_list_fr, res_gam_bird_FR_nosignif[which(res_gam_bird_FR_nosignif$pressure_removed == "none"),], by.x="Species",by.y = "sci_name_out", all.x=TRUE)

species_list_fr$pressure_removed <- NULL
species_list_fr$PLS <- NULL

names(species_list_fr)[6:25] <- c("Protected_area_on_abundance","Openland_vs_forest_on_abundance","Otherland_vs_forest_on_abundance","Urban_vs_forest_on_abundance",
                                  "Temperature_on_abundance","Precipitation_on_abundance","Landscape_diversity_on_abundance",
                                  "Primary_production_on_abundance","d_urbanisation_on_trend","d_tree_density_on_trend",
                                  "lowmedium_intensive_forest_on_trend","high_intensive_forest_on_trend","d_agricultural_cover_on_trend",
                                  "low_intensive_farmland_on_trend","high_intensive_farmland_on_trend","pesticide_exposure_on_trend",
                                  "d_temperature_on_trend","d_landscape_diversity_on_trend","Deviance_explained",
                                  "Number_observation")

write.csv(species_list_fr,"output/species_list_fr.csv", row.names = FALSE)

species_list_fr <- read.csv("output/species_list_fr.csv")



life_trait <- read.table("raw_data/Life-history+characteristics+of+European+birds.txt", sep="\t", header=TRUE)
life_trait$Forest <- ifelse(life_trait$Deciduous.forest == 1 | life_trait$Coniferous.forest == 1 | life_trait$Woodland == 1, 1, 0)
life_trait$Openland <- ifelse(life_trait$Shrub == 1 | life_trait$Savanna == 1 | life_trait$Tundra == 1 | life_trait$Grassland == 1 | life_trait$Mountain.meadows == 1, 1, 0)
life_trait$Species[which(life_trait$Species == "Leiopicus medius")] <- "Dendrocoptes medius"
life_trait$Species[which(life_trait$Species == "Cyanecula svecica")] <- "Luscinia svecica"


species_list_fr2 <- merge(species_list_fr, life_trait[,c("Species","Forest","Openland","Deciduous.forest","Coniferous.forest","Woodland","Shrub","Savanna","Tundra","Grassland","Mountain.meadows")], by="Species", all.x=TRUE)

species_list_fr2$class <- NA
species_list_fr2$class[which(species_list_fr2$Openland == 0 & species_list_fr2$Woodland == 0 &(species_list_fr2$Deciduous.forest == 1 | species_list_fr2$Coniferous.forest == 1))] <- "forest_dwellers"
species_list_fr2$class[which(species_list_fr2$Forest == 0 & species_list_fr2$Openland == 1 & species_list_fr2$Shrub == 0)] <- "openland_dwellers"
species_list_fr2$class[which(species_list_fr2$Forest == 0 & species_list_fr2$Openland == 0)] <- "other"
species_list_fr2$class[which((species_list_fr2$Deciduous.forest == 1 | species_list_fr2$Coniferous.forest == 1) & (species_list_fr2$Woodland == 1 | species_list_fr2$Shrub == 1))] <- "forest_edge_dwellers"
species_list_fr2$class[which((species_list_fr2$Deciduous.forest == 0 & species_list_fr2$Coniferous.forest == 0) & species_list_fr2$Woodland == 1 & species_list_fr2$Openland == 0)] <- "forest_edge_dwellers"
species_list_fr2$class[which((species_list_fr2$Deciduous.forest == 0 & species_list_fr2$Coniferous.forest == 0) & species_list_fr2$Shrub == 1)] <- "shrub_dwellers"
species_list_fr2$class[which(is.na(species_list_fr2$class))] <- "other"

write.csv(species_list_fr2,"output/species_list_fr2.csv", row.names = FALSE)

