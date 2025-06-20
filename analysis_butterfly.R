grid_eu_mainland <- st_read("output/grid_eu_mainland.gpkg")

grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")

grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")

butterfly_data_clean <- readRDS("output/butterfly_data_clean.rds")


## site and bird data

sites <- read.csv(file = "raw_data/butterfly/ebms_transect_coord.csv", header = TRUE)

site_mainland <- sites[which(sites$transect_lon > 2100000),] # remove canary islands

butterfly_data_mainland <- butterfly_data_clean[which(butterfly_data_clean$transect_id %in% unique(site_mainland$transect_id)),]

site_mainland_sf <- st_as_sf(site_mainland, coords = c("transect_lon","transect_lat"))

st_crs(site_mainland_sf) <- 3035

site_mainland_sf_reproj <- st_transform(site_mainland_sf,crs(grid_eu_mainland_outline))

site_mainland$Long_LAEA <- st_coordinates(site_mainland_sf_reproj)[,1]
site_mainland$Lat_LAEA <- st_coordinates(site_mainland_sf_reproj)[,2]

site_mainland_sf_reproj_butterfly <- site_mainland_sf_reproj
#saveRDS(site_mainland_sf_reproj_butterfly,"output/site_mainland_sf_reproj_butterfly.rds")

## plot sites and area monitored

ggplot(grid_eu_mainland_outline) +
  geom_sf() +
  geom_sf(data=site_mainland_sf_reproj, size=1) +
  theme_minimal()

ggsave("output/EBMS_site.png",
       width = 8,
       height = 10,
       dpi = 300
)

radius_buffer <- sqrt(median(site_mainland_sf_reproj_butterfly$transect_length,na.rm=TRUE)/pi)
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj_butterfly, dist = 2500)
area_site_mainland <-  st_intersection(site_mainland_buffer, grid_eu_mainland)
area_site_mainland$area <- as.numeric(st_area(area_site_mainland))


## summarize external variable for each site and format them
area_site_mainland_df <- area_site_mainland
st_geometry(area_site_mainland_df) <- NULL


value_site_mainland <- ddply(area_site_mainland_df,.(transect_id),
                             .fun = function(x){
                               
                               pop2000 = weighted.mean(x$pop2000,x$area); pop2020 = weighted.mean(x$pop2020,x$area)
                               impervious2006 = weighted.mean(x$impervious2006,x$area); impervious2018 = weighted.mean(x$impervious2018,x$area)
                               treedensity2012 = weighted.mean(x$treedensity2012,x$area); treedensity2018 = weighted.mean(x$treedensity2018,x$area)
                               lightpollution2000 = weighted.mean(x$lightpollution2000,x$area); lightpollution2013 = weighted.mean(x$lightpollution2013,x$area)
                               pesticide_kg = weighted.mean(x$pesticide_kg,x$area); pesticide_kg_ha = weighted.mean(x$pesticide_kg_ha,x$area); pesticide_nodu_ha = weighted.mean(x$pesticide_nodu_ha,x$area)
                               woodprod2000 = weighted.mean(x$woodprod2000,x$area); woodprod2010 = weighted.mean(x$woodprod2010,x$area); woodprodaverage = weighted.mean(x$woodprodaverage,x$area)
                               drymatter2000 = weighted.mean(x$drymatter2000,x$area); drymatter2018 = weighted.mean(x$drymatter2018,x$area)
                               smallwoodyfeatures = weighted.mean(x$smallwoodyfeatures,x$area); fragmentation = weighted.mean(x$fragmentation,x$area)
                               forestintegrity = weighted.mean(x$forestintegrity,x$area,na.rm=TRUE)
                               temp2000 = weighted.mean(x$temp2000,x$area); temp2020 = weighted.mean(x$temp2020,x$area); tempspring2000 = weighted.mean(x$tempspring2000,x$area); tempspring2020 = weighted.mean(x$tempspring2020,x$area)
                               tempspringvar2000 = weighted.mean(x$tempspringvar2000,x$area); tempspringvar2020 = weighted.mean(x$tempspringvar2020,x$area)
                               prec2000 = weighted.mean(x$prec2000,x$area); prec2020 = weighted.mean(x$prec2020,x$area); precspring2000 = weighted.mean(x$precspring2000,x$area); precspring2020 = weighted.mean(x$precspring2020,x$area)
                               precspringvar2000 = weighted.mean(x$precspringvar2000); precspringvar2020 = weighted.mean(x$precspringvar2020,x$area)
                               humidity2000 = weighted.mean(x$humidity2000,x$area); humidity2020 = weighted.mean(x$humidity2020,x$area); humidityspring2000 = weighted.mean(x$humidityspring2000,x$area)
                               humidityspring2020 = weighted.mean(x$humidityspring2020,x$area); humidityspringvar2000 = weighted.mean(x$humidityspringvar2000,x$area); humidityspringvar2020 = weighted.mean(x$humidityspringvar2020,x$area)
                               shannon2000 = weighted.mean(x$shannon_2000,x$area); shannon2018 = weighted.mean(x$shannon_2018,x$area)
                               agri2000 = weighted.mean(x$agri_2000,x$area); agri2018 = weighted.mean(x$agri_2018,x$area)
                               GDP2000 = weighted.mean(x$GDP2000,x$area); GDP2015 = weighted.mean(x$GDP2015,x$area)
                               
                               biogeo_area = data.frame(x %>% group_by(biogeo_area) %>% summarise(biogeo_surface=sum(area)))
                               biogeo_area = biogeo_area$biogeo_area[which.max(biogeo_area$biogeo_surface)]
                               
                               PLS = data.frame(x %>% group_by(PLS) %>% summarise(biogeo_surface=sum(area)))
                               PLS = PLS$PLS[which.max(PLS$biogeo_surface)]
                               
                               forestintegrity_cat = data.frame(x %>% group_by(forestintegrity_cat) %>% summarise(biogeo_surface=sum(area)))
                               if(anyNA(forestintegrity_cat$forestintegrity_cat)){
                                 forestintegrity_cat <- na.omit(forestintegrity_cat)
                                 if(nrow(forestintegrity_cat > 0)){
                                   forestintegrity_cat = forestintegrity_cat$forestintegrity_cat[which.max(forestintegrity_cat$biogeo_surface)]
                                 }else{
                                   forestintegrity_cat = NA
                                 }
                               }else{
                                 forestintegrity_cat = forestintegrity_cat$forestintegrity_cat[which.max(forestintegrity_cat$biogeo_surface)]
                               }
                               
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
                               
                               return(data.frame(pop2000,pop2020,impervious2006,impervious2018,treedensity2012,treedensity2018,
                                                 lightpollution2000,lightpollution2013,pesticide_kg,pesticide_kg_ha,pesticide_nodu_ha,
                                                 woodprod2000,woodprod2010,woodprodaverage,drymatter2000,drymatter2018,
                                                 smallwoodyfeatures,fragmentation,forestintegrity,temp2000,temp2020,tempspring2000,tempspring2020,
                                                 tempspringvar2000,tempspringvar2020,prec2000,prec2020,precspring2000,precspring2020,
                                                 precspringvar2000,precspringvar2020,humidity2000,humidity2020,humidityspring2000,humidityspring2020,
                                                 humidityspringvar2000,humidityspringvar2020,shannon2000,shannon2018,agri2000,agri2018,GDP2000,GDP2015,
                                                 biogeo_area,PLS,forestintegrity_cat,eulandsystem_max,grassland,farmland,
                                                 low_farmland,high_farmland,low_farmland_tot,high_farmland_tot,protectedarea_cat,
                                                 protectedarea_perc,protectedarea_type,protectedarea_size,eulandsystem_cat_forest,eulandsystem_cat_urban,eulandsystem_cat_farmland,
                                                 eulandsystem_farmland_low,eulandsystem_farmland_medium,eulandsystem_farmland_high,
                                                 eulandsystem_forest_lowmedium,eulandsystem_forest_high))
                               
                             },.progress = "text")



value_site_mainland$GDP2000_percap <- value_site_mainland$GDP2000/value_site_mainland$pop2000
pop2015 <- (value_site_mainland$pop2020-value_site_mainland$pop2000)/20*(2015-2020)+value_site_mainland$pop2020
value_site_mainland$GDP2015_percap <- value_site_mainland$GDP2015/pop2015

value_site_mainland$eulandsystem_cat <- NA
#value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem_max %in% c(11,12,13,80,90))] <-"no_intensity"
#value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem_max %in% c(21,31,41,51,61,731,71:75))] <-"low_intensity"
value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem_max %in% c(11,12,13,80,90,21,31,41,51,61,731,71:75))] <-"low_intensity"
value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem_max %in% c(22,32,42,52,62,732))] <-"medium_intensity"
value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem_max %in% c(23,43,53,63,733))] <-"high_intensity"
value_site_mainland$eulandsystem_cat <- factor(value_site_mainland$eulandsystem_cat, levels = c("no_intensity","low_intensity","medium_intensity","high_intensity")) 
value_site_mainland$eulandsystem_cat_urban <- factor(value_site_mainland$eulandsystem_cat_urban, levels = c("low_intensity","medium_intensity","high_intensity","no_urban")) 
value_site_mainland$eulandsystem_cat_forest <- factor(value_site_mainland$eulandsystem_cat_forest, levels = c("low_intensity","medium_intensity","high_intensity","no_forest")) 
value_site_mainland$eulandsystem_cat_farmland <- factor(value_site_mainland$eulandsystem_cat_farmland, levels = c("low_intensity","medium_intensity","high_intensity","no_farmland")) 

value_site_mainland$protectedarea_size_cor <- value_site_mainland$protectedarea_size
value_site_mainland$protectedarea_size_cor[which(value_site_mainland$protectedarea_size>500)] <- 500

value_site_mainland_butterfly <- value_site_mainland

saveRDS(value_site_mainland_butterfly,"output/value_site_mainland_butterfly.rds")

## add zero when species no present at monitored site

wide_butterfly_data <- data.frame(butterfly_data_mainland[,c("transect_id","year","species_name","count_corrected")] %>% group_by(transect_id) %>% tidyr::complete(year,species_name))
wide_butterfly_data$count_corrected[which(is.na(wide_butterfly_data$count_corrected))] <- 0

butterfly_data_mainland <- merge(wide_butterfly_data,site_mainland,by=c("transect_id"), all.x=T)

#butterfly_data_mainland <- merge(butterfly_data_mainland,value_site_mainland, by="transect_id", all.x=TRUE)

saveRDS(butterfly_data_mainland,"output/butterfly_data_mainland.rds")


## remove site not followed enough

nb_year_p_site <- data.frame(butterfly_data_mainland %>% group_by(transect_id, year) %>% summarise(count=n()))
nb_year_p_site <- data.frame(nb_year_p_site %>% group_by(transect_id) %>% summarise(nb_year=n(),
                                                                               min_year=min(year),
                                                                               max_year=max(year)))

selected_site_mainland <- nb_year_p_site[which(nb_year_p_site$nb_year >= 5 & nb_year_p_site$max_year >= 2011),]

subsite_data_mainland_trend <- butterfly_data_mainland[which(butterfly_data_mainland$transect_id %in% c(selected_site_mainland$transect_id)),]

subsite_data_mainland_trend <- subsite_data_mainland_trend[which(subsite_data_mainland_trend$transect_id %in% unique(value_site_mainland_butterfly$transect_id)),] # remove some island site no longer in grid_eu_mainland

subsite_data_mainland_trend_butterfly <- subsite_data_mainland_trend

saveRDS(subsite_data_mainland_trend_butterfly,"output/subsite_data_mainland_trend_butterfly.rds")

ggplot(grid_eu_mainland_outline) +
  geom_sf() +
  geom_sf(data=site_mainland_sf_reproj_butterfly[which(site_mainland_sf_reproj_butterfly$transect_id %in% unique(subsite_data_mainland_trend_butterfly$transect_id)),], size=1) +
  theme_minimal()

ggsave("output/EBMS_site_selected.png",
       width = 8,
       height = 10,
       dpi = 300
)


site_pls <- unique(press_mainland_trend_butterfly_scale[,c("transect_id","PLS")])
year_site_pls <- merge(subsite_data_mainland_trend_butterfly,site_pls, by="transect_id")
year_site <- data.frame(year_site_pls %>% group_by(transect_id,bms_id,year) %>% summarise(count=n()))
year_site <- data.frame(year_site %>% group_by(bms_id,year) %>% summarise(count=n()))
year_site <- data.frame(year_site %>% group_by(bms_id) %>% mutate(label=case_when(year==max(year) ~ bms_id)))
year_site2 <- data.frame(year_site_pls %>% group_by(transect_id,PLS,year) %>% summarise(count=n()))
year_site2 <- na.omit(data.frame(year_site2 %>% group_by(PLS,year) %>% summarise(count=n())))
year_site2$PLS <- factor(as.character(year_site2$PLS), levels = as.character(c(1:25)))
year_site2 <- data.frame(year_site2 %>% group_by(PLS) %>% mutate(label=case_when(year==max(year) ~ PLS)))

ggplot(year_site, aes(y=count, col=bms_id, x=year)) +
  geom_line() +
  scale_y_log10(name = "Number of sites") + 
  geom_vline(xintercept = 2021) + geom_label_repel(aes(label = label),nudge_x = 4,na.rm = TRUE) +
  theme_modern() +
  theme(legend.position = "none")

ggplot(year_site2, aes(x=year,y=count, col=PLS)) +
  geom_line() +
  scale_y_log10(name = "Number of sites") + scale_color_viridis_d() +
  geom_vline(xintercept = 2021) + geom_label_repel(aes(label = label),nudge_x = 4,na.rm = TRUE) +
  theme_modern() +
  theme(legend.position = "none")

ggsave("output/site_year_scheme_butterfly.png",
       width = 12,
       height = 8,
       dpi = 300
)

## get value per year per pressure

press_mainland_trend <- ddply(distinct(subsite_data_mainland_trend_butterfly,transect_id,year,.keep_all=TRUE), .(transect_id,year),
                              .fun = function(x,pressure_data){
                                
                                pressure_subdata <- pressure_data[which(pressure_data$transect_id == x$transect_id),]
                                
                                impervious_2018 <- pressure_subdata$impervious2018
                                treedensity_2018 <- pressure_subdata$treedensity2018
                                agri_2018 <- pressure_subdata$agri2018
                                tempspring_2020 <- pressure_subdata$tempspring2020
                                tempspringvar_2020 <- pressure_subdata$tempspringvar2020
                                precspring_2020 <- pressure_subdata$precspring2020
                                shannon_2018 <- pressure_subdata$shannon2018
                                
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
                                
                                PLS <- pressure_subdata$PLS
                                
                                trend_result <- data.frame(impervious_2018,treedensity_2018,agri_2018,tempspring_2020,tempspringvar_2020,precspring_2020,shannon_2018,
                                                           d_impervious,d_treedensity,d_agri,d_tempsrping,tempsrping,d_tempsrpingvar,d_precspring,precspring,
                                                           d_shannon,shannon,milieu,drymatter,protectedarea_perc,protectedarea_type,
                                                           eulandsystem_cat,eulandsystem_farmland_low,eulandsystem_farmland_medium,eulandsystem_farmland_high,
                                                           eulandsystem_forest_lowmedium,eulandsystem_forest_high,PLS)
                                return(trend_result)
                              },pressure_data = value_site_mainland_butterfly,
                              .progress = "text")

press_mainland_trend$milieu_cat <- NA
press_mainland_trend$milieu_cat[which(press_mainland_trend$milieu %in% c(21,22,23))] <- "urban"
press_mainland_trend$milieu_cat[which(press_mainland_trend$milieu %in% c(41,42,43,71,72,74,75))] <- "forest and shrub"
press_mainland_trend$milieu_cat[which(press_mainland_trend$milieu %in% c(31,32,51,52,53,61,62,63,731,732,733))] <- "openland"
press_mainland_trend$milieu_cat[which(press_mainland_trend$milieu %in% c(0,11,12,13,80,90))] <- "others"

press_mainland_trend_butterfly <- press_mainland_trend
saveRDS(press_mainland_trend_butterfly,"output/press_mainland_trend_butterfly.rds") 


press_mainland_trend_scale <- press_mainland_trend_butterfly
press_mainland_trend_scale$eulandsystem_cat <- droplevels(press_mainland_trend_scale$eulandsystem_cat)
press_mainland_trend_scale[,c("d_impervious","d_treedensity","d_agri",
                              "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                              "d_shannon","shannon","drymatter","protectedarea_perc",
                              "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",   
                              "eulandsystem_forest_lowmedium","eulandsystem_forest_high")] <- scale(press_mainland_trend_scale[,c("d_impervious","d_treedensity","d_agri",
                                                                                                                                  "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                                                                                                                  "d_shannon","shannon","drymatter","protectedarea_perc",
                                                                                                                                  "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",   
                                                                                                                                  "eulandsystem_forest_lowmedium","eulandsystem_forest_high")])



press_mainland_trend_butterfly_scale <- press_mainland_trend_scale
saveRDS(press_mainland_trend_butterfly_scale,"output/press_mainland_trend_butterfly_scale.rds") 


###### Analysis

### Load previously produced datasets

butterfly_data_mainland <- readRDS("output/butterfly_data_mainland.rds")
grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")
press_mainland_trend_butterfly_scale <- readRDS("output/press_mainland_trend_butterfly_scale.rds")
site_mainland_sf_reproj_butterfly <- readRDS("output/site_mainland_sf_reproj_butterfly.rds")
subsite_data_mainland_trend_butterfly <- readRDS("output/subsite_data_mainland_trend_butterfly.rds")


### correlation between covariables

test_multicor <- press_mainland_trend_butterfly_scale[which(press_mainland_trend_butterfly_scale$year==2010),c("d_impervious","d_treedensity","d_agri",
                                                                                           "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                                                                           "d_shannon","shannon","drymatter","protectedarea_perc","protectedarea_type",
                                                                                           "eulandsystem_cat","eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                                                                           "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat")]
test_multicor$milieu_cat <- as.numeric(as.factor(test_multicor$milieu_cat))
test_multicor$eulandsystem_cat <- as.numeric(test_multicor$eulandsystem_cat)
test_multicor <- round(cor(na.omit(test_multicor)),2)
get_upper_tri <- function(test_multicor){
  test_multicor[lower.tri(test_multicor)]<- NA
  return(test_multicor)
}
test_multicor <- get_upper_tri(test_multicor)
test_multicor <- melt(test_multicor, na.rm = TRUE)
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

ggsave("output/cor_heatmap_butterfly.png",
       width = 15,
       height = 15,
       dpi = 300
)

### find minimum nubmer of site for GLMP

find_site_nubmer <- function(butterfly_data,pressure_data){
  
  species_press_data_year <- merge(butterfly_data, pressure_data[which(pressure_data$transect_id %in% unique(butterfly_data$transect_id) & pressure_data$year %in% unique(butterfly_data$year)),], by =c("transect_id","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("transect_id","count_corrected","year","bms_id","Long_LAEA","Lat_LAEA",
                                                   "pop","impervious","treedensity","drymatter","tempspring","tempspringvar",  
                                                   "precspring","protectedarea_perc","smallwoodyfeatures",
                                                   "shannon","eulandsystem_cat","grassland","farmland","PLS")])
  
  unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
  
  result_site_number <- data.frame(unique_poisson_df %>% group_by(PLS) %>% summarize(count=n()))
  
  return(result_site_number)
}

site_number_species_biogeo_butterfly <- ddply(subsite_data_mainland_trend_butterfly,
                                    .(species_name),.fun=find_site_nubmer,
                                    pressure_data=press_mainland_trend_butterfly_scale, .progress = "text")

saveRDS(site_number_species_biogeo_butterfly,"output/site_number_species_biogeo_butterfly.rds")



source("functions.R")


res_gamm_butterfly <- ddply(subsite_data_mainland_trend_butterfly,
                       .(species_name),.fun=gam_species_PLS1b,
                       pressure_data=press_mainland_trend_butterfly_scale,site_data=site_mainland_sf_reproj_butterfly,
                       .progress = "text")
res_gamm_butterfly <- res_gamm_butterfly[which(!is.na(res_gamm_butterfly$PLS)),]

#saveRDS(res_gamm_butterfly,"output/res_gamm_butterfly1.rds")


res_gamm_butterfly <- ddply(subsite_data_mainland_trend_butterfly,
                            .(species_name),.fun=gam_species_PLS2b,
                            pressure_data=press_mainland_trend_butterfly_scale,site_data=site_mainland_sf_reproj_butterfly,
                            .progress = "text")
res_gamm_butterfly <- res_gamm_butterfly[which(!is.na(res_gamm_butterfly$PLS)),]

#saveRDS(res_gamm_butterfly,"output/res_gamm_butterfly2.rds")

for(i in sort(unique(res_gamm_butterfly_correct$species_name))){
  print(i)
  plot_check <- gam_species_PLS2b_check(subsite_data_mainland_trend_butterfly[which(subsite_data_mainland_trend_butterfly$species_name == i),],
                                       pressure_data=press_mainland_trend_butterfly_scale,site_data=site_mainland_sf_reproj_butterfly)
  ggsave(plot=plot_check,
         filename = paste0("output/plot_check_",i,".png"),
         width=8, height=4, dpi=200)
}

#res_gamm_butterfly <- readRDS("output/res_gamm_butterflynew.rds")

res_gamm_butterfly_correct <- res_gamm_butterfly[which(res_gamm_butterfly$dev_exp>0.2),]
res_gamm_butterfly_correct <- res_gamm_butterfly[which(res_gamm_butterfly$dev_exp>0.2 & res_gamm_butterfly$species_name %in% c(grassland_species, woodland_species, woodland_ind_species, wetland_species)),]



res_gam_butterfly <- ddply(subsite_data_mainland_trend_butterfly,
                      .(species_name),.fun=gam_species_PLS3b,
                      pressure_data=press_mainland_trend_butterfly_scale,site_data=site_mainland_sf_reproj_butterfly,
                      .progress = "text")
res_gam_butterfly <- res_gam_butterfly[which(!is.na(res_gam_butterfly$PLS)),]

#saveRDS(res_gam_butterfly,"output/res_gam_butterfly.rds")


dec_species_PLS <- res_gam_mainland_species_PLS_trend_butterfly
dec_species_PLS$year[which(is.na(dec_species_PLS$year))] <- 0
dec_species_PLS <- dec_species_PLS %>% group_by(PLS) %>% summarise(nb_pos = length(which(year > 0)),
                                                                   nb_neg = length(which(year < 0)),
                                                                   nb_stable = length(which(year == 0)),
                                                                   nb_tot = length(year))
dec_species_PLS$perc_species_neg <- dec_species_PLS$nb_neg/dec_species_PLS$nb_tot
dec_species_PLS$perc_species_pos <- dec_species_PLS$nb_pos/dec_species_PLS$nb_tot


dec_species_PLS_sf <- merge(grid_eu_mainland_biogeo,dec_species_PLS,by="PLS", all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=dec_species_PLS_sf, aes(fill=perc_species_neg)) + scale_fill_gradientn(colors = paletteer_c("ggthemes::Classic Area Red", 30))
ggplot() + geom_sf() +  
  geom_sf(data=dec_species_PLS_sf, aes(fill=perc_species_pos)) + scale_fill_gradientn(colors = paletteer_c("ggthemes::Classic Blue", 30))


##### All species

matrix_pressure_PLS_neg_butterfly <- res_gam_butterfly
#matrix_pressure_PLS_neg_butterfly <- res_gam_butterfly_grassland
#matrix_pressure_PLS_neg_butterfly <- res_gam_butterfly_woodland
#matrix_pressure_PLS_neg_butterfly <- res_gam_butterfly_woodland_ind
#matrix_pressure_PLS_neg_butterfly <- res_gam_butterfly_wetland
matrix_pressure_PLS_neg_butterfly <- matrix_pressure_PLS_neg_butterfly %>% group_by(PLS) %>% summarise(openland_effect = length(which(milieu_catopenland < 0))/length(milieu_catopenland),
                                                                                             otherLULC_effect = length(which(milieu_catothers < 0))/length(milieu_catothers),
                                                                                             urban_effect = length(which(milieu_caturban < 0))/length(milieu_caturban),
                                                                                             tempspring_effect = length(which(tempsrping < 0))/length(tempsrping),
                                                                                             precspring_effect = length(which(precspring < 0))/length(precspring),
                                                                                             shannon_effect = length(which(shannon < 0))/length(shannon),
                                                                                             drymatter_effect = length(which(drymatter < 0))/length(drymatter),
                                                                                             d_impervious_effect =  length(which(`year:d_impervious` < 0))/length(`year:d_impervious`),
                                                                                             d_tempsrping_effect =  length(which(`year:d_tempsrping` < 0))/length(`year:d_tempsrping`),
                                                                                             d_tempsrpingvar_effect =  length(which(`year:d_tempsrpingvar` < 0))/length(`year:d_tempsrpingvar`),
                                                                                             d_precspring_effect =  length(which(`year:d_precspring` < 0))/length(`year:d_precspring`),
                                                                                             d_shannon_effect =  length(which(`year:d_shannon` < 0))/length(`year:d_shannon`),
                                                                                             protectedarea_effect =  length(which(`year:protectedarea_perc` < 0))/length(`year:protectedarea_perc`),
                                                                                             d_treedensity_lowmedium_effect =  length(which(`year:d_treedensity:eulandsystem_forest_lowmedium` < 0))/length(`year:d_treedensity:eulandsystem_forest_lowmedium`),
                                                                                             d_treedensity_high_effect =  length(which(`year:d_treedensity:eulandsystem_forest_high` < 0))/length(`year:d_treedensity:eulandsystem_forest_high`),
                                                                                             d_agri_low_effect =  length(which(`year:d_agri:eulandsystem_farmland_low` < 0))/length(`year:d_agri:eulandsystem_farmland_low`),
                                                                                             d_agri_medium_effect =  length(which(`year:d_agri:eulandsystem_farmland_medium` < 0))/length(`year:d_agri:eulandsystem_farmland_medium`),
                                                                                             d_agri_high_effect =  length(which(`year:d_agri:eulandsystem_farmland_high` < 0))/length(`year:d_agri:eulandsystem_farmland_high`),
                                                                                             protectedarea_type_effect =  length(which(`year:protectedarea_perc:protectedarea_type` < 0))/length(`year:protectedarea_perc:protectedarea_type`),
                                                                                             nb_sp = length(`drymatter`))


matrix_pressure_PLS_pos_butterfly <- res_gam_butterfly
#matrix_pressure_PLS_pos_butterfly <- res_gam_butterfly_grassland
#matrix_pressure_PLS_pos_butterfly <- res_gam_butterfly_woodland
#matrix_pressure_PLS_pos_butterfly <- res_gam_butterfly_woodland_ind
#matrix_pressure_PLS_pos_butterfly <- res_gam_butterfly_wetland
matrix_pressure_PLS_pos_butterfly <- matrix_pressure_PLS_pos_butterfly %>% group_by(PLS) %>% summarise(openland_effect = length(which(milieu_catopenland > 0))/length(milieu_catopenland),
                                                                                             otherLULC_effect = length(which(milieu_catothers > 0))/length(milieu_catothers),
                                                                                             urban_effect = length(which(milieu_caturban > 0))/length(milieu_caturban),
                                                                                             tempspring_effect = length(which(tempsrping > 0))/length(tempsrping),
                                                                                             precspring_effect = length(which(precspring > 0))/length(precspring),
                                                                                             shannon_effect = length(which(shannon > 0))/length(shannon),
                                                                                             drymatter_effect = length(which(drymatter > 0))/length(drymatter),
                                                                                             d_impervious_effect =  length(which(`year:d_impervious` > 0))/length(`year:d_impervious`),
                                                                                             d_tempsrping_effect =  length(which(`year:d_tempsrping` > 0))/length(`year:d_tempsrping`),
                                                                                             d_tempsrpingvar_effect =  length(which(`year:d_tempsrpingvar` > 0))/length(`year:d_tempsrpingvar`),
                                                                                             d_precspring_effect =  length(which(`year:d_precspring` > 0))/length(`year:d_precspring`),
                                                                                             d_shannon_effect =  length(which(`year:d_shannon` > 0))/length(`year:d_shannon`),
                                                                                             protectedarea_effect =  length(which(`year:protectedarea_perc` > 0))/length(`year:protectedarea_perc`),
                                                                                             d_treedensity_lowmedium_effect =  length(which(`year:d_treedensity:eulandsystem_forest_lowmedium` > 0))/length(`year:d_treedensity:eulandsystem_forest_lowmedium`),
                                                                                             d_treedensity_high_effect =  length(which(`year:d_treedensity:eulandsystem_forest_high` > 0))/length(`year:d_treedensity:eulandsystem_forest_high`),
                                                                                             d_agri_low_effect =  length(which(`year:d_agri:eulandsystem_farmland_low` > 0))/length(`year:d_agri:eulandsystem_farmland_low`),
                                                                                             d_agri_medium_effect =  length(which(`year:d_agri:eulandsystem_farmland_medium` > 0))/length(`year:d_agri:eulandsystem_farmland_medium`),
                                                                                             d_agri_high_effect =  length(which(`year:d_agri:eulandsystem_farmland_high` > 0))/length(`year:d_agri:eulandsystem_farmland_high`),
                                                                                             protectedarea_type_effect =  length(which(`year:protectedarea_perc:protectedarea_type` > 0))/length(`year:protectedarea_perc:protectedarea_type`),
                                                                                             nb_sp = length(`drymatter`))

pressure_EU_butterfly <- data.frame(rbind(matrix_pressure_PLS_pos_butterfly[matrix_pressure_PLS_pos_butterfly$PLS=="europe",],
                                     matrix_pressure_PLS_neg_butterfly[matrix_pressure_PLS_neg_butterfly$PLS=="europe",]))
pressure_EU_butterfly$PLS <- pressure_EU_butterfly$nb_sp <- NULL
pressure_EU_butterfly$sign <- c("positive effect","negative effect")
pressure_EU_butterfly <- melt(pressure_EU_butterfly, id.vars="sign")

df_pressure_EU_butterfly <- pressure_EU_butterfly %>%
  mutate(value = ifelse(sign == "positive effect", value, -1 * value))

df_pressure_EU_butterfly_factor <- df_pressure_EU_butterfly %>% group_by(variable) %>% summarise(score = sum(value)/sum(abs(value)))
df_pressure_EU_butterfly_factor <- df_pressure_EU_butterfly_factor[order(df_pressure_EU_butterfly_factor$score),]
df_pressure_EU_butterfly_factor$variable <- as.character(df_pressure_EU_butterfly_factor$variable)

df_pressure_EU_butterfly$variable <- as.character(df_pressure_EU_butterfly$variable)
df_pressure_EU_butterfly$variable <- factor(df_pressure_EU_butterfly$variable, df_pressure_EU_butterfly_factor$variable)

ggplot(df_pressure_EU_butterfly, aes(x = variable, y = value, fill = sign)) +
  geom_col(alpha=0.5) + geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = c("positive effect" = "blue","negative effect" = "red")) +
  scale_x_discrete(labels=c("openland_effect" = "Openland vs. forest on abundance", "otherLULC_effect" = "Other LULC vs. forest on abundance", "urban_effect" = "Urban land vs. forest on abundance", "tempspring_effect" = "Mean temperature on abundance",             
                            "precspring_effect" = "Mean precipitation on abundance", "shannon_effect" = "Landscape diversity on abundance", "drymatter_effect" = "Ecosystem productivity on abundance", "d_impervious_effect" = "D urbanisation on trend",           
                            "d_tempsrping_effect" = "D temperature on trend", "d_tempsrpingvar_effect" = "D temperature variation on trend", "d_precspring_effect" = "D precipitation on trend", "d_shannon_effect" = "D landscape diversity on trend",              
                            "protectedarea_effect" = "Protected area percentage on trend", "d_treedensity_lowmedium_effect" = "D tree density in low/medium intensive forests on trend", "d_treedensity_high_effect" = "D tree density in high intensive forests on trend", "d_agri_low_effect" = "D agricultural surface in low intensive farmland on trend",             
                            "d_agri_medium_effect" = "D agricultural surface in medium intensive farmland on trend", "d_agri_high_effect" = "D agricultural surface in high intensive farmland on trend", "protectedarea_type_effect" = "Protected area type on trend")) +
  xlab("Pressures") + ylab("% of species impacted") +
  theme_minimal() + coord_flip() +
  theme(legend.position = "none")

ggsave("output/pressure_butterfly_eu.png",
       width = 8,
       height = 6,
       dpi = 300
)

ggplot(df_pressure_EU_butterfly[which(df_pressure_EU_butterfly$variable %in% c("d_impervious_effect","d_tempsrping_effect","d_tempsrpingvar_effect",
                                                                     "d_precspring_effect","d_shannon_effect", "protectedarea_effect",
                                                                     "d_treedensity_lowmedium_effect","d_treedensity_high_effect","d_agri_low_effect",
                                                                     "d_agri_medium_effect","d_agri_high_effect","protectedarea_type_effect")),], aes(x = variable, y = value, fill = sign)) +
  geom_col(alpha=0.5) + geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = c("positive effect" = "blue","negative effect" = "red")) +
  scale_x_discrete(labels=c("d_impervious_effect" = "D urbanisation on trend",           
                            "d_tempsrping_effect" = "D temperature on trend", "d_tempsrpingvar_effect" = "D temperature variation on trend", "d_precspring_effect" = "D precipitation on trend", "d_shannon_effect" = "D landscape diversity on trend",              
                            "protectedarea_effect" = "Protected area percentage on trend", "d_treedensity_lowmedium_effect" = "D tree density in low/medium intensive forests on trend", "d_treedensity_high_effect" = "D tree density in high intensive forests on trend", "d_agri_low_effect" = "D agricultural surface in low intensive farmland on trend",             
                            "d_agri_medium_effect" = "D agricultural surface in medium intensive farmland on trend", "d_agri_high_effect" = "D agricultural surface in high intensive farmland on trend", "protectedarea_type_effect" = "Protected area type on trend")) +
  xlab("Pressures") + ylab("% of species impacted") +
  theme_minimal() + coord_flip() +
  theme(legend.position = "none")

ggsave("output/pressure_trend_butterfly_eu.png",
       width = 8,
       height = 6,
       dpi = 300
)

ggplot(df_pressure_EU_butterfly[which(df_pressure_EU_butterfly$variable %in% c("openland_effect","otherLULC_effect","urban_effect","tempspring_effect",
                                                                     "precspring_effect","shannon_effect","drymatter_effect")),], aes(x = variable, y = value, fill = sign)) +
  geom_col(alpha=0.5) + geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = c("positive effect" = "blue","negative effect" = "red")) +
  scale_x_discrete(labels=c("openland_effect" = "Openland vs. forest on abundance", "otherLULC_effect" = "Other LULC vs. forest on abundance", "urban_effect" = "Urban land vs. forest on abundance", "tempspring_effect" = "Mean temperature on abundance",             
                            "precspring_effect" = "Mean precipitation on abundance", "shannon_effect" = "Landscape diversity on abundance", "drymatter_effect" = "Ecosystem productivity on abundance")) +
  xlab("Pressures") + ylab("% of species impacted") +
  theme_minimal() + coord_flip() +
  theme(legend.position = "none")

ggsave("output/control_ab_butterfly_eu.png",
       width = 8,
       height = 6,
       dpi = 300
)


### by habitat from https://butterfly-monitoring.net/sites/default/files/Publications/Technical%20report%20EU%20Grassland%20indicator%201990-2017%20June%202019%20v4%20(3).pdf
# https://butterfly-monitoring.net/sites/default/files/Pdf/Reports/Assessing%20Butterflies%20in%20Europe%20-%20Butterfly%20Indicators%20Revised.pdf

grassland_species <- c("Lasiommata megera","Coenonympha pamphilus","Lycaena phlaeas","Ochlodes sylvanus",
                       "Polyommatus icarus","Thymelicus acteon","Anthocharis cardamines","Cupido minimus",
                       "Cyaniris semiargus","Erynnis tages","Lysandra bellargus","Lysandra coridon",
                       "Maniola jurtina","Euphydryas aurinia","Phengaris arion","Phengaris nausithous",
                       "Spialia sertorius")

woodland_species <- c("Apatura ilia","Apatura iris","Apatura metis","Aporia crataegi","Araschnia levana",
                      "Argynnis laodice","Argynnis pandora","Argynnis paphia","Boloria euphrosyne","Boloria graeca",
                      "Brintesia circe","Carterocephalus silvicola","Celastrina argiolus","Coenonympha arcania","Coenonympha hero",
                      "Coenonympha leander","Colias caucasica","Erebia aethiopellus","Erebia aethiops","Erebia cyclopius",
                      "Erebia euryale","Erebia ligea","Erebia neoridas","Erebia zapateri","Euphydryas intermedia",
                      "Euphydryas maturna","Fabriciana elisa","Favonius quercus","Gonepteryx farinosa","Gonepteryx maderensis",
                      "Gonepteryx rhamni","Hipparchia fagi","Hipparchia hermione","Hipparchia maderensis","Hipparchia mersina",
                      "Hipparchia syriaca","Issoria eugenia","Kirinia climene","Kirinia roxelana","Laeosopis roboris",
                      "Lasiommata deidamia","Lasiommata maera","Lasiommata petropolitana","Leptidea morsei","Libythea celtis",
                      "Limenitis camilla","Limenitis populi","Limenitis reducta","Lopinga achine","Melitaea aetherie",
                      "Neptis rivularis","Neptis sappho","Nymphalis antiopa","Nymphalis polychloros","Nymphalis vaualbum",
                      "Nymphalis xanthomelas","Pararge aegeria","Pararge xiphia","Pieris balcana","Polygonia c-album",
                      "Pseudochazara tisiphone","Satyrium acaciae","Satyrium ilicis","Satyrium pruni","Satyrium w-album",
                      "Thecla betulae","Tomares callimachus")

woodland_ind_species <- c("Apatura ilia","Apatura iris","Aporia crataegi","Araschnia levana",
                          "Argynnis pandora","Argynnis paphia","Boloria euphrosyne",
                          "Brintesia circe","Carterocephalus silvicola","Celastrina argiolus","Coenonympha arcania","Coenonympha hero",
                          "Erebia aethiops","Erebia ligea",
                          "Euphydryas maturna","Favonius quercus",
                          "Gonepteryx rhamni","Hipparchia fagi","Hipparchia hermione",
                          "Lasiommata maera","Lasiommata petropolitana","Libythea celtis",
                          "Limenitis camilla","Limenitis populi","Limenitis reducta",
                          "Nymphalis antiopa","Nymphalis polychloros",
                          "Pararge aegeria","Polygonia c-album",
                          "Satyrium acaciae","Satyrium ilicis","Satyrium w-album",
                          "Thecla betulae")

wetland_species <- c("Agriades optilete","Boloria aquilonaris","Boloria eunomia","Boloria freija","Boloria frigga",
                     "Coenonympha oedippus","Coenonympha tullia","Colias palaeno","Erebia disa","Erebia embla",
                     "Oeneis jutta","Pyrgus centaureae")

res_gam_butterfly_grassland <- res_gam_butterfly[which(res_gam_butterfly$species_name %in% grassland_species),]
res_gam_butterfly_woodland <- res_gam_butterfly[which(res_gam_butterfly$species_name %in% woodland_species),]
res_gam_butterfly_woodland_ind <- res_gam_butterfly[which(res_gam_butterfly$species_name %in% woodland_ind_species),]
res_gam_butterfly_wetland <- res_gam_butterfly[which(res_gam_butterfly$species_name %in% wetland_species),]


data_plot_r2 <- res_gamm_butterfly
data_plot_r2 <- res_gamm_butterfly_correct
data_plot_r2$PLS <- factor(data_plot_r2$PLS, levels = c(as.character(c(1:25)),"europe"))
ggplot(data_plot_r2, aes(x= PLS, y=dev_exp, fill=PLS)) + 
  geom_boxplot() + ylim(c(0,1)) + xlab("Biophysical region") + ylab("Explained deviance") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  stat_summary(fun.data = give.n, geom = "text", fun = median, 
               position = position_dodge(width = 0.75)) +
  theme_minimal() + theme(legend.position = "none")

ggsave("output/deviance_butterfly_before.png",
       width = 8,
       height = 4,
       dpi = 300)


table_species_butterfly <- data.frame(Species = unique(res_gamm_butterfly$species_name), Indicator = NA)
table_species_butterfly$Indicator[which(table_species_butterfly$Species %in% grassland_species)] <- "Grassland"
table_species_butterfly$Indicator[which(table_species_butterfly$Species %in% woodland_ind_species)] <- "Woodland"
table_species_butterfly$Indicator[which(table_species_butterfly$Species %in% wetland_species)] <- "Wetland"

write.csv(table_species_butterfly,"output/table_species_butterfly.csv", row.names = FALSE)

table_species_butterfly2 <- read.csv("output/table_species_butterfly2.csv", header=TRUE)
table_species_butterfly2$Habitat.indicator <- table_species_butterfly2$Indicator
table_species_butterfly2$Habitat.indicator[which(table_species_butterfly2$Habitat.indicator == "Wetland")] <- NA
table_species_butterfly2$All.butterfly.indicator <- table_species_butterfly2$Indicator
table_species_butterfly2$All.butterfly.indicator[which(table_species_butterfly2$All.butterfly.indicator %in% c("Wetland","Grassland","Woodland"))] <- "ABI"
table_species_butterfly2$Indicator <- NULL

pressure_EU_butterfly <- pressure_EU_butterfly[,c("species_name","milieu_catopenland","milieu_caturban","tempsrping","precspring","shannon","drymatter",
                                        "year:d_impervious","year:d_treedensity",
                                        "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri","year:eulandsystem_farmland_low",
                                        "year:eulandsystem_farmland_medium","year:eulandsystem_farmland_high",
                                        "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",         
                                        "year:d_shannon","year:protectedarea_perc","dev_exp")]
names(pressure_EU_butterfly) <- c("Species","open_vs_forest","urban_vs_forest",
                             "spring_mean_temperature","spring_rainfall","landscape_diversity",
                             "primary_productivity","urbanisation_increase_effect_on_trend","treedensity_increase_effect_on_trend",
                             "non_intensively_managed_forest_effect_on_trend","intensively_managed_forest_effect_on_trend","farmland_increase_effect_on_trend",
                             "low_intensively_managed_farmland_effect_on_trend","medium_intensively_managed_farmland_effect_on_trend","high_intensively_managed_farmland_effect_on_trend",
                             "temperature_increase_effect_on_trend","temperature_variation_increase_effect_on_trend","rainfall_increase_effect_on_trend",
                             "landscape_diversity_increase_effect_on_trend","protected_area_effect_on_trend","dev_exp")
table_species_butterfly2 <- merge(table_species_butterfly2,pressure_EU_butterfly, by="Species", all.x=TRUE)
write.csv(table_species_butterfly2,"output/table_species_butterfly3.csv", row.names = FALSE)


# boxplot r2 by pls

ggplot(res_gam_butterfly, aes(x= PLS, y=dev_exp, fill=PLS)) + 
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_minimal()


# other representation

pressure_EU_butterfly <- res_gam_butterfly[which(res_gam_butterfly$PLS=="europe"),]
pressure_EU_butterfly <- res_gamm_butterfly_correct[which(res_gamm_butterfly_correct$PLS=="europe"),]
pressure_EU_butterfly <- res_gamm_butterfly_correct[which(res_gamm_butterfly_correct$PLS=="europe" & res_gamm_butterfly_correct$species_name %in% grassland_species),]
pressure_EU_butterfly <- res_gamm_butterfly_correct[which(res_gamm_butterfly_correct$PLS=="europe" & res_gamm_butterfly_correct$species_name %in% woodland_ind_species),]
pressure_EU_butterfly_long <- reshape::melt(pressure_EU_butterfly, id.vars=c("species_name","PLS"))
pressure_EU_butterfly_long <- pressure_EU_butterfly_long[which(!pressure_EU_butterfly_long$variable %in% c("(Intercept)","PLS","dev_exp","n_obs")),]



ggplot(pressure_EU_butterfly_long[which(pressure_EU_butterfly_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                         "year:d_shannon","year:protectedarea_perc","year:d_treedensity:eulandsystem_forest_lowmedium","year:d_treedensity:eulandsystem_forest_high",
                                                                         "year:d_agri:eulandsystem_farmland_low","year:d_agri:eulandsystem_farmland_medium",
                                                                         "year:d_agri:eulandsystem_farmland_high","year:protectedarea_perc:protectedarea_type")),], aes(x = value, y = variable, fill = variable)) +
  scale_y_discrete(labels=c("year:d_impervious" = "D urbanisation on trend","year:d_tempsrping" = "D temperature on trend", "year:d_tempsrpingvar" = "D temperature variation on trend", "year:d_precspring" = "D precipitation on trend", "year:d_shannon" = "D landscape diversity on trend",              
                            "year:protectedarea_perc" = "Protected area percentage on trend", "year:d_treedensity:eulandsystem_forest_lowmedium" = "D tree density in low/medium intensive forests on trend", "year:d_treedensity:eulandsystem_forest_high" = "D tree density in high intensive forests on trend", "year:d_agri:eulandsystem_farmland_low" = "D agricultural surface in low intensive farmland on trend",             
                            "year:d_agri:eulandsystem_farmland_medium" = "D agricultural surface in medium intensive farmland on trend", "year:d_agri:eulandsystem_farmland_high" = "D agricultural surface in high intensive farmland on trend", "year:protectedarea_perc:protectedarea_type" = "Protected area type on trend")) + 
  geom_density_ridges(stat = "binline",
                      bins = 60, draw_baseline = FALSE) + xlim(c(-0.05,0.05))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75,
                      quantiles = 2) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  xlab("Pressures") + ylab("Estimate") +
  theme(legend.position = "none")

ggplot(pressure_EU_butterfly_long[which(pressure_EU_butterfly_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                                   "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                   "year:d_agri","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                   "year:eulandsystem_farmland_high")),], aes(x = value, y = variable, fill = variable)) +
  scale_y_discrete(labels=c("year:d_impervious" = "D urbanisation on trend","year:d_tempsrping" = "D temperature on trend", "year:d_tempsrpingvar" = "D temperature variation on trend", "year:d_precspring" = "D precipitation on trend", "year:d_shannon" = "D landscape diversity on trend",              
                            "year:protectedarea_perc" = "Protected area percentage on trend", "year:d_treedensity" = "D tree density on trend","year:eulandsystem_forest_lowmedium" = "Low/medium intensive forests on trend", "year:eulandsystem_forest_high" = "High intensive forests on trend",
                            "year:d_agri" = "D agricultural surface on trend","year:eulandsystem_farmland_low" = "Low intensive farmland on trend",
                            "year:eulandsystem_farmland_medium" = "Medium intensive farmland on trend", "year:eulandsystem_farmland_high" = "High intensive farmland on trend")) + 
  geom_density_ridges(stat = "binline",
                      bins = 60, draw_baseline = FALSE) + xlim(c(-1,1))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75,
                      quantiles = 2) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  xlab("Pressures") + ylab("Estimate") +
  theme(legend.position = "none")


pressure_EU_butterfly_long_d <- pressure_EU_butterfly_long[which(pressure_EU_butterfly_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                                             "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                             "year:d_agri","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                             "year:eulandsystem_farmland_high")),]

pressure_EU_butterfly_long_d$variable <- factor(pressure_EU_butterfly_long_d$variable , levels = c("year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_impervious",
                                                                                         "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                         "year:d_agri","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                         "year:eulandsystem_farmland_high"))

ggplot(pressure_EU_butterfly_long_d, aes(x = value, y = variable, fill = variable)) +
  scale_y_discrete(labels=c("year:d_impervious" = "D urbanisation on trend","year:d_tempsrping" = "D temperature on trend", "year:d_tempsrpingvar" = "D temperature variation on trend", "year:d_precspring" = "D precipitation on trend", "year:d_shannon" = "D landscape diversity on trend",              
                            "year:protectedarea_perc" = "Protected area percentage on trend", "year:d_treedensity" = "D tree density on trend","year:eulandsystem_forest_lowmedium" = "Low/medium intensive forests on trend", "year:eulandsystem_forest_high" = "High intensive forests on trend",
                            "year:d_agri" = "D agricultural surface on trend","year:eulandsystem_farmland_low" = "Low intensive farmland on trend",
                            "year:eulandsystem_farmland_medium" = "Medium intensive farmland on trend", "year:eulandsystem_farmland_high" = "High intensive farmland on trend")) + 
  geom_density_ridges(stat = "binline", col=NA,scale = 0.9,
                      bins = 60, draw_baseline = FALSE) + xlim(c(-1,1))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.2, scale = 0.9,
                      quantiles = 2) +
  scale_fill_manual(values = c("year:d_impervious"="#33a02c","year:d_tempsrping"="#1f78b4","year:d_tempsrpingvar"="#1f78b4","year:d_precspring"="#1f78b4",
                               "year:d_shannon"="#33a02c","year:protectedarea_perc"="#b2df8a","year:d_treedensity"="#33a02c","year:eulandsystem_forest_lowmedium"="#b2df8a","year:eulandsystem_forest_high"="#b2df8a",
                               "year:d_agri"="#33a02c","year:eulandsystem_farmland_low"="#b2df8a","year:eulandsystem_farmland_medium"="#b2df8a",
                               "year:eulandsystem_farmland_high"="#b2df8a")) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  theme(legend.position = "none", axis.title = element_blank())


ggsave("output/pressure_trend_butterfly_eu_hist.png",
       width = 6,
       height = 6,
       dpi = 300
)


pressure_EU_butterfly_long_s <- pressure_EU_butterfly_long[which(pressure_EU_butterfly_long$variable %in% c("milieu_catopenland","milieu_caturban","tempsrping",
                                                                                             "precspring","shannon","drymatter")),]

pressure_EU_butterfly_long_s$variable <- factor(pressure_EU_butterfly_long_s$variable , levels = c("tempsrping","precspring","milieu_catopenland",
                                                                                         "milieu_caturban","shannon","drymatter"))

ggplot(pressure_EU_butterfly_long_s, aes(x = value, y = variable, fill = variable)) +
  scale_y_discrete(labels=c("tempsrping" = "Temperature on abundance","precspring"= "Precipitation on abundance","milieu_catopenland" = "Openland vs forest on abundance",
                            "milieu_caturban" = "Urban vs forest on abundance","shannon" = "Landscape diversity on abundance","drymatter" = "Productivity on abundance")) + 
  geom_density_ridges(stat = "binline", col=NA,scale = 0.9,
                      bins = 60, draw_baseline = FALSE) + xlim(c(-5,5))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.2, scale = 0.9,
                      quantiles = 2) +
  scale_fill_manual(values = c("tempsrping"="#1f78b4","precspring"="#1f78b4","milieu_catopenland"="#33a02c","milieu_catothers"="#33a02c",
                               "milieu_caturban"="#33a02c","shannon"="#33a02c","drymatter"="#33a02c")) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  xlab("Pressures") + ylab("Estimate") +
  theme(legend.position = "none")

ggsave("output/pressure_state_butterfly_eu_hist.png",
       width = 6,
       height = 6,
       dpi = 300
)



matrix_pressure_PLS <- data.frame(res_gamm_butterfly_correct %>% group_by(PLS) %>% summarise(nb_sp_neg_lulc = length(which(`year:d_impervious` < 0 | `year:d_shannon` < 0 | `year:d_treedensity` <0 |`year:d_agri` < 0)),
                                                                                             nb_sp_neg_lulc_int = length(which(`year:eulandsystem_farmland_high` < 0 | `year:eulandsystem_forest_high` < 0)),
                                                                                             nb_sp_neg_climate = length(which(`year:d_tempsrping` < 0 | `year:d_tempsrpingvar` < 0 | `year:d_precspring` < 0)),
                                                                                             #max_effect = ifelse(nb_sp_neg_lulc > nb_sp_neg_climate, "lulc", "climate"),
                                                                                             nb_sp_neg = length(which(year < 0)),
                                                                                             nb_sp_pos = length(which(year > 0)),
                                                                                             nb_sp = n(),
                                                                                             perc_sp_neg_lulc = nb_sp_neg_lulc/nb_sp,
                                                                                             perc_sp_neg_lulc_int = nb_sp_neg_lulc_int/nb_sp,
                                                                                             perc_sp_neg_climate = nb_sp_neg_climate/nb_sp))
                                                                                             #max_effect_percent = ifelse(nb_sp_neg_lulc > nb_sp_neg_climate, nb_sp_neg_lulc/nb_sp, nb_sp_neg_climate/nb_sp),
                                                                                             #min_effect_percent = ifelse(nb_sp_neg_lulc < nb_sp_neg_climate, nb_sp_neg_lulc/nb_sp, nb_sp_neg_climate/nb_sp)))


matrix_pressure_PLS_sf <- merge(grid_eu_mainland_biogeo,matrix_pressure_PLS,by="PLS",all.x=TRUE)
ggplot(grid_eu_mainland_outline) + geom_sf(fill=NA) +  
  geom_sf(data=matrix_pressure_PLS_sf, aes(fill=perc_sp_neg_lulc), col=NA) + 
  scale_fill_gradient(low= "white",high = "#33a02c") +
  theme_void() + theme(legend.title = element_blank())
ggplot(grid_eu_mainland_outline) + geom_sf(fill=NA) +  
  geom_sf(data=matrix_pressure_PLS_sf, aes(fill=perc_sp_neg_lulc_int), col=NA) + 
  scale_fill_gradient(low= "white",high = "#b2df8a") +
  theme_void() + theme(legend.title = element_blank())
ggplot(grid_eu_mainland_outline) + geom_sf(fill=NA) +  
  geom_sf(data=matrix_pressure_PLS_sf, aes(fill=perc_sp_neg_climate), col=NA) + 
  scale_fill_gradient(low= "white",high = "#1f78b4") +
  theme_void() + theme(legend.title = element_blank())
ggplot(grid_eu_mainland_outline) + geom_sf(fill=NA) +  
  geom_sf(data=matrix_pressure_PLS_sf, aes(fill=nb_sp), col=NA) + 
  scale_fill_gradient(low= "white",high = "#fb9a99") +
  theme_void() + theme(legend.title = element_blank())

ggsave("output/main_pressure_neg_butterfly_lulc.png",
       width = 8,
       height = 8,
       dpi = 300
)

