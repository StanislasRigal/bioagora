# Load pressure data

grid_eu <- st_read("output/grid_eu_gdp.gpkg")
grid_eu_bioregion <- st_read("output/grid_eu_bioregion.gpkg")
grid_eu$biogeo_area <- grid_eu_bioregion$biogeo_area

st_write(grid_eu,"output/grid_eu_all.gpkg")

grid_eu_all <- st_read("output/grid_eu_all.gpkg")

grid_eu_all$diff_pop_perc <- NULL
grid_eu_all$diff_impervious <- NULL
grid_eu_all$diff_treedensity <- NULL
grid_eu_all$diff_lightpollution <- NULL
grid_eu_all$diff_woodprod <- NULL
grid_eu_all$diff_drymatter <- NULL
grid_eu_all$CNTR_ID <- NULL
grid_eu_all$NUTS2016_3 <- NULL
grid_eu_all$NUTS2016_2 <- NULL
grid_eu_all$NUTS2016_1 <- NULL
grid_eu_all$NUTS2016_0 <- NULL

grid_eu_all$PLS <- column_to_add

st_write(grid_eu_all,"output/grid_eu_all_clean.gpkg")
grid_eu_all <- st_read("output/grid_eu_all_clean.gpkg")


bird_data_clean <- readRDS("output/bird_data_clean.rds")

# Test with subregion (e.g. Catalonia)

## grid data for Catalonia

grid_eu_cat <- grid_eu_all[grep("ES51",grid_eu_all$NUTS2021_2),]

st_write(grid_eu_cat,"output/grid_eu_cat_test.gpkg")
grid_eu_cat <- st_read("output/grid_eu_cat_test.gpkg")

## site and bird data from the Catalonian survey

sites <- read.table(file = "raw_data/pecbms_bird_data/sites.txt", header = TRUE, sep = "\t")

bird_data_cat <- bird_data_clean[which(bird_data_clean$scheme_code == "ES_CAT"),]

site_cat <- sites[which(sites$siteID %in% unique(bird_data_cat$siteID)),]

site_cat_sf <- st_as_sf(site_cat, coords = c("Long_WGS84","Lat_WGS84"))

st_crs(site_cat_sf) <- 4326

site_cat_sf_reproj <- st_transform(site_cat_sf,crs(grid_eu_cat))

## site and bird data from the Spanish survey in Catalonia

bird_data_esp <- bird_data_clean[which(bird_data_clean$scheme_code %in% c("ES")),]

site_esp <- sites[which(sites$siteID %in% unique(bird_data_esp$siteID)),]

site_esp_sf <- st_as_sf(site_esp, coords = c("Long_WGS84","Lat_WGS84"))
st_crs(site_esp_sf) <- 4326

site_esp_sf_reproj <- st_transform(site_esp_sf,crs(grid_eu_cat))

site_esp_sf_reproj <- st_intersection(site_esp_sf_reproj,grid_eu_cat)

## merge both and get bird data

site_cat_sf_all <- rbind(site_cat_sf_reproj, site_esp_sf_reproj[,c(1:(ncol(site_cat_sf_reproj)-1),ncol(site_esp_sf_reproj))])

bird_data_cat_all <- bird_data_clean[which(bird_data_clean$siteID %in% unique(site_cat_sf_all$siteID)),]

site_cat_all <- sites[which(sites$siteID %in% unique(bird_data_cat_all$siteID)),]

site_cat_all$Long_LAEA <- st_coordinates(site_cat_sf_all)[,1]
site_cat_all$Lat_LAEA <- st_coordinates(site_cat_sf_all)[,2]

## plot sites and area monitored

ggplot(grid_eu_cat) +
  geom_sf() +
  geom_sf(data=site_cat_sf_all)

site_cat_buffer <- st_buffer(site_cat_sf_all, dist = sqrt(site_cat_sf_all$area_sampled_m2/pi))

ggplot(grid_eu_cat) +
  geom_sf() +
  geom_sf(data=site_cat_buffer)

area_site_cat <-  st_intersection(site_cat_buffer, grid_eu_cat)

ggplot(area_site_cat) +
  geom_sf() 

area_site_cat$area <- as.numeric(st_area(area_site_cat))

## plot a zoom

crop_factor <- st_bbox(c(xmin = 1.5, 
                         xmax = 2.5, 
                         ymax = 41, 
                         ymin = 42),
                       crs = st_crs(site_esp_sf))

grid_eu_cat_cropped <- st_transform(grid_eu_cat,st_crs(crop_factor))
grid_eu_cat_cropped <- st_crop(grid_eu_cat_cropped, crop_factor)
site_cat_sf_all_cropped <- st_transform(site_cat_sf_all,st_crs(crop_factor))
site_cat_sf_all_cropped <- st_crop(site_cat_sf_all_cropped, crop_factor)

ggplot(grid_eu_cat_cropped) +
  geom_sf() +
  geom_sf(data=site_cat_sf_all_cropped)

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=impervious2006)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=treedensity2012)) +
  scale_fill_viridis_b()

grid_eu_cat_cropped$eulandsystem_cat <- NA
grid_eu_cat_cropped$eulandsystem_cat[which(grid_eu_cat_cropped$eulandsystem %in% c(11,12,13,80,90))] <-"no_intensity"
grid_eu_cat_cropped$eulandsystem_cat[which(grid_eu_cat_cropped$eulandsystem %in% c(21,31,41,51,61,731,71:75))] <-"low_intensity"
grid_eu_cat_cropped$eulandsystem_cat[which(grid_eu_cat_cropped$eulandsystem %in% c(22,32,42,52,62,732))] <-"medium_intensity"
grid_eu_cat_cropped$eulandsystem_cat[which(grid_eu_cat_cropped$eulandsystem %in% c(23,43,53,63,733))] <-"high_intensity"
grid_eu_cat_cropped$eulandsystem_cat <- factor(grid_eu_cat_cropped$eulandsystem_cat, levels = c("no_intensity","low_intensity","medium_intensity","high_intensity")) 
grid_eu_cat_cropped$eulandsystem <- NULL

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=eulandsystem_cat)) +
  scale_fill_viridis_d()

ggsave(
  "output/euland.png",
  width = 5,
  height = 5,
  dpi = 300,
)

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=lightpollution2000)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=pesticide_nodu_kg)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=tempspring2000)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=precspring2000)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=shannon)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=GDP2015)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=pop2020)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=protectedarea)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=woodprodaverage)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=drymatter2018)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=smallwoodyfeatures)) +
  scale_fill_viridis_b()

ggplot(grid_eu_cat_cropped) +
  geom_sf(aes(fill=fragmentation)) +
  scale_fill_viridis_b()

area_site_cat_cropped <- st_transform(area_site_cat,st_crs(crop_factor))
area_site_cat_cropped <- st_crop(area_site_cat_cropped, crop_factor)

ggplot() +
  geom_sf(data =area_site_cat_cropped)




## summarize external variable for each site and format them

value_site_cat <- as.data.frame(area_site_cat) %>% group_by(siteID) %>% summarize(pop2000 = weighted.mean(pop2000,area), pop2020 = weighted.mean(pop2020,area),
                                                                                  impervious2006 = weighted.mean(impervious2006,area), impervious2018 = weighted.mean(impervious2018,area),
                                                                                  treedensity2012 = weighted.mean(treedensity2012,area), treedensity2018 = weighted.mean(treedensity2018,area), 
                                                                                  eulandsystem = eulandsystem[which.max(area)], protectedarea = protectedarea[which.max(area)],
                                                                                  lightpollution2000 = weighted.mean(lightpollution2000,area), lightpollution2013 = weighted.mean(lightpollution2013,area), 
                                                                                  pesticide_kg = weighted.mean(pesticide_kg,area), pesticide_kg_ha = weighted.mean(pesticide_kg_ha,area), pesticide_nodu_kg = weighted.mean(pesticide_nodu_kg,area), 
                                                                                  woodprod2000 = weighted.mean(woodprod2000,area), woodprod2010 = weighted.mean(woodprod2010,area), woodprodaverage = weighted.mean(woodprodaverage,area),
                                                                                  drymatter2000 = weighted.mean(drymatter2000,area), drymatter2018 = weighted.mean(drymatter2018,area), declineproductivity = weighted.mean(declineproductivity,area),
                                                                                  smallwoodyfeatures = weighted.mean(smallwoodyfeatures,area), fragmentation = weighted.mean(fragmentation,area),
                                                                                  forestintegrity = weighted.mean(forestintegrity,area), forestintegrity_cat = forestintegrity_cat[which.max(area)],
                                                                                  temp2000 = weighted.mean(temp2000,area), temp2020 = weighted.mean(temp2020,area), tempspring2000 = weighted.mean(tempspring2000,area), tempspring2020 = weighted.mean(tempspring2020,area),
                                                                                  tempspringvar2000 = weighted.mean(tempspringvar2000,area), tempspringvar2020 = weighted.mean(tempspringvar2020,area),
                                                                                  prec2000 = weighted.mean(prec2000,area), prec2020 = weighted.mean(prec2020,area), precspring2000 = weighted.mean(precspring2000,area), precspring2020 = weighted.mean(precspring2020,area),
                                                                                  precspringvar2000 = weighted.mean(precspringvar2000), precspringvar2020 = weighted.mean(precspringvar2020,area),
                                                                                  humidity2000 = weighted.mean(humidity2000,area), humidity2020 = weighted.mean(humidity2020,area), humidityspring2000 = weighted.mean(humidityspring2000,area),
                                                                                  humidityspring2020 = weighted.mean(humidityspring2020,area), humidityspringvar2000 = weighted.mean(humidityspringvar2000,area), humidityspringvar2020 = weighted.mean(humidityspringvar2020,area),
                                                                                  shannon = weighted.mean(shannon,area), GDP2000 = weighted.mean(GDP2000,area), GDP2015 = weighted.mean(GDP2015,area), biogeo_area = biogeo_area[which.max(area)])

#value_site_cat$diff_pop_perc <- (value_site_cat$pop2020-value_site_cat$pop2000)/value_site_cat$pop2000
#value_site_cat$diff_impervious <- (value_site_cat$impervious2018-value_site_cat$impervious2006)/value_site_cat$impervious2006
#value_site_cat$diff_treedensity <- (value_site_cat$treedensity2018-value_site_cat$treedensity2012)/value_site_cat$treedensity2012
#value_site_cat$diff_lightpollution <- (value_site_cat$lightpollution2013-value_site_cat$lightpollution2000)/value_site_cat$lightpollution2000
#value_site_cat$diff_woodprod <- (value_site_cat$woodprod2010-value_site_cat$woodprod2000)/value_site_cat$woodprod2000
#value_site_cat$diff_drymatter <- (value_site_cat$drymatter2018-value_site_cat$drymatter2000)/value_site_cat$drymatter2000
value_site_cat$GDP2000_percap <- value_site_cat$GDP2000/value_site_cat$pop2000
pop2015 <- (value_site_cat$pop2020-value_site_cat$pop2000)/20*(2015-2020)+value_site_cat$pop2020
value_site_cat$GDP2015_percap <- value_site_cat$GDP2015/pop2015
#value_site_cat$diff_gdp_percap <- (value_site_cat$GDP2015_percap-value_site_cat$GDP2000_percap)/value_site_cat$GDP2000_percap

value_site_cat$protectedarea[which(is.na(value_site_cat$protectedarea))] <- 0
value_site_cat$protectedarea[which(value_site_cat$protectedarea>0)] <- 1
value_site_cat$eulandsystem_cat <- NA
value_site_cat$eulandsystem_cat[which(value_site_cat$eulandsystem %in% c(11,12,13,80,90))] <-"no_intensity"
value_site_cat$eulandsystem_cat[which(value_site_cat$eulandsystem %in% c(21,31,41,51,61,731,71:75))] <-"low_intensity"
value_site_cat$eulandsystem_cat[which(value_site_cat$eulandsystem %in% c(22,32,42,52,62,732))] <-"medium_intensity"
value_site_cat$eulandsystem_cat[which(value_site_cat$eulandsystem %in% c(23,43,53,63,733))] <-"high_intensity"
value_site_cat$eulandsystem_cat <- factor(value_site_cat$eulandsystem_cat, levels = c("no_intensity","low_intensity","medium_intensity","high_intensity")) 
value_site_cat$eulandsystem <- NULL


## add zero when species no present at monitored site

wide_bird_data <- data.frame(bird_data_cat_all[,c("siteID","year","sci_name_out","count")] %>% group_by(siteID) %>% tidyr::complete(year,sci_name_out))
wide_bird_data$count[which(is.na(wide_bird_data$count))] <- 0

bird_data_cat <- merge(wide_bird_data,site_cat_all,by=c("siteID"), all.x=T)

bird_data_cat <- merge(bird_data_cat,value_site_cat, by="siteID", all.x=TRUE)


### test with the most commun species

fricoe_data_cat <- droplevels(bird_data_cat[which(bird_data_cat$sci_name_out=="Fringilla coelebs"),])

### compute trend species

nb_year_p_site <- data.frame(fricoe_data_cat %>% group_by(siteID) %>% summarise(nb_year=n(),
                                                                                     min_year=min(year),
                                                                                     max_year=max(year)))

selected_site_trend <- nb_year_p_site[which(nb_year_p_site$nb_year > 4 &
                                            nb_year_p_site$min_year < 2006 &
                                            nb_year_p_site$max_year > 2014),]

fricoe_data_cat_trend <- fricoe_data_cat[which(fricoe_data_cat$siteID %in% c(selected_site_trend$siteID)),]

fricoe_cat_trend <- ddply(fricoe_data_cat_trend, .(siteID),
                          .fun = function(x){
                            mod <- glm(count~year, data=x, family = "poisson")
                            mod_result <- as.data.frame(t(summary(mod)$coef[2,]))
                            mod_result$mean_ab <- mean(x$count)
                            return(mod_result)
                            },
                          .progress = "text")

### compute pressure yearly estimate


fricoe_press_cat_trend <- ddply(fricoe_data_cat_trend, .(siteID,year),
                          .fun = function(x){
                            pop <- (x$pop2020-x$pop2000)/21*(x$year-2000)+x$pop2000
                            impervious <- (x$impervious2018-x$impervious2006)/13*(x$year-2006)+x$impervious2006
                            treedensity <- (x$treedensity2018-x$treedensity2012)/7*(x$year-2012)+x$treedensity2012
                            lightpollution <- (x$lightpollution2013-x$lightpollution2000)/14*(x$year-2000)+x$lightpollution2000
                            woodprod <- (x$woodprod2010-x$woodprod2000)/11*(x$year-2000)+x$woodprod2000
                            drymatter <- (x$drymatter2018-x$drymatter2000)/19*(x$year-2000)+x$drymatter2000
                            temp <- (x$temp2020-x$temp2000)/21*(x$year-2000)+x$temp2000
                            tempspring <- (x$tempspring2020-x$tempspring2000)/21*(x$year-2000)+x$tempspring2000
                            tempspringvar <- (x$tempspringvar2020-x$tempspringvar2000)/21*(x$year-2000)+x$tempspringvar2000
                            prec <- (x$prec2020-x$prec2000)/21*(x$year-2000)+x$prec2000
                            precspring <- (x$precspring2020-x$precspring2000)/21*(x$year-2000)+x$precspring2000
                            precspringvar <- (x$precspringvar2020-x$precspringvar2000)/21*(x$year-2000)+x$precspringvar2000
                            GDP_percap <- (x$GDP2015_percap-x$GDP2000_percap)/16*(x$year-2000)+x$GDP2000_percap
                            GDP <- (x$GDP2015-x$GDP2000)/16*(x$year-2000)+x$GDP2000
                            trend_result <- data.frame(pop,impervious,treedensity,lightpollution,woodprod,drymatter,
                                                        temp,tempspring,tempspringvar,prec,precspring,precspringvar,
                                                        GDP_percap,GDP)
                            return(trend_result)
                          },
                          .progress = "text")

fricoe_press_cat_trend_scale <- fricoe_press_cat_trend

fricoe_press_cat_trend_scale[,c("pop","impervious","treedensity","lightpollution",
                          "woodprod","drymatter","temp","tempspring","tempspringvar","prec",        
                          "precspring","precspringvar","GDP_percap","GDP")] <- scale(fricoe_press_cat_trend_scale[,c("pop","impervious","treedensity","lightpollution",
                                                                                                                     "woodprod","drymatter","temp","tempspring","tempspringvar","prec",        
                                                                                                                     "precspring","precspringvar","GDP_percap","GDP")])

fricoe_press_data_year <- merge(fricoe_data_cat_trend[,c("siteID","year","sci_name_out","count",              
                                                         "scheme","scheme_code","Long_WGS84","Lat_WGS84",            
                                                         "plot_size_ha","num_points","transect_length_m","time_effort",
                                                         "area_sampled_m2","country","method","num_visits",
                                                         "count_unit","count_aggregation","species","Long_LAEA","Lat_LAEA")],
                                fricoe_press_cat_trend_scale, by =c("siteID","year"))

# First option: two steps framework (trend and MGWR)

### prepare data for Multiscale gwr

fricoe_cat_trend <- merge(fricoe_cat_trend,site_cat_all,by=c("siteID"), all.x=TRUE)
fricoe_cat_trend <- merge(fricoe_cat_trend,value_site_cat, by="siteID", all.x=TRUE)

fricoe_cat_trend <- fricoe_cat_trend[which(abs(fricoe_cat_trend$Estimate) > fricoe_cat_trend$`Std. Error`),]


fricoe_cat_data_analysis <- fricoe_cat_trend[,c("Estimate","Std. Error","mean_ab","pop2000","pop2020",
                                                "impervious2006","impervious2018","treedensity2012","treedensity2018",
                                                "eulandsystem_cat","protectedarea","lightpollution2000","lightpollution2013",
                                                "pesticide_nodu_kg","woodprod2000","woodprod2010","drymatter2000","drymatter2018",
                                                "smallwoodyfeatures","fragmentation","forestintegrity_cat",
                                                "tempspring2000","tempspring2020","tempspringmaxvar2000","tempspringmaxvar2000",
                                                "precspring2000","precspring2020","precspringvar2000","precspringvar2020",
                                                "shannon",'scheme_code',
                                                "Long_WGS84", "Lat_WGS84")]


fricoe_cat_data_analysis[,c("pop2000","impervious2006","treedensity2012","lightpollution2000",
                            "pesticide_nodu_kg","woodprod2000","drymatter2000",
                            "smallwoodyfeatures","fragmentation",
                            "tempspring2000","tempspringmaxvar2000","precspring2000",
                            "precspringvar2000","shannon")] <- scale(fricoe_cat_data_analysis[,c("pop2000","impervious2006","treedensity2012","lightpollution2000",
                                                                                                                "pesticide_nodu_kg","woodprod2000","drymatter2000",
                                                                                                                "smallwoodyfeatures","fragmentation",
                                                                                                                "tempspring2000","tempspringmaxvar2000","precspring2000",
                                                                                                                "precspringvar2000","shannon")])

fricoe_cat_trend_sp <- st_as_sf(na.omit(fricoe_cat_data_analysis), coords = c("Long_WGS84", "Lat_WGS84"), crs = 4326) 

### run MSGWR

gw.ms <- gwr.multiscale(Estimate ~ mean_ab + pop2000 + impervious2006 + treedensity2012 +
                        eulandsystem_cat + protectedarea + lightpollution2000 +
                        pesticide_nodu_kg + woodprod2000 + drymatter2000 +
                        smallwoodyfeatures + fragmentation + forestintegrity_cat +
                        tempspring2000 + tempspringmaxvar2000 + precspring2000 +
                        precspringvar2000 + shannon + scheme_code, 
                        data = as(fricoe_cat_trend_sp, "Spatial"),
                        adaptive = T, max.iterations = 1000,
                        predictor.centered = rep(TRUE,(ncol(fricoe_cat_data_analysis)-3)),
                        criterion="CVR",
                        kernel = "bisquare",
                        bws0 = rep(100,(ncol(fricoe_cat_data_analysis)-3)),
                        verbose = F)
gw.ms$GW.arguments$bws
coefs_msgwr <- apply(gw.ms$SDF@data[, 1:(ncol(fricoe_cat_data_analysis))], 2, summary)
round(coefs_msgwr,1)
mgwr_sf <- st_as_sf(gw.ms$SDF)
tm_shape(mgwr_sf) + tm_bubbles("woodprod2000",col="woodprod2000")+
  tm_layout(legend.position = c("right","top"), frame = F)


## expend trend analysis for all species

### compute trend species

sub_bird_data <- droplevels(bird_data_cat[which(bird_data_cat$sci_name_out=="Fringilla coelebs"),])


nb_year_p_site <- data.frame(bird_data_cat %>% group_by(siteID, year) %>% summarise(count=n()))
nb_year_p_site <- data.frame(nb_year_p_site %>% group_by(siteID) %>% summarise(nb_year=n(),
                                                            min_year=min(year),
                                                            max_year=max(year)))

selected_site_trend <- nb_year_p_site[which(nb_year_p_site$nb_year > 4 &
                                              nb_year_p_site$min_year < 2006 &
                                              nb_year_p_site$max_year > 2014),]

bird_data_cat_trend <- bird_data_cat[which(bird_data_cat$siteID %in% c(selected_site_trend$siteID)),]

get_species_trend <- function(sub_bird_data){
  mod <- glm(count~year, data=sub_bird_data, family = "poisson")
  mod_result <- as.data.frame(t(summary(mod)$coef[2,]))
  mod_result$mean_ab <- mean(sub_bird_data$count)
  return(mod_result)
}

bird_cat_trend <- ddply(bird_data_cat_trend, .(sci_name_out, siteID),
                        .fun = get_species_trend,
                        .progress = "text")

### remove outlier trends

bird_cat_trend_clean <- bird_cat_trend[which(abs(bird_cat_trend$Estimate) > bird_cat_trend$`Std. Error`),]

### prepare and format data for multiscale gwr

bird_data_mgwr_cat <- merge(bird_cat_trend_clean,site_cat_all,by=c("siteID"), all.x=TRUE)
bird_data_mgwr_cat <- merge(bird_data_mgwr_cat,value_site_cat, by="siteID", all.x=TRUE)

bird_data_mgwr_cat <- bird_data_mgwr_cat[,c("sci_name_out","Estimate","mean_ab","pop2000","impervious2006","treedensity2012",
                                                "eulandsystem_cat","protectedarea","lightpollution2000",
                                                "pesticide_nodu_kg","woodprod2000","drymatter2000",
                                                "smallwoodyfeatures","fragmentation","forestintegrity_cat",
                                                "tempspring2000","tempspringmaxvar2000","precspring2000",
                                                "precspringvar2000","humidity2000",
                                                "shannon",'scheme_code',
                                                "Long_WGS84", "Lat_WGS84")]



bird_data_mgwr_cat[,c("pop2000","impervious2006","treedensity2012","lightpollution2000",
                            "pesticide_nodu_kg","woodprod2000","drymatter2000",
                            "smallwoodyfeatures","fragmentation",
                            "tempspring2000","tempspringmaxvar2000","precspring2000",
                            "precspringvar2000","humidity2000","shannon")] <- scale(bird_data_mgwr_cat[,c("pop2000","impervious2006","treedensity2012","lightpollution2000",
                                                                                                                "pesticide_nodu_kg","woodprod2000","drymatter2000",
                                                                                                                "smallwoodyfeatures","fragmentation",
                                                                                                                "tempspring2000","tempspringmaxvar2000","precspring2000",
                                                                                                                "precspringvar2000","humidity2000","shannon")])
### run MGWR per species

mgwr_bird <- function(sub_bird_data_mgwr){
  
  sub_bird_data_mgwr <- na.omit(sub_bird_data_mgwr)
  
  if(nrow(sub_bird_data_mgwr) > 40){ # 20 variables so a minimum of data is required
    sub_bird_data_mgwr_sp <- st_as_sf(sub_bird_data_mgwr, coords = c("Long_WGS84", "Lat_WGS84"), crs = 4326) 
    
    gw.ms <- gwr.multiscale(Estimate ~ mean_ab + pop2000 + impervious2006 + treedensity2012 +
                              eulandsystem_cat + protectedarea + lightpollution2000 +
                              pesticide_nodu_kg + woodprod2000 + drymatter2000 +
                              smallwoodyfeatures + fragmentation + forestintegrity_cat +
                              tempspring2000 + tempspringmaxvar2000 + precspring2000 +
                              precspringvar2000 + humidity2000 + shannon + scheme_code, 
                            data = as(sub_bird_data_mgwr_sp, "Spatial"),
                            adaptive = T, max.iterations = 1000,
                            predictor.centered = rep(TRUE,(ncol(sub_bird_data_mgwr_sp)-3)),
                            criterion="CVR",
                            kernel = "bisquare",
                            bws0 = rep(100,(ncol(sub_bird_data_mgwr_sp)-3)),
                            verbose = F)
    gw.ms$GW.arguments$bws
    coefs_msgwr <- apply(gw.ms$SDF@data[, 1:(ncol(sub_bird_data_mgwr_sp))], 2, summary)
    res_data <- data.frame(t(coefs_msgwr[3,]))
  }else{
    res_data <- data.frame(Intercept=NA,mean_ab=NA,pop2000=NA,impervious2006=NA,
                       treedensity2012=NA,eulandsystem_catlow_intensity=NA,eulandsystem_catmedium_intensity=NA,eulandsystem_cathigh_intensity=NA,
                       protectedarea=NA,lightpollution2000=NA,pesticide_nodu_kg=NA,woodprod2000=NA,
                       drymatter2000=NA,smallwoodyfeatures=NA,fragmentation=NA,forestintegrity_catmedium=NA,
                       tempspring2000=NA,tempspringmaxvar2000=NA,precspring2000=NA,precspringvar2000=NA,
                       humidity2000=NA,shannon=NA,scheme_codeES_CAT=NA)
    }
  
  return(res_data)
}

bird_cat_mgwr <- ddply(bird_data_mgwr_cat, .(sci_name_out),
                        .fun = mgwr_bird,
                        .progress = "text")

test <- ddply(bird_data_mgwr_cat[which(bird_data_mgwr_cat$sci_name_out %in%
                                         unique(bird_data_mgwr_cat$sci_name_out)[36]),], .(sci_name_out),
              .fun = mgwr_bird,
              .progress = "text")


# Second option: 1 step framework (GWPR)

## Get the number of sites as a function of bandwidth

data_mat_dist <- SpatialPoints(coords = as.matrix(site_cat_all[,c("Long_LAEA","Lat_LAEA")]), proj4string = CRS(crs(site_cat_sf_all)))
DM_cat <- gw.dist(dp.locat=coordinates(data_mat_dist))

nb_site_window_fun <- function(x,radius){
  nb_site <- length(which(x < radius))
  return(nb_site)
}

for(i in seq(10000,100000,by=1000)){
  nb_site_temporary <- data.frame(apply(DM_cat,1,nb_site_window_fun,radius=i))
  names(nb_site_temporary) <- paste0("b",i)
  if(i == 10000){
    nb_site_window <- nb_site_temporary
  }else{
    nb_site_window <- cbind(nb_site_window,nb_site_temporary)
  }
}

apply(nb_site_window,2,min)


## test with GWPoissonR, cf roadmap doi: 10.1111/gean.12316

library(GWmodel)

test_poisson_df <- na.omit(fricoe_press_data_year[,c("count","year","scheme_code","Long_LAEA","Lat_LAEA","pop","impervious","treedensity","lightpollution",
                                                     "woodprod","drymatter","temp","tempspring","tempspringvar",  
                                                     "prec","precspring","precspringvar","GDP_percap","GDP")])

### global poisson model

global_mod <- glm(count~scheme_code+year:treedensity+year:impervious+year:pop+year:lightpollution+
                   year:woodprod+year:drymatter+year:tempspring+year:tempspringvar+
                   year:precspring+year:precspringvar+year:GDP_percap, family="poisson", data=test_poisson_df)

### autocorrelation of residuals

test_poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(test_poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = test_poisson_df,
                                          proj4string = CRS(crs(site_cat_sf_all)))

library(spdep)
nb <- dnearneigh(test_poisson_sf@coords, 10000,50000)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
moran_I <- lm.morantest(global_mod,lw)
c(unlist(moran_I[3])[1],unlist(moran_I[2]) ) # autocorrelated so woth doing a GWR


### GWPR

DM <- gw.dist(dp.locat=coordinates(test_poisson_sf))
#bw.f1 <- bw.ggwr(count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:GDP_percap,data=test_poisson_sf, dMat=DM,kernel = "gaussian")
bw.f1 <- 51000 # based on apply(nb_site_window,2,min) > 30
res.poisson <- ggwr.basic(count~scheme_code+year:treedensity+year:impervious+year:pop+year:lightpollution+
                            year:woodprod+year:drymatter+year:tempspring+year:tempspringvar+
                            year:precspring+year:precspringvar+year:GDP_percap, bw=bw.f1,
                          data=test_poisson_sf, dMat=DM, kernel = "gaussian")

res_plot <- st_as_sf(res.poisson$SDF)
ggplot(grid_eu_cat) +
  geom_sf() +
  geom_sf(data=res_plot, aes(col=exp(`year:treedensity`)))+
  scale_color_gradientn(colors = sf.colors(20))




#######


# Moving toward the analysis for all Europe

## test for Spain and France

### grid data for Spain and France

grid_eu_spafra <- grid_eu_all[grep("ES|FR",grid_eu_all$NUTS2021_0),]
grid_eu_spafra <- grid_eu_spafra[grep("ES7|FRY",grid_eu_spafra$NUTS2021_1,invert = TRUE),] # remove oversea territories

st_write(grid_eu_spafra,"output/grid_eu_spafra_test.gpkg")
grid_eu_spafra <- st_read("output/grid_eu_spafra_test.gpkg")

grid_eu_spafra_outline <- grid_eu_spafra %>% summarise(id="europe") # or grid_eu_spafra_outline <- st_union(grid_eu_spafra)
grid_eu_spafra_biogeo <- grid_eu_spafra %>% group_by(biogeo_area) %>% summarise(id="biogeo") 

bird_data_clean <- readRDS("output/bird_data_clean.rds")

## site and bird data from the Catalonian survey

sites <- read.table(file = "raw_data/pecbms_bird_data/sites.txt", header = TRUE, sep = "\t")

bird_data_spafra <- bird_data_clean[which(bird_data_clean$scheme_code %in% c("ES","FR","ES_CAT")),]

site_spafra <- sites[which(sites$siteID %in% unique(bird_data_spafra$siteID) & sites$Long_WGS84 > -10 ),] # remove canary islands

bird_data_spafra <- bird_data_spafra[which(bird_data_spafra$siteID %in% unique(site_spafra$siteID)),]

site_spafra_sf <- st_as_sf(site_spafra, coords = c("Long_WGS84","Lat_WGS84"))

st_crs(site_spafra_sf) <- 4326

site_spafra_sf_reproj <- st_transform(site_spafra_sf,crs(grid_eu_spafra))

site_spafra$Long_LAEA <- st_coordinates(site_spafra_sf_reproj)[,1]
site_spafra$Lat_LAEA <- st_coordinates(site_spafra_sf_reproj)[,2]

## plot sites and area monitored

ggplot(grid_eu_spafra_outline) +
  geom_sf() +
  geom_sf(data=site_spafra_sf_reproj)

site_spafra_buffer <- st_buffer(site_spafra_sf_reproj, dist = sqrt(site_spafra_sf_reproj$area_sampled_m2/pi))

area_site_spafra <-  st_intersection(site_spafra_buffer, grid_eu_spafra)

area_site_spafra$area <- as.numeric(st_area(area_site_spafra))

## plot a zoom

crop_factor <- st_bbox(c(xmin = 4.5, #0
                         xmax = 5.3, #3
                         ymax = 46.1, #44
                         ymin = 45.3), #41
                       crs = st_crs(site_spafra_sf))

grid_eu_spafra_cropped <- st_transform(grid_eu_spafra,st_crs(crop_factor))
grid_eu_spafra_cropped <- st_crop(grid_eu_spafra_cropped, crop_factor)
site_spafra_cropped <- st_transform(site_spafra_sf_reproj,st_crs(crop_factor))
site_spafra_cropped <- st_crop(site_spafra_cropped, crop_factor)

ggplot(grid_eu_spafra_cropped) +
  geom_sf() +
  geom_sf(data=site_spafra_cropped)

ggplot(grid_eu_spafra_cropped) +
  geom_sf(aes(fill=impervious2006),col=NA) +
  scale_fill_viridis_b() + theme_minimal()

ggplot(grid_eu_spafra_cropped) +
  geom_sf(aes(fill=treedensity2012),col=NA) +
  scale_fill_viridis_b() + theme_minimal()

grid_eu_spafra_cropped$eulandsystem_spafra <- NA
grid_eu_spafra_cropped$eulandsystem_spafra[which(grid_eu_spafra_cropped$eulandsystem %in% c(11,12,13,80,90))] <-"no_intensity"
grid_eu_spafra_cropped$eulandsystem_spafra[which(grid_eu_spafra_cropped$eulandsystem %in% c(21,31,41,51,61,731,71:75))] <-"low_intensity"
grid_eu_spafra_cropped$eulandsystem_spafra[which(grid_eu_spafra_cropped$eulandsystem %in% c(22,32,42,52,62,732))] <-"medium_intensity"
grid_eu_spafra_cropped$eulandsystem_spafra[which(grid_eu_spafra_cropped$eulandsystem %in% c(23,43,53,63,733))] <-"high_intensity"
grid_eu_spafra_cropped$eulandsystem_spafra <- factor(grid_eu_spafra_cropped$eulandsystem_spafra, levels = c("no_intensity","low_intensity","medium_intensity","high_intensity")) 
grid_eu_spafra_cropped$eulandsystem <- NULL

ggplot(grid_eu_spafra_cropped) +
  geom_sf(aes(fill=eulandsystem_spafra),col=NA) +
  scale_fill_viridis_d() + theme_minimal()

## summarize external variable for each site and format them

value_site_spafra <- as.data.frame(area_site_spafra) %>% group_by(siteID) %>% summarize(pop2000 = weighted.mean(pop2000,area), pop2020 = weighted.mean(pop2020,area),
                                                                                  impervious2006 = weighted.mean(impervious2006,area), impervious2018 = weighted.mean(impervious2018,area),
                                                                                  treedensity2012 = weighted.mean(treedensity2012,area), treedensity2018 = weighted.mean(treedensity2018,area), 
                                                                                  eulandsystem = eulandsystem[which.max(area)], protectedarea = protectedarea[which.max(area)],
                                                                                  lightpollution2000 = weighted.mean(lightpollution2000,area), lightpollution2013 = weighted.mean(lightpollution2013,area), 
                                                                                  pesticide_kg = weighted.mean(pesticide_kg,area), pesticide_kg_ha = weighted.mean(pesticide_kg_ha,area), pesticide_nodu_kg = weighted.mean(pesticide_nodu_kg,area), 
                                                                                  woodprod2000 = weighted.mean(woodprod2000,area), woodprod2010 = weighted.mean(woodprod2010,area), woodprodaverage = weighted.mean(woodprodaverage,area),
                                                                                  drymatter2000 = weighted.mean(drymatter2000,area), drymatter2018 = weighted.mean(drymatter2018,area), declineproductivity = weighted.mean(declineproductivity,area),
                                                                                  smallwoodyfeatures = weighted.mean(smallwoodyfeatures,area), fragmentation = weighted.mean(fragmentation,area),
                                                                                  forestintegrity = weighted.mean(forestintegrity,area), forestintegrity_cat = forestintegrity_cat[which.max(area)],
                                                                                  temp2000 = weighted.mean(temp2000,area), temp2020 = weighted.mean(temp2020,area), tempspring2000 = weighted.mean(tempspring2000,area), tempspring2020 = weighted.mean(tempspring2020,area),
                                                                                  tempspringvar2000 = weighted.mean(tempspringvar2000,area), tempspringvar2020 = weighted.mean(tempspringvar2020,area),
                                                                                  prec2000 = weighted.mean(prec2000,area), prec2020 = weighted.mean(prec2020,area), precspring2000 = weighted.mean(precspring2000,area), precspring2020 = weighted.mean(precspring2020,area),
                                                                                  precspringvar2000 = weighted.mean(precspringvar2000), precspringvar2020 = weighted.mean(precspringvar2020,area),
                                                                                  humidity2000 = weighted.mean(humidity2000,area), humidity2020 = weighted.mean(humidity2020,area), humidityspring2000 = weighted.mean(humidityspring2000,area),
                                                                                  humidityspring2020 = weighted.mean(humidityspring2020,area), humidityspringvar2000 = weighted.mean(humidityspringvar2000,area), humidityspringvar2020 = weighted.mean(humidityspringvar2020,area),
                                                                                  shannon = weighted.mean(shannon,area), GDP2000 = weighted.mean(GDP2000,area), GDP2015 = weighted.mean(GDP2015,area), biogeo_area = biogeo_area[which.max(area)])

value_site_spafra$GDP2000_percap <- value_site_spafra$GDP2000/value_site_spafra$pop2000
pop2015 <- (value_site_spafra$pop2020-value_site_spafra$pop2000)/20*(2015-2020)+value_site_spafra$pop2020
value_site_spafra$GDP2015_percap <- value_site_spafra$GDP2015/pop2015

value_site_spafra$protectedarea[which(is.na(value_site_spafra$protectedarea))] <- 0
value_site_spafra$protectedarea[which(value_site_spafra$protectedarea>0)] <- 1
value_site_spafra$eulandsystem_cat <- NA
value_site_spafra$eulandsystem_cat[which(value_site_spafra$eulandsystem %in% c(11,12,13,80,90))] <-"no_intensity"
value_site_spafra$eulandsystem_cat[which(value_site_spafra$eulandsystem %in% c(21,31,41,51,61,731,71:75))] <-"low_intensity"
value_site_spafra$eulandsystem_cat[which(value_site_spafra$eulandsystem %in% c(22,32,42,52,62,732))] <-"medium_intensity"
value_site_spafra$eulandsystem_cat[which(value_site_spafra$eulandsystem %in% c(23,43,53,63,733))] <-"high_intensity"
value_site_spafra$eulandsystem_cat <- factor(value_site_spafra$eulandsystem_cat, levels = c("no_intensity","low_intensity","medium_intensity","high_intensity")) 
value_site_spafra$eulandsystem <- NULL


## add zero when species no present at monitored site

wide_bird_data <- data.frame(bird_data_spafra[,c("siteID","year","sci_name_out","count")] %>% group_by(siteID) %>% tidyr::complete(year,sci_name_out))
wide_bird_data$count[which(is.na(wide_bird_data$count))] <- 0

bird_data_spafra <- merge(wide_bird_data,site_spafra,by=c("siteID"), all.x=T)

bird_data_spafra <- merge(bird_data_spafra,value_site_spafra, by="siteID", all.x=TRUE)

## remove site not followed enough

nb_year_p_site <- data.frame(bird_data_spafra %>% group_by(siteID, year) %>% summarise(count=n()))
nb_year_p_site <- data.frame(nb_year_p_site %>% group_by(siteID) %>% summarise(nb_year=n(),
                                                                               min_year=min(year),
                                                                               max_year=max(year)))

selected_site_spafra <- nb_year_p_site[which(nb_year_p_site$nb_year > 4 &
                                              nb_year_p_site$min_year < 2006 &
                                              nb_year_p_site$max_year > 2014),]

subsite_data_spafra_trend <- bird_data_spafra[which(bird_data_spafra$siteID %in% c(selected_site_spafra$siteID)),]


## get value per year per pressure

press_spafra_trend <- ddply(distinct(subsite_data_spafra_trend,siteID,year,.keep_all=TRUE), .(siteID,year),
                                .fun = function(x){
                                  pop <- (x$pop2020-x$pop2000)/21*(x$year-2000)+x$pop2000
                                  impervious <- (x$impervious2018-x$impervious2006)/13*(x$year-2006)+x$impervious2006
                                  treedensity <- (x$treedensity2018-x$treedensity2012)/7*(x$year-2012)+x$treedensity2012
                                  lightpollution <- (x$lightpollution2013-x$lightpollution2000)/14*(x$year-2000)+x$lightpollution2000
                                  woodprod <- (x$woodprod2010-x$woodprod2000)/11*(x$year-2000)+x$woodprod2000
                                  drymatter <- (x$drymatter2018-x$drymatter2000)/19*(x$year-2000)+x$drymatter2000
                                  temp <- (x$temp2020-x$temp2000)/21*(x$year-2000)+x$temp2000
                                  tempspring <- (x$tempspring2020-x$tempspring2000)/21*(x$year-2000)+x$tempspring2000
                                  tempspringvar <- (x$tempspringvar2020-x$tempspringvar2000)/21*(x$year-2000)+x$tempspringvar2000
                                  prec <- (x$prec2020-x$prec2000)/21*(x$year-2000)+x$prec2000
                                  precspring <- (x$precspring2020-x$precspring2000)/21*(x$year-2000)+x$precspring2000
                                  precspringvar <- (x$precspringvar2020-x$precspringvar2000)/21*(x$year-2000)+x$precspringvar2000
                                  GDP_percap <- (x$GDP2015_percap-x$GDP2000_percap)/16*(x$year-2000)+x$GDP2000_percap
                                  GDP <- (x$GDP2015-x$GDP2000)/16*(x$year-2000)+x$GDP2000
                                  protectedarea <- x$protectedarea
                                  pesticide_nodu <- x$pesticide_nodu_kg
                                  smallwoodyfeatures <- x$smallwoodyfeatures
                                  fragmentation <- x$fragmentation
                                  forestintegrity_cat <- x$forestintegrity_cat
                                  shannon <- x$shannon
                                  eulandsystem_cat <- x$eulandsystem_cat
                                  biogeo_area <- x$biogeo_area
                                  trend_result <- data.frame(pop,impervious,treedensity,lightpollution,woodprod,drymatter,
                                                             temp,tempspring,tempspringvar,prec,precspring,precspringvar,
                                                             GDP_percap,GDP,protectedarea,pesticide_nodu,smallwoodyfeatures,
                                                             fragmentation,forestintegrity_cat,shannon,eulandsystem_cat,biogeo_area)
                                  return(trend_result)
                                },
                                .progress = "text")

press_spafra_trend_scale <- press_spafra_trend

press_spafra_trend_scale[,c("pop","impervious","treedensity","lightpollution",
                            "woodprod","drymatter","temp","tempspring","tempspringvar","prec",        
                            "precspring","precspringvar","GDP_percap","GDP","pesticide_nodu",
                            "smallwoodyfeatures","fragmentation","shannon")] <- scale(press_spafra_trend_scale[,c("pop","impervious","treedensity","lightpollution",
                                                                                                                  "woodprod","drymatter","temp","tempspring","tempspringvar","prec",
                                                                                                                  "precspring","precspringvar","GDP_percap","GDP","pesticide_nodu",
                                                                                                                  "smallwoodyfeatures","fragmentation","shannon")])
## Get the number of sites as a function of bandwidth

data_mat_dist <- SpatialPoints(coords = as.matrix(site_spafra[,c("Long_LAEA","Lat_LAEA")]), proj4string = CRS(crs(site_spafra_sf_reproj)))
DM_spafra <- gw.dist(dp.locat=coordinates(data_mat_dist))

nb_site_window_fun <- function(x,radius){
  nb_site <- length(which(x < radius))
  return(nb_site)
}

for(i in seq(10000,100000,by=1000)){
  nb_site_temporary <- data.frame(apply(DM_spafra,1,nb_site_window_fun,radius=i))
  names(nb_site_temporary) <- paste0("b",i)
  if(i == 10000){
    nb_site_window <- nb_site_temporary
  }else{
    nb_site_window <- cbind(nb_site_window,nb_site_temporary)
  }
}

apply(nb_site_window,2,function(x){quantile(x,0.1)})


## GWPR per species

pressure_data_test <- press_spafra_trend_scale

bird_data_test <- subsite_data_spafra_trend[which(subsite_data_spafra_trend$sci_name_out=="Fringilla coelebs"),c("siteID","year","sci_name_out","count",              
                                                                                                                 "scheme","scheme_code","Long_WGS84","Lat_WGS84",            
                                                                                                                 "plot_size_ha","num_points","transect_length_m","time_effort",
                                                                                                                 "area_sampled_m2","country","method","num_visits",
                                                                                                                 "count_unit","count_aggregation","species","Long_LAEA","Lat_LAEA")]


site_data_test <- site_spafra_sf_reproj

formula_gwpr_test <- count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
  year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
  year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
  year:shannon+year:eulandsystem_cat
formula_gwpr_scheme_test <- count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
  year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
  year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
  year:shannon+year:eulandsystem_cat + scheme_code

min_site_number <- 50
bandwidth_test <- as.numeric(gsub("b","",names(apply(nb_site_window,2,function(x){quantile(x,0.1)})[min(which(apply(nb_site_window,2,function(x){quantile(x,0.1)}) >= min_site_number))])))

bird_data=bird_data_test;pressure_data=pressure_data_test;site_data=site_data_test
bandwidth=bandwidth_test;bandwidth_auto=FALSE;formula_gwpr=formula_gwpr_test;
formula_gwpr_scheme=formula_gwpr_scheme_test;min_site_number_per_species=10

source("functions.R")

min_site_number <- 50

res_spafra_species <- ddply(subsite_data_spafra_trend[,c("siteID","year","sci_name_out","count",
                                   "scheme","scheme_code","Long_WGS84","Lat_WGS84",
                                   "plot_size_ha","num_points","transect_length_m","time_effort",
                                   "area_sampled_m2","country","method","num_visits",
                                   "count_unit","count_aggregation","species","Long_LAEA","Lat_LAEA")],
                            .(sci_name_out),.fun=gwpr_species_manual,
                            pressure_data=press_spafra_trend_scale,site_data=site_spafra_sf_reproj,
                            bandwidth=as.numeric(gsub("b","",names(apply(nb_site_window,2,function(x){quantile(x,0.1)})[min(which(apply(nb_site_window,2,function(x){quantile(x,0.1)}) >= min_site_number))]))),
                            bandwidth_auto=FALSE,
                            formula_gwpr= count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                              year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+
                              year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                              year:shannon+year:eulandsystem_cat,
                            formula_gwpr_scheme=count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                              year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+
                              year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                              year:shannon+year:eulandsystem_cat + scheme_code,
                            min_site_number_per_species=10, .progress = "text")



#saveRDS(res_spafra_species,"output/res_spafra_species.rds")
res_spafra_species <- readRDS("output/res_spafra_species.rds")

res_species_df <- res_spafra_species[which(res_spafra_species$sci_name_out == unique(res_spafra_species$sci_name_out)[9]),]

res_species_sf <- merge(site_spafra_sf[,c("siteID")],res_species_df)
res_plot <- st_as_sf(res_species_sf)
ggplot(grid_eu_spafra_outline) + geom_sf() +  geom_sf(data=res_plot, aes(col=exp(`year:treedensity`))) + scale_color_gradientn(colors = sf.colors(20))



### GLM Poisson per biogeo area

bird_data=bird_data_test;pressure_data=pressure_data_test;site_data=site_data_test
formula_glmp=formula_gwpr_test;formula_glmp_scheme=formula_gwpr_scheme_test;min_site_number_per_species=10

source("functions.R")

res_spafra_species_biogeo <- ddply(subsite_data_spafra_trend[,c("siteID","year","sci_name_out","count",
                                                         "scheme","scheme_code","Long_WGS84","Lat_WGS84",
                                                         "plot_size_ha","num_points","transect_length_m","time_effort",
                                                         "area_sampled_m2","country","method","num_visits",
                                                         "count_unit","count_aggregation","species","Long_LAEA","Lat_LAEA")],
                            .(sci_name_out),.fun=glm_species_biogeo,
                            pressure_data=press_spafra_trend_scale,site_data=site_spafra_sf_reproj,
                            formula_glmp= count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                              year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+
                              year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                              year:shannon+year:eulandsystem_cat,
                            formula_glmp_scheme=count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                              year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+
                              year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                              year:shannon+year:eulandsystem_cat + scheme_code,
                            min_site_number_per_species=10, .progress = "text")


#saveRDS(res_spafra_species_biogeo,"output/res_spafra_species_biogeo.rds")
res_spafra_species_biogeo <- readRDS("output/res_spafra_species_biogeo.rds")

res_species_biogeo_df <- res_spafra_species_biogeo[which(res_spafra_species_biogeo$sci_name_out == unique(res_spafra_species_biogeo$sci_name_out)[9]),]

res_species_biogeo_sf <- merge(grid_eu_spafra_biogeo,res_species_biogeo_df, by="biogeo_area")
ggplot() + geom_sf() +  geom_sf(data=res_species_biogeo_sf, aes(fill=exp(`year:treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))






## analysis for the whole dataset

### grid data

grid_eu_all <- st_read("output/grid_eu_all_clean.gpkg")

grid_id_andorra <- readRDS("output/grid_id_andorra.rds")
grid_eu_all$NUTS2021_0[which(grid_eu_all$GRD_ID %in% grid_id_andorra)] <- "AD" # add country id for Andorra
grid_eu_all$PLS[which(grid_eu_all$GRD_ID %in% grid_id_andorra)] <- 18

grid_eu_all$PLS[grep("HR061|HR063|HR021|HR022|HR023|HR024|HR025|HR026",grid_eu_all$NUTS2021_3)] <- 16
grid_eu_all$PLS[grep("HR062|HR064|HR065|HR050|HR027|HR028|HR031|HR032|HR033|HR034|HR035|HR036|HR037",grid_eu_all$NUTS2021_3)] <- 12

grid_eu_mainland <- grid_eu_all[grep("AL|CY|EL|IS|ME|MK|MT|PT|RS|SK|TR",grid_eu_all$NUTS2021_0,invert = TRUE),] # remove countries without data

grid_eu_mainland <- grid_eu_mainland[grep("ES7|FRY",grid_eu_mainland$NUTS2021_1,invert = TRUE),] # remove oversea territories

grid_eu_mainland <- grid_eu_mainland[which(!(grid_eu_mainland$NUTS2021_0 == "")),]

#st_write(grid_eu_mainland,"output/grid_eu_mainland.gpkg")
grid_eu_mainland <- st_read("output/grid_eu_mainland.gpkg")

grid_eu_mainland2 <- grid_eu_all[grep("CY|IS|TR|MT|AL|EL|ME|MK|RS",grid_eu_all$NUTS2021_0,invert = TRUE),]
grid_eu_mainland2 <- grid_eu_mainland2[grep("ES7|FRY|PT2|PT3",grid_eu_mainland2$NUTS2021_1,invert = TRUE),] # remove oversea territories
grid_eu_mainland2 <- grid_eu_mainland2[which(!(grid_eu_mainland2$NUTS2021_0 == "")),]
#grid_eu_mainland2$PLS[grep("AL|EL|ME|MK|RS",grid_eu_mainland2$NUTS2021_0)] <- 30

#st_write(grid_eu_mainland2,"output/grid_eu_mainland2.gpkg")
grid_eu_mainland2 <- st_read("output/grid_eu_mainland2.gpkg")

sf_use_s2(FALSE)

grid_eu_mainland_biogeo <- grid_eu_mainland2 %>% group_by(PLS) %>% summarise(id="PLS_region")#grid_eu_mainland %>% group_by(biogeo_area) %>% summarise(id="biogeo") 

#st_write(grid_eu_mainland_biogeo,"output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")

grid_eu_mainland_outline <- grid_eu_mainland_biogeo[,1] %>% summarise(id="europe")

#st_write(grid_eu_mainland_outline,"output/grid_eu_mainland_outline.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")

ggplot(grid_eu_mainland_biogeo) + geom_sf(aes(fill=as.character(PLS)),col=NA) + 
  scale_fill_viridis_d()+theme_minimal() + theme(legend.position = "none") +
  theme(text = element_text(colour = "white"),panel.grid = element_line(colour = "white"),axis.text =element_text(colour = "white"))

ggsave("output/biogeo_area.png",
       width = 8,
       height = 10,
       dpi = 300
)


bird_data_clean <- readRDS("output/bird_data_clean.rds")

## site and bird data

sites <- read.table(file = "raw_data/pecbms_bird_data/sites.txt", header = TRUE, sep = "\t")

site_mainland <- sites[which(sites$Long_WGS84 > -10 ),] # remove canary islands

bird_data_mainland <- bird_data_clean[which(bird_data_clean$siteID %in% unique(site_mainland$siteID)),]

site_mainland_sf <- st_as_sf(site_mainland, coords = c("Long_WGS84","Lat_WGS84"))

st_crs(site_mainland_sf) <- 4326

site_mainland_sf_reproj <- st_transform(site_mainland_sf,crs(grid_eu_mainland_outline))

#saveRDS(site_mainland_sf_reproj,"output/site_mainland_sf_reproj.rds")
site_mainland_sf_reproj <- readRDS("output/site_mainland_sf_reproj.rds")

site_mainland$Long_LAEA <- st_coordinates(site_mainland_sf_reproj)[,1]
site_mainland$Lat_LAEA <- st_coordinates(site_mainland_sf_reproj)[,2]


## plot sites and area monitored

ggplot(grid_eu_mainland_outline) +
  geom_sf() +
  geom_sf(data=site_mainland_sf_reproj, size=1) +
  theme_minimal()

ggsave("output/PECBMS_site.png",
       width = 8,
       height = 10,
       dpi = 300
)

#site_mainland_buffer <- st_buffer(site_mainland_sf_reproj, dist = sqrt(site_mainland_sf_reproj$area_sampled_m2/pi))
#area_site_mainland <-  st_intersection(site_mainland_buffer, grid_eu_mainland) # version where buffer is equal to area sampled => issue as pressure are not observed at the same scale between plot, better to assume a contant buffer (median of area sample) and control the count for the area sampled
#area_site_mainland$area <- as.numeric(st_area(area_site_mainland))

radius_buffer <- sqrt(median(site_mainland_sf_reproj$area_sampled_m2)/pi)
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj, dist = 2500)
area_site_mainland <-  st_intersection(site_mainland_buffer, grid_eu_mainland)
area_site_mainland$area <- as.numeric(st_area(area_site_mainland))


## summarize external variable for each site and format them
area_site_mainland_df <- area_site_mainland
st_geometry(area_site_mainland_df) <- NULL

value_site_mainland <- ddply(area_site_mainland_df,.(siteID),
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

saveRDS(value_site_mainland,"output/value_site_mainland.rds")

## add zero when species no present at monitored site

wide_bird_data <- data.frame(bird_data_mainland[,c("siteID","year","sci_name_out","count")] %>% group_by(siteID) %>% tidyr::complete(year,sci_name_out))
wide_bird_data$count[which(is.na(wide_bird_data$count))] <- 0

bird_data_mainland <- merge(wide_bird_data,site_mainland,by=c("siteID"), all.x=T)

#bird_data_mainland <- merge(bird_data_mainland,value_site_mainland, by="siteID", all.x=TRUE)

saveRDS(bird_data_mainland,"output/bird_data_mainland.rds")


## remove site not followed enough

nb_year_p_site <- data.frame(bird_data_mainland %>% group_by(siteID, year) %>% summarise(count=n()))
nb_year_p_site <- data.frame(nb_year_p_site %>% group_by(siteID) %>% summarise(nb_year=n(),
                                                                               min_year=min(year),
                                                                               max_year=max(year)))

selected_site_mainland <- nb_year_p_site[which(nb_year_p_site$nb_year >= 5 &
                                               #nb_year_p_site$min_year <= 2011 &
                                               nb_year_p_site$max_year >= 2011),]

subsite_data_mainland_trend <- bird_data_mainland[which(bird_data_mainland$siteID %in% c(selected_site_mainland$siteID)),]

subsite_data_mainland_trend <- subsite_data_mainland_trend[which(subsite_data_mainland_trend$siteID %in% unique(value_site_mainland$siteID)),] # remove some island site no longer in grid_eu_mainland

saveRDS(subsite_data_mainland_trend,"output/subsite_data_mainland_trend.rds")

site_mainland_sf_reproj <- site_mainland_sf_reproj[which(site_mainland_sf_reproj$siteID %in% unique(value_site_mainland$siteID)),] # remove some island site no longer in grid_eu_mainland

#saveRDS(site_mainland_sf_reproj,"output/site_mainland_sf_reproj.rds")

ggplot(grid_eu_mainland_outline) +
  geom_sf() +
  geom_sf(data=site_mainland_sf_reproj[which(site_mainland_sf_reproj$siteID %in% unique(subsite_data_mainland_trend$siteID)),], size=1) +
  theme_minimal()

ggsave("output/PECBMS_site_selected.png",
       width = 8,
       height = 10,
       dpi = 300
)

## get value per year per pressure

press_mainland_trend <- ddply(distinct(subsite_data_mainland_trend,siteID,year,.keep_all=TRUE), .(siteID,year),
                            .fun = function(x,pressure_data){
                              
                              pressure_subdata <- pressure_data[which(pressure_data$siteID == x$siteID),]
                              
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
                            },pressure_data = value_site_mainland,
                            .progress = "text")

press_mainland_trend$milieu_cat <- NA
press_mainland_trend$milieu_cat[which(press_mainland_trend$milieu %in% c(21,22,23))] <- "urban"
press_mainland_trend$milieu_cat[which(press_mainland_trend$milieu %in% c(41,42,43,71,72,74,75))] <- "forest and shrub"
press_mainland_trend$milieu_cat[which(press_mainland_trend$milieu %in% c(31,32,51,52,53,61,62,63,731,732,733))] <- "openland"
press_mainland_trend$milieu_cat[which(press_mainland_trend$milieu %in% c(0,11,12,13,80,90))] <- "others"


saveRDS(press_mainland_trend,"output/press_mainland_trend.rds") 

press_mainland_trend_scale <- press_mainland_trend
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



saveRDS(press_mainland_trend_scale,"output/press_mainland_trend_scale.rds") 


###### Analysis

### Load previously produced datasets

bird_data_mainland <- readRDS("output/bird_data_mainland.rds")
grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")
press_mainland_trend_scale <- readRDS("output/press_mainland_trend_scale.rds")
site_mainland_sf_reproj <- readRDS("output/site_mainland_sf_reproj.rds")
subsite_data_mainland_trend <- readRDS("output/subsite_data_mainland_trend.rds")


### correlation between covariables

test_multicor <- press_mainland_trend_scale[which(press_mainland_trend_scale$year==2010),c("pop","impervious","treedensity","lightpollution",
                                                                                           "woodprod","drymatter","tempspring","tempspringvar",  
                                                                                           "precspring","precspringvar","humidityspring",
                                                                                           "protectedarea_cat","protectedarea_perc","protectedarea_type","protectedarea_size_cor",
                                                                                           "pesticide_nodu","smallwoodyfeatures",
                                                                                           "fragmentation","shannon","eulandsystem_cat",
                                                                                           "grassland","farmland","low_farmland","high_farmland","low_farmland_tot","high_farmland_tot",
                                                                                           "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                                                                           "eulandsystem_forest_lowmedium","eulandsystem_forest_high")]
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



test_multicor <- press_mainland_trend_scale[which(press_mainland_trend_scale$year==2010),c("d_impervious","d_treedensity","d_agri",
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

ggsave("output/cor_heatmap.png",
       width = 15,
       height = 15,
       dpi = 300
)

### find minimum nubmer of site for GLMP

find_site_nubmer <- function(bird_data,pressure_data){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","scheme_code","Long_LAEA","Lat_LAEA",
                                                   "pop","impervious","treedensity","drymatter","tempspring","tempspringvar",  
                                                   "precspring","protectedarea_perc","protectedarea_type","smallwoodyfeatures",
                                                   "shannon","eulandsystem_cat",
                                                   "grassland","farmland","PLS")])
  
  unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
  
  result_site_number <- data.frame(unique_poisson_df %>% group_by(PLS) %>% summarize(count=n()))
  
  return(result_site_number)
}

site_number_species_biogeo <- ddply(subsite_data_mainland_trend,
                                    .(sci_name_out),.fun=find_site_nubmer,
                                    pressure_data=press_mainland_trend_scale, .progress = "text")

saveRDS(site_number_species_biogeo,"output/site_number_species_biogeo.rds")

### GAM Poisson per PLS biogeo area

source("functions.R")

res_gamm_bird <- ddply(subsite_data_mainland_trend,
                      .(sci_name_out),.fun=gam_species_PLS1,
                      pressure_data=press_mainland_trend_scale,site_data=site_mainland_sf_reproj,
                      .progress = "text")
res_gamm_bird <- res_gamm_bird[which(!is.na(res_gamm_bird$PLS)),]

#saveRDS(res_gamm_bird,"output/res_gamm_bird.rds")
#res_gamm_bird<-readRDS("output/res_gamm_bird.rds")

res_gam_bird <- ddply(subsite_data_mainland_trend,
                      .(sci_name_out),.fun=gam_species_PLS3,
                      pressure_data=press_mainland_trend_scale,site_data=site_mainland_sf_reproj,
                      .progress = "text")
res_gam_bird <- res_gam_bird[which(!is.na(res_gam_bird$PLS)),]

#saveRDS(res_gam_bird,"output/res_gam_bird.rds")
#res_gam_bird<-readRDS("output/res_gam_bird.rds")

### select good model fit and compare with PECBMS trends

pecbms_trend_class <- read.csv("output/pecbms_trend_class.csv", header=TRUE)

res_gamm_bird_eu <- res_gamm_bird[which(res_gamm_bird$PLS=="europe"),]

res_gamm_bird_eu <- merge(res_gamm_bird_eu,pecbms_trend_class,ny="sci_name_out")

res_gamm_bird_correct <- res_gamm_bird[which(res_gamm_bird$dev_exp>0.25),]

#### Declining species per PLS

dec_species_PLS <- res_gam_bird
dec_species_PLS$year[which(is.na(dec_species_PLS$year))] <- 0
dec_species_PLS <- dec_species_PLS %>% group_by(PLS) %>% summarise(nb_pos = length(which(year > 0)),
                                                                   nb_neg = length(which(year < 0)),
                                                                   nb_stable = length(which(year == 0)),
                                                                   nb_tot = length(year))
dec_species_PLS$perc_species_neg <- dec_species_PLS$nb_neg/dec_species_PLS$nb_tot
dec_species_PLS$perc_species_pos <- dec_species_PLS$nb_pos/dec_species_PLS$nb_tot


dec_species_PLS_sf <- merge(grid_eu_mainland_biogeo,dec_species_PLS,by="PLS")
ggplot() + geom_sf() +  
  geom_sf(data=dec_species_PLS_sf, aes(fill=perc_species_neg)) + scale_fill_gradientn(colors = paletteer_c("ggthemes::Classic Area Red", 30))
ggplot() + geom_sf() +  
  geom_sf(data=dec_species_PLS_sf, aes(fill=perc_species_pos)) + scale_fill_gradientn(colors = paletteer_c("ggthemes::Classic Blue", 30))


#### Pressure by PLS

matrix_pressure_PLS_neg_bird <- res_gam_bird
#matrix_pressure_PLS_neg_bird <- res_gam_bird_farmland
#matrix_pressure_PLS_neg_bird <- res_gam_bird_forest
matrix_pressure_PLS_neg_bird <- matrix_pressure_PLS_neg_bird %>% group_by(PLS) %>% summarise(openland_effect = length(which(milieu_catopenland < 0))/length(milieu_catopenland),
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


matrix_pressure_PLS_neg_bird_sf <- merge(grid_eu_mainland_biogeo,matrix_pressure_PLS_neg_bird,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=matrix_pressure_PLS_neg_bird_sf, aes(fill=treedensity_high_effect)) + scale_fill_gradient2(low="white", high="red", limits = c(0, max(matrix_pressure_PLS_neg_bird[,2:16])))

sort_matrix_pressure_PLS_neg_bird <- as.matrix(matrix_pressure_PLS_neg_bird[,c("drymatter_effect","protectedarea_perc_effect","shannon_effect","tempspring_effect","tempspringvar_effect","precspring_effect","impervious_effect","impervious_med_effect","impervious_high_effect",
                                                                                         "treedensity_effect","treedensity_med_effect","treedensity_high_effect",
                                                                                         "farmland_effect","farmland_med_effect","farmland_high_effect")])


for(i in unique(matrix_pressure_PLS_neg_bird$PLS)){
  temporary_df <- data.frame(rank = rank(sort_matrix_pressure_PLS_neg_bird[which(matrix_pressure_PLS_neg_bird$PLS == i),]), PLS = i)
  temporary_df$pressure <- row.names(temporary_df)
  if(i == "1"){
    rank_pressure_neg_bird <- temporary_df
  }else{
    rank_pressure_neg_bird <- rbind(rank_pressure_neg_bird,temporary_df)
  }
}

rank_pressure_neg_bird_short <- dcast(rank_pressure_neg_bird,  PLS ~ pressure, value.var = "rank" )
rank_pressure_neg_bird_short_sf <- merge(grid_eu_mainland_biogeo,rank_pressure_neg_bird_short,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=rank_pressure_neg_bird_short_sf, aes(fill=farmland_high_effect)) + scale_fill_gradient2(low="white", high="red", limits = c(1, 15))
ggplot() + geom_sf() +  
  geom_sf(data=rank_pressure_neg_bird_short_sf, aes(fill=as.character(farmland_high_effect))) + scale_fill_manual(values=c("1"="#fee5d9","2"="#fee5d9","3"="#fee5d9",
                                                                                                                  "4"="#fcae91","5"="#fcae91","6"="#fcae91",
                                                                                                                  "7"="#fb6a4a","8"="#fb6a4a","9"="#fb6a4a",
                                                                                                                  "10"="#de2d26","11"="#de2d26","12"="#de2d26",
                                                                                                                  "13"="#a50f15","14"="#a50f15","15"="#a50f15"),name="High-intensity farmland")

ggsave("output/pressure_highfarm_neg_bird.png",
       width = 8,
       height = 8,
       dpi = 300
)

matrix_pressure_PLS_pos_bird <- res_gam_bird
#matrix_pressure_PLS_pos_bird <- res_gam_bird_farmland
#matrix_pressure_PLS_pos_bird <- res_gam_bird_forest
matrix_pressure_PLS_pos_bird <- matrix_pressure_PLS_pos_bird %>% group_by(PLS) %>% summarise(openland_effect = length(which(milieu_catopenland > 0))/length(milieu_catopenland),
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

matrix_pressure_PLS_pos_bird_sf <- merge(grid_eu_mainland_biogeo,matrix_pressure_PLS_pos_bird,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=matrix_pressure_PLS_pos_bird_sf, aes(fill=impervious_effect)) + scale_fill_gradient2(low="white", high="blue", limits = c(0, max(matrix_pressure_PLS_neg_bird[,2:16])))

sort_matrix_pressure_PLS_pos_bird <- as.matrix(matrix_pressure_PLS_pos_bird[,c("drymatter_effect","protectedarea_perc_effect","shannon_effect","tempspring_effect","tempspringvar_effect","precspring_effect","impervious_effect","impervious_med_effect","impervious_high_effect",
                                                                                         "treedensity_effect","treedensity_med_effect","treedensity_high_effect",
                                                                                         "farmland_effect","farmland_med_effect","farmland_high_effect")])


for(i in unique(matrix_pressure_PLS_pos_bird$PLS)){
  temporary_df <- data.frame(rank = rank(sort_matrix_pressure_PLS_pos_bird[which(matrix_pressure_PLS_pos_bird$PLS == i),]), PLS = i)
  temporary_df$pressure <- row.names(temporary_df)
  if(i == "1"){
    rank_pressure_pos_bird <- temporary_df
  }else{
    rank_pressure_pos_bird <- rbind(rank_pressure_pos_bird,temporary_df)
  }
}

rank_pressure_pos_bird_short <- dcast(rank_pressure_pos_bird,  PLS ~ pressure, value.var = "rank" )
rank_pressure_pos_bird_short_sf <- merge(grid_eu_mainland_biogeo,rank_pressure_pos_bird_short,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=rank_pressure_pos_bird_short_sf, aes(fill=protectedarea_perc_effect)) + scale_fill_gradient2(low="white", high="blue", limits = c(1, 15))
ggplot() + geom_sf() +  
  geom_sf(data=rank_pressure_pos_bird_short_sf, aes(fill=as.character(protectedarea_perc_effect))) + scale_fill_manual(values=c("1"="#eff3ff","2"="#eff3ff","3"="#eff3ff",
                                                                                                                           "4"="#bdd7e7","5"="#bdd7e7","6"="#bdd7e7",
                                                                                                                           "7"="#6baed6","8"="#6baed6","9"="#6baed6",
                                                                                                                           "10"="#3182bd","11"="#3182bd","12"="#3182bd",
                                                                                                                           "13"="#08519c","14"="#08519c","15"="#08519c"),name="Protected area")

ggsave("output/pressure_protectedarea_pos_bird.png",
       width = 8,
       height = 8,
       dpi = 300
)



pressure_EU_bird <- data.frame(rbind(matrix_pressure_PLS_pos_bird[matrix_pressure_PLS_pos_bird$PLS=="europe",],
                                          matrix_pressure_PLS_neg_bird[matrix_pressure_PLS_neg_bird$PLS=="europe",]))
pressure_EU_bird$PLS <- pressure_EU_bird$nb_sp <- NULL
pressure_EU_bird$sign <- c("positive effect","negative effect")
pressure_EU_bird <- melt(pressure_EU_bird, id.vars="sign")

df_pressure_EU_bird <- pressure_EU_bird %>%
  mutate(value = ifelse(sign == "positive effect", value, -1 * value))

df_pressure_EU_bird_factor <- df_pressure_EU_bird %>% group_by(variable) %>% summarise(score = sum(value)/sum(abs(value)))
df_pressure_EU_bird_factor <- df_pressure_EU_bird_factor[order(df_pressure_EU_bird_factor$score),]
df_pressure_EU_bird_factor$variable <- as.character(df_pressure_EU_bird_factor$variable)

df_pressure_EU_bird$variable <- as.character(df_pressure_EU_bird$variable)
df_pressure_EU_bird$variable <- factor(df_pressure_EU_bird$variable, df_pressure_EU_bird_factor$variable)

ggplot(df_pressure_EU_bird, aes(x = variable, y = value, fill = sign)) +
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

ggsave("output/pressure_bird_eu.png",
       width = 8,
       height = 6,
       dpi = 300
)

ggplot(df_pressure_EU_bird[which(df_pressure_EU_bird$variable %in% c("d_impervious_effect","d_tempsrping_effect","d_tempsrpingvar_effect",
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

ggsave("output/pressure_trend_bird_eu.png",
       width = 8,
       height = 6,
       dpi = 300
)

ggplot(df_pressure_EU_bird[which(df_pressure_EU_bird$variable %in% c("openland_effect","otherLULC_effect","urban_effect","tempspring_effect",
                                                                     "precspring_effect","shannon_effect","drymatter_effect")),], aes(x = variable, y = value, fill = sign)) +
  geom_col(alpha=0.5) + geom_hline(aes(yintercept = 0)) +
  scale_fill_manual(values = c("positive effect" = "blue","negative effect" = "red")) +
  scale_x_discrete(labels=c("openland_effect" = "Openland vs. forest on abundance", "otherLULC_effect" = "Other LULC vs. forest on abundance", "urban_effect" = "Urban land vs. forest on abundance", "tempspring_effect" = "Mean temperature on abundance",             
                            "precspring_effect" = "Mean precipitation on abundance", "shannon_effect" = "Landscape diversity on abundance", "drymatter_effect" = "Ecosystem productivity on abundance")) +
  xlab("Pressures") + ylab("% of species impacted") +
  theme_minimal() + coord_flip() +
  theme(legend.position = "none")

ggsave("output/control_ab_bird_eu.png",
       width = 8,
       height = 6,
       dpi = 300
)


### check by species type


species_habitat <- read.csv2("raw_data/Habitat_class_PECBMS.csv")
res_gam_bird_farmland <- res_gam_bird[which(res_gam_bird$sci_name_out %in% unique(species_habitat$Species[which(species_habitat$Habitat=="Farmland")])),]
res_gam_bird_forest <- res_gam_bird[which(res_gam_bird$sci_name_out %in% unique(species_habitat$Species[which(species_habitat$Habitat=="Forest")])),]


ggplot(res_gam_bird, aes(x= PLS, y=dev_exp, fill=PLS)) + 
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_minimal()

# other representation

pressure_EU_bird <- res_gam_bird[which(res_gam_bird$PLS=="europe"),]
pressure_EU_bird <- res_gamm_bird_correct[which(res_gamm_bird_correct$PLS=="europe"),]
pressure_EU_bird_long <- melt(pressure_EU_bird, id.vars=c("sci_name_out","PLS"))
pressure_EU_bird_long <- pressure_EU_bird_long[which(!pressure_EU_bird_long$variable %in% c("(Intercept)","PLS","dev_exp","n_obs")),]



ggplot(pressure_EU_bird_long[which(pressure_EU_bird_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
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


ggsave("output/pressure_trend_bird_eu_hist.png",
       width = 12,
       height = 8,
       dpi = 300
)






matrix_pressure_PLS <- data.frame(res_gamm_bird_correct %>% group_by(PLS) %>% summarise(nb_sp_neg_lulc = length(which(`year:d_impervious` < 0 | `year:d_shannon` < 0 | `year:d_treedensity:eulandsystem_forest_lowmedium` <0 | `year:d_treedensity:eulandsystem_forest_high` < 0 | `year:d_agri:eulandsystem_farmland_low` < 0 | `year:d_agri:eulandsystem_farmland_medium` < 0 | `year:d_agri:eulandsystem_farmland_high` < 0)),
                                                                             nb_sp_neg_climate = length(which(`year:d_tempsrping` < 0 | `year:d_tempsrpingvar` < 0 | `year:d_precspring` < 0)),
                                                                             max_effect = ifelse(nb_sp_neg_lulc > nb_sp_neg_climate, "lulc", "climate"),
                                                                             nb_sp_neg = length(which(year < 0)),
                                                                             nb_sp_pos = length(which(year > 0)),
                                                                             nb_sp = n(),
                                                                             max_effect_percent = ifelse(nb_sp_neg_lulc > nb_sp_neg_climate, nb_sp_neg_lulc/nb_sp, nb_sp_neg_climate/nb_sp),
                                                                             min_effect_percent = ifelse(nb_sp_neg_lulc < nb_sp_neg_climate, nb_sp_neg_lulc/nb_sp, nb_sp_neg_climate/nb_sp)))


matrix_pressure_PLS_sf <- merge(grid_eu_mainland_biogeo,matrix_pressure_PLS,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=matrix_pressure_PLS_sf, aes(fill=max_effect, alpha=max_effect_percent), col=NA) + scale_fill_manual(values = c("lulc" = "#33a02c", "climate" = "#1f78b4")) +
  scale_alpha_continuous(range = c(0.35, 0.95)) + theme(legend.position = "none")
  

ggsave("output/main_pressure_neg_bird.png",
       width = 8,
       height = 8,
       dpi = 300
)


grid_eu_mainland_biogeo_plot <- grid_eu_mainland_biogeo
grid_eu_mainland_biogeo_plot$color <- as.character(c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5)))#grid_eu_mainland_biogeo_plot$PLS %% 2 == 0
poly_grid_eu_mainland_biogeo <- st_cast(grid_eu_mainland_biogeo, "POLYGON")
poly_grid_eu_mainland_biogeo <- poly_grid_eu_mainland_biogeo[which(as.numeric(st_area(poly_grid_eu_mainland_biogeo)) > 2000000000),]
poly_grid_eu_mainland_biogeo_center <- st_centroid(poly_grid_eu_mainland_biogeo)
text_coord <- st_drop_geometry(poly_grid_eu_mainland_biogeo_center)
text_coord$Long <- st_coordinates(poly_grid_eu_mainland_biogeo_center)[,1]
text_coord$Lat <- st_coordinates(poly_grid_eu_mainland_biogeo_center)[,2]
ggplot() + geom_sf() +  
  geom_sf(data=grid_eu_mainland_biogeo_plot,aes(fill=color), alpha=0.5) + 
  scale_fill_viridis_d() +
  geom_text(data = text_coord, aes(x=Long, y=Lat, label=PLS), size = 3) +
  theme(legend.position = "none")

ggsave("output/bioregions.png",
       width = 8,
       height = 8,
       dpi = 300
)









### INLA instead of frequentist #https://punama.github.io/BDI_INLA/

library(INLA)
library(fmesher)

grid_eu_20 <- st_read("raw_data/grid_eu/grid_20km_surf.gpkg")
grid_eu_20 <- grid_eu_20[grep("CY|IS|TR|MT|AL|EL|ME|MK|RS",grid_eu_20$NUTS2021_0,invert = TRUE),]
grid_eu_20 <- grid_eu_20[grep("ES7|FRY|PT2|PT3",grid_eu_20$NUTS2021_1,invert = TRUE),] # remove oversea territories
grid_eu_20 <- grid_eu_20[which(!(grid_eu_20$NUTS2021_0 == "")),]



grid_coords=as.matrix(st_coordinates(st_centroid(grid_eu_20))[,c("X","Y")]%>%st_drop_geometry())
## Compute the boundaries of the mesh
# - Inner boundary with slight concavity to follow data
bndint = inla.nonconvex.hull(grid_coords, convex = -0.05)
# - Outer boundary with stronger concavity for extension
bndext = inla.nonconvex.hull(grid_coords, convex = -0.2)
# Build triangular mesh:
# - max.edge: Maximum allowed triangle edge length (inner, outer zones)
# - cutoff: Minimum distance between mesh vertices
mesh = fm_mesh_2d_inla(grid_coords,
                       boundary = list(bndint, bndext),
                       max.edge = c(50000, 200000), cutoff = 100000)
print(mesh$n) # Number of mesh vertices, 2000 c'est pas mal, trop risque de faire buguer

par(mar = rep(0.5, 4))
plot(mesh, main = "", asp = 1)
points(grid_coords,col=2,cex=0.2,pch=16)
points(mesh$loc, col=4, cex=0.5)

poisson_sf <- merge(site_data[,"siteID"],poisson_df)

sp_dat_sf = poisson_sf%>%
  #filter(species==species_name)%>%
  #select(Month,Time_period,Year,geometry)%>%
  st_join(grid_eu_20[c('GRD_ID')],join=st_within)

plot(grid_eu_mainland_outline)
plot(sp_dat_sf,pch=16,col=4,add=T,cex=0.5)


tmp <- na.omit(sp_dat_sf %>%
  st_drop_geometry()%>%
  #select(GRD_ID)%>%
  group_by(GRD_ID)%>%
  summarize(nFocal=n()))

Poisson_sf <- na.omit(sp_dat_sf %>%
  st_drop_geometry()%>%
  #select(GRD_ID)%>%
  group_by(GRD_ID)%>%
  summarize(temp=mean(tempsrping),protect=mean(protectedarea_perc),shannon=mean(shannon)))


# cell centroids coordinates per sampled spatio-temporal cell.
coord_data = as.matrix(st_coordinates(st_centroid(grid_eu_20[which(grid_eu_20$GRD_ID %in% unique(Poisson_sf$GRD_ID)),]))[,c("X","Y")])
# Compute the observation matrix A
A = inla.spde.make.A(mesh, loc = coord_data)
# Indices for the total observation matrix A implemented with inla.stack.
idx_sp = inla.spde.make.index("sp",n.spde = mesh$n)

stk = inla.stack(
  data = list(y = as.numeric(tmp$nFocal)),  # Specify the response variable
  A = list(1, A),         # Vector of multiplication factors for random and fixed effects
  effects = list(as.data.frame(Poisson_sf), idx_sp),
  tag = "obs"  # Detail the random and fixed effects
)


nu = 1
d = 2 # dimension
alpha = nu + d / 2
spde_sp = inla.spde2.pcmatern(
  mesh = mesh, alpha = alpha,constr = T, 
  prior.range = c(10, 0.5), prior.sigma = c(1, 0.5)
)


formula = y ~ temp + protect + shannon + 
  f(sp, model = spde_sp)


fit = inla(
  formula, # Formula of the model
  #offset = fixed_effects$log_TG_Count,
  data = inla.stack.data(stk), # data = Observation matrix
  family = "poisson",  # Distribution of the response variable
  control.compute = list(cpo=T, dic=T, mlik=T, waic=T, return.marginals.predictor = T, config = TRUE),  # Statistics criteria
  control.predictor=list(compute = FALSE, A = inla.stack.A(stk), link = 1), # link = 1 to compute the fitted values with the links function
  control.inla = list(int.strategy = "eb", strategy = "adaptive"),  # Optimization settings
  verbose = F,#T, # Text option
  #inla.mode = "experimental",
  num.threads = 10)
summary(fit)


# Create projection matrix from mesh to prediction points
A_pred <- inla.spde.make.A(mesh = mesh, loc = grid_coords)

# Get estimated spatial field values (posterior mean)
spatial_field <- fit$summary.random$sp$mean

# Calculate spatial component at prediction points
spatial_part <- as.vector(A_pred %*% spatial_field)

grid_eu_20$intensity <- exp(spatial_part)

# With geom_sf
ggplot() +
  geom_sf(data=grid_eu_20,
          aes(fill=log10(intensity))) +
  scale_fill_viridis()+
  theme_minimal() +
  labs(title = "Estimated Intensity Surface")




bird_data = subsite_data_mainland_trend[which(subsite_data_mainland_trend$sci_name_out=="Alauda arvensis"),]
pressure_data = press_mainland_trend_scale
site_data = site_mainland_sf_reproj
site_data$Long_LAEA <- st_coordinates(site_data)[,1]
site_data$Lat_LAEA <- st_coordinates(site_data)[,2]

coords = cbind(site_data$Long_LAEA, site_data$Lat_LAEA)
bdry <- inla.sp2segment(site_data)
bdry$loc <- inla.mesh.map(bdry$loc)

#mesh0 <- inla.mesh.2d(loc = coords, boundary = bdry, max.edge=c(0.5))
mesh1 <- inla.mesh.2d(loc = coords, boundary = bdry, max.edge=c(0.5, 1))
mesh2 <- inla.mesh.2d(loc = coords, boundary = bdry, max.edge=c(0.5, 1),
                      offset = c(0.5,1))
mesh3 <- inla.mesh.2d(loc = coords, boundary = bdry, max.edge=c(0.5,1), 
                      offset = c(0.5, 1),
                      cutoff = 0.3)

non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
mesh4 <- inla.mesh.2d(boundary = non_convex_bdry, max.edge=c(0.5,1), 
                      offset = c(0.5, 1),
                      cutoff = 0.3)


species_press_data_year <- merge(bird_data,
                                 pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),],
                                 by = c("siteID","year"), all.x=TRUE)

resp <- "count"
covar1 <- c("year","time_effort","area_sampled_m2")
covar2 <- c("impervious","treedensity","lightpollution",
           "woodprod","drymatter","tempspring","tempspringvar",  
           "precspring","precspringvar","humidityspring",
           "protectedarea_cat","protectedarea_strong_perc",
           "pesticide_nodu","smallwoodyfeatures",
           "shannon","eulandsystem_cat",
           "grassland","farmland","low_farmland","high_farmland")
phen <- c("siteID","scheme_code","Long_LAEA","Lat_LAEA")



poisson_df <- na.omit(species_press_data_year[,c(resp,covar,phen)])
poisson_df$count <- round(poisson_df$count)

table(poisson_df$scheme_code)

f0.1 <- as.formula(paste0(resp, " ~ ", # Response first
                          paste0(paste(covar1[-1], collapse = " + "),
                                 sep = " + ",
                                 paste(paste0("year:",covar2), collapse = " + ")) # Collapse the vector of covariates
))

# Run the model
IM0.1  <- inla(f0.1,
               family = "poisson", # Specify the family. Can be a wide range (see r-inla.org).
               data = poisson_df) # Specify the data

# Then with an ID random effect ####

f0.2 <- as.formula(paste0(resp, " ~ ", # Response first
                          paste0(paste(covar1[-1], collapse = " + "),
                                 sep = " + ",
                                 paste(paste0("year:",covar2), collapse = " + "),
                                 " +  f(scheme_code, model = 'iid')") # Collapse the vector of covariates
))


IM0.2  <- inla(f0.2, 
               family = "poisson",
               data = poisson_df,control.compute = list(dic = TRUE)) 

summary(IM0.2)
summary(IM0.2)$dic$dic

Graph <- as.data.frame(summary(IM0.1)$fixed)














###### Community analysis

## specialisation index (SSI)

SSI <- read.csv("raw_data/species_indices/SSI_10.1002.ece3.5419.csv")  # from https://doi.org/10.1002/ece3.5419 reused in DOI:10.1111/geb.13405


## Species temperature index (STI)
# https://royalsocietypublishing.org/doi/epdf/10.1098/rspb.2008.0878
# https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1600-0587.2012.07799.x
# get bird occurence from https://ebba2.info/data-request/
# get climat data from https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php

# species occurrence

species_occ <- read.csv("raw_data/species_indices/ebba2_data_request_occurrence_50km/ebba2_data_occurrence_50km.csv", sep=";", header = T)

# grid of occurrence

grid_occ <- read_sf(dsn = "raw_data/species_indices/ebba2_grid50x50_v1/", layer = "ebba2_grid50x50_v1")

# temperature by grid cell and by year

for(i in 2000:2022){ 
  print(i)
  year <- paste0("temp_",i)
  path <- paste0("output/temp_",i,".tif")

  map_year <- assign(year, raster(path))
  
  mean_value_year <- exact_extract(map_year,grid_occ, "mean")

  grid_occ$new_col <- mean_value_year
  names(grid_occ)[which(names(grid_occ)=="new_col")] <- year
  
}

ggplot(grid_occ) + 
  geom_sf(aes(fill=temp_2000), col=NA)+
  scale_fill_gradient2()

grid_occ$temp_mean <- apply(grid_occ[,c(3:25)],1,function(x){return(mean(as.numeric(x), na.rm=T))})

grid_mean <- grid_occ
st_geometry(grid_mean) <- NULL

# Merge species occurrence and temperature

species_occ_temp <- merge(species_occ,data.frame(grid_mean),
                          by = "cell50x50", all.x=TRUE)

STI <- data.frame(species_occ_temp %>% group_by(birdlife_scientific_name) %>% summarise(STI = mean(temp_mean, na.rm=T),
                                                                                                sd_STI = sd(temp_mean, na.rm=T)))
#saveRDS(STI,"raw_data/species_indices/STI.rds")
STI <- readRDS("raw_data/species_indices/STI.rds")

SSI$Species[which(SSI$Species=="Carduelis chloris")] <- "Chloris chloris"
SSI$Species[which(SSI$Species=="Parus caeruleus")] <- "Cyanistes caeruleus"
SSI$Species[which(SSI$Species=="Miliaria calandra")] <- "Emberiza calandra"
SSI$Species[which(SSI$Species=="Anas penelope")] <- "Mareca penelope"
SSI$Species[which(SSI$Species=="Anas strepera")] <- "Mareca strepera"
SSI$Species[which(SSI$Species=="Parus ater")] <- "Periparus ater"
SSI$Species[which(SSI$Species=="Parus montanus")] <- "Poecile montanus"
SSI$Species[which(SSI$Species=="Hirundo rupestris")] <- "Ptyonoprogne rupestris"


SXI <- merge(STI,SSI,by.x="birdlife_scientific_name",by.y="Species", all.x=TRUE)

#saveRDS(SXI,"raw_data/species_indices/SXI.rds")
SXI <- readRDS("raw_data/species_indices/SXI.rds")



community_data <- merge(subsite_data_mainland_trend,SXI,by.x="sci_name_out", by.y="birdlife_scientific_name", all.x=TRUE)

community_data$count_STI <- community_data$count*community_data$STI

community_data$count_SSI <- community_data$count*community_data$Specialism.index..overall.

community_data_SXI <- data.frame(community_data %>% group_by(siteID,year,scheme_code,Long_WGS84,Lat_WGS84,Long_LAEA,Lat_LAEA,
                                                             time_effort,area_sampled_m2) %>%
                                   summarize(sum_count_STI = sum(count_STI, na.rm=TRUE), sum_count = sum(count, na.rm = TRUE),
                                             sum_count_SSI = sum(count_SSI, na.rm=TRUE), sum_count2 = sum(count[which(!is.na(Specialism.index..overall.))])))

community_data_SXI$CTI <- community_data_SXI$sum_count_STI/community_data_SXI$sum_count
community_data_SXI$CSI <- community_data_SXI$sum_count_SSI/community_data_SXI$sum_count2


res_mainland_cxi_biogeo <- lm_CXI_biogeo(community_data_SXI,press_mainland_trend_scale,site_mainland_sf_reproj)

#saveRDS(res_mainland_cxi_biogeo,"output/res_mainland_cxi_biogeo.rds")
res_mainland_cxi_biogeo <- readRDS("output/res_mainland_cxi_biogeo.rds")


res_mainland_cxi_biogeo_CTI <- res_mainland_cxi_biogeo[which(res_mainland_cxi_biogeo$variable != "(Intercept)" & res_mainland_cxi_biogeo$cxi=="CTI"),]

ggplot(res_mainland_cxi_biogeo_CTI, aes(x = variable, y = exp(Estimate))) + 
  geom_point(position = position_jitterdodge(jitter.width=0.15,dodge.width = 0.6), 
             alpha = 0.2, size = 3, stroke = 0, na.rm = TRUE, aes(col=variable)) +
  geom_violin(width = 0.6, alpha = 0.1, na.rm = TRUE, aes(fill = variable)) +
  geom_boxplot(width = 0.6, alpha = 0.1, na.rm = TRUE,outlier.shape = NA,aes(fill = variable)) + 
  theme_ggstatsplot() +
  labs(y="Estimate") + theme(axis.title.x = element_blank(),
                             axis.text.x = element_text(angle=45, hjust = 1),
                             legend.position = "none")


res_mainland_cxi_biogeo_CSI <- res_mainland_cxi_biogeo[which(res_mainland_cxi_biogeo$variable != "(Intercept)" & res_mainland_cxi_biogeo$cxi=="CSI"),]

ggplot(res_mainland_cxi_biogeo_CSI, aes(x = variable, y = exp(Estimate))) + 
  geom_point(position = position_jitterdodge(jitter.width=0.15,dodge.width = 0.6), 
             alpha = 0.2, size = 3, stroke = 0, na.rm = TRUE, aes(col=variable)) +
  geom_violin(width = 0.6, alpha = 0.1, na.rm = TRUE, aes(fill = variable)) +
  geom_boxplot(width = 0.6, alpha = 0.1, na.rm = TRUE,outlier.shape = NA,aes(fill = variable)) + 
  theme_ggstatsplot() +
  labs(y="Estimate") + theme(axis.title.x = element_blank(),
                             axis.text.x = element_text(angle=45, hjust = 1),
                             legend.position = "none")



























###### ADDITIONAL MATERIAL

### GLMM Poisson per biogeo areas and sites

source("functions.R")

res_mainland_species_biogeo_glmm <- ddply(subsite_data_mainland_trend,
                                          .(sci_name_out),.fun=glmm_species_biogeo,
                                          pressure_data=press_mainland_trend_scale,site_data=site_mainland_sf_reproj,
                                          formula_glmp= count~year+year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+
                                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                            year:shannon+year:eulandsystem_cat + (1|siteID),
                                          formula_glmp_scheme=count~year+year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+
                                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                            year:shannon+year:eulandsystem_cat + (1|scheme_code) + (1|siteID),
                                          min_site_number_per_species=40, .progress = "text")


#saveRDS(res_mainland_species_biogeo,"output/res_mainland_species_biogeo.rds")
res_mainland_species_biogeo <- readRDS("output/res_mainland_species_biogeo.rds")











#st_write(grid_eu_no_data,"output/grid_eu_no_data.gpkg")
grid_eu_no_data <- st_read("output/grid_eu_no_data.gpkg")

centroid_eu <- st_centroid(grid_eu_no_data)

grid_eu_cat_cropped <- st_transform(centroid_eu,st_crs(site_mainland_sf))

grid_eu_cat_cropped$lon <- st_coordinates(grid_eu_cat_cropped)[,1]
grid_eu_cat_cropped$lat <- st_coordinates(grid_eu_cat_cropped)[,2]
grid_eu_cat_cropped2 <- grid_eu_cat_cropped[which(grid_eu_cat_cropped$lat > 42.4 & grid_eu_cat_cropped$lat < 42.7),]
grid_eu_cat_cropped2 <- grid_eu_cat_cropped2[which(grid_eu_cat_cropped2$lon > 1.3 & grid_eu_cat_cropped2$lon < 1.9),]

grid_eu_cat_cropped3 <- grid_eu_no_data[which(grid_eu_no_data$GRD_ID %in% unique(grid_eu_cat_cropped2$GRD_ID)),]

crop_factor <- st_bbox(c(xmin = 1.3, 
                         xmax = 1.9, 
                         ymin = 42.4, 
                         ymax = 42.7),
                       crs = st_crs(site_mainland_sf))

site_cat_sf_all_cropped <- st_transform(site_mainland_sf,st_crs(crop_factor))
site_cat_sf_all_cropped <- st_crop(site_cat_sf_all_cropped, crop_factor)
site_cat_sf_all_cropped2 <- site_cat_sf_all_cropped[which(site_cat_sf_all_cropped$country == "Andorra"),]

ggplot(grid_eu_cat_cropped3) +
  geom_sf() +
  geom_sf(data=site_cat_sf_all_cropped)

grid_eu_cat_cropped4 <- grid_eu_cat_cropped3[grep("ES|FR",grid_eu_cat_cropped3$NUTS2021_1,invert = TRUE),]

ggplot(grid_eu_cat_cropped4) +
  geom_sf() +
  geom_sf(data=site_cat_sf_all_cropped2)

grid_id_andorra <- grid_eu_cat_cropped4$GRD_ID

saveRDS(grid_id_andorra,"output/grid_id_andorra.rds")








gwpr_species <- function(bird_data,pressure_data,site_data,bandwidth,bandwidth_auto=FALSE,formula_gwpr,min_site_number_per_species){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","scheme_code","Long_LAEA","Lat_LAEA","pop","impervious","treedensity","lightpollution",
                                                   "woodprod","drymatter","tempspring","tempspringvar",  
                                                   "precspring","precspringvar",
                                                   "protectedarea","pesticide_nodu","smallwoodyfeatures",
                                                   "fragmentation","shannon","eulandsystem_cat")])
  
  ### global poisson model
  
  global_mod <- glm(formula_gwpr, family="poisson", data=poisson_df)
  
  ### autocorrelation of residuals
  
  poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = poisson_df,
                                       proj4string = CRS(crs(site_data)))
  
  nb <- dnearneigh(poisson_sf@coords, 10000,bandwidth)
  lw <- nb2listw(nb, style="W", zero.policy=TRUE)
  moran_I <- lm.morantest(global_mod,lw)
  moran_res <- c(unlist(moran_I[3])[1],unlist(moran_I[2])) # check if autocorrelated
  
  if(moran_res[2]<0.05){
    
    ### GWPR
    
    DM <- gw.dist(dp.locat = coordinates(poisson_sf))
    
    if(bandwidth_auto == TRUE){
      bw.f1 <- bw.ggwr(formula = formula_gwpr, data = poisson_sf, dMat = DM,kernel = "gaussian")
    }
    
    bw.f1 <- bandwidth_test # based on apply(nb_site_window,2,min) > 30
    res.poisson <- ggwr.basic(formula = formula_gwpr, bw = bw.f1,
                              data = poisson_sf, dMat = DM, kernel = "gaussian")
    
    ### Remove edge effect by keeping sites with enough neighbourg
    
    unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
    
    unique_poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(unique_poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = unique_poisson_df,
                                                proj4string = CRS(crs(site_data)))
    
    unique_DM <- gw.dist(dp.locat = coordinates(unique_poisson_sf))
    
    site_to_keep <- unique_poisson_df$siteID[which(apply(unique_DM,1,function(x){length(which(x < bw.f1))}) > min_site_number_per_species)]
    
    res.poisson_noedge <- res.poisson$SDF[which(poisson_sf$siteID %in% site_to_keep),]
    
    #res_plot <- st_as_sf(res.poisson_noedge)
    #ggplot(grid_eu_spafra_outline) + geom_sf() +  geom_sf(data=res_plot, aes(col=exp(`year:treedensity`))) + scale_color_gradientn(colors = sf.colors(20))
    
    return(res.poisson_noedge)
    
  }
}





for(i in 1:length(num_site_within_bw)){
  
  unique_poisson_df_i <- unique_poisson_df[num_site_within_bw[[i]],]
  
  if(nrow(unique_poisson_df_i) >= min_site_number_per_species){
    
    unique_poisson_df_i$w <- exp(-.5*(unique_DM[i,num_site_within_bw[[i]]]/bw.f1)^2)
    
    poisson_df_i <- poisson_df[which(poisson_df$siteID %in% unique_poisson_df_i$siteID),]
    
    weigth_i <- merge(poisson_df_i,unique_poisson_df_i[,c("siteID","w")],by="siteID")
    
    if(length(unique(weigth_i$eulandsystem_cat)) > 1){
      if(length(unique(weigth_i$scheme_code)) > 1){
        res.poisson_i <- glm(formula_gwpr_scheme, family="poisson",
                             data=weigth_i,
                             weights = weigth_i$w) # bisquare
        result_i <- summary(res.poisson_i)$coefficients
        result_i <- result_i[grep("scheme_code",row.names(result_i),invert = TRUE),]
      }else{
        res.poisson_i <- glm(formula_gwpr, family="poisson",
                             data=weigth_i,
                             weights = weigth_i$w) # bisquare
        result_i <- summary(res.poisson_i)$coefficients
      }
    }else{
      if(length(unique(weigth_i$scheme_code)) > 1){
        res.poisson_i <- glm(count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                               year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
                               year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                               year:shannon + scheme_code, family="poisson",
                             data=weigth_i,
                             weights = weigth_i$w) # bisquare
        result_i <- result_i[grep("scheme_code",row.names(result_i),invert = TRUE),]
      }else{
        res.poisson_i <- glm(count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                               year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
                               year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                               year:shannon, family="poisson",
                             data=weigth_i,
                             weights = weigth_i$w) # bisquare
      }
    }
    
    if(i==1){
      result_all_site <- array(NA,c(19,4,length(num_site_within_bw)))
    }
    
    if(nrow(result_i) == 19){
      result_all_site[,,i] <- result_i
    }else{
      row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
      row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
      result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
      result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
      result_i_complet <- as.matrix(result_i_complet[2:5])
      result_all_site[,,i] <- result_i_complet
    }
  }else{
    
    if(i==1){
      result_all_site <- array(NA,c(19,4,length(num_site_within_bw)))
    }else{
      result_all_site[,,i] <- NA
    }
  }
}
