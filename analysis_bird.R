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

crop_factor <- st_bbox(c(xmin = 0, 
                         xmax = 3, 
                         ymax = 41, 
                         ymin = 44),
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
  scale_fill_viridis_b()

ggplot(grid_eu_spafra_cropped) +
  geom_sf(aes(fill=treedensity2012),col=NA) +
  scale_fill_viridis_b()

grid_eu_spafra_cropped$eulandsystem_spafra <- NA
grid_eu_spafra_cropped$eulandsystem_spafra[which(grid_eu_spafra_cropped$eulandsystem %in% c(11,12,13,80,90))] <-"no_intensity"
grid_eu_spafra_cropped$eulandsystem_spafra[which(grid_eu_spafra_cropped$eulandsystem %in% c(21,31,41,51,61,731,71:75))] <-"low_intensity"
grid_eu_spafra_cropped$eulandsystem_spafra[which(grid_eu_spafra_cropped$eulandsystem %in% c(22,32,42,52,62,732))] <-"medium_intensity"
grid_eu_spafra_cropped$eulandsystem_spafra[which(grid_eu_spafra_cropped$eulandsystem %in% c(23,43,53,63,733))] <-"high_intensity"
grid_eu_spafra_cropped$eulandsystem_spafra <- factor(grid_eu_spafra_cropped$eulandsystem_spafra, levels = c("no_intensity","low_intensity","medium_intensity","high_intensity")) 
grid_eu_spafra_cropped$eulandsystem <- NULL

ggplot(grid_eu_spafra_cropped) +
  geom_sf(aes(fill=eulandsystem_spafra),col=NA) +
  scale_fill_viridis_d()

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

grid_eu_mainland <- grid_eu_all[grep("AL|CY|EL|IS|ME|MK|MT|PT|RS|SK|TR",grid_eu_all$NUTS2021_0,invert = TRUE),] # remove countries without data

grid_eu_mainland <- grid_eu_mainland[grep("ES7|FRY",grid_eu_mainland$NUTS2021_1,invert = TRUE),] # remove oversea territories

grid_eu_mainland <- grid_eu_mainland[which(!(grid_eu_mainland$NUTS2021_0 == "")),]

#st_write(grid_eu_mainland,"output/grid_eu_mainland.gpkg")
grid_eu_mainland <- st_read("output/grid_eu_mainland.gpkg")

sf_use_s2(FALSE)
grid_eu_mainland_outline <- grid_eu_mainland[,1] %>% summarise(id="europe")

#st_write(grid_eu_mainland_outline,"output/grid_eu_mainland_outline.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")

grid_eu_mainland_biogeo <- grid_eu_mainland %>% group_by(biogeo_area) %>% summarise(id="biogeo") 

#st_write(grid_eu_mainland_biogeo,"output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")


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
  geom_sf(data=site_mainland_sf_reproj, size=1)

ggsave("output/PECBMS_site.png",
       width = 8,
       height = 10,
       dpi = 300
)

site_mainland_buffer <- st_buffer(site_mainland_sf_reproj, dist = sqrt(site_mainland_sf_reproj$area_sampled_m2/pi))

area_site_mainland <-  st_intersection(site_mainland_buffer, grid_eu_mainland)

area_site_mainland$area <- as.numeric(st_area(area_site_mainland))



## summarize external variable for each site and format them

value_site_mainland <- as.data.frame(area_site_mainland) %>% group_by(siteID) %>% summarize(pop2000 = weighted.mean(pop2000,area), pop2020 = weighted.mean(pop2020,area),
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

value_site_mainland$GDP2000_percap <- value_site_mainland$GDP2000/value_site_mainland$pop2000
pop2015 <- (value_site_mainland$pop2020-value_site_mainland$pop2000)/20*(2015-2020)+value_site_mainland$pop2020
value_site_mainland$GDP2015_percap <- value_site_mainland$GDP2015/pop2015

value_site_mainland$protectedarea[which(is.na(value_site_mainland$protectedarea))] <- 0
value_site_mainland$protectedarea[which(value_site_mainland$protectedarea>0)] <- 1
value_site_mainland$eulandsystem_cat <- NA
value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem %in% c(11,12,13,80,90))] <-"no_intensity"
value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem %in% c(21,31,41,51,61,731,71:75))] <-"low_intensity"
value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem %in% c(22,32,42,52,62,732))] <-"medium_intensity"
value_site_mainland$eulandsystem_cat[which(value_site_mainland$eulandsystem %in% c(23,43,53,63,733))] <-"high_intensity"
value_site_mainland$eulandsystem_cat <- factor(value_site_mainland$eulandsystem_cat, levels = c("no_intensity","low_intensity","medium_intensity","high_intensity")) 
value_site_mainland$eulandsystem <- NULL

value_site_mainland <- as.data.frame(value_site_mainland)

saveRDS(value_site_mainland,"output/value_site_mainland.rds")

## add zero when species no present at monitored site

wide_bird_data <- data.frame(bird_data_mainland[,c("siteID","year","sci_name_out","count")] %>% group_by(siteID) %>% tidyr::complete(year,sci_name_out))
wide_bird_data$count[which(is.na(wide_bird_data$count))] <- 0

bird_data_mainland <- merge(wide_bird_data,site_mainland,by=c("siteID"), all.x=T)

#bird_data_mainland <- merge(bird_data_mainland,value_site_mainland, by="siteID", all.x=TRUE)

## remove site not followed enough

nb_year_p_site <- data.frame(bird_data_mainland %>% group_by(siteID, year) %>% summarise(count=n()))
nb_year_p_site <- data.frame(nb_year_p_site %>% group_by(siteID) %>% summarise(nb_year=n(),
                                                                               min_year=min(year),
                                                                               max_year=max(year)))

selected_site_mainland <- nb_year_p_site[which(nb_year_p_site$nb_year >= 5 &
                                               nb_year_p_site$min_year <= 2011 &
                                               nb_year_p_site$max_year >= 2012),]

subsite_data_mainland_trend <- bird_data_mainland[which(bird_data_mainland$siteID %in% c(selected_site_mainland$siteID)),]

saveRDS(subsite_data_mainland_trend,"output/subsite_data_mainland_trend.rds")

## get value per year per pressure

press_mainland_trend <- ddply(distinct(subsite_data_mainland_trend,siteID,year,.keep_all=TRUE), .(siteID,year),
                            .fun = function(x,pressure_data){
                              
                              pressure_subdata <- pressure_data[which(pressure_data$siteID == x$siteID),]
                              
                              pop <- (pressure_subdata$pop2020-pressure_subdata$pop2000)/21*(x$year-2000)+pressure_subdata$pop2000
                              impervious <- (pressure_subdata$impervious2018-pressure_subdata$impervious2006)/13*(x$year-2006)+pressure_subdata$impervious2006
                              treedensity <- (pressure_subdata$treedensity2018-pressure_subdata$treedensity2012)/7*(x$year-2012)+pressure_subdata$treedensity2012
                              lightpollution <- (pressure_subdata$lightpollution2013-pressure_subdata$lightpollution2000)/14*(x$year-2000)+pressure_subdata$lightpollution2000
                              woodprod <- (pressure_subdata$woodprod2010-pressure_subdata$woodprod2000)/11*(x$year-2000)+pressure_subdata$woodprod2000
                              drymatter <- (pressure_subdata$drymatter2018-pressure_subdata$drymatter2000)/19*(x$year-2000)+pressure_subdata$drymatter2000
                              temp <- (pressure_subdata$temp2020-pressure_subdata$temp2000)/21*(x$year-2000)+pressure_subdata$temp2000
                              tempspring <- (pressure_subdata$tempspring2020-pressure_subdata$tempspring2000)/21*(x$year-2000)+pressure_subdata$tempspring2000
                              tempspringvar <- (pressure_subdata$tempspringvar2020-pressure_subdata$tempspringvar2000)/21*(x$year-2000)+pressure_subdata$tempspringvar2000
                              prec <- (pressure_subdata$prec2020-pressure_subdata$prec2000)/21*(x$year-2000)+pressure_subdata$prec2000
                              precspring <- (pressure_subdata$precspring2020-pressure_subdata$precspring2000)/21*(x$year-2000)+pressure_subdata$precspring2000
                              precspringvar <- (pressure_subdata$precspringvar2020-pressure_subdata$precspringvar2000)/21*(x$year-2000)+pressure_subdata$precspringvar2000
                              humidityspring <- (pressure_subdata$humidityspring2020-pressure_subdata$humidityspring2000)/21*(x$year-2000)+pressure_subdata$humidityspring2000
                              GDP_percap <- (pressure_subdata$GDP2015_percap-pressure_subdata$GDP2000_percap)/16*(x$year-2000)+pressure_subdata$GDP2000_percap
                              GDP <- (pressure_subdata$GDP2015-pressure_subdata$GDP2000)/16*(x$year-2000)+pressure_subdata$GDP2000
                              protectedarea <- pressure_subdata$protectedarea
                              pesticide_nodu <- pressure_subdata$pesticide_nodu_kg
                              smallwoodyfeatures <- pressure_subdata$smallwoodyfeatures
                              fragmentation <- pressure_subdata$fragmentation
                              forestintegrity_cat <- pressure_subdata$forestintegrity_cat
                              shannon <- pressure_subdata$shannon
                              eulandsystem_cat <- pressure_subdata$eulandsystem_cat
                              biogeo_area <- pressure_subdata$biogeo_area
                              
                              trend_result <- data.frame(pop,impervious,treedensity,lightpollution,woodprod,drymatter,
                                                         temp,tempspring,tempspringvar,prec,precspring,precspringvar,humidityspring,
                                                         GDP_percap,GDP,protectedarea,pesticide_nodu,smallwoodyfeatures,
                                                         fragmentation,forestintegrity_cat,shannon,eulandsystem_cat,biogeo_area)
                              return(trend_result)
                            },pressure_data = value_site_mainland,
                            .progress = "text")


saveRDS(press_mainland_trend,"output/press_mainland_trend.rds") 

press_mainland_trend_scale <- press_mainland_trend
press_mainland_trend_scale$pesticide_nodu <- sqrt(press_mainland_trend_scale$pesticide_nodu)
press_mainland_trend_scale$fragmentation <- sqrt(press_mainland_trend_scale$fragmentation)
#press_mainland_trend_scale$pop <- sqrt(press_mainland_trend_scale$pop) ############### pb ici
press_mainland_trend_scale[,c("pop","impervious","treedensity","lightpollution",
                            "woodprod","drymatter","temp","tempspring","tempspringvar","prec",        
                            "precspring","precspringvar","humidityspring","GDP_percap","GDP","pesticide_nodu",
                            "smallwoodyfeatures","fragmentation","shannon")] <- scale(press_mainland_trend_scale[,c("pop","impervious","treedensity","lightpollution",
                                                                                                                  "woodprod","drymatter","temp","tempspring","tempspringvar","prec",
                                                                                                                  "precspring","precspringvar","humidityspring","GDP_percap","GDP","pesticide_nodu",
                                                                                                                  "smallwoodyfeatures","fragmentation","shannon")])



saveRDS(press_mainland_trend_scale,"output/press_mainland_trend_scale.rds") 



subsite_data_mainland_trend <- readRDS("output/subsite_data_mainland_trend.rds")
press_mainland_trend_scale <- readRDS("output/press_mainland_trend_scale.rds") 


### find minimum nubmer of site for GLMP

find_site_nubmer <- function(bird_data,pressure_data){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","scheme_code","Long_LAEA","Lat_LAEA","pop","impervious","treedensity","lightpollution",
                                                   "woodprod","drymatter","tempspring","tempspringvar",  
                                                   "precspring","precspringvar",
                                                   "protectedarea","pesticide_nodu","smallwoodyfeatures",
                                                   "fragmentation","shannon","eulandsystem_cat","biogeo_area")])
  
  unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
  
  result_site_number <- data.frame(unique_poisson_df %>% group_by(biogeo_area) %>% summarize(count=n()))
  
  return(result_site_number)
}

site_number_species_biogeo <- ddply(subsite_data_mainland_trend,
                                    .(sci_name_out),.fun=find_site_nubmer,
                                    pressure_data=press_mainland_trend_scale, .progress = "text")

### GLM Poisson per biogeo area

source("functions.R")

res_mainland_species_biogeo <- ddply(subsite_data_mainland_trend,
                                   .(sci_name_out),.fun=glm_species_biogeo,
                                   pressure_data=press_mainland_trend_scale,site_data=site_mainland_sf_reproj,
                                   formula_glmp = count_scale_all ~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon+year:eulandsystem_cat + time_effort + area_sampled_m2,
                                   formula_glmp_scheme = count_scale_all ~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon+year:eulandsystem_cat + time_effort + area_sampled_m2 + scheme_code,
                                   min_site_number_per_species=40, family="poisson", .progress = "text")


#saveRDS(res_mainland_species_biogeo,"output/res_mainland_species_biogeo.rds")
res_mainland_species_biogeo <- readRDS("output/res_mainland_species_biogeo.rds")

res_mainland_species_biogeo_quasi <- ddply(subsite_data_mainland_trend,
                                     .(sci_name_out),.fun=glm_species_biogeo,
                                     pressure_data=press_mainland_trend_scale,site_data=site_mainland_sf_reproj,
                                     formula_glmp = count_scale_all ~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                       year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                       year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                       year:shannon+year:eulandsystem_cat + time_effort + area_sampled_m2,
                                     formula_glmp_scheme = count_scale_all ~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                       year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                       year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                       year:shannon+year:eulandsystem_cat + time_effort + area_sampled_m2 + scheme_code,
                                     min_site_number_per_species=40, family="quasipoisson", .progress = "text")


#saveRDS(res_mainland_species_biogeo_quasi,"output/res_mainland_species_biogeo_quasi.rds")
res_mainland_species_biogeo <- readRDS("output/res_mainland_species_biogeo_quasi.rds")


### Plot spatial by species 
res_species_biogeo_df <- res_mainland_species_biogeo[which(res_mainland_species_biogeo$sci_name_out == unique(res_mainland_species_biogeo$sci_name_out)[12]),]

res_species_biogeo_sf <- merge(grid_eu_mainland_biogeo,res_species_biogeo_df, by="biogeo_area",all.x=TRUE)
ggplot() + geom_sf() +  geom_sf(data=res_species_biogeo_sf, aes(fill=exp(`year:treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))


### Plot estimate by pressure

res_mainland_species_eu <- res_mainland_species_biogeo[which(res_mainland_species_biogeo$biogeo_area=="europe"),]
increasing_sp <- res_mainland_species_eu$sci_name_out[which(res_mainland_species_eu$year>0)]
decreasing_sp <- res_mainland_species_eu$sci_name_out[which(res_mainland_species_eu$year<0)]


res_mainland_species_biogeo_long <- melt(res_mainland_species_eu, id.vars=c("sci_name_out","biogeo_area"))

res_mainland_species_biogeo_long <- res_mainland_species_biogeo_long[which(res_mainland_species_biogeo_long$variable != "(Intercept)"),]

ggplot(res_mainland_species_biogeo_long, aes(x = variable, y = exp(value))) + 
  geom_point(position = position_jitterdodge(jitter.width=0.15,dodge.width = 0.6), 
             alpha = 0.2, size = 3, stroke = 0, na.rm = TRUE, aes(col=variable)) +
  geom_violin(width = 0.6, alpha = 0.1, na.rm = TRUE, aes(fill = variable)) +
  geom_boxplot(width = 0.6, alpha = 0.1, na.rm = TRUE,outlier.shape = NA,aes(fill = variable)) + 
  #scale_fill_manual(values = c("trees" = "#29c200","no_tree" = "#b1b1b1")) +
  #scale_color_manual(values = c("trees" = "#29c200","no_tree" = "#b1b1b1")) +
  theme_ggstatsplot() +
  labs(y="Estimate") + theme(axis.title.x = element_blank(),
                             axis.text.x = element_text(angle=45, hjust = 1),
                             legend.position = "none")



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


community_data <- merge(subsite_data_mainland_trend,STI)


























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
