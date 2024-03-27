# Load pressure data

grid_eu <- st_read("output/grid_eu_gdp.gpkg")
grid_eu_bioregion <- st_read("output/grid_eu_bioregion.gpkg")
grid_eu$biogeo_area <- grid_eu_bioregion$biogeo_area

st_write(grid_eu,"output/grid_eu_all.gpkg")

grid_eu_all <- st_read("output/grid_eu_all.gpkg")

bird_data_clean <- readRDS("output/bird_data_clean.rds")

# Test with subregion (e.g. Catalonia)

bird_data_cat <- bird_data_clean[which(bird_data_clean$scheme_code == "ES_CAT"),]

sites <- read.table(file = "raw_data/pecbms_bird_data/sites.txt", header = TRUE, sep = "\t")

site_cat <- sites[which(sites$siteID %in% unique(bird_data_cat$siteID)),]

site_cat_sf <- st_as_sf(site_cat, coords = c("Long_WGS84","Lat_WGS84"))
st_crs(site_cat_sf) <- 4326

grid_eu_cat <- grid_eu_all[which(grid_eu_all$NUTS2021_2 == "ES511"),]

st_write(grid_eu_cat,"output/grid_eu_cat_test.gpkg")
grid_eu_cat <- st_read("output/grid_eu_cat.gpkg")

site_cat_sf_reproj <- st_transform(site_cat_sf,crs(grid_eu_cat))

ggplot(grid_eu_cat) +
  geom_sf() +
  geom_sf(data=site_cat_sf_reproj)

site_cat_buffer <- st_buffer(site_cat_sf_reproj, dist = sqrt(site_cat_sf_reproj$area_sampled_m2/pi))

ggplot(grid_eu_cat) +
  geom_sf() +
  geom_sf(data=site_cat_buffer)

area_site_cat <-  st_intersection(site_cat_buffer, grid_eu_cat)

ggplot(area_site_cat) +
  geom_sf() 

area_site_cat$area <- as.numeric(st_area(area_site_cat))

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
                                                                                  tempspringmin2000 = weighted.mean(tempspringmin2000,area), tempspringmin2020 = weighted.mean(tempspringmin2020,area), tempspringminvar2000 = weighted.mean(tempspringminvar2000,area), tempspringminvar2020 = weighted.mean(tempspringminvar2020,area),
                                                                                  tempspringmax2000 = weighted.mean(tempspringmax2000,area), tempspringmax2020 = weighted.mean(tempspringmax2020,area), tempspringmaxvar2000 = weighted.mean(tempspringmaxvar2000,area), tempspringmaxvar2020 = weighted.mean(tempspringmaxvar2020,area),
                                                                                  prec2000 = weighted.mean(prec2000,area), prec2020 = weighted.mean(prec2020,area), precspring2000 = weighted.mean(precspring2000,area), precspring2020 = weighted.mean(precspring2020,area),
                                                                                  precspringvar2000 = weighted.mean(precspringvar2000), precspringvar2020 = weighted.mean(precspringvar2020,area),
                                                                                  humidity2000 = weighted.mean(humidity2000,area), humidity2020 = weighted.mean(humidity2020,area), humidityspring2000 = weighted.mean(humidityspring2000,area),
                                                                                  humidityspring2020 = weighted.mean(humidityspring2020,area), humidityspringvar2000 = weighted.mean(humidityspringvar2000,area), humidityspringvar2020 = weighted.mean(humidityspringvar2020,area),
                                                                                  shannon = weighted.mean(shannon,area), GDP2000 = weighted.mean(GDP2000,area), GDP2015 = weighted.mean(GDP2015,area), biogeo_area = biogeo_area[which.max(area)])

value_site_cat$diff_pop_perc <- (value_site_cat$pop2020-value_site_cat$pop2000)/value_site_cat$pop2000
value_site_cat$diff_impervious <- (value_site_cat$impervious2018-value_site_cat$impervious2006)/value_site_cat$impervious2006
value_site_cat$diff_treedensity <- (value_site_cat$treedensity2018-value_site_cat$treedensity2012)/value_site_cat$treedensity2012
value_site_cat$diff_lightpollution <- (value_site_cat$lightpollution2013-value_site_cat$lightpollution2000)/value_site_cat$lightpollution2000
value_site_cat$diff_woodprod <- (value_site_cat$woodprod2010-value_site_cat$woodprod2000)/value_site_cat$woodprod2000
value_site_cat$diff_drymatter <- (value_site_cat$drymatter2018-value_site_cat$drymatter2000)/value_site_cat$drymatter2000
value_site_cat$GDP2000_percap <- value_site_cat$GDP2000/value_site_cat$pop2000
pop2015 <- (value_site_cat$pop2020-value_site_cat$pop2000)/20*2015+value_site_cat$pop2020-(value_site_cat$pop2020-value_site_cat$pop2000)/20*2020
value_site_cat$GDP2015_percap <- value_site_cat$GDP2015/pop2015
value_site_cat$diff_gdp_percap <- (value_site_cat$GDP2015_percap-value_site_cat$GDP2000_percap)/value_site_cat$GDP2000_percap

bird_data_cat <- merge(bird_data_cat,value_site_cat, by="siteID", all.x=TRUE)
# compute trend species

