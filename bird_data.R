# load bird data

bird_data <- read.table(file = "raw_data/pecbms_bird_data/all_count_bird_data_bv.txt", header = TRUE, sep = "\t")

# load sites

sites <- read.table(file = "raw_data/pecbms_bird_data/sites.txt", header = TRUE, sep = "\t")

# plot sampled site on a map

sample_sites <- sites[which(sites$siteID %in% unique(bird_data$siteID)),] # idem sites

sample_sites_line <- sample_sites[which(sample_sites$method=="Line transect"),]

sample_sites_point <- sample_sites[which(sample_sites$method=="Point counts"),]

sample_sites_territory <- sample_sites[which(sample_sites$method=="Territory mapping"),]

sf::sf_use_s2(FALSE)

worldmap <- ne_countries(scale = 'medium', type = 'countries',returnclass = 'sf')

europe_cropped <- st_crop(worldmap[worldmap$sovereignt %in% c("Austria","Belgium","Bulgaria","Cyprus","Czech Republic","Denmark",
                                                              "Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
                                                              "Italy","Latvia","Lithuania","Netherlands","Norway","Portugal","Poland","Romania",
                                                              "Slovakia","Spain","Sweden","Switzerland","United Kingdom",
                                                              "Croatia","Republic of Serbia","Albania","Slovenia","Bosnia and Herzegovina","Kosovo",
                                                              "Montenegro", "Belarus","Ukraine","Russia","Moldova","Macedonia","Luxembourg"),],
                          xmin = -12, xmax = 35,ymin = 30, ymax = 73)

europe_cropped$fill_param <- rep(NA,nrow(europe_cropped))

europe_cropped$fill_param[europe_cropped$sovereignt %in% c("Austria","Belgium","Bulgaria","Cyprus","Czech Republic","Denmark",
                                                           "Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
                                                           "Italy","Latvia","Lithuania","Netherlands","Norway","Portugal","Poland","Romania",
                                                           "Slovakia","Spain","Sweden","Switzerland","United Kingdom","Slovenia","Luxembourg")] <- "PECBMS member (in 2016)"

# Simplify map

europe_map_simpl <- ms_simplify(europe_cropped, keep = 0.5,
                                keep_shapes = FALSE)


# Map of high input farm cover

ggplot() + geom_sf(data = europe_map_simpl, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites, aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)

ggplot() + geom_sf(data = europe_map_simpl, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites_line, aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)

ggplot() + geom_sf(data = europe_map_simpl, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites_point, aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)

ggplot() + geom_sf(data = europe_map_simpl, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites_territory, aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)


# country example

fr_cropped <- st_crop(worldmap[worldmap$sovereignt %in% c("France"),],
                          xmin = -12, xmax = 35,ymin = 30, ymax = 73)

fr_cropped$fill_param <- rep("a",nrow(fr_cropped))

ggplot() + geom_sf(data = fr_cropped, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites[which(sample_sites$scheme_code=="FR"),], aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)

ge_cropped <- st_crop(worldmap[worldmap$sovereignt %in% c("Germany"),],
                      xmin = -12, xmax = 35,ymin = 30, ymax = 73)

ge_cropped$fill_param <- rep("a",nrow(fr_cropped))

ggplot() + geom_sf(data = ge_cropped, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites[which(sample_sites$scheme_code=="DE"),], aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)


# Distribution type sampling

ggplot() +
  geom_density(data=sample_sites, aes(x=area_sampled_m2, col=method)) +
  xlim(0,5000000) + theme_modern()

