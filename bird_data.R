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


# Map of monitoring sites

ggplot() + geom_sf(data = europe_map_simpl, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites, aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)

ggplot() + geom_sf(data = europe_map_simpl, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites_line, aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)

ggplot() + geom_sf(data = europe_map_simpl, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites_point, aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)

ggplot() + geom_sf(data = europe_map_simpl, aes(fill = fill_param)) + scale_fill_discrete(na.value="lightgrey")+
  theme_void()+ coord_sf(datum = NA) + geom_point(data=sample_sites_territory, aes(x=Long_WGS84, y=Lat_WGS84),size=0.5)

ggplot() + geom_sf(data = europe_cropped[which(europe_cropped$admin == "Finland"),])+
  theme_void()+ coord_sf(datum = NA) + 
  geom_point(data=sample_sites_point[which(sample_sites_point$country == "Finland"),], aes(x=Long_WGS84, y=Lat_WGS84),size=0.5) +
  geom_point(data=sample_sites_line[which(sample_sites_line$country == "Finland"),], aes(x=Long_WGS84, y=Lat_WGS84),size=0.5, col="red")

ggplot() + geom_sf(data = europe_cropped[which(europe_cropped$admin == "Spain"),])+
  theme_void()+ coord_sf(datum = NA, xlim = c(0.15, 3.35),ylim = c(40.55, 43)) + 
  geom_point(data=sample_sites_point[which(sample_sites_point$country == "Spain"),], aes(x=Long_WGS84, y=Lat_WGS84),size=0.5) +
  geom_point(data=sample_sites_line[which(sample_sites_line$country == "Spain"),], aes(x=Long_WGS84, y=Lat_WGS84),size=0.5, col="red")


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

# Look at species frequency and distribution to select species to keep

abundance_bird_data <- bird_data[which(bird_data$count>0),]

abundance_bird_data_summary <- abundance_bird_data %>% group_by(sci_name_out, siteID) %>% summarize(abundance=sum(count))

bird_data_frequency <- abundance_bird_data_summary %>% group_by(sci_name_out) %>% summarize(freq=sum(abundance))
bird_data_frequency <- bird_data_frequency[order(-bird_data_frequency$freq),]
bird_data_frequency$cum_abundance <- cumsum(bird_data_frequency$freq)
bird_data_frequency$frequency <- bird_data_frequency$cum_abundance/sum(bird_data_frequency$freq)
bird_data_frequency <- bird_data_frequency %>% mutate(sci_name_out = fct_reorder(sci_name_out, frequency))
bird_data_frequency$selected <- "no"
bird_data_frequency$selected[which(bird_data_frequency$frequency < 0.99)] <- "yes_freq"
bird_data_frequency$selected[which(bird_data_frequency$freq > 1000 & bird_data_frequency$frequency >= 0.99)] <- "yes_abund"

bird_data_distribution <- abundance_bird_data_summary %>% group_by(sci_name_out) %>% summarize(distrib=n())
bird_data_distribution <- bird_data_distribution[order(-bird_data_distribution$distrib),]
bird_data_distribution$frequency <- bird_data_distribution$distrib/length(unique(abundance_bird_data_summary$siteID))
bird_data_distribution <- bird_data_distribution %>% mutate(sci_name_out = fct_reorder(sci_name_out, frequency))
bird_data_distribution$selected <- "no"
bird_data_distribution$selected[which(bird_data_distribution$frequency > 0.01)] <- "yes_freq_occurence"
bird_data_distribution$selected[which(bird_data_distribution$distrib > 100 & bird_data_distribution$frequency <= 0.01)] <- "yes_abund_occurrence"

write.csv(bird_data_distribution,"output/bird_data_distribution.csv", row.names = FALSE)

ggplot() +
  geom_density(data=bird_data_frequency, aes(x=freq)) +
  xlim(0,100000) + theme_modern()
ggplot(bird_data_frequency, aes(x=sci_name_out, y=frequency)) +
  geom_bar(stat="identity", aes(fill=selected), alpha=.6, width=.4) + 
  theme_modern() + xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot() +
  geom_density(data=bird_data_distribution, aes(x=distrib)) + theme_modern()
ggplot(bird_data_distribution, aes(x=sci_name_out, y=frequency)) +
  geom_bar(stat="identity", aes(fill=selected), alpha=.6, width=.4) + 
  theme_modern() + xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# as we will work with spatial data, distribution more important than abundance
selected_species <- as.character(droplevels(bird_data_distribution$sci_name_out[which(bird_data_distribution$selected=="yes_freq_occurence")])) 

saveRDS(selected_species,"output/selected_species.rds")

# Select time period of interest

time_period_bird_data <- bird_data[,c("siteID","year")] %>% group_by(siteID,year) %>% summarize(nb_sp=n())
time_period_bird_data <- merge(time_period_bird_data,sites, by="siteID", all.x=T)
time_period_bird_data_site <- time_period_bird_data %>% group_by(year) %>% summarize(nb_site=n())
time_period_bird_data_country <- time_period_bird_data %>% group_by(year,country) %>% summarize(nb_site=n())
time_period_bird_data_country <- time_period_bird_data_country %>% group_by(year) %>% summarize(nb_country=n())
time_period_bird_data <- merge(time_period_bird_data_site,time_period_bird_data_country,by="year")

ggplot(time_period_bird_data, aes(x=year)) +
  geom_line(aes(y=nb_site), col="grey") +
  geom_line(aes(y=nb_country*400), col="red") +
  scale_y_continuous(name = "Number of monitored sites", sec.axis = sec_axis(~./400, name="Number of countries")) +
  theme_modern() + xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(
  "output/site_country_time.png",
  width = 7,
  height = 5,
  dpi = 300,
)

# more than 15 countries
selected_year <- time_period_bird_data$year[which(time_period_bird_data$nb_country>=15)]

# check scheme by country

bird_data_preclean <- droplevels(bird_data[which(bird_data$sci_name_out %in% selected_species & bird_data$year %in% selected_year),])

scheme_sites <- sites[which(sites$siteID %in% unique(bird_data_preclean$siteID)),]

scheme_sites <- scheme_sites %>% group_by(country, scheme_code, method) %>% summarize(count=n())

# proceed to data cleaning and formating according to the PECBMS metadata

# remove line transect for Czechia (too short 2018-2021), keep both for Catalonia and Finland (longer and better coverage), only one scheme for other countries
sites_to_remove <- sites$siteID[which(sites$scheme_code == "CZ_LSD")]

bird_data_preclean <- bird_data_preclean[which(!(bird_data_preclean$siteID %in% sites_to_remove)),]

# remove 0 value from territory survey
bird_data_preclean <- bird_data_preclean[which(bird_data_preclean$count>0),]

# merge with site information
bird_data_preclean <- merge(bird_data_preclean, sites[,c("scheme", "scheme_code", "siteID", "Long_WGS84", "Lat_WGS84",
                                                   "time_effort","area_sampled_m2", "country", "method", "count_unit", "species")], by="siteID", all.x=TRUE)

# multiply by 2 for pair and territories (as stated in the PECBMS_SLD_information_v1.pdf)
bird_data_preclean$count[which(bird_data_preclean$count_unit %in% c("pairs","territories"))] <- 2*bird_data_preclean$count[which(bird_data_preclean$count_unit %in% c("pairs","territories"))]

# remove records of large groups
bird_data_preclean <- bird_data_preclean[which(bird_data_preclean$count <= quantile(bird_data_preclean$count,0.9995)),]

bird_data_clean <- bird_data_preclean

saveRDS(bird_data_clean,"output/bird_data_clean.rds")
