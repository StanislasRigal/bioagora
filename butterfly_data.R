# load butterfly data

butterfly_data <- read.csv(file = "raw_data/butterfly/ebms_count.csv", header = TRUE)

# load sites

sites <- read.csv(file = "raw_data/butterfly/ebms_transect_coord.csv", header = TRUE)

# load visits

visites <- read.csv(file = "raw_data/butterfly/ebms_visit.csv", header = TRUE)

# plot sampled site on a map

sample_sites <- sites[which(sites$transect_id %in% unique(butterfly_data$transect_id)),] # idem sites

sf::sf_use_s2(FALSE)

worldmap <- ne_countries(scale = 'medium', type = 'countries',returnclass = 'sf')

europe_cropped <- st_crop(worldmap[worldmap$sovereignt %in% c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark",
                                                              "Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
                                                              "Italy","Latvia","Lithuania","Netherlands","Norway","Portugal","Poland","Romania",
                                                              "Slovakia","Spain","Sweden","Switzerland","United Kingdom",
                                                              "Croatia","Republic of Serbia","Albania","Slovenia","Bosnia and Herzegovina","Kosovo",
                                                              "Montenegro", "Belarus","Ukraine","Russia","Moldova","North Macedonia","Luxembourg"),],
                          xmin = -12, xmax = 35,ymin = 30, ymax = 73)

europe_cropped <- st_transform(europe_cropped,crs=3035)

# Simplify map

europe_map_simpl <- ms_simplify(europe_cropped, keep = 0.5,
                                keep_shapes = FALSE)


# Map of monitoring sites

ggplot() + geom_sf(data = europe_map_simpl) + geom_point(data=sample_sites, aes(x=transect_lon, y=transect_lat),size=0.5)


# country example

fr_cropped <- st_crop(worldmap[worldmap$sovereignt %in% c("France"),],
                          xmin = -12, xmax = 35,ymin = 30, ymax = 73)

fr_cropped <- st_transform(fr_cropped,crs=3035)
ggplot() + geom_sf(data = fr_cropped) + geom_point(data=sample_sites[which(sample_sites$bms_id=="FRBMS"),], aes(x=transect_lon, y=transect_lat),size=0.5)


# Account for sampling effort

sampling_effort <- visites %>% group_by(transect_id, year) %>% summarize(nb_visit=n())

butterfly_data <- merge(butterfly_data, sampling_effort, by=c("transect_id","year"), all.x=TRUE)
butterfly_data$count_corrected <- butterfly_data$site_yearly_count/butterfly_data$nb_visit

butterfly_data <- butterfly_data[which(!is.na(butterfly_data$count_corrected)),]

# Look at species frequency and distribution to select species to keep

abundance_butterfly_data_summary <- butterfly_data %>% group_by(species_name, transect_id) %>% summarize(abundance=sum(count_corrected))

butterfly_data_frequency <- abundance_butterfly_data_summary %>% group_by(species_name) %>% summarize(freq=sum(abundance))
butterfly_data_frequency <- butterfly_data_frequency[order(-butterfly_data_frequency$freq),]
butterfly_data_frequency$cum_abundance <- cumsum(butterfly_data_frequency$freq)
butterfly_data_frequency$frequency <- butterfly_data_frequency$cum_abundance/sum(butterfly_data_frequency$freq)
butterfly_data_frequency <- butterfly_data_frequency %>% mutate(species_name = fct_reorder(species_name, frequency))
butterfly_data_frequency$selected <- "no"
butterfly_data_frequency$selected[which(butterfly_data_frequency$frequency < 0.99)] <- "yes_freq"
butterfly_data_frequency$selected[which(butterfly_data_frequency$freq > 1000 & butterfly_data_frequency$frequency >= 0.99)] <- "yes_abund"

butterfly_data_distribution <- abundance_butterfly_data_summary %>% group_by(species_name) %>% summarize(distrib=n())
butterfly_data_distribution <- butterfly_data_distribution[order(-butterfly_data_distribution$distrib),]
butterfly_data_distribution$frequency <- butterfly_data_distribution$distrib/length(unique(abundance_butterfly_data_summary$transect_id))
butterfly_data_distribution <- butterfly_data_distribution %>% mutate(species_name = fct_reorder(species_name, frequency))
butterfly_data_distribution$selected <- "no"
butterfly_data_distribution$selected[which(butterfly_data_distribution$frequency > 0.01)] <- "yes_freq_occurence"
butterfly_data_distribution$selected[which(butterfly_data_distribution$distrib > 100 & butterfly_data_distribution$frequency <= 0.01)] <- "yes_abund_occurrence"

write.csv(butterfly_data_distribution,"output/butterfly_data_distribution.csv", row.names = FALSE)

ggplot() +
  geom_density(data=butterfly_data_frequency, aes(x=freq)) +
  xlim(0,100000) + theme_modern()
ggplot(butterfly_data_frequency, aes(x=species_name, y=frequency)) +
  geom_bar(stat="identity", aes(fill=selected), alpha=.6, width=.4) + 
  theme_modern() + xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot() +
  geom_density(data=butterfly_data_distribution, aes(x=distrib)) + theme_modern()
ggplot(butterfly_data_distribution, aes(x=species_name, y=frequency)) +
  geom_bar(stat="identity", aes(fill=selected), alpha=.6, width=.4) + 
  theme_modern() + xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# as we will work with spatial data, distribution more important than abundance
selected_species_butterfly <- as.character(droplevels(butterfly_data_distribution$species_name[which(butterfly_data_distribution$selected=="yes_freq_occurence")])) 

saveRDS(selected_species_butterfly,"output/selected_species_butterfly.rds")

# Select time period of interest

time_period_butterfly_data <- butterfly_data[,c("transect_id","year")] %>% group_by(transect_id,year) %>% summarize(nb_sp=n())
time_period_butterfly_data <- merge(time_period_butterfly_data,sites, by="transect_id", all.x=T)
time_period_butterfly_data_site <- time_period_butterfly_data %>% group_by(year) %>% summarize(nb_site=n())
time_period_butterfly_data_country <- time_period_butterfly_data %>% group_by(year,bms_id) %>% summarize(nb_site=n())
time_period_butterfly_data_country <- time_period_butterfly_data_country %>% group_by(year) %>% summarize(nb_country=n())
time_period_butterfly_data <- merge(time_period_butterfly_data_site,time_period_butterfly_data_country,by="year")

ggplot(time_period_butterfly_data, aes(x=year)) +
  geom_line(aes(y=nb_site), col="grey") +
  geom_line(aes(y=nb_country*400), col="red") +
  scale_y_continuous(name = "Number of monitored sites", sec.axis = sec_axis(~./400, name="Number of countries")) +
  theme_modern() + xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(
  "output/site_country_time_butterfly.png",
  width = 7,
  height = 5,
  dpi = 300,
)

# same as for birds
selected_year <- 2000:2021#time_period_butterfly_data$year[which(time_period_butterfly_data$nb_country>=15)]

# check scheme by country

butterfly_data_preclean <- droplevels(butterfly_data[which(butterfly_data$species_name %in% selected_species_butterfly & butterfly_data$year %in% selected_year),])

scheme_sites <- sites[which(sites$transect_id %in% unique(butterfly_data_preclean$transect_id)),]

scheme_sites <- scheme_sites %>% group_by(bms_id) %>% summarize(count=n())

# remove records of large groups
butterfly_data_preclean <- butterfly_data_preclean[which(butterfly_data_preclean$count_corrected <= quantile(butterfly_data_preclean$count_corrected,0.999)),]

butterfly_data_clean <- butterfly_data_preclean

saveRDS(butterfly_data_clean,"output/butterfly_data_clean.rds")
