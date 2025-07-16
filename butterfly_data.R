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
butterfly_data_preclean <- butterfly_data_preclean[which(butterfly_data_preclean$count_corrected <= quantile(butterfly_data_preclean$count_corrected,0.99)),]

butterfly_data_clean <- butterfly_data_preclean

saveRDS(butterfly_data_clean,"output/butterfly_data_clean.rds")

# impossible with rbms because only yearly count
# test for species available in GBI report

sub_species <- c("Thymelicus acteon","Euphydryas aurinia","Ochlodes sylvanus","Erynnis tages","Anthocharis cardamines",
                 "Cupido minimus","Lysandra coridon","Maniola jurtina","Polyommatus icarus","Cyaniris semiargus",
                 "Lycanea phlaeas","Spialia sertorius","Coenonympha pamphilus","Lysandra bellargus","Lasiommata megera","Phengaris arion")

butterfly_data_test <- droplevels(butterfly_data[which(butterfly_data$species_name %in% sub_species),])

butterfly_data_test <- butterfly_data_test[which(butterfly_data_test$count_corrected <= 31.33),]


sites <- read.csv(file = "raw_data/butterfly/ebms_transect_coord.csv", header = TRUE)

site_mainland <- sites[which(sites$transect_lon > 2100000),] # remove canary islands

butterfly_data_test <- butterfly_data_test[which(butterfly_data_test$transect_id %in% unique(site_mainland$transect_id)),]

site_mainland_sf <- st_as_sf(site_mainland, coords = c("transect_lon","transect_lat"))

st_crs(site_mainland_sf) <- 3035

site_mainland_sf_reproj <- st_transform(site_mainland_sf,crs(grid_eu_mainland_outline))

site_mainland$Long_LAEA <- st_coordinates(site_mainland_sf_reproj)[,1]
site_mainland$Lat_LAEA <- st_coordinates(site_mainland_sf_reproj)[,2]

site_mainland_sf_reproj_butterfly <- site_mainland_sf_reproj




nb_year_p_site <- data.frame(butterfly_data_test %>% group_by(transect_id, year) %>% summarise(count=n()))
nb_year_p_site <- data.frame(nb_year_p_site %>% group_by(transect_id) %>% summarise(nb_year=n(),
                                                                                    min_year=min(year),
                                                                                    max_year=max(year)))

selected_site_mainland <- nb_year_p_site[which(nb_year_p_site$nb_year >= 5 & nb_year_p_site$max_year >= 2011),]

subsite_data_mainland_trend <- butterfly_data_test[which(butterfly_data_test$transect_id %in% c(selected_site_mainland$transect_id)),]


temp_sp <- list(c(2005:2020),c(1994:2020),c(1990:2020),c(1999:2020),c(1990:2020),
                c(2003:2020),c(2002:2020),c(1990:2020),c(1990:2020),c(2003:2020),
                c(1990:2020),c(2003:2020),c(1990:2020),c(1995:2020),c(1990:2020),c(2010:2020))

result_butterfly_test <- data.frame(name=NA,coef=NA,se=NA,pval=NA)

for(i in 1:16){
  butterfly_data_sub <- droplevels(subsite_data_mainland_trend[which(subsite_data_mainland_trend$species_name == sub_species[i] &
                                                                       subsite_data_mainland_trend$year %in% temp_sp[[i]]),])
  
  species_data_year <- merge(butterfly_data_sub, site_mainland[,c("transect_id","Long_LAEA","Lat_LAEA","transect_length")], by =c("transect_id"), all.x=TRUE)
  
  poisson_df <- na.omit(species_data_year)
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - min(poisson_df$year)
  
  if(length(table(poisson_df$transect_length)) > length(unique(poisson_df$bms_id))){
    one_scheme_time_area <- 0 
    poisson_df$transect_length <- scale(poisson_df$transect_length)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_corrected_scale_all <- poisson_df$count_corrected#scales::rescale(poisson_df$count_corrected)
  
  formula_gam <- "count_corrected_scale_all ~ year"
  
  col_names <- c("(Intercept)","year")
  
  if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 0){
    global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                      family="quasipoisson", data=poisson_df)
  }
  
  result_butterfly_test <- rbind(result_butterfly_test,data.frame(name=sub_species[i],coef=summary(global_mod)$p.coef[2],se=summary(global_mod)$se[2],pval=summary(global_mod)$p.pv[2]))
  
}
result_butterfly_test <- result_butterfly_test[-1,]


temp_sp_eu27 <- list(c(2005:2020),c(2010:2020),c(1991:2020),c(2004:2020),c(1991:2020),
                     c(2006:2020),c(2005:2020),c(1991:2020),c(1991:2020),c(2003:2020),
                     c(1991:2020),c(2010:2020),c(1991:2020),c(2005:2020),c(1994:2020))

result_butterfly_test_eu27 <- data.frame(name=NA,coef27=NA,se27=NA,pval27=NA)

for(i in 1:15){
  butterfly_data_sub <- droplevels(subsite_data_mainland_trend[which(subsite_data_mainland_trend$species_name == sub_species[i] &
                                                                       subsite_data_mainland_trend$year %in% temp_sp_eu27[[i]]),])
  
  species_data_year <- merge(butterfly_data_sub, site_mainland[,c("transect_id","Long_LAEA","Lat_LAEA","transect_length")], by =c("transect_id"), all.x=TRUE)
  
  poisson_df <- na.omit(species_data_year)
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - min(poisson_df$year)
  
  if(length(table(poisson_df$transect_length)) > length(unique(poisson_df$bms_id))){
    one_scheme_time_area <- 0 
    poisson_df$transect_length <- scale(poisson_df$transect_length)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_corrected_scale_all <- poisson_df$count_corrected#scales::rescale(poisson_df$count_corrected)
  
  formula_gam <- "count_corrected_scale_all ~ year"
  
  col_names <- c("(Intercept)","year")
  
  if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 0){
    global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                      family="quasipoisson", data=poisson_df)
  }
  
  result_butterfly_test_eu27 <- rbind(result_butterfly_test_eu27,data.frame(name=sub_species[i],coef27=summary(global_mod)$p.coef[2],se27=summary(global_mod)$se[2],pval27=summary(global_mod)$p.pv[2]))
  
}
result_butterfly_test_eu27 <- result_butterfly_test_eu27[-1,]


result_butterfly_GBI <- data.frame(name=sub_species,
                                   nb_year_eu = c(16,27,31,22,31,
                                               18,19,31,31,18,
                                               31,18,31,26,31,11),
                                   coef_eu = c(145,42,41,1,0,-3,-24,-26,-42,-43,-45,-60,-61,-64,-68,-82),
                                   signif_eu = c("Uncertain","Uncertain","Stable","Stable","Stable",
                                                 "Uncertain","Uncertain","Moderate decline","Moderate decline","Uncertain",
                                                 "Moderate decline","Uncertain","Moderate decline","Moderate decline","Moderate decline",
                                                 "Strong decline"),
                                   nb_year_eu27 = c(16,11,30,17,30,
                                                    15,16,30,30,18,
                                                    30,11,30,16,27,NA),
                                   coef_eu27 = c(145,29,18,-18,116,-22,-18,-28,-57,-46,5,-69,-65,-61,-60,NA),
                                   signif_eu27 = c("Uncertain","Uncertain","Stable","Uncertain","Moderate increase",
                                                   "Uncertain","Uncertain","Stable","Moderate decline","Uncertain",
                                                   "Stable","Moderate decline","Moderate decline","Moderate decline","Moderate decline",NA))

result_butterfly_GBI$trend_eu <- result_butterfly_GBI$coef_eu/result_butterfly_GBI$nb_year_eu
result_butterfly_GBI$trend_eu27 <- result_butterfly_GBI$coef_eu27/result_butterfly_GBI$nb_year_eu27

result_butterfly_GBI <- merge(result_butterfly_GBI,result_butterfly_test, by="name")
result_butterfly_GBI <- merge(result_butterfly_GBI,result_butterfly_test_eu27, by="name", all.x=TRUE)
result_butterfly_GBI$coef_signif <- "Uncertain"
result_butterfly_GBI$coef_signif[which(result_butterfly_GBI$pval < 0.05 & result_butterfly_GBI$coef > 0)] <- "Increase"
result_butterfly_GBI$coef_signif[which(result_butterfly_GBI$pval < 0.05 & result_butterfly_GBI$coef < 0)] <- "Decrease"
result_butterfly_GBI$coef27_signif <- "Uncertain"
result_butterfly_GBI$coef27_signif[which(result_butterfly_GBI$pval27 < 0.05 & result_butterfly_GBI$coef27 > 0)] <- "Increase"
result_butterfly_GBI$coef27_signif[which(result_butterfly_GBI$pval27 < 0.05 & result_butterfly_GBI$coef27 < 0)] <- "Decrease"

ggplot(result_butterfly_GBI,aes(x=name)) + 
  geom_bar(aes(y=trend_eu/100, fill=signif_eu),stat="identity", alpha=.6, width=.4) +
  scale_fill_manual(values = c("Uncertain" = "#bdbdbdff","Stable"="#00beffff", "Moderate decline" = "#f98071ff", "Strong decline"="#b12121ff")) +
  coord_flip() + ylim(c(-0.12,0.12)) +
  xlab("") + ylab("Trend Europe") +
  theme_bw() + theme(legend.position = "none")

ggplot(result_butterfly_GBI,aes(x=name)) + 
  geom_bar(aes(y=exp(coef)-1, fill=coef_signif),stat="identity", alpha=.6, width=.4) +
  scale_fill_manual(values = c("Uncertain" = "#bdbdbdff","Decrease" = "#f98071ff", "Increase"="#a1cd5aff")) +
  coord_flip() + ylim(c(-0.12,0.12)) +
  xlab("") + ylab("Calculated trend") +
  theme_bw() + theme(legend.position = "none")

ggplot(result_butterfly_GBI,aes(x=name)) + 
  geom_bar(aes(y=trend_eu27/100, fill=signif_eu27),stat="identity", alpha=.6, width=.4) +
  scale_fill_manual(values = c("Uncertain" = "#bdbdbdff","Stable"="#00beffff", "Moderate decline" = "#f98071ff", "Moderate increase"="#a1cd5aff")) +
  coord_flip() + ylim(c(-0.12,0.12)) +
  xlab("") + ylab("Trend EU27") +
  theme_bw() + theme(legend.position = "none")

ggplot(result_butterfly_GBI,aes(x=name)) + 
  geom_bar(aes(y=exp(coef27)-1, fill=coef27_signif),stat="identity", alpha=.6, width=.4) +
  scale_fill_manual(values = c("Uncertain" = "#bdbdbdff","Decrease" = "#f98071ff", "Increase"="#a1cd5aff")) +
  coord_flip() + ylim(c(-0.12,0.12)) +
  xlab("") + 
  theme_bw() + theme(legend.position = "none")


ggsave(
  "output/compare_trend1.png",
  width = 4,
  height = 6,
  dpi = 300,
)
