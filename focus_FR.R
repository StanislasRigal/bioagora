
### Load previously produced datasets

bird_data_mainland <- readRDS("output/bird_data_mainland.rds")
grid_eu_mainland_biogeo <- st_read("output/grid_eu_mainland_biogeo.gpkg")
grid_eu_mainland_outline <- st_read("output/grid_eu_mainland_outline.gpkg")
press_mainland_trend_scale <- readRDS("output/press_mainland_trend_scale.rds")
press_mainland_trend <- readRDS("output/press_mainland_trend.rds")
site_mainland_sf_reproj <- readRDS("output/site_mainland_sf_reproj.rds")
subsite_data_mainland_trend <- readRDS("output/subsite_data_mainland_trend.rds")


press_mainland_trend_FR <- press_mainland_trend[which(press_mainland_trend$siteID %in% unique(bird_data_mainland[which(bird_data_mainland$scheme_code=="FR"),]$siteID)),]

press_mainland_trend_FR$eufarm <- NA
press_mainland_trend_FR$eufarm[which(press_mainland_trend_FR$eulandsystem_farmland_low > press_mainland_trend_FR$eulandsystem_farmland_medium & press_mainland_trend_FR$eulandsystem_farmland_low > press_mainland_trend_FR$eulandsystem_farmland_high)] <- "low"
press_mainland_trend_FR$eufarm[which(press_mainland_trend_FR$eulandsystem_farmland_medium > press_mainland_trend_FR$eulandsystem_farmland_low & press_mainland_trend_FR$eulandsystem_farmland_medium > press_mainland_trend_FR$eulandsystem_farmland_high)] <- "medium"
press_mainland_trend_FR$eufarm[which(press_mainland_trend_FR$eulandsystem_farmland_high > press_mainland_trend_FR$eulandsystem_farmland_low & press_mainland_trend_FR$eulandsystem_farmland_high > press_mainland_trend_FR$eulandsystem_farmland_medium)] <- "high"
press_mainland_trend_FR$eufarm <- factor(press_mainland_trend_FR$eufarm , levels = c("low","medium","high"))

press_mainland_trend_scale_FR <- press_mainland_trend_FR
press_mainland_trend_scale_FR[,c("d_impervious","d_treedensity","d_agri",
                              "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                              "d_shannon","shannon","drymatter","protectedarea_perc",
                              "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",   
                              "eulandsystem_forest_lowmedium","eulandsystem_forest_high")] <- scale(press_mainland_trend_scale_FR[,c("d_impervious","d_treedensity","d_agri",
                                                                                                                                  "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                                                                                                                  "d_shannon","shannon","drymatter","protectedarea_perc",
                                                                                                                                  "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",   
                                                                                                                                  "eulandsystem_forest_lowmedium","eulandsystem_forest_high")])


saveRDS(press_mainland_trend_FR,"output/press_mainland_trend_FR.rds") 
saveRDS(press_mainland_trend_scale_FR,"output/press_mainland_trend_scale_FR.rds") 



source("functions.R")

subsite_data_mainland_trend_fr <- subsite_data_mainland_trend[which(subsite_data_mainland_trend$scheme_code=="FR"),]

res_gam_bird_FR <- ddply(subsite_data_mainland_trend_fr,
                      .(sci_name_out),.fun=gam_species_FR,
                      pressure_data=press_mainland_trend_scale_FR,
                      pressure_data_unscale=press_mainland_trend_FR,
                      pressure_change=pressure_change,
                      site_data=site_mainland_sf_reproj,
                      .progress = "text")

saveRDS(res_gam_bird_FR,"output/res_gam_bird_FR.rds")


# pressure change

pressure_change <- data.frame(variable = c("Low intensity farmland","Medium intensity farmland","High intensity farmland",
                                           "Forest cover","Urban cover",
                                           "Agricultural cover","Temperature","Wood production","Hedge"),
                              initial = c(0.05,0.02,0.93,16914,5263,28.8,14.1,52,500),
                              tend = c(0.20,0.10,0.70,18606,7369,25.1,15,61,547),
                              s1 = c(0.70,0.30,0,19769,5061,25.8,15,52,935),
                              s2 = c(0.50,0.50,0,17515,5567,27.7,15,61,939),
                              s3 = c(0.20,0.50,0.30,17564,6073,27.1,15,71,619),
                              s4 = c(0.10,0.20,0.70,17264,7085,26.8,15,71,549))



bird_data <- droplevels(subsite_data_mainland_trend[which(subsite_data_mainland_trend$sci_name_out == "Alauda arvensis"),])
pressure_data <- press_mainland_trend_scale_FR
pressure_data_unscale <- press_mainland_trend_FR
site_data <- site_mainland_sf_reproj
min_site_number_per_species <- 60
min_occurence_species <- 300
family <- "quasipoisson"
pressure_name = c("d_impervious","d_treedensity","d_agri",
                  "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                  "d_shannon","shannon","drymatter","protectedarea_perc",
                  "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                  "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat")

bird_data <- bird_data[which(bird_data$scheme_code=="FR"),]

species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)

poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                 pressure_name,"PLS")])

species_press_data_year_unscale <- merge(bird_data, pressure_data_unscale[which(pressure_data_unscale$siteID %in% unique(bird_data$siteID) & pressure_data_unscale$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)

poisson_df_unscale <- na.omit(species_press_data_year_unscale[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                                 pressure_name,"tempspring_2020","tempspringvar_2020","precspring_2020","agri_2018","shannon_2018","impervious_2018","treedensity_2018","PLS")])


poisson_df$year <- poisson_df$year - 2000

poisson_df$count_scale_all <- scales::rescale(poisson_df$count)

poisson_df$PLS <- as.character(poisson_df$PLS)

if(length(pressure_name) > 1){
  formula_gam <- "count_scale_all ~ year + year:d_impervious + year:d_treedensity:eulandsystem_forest_lowmedium + year:d_treedensity:eulandsystem_forest_high +
    year:d_agri:eulandsystem_farmland_low + year:d_agri:eulandsystem_farmland_medium + year:d_agri:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
}else{
  formula_gam <- paste("count_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
}

col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
               "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_tempsrping",
               "year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc",
               "year:d_treedensity:eulandsystem_forest_lowmedium","year:d_treedensity:eulandsystem_forest_high",
               "year:d_agri:eulandsystem_farmland_low","year:d_agri:eulandsystem_farmland_medium",
               "year:d_agri:eulandsystem_farmland_high")

### global poisson model
  
global_mod <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                     family=family, data=poisson_df,random=list(siteID=~1|PLS))

# unscale pressure estimates https://stackoverflow.com/questions/23642111/how-to-unscale-the-coefficients-from-an-lmer-model-fitted-with-a-scaled-respon

mod_coef <- summary(global_mod$gam)$p.table[grep("year",row.names(summary(global_mod$gam)$p.table)),]

d_impervious_si <- sd(na.omit(pressure_data_unscale$d_impervious))
d_tempsrping_si <- sd(na.omit(pressure_data_unscale$d_tempsrping))
d_tempsrpingvar_si <- sd(na.omit(pressure_data_unscale$d_tempsrpingvar))
d_precspring_si <- sd(na.omit(pressure_data_unscale$d_precspring))
d_shannon_si <- sd(na.omit(pressure_data_unscale$d_shannon))
protectedarea_perc_si <- sd(na.omit(pressure_data_unscale$protectedarea_perc))
d_treedensity_si <- sd(na.omit(pressure_data_unscale$d_treedensity))
d_agri_si <- sd(na.omit(pressure_data_unscale$d_agri))
eulandsystem_farmland_low_si <- sd(na.omit(pressure_data_unscale$eulandsystem_farmland_low))
eulandsystem_farmland_medium_si <- sd(na.omit(pressure_data_unscale$eulandsystem_farmland_medium))
eulandsystem_farmland_high_si <- sd(na.omit(pressure_data_unscale$eulandsystem_farmland_high))
eulandsystem_forest_lowmedium_si <- sd(na.omit(pressure_data_unscale$eulandsystem_forest_lowmedium))
eulandsystem_forest_high_si <- sd(na.omit(pressure_data_unscale$eulandsystem_forest_high))

mod_coef_unscale <- mod_coef

mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_impervious"),c("Estimate","Std. Error")]/d_impervious_si  # delta method for se, taylor expansion g(x)=ax & s^2(g(x))=(g'(x))^2.s^(x)
mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),c("Estimate","Std. Error")]/d_tempsrping_si
mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrpingvar"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),c("Estimate","Std. Error")]/d_tempsrpingvar_si
mod_coef_unscale[which(row.names(mod_coef)=="year:d_precspring"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_precspring"),c("Estimate","Std. Error")]/d_precspring_si
mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_shannon"),c("Estimate","Std. Error")]/d_shannon_si
mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),c("Estimate","Std. Error")]/protectedarea_perc_si
mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),c("Estimate","Std. Error")]/(d_treedensity_si*eulandsystem_forest_lowmedium_si)
mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),c("Estimate","Std. Error")]/(d_treedensity_si*eulandsystem_forest_high_si)
mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),c("Estimate","Std. Error")]/(d_agri_si*eulandsystem_farmland_low_si)
mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),c("Estimate","Std. Error")]/(d_agri_si*eulandsystem_farmland_medium_si)
mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),c("Estimate","Std. Error")]/(d_agri_si*eulandsystem_farmland_high_si)
mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc:protectedarea_type"),c("Estimate","Std. Error")] <- mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc:protectedarea_type"),c("Estimate","Std. Error")]/protectedarea_perc_si

nb_rep <- 1000

# rate of change from estimate in proportion from recent history
# pressure_level_2050 = pressure_level_2020*proxy2050/proxy2020
# d_pressure_2050_2020 = (pressure_level_2050-pressure_level_2020)/(2050-2020) = pressure_level_2020*(proxy2050/proxy2020-1)/(2050-2020)
d_impervious_tend <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$tend[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_tend <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$tend[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_tend <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$tend[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_tot_tend <- mean(poisson_df_unscale$agri_2018)*
  pressure_change$tend[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]
agri_low_tend <- agri_tot_tend * pressure_change$tend[which(pressure_change$variable %in% c("Low intensity farmland"))]
agri_init_low_tend <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Low intensity farmland"))]
d_agri_low_tend <- (agri_low_tend-agri_init_low_tend)/(2050-2020)
agri_medium_tend <- agri_tot_tend * pressure_change$tend[which(pressure_change$variable %in% c("Medium intensity farmland"))]
agri_init_medium_tend <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Medium intensity farmland"))]
d_agri_medium_tend <- (agri_medium_tend-agri_init_medium_tend)/(2050-2020)
agri_high_tend <- agri_tot_tend * pressure_change$tend[which(pressure_change$variable %in% c("High intensity farmland"))]
agri_init_high_tend <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("High intensity farmland"))]
d_agri_high_tend <- (agri_high_tend-agri_init_high_tend)/(2050-2020)
d_treedensity_tend <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$tend[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
treedensity_tot_tend <- mean(poisson_df_unscale$treedensity_2018)*
  pressure_change$tend[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
treedensity_high_tend <- treedensity_tot_tend * mean(poisson_df_unscale$eulandsystem_forest_high)  * pressure_change$tend[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]
treedensity_init_high_tend <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_high) 
d_treedensity_high_tend <- (treedensity_high_tend-treedensity_init_high_tend)/(2050-2020)
treedensity_lowmedium_tend <-  treedensity_tot_tend * mean(poisson_df_unscale$eulandsystem_forest_lowmedium)* (2-pressure_change$tend[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))])
treedensity_init_lowmedium_tend <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_lowmedium) 
d_treedensity_lowmedium_tend <- (treedensity_lowmedium_tend-treedensity_init_lowmedium_tend)/(2050-2020)
d_tempspring_tend <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$tend[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_tend <- mean(poisson_df_unscale$protectedarea_perc)

beta1_tend <- mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*d_impervious_tend +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*d_tempspring_tend +
  #mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*d_tempspringvar_tend +
  #mod_coef_unscale[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*d_precspring_tend +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*d_shannon_tend +
  mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*protectedarea_perc_tend +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"]*d_treedensity_lowmedium_tend +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"]*d_treedensity_high_tend +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"]*d_agri_low_tend +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"]*d_agri_medium_tend +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"]*d_agri_high_tend


beta1_tend_sample <- rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*d_impervious_tend +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*d_tempspring_tend +
  #rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*d_tempspringvar_tend +
  #rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*d_precspring_tend +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*d_shannon_tend +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*protectedarea_perc_tend +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Std. Error"])*d_treedensity_lowmedium_tend +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Std. Error"])*d_treedensity_high_tend +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Std. Error"])*d_agri_high_tend +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Std. Error"])*d_agri_medium_tend +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Std. Error"])*d_agri_high_tend


d_impervious_s1 <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_s1 <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_s1 <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_tot_s1 <- mean(poisson_df_unscale$agri_2018)*
  pressure_change$s1[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]
agri_low_s1 <- agri_tot_s1 * pressure_change$s1[which(pressure_change$variable %in% c("Low intensity farmland"))]
agri_init_low_s1 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Low intensity farmland"))]
d_agri_low_s1 <- (agri_low_s1-agri_init_low_s1)/(2050-2020)
agri_medium_s1 <- agri_tot_s1 * pressure_change$s1[which(pressure_change$variable %in% c("Medium intensity farmland"))]
agri_init_medium_s1 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Medium intensity farmland"))]
d_agri_medium_s1 <- (agri_medium_s1-agri_init_medium_s1)/(2050-2020)
agri_high_s1 <- agri_tot_s1 * pressure_change$s1[which(pressure_change$variable %in% c("High intensity farmland"))]
agri_init_high_s1 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("High intensity farmland"))]
d_agri_high_s1 <- (agri_high_s1-agri_init_high_s1)/(2050-2020)
d_treedensity_s1 <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
treedensity_tot_s1 <- mean(poisson_df_unscale$treedensity_2018)*
  pressure_change$s1[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
treedensity_high_s1 <- treedensity_tot_s1 * mean(poisson_df_unscale$eulandsystem_forest_high)  * pressure_change$s1[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]
treedensity_init_high_s1 <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_high) 
d_treedensity_high_s1 <- (treedensity_high_s1-treedensity_init_high_s1)/(2050-2020)
treedensity_lowmedium_s1 <-  treedensity_tot_s1 * mean(poisson_df_unscale$eulandsystem_forest_lowmedium)* (2-pressure_change$s1[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))])
treedensity_init_lowmedium_s1 <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_lowmedium) 
d_treedensity_lowmedium_s1 <- (treedensity_lowmedium_s1-treedensity_init_lowmedium_s1)/(2050-2020)
d_tempspring_s1 <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_s1 <- mean(poisson_df_unscale$protectedarea_perc)

beta1_s1 <- mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*d_impervious_s1 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*d_tempspring_s1 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*d_shannon_s1 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*protectedarea_perc_s1 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"]*d_treedensity_lowmedium_s1 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"]*d_treedensity_high_s1 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"]*d_agri_low_s1 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"]*d_agri_medium_s1 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"]*d_agri_high_s1


beta1_s1_sample <- rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*d_impervious_s1 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*d_tempspring_s1 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*d_shannon_s1 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*protectedarea_perc_s1 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Std. Error"])*d_treedensity_lowmedium_s1 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Std. Error"])*d_treedensity_high_s1 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Std. Error"])*d_agri_high_s1 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Std. Error"])*d_agri_medium_s1 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Std. Error"])*d_agri_high_s1


d_impervious_s2 <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_s2 <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_s2 <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_tot_s2 <- mean(poisson_df_unscale$agri_2018)*
  pressure_change$s2[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]
agri_low_s2 <- agri_tot_s2 * pressure_change$s2[which(pressure_change$variable %in% c("Low intensity farmland"))]
agri_init_low_s2 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Low intensity farmland"))]
d_agri_low_s2 <- (agri_low_s2-agri_init_low_s2)/(2050-2020)
agri_medium_s2 <- agri_tot_s2 * pressure_change$s2[which(pressure_change$variable %in% c("Medium intensity farmland"))]
agri_init_medium_s2 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Medium intensity farmland"))]
d_agri_medium_s2 <- (agri_medium_s2-agri_init_medium_s2)/(2050-2020)
agri_high_s2 <- agri_tot_s2 * pressure_change$s2[which(pressure_change$variable %in% c("High intensity farmland"))]
agri_init_high_s2 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("High intensity farmland"))]
d_agri_high_s2 <- (agri_high_s2-agri_init_high_s2)/(2050-2020)
d_treedensity_s2 <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
treedensity_tot_s2 <- mean(poisson_df_unscale$treedensity_2018)*
  pressure_change$s2[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
treedensity_high_s2 <- treedensity_tot_s2 * mean(poisson_df_unscale$eulandsystem_forest_high)  * pressure_change$s2[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]
treedensity_init_high_s2 <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_high) 
d_treedensity_high_s2 <- (treedensity_high_s2-treedensity_init_high_s2)/(2050-2020)
treedensity_lowmedium_s2 <-  treedensity_tot_s2 * mean(poisson_df_unscale$eulandsystem_forest_lowmedium)* (2-pressure_change$s2[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))])
treedensity_init_lowmedium_s2 <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_lowmedium) 
d_treedensity_lowmedium_s2 <- (treedensity_lowmedium_s2-treedensity_init_lowmedium_s2)/(2050-2020)
d_tempspring_s2 <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_s2 <- mean(poisson_df_unscale$protectedarea_perc)

beta1_s2 <- mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*d_impervious_s2 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*d_tempspring_s2 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*d_shannon_s2 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*protectedarea_perc_s2 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"]*d_treedensity_lowmedium_s2 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"]*d_treedensity_high_s2 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"]*d_agri_low_s2 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"]*d_agri_medium_s2 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"]*d_agri_high_s2


beta1_s2_sample <- rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*d_impervious_s2 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*d_tempspring_s2 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*d_shannon_s2 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*protectedarea_perc_s2 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Std. Error"])*d_treedensity_lowmedium_s2 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Std. Error"])*d_treedensity_high_s2 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Std. Error"])*d_agri_high_s2 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Std. Error"])*d_agri_medium_s2 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Std. Error"])*d_agri_high_s2



d_impervious_s3 <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_s3 <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_s3 <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_tot_s3 <- mean(poisson_df_unscale$agri_2018)*
  pressure_change$s3[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]
agri_low_s3 <- agri_tot_s3 * pressure_change$s3[which(pressure_change$variable %in% c("Low intensity farmland"))]
agri_init_low_s3 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Low intensity farmland"))]
d_agri_low_s3 <- (agri_low_s3-agri_init_low_s3)/(2050-2020)
agri_medium_s3 <- agri_tot_s3 * pressure_change$s3[which(pressure_change$variable %in% c("Medium intensity farmland"))]
agri_init_medium_s3 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Medium intensity farmland"))]
d_agri_medium_s3 <- (agri_medium_s3-agri_init_medium_s3)/(2050-2020)
agri_high_s3 <- agri_tot_s3 * pressure_change$s3[which(pressure_change$variable %in% c("High intensity farmland"))]
agri_init_high_s3 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("High intensity farmland"))]
d_agri_high_s3 <- (agri_high_s3-agri_init_high_s3)/(2050-2020)
d_treedensity_s3 <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
treedensity_tot_s3 <- mean(poisson_df_unscale$treedensity_2018)*
  pressure_change$s3[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
treedensity_high_s3 <- treedensity_tot_s3 * mean(poisson_df_unscale$eulandsystem_forest_high)  * pressure_change$s3[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]
treedensity_init_high_s3 <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_high) 
d_treedensity_high_s3 <- (treedensity_high_s3-treedensity_init_high_s3)/(2050-2020)
treedensity_lowmedium_s3 <-  treedensity_tot_s3 * mean(poisson_df_unscale$eulandsystem_forest_lowmedium)* (2-pressure_change$s3[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))])
treedensity_init_lowmedium_s3 <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_lowmedium) 
d_treedensity_lowmedium_s3 <- (treedensity_lowmedium_s3-treedensity_init_lowmedium_s3)/(2050-2020)
d_tempspring_s3 <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_s3 <- mean(poisson_df_unscale$protectedarea_perc)

beta1_s3 <- mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*d_impervious_s3 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*d_tempspring_s3 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*d_shannon_s3 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*protectedarea_perc_s3 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"]*d_treedensity_lowmedium_s3 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"]*d_treedensity_high_s3 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"]*d_agri_low_s3 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"]*d_agri_medium_s3 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"]*d_agri_high_s3


beta1_s3_sample <- rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*d_impervious_s3 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*d_tempspring_s3 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*d_shannon_s3 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*protectedarea_perc_s3 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Std. Error"])*d_treedensity_lowmedium_s3 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Std. Error"])*d_treedensity_high_s3 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Std. Error"])*d_agri_high_s3 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Std. Error"])*d_agri_medium_s3 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Std. Error"])*d_agri_high_s3


d_impervious_s4 <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_s4 <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_s4 <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_tot_s4 <- mean(poisson_df_unscale$agri_2018)*
  pressure_change$s4[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]
agri_low_s4 <- agri_tot_s4 * pressure_change$s4[which(pressure_change$variable %in% c("Low intensity farmland"))]
agri_init_low_s4 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Low intensity farmland"))]
d_agri_low_s4 <- (agri_low_s4-agri_init_low_s4)/(2050-2020)
agri_medium_s4 <- agri_tot_s4 * pressure_change$s4[which(pressure_change$variable %in% c("Medium intensity farmland"))]
agri_init_medium_s4 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("Medium intensity farmland"))]
d_agri_medium_s4 <- (agri_medium_s4-agri_init_medium_s4)/(2050-2020)
agri_high_s4 <- agri_tot_s4 * pressure_change$s4[which(pressure_change$variable %in% c("High intensity farmland"))]
agri_init_high_s4 <- mean(poisson_df_unscale$agri_2018)* pressure_change$initial[which(pressure_change$variable %in% c("High intensity farmland"))]
d_agri_high_s4 <- (agri_high_s4-agri_init_high_s4)/(2050-2020)
d_treedensity_s4 <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
treedensity_tot_s4 <- mean(poisson_df_unscale$treedensity_2018)*
  pressure_change$s4[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
treedensity_high_s4 <- treedensity_tot_s4 * mean(poisson_df_unscale$eulandsystem_forest_high)  * pressure_change$s4[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]
treedensity_init_high_s4 <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_high) 
d_treedensity_high_s4 <- (treedensity_high_s4-treedensity_init_high_s4)/(2050-2020)
treedensity_lowmedium_s4 <-  treedensity_tot_s4 * mean(poisson_df_unscale$eulandsystem_forest_lowmedium)* (2-pressure_change$s4[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))])
treedensity_init_lowmedium_s4 <- mean(poisson_df_unscale$treedensity_2018) * mean(poisson_df_unscale$eulandsystem_forest_lowmedium) 
d_treedensity_lowmedium_s4 <- (treedensity_lowmedium_s4-treedensity_init_lowmedium_s4)/(2050-2020)
d_tempspring_s4 <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_s4 <- mean(poisson_df_unscale$protectedarea_perc)

beta1_s4 <- mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*d_impervious_s4 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*d_tempspring_s4 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*d_shannon_s4 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*protectedarea_perc_s4 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"]*d_treedensity_lowmedium_s4 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"]*d_treedensity_high_s4 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"]*d_agri_low_s4 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"]*d_agri_medium_s4 +
  mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"]*d_agri_high_s4


beta1_s4_sample <- rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*d_impervious_s4 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*d_tempspring_s4 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*d_shannon_s4 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*protectedarea_perc_s4 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_lowmedium"),"Std. Error"])*d_treedensity_lowmedium_s4 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_treedensity:eulandsystem_forest_high"),"Std. Error"])*d_treedensity_high_s4 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_low"),"Std. Error"])*d_agri_high_s4 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_medium"),"Std. Error"])*d_agri_medium_s4 +
  rnorm(nb_rep,mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_unscale[which(row.names(mod_coef)=="year:d_agri:eulandsystem_farmland_high"),"Std. Error"])*d_agri_high_s4




# by species type:

farmland_species <- c("Alauda arvensis","Alectoris rufa","Anthus campestris","Corvus frugilegus",
                      "Emberiza calandra","Emberiza cirlus","Galerida cristata","Lanius collurio",
                      "Motacilla flava","Perdix perdix","Upupa epops","Buteo buteo",
                      "Sylvia communis","Falco tinnunculus","Linaria cannabina","Lullula arborea",
                      "Oenanthe oenanthe","Saxicola torquatus","Anthus pratensis","Emberiza citrinella",
                      "Emberiza hortulana","Saxicola rubetra")
forest_species <- c("Certhia familiaris","Coccothraustes coccothraustes","Periparus ater","Phylloscopus bonelli",
                    "Phylloscopus sibilatrix","Phylloscopus trochilus","Pyrrhula pyrrhula","Dendrocopos major",
                    "Leiopicus medius","Poecile montanus","Poecile palustris","Sitta europaea",
                    "Certhia brachydactyla","Sylvia melanocephala","Dryocopus martius","Erithacus rubecula",
                    "Lophophanes cristatus","Phylloscopus collybita","Regulus ignicapilla","Regulus regulus",
                    "Troglodytes troglodytes","Turdus philomelos","Turdus viscivorus")

smap_sp_mean <- read.csv("raw_data/smap_sp_mean.csv")
species_affected <- smap_sp_mean$Species[which(!is.na(smap_sp_mean$temp) | !is.na(smap_sp_mean$urb) | !is.na(smap_sp_mean$hico) | !is.na(smap_sp_mean$forest))]

pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR, id.vars=c("sci_name_out","PLS"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR[which(res_gam_bird_FR$sci_name_out %in% farmland_species),], id.vars=c("sci_name_out","PLS"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR[which(res_gam_bird_FR$sci_name_out %in% forest_species),], id.vars=c("sci_name_out","PLS"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR[which(res_gam_bird_FR$sci_name_out %in% species_affected),], id.vars=c("sci_name_out","PLS"))
pressure_FR_bird_long <- pressure_FR_bird_long[which(!pressure_FR_bird_long$variable %in% c("(Intercept)","PLS","dev_exp","n_obs")),]



ggplot(pressure_FR_bird_long[which(pressure_FR_bird_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                         "year:d_shannon","year:protectedarea_perc","year:d_treedensity:eulandsystem_forest_lowmedium","year:d_treedensity:eulandsystem_forest_high",
                                                                         "year:d_agri:eulandsystem_farmland_low","year:d_agri:eulandsystem_farmland_medium",
                                                                         "year:d_agri:eulandsystem_farmland_high","year:protectedarea_perc:protectedarea_type")),], aes(x = value, y = variable, fill = variable)) +
  scale_y_discrete(labels=c("year:d_impervious" = "D urbanisation on trend","year:d_tempsrping" = "D temperature on trend", "year:d_tempsrpingvar" = "D temperature variation on trend", "year:d_precspring" = "D precipitation on trend", "year:d_shannon" = "D landscape diversity on trend",              
                            "year:protectedarea_perc" = "Protected area percentage on trend", "year:d_treedensity:eulandsystem_forest_lowmedium" = "D tree density in low/medium intensive forests on trend", "year:d_treedensity:eulandsystem_forest_high" = "D tree density in high intensive forests on trend", "year:d_agri:eulandsystem_farmland_low" = "D agricultural surface in low intensive farmland on trend",             
                            "year:d_agri:eulandsystem_farmland_medium" = "D agricultural surface in medium intensive farmland on trend", "year:d_agri:eulandsystem_farmland_high" = "D agricultural surface in high intensive farmland on trend", "year:protectedarea_perc:protectedarea_type" = "Protected area type on trend")) + 
  geom_density_ridges(stat = "binline",
                      bins = 60, draw_baseline = FALSE) + xlim(c(-0.5,0.5))+
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  xlab("Pressures") + ylab("Estimate") +
  theme(legend.position = "none")

ggplot(pressure_FR_bird_long[which(pressure_FR_bird_long$variable %in% c("trend_tend","trend_s1","trend_s2","trend_s3","trend_s4")),], aes(x = value, y = variable, fill = variable)) +
  geom_density_ridges(stat = "binline",
                      bins = 60, draw_baseline = FALSE) + xlim(c(-0.5,0.5))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75,
                      quantiles = 2) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  xlab("Pressures") + ylab("Estimate") +
  theme(legend.position = "none")


