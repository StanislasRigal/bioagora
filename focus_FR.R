
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

col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
               "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
               "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
               "year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium","year:eulandsystem_farmland_high",
               "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")
if_fail <- data.frame(t(rep(NA,(length(col_names)+23))))
names(if_fail) <- c(col_names,"dev_exp","n_obs","PLS",
                           "trend_tend","sd_tend","trend_s1","sd_s1",
                           "trend_s2","sd_s2","trend_s3","sd_s3",
                           "trend_s4","sd_s4",
                           "trend_tend_signif","sd_tend_signif","trend_s1_signif","sd_s1_signif",
                           "trend_s2_signif","sd_s2_signif","trend_s3_signif","sd_s3_signif",
                           "trend_s4_signif","sd_s4_signif")

res_gam_bird_FR <- ddply(subsite_data_mainland_trend_fr,
                      .(sci_name_out),.fun=purrr::possibly(otherwise=if_fail,
                                                                .f=gam_species_FR),
                      pressure_data=press_mainland_trend_scale_FR,
                      pressure_data_unscale=press_mainland_trend_FR,
                      pressure_change=pressure_change,
                      site_data=site_mainland_sf_reproj,
                      .progress = "text")

saveRDS(res_gam_bird_FR,"output/res_gam_bird_FR.rds")

### select good model fit and compare with PECBMS trends

pecbms_trend_class <- read.csv("output/pecbms_trend_class.csv", header=TRUE)
pecbms_trend_class <- read.csv("output/pecbms_test2.csv", header=TRUE)
pecbms_trend_class$PECBMS_slope_long <- as.numeric(substr(pecbms_trend_class$PECBMS_slope_long,1,6))
pecbms_trend_class$PECBMS_slope_short <- as.numeric(substr(pecbms_trend_class$PECBMS_slope_short,1,6))
pecbms_trend_class$PECBMS_slope_long[which(pecbms_trend_class$sci_name_out=="Sturnus vulgaris")] <- 0.98
pecbms_trend_class$PECBMS_slope_short[which(pecbms_trend_class$sci_name_out=="Chloris chloris")] <- 0.98

res_gam_bird_FR2 <- merge(res_gam_bird_FR[which(res_gam_bird_FR$dev_exp > 0.15 & res_gam_bird_FR$n_obs > 400),],pecbms_trend_class,ny="sci_name_out")

res_gam_bird_FR2$PECBMS_slope_mid <- (res_gam_bird_FR2$PECBMS_slope_long + res_gam_bird_FR2$PECBMS_slope_short)/2

plot(exp(year)~PECBMS_slope_long,res_gam_bird_FR2)

res_gam_bird_FR_correct <- res_gam_bird_FR[which(res_gam_bird_FR$dev_exp > 0.15 & res_gam_bird_FR$n_obs > 400),]


### check with Benoit a priori expectation

expected_effect <- read.csv2("raw_data/pressions_oiseaux.csv")
expected_effect <- read.csv("raw_data/pressions_oiseaux_gl.csv",sep = "\t")

obs_vs_expected <- merge(res_gam_bird_FR_correct,expected_effect, by.x = "sci_name_out", by.y="Species", all.x=TRUE)

obs_vs_expected$Augmentation.des.températures <- factor(obs_vs_expected$Augmentation.des.températures, levels = c("--","-","0","+","++"))
obs_vs_expected$Augmentation.de.l.artificialisation <- factor(obs_vs_expected$Augmentation.de.l.artificialisation, levels = c("--","-","0","+","++"))
obs_vs_expected$Augmentation.du.couvert.forestier <- factor(obs_vs_expected$Augmentation.du.couvert.forestier, levels = c("--","-","0","+","++"))
obs_vs_expected$Intensification.de.l.agriculture <- factor(obs_vs_expected$Intensification.de.l.agriculture, levels = c("--","-","0","+","++"))
obs_vs_expected$Diversité.des.paysages <- factor(obs_vs_expected$Diversité.des.paysages, levels = c("--","-","0","+","++"))
obs_vs_expected$Aires.protégées <- factor(obs_vs_expected$Aires.protégées, levels = c("--","-","0","+","++"))

ggplot(obs_vs_expected, aes(x=Augmentation.des.températures, y=tempsrping)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.des.températures, y=`year:d_tempsrping`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()

ggplot(obs_vs_expected, aes(x=Augmentation.de.l.artificialisation, y=milieu_caturban)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.de.l.artificialisation, y=`year:d_impervious`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()

ggplot(obs_vs_expected, aes(x=Augmentation.du.couvert.forestier, y=milieu_catopenland)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.du.couvert.forestier, y=`year:d_treedensity`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.du.couvert.forestier, y=`year:eulandsystem_forest_lowmedium`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Augmentation.du.couvert.forestier, y=`year:eulandsystem_forest_high`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()

ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=milieu_catopenland)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:d_agri`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:eulandsystem_farmland_low`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:eulandsystem_farmland_medium`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Intensification.de.l.agriculture, y=`year:eulandsystem_farmland_high`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()

ggplot(obs_vs_expected, aes(x=Diversité.des.paysages, y=shannon)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()
ggplot(obs_vs_expected, aes(x=Diversité.des.paysages, y=`year:d_shannon`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()

ggplot(obs_vs_expected, aes(x=Aires.protégées, y=`year:protectedarea_perc`)) + 
  geom_boxplot(color="blue",fill="blue",alpha=0.2,notch=TRUE,notchwidth = 0.8,outlier.colour="red",outlier.fill="red",outlier.size=3) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_minimal()



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


poisson_df$year <- scale(poisson_df$year)#poisson_df$year - 2000

poisson_df$count_scale_all <- poisson_df$count#scales::rescale(poisson_df$count)

poisson_df$PLS <- as.character(poisson_df$PLS)

if(length(pressure_name) > 1){
  formula_gam <- "count_scale_all ~ year + year:d_impervious + year:d_treedensity +
    year:eulandsystem_forest_lowmedium + year:eulandsystem_forest_high +
    year:d_agri + year:eulandsystem_farmland_low + year:eulandsystem_farmland_medium + year:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
}else{
  formula_gam <- paste("count_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
}

col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
               "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
               "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
               "year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium","year:eulandsystem_farmland_high",
               "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")

### global poisson model
  
global_mod <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                     family=family, data=poisson_df,random=list(siteID=~1|PLS))

# unscale pressure estimates https://stackoverflow.com/questions/23642111/how-to-unscale-the-coefficients-from-an-lmer-model-fitted-with-a-scaled-respon

mod_coef <- summary(global_mod$gam)$p.table[grep("year",row.names(summary(global_mod$gam)$p.table)),]

year_si <- sd(na.omit(pressure_data_unscale$year))
d_impervious_si <- sd(na.omit(pressure_data_unscale$d_impervious))
d_tempspring_si <- sd(na.omit(pressure_data_unscale$d_tempsrping))
d_tempspringvar_si <- sd(na.omit(pressure_data_unscale$d_tempsrpingvar))
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

year_mu <- mean(na.omit(pressure_data_unscale$year))
d_impervious_mu <- mean(na.omit(pressure_data_unscale$d_impervious))
d_tempspring_mu <- mean(na.omit(pressure_data_unscale$d_tempsrping))
d_tempspringvar_mu <- mean(na.omit(pressure_data_unscale$d_tempsrpingvar))
d_precspring_mu <- mean(na.omit(pressure_data_unscale$d_precspring))
d_shannon_mu <- mean(na.omit(pressure_data_unscale$d_shannon))
protectedarea_perc_mu <- mean(na.omit(pressure_data_unscale$protectedarea_perc))
d_treedensity_mu <- mean(na.omit(pressure_data_unscale$d_treedensity))
d_agri_mu <- mean(na.omit(pressure_data_unscale$d_agri))
eulandsystem_farmland_low_mu <- mean(na.omit(pressure_data_unscale$eulandsystem_farmland_low))
eulandsystem_farmland_medium_mu <- mean(na.omit(pressure_data_unscale$eulandsystem_farmland_medium))
eulandsystem_farmland_high_mu <- mean(na.omit(pressure_data_unscale$eulandsystem_farmland_high))
eulandsystem_forest_lowmedium_mu <- mean(na.omit(pressure_data_unscale$eulandsystem_forest_lowmedium))
eulandsystem_forest_high_mu <- mean(na.omit(pressure_data_unscale$eulandsystem_forest_high))

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
agri_low_tend <- mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$tend[which(pressure_change$variable %in% c("Low intensity farmland"))]*pressure_change$tend[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_medium_tend <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$tend[which(pressure_change$variable %in% c("Medium intensity farmland"))]*pressure_change$tend[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_high_tend <- mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$tend[which(pressure_change$variable %in% c("High intensity farmland"))]*pressure_change$tend[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
d_treedensity_tend <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$tend[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
forest_high_init <- mean(poisson_df_unscale$eulandsystem_forest_high)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_lowmedium_init <-  mean(poisson_df_unscale$eulandsystem_forest_lowmedium)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_high_tend <- pressure_change$tend[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]*forest_high_init # hyp toute production par high, puis regle de 3
forest_lowmedium_tend <- pressure_change$tend[which(pressure_change$variable %in% c("Forest cover"))] - forest_high_tend
forest_high_tend <- mean(poisson_df_unscale$eulandsystem_forest_high)*forest_high_tend/forest_high_init
forest_lowmedium_tend <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*forest_lowmedium_tend/forest_lowmedium_init
d_tempspring_tend <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$tend[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_tend <- mean(poisson_df_unscale$protectedarea_perc)
d_tempspringvar_tend <- mean(poisson_df_unscale$d_tempsrpingvar)
d_precspring_tend <- mean(poisson_df_unscale$d_precspring)

beta1_tend <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_tend - d_impervious_mu)/d_impervious_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_tend - d_tempspring_mu)/d_tempspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_tend - d_tempspringvar_mu)/d_tempspringvar_si +
  mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_tend - d_precspring_mu)/d_precspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_tend - d_shannon_mu)/d_shannon_si +
  mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_tend - protectedarea_perc_mu)/protectedarea_perc_si +
  mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_tend  - d_treedensity_mu)/d_treedensity_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_tend - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_tend - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_tend - d_agri_mu)/d_agri_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_tend - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_tend - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_tend - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si


beta1_tend_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_tend - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_tend - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_tend - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_tend - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_tend - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_tend - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_tend  - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_tend - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_tend - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_tend - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_tend - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_tend - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_tend - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si


d_impervious_s1 <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_s1 <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_s1 <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_low_s1 <- mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$s1[which(pressure_change$variable %in% c("Low intensity farmland"))]*pressure_change$s1[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_medium_s1 <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$s1[which(pressure_change$variable %in% c("Medium intensity farmland"))]*pressure_change$s1[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_high_s1 <- mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$s1[which(pressure_change$variable %in% c("High intensity farmland"))]*pressure_change$s1[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
d_treedensity_s1 <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
forest_high_init <- mean(poisson_df_unscale$eulandsystem_forest_high)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_lowmedium_init <-  mean(poisson_df_unscale$eulandsystem_forest_lowmedium)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_high_s1 <- pressure_change$s1[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]*forest_high_init # hyp toute production par high, puis regle de 3
forest_lowmedium_s1 <- pressure_change$s1[which(pressure_change$variable %in% c("Forest cover"))] - forest_high_s1
forest_high_s1 <- mean(poisson_df_unscale$eulandsystem_forest_high)*forest_high_s1/forest_high_init
forest_lowmedium_s1 <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*forest_lowmedium_s1/forest_lowmedium_init
d_tempspring_s1 <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$s1[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_s1 <- mean(poisson_df_unscale$protectedarea_perc)
d_tempspringvar_s1 <- mean(poisson_df_unscale$d_tempsrpingvar)
d_precspring_s1 <- mean(poisson_df_unscale$d_precspring)

beta1_s1 <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_s1 - d_impervious_mu)/d_impervious_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s1 - d_tempspring_mu)/d_tempspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s1 - d_tempspringvar_mu)/d_tempspringvar_si +
  mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_s1 - d_precspring_mu)/d_precspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_s1 - d_shannon_mu)/d_shannon_si +
  mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s1 - protectedarea_perc_mu)/protectedarea_perc_si +
  mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s1  - d_treedensity_mu)/d_treedensity_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_s1 - d_agri_mu)/d_agri_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_s1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_s1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_s1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si


beta1_s1_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_s1 - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s1 - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s1 - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_s1 - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_s1 - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s1 - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s1  - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_s1 - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_s1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_s1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_s1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si



d_impervious_s2 <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_s2 <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_s2 <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_low_s2 <- mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$s2[which(pressure_change$variable %in% c("Low intensity farmland"))]*pressure_change$s2[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_medium_s2 <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$s2[which(pressure_change$variable %in% c("Medium intensity farmland"))]*pressure_change$s2[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_high_s2 <- mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$s2[which(pressure_change$variable %in% c("High intensity farmland"))]*pressure_change$s2[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
d_treedensity_s2 <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
forest_high_init <- mean(poisson_df_unscale$eulandsystem_forest_high)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_lowmedium_init <-  mean(poisson_df_unscale$eulandsystem_forest_lowmedium)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_high_s2 <- pressure_change$s2[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]*forest_high_init # hyp toute production par high, puis regle de 3
forest_lowmedium_s2 <- pressure_change$s2[which(pressure_change$variable %in% c("Forest cover"))] - forest_high_s2
forest_high_s2 <- mean(poisson_df_unscale$eulandsystem_forest_high)*forest_high_s2/forest_high_init
forest_lowmedium_s2 <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*forest_lowmedium_s2/forest_lowmedium_init
d_tempspring_s2 <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$s2[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_s2 <- mean(poisson_df_unscale$protectedarea_perc)
d_tempspringvar_s2 <- mean(poisson_df_unscale$d_tempsrpingvar)
d_precspring_s2 <- mean(poisson_df_unscale$d_precspring)

beta1_s2 <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_s2 - d_impervious_mu)/d_impervious_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s2 - d_tempspring_mu)/d_tempspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s2 - d_tempspringvar_mu)/d_tempspringvar_si +
  mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_s2 - d_precspring_mu)/d_precspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_s2 - d_shannon_mu)/d_shannon_si +
  mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s2 - protectedarea_perc_mu)/protectedarea_perc_si +
  mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s2  - d_treedensity_mu)/d_treedensity_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s2 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s2 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_s2 - d_agri_mu)/d_agri_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_s2 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_s2 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_s2 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si


beta1_s2_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_s2 - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s2 - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s2 - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_s2 - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_s2 - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s2 - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s2  - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s2 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s2 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_s2 - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_s2 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_s2 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_s2 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si



d_impervious_s3 <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_s3 <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_s3 <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_low_s3 <- mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$s3[which(pressure_change$variable %in% c("Low intensity farmland"))]*pressure_change$s3[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_medium_s3 <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$s3[which(pressure_change$variable %in% c("Medium intensity farmland"))]*pressure_change$s3[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_high_s3 <- mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$s3[which(pressure_change$variable %in% c("High intensity farmland"))]*pressure_change$s3[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
d_treedensity_s3 <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
forest_high_init <- mean(poisson_df_unscale$eulandsystem_forest_high)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_lowmedium_init <-  mean(poisson_df_unscale$eulandsystem_forest_lowmedium)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_high_s3 <- pressure_change$s3[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]*forest_high_init # hyp toute production par high, puis regle de 3
forest_lowmedium_s3 <- pressure_change$s3[which(pressure_change$variable %in% c("Forest cover"))] - forest_high_s3
forest_high_s3 <- mean(poisson_df_unscale$eulandsystem_forest_high)*forest_high_s3/forest_high_init
forest_lowmedium_s3 <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*forest_lowmedium_s3/forest_lowmedium_init
d_tempspring_s3 <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$s3[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_s3 <- mean(poisson_df_unscale$protectedarea_perc)
d_tempspringvar_s3 <- mean(poisson_df_unscale$d_tempsrpingvar)
d_precspring_s3 <- mean(poisson_df_unscale$d_precspring)

beta1_s3 <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_s3 - d_impervious_mu)/d_impervious_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s3 - d_tempspring_mu)/d_tempspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s3 - d_tempspringvar_mu)/d_tempspringvar_si +
  mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_s3 - d_precspring_mu)/d_precspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_s3 - d_shannon_mu)/d_shannon_si +
  mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s3 - protectedarea_perc_mu)/protectedarea_perc_si +
  mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s3  - d_treedensity_mu)/d_treedensity_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_s3 - d_agri_mu)/d_agri_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_s3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_s3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_s3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si


beta1_s3_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_s3 - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s3 - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s3 - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_s3 - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_s3 - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s3 - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s3  - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_s3 - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_s3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_s3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_s3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si



d_impervious_s4 <- mean(poisson_df_unscale$impervious_2018)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
d_shannon_s4 <- mean(poisson_df_unscale$shannon_2018)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
d_agri_s4 <- mean(poisson_df_unscale$agri_2018)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
agri_low_s4 <- mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$s4[which(pressure_change$variable %in% c("Low intensity farmland"))]*pressure_change$s4[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_low)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_medium_s4 <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$s4[which(pressure_change$variable %in% c("Medium intensity farmland"))]*pressure_change$s4[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_medium)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
agri_high_s4 <- mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$s4[which(pressure_change$variable %in% c("High intensity farmland"))]*pressure_change$s4[which(pressure_change$variable %in% c("Agricultural cover"))]/(mean(poisson_df_unscale$eulandsystem_farmland_high)*pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))])
d_treedensity_s4 <- mean(poisson_df_unscale$treedensity_2018)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Forest cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]-1)/(2050-2020) 
forest_high_init <- mean(poisson_df_unscale$eulandsystem_forest_high)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_lowmedium_init <-  mean(poisson_df_unscale$eulandsystem_forest_lowmedium)/(mean(poisson_df_unscale$eulandsystem_forest_high)+mean(poisson_df_unscale$eulandsystem_forest_lowmedium))*pressure_change$initial[which(pressure_change$variable %in% c("Forest cover"))]
forest_high_s4 <- pressure_change$s4[which(pressure_change$variable %in% c("Wood production"))]/pressure_change$initial[which(pressure_change$variable %in% c("Wood production"))]*forest_high_init # hyp toute production par high, puis regle de 3
forest_lowmedium_s4 <- pressure_change$s4[which(pressure_change$variable %in% c("Forest cover"))] - forest_high_s4
forest_high_s4 <- mean(poisson_df_unscale$eulandsystem_forest_high)*forest_high_s4/forest_high_init
forest_lowmedium_s4 <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*forest_lowmedium_s4/forest_lowmedium_init
d_tempspring_s4 <- mean(poisson_df_unscale$tempspring_2020)*
  (pressure_change$s4[which(pressure_change$variable %in% c("Temperature"))]/pressure_change$initial[which(pressure_change$variable %in% c("Temperature"))]-1)/(2050-2020) 
protectedarea_perc_s4 <- mean(poisson_df_unscale$protectedarea_perc)
d_tempspringvar_s4 <- mean(poisson_df_unscale$d_tempsrpingvar)
d_precspring_s4 <- mean(poisson_df_unscale$d_precspring)

beta1_s4 <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
  mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_s4 - d_impervious_mu)/d_impervious_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s4 - d_tempspring_mu)/d_tempspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s4 - d_tempspringvar_mu)/d_tempspringvar_si +
  mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_s4 - d_precspring_mu)/d_precspring_si +
  mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_s4 - d_shannon_mu)/d_shannon_si +
  mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s4 - protectedarea_perc_mu)/protectedarea_perc_si +
  mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s4  - d_treedensity_mu)/d_treedensity_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s4 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s4 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_s4 - d_agri_mu)/d_agri_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_s4 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_s4 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_s4 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si


beta1_s4_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_s4 - d_impervious_mu)/d_impervious_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s4 - d_tempspring_mu)/d_tempspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s4 - d_tempspringvar_mu)/d_tempspringvar_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_s4 - d_precspring_mu)/d_precspring_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_s4 - d_shannon_mu)/d_shannon_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s4 - protectedarea_perc_mu)/protectedarea_perc_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s4  - d_treedensity_mu)/d_treedensity_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s4 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s4 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_s4 - d_agri_mu)/d_agri_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_s4 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_s4 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
  rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_s4 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si





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

pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct, id.vars=c("sci_name_out","PLS"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% farmland_species),], id.vars=c("sci_name_out","PLS"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% forest_species),], id.vars=c("sci_name_out","PLS"))
pressure_FR_bird_long <- reshape2::melt(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% species_affected),], id.vars=c("sci_name_out","PLS"))
pressure_FR_bird_long <- pressure_FR_bird_long[which(!pressure_FR_bird_long$variable %in% c("(Intercept)","PLS","dev_exp","n_obs")),]



ggplot(pressure_FR_bird_long[which(pressure_FR_bird_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                         "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                         "year:d_agri","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                         "year:eulandsystem_farmland_high")),], aes(x = value, y = variable, fill = variable)) +
  scale_y_discrete(labels=c("year:d_impervious" = "D urbanisation on trend","year:d_tempsrping" = "D temperature on trend", "year:d_tempsrpingvar" = "D temperature variation on trend", "year:d_precspring" = "D precipitation on trend", "year:d_shannon" = "D landscape diversity on trend",              
                            "year:protectedarea_perc" = "Protected area percentage on trend", "year:d_treedensity" = "D tree density on trend","year:eulandsystem_forest_lowmedium" = "Low/medium intensive forests on trend", "year:eulandsystem_forest_high" = "High intensive forests on trend",
                            "year:d_agri" = "D agricultural surface on trend","year:eulandsystem_farmland_low" = "Low intensive farmland on trend",
                            "year:eulandsystem_farmland_medium" = "Medium intensive farmland on trend", "year:eulandsystem_farmland_high" = "High intensive farmland on trend")) + 
  geom_density_ridges(stat = "binline",
                      bins = 60, draw_baseline = FALSE) + xlim(c(-0.5,0.5))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.75,
                      quantiles = 2) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  xlab("Pressures") + ylab("Estimate") +
  theme(legend.position = "none")


pressure_FR_bird_long_d <- pressure_FR_bird_long[which(pressure_FR_bird_long$variable %in% c("year:d_impervious","year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring",
                                                                                             "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                             "year:d_agri","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                             "year:eulandsystem_farmland_high")),]

pressure_FR_bird_long_d$variable <- factor(pressure_FR_bird_long_d$variable , levels = c("year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_impervious",
                                                                                         "year:d_shannon","year:protectedarea_perc","year:d_treedensity","year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high",
                                                                                         "year:d_agri","year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium",
                                                                                         "year:eulandsystem_farmland_high"))

ggplot(pressure_FR_bird_long_d, aes(x = value, y = variable, fill = variable)) +
  scale_y_discrete(labels=c("year:d_impervious" = "D urbanisation on trend","year:d_tempsrping" = "D temperature on trend", "year:d_tempsrpingvar" = "D temperature variation on trend", "year:d_precspring" = "D precipitation on trend", "year:d_shannon" = "D landscape diversity on trend",              
                            "year:protectedarea_perc" = "Protected area percentage on trend", "year:d_treedensity" = "D tree density on trend","year:eulandsystem_forest_lowmedium" = "Low/medium intensive forests on trend", "year:eulandsystem_forest_high" = "High intensive forests on trend",
                            "year:d_agri" = "D agricultural surface on trend","year:eulandsystem_farmland_low" = "Low intensive farmland on trend",
                            "year:eulandsystem_farmland_medium" = "Medium intensive farmland on trend", "year:eulandsystem_farmland_high" = "High intensive farmland on trend")) + 
  geom_density_ridges(stat = "binline", col=NA,scale = 0.9,
                      bins = 60, draw_baseline = FALSE) + xlim(c(-0.2,0.2))+
  stat_density_ridges(quantile_lines = TRUE, alpha = 0.2, scale = 0.9,
                      quantiles = 2) +
  scale_fill_manual(values = c("year:d_impervious"="#33a02c","year:d_tempsrping"="#1f78b4","year:d_tempsrpingvar"="#1f78b4","year:d_precspring"="#1f78b4",
                               "year:d_shannon"="#33a02c","year:protectedarea_perc"="#b2df8a","year:d_treedensity"="#33a02c","year:eulandsystem_forest_lowmedium"="#b2df8a","year:eulandsystem_forest_high"="#b2df8a",
                               "year:d_agri"="#33a02c","year:eulandsystem_farmland_low"="#b2df8a","year:eulandsystem_farmland_medium"="#b2df8a",
                               "year:eulandsystem_farmland_high"="#b2df8a")) +
  theme_ridges() + geom_vline(aes(xintercept = 0), lty=2) +
  theme(legend.position = "none", axis.title = element_blank())


ggsave("output/pressure_trend_bird_FR_hist.png",
       width = 6,
       height = 6,
       dpi = 300
)


pressure_FR_bird_long_s <- pressure_FR_bird_long[which(pressure_FR_bird_long$variable %in% c("milieu_catopenland","milieu_caturban","tempsrping",
                                                                                             "precspring","shannon","drymatter")),]

pressure_FR_bird_long_s$variable <- factor(pressure_FR_bird_long_s$variable , levels = c("tempsrping","precspring","milieu_catopenland",
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

ggsave("output/pressure_state_bird_FR_hist.png",
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



overall_trend_all <- overall_mean_sd_trend_FR(res_gam_bird_FR_correct)
overall_trend_all <- overall_mean_sd_trend_FR(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% farmland_species),])
overall_trend_all <- overall_mean_sd_trend_FR(res_gam_bird_FR_correct[which(res_gam_bird_FR_correct$sci_name_out %in% forest_species),])


FR_all <- data.frame(value = c(overall_trend_all$mu_tend,
                                      overall_trend_all$mu_s1,
                                      overall_trend_all$mu_s2,
                                      overall_trend_all$mu_s3,
                                      overall_trend_all$mu_s4),
                            sd = c(overall_trend_all$sd_tend,
                                   overall_trend_all$sd_s1,
                                   overall_trend_all$sd_s2,
                                   overall_trend_all$sd_s3,
                                   overall_trend_all$sd_s4),
                            se = c(overall_trend_all$se_tend,
                                   overall_trend_all$se_s1,
                                   overall_trend_all$se_s2,
                                   overall_trend_all$se_s3,
                                   overall_trend_all$se_s4),
                            variable = c("tend","s1","s2","s3","s4"))

FR_all$variable <- factor(FR_all$variable, levels = c("tend","s1","s2","s3","s4"))
ggplot(FR_all, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("tend"="red","s1"="darkgreen","s2"="green","s3"="lightgreen","s4"="blue")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")


ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="tend")]^x/FR_all$value[which(FR_all$variable=="tend")]^2023*100}, colour = "red", linetype=2, xlim=c(2000,2022)) +
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="tend")]^x/FR_all$value[which(FR_all$variable=="tend")]^2023*100}, colour = "red", xlim=c(2023,2050)) + 
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="s1")]^x/FR_all$value[which(FR_all$variable=="s1")]^2023*100}, colour = "darkgreen", xlim=c(2023,2050)) + 
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="s2")]^x/FR_all$value[which(FR_all$variable=="s2")]^2023*100}, colour = "green", xlim=c(2023,2050)) + 
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="s3")]^x/FR_all$value[which(FR_all$variable=="s3")]^2023*100}, colour = "lightgreen", xlim=c(2023,2050)) + 
  geom_function(fun = function(x){FR_all$value[which(FR_all$variable=="s4")]^x/FR_all$value[which(FR_all$variable=="s4")]^2023*100}, colour = "blue", xlim=c(2023,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")


FR_all_signif <- data.frame(value = c(overall_trend_all$mu_tend_signif,
                                          overall_trend_all$mu_s1_signif,
                                          overall_trend_all$mu_s2_signif,
                                          overall_trend_all$mu_s3_signif,
                                          overall_trend_all$mu_s4_signif),
                                sd = c(overall_trend_all$sd_tend_signif,
                                       overall_trend_all$sd_s1_signif,
                                       overall_trend_all$sd_s2_signif,
                                       overall_trend_all$sd_s3_signif,
                                       overall_trend_all$sd_s4_signif),
                                se = c(overall_trend_all$se_tend_signif,
                                       overall_trend_all$se_s1_signif,
                                       overall_trend_all$se_s2_signif,
                                       overall_trend_all$se_s3_signif,
                                       overall_trend_all$se_s4_signif),
                                variable = c("tend","s1","s2","s3","s4"))

FR_all_signif$variable <- factor(FR_all_signif$variable, levels = c("tend","s1","s2","s3","s4"))
ggplot(FR_all_signif, aes(x=value,y = variable)) + 
  geom_vline(xintercept = 1, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = value-1.96*se, xmin = value+1.96*se), linewidth = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = variable)) + 
  scale_color_manual(values = c("tend"="red","s1"="darkgreen","s2"="green","s3"="lightgreen","s4"="blue")) + 
  theme_minimal() + theme(legend.position = "none") +
  xlab("Slope") + ylab("Scenarios")


ggplot(data.frame(x = 2000:2050), aes(x)) +
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="tend")]^x/FR_all_signif$value[which(FR_all_signif$variable=="tend")]^2023*100}, colour = "red", linetype=2, xlim=c(2000,2022)) +
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="tend")]^x/FR_all_signif$value[which(FR_all_signif$variable=="tend")]^2023*100}, colour = "red", xlim=c(2023,2050)) + 
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="s1")]^x/FR_all_signif$value[which(FR_all_signif$variable=="s1")]^2023*100}, colour = "darkgreen", xlim=c(2023,2050)) + 
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="s2")]^x/FR_all_signif$value[which(FR_all_signif$variable=="s2")]^2023*100}, colour = "green", xlim=c(2023,2050)) + 
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="s3")]^x/FR_all_signif$value[which(FR_all_signif$variable=="s3")]^2023*100}, colour = "lightgreen", xlim=c(2023,2050)) + 
  geom_function(fun = function(x){FR_all_signif$value[which(FR_all_signif$variable=="s4")]^x/FR_all_signif$value[which(FR_all_signif$variable=="s4")]^2023*100}, colour = "blue", xlim=c(2023,2050)) + 
  coord_trans(y='log') +
  theme_minimal() + xlab("Year") + ylab("Abundance")
