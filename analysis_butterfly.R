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
saveRDS(site_mainland_sf_reproj_butterfly,"output/site_mainland_sf_reproj_butterfly.rds")

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

radius_buffer <- sqrt(median(site_mainland_sf_reproj$transect_length,na.rm=TRUE)/pi)
site_mainland_buffer <- st_buffer(site_mainland_sf_reproj, dist = 2500)
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
                               drymatter2000 = weighted.mean(x$drymatter2000,x$area); drymatter2018 = weighted.mean(x$drymatter2018,x$area); declineproductivity = weighted.mean(x$declineproductivity,x$area,na.rm=TRUE)
                               smallwoodyfeatures = weighted.mean(x$smallwoodyfeatures,x$area); fragmentation = weighted.mean(x$fragmentation,x$area)
                               forestintegrity = weighted.mean(x$forestintegrity,x$area,na.rm=TRUE); 
                               temp2000 = weighted.mean(x$temp2000,x$area); temp2020 = weighted.mean(x$temp2020,x$area); tempspring2000 = weighted.mean(x$tempspring2000,x$area); tempspring2020 = weighted.mean(x$tempspring2020,x$area)
                               tempspringvar2000 = weighted.mean(x$tempspringvar2000,x$area); tempspringvar2020 = weighted.mean(x$tempspringvar2020,x$area)
                               prec2000 = weighted.mean(x$prec2000,x$area); prec2020 = weighted.mean(x$prec2020,x$area); precspring2000 = weighted.mean(x$precspring2000,x$area); precspring2020 = weighted.mean(x$precspring2020,x$area)
                               precspringvar2000 = weighted.mean(x$precspringvar2000); precspringvar2020 = weighted.mean(x$precspringvar2020,x$area)
                               humidity2000 = weighted.mean(x$humidity2000,x$area); humidity2020 = weighted.mean(x$humidity2020,x$area); humidityspring2000 = weighted.mean(x$humidityspring2000,x$area)
                               humidityspring2020 = weighted.mean(x$humidityspring2020,x$area); humidityspringvar2000 = weighted.mean(x$humidityspringvar2000,x$area); humidityspringvar2020 = weighted.mean(x$humidityspringvar2020,x$area)
                               shannon = weighted.mean(x$shannon,x$area); GDP2000 = weighted.mean(x$GDP2000,x$area); GDP2015 = weighted.mean(x$GDP2015,x$area)
                               
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
                               if(length(eulandsystem_max_forest) > 1){
                                 eulandsystem_cat_forest <-"low_intensity"
                                 if(eulandsystem_max_forest == 42){
                                   eulandsystem_cat_forest <- "medium_intensity"
                                 }
                                 if(eulandsystem_max_forest == 43){
                                   eulandsystem_cat_forest <- "high_intensity"
                                 }
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
                                                 woodprod2000,woodprod2010,woodprodaverage,drymatter2000,drymatter2018,declineproductivity,
                                                 smallwoodyfeatures,fragmentation,forestintegrity,temp2000,temp2020,tempspring2000,tempspring2020,
                                                 tempspringvar2000,tempspringvar2020,prec2000,prec2020,precspring2000,precspring2020,
                                                 precspringvar2000,precspringvar2020,humidity2000,humidity2020,humidityspring2000,humidityspring2020,
                                                 humidityspringvar2000,humidityspringvar2020,shannon,GDP2000,GDP2015,
                                                 biogeo_area,PLS,forestintegrity_cat,eulandsystem_max,grassland,farmland,
                                                 low_farmland,high_farmland,low_farmland_tot,high_farmland_tot,protectedarea_cat,
                                                 protectedarea_perc,protectedarea_type,protectedarea_size,eulandsystem_cat_forest,eulandsystem_cat_urban,eulandsystem_cat_farmland))
                               
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

## get value per year per pressure

press_mainland_trend <- ddply(distinct(subsite_data_mainland_trend,transect_id,year,.keep_all=TRUE), .(transect_id,year),
                              .fun = function(x,pressure_data){
                                
                                pressure_subdata <- pressure_data[which(pressure_data$transect_id == x$transect_id),]
                                
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
                                protectedarea_cat <- pressure_subdata$protectedarea_cat
                                protectedarea_perc <- pressure_subdata$protectedarea_perc
                                protectedarea_type <- pressure_subdata$protectedarea_type
                                protectedarea_size_cor <- pressure_subdata$protectedarea_size_cor
                                pesticide_nodu <- pressure_subdata$pesticide_nodu_ha
                                smallwoodyfeatures <- pressure_subdata$smallwoodyfeatures
                                fragmentation <- pressure_subdata$fragmentation
                                forestintegrity_cat <- pressure_subdata$forestintegrity_cat
                                shannon <- pressure_subdata$shannon
                                eulandsystem_cat <- pressure_subdata$eulandsystem_cat
                                grassland <- pressure_subdata$grassland
                                farmland <- pressure_subdata$farmland
                                low_farmland <- pressure_subdata$low_farmland
                                high_farmland <- pressure_subdata$high_farmland
                                low_farmland_tot <- pressure_subdata$low_farmland_tot
                                high_farmland_tot <- pressure_subdata$high_farmland_tot
                                biogeo_area <- pressure_subdata$biogeo_area
                                PLS <- pressure_subdata$PLS
                                
                                trend_result <- data.frame(pop,impervious,treedensity,lightpollution,woodprod,drymatter,
                                                           temp,tempspring,tempspringvar,prec,precspring,precspringvar,humidityspring,
                                                           GDP_percap,GDP,protectedarea_cat,protectedarea_perc,protectedarea_type,protectedarea_size_cor,
                                                           pesticide_nodu,smallwoodyfeatures,fragmentation,forestintegrity_cat,shannon,
                                                           eulandsystem_cat,grassland,farmland,low_farmland,high_farmland,low_farmland_tot,
                                                           high_farmland_tot,biogeo_area,PLS)
                                return(trend_result)
                              },pressure_data = value_site_mainland,
                              .progress = "text")

press_mainland_trend_butterfly <- press_mainland_trend
saveRDS(press_mainland_trend_butterfly,"output/press_mainland_trend_butterfly.rds") 

press_mainland_trend_scale <- press_mainland_trend
press_mainland_trend_scale$eulandsystem_cat <- droplevels(press_mainland_trend_scale$eulandsystem_cat)
press_mainland_trend_scale$pesticide_nodu <- sqrt(press_mainland_trend_scale$pesticide_nodu)
press_mainland_trend_scale$fragmentation <- sqrt(press_mainland_trend_scale$fragmentation)
#press_mainland_trend_scale$pop <- sqrt(press_mainland_trend_scale$pop) ############### pb ici
press_mainland_trend_scale[,c("pop","impervious","treedensity","lightpollution",
                              "woodprod","drymatter","temp","tempspring","tempspringvar","prec",        
                              "precspring","precspringvar","humidityspring","GDP_percap","GDP",
                              "pesticide_nodu","smallwoodyfeatures","fragmentation","shannon",
                              "protectedarea_perc","protectedarea_size_cor","grassland","farmland",
                              "low_farmland","high_farmland","low_farmland_tot","high_farmland_tot")] <- scale(press_mainland_trend_scale[,c("pop","impervious","treedensity","lightpollution",
                                                                                                                                             "woodprod","drymatter","temp","tempspring","tempspringvar","prec",        
                                                                                                                                             "precspring","precspringvar","humidityspring","GDP_percap","GDP",
                                                                                                                                             "pesticide_nodu","smallwoodyfeatures","fragmentation","shannon",
                                                                                                                                             "protectedarea_perc","protectedarea_size_cor","grassland","farmland",
                                                                                                                                             "low_farmland","high_farmland","low_farmland_tot","high_farmland_tot")])


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

test_multicor <- press_mainland_trend_butterfly_scale[which(press_mainland_trend_butterfly_scale$year==2010),c("pop","impervious","treedensity","lightpollution",
                                                                                           "woodprod","drymatter","tempspring","tempspringvar",  
                                                                                           "precspring","precspringvar","humidityspring",
                                                                                           "protectedarea_cat","protectedarea_perc","protectedarea_strong_perc",
                                                                                           "pesticide_nodu","smallwoodyfeatures",
                                                                                           "fragmentation","shannon","eulandsystem_cat",
                                                                                           "grassland","farmland","low_farmland","high_farmland","low_farmland_tot","high_farmland_tot")]
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

test_multicor <- press_mainland_trend_scale[which(press_mainland_trend_scale$year==2010),c("impervious","treedensity","drymatter","tempspring","tempspringvar",  
                                                                                           "precspring","protectedarea_perc","smallwoodyfeatures",
                                                                                           "shannon","eulandsystem_cat","grassland","farmland")]
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



res_gam_mainland_species_PLS_trend_butterfly <- ddply(subsite_data_mainland_trend_butterfly,
                                            .(species_name),.fun=gam_species_PLS1b,
                                            pressure_data=press_mainland_trend_butterfly_scale,site_data=site_mainland_sf_reproj_butterfly,
                                            pressure_name="year",
                                            .progress = "text")

#saveRDS(res_gam_mainland_species_PLS_trend_butterfly,"output/res_gam_mainland_species_PLS_trend_butterfly.rds")
#res_gam_mainland_species_PLS_trend_butterfly <- readRDS("output/res_gam_mainland_species_PLS_trend_butterfly.rds")

res_gam_mainland_species_PLS_butterfly <- ddply(subsite_data_mainland_trend_butterfly,
                                      .(species_name),.fun=gam_species_PLS2b,
                                      pressure_data=press_mainland_trend_butterfly_scale,site_data=site_mainland_sf_reproj_butterfly,
                                      .progress = "text")

#saveRDS(res_gam_mainland_species_PLS_butterfly,"output/res_gam_mainland_species_PLS_butterfly.rds")
#res_gam_mainland_species_PLS_butterfly <- readRDS("output/res_gam_mainland_species_PLS_butterfly.rds")
res_gam_mainland_species_PLS_butterfly <- res_gam_mainland_species_PLS_butterfly[which(!is.na(res_gam_mainland_species_PLS_butterfly$PLS)),]


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

res_gam_mainland_species_PLS_interaction_butterfly <- res_gam_mainland_species_PLS_butterfly
#res_gam_mainland_species_PLS_interaction_butterfly$`year:impervious:eulandsystem_catmedium_intensity` <- res_gam_mainland_species_PLS_interaction_butterfly$`year:impervious` + res_gam_mainland_species_PLS_interaction_butterfly$`year:impervious:eulandsystem_catmedium_intensity`
#res_gam_mainland_species_PLS_interaction_butterfly$`year:impervious:eulandsystem_cathigh_intensity` <- res_gam_mainland_species_PLS_interaction_butterfly$`year:impervious` + res_gam_mainland_species_PLS_interaction_butterfly$`year:impervious:eulandsystem_cathigh_intensity`
#res_gam_mainland_species_PLS_interaction_butterfly$`year:treedensity:eulandsystem_catmedium_intensity` <- res_gam_mainland_species_PLS_interaction_butterfly$`year:treedensity` + res_gam_mainland_species_PLS_interaction_butterfly$`year:treedensity:eulandsystem_catmedium_intensity`
#res_gam_mainland_species_PLS_interaction_butterfly$`year:treedensity:eulandsystem_cathigh_intensity` <- res_gam_mainland_species_PLS_interaction_butterfly$`year:treedensity` + res_gam_mainland_species_PLS_interaction_butterfly$`year:treedensity:eulandsystem_cathigh_intensity`
#res_gam_mainland_species_PLS_interaction_butterfly$`year:farmland:eulandsystem_catmedium_intensity` <- res_gam_mainland_species_PLS_interaction_butterfly$`year:farmland` + res_gam_mainland_species_PLS_interaction_butterfly$`year:farmland:eulandsystem_catmedium_intensity`
#res_gam_mainland_species_PLS_interaction_butterfly$`year:farmland:eulandsystem_cathigh_intensity` <- res_gam_mainland_species_PLS_interaction_butterfly$`year:farmland` + res_gam_mainland_species_PLS_interaction_butterfly$`year:farmland:eulandsystem_cathigh_intensity`
#res_gam_mainland_species_PLS_interaction_butterfly$`year:grassland:eulandsystem_catmedium_intensity` <- res_gam_mainland_species_PLS_interaction_butterfly$`year:grassland` + res_gam_mainland_species_PLS_interaction_butterfly$`year:grassland:eulandsystem_catmedium_intensity`
#res_gam_mainland_species_PLS_interaction_butterfly$`year:grassland:eulandsystem_cathigh_intensity` <- res_gam_mainland_species_PLS_interaction_butterfly$`year:grassland` + res_gam_mainland_species_PLS_interaction_butterfly$`year:grassland:eulandsystem_cathigh_intensity`

pressure_PLS_butterfly <- res_gam_mainland_species_PLS_interaction_butterfly
pressure_PLS_butterfly <- pressure_PLS_butterfly %>% group_by(PLS) %>% summarise(drymatter_pos = length(which(`year:drymatter` > 0)), drymatter_neg = length(which(`year:drymatter` < 0)),
                                                             tempspring_pos = length(which(`year:tempspring` > 0)), tempspring_neg = length(which(`year:tempspring` < 0)),
                                                             tempspringvar_pos = length(which(`year:tempspringvar` > 0)), tempspringvar_neg = length(which(`year:tempspringvar` < 0)),
                                                             precspring_pos = length(which(`year:precspring` > 0)), precspring_neg = length(which(`year:precspring` < 0)),
                                                             protectedarea_perc_pos = length(which(`year:protectedarea_perc` > 0)), protectedarea_perc_neg = length(which(`year:protectedarea_perc` < 0)),
                                                             #smallwoodyfeatures_pos = length(which(`year:smallwoodyfeatures` > 0)), smallwoodyfeatures_neg = length(which(`year:smallwoodyfeatures` < 0)),
                                                             shannon_pos = length(which(`year:shannon` > 0)), shannon_neg = length(which(`year:shannon` < 0)),
                                                             impervious_pos = length(which(`year:impervious` > 0)), impervious_neg = length(which(`year:impervious` < 0)),
                                                             impervious_med_pos = length(which(`year:impervious:eulandsystem_catmedium_intensity` > 0)), impervious_med_neg = length(which(`year:impervious:eulandsystem_catmedium_intensity` < 0)),
                                                             impervious_high_pos = length(which(`year:impervious:eulandsystem_cathigh_intensity` > 0)), impervious_high_neg = length(which(`year:impervious:eulandsystem_cathigh_intensity` < 0)),
                                                             treedensity_pos = length(which(`year:treedensity` > 0)), treedensity_neg = length(which(`year:treedensity` < 0)),
                                                             treedensity_med_pos = length(which(`year:treedensity:eulandsystem_catmedium_intensity` > 0)), treedensity_med_neg = length(which(`year:treedensity:eulandsystem_catmedium_intensity` < 0)),
                                                             treedensity_high_pos = length(which(`year:treedensity:eulandsystem_cathigh_intensity` > 0)), treedensity_high_neg = length(which(`year:treedensity:eulandsystem_cathigh_intensity` < 0)),
                                                             #grassland_pos = length(which(`year:grassland` > 0)), grassland_neg = length(which(`year:grassland` < 0)),
                                                             #grassland_med_pos = length(which(`year:grassland:eulandsystem_catmedium_intensity` > 0)), grassland_med_neg = length(which(`year:grassland:eulandsystem_catmedium_intensity` < 0)),
                                                             #grassland_high_pos = length(which(`year:grassland:eulandsystem_cathigh_intensity` > 0)), grassland_high_neg = length(which(`year:grassland:eulandsystem_cathigh_intensity` < 0)),
                                                             farmland_pos = length(which(`year:farmland` > 0)), farmland_neg = length(which(`year:farmland` < 0)),
                                                             farmland_med_pos = length(which(`year:farmland:eulandsystem_catmedium_intensity` > 0)), farmland_med_neg = length(which(`year:farmland:eulandsystem_catmedium_intensity` < 0)),
                                                             farmland_high_pos = length(which(`year:farmland:eulandsystem_cathigh_intensity` > 0)), farmland_high_neg = length(which(`year:farmland:eulandsystem_cathigh_intensity` < 0)))

pressure_PLS_butterfly_sf <- merge(grid_eu_mainland_biogeo,pressure_PLS_butterfly,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=pressure_PLS_butterfly_sf, aes(fill=impervious_neg)) + scale_fill_gradientn(colors = paletteer_c("ggthemes::Classic Area Red", 30))


matrix_pressure_PLS_butterfly <- res_gam_mainland_species_PLS_interaction_butterfly
matrix_pressure_PLS_butterfly <- matrix_pressure_PLS_butterfly %>% group_by(PLS) %>% summarise(drymatter_effect = (length(which(`year:drymatter` > 0))- length(which(`year:drymatter` < 0)))/length(`year:drymatter`),
                                                                               tempspring_effect = (length(which(`year:tempspring` > 0)) - length(which(`year:tempspring` < 0)))/length(`year:drymatter`),
                                                                               tempspringvar_effect = (length(which(`year:tempspringvar` > 0)) - length(which(`year:tempspringvar` < 0)))/length(`year:drymatter`),
                                                                               precspring_effect = (length(which(`year:precspring` > 0)) - length(which(`year:precspring` < 0)))/length(`year:drymatter`),
                                                                               protectedarea_perc_effect = (length(which(`year:protectedarea_perc` > 0)) - length(which(`year:protectedarea_perc` < 0)))/length(`year:drymatter`),
                                                                               #smallwoodyfeatures_effect = (length(which(`year:smallwoodyfeatures` > 0)) - length(which(`year:smallwoodyfeatures` < 0)))/length(`year:drymatter`),
                                                                               shannon_effect = (length(which(`year:shannon` > 0)) - length(which(`year:shannon` < 0)))/length(`year:drymatter`),
                                                                               impervious_effect = (length(which(`year:impervious` > 0)) - length(which(`year:impervious` < 0)))/length(`year:drymatter`),
                                                                               impervious_med_effect = (length(which(`year:impervious:eulandsystem_catmedium_intensity` > 0)) - length(which(`year:impervious:eulandsystem_catmedium_intensity` < 0)))/length(`year:drymatter`),
                                                                               impervious_high_effect = (length(which(`year:impervious:eulandsystem_cathigh_intensity` > 0)) - length(which(`year:impervious:eulandsystem_cathigh_intensity` < 0)))/length(`year:drymatter`),
                                                                               treedensity_effect = (length(which(`year:treedensity` > 0)) - length(which(`year:treedensity` < 0)))/length(`year:drymatter`),
                                                                               treedensity_med_effect = (length(which(`year:treedensity:eulandsystem_catmedium_intensity` > 0)) - length(which(`year:treedensity:eulandsystem_catmedium_intensity` < 0)))/length(`year:drymatter`),
                                                                               treedensity_high_effect = (length(which(`year:treedensity:eulandsystem_cathigh_intensity` > 0)) - length(which(`year:treedensity:eulandsystem_cathigh_intensity` < 0)))/length(`year:drymatter`),
                                                                               #grassland_effect = (length(which(`year:grassland` > 0)) - length(which(`year:grassland` < 0)))/length(`year:drymatter`),
                                                                               #grassland_med_effect = (length(which(`year:grassland:eulandsystem_catmedium_intensity` > 0)) - length(which(`year:grassland:eulandsystem_catmedium_intensity` < 0)))/length(`year:drymatter`),
                                                                               #grassland_high_effect = (length(which(`year:grassland:eulandsystem_cathigh_intensity` > 0)) - length(which(`year:grassland:eulandsystem_cathigh_intensity` < 0)))/length(`year:drymatter`),
                                                                               farmland_effect = (length(which(`year:farmland` > 0)) - length(which(`year:farmland` < 0)))/length(`year:drymatter`),
                                                                               farmland_med_effect = (length(which(`year:farmland:eulandsystem_catmedium_intensity` > 0)) - length(which(`year:farmland:eulandsystem_catmedium_intensity` < 0)))/length(`year:drymatter`),
                                                                               farmland_high_effect = (length(which(`year:farmland:eulandsystem_cathigh_intensity` > 0)) - length(which(`year:farmland:eulandsystem_cathigh_intensity` < 0)))/length(`year:drymatter`),
                                                                               nb_sp = length(`year:drymatter`))

sort_matrix_pressure_PLS_butterfly <- as.matrix(matrix_pressure_PLS_butterfly[,c("tempspring_effect","tempspringvar_effect","precspring_effect","impervious_effect","impervious_med_effect","impervious_high_effect",
                                                                                 "treedensity_effect","treedensity_med_effect","treedensity_high_effect",
                                                                                 "farmland_effect","farmland_med_effect","farmland_high_effect")])

for(i in unique(matrix_pressure_PLS_butterfly$PLS)){
  temporary_df <- data.frame(rank = rank(sort_matrix_pressure_PLS_butterfly[which(matrix_pressure_PLS_butterfly$PLS == i),]), PLS = i)
  temporary_df$pressure <- row.names(temporary_df)
  if(i == "1"){
    rank_pressure_butterfly <- temporary_df
  }else{
    rank_pressure_butterfly <- rbind(rank_pressure_butterfly,temporary_df)
  }
}

rank_pressure_butterfly_short <- dcast(rank_pressure_butterfly,  PLS ~ pressure, value.var = "rank" )
rank_pressure_butterfly_short_sf <- merge(grid_eu_mainland_biogeo,rank_pressure_butterfly_short,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=rank_pressure_butterfly_short_sf, aes(fill=impervious_effect)) + scale_fill_gradientn(colors = paletteer_c("ggthemes::Red-Blue Diverging", 30))


matrix_pressure_PLS_neg_butterfly <- res_gam_mainland_species_PLS_interaction_butterfly
matrix_pressure_PLS_neg_butterfly <- matrix_pressure_PLS_neg_butterfly %>% group_by(PLS) %>% summarise(drymatter_effect = (length(which(`year:drymatter` < 0)))/length(`year:drymatter`),
                                                                                               tempspring_effect = (length(which(`year:tempspring` < 0)))/length(`year:drymatter`),
                                                                                               tempspringvar_effect = (length(which(`year:tempspringvar` < 0)))/length(`year:drymatter`),
                                                                                               precspring_effect = (length(which(`year:precspring` < 0)))/length(`year:drymatter`),
                                                                                               protectedarea_perc_effect = (length(which(`year:protectedarea_perc` < 0)))/length(`year:drymatter`),
                                                                                               protectedarea_type_effect = (length(which(`year:protectedarea_perc:protectedarea_type` < 0)))/length(`year:drymatter`),
                                                                                               #smallwoodyfeatures_effect = (length(which(`year:smallwoodyfeatures` < 0)))/length(`year:drymatter`),
                                                                                               shannon_effect = (length(which(`year:shannon` < 0)))/length(`year:drymatter`),
                                                                                               impervious_effect = (length(which(`year:impervious` < 0)))/length(`year:drymatter`),
                                                                                               impervious_med_effect = (length(which(`year:impervious:eulandsystem_catmedium_intensity` < 0)))/length(`year:drymatter`),
                                                                                               impervious_high_effect = (length(which(`year:impervious:eulandsystem_cathigh_intensity` < 0)))/length(`year:drymatter`),
                                                                                               treedensity_effect = (length(which(`year:treedensity` < 0)))/length(`year:drymatter`),
                                                                                               treedensity_med_effect = (length(which(`year:treedensity:eulandsystem_catmedium_intensity` < 0)))/length(`year:drymatter`),
                                                                                               treedensity_high_effect = (length(which(`year:treedensity:eulandsystem_cathigh_intensity` < 0)))/length(`year:drymatter`),
                                                                                               #grassland_effect = (length(which(`year:grassland` < 0)))/length(`year:drymatter`),
                                                                                               #grassland_med_effect = (length(which(`year:grassland:eulandsystem_catmedium_intensity` < 0)))/length(`year:drymatter`),
                                                                                               #grassland_high_effect = (length(which(`year:grassland:eulandsystem_cathigh_intensity` < 0)))/length(`year:drymatter`),
                                                                                               farmland_effect = (length(which(`year:farmland` < 0)))/length(`year:drymatter`),
                                                                                               farmland_med_effect = (length(which(`year:farmland:eulandsystem_catmedium_intensity` < 0)))/length(`year:drymatter`),
                                                                                               farmland_high_effect = (length(which(`year:farmland:eulandsystem_cathigh_intensity` < 0)))/length(`year:drymatter`),
                                                                                               nb_sp = length(`year:drymatter`))


matrix_pressure_PLS_neg_butterfly_sf <- merge(grid_eu_mainland_biogeo,matrix_pressure_PLS_neg_butterfly,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=matrix_pressure_PLS_neg_butterfly_sf, aes(fill=impervious_effect)) + scale_fill_gradient2(low="white", high="red", limits = c(0, max(matrix_pressure_PLS_neg_butterfly[,2:16])))

sort_matrix_pressure_PLS_neg_butterfly <- as.matrix(matrix_pressure_PLS_neg_butterfly[,c("tempspring_effect","tempspringvar_effect","precspring_effect","impervious_effect","impervious_med_effect","impervious_high_effect",
                                                                                 "treedensity_effect","treedensity_med_effect","treedensity_high_effect",
                                                                                 "farmland_effect","farmland_med_effect","farmland_high_effect")])


for(i in unique(matrix_pressure_PLS_neg_butterfly$PLS)){
  temporary_df <- data.frame(rank = rank(sort_matrix_pressure_PLS_neg_butterfly[which(matrix_pressure_PLS_neg_butterfly$PLS == i),]), PLS = i)
  temporary_df$pressure <- row.names(temporary_df)
  if(i == "1"){
    rank_pressure_neg_butterfly <- temporary_df
  }else{
    rank_pressure_neg_butterfly <- rbind(rank_pressure_neg_butterfly,temporary_df)
  }
}

rank_pressure_neg_butterfly_short <- dcast(rank_pressure_neg_butterfly,  PLS ~ pressure, value.var = "rank" )
rank_pressure_neg_butterfly_short_sf <- merge(grid_eu_mainland_biogeo,rank_pressure_neg_butterfly_short,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=rank_pressure_neg_butterfly_short_sf, aes(fill=impervious_effect)) + scale_fill_gradient2(low="white", high="red", limits = c(1, 15))



matrix_pressure_PLS_pos_butterfly <- res_gam_mainland_species_PLS_interaction_butterfly
matrix_pressure_PLS_pos_butterfly <- matrix_pressure_PLS_pos_butterfly %>% group_by(PLS) %>% summarise(drymatter_effect = (length(which(`year:drymatter` > 0)))/length(`year:drymatter`),
                                                                                                       tempspring_effect = (length(which(`year:tempspring` > 0)))/length(`year:drymatter`),
                                                                                                       tempspringvar_effect = (length(which(`year:tempspringvar` > 0)))/length(`year:drymatter`),
                                                                                                       precspring_effect = (length(which(`year:precspring` > 0)))/length(`year:drymatter`),
                                                                                                       protectedarea_perc_effect = (length(which(`year:protectedarea_perc` > 0)))/length(`year:drymatter`),
                                                                                                       protectedarea_type_effect = (length(which(`year:protectedarea_perc:protectedarea_type` > 0)))/length(`year:drymatter`),
                                                                                                       #smallwoodyfeatures_effect = (length(which(`year:smallwoodyfeatures` > 0)))/length(`year:drymatter`),
                                                                                                       shannon_effect = (length(which(`year:shannon` > 0)))/length(`year:drymatter`),
                                                                                                       impervious_effect = (length(which(`year:impervious` > 0)))/length(`year:drymatter`),
                                                                                                       impervious_med_effect = (length(which(`year:impervious:eulandsystem_catmedium_intensity` > 0)))/length(`year:drymatter`),
                                                                                                       impervious_high_effect = (length(which(`year:impervious:eulandsystem_cathigh_intensity` > 0)))/length(`year:drymatter`),
                                                                                                       treedensity_effect = (length(which(`year:treedensity` > 0)))/length(`year:drymatter`),
                                                                                                       treedensity_med_effect = (length(which(`year:treedensity:eulandsystem_catmedium_intensity` > 0)))/length(`year:drymatter`),
                                                                                                       treedensity_high_effect = (length(which(`year:treedensity:eulandsystem_cathigh_intensity` > 0)))/length(`year:drymatter`),
                                                                                                       #grassland_effect = (length(which(`year:grassland` > 0)))/length(`year:drymatter`),
                                                                                                       #grassland_med_effect = (length(which(`year:grassland:eulandsystem_catmedium_intensity` > 0)))/length(`year:drymatter`),
                                                                                                       #grassland_high_effect = (length(which(`year:grassland:eulandsystem_cathigh_intensity` > 0)))/length(`year:drymatter`),
                                                                                                       farmland_effect = (length(which(`year:farmland` > 0)))/length(`year:drymatter`),
                                                                                                       farmland_med_effect = (length(which(`year:farmland:eulandsystem_catmedium_intensity` > 0)))/length(`year:drymatter`),
                                                                                                       farmland_high_effect = (length(which(`year:farmland:eulandsystem_cathigh_intensity` > 0)))/length(`year:drymatter`),
                                                                                                       nb_sp = length(`year:drymatter`))


matrix_pressure_PLS_pos_butterfly_sf <- merge(grid_eu_mainland_biogeo,matrix_pressure_PLS_pos_butterfly,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=matrix_pressure_PLS_pos_butterfly_sf, aes(fill=impervious_effect)) + scale_fill_gradient2(low="white", high="blue", limits = c(0, max(matrix_pressure_PLS_neg_butterfly[,2:16])))

sort_matrix_pressure_PLS_pos_butterfly <- as.matrix(matrix_pressure_PLS_pos_butterfly[,c("tempspring_effect","tempspringvar_effect","precspring_effect","impervious_effect","impervious_med_effect","impervious_high_effect",
                                                                                         "treedensity_effect","treedensity_med_effect","treedensity_high_effect",
                                                                                         "farmland_effect","farmland_med_effect","farmland_high_effect")])


for(i in unique(matrix_pressure_PLS_pos_butterfly$PLS)){
  temporary_df <- data.frame(rank = rank(sort_matrix_pressure_PLS_pos_butterfly[which(matrix_pressure_PLS_pos_butterfly$PLS == i),]), PLS = i)
  temporary_df$pressure <- row.names(temporary_df)
  if(i == "1"){
    rank_pressure_pos_butterfly <- temporary_df
  }else{
    rank_pressure_pos_butterfly <- rbind(rank_pressure_pos_butterfly,temporary_df)
  }
}

rank_pressure_pos_butterfly_short <- dcast(rank_pressure_pos_butterfly,  PLS ~ pressure, value.var = "rank" )
rank_pressure_pos_butterfly_short_sf <- merge(grid_eu_mainland_biogeo,rank_pressure_pos_butterfly_short,by="PLS",all.x=TRUE)
ggplot() + geom_sf() +  
  geom_sf(data=rank_pressure_pos_butterfly_short_sf, aes(fill=impervious_effect)) + scale_fill_gradient2(low="white", high="blue", limits = c(1, 15))


pressure_EU_butterfly <- res_gam_mainland_species_PLS_butterfly[which(res_gam_mainland_species_PLS_butterfly$PLS=="europe"),]
pressure_EU_butterfly_long <- melt(pressure_EU_butterfly, id.vars=c("species_name","PLS"))
pressure_EU_butterfly_long <- pressure_EU_butterfly_long[which(!pressure_EU_butterfly_long$variable %in% c("(Intercept)","PLS","dev_exp","n_obs")),]
pressure_EU_butterfly_long <- pressure_EU_butterfly_long %>% group_by(variable) %>% summarise(median_pressure=median(value, na.rm=TRUE),
                                                                                              mean_pressure=mean(value, na.rm=TRUE),
                                                                                              sd_pressure=sd(value, na.rm=TRUE))

ggplot(pressure_EU_butterfly_long,
       aes(x = variable, y = exp(median_pressure))) + geom_point() +
  theme_ggstatsplot() + scale_x_discrete(labels=c("year:treedensity" = "Tree density","year:impervious"="Imperviousness",
                                                  "year:drymatter"="Productivity","year:tempspring"="Temperature",
                                                  "year:tempspringvar"="Temp variation","year:precspring"="Precipitation",
                                                  "year:protectedarea_perc"="Protected area","year:smallwoodyfeatures"="Hedges",
                                                  "year:shannon"="Landscape diversity")) +
  labs(y="Estimate") + theme(axis.title.x = element_blank(),
                             axis.text.x = element_text(angle=45, hjust = 1),
                             legend.position = "none")


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
  scale_x_discrete(labels=c("farmland_high_effect" = "High-intensity farmland","tempspring_effect" = "Spring temperature","precspring_effect" = "Spring precipitation",
                            "treedensity_high_effect" = "Tree density in high-instensity area","impervious_high_effect" = "Impervious in high-instensity area","farmland_med_effect" = "Medium-intensity farmland","impervious_med_effect" = "Impervious in medium-intensity area",
                            "treedensity_effect" = "Tree density in low-instensity area","tempspringvar_effect" = "Spring temperature variation","treedensity_med_effect" = "Tree density in medium-instensity area",
                            "shannon_effect" = "Landscape shannon diversity","impervious_effect" = "Impervious in low-instensity area","drymatter_effect" = "Vegetation productivity",
                            "farmland_effect" = "Low-intensity farmland","protectedarea_perc_effect" = "Protected area","protectedarea_type_effect" = "Protected area type")) +
  xlab("Pressures") + ylab("% of species impacted") +
  theme_minimal() + coord_flip() +
  theme(legend.position = "non")


ggsave("output/pressure_butterfly_eu.png",
       width = 8,
       height = 6,
       dpi = 300
)
