# load land use tile for example from https://doi.org/10.7910/DVN/86M4PO and https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/86M4PO

ex_tile <- raster(x = "raw_data/landuse_intensity/Cropping_Intensity_30m_2016_2018_N50E000.tif")


# european land system 

eu_land_system <- raster(x = "raw_data/land_system/EU_landSystem.tif")


# protected areas 
#UNEP-WCMC and IUCN (2024), Protected Planet: The World Database on Protected Areas (WDPA) and World Database on Other Effective Area-based Conservation Measures (WD-OECM) [Online], January 2024, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.

protected_area_shp0 <- read_sf(dsn = "raw_data/protected_area/WDPA_shp_0/", layer = "WDPA_WDOECM_Jan2024_Public_EU_shp-polygons")
protected_area_shp1 <- read_sf(dsn = "raw_data/protected_area/WDPA_shp_1/", layer = "WDPA_WDOECM_Jan2024_Public_EU_shp-polygons")
protected_area_shp2 <- read_sf(dsn = "raw_data/protected_area/WDPA_shp_2/", layer = "WDPA_WDOECM_Jan2024_Public_EU_shp-polygons")
protected_area <- rbind(protected_area_shp0, protected_area_shp1)
protected_area <- rbind(protected_area, protected_area_shp2)

# land cover

clc_land_cover <- raster(x = "raw_data/land_cover/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")

### download eurostat data of population counts by NUTS-3 region
#euro_pop <- get_eurostat('demo_r_pjanaggr3', stringsAsFactors = FALSE) %>%  filter(sex == 'T', str_length(geo) == 5, age == 'TOTAL') # NUTS-3

### download geospatial data for NUTS-3 regions

euro_nuts3_sf <- get_eurostat_geospatial(output_class = 'sf', 
                                         resolution ='60', nuts_level = "3") %>%
  st_transform(crs = 3035)

### Get percentage of agricultural surface by nuts 3
rsamp <- euro_nuts3_sf
sfsamp <- st_as_sf(rsamp)
vv <- exact_extract(clc_land_cover, sfsamp) 
perc_agri <- function(x){
  x <- x %>% group_by(value) %>% summarize(cumul=sum(coverage_fraction))
  surf_tot <- sum(x$cumul[which(x$value < 50)])
  surf_agri <- sum(x$cumul[which(x$value >=20 & x$value < 30)])
  percent_area_agri <- surf_agri/surf_tot
  return(percent_area_agri)}
perc_agri_res <- sapply(vv, perc_agri)

euro_agri_surf <- data.frame(geo=euro_nuts3_sf$geo,perc_agri=perc_agri_res)


### download eurostat data of surface by NUTS-3 region
euro_surface_nuts <- get_eurostat('demo_r_d3area', stringsAsFactors = FALSE) %>%  filter(TIME_PERIOD=="2015-01-01", str_length(geo) == 5, landuse=="TOTAL") # NUTS-3

euro_surface_nuts$geo <- sub("FR21","FRF2",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR22","FRE2",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR23","FRD2",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR24","FRB0",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR25","FRD1",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR26","FRC1",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR30","FRE1",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR41","FRF3",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR42","FRF1",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR43","FRC2",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR51","FRG0",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR52","FRH0",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR53","FRI3",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR61","FRI1",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR62","FRJ2",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR63","FRI2",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR71","FRK2",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR72","FRK1",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR81","FRJ1",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR82","FRL0",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FR83","FRM0",euro_surface_nuts$geo)
#euro_surface_nuts$geo <- sub("FRA","FRY",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("DE915","DE91C",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("DE919","DE91C",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("DEB16","DEB1C",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("DEB19","DEB1D",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FI1D4","FI1D8",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("FI1D6","FI1D9",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("HU101","HU110",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("HU102","HU120",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("IE011","IE041",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("IE012","IE063",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("IE013","IE042",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("IE021","IE061",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("IE022","IE062",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("IE023","IE051",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("IE024","IE052",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("IE025","IE053",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT00A","LT011",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT001","LT021",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT002","LT022",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT003","LT023",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT004","LT024",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT005","LT025",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT006","LT026",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT007","LT027",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT008","LT028",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("LT009","LT029",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NL121","NL124",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NL122","NL125",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NL123","NL126",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NL322","NL328",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NL326","NL329",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NL338","NL33B",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NL339","NL33C",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NO061","NO060",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("NO062","NO060",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL113","PL711",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL114","PL712",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL115","PL713",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL116","PL714",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL117","PL715",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL127","PL911",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL128","PL921",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL129","PL912",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL12A","PL913",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL12B","PL922",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL12C","PL923",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL12D","PL924",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL12E","PL925",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL311","PL811",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL312","PL812",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL314","PL814",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL315","PL815",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL323","PL821",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL324","PL822",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL325","PL823",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL326","PL824",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL331","PL721",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL332","PL722",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL343","PL841",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL344","PL842",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("PL345","PL843",euro_surface_nuts$geo)
temporary_row <- euro_surface_nuts[which(euro_surface_nuts$geo=="PL913"),]
temporary_row$geo <- "PL926"
euro_surface_nuts <- rbind(euro_surface_nuts,temporary_row)
euro_surface_nuts$geo <- sub("UKM21","UKM71",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM22","UKM72",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM23","UKM73",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM24","UKM91",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM25","UKM75",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM26","UKM76",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM27","UKM77",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM28","UKM78",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM31","UKM81",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM32","UKM92",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM33","UKM93",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM34","UKM82",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM35","UKM83",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM36","UKM84",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM37","UKM94",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKM38","UKM95",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKN01","UKN06",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKN02","UKN14",euro_surface_nuts$geo)
euro_surface_nuts$geo <- sub("UKN03","UKN13",euro_surface_nuts$geo) # UKN15 UKN09
temporary_row <- euro_surface_nuts[which(euro_surface_nuts$geo=="UKN13"),]
temporary_row$geo <- "UKN15"
euro_surface_nuts <- rbind(euro_surface_nuts,temporary_row)
temporary_row <- euro_surface_nuts[which(euro_surface_nuts$geo=="UKN13"),]
temporary_row$geo <- "UKN09"
euro_surface_nuts <- rbind(euro_surface_nuts,temporary_row)
euro_surface_nuts$geo <- sub("UKN04","UKN10",euro_surface_nuts$geo) # UKN12
temporary_row <- euro_surface_nuts[which(euro_surface_nuts$geo=="UKN10"),]
temporary_row$geo <- "UKN12"
euro_surface_nuts <- rbind(euro_surface_nuts,temporary_row)
euro_surface_nuts$geo <- sub("UKN05","UKN16",euro_surface_nuts$geo) # UKN11 UKN08 UKN07
temporary_row <- euro_surface_nuts[which(euro_surface_nuts$geo=="UKN16"),]
temporary_row$geo <- "UKN11"
euro_surface_nuts <- rbind(euro_surface_nuts,temporary_row)
temporary_row <- euro_surface_nuts[which(euro_surface_nuts$geo=="UKN16"),]
temporary_row$geo <- "UKN08"
euro_surface_nuts <- rbind(euro_surface_nuts,temporary_row)
temporary_row <- euro_surface_nuts[which(euro_surface_nuts$geo=="UKN16"),]
temporary_row$geo <- "UKN07"
euro_surface_nuts <- rbind(euro_surface_nuts,temporary_row)


euro_agri_surf <- merge(euro_agri_surf,euro_surface_nuts, by="geo", all.x=TRUE)
euro_agri_surf$value_ha <- euro_agri_surf$values*100
euro_agri_surf$agri_surf <- euro_agri_surf$perc_agri*euro_agri_surf$value_ha

# pesticide

pesticide_table <- read.csv("raw_data/pesticide/pesticideActiveSubtances.csv")
pesticide_table <- pesticide_table[which(!(pesticide_table$COUNTRY %in% c("1-","2-","3-","4-","5-","6-"))),]

### from quantity to unit dose

pesticide_dose <- read.csv2("raw_data/pesticide/pesticide_fr/agence_eau/usage_pesticide.csv", header=TRUE)

pesticide_name <- data.frame(Substances_common_names=unique(pesticide_table$Substances_common_names),Dose=NA, Unit=NA, Prod_unit=NA)
pesticide_name$Substances_common_names2 <- pesticide_name$Substances_common_names
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="FOSETYL-AL")] <- "FOSETYL d'aluminium"
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="IMAZALIL (ENILCONAZOLE)")] <- "IMAZALIL"
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="BROMOXYNIL OCTANOATE AND/OR HEPTANOATE")] <- "BROMOXYNIL"
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="CLOPYRALID MONOETHANOLAMIN SALT")] <- "CLOPYRALID"
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="PROPOXYCARBAZONE-SODIUM")] <- "PROPOXYCARBAZONE"

for(i in 1:nrow(pesticide_name)){
  if(!(i %in% c(6,12,14,20,21,24,25,26,34,39,40,41,44,47,49,51,63,64,70,71,72,91,97,125,126,127,129,130,131,132,134,136,137,138,143,145,146,150))){
    pest_name_dose <- grep(pesticide_name$Substances_common_names[i],unique(pesticide_dose$sa),ignore.case=TRUE,value=TRUE)[which.min(nchar(grep(pesticide_name$Substances_common_names[i],unique(pesticide_dose$sa),ignore.case=TRUE,value=TRUE)))]
    pest_dose <- pesticide_dose[which(pesticide_dose$sa==pest_name_dose),]
    pest_dose$dose <- as.numeric(pest_dose$dose)
    product_dose <- pest_dose$dose[grepl("ha",pest_dose$unite,ignore.case=TRUE)]
    product_unit <- pest_dose$unite[grepl("ha",pest_dose$unite,ignore.case=TRUE)]
    pesticide_name$Prod_unit[i] <- product_unit[1]
    str_sa <- pest_dose$sa
    sa_dose <- as.numeric(unlist(regmatches(str_sa,gregexpr("[-]{0,1}[[:digit:]]+\\.{0,1}[[:digit:]]*",str_sa))))[ grepl("ha",pest_dose$unite,ignore.case=TRUE)]
    if(str_sub(str_sa[grepl("ha",pest_dose$unite,ignore.case=TRUE)], - 1, - 1)[1]=="%"){
      nodu <- product_dose*sa_dose*0.01
      nodu_unit <- pest_dose$unite[1]
    }else{
      sa_unit <- str_sub(str_sa[grepl("ha",pest_dose$unite,ignore.case=TRUE)], - 4, - 1)
      sa_unit <- gsub(" ", "", sa_unit, fixed = TRUE)
      nodu <- product_dose*sa_dose
      nodu_unit <- paste0(sub("/.*", "", sa_unit),"/ha")
      if(product_unit[1]=="g/ha"){
        nodu <- nodu/1000
      }
    }
    
    if(all_same(nodu_unit)){
      pesticide_name$Dose[i] <- mean(nodu, na.rm=TRUE)
      pesticide_name$Unit[i] <- nodu_unit[1]
    }else{
      pesticide_name$Dose[i] <- mean(nodu, na.rm=TRUE)
      pesticide_name$Unit[i] <- NA
    }
  }
  if(i==6){ # https://ephy.anses.fr/ppp/amulette
    pesticide_name$Dose[i] <- 0.5*0.5
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==12){ # https://ephy.anses.fr/ppp/balteor-fruits
    pesticide_name$Dose[i] <- 0.5*0.4
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==14){ # https://ephy.anses.fr/ppp/defango
    pesticide_name$Dose[i] <- 0.05*2
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==20){ # https://ephy.anses.fr/ppp/acylon-p
    pesticide_name$Dose[i] <- 0.112*2
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==21){ # https://ephy.anses.fr/ppp/monceren
    pesticide_name$Dose[i] <- 0.25*11.5
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==24){ # https://ephy.anses.fr/ppp/apotheose-bis
    pesticide_name$Dose[i] <- 0.4*1.77
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==25){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/1109
    pesticide_name$Dose[i] <- 0.300
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==26){ # https://ephy.anses.fr/ppp/lexoriz
    pesticide_name$Dose[i] <- 0.5*0.25*2 # 2 tonnes de semis à l'ha https://wikifarmer.com/fr/questions-et-reponses-sur-les-pommes-de-terre/
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==34){ # https://ephy.anses.fr/ppp/thionic-autodispersible-76
    pesticide_name$Dose[i] <- 0.76*2.5
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==39){ # https://www.fao.org/fileadmin/templates/agphome/documents/Pests_Pesticides/JMPR/Report2017/5.12_FENPROPIMORPH__188_.pdf
    pesticide_name$Dose[i] <- 0.750
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==40){ # https://ephy.anses.fr/ppp/imazatop-100
    pesticide_name$Dose[i] <- 0.1*0.15*2 # 2 tonnes de semis à l'ha
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==41){ # https://ephy.anses.fr/ppp/cazosanth
    pesticide_name$Dose[i] <- 0.4652*0.5
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==44){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/75
    pesticide_name$Dose[i] <- 0.250
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==47){ # https://ephy.anses.fr/ppp/tebutec
    pesticide_name$Dose[i] <- 0.25*0.37
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==49){ # https://www.fao.org/fileadmin/templates/agphome/documents/Pests_Pesticides/JMPR/Evaluation96/thiram.pdf
    pesticide_name$Dose[i] <- 2.4
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==51){ # https://ephy.anses.fr/ppp/a20078f
    pesticide_name$Dose[i] <- 0.025*0.2*1 # 100 kg/ha semis https://livre-blanc-cereales.be/thematiques/semis/densite-de-semis/
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==63){ # https://ephy.anses.fr/ppp/illoxan-ce
    pesticide_name$Dose[i] <- 0.36*2.7
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==64){ # https://ephy.anses.fr/ppp/diquanet
    pesticide_name$Dose[i] <- 0.2*2.7
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==70){ # https://ephy.anses.fr/substance/flurtamone
    pesticide_name$Dose[i] <- 0.25*1
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==71){ # https://ephy.anses.fr/ppp/glufo
    pesticide_name$Dose[i] <- 0.15*3.9
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==72){ # https://ephy.anses.fr/ppp/roundup-dynamic
    pesticide_name$Dose[i] <- 0.5*3.16
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==86){ # https://ephy.anses.fr/ppp/tribel
    pesticide_name$Dose[i] <- 0.4*1.6
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==91){ # https://ephy.anses.fr/ppp/atlas
    pesticide_name$Dose[i] <- 0.67*3
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==97){ # https://ephy.anses.fr/ppp/diuree
    pesticide_name$Dose[i] <- 0.8*2.35
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==125){ # https://ephy.anses.fr/ppp/fentryn
    pesticide_name$Dose[i] <- 0.003*7.3
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==126){ # https://ephy.anses.fr/ppp/chlorplus
    pesticide_name$Dose[i] <- 0.48*0.7
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==127){ # https://ephy.anses.fr/ppp/garvine
    pesticide_name$Dose[i] <- 0.225*1.5
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==129){ # https://ephy.anses.fr/ppp/dimetoxal
    pesticide_name$Dose[i] <- 0.4*0.94
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==130){ # https://ephy.anses.fr/ppp/dicarzol-200
    pesticide_name$Dose[i] <- 0.2*2.2
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==131){ # https://ephy.anses.fr/ppp/malathane
    pesticide_name$Dose[i] <- 0.15*5
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==132){ # https://ephy.anses.fr/ppp/mesurol-50
    pesticide_name$Dose[i] <- 0.5*1.67
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==134){ # https://ephy.anses.fr/ppp/dantop-50-wg
    pesticide_name$Dose[i] <- 0.095*0.5
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==136){ # https://ephy.anses.fr/ppp/gaucho
    pesticide_name$Dose[i] <- 0.11*1.1 # 1.1 unité semence/ha https://www.itbfr.org/tous-les-articles/article/news/les-criteres-cles-pour-reussir-le-semis/
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==137){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/205
    pesticide_name$Dose[i] <- 0.24
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==138){ # https://ephy.anses.fr/ppp/pirimor-g
    pesticide_name$Dose[i] <- 0.5*0.6
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==143){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/796
    pesticide_name$Dose[i] <- 0.094
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==145){ # https://ephy.anses.fr/ppp/helixer
    pesticide_name$Dose[i] <- 0.03*7
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==146){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/476
    pesticide_name$Dose[i] <- 0.000375
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==150){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/380
    pesticide_name$Dose[i] <- 0.034
      pesticide_name$Unit[i] <- "kg/ha"
  }
}

pesticide_name$Dose[which(pesticide_name$Unit=="g/ha")] <- pesticide_name$Dose[which(pesticide_name$Unit=="g/ha")]/1000
pesticide_name$Unit <- "kg/ha"
pesticide_name$Substances_common_names <- pesticide_name$Substances_common_names2

pesticide_table <- merge(pesticide_table,pesticide_name, by="Substances_common_names",all.x=TRUE)
pesticide_table$nodu <- pesticide_table$KG_TOT/pesticide_table$Dose

### update NUTS names
pesticide_table$NUTS3 <- sub("FR21","FRF2",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR22","FRE2",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR23","FRD2",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR24","FRB0",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR25","FRD1",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR26","FRC1",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR30","FRE1",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR41","FRF3",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR42","FRF1",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR43","FRC2",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR51","FRG0",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR52","FRH0",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR53","FRI3",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR61","FRI1",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR62","FRJ2",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR63","FRI2",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR71","FRK2",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR72","FRK1",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR81","FRJ1",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR82","FRL0",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FR83","FRM0",pesticide_table$NUTS3)
#pesticide_table$NUTS3 <- sub("FRA","FRY",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("DE915","DE91C",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("DE919","DE91C",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("DEB16","DEB1C",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("DEB19","DEB1D",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FI1D4","FI1D8",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("FI1D6","FI1D9",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("HU101","HU110",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("HU102","HU120",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("IE011","IE041",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("IE012","IE063",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("IE013","IE042",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("IE021","IE061",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("IE022","IE062",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("IE023","IE051",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("IE024","IE052",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("IE025","IE053",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT00A","LT011",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT001","LT021",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT002","LT022",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT003","LT023",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT004","LT024",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT005","LT025",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT006","LT026",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT007","LT027",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT008","LT028",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("LT009","LT029",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NL121","NL124",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NL122","NL125",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NL123","NL126",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NL322","NL328",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NL326","NL329",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NL338","NL33B",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NL339","NL33C",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NO061","NO060",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("NO062","NO060",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL113","PL711",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL114","PL712",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL115","PL713",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL116","PL714",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL117","PL715",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL127","PL911",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL128","PL921",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL129","PL912",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL12A","PL913",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL12B","PL922",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL12C","PL923",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL12D","PL924",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL12E","PL925",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL311","PL811",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL312","PL812",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL314","PL814",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL315","PL815",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL323","PL821",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL324","PL822",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL325","PL823",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL326","PL824",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL331","PL721",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL332","PL722",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL343","PL841",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL344","PL842",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("PL345","PL843",pesticide_table$NUTS3)
temporary_row <- pesticide_table[which(pesticide_table$NUTS3=="PL913"),]
temporary_row$NUTS3 <- "PL926"
pesticide_table <- rbind(pesticide_table,temporary_row)
pesticide_table$NUTS3 <- sub("UKM21","UKM71",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM22","UKM72",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM23","UKM73",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM24","UKM91",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM25","UKM75",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM26","UKM76",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM27","UKM77",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM28","UKM78",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM31","UKM81",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM32","UKM92",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM33","UKM93",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM34","UKM82",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM35","UKM83",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM36","UKM84",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM37","UKM94",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKM38","UKM95",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKN01","UKN06",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKN02","UKN14",pesticide_table$NUTS3)
pesticide_table$NUTS3 <- sub("UKN03","UKN13",pesticide_table$NUTS3) # UKN15 UKN09
temporary_row <- pesticide_table[which(pesticide_table$NUTS3=="UKN13"),]
temporary_row$NUTS3 <- "UKN15"
pesticide_table <- rbind(pesticide_table,temporary_row)
temporary_row <- pesticide_table[which(pesticide_table$NUTS3=="UKN13"),]
temporary_row$NUTS3 <- "UKN09"
pesticide_table <- rbind(pesticide_table,temporary_row)
pesticide_table$NUTS3 <- sub("UKN04","UKN10",pesticide_table$NUTS3) # UKN12
temporary_row <- pesticide_table[which(pesticide_table$NUTS3=="UKN10"),]
temporary_row$NUTS3 <- "UKN12"
pesticide_table <- rbind(pesticide_table,temporary_row)
pesticide_table$NUTS3 <- sub("UKN05","UKN16",pesticide_table$NUTS3) # UKN11 UKN08 UKN07
temporary_row <- pesticide_table[which(pesticide_table$NUTS3=="UKN16"),]
temporary_row$NUTS3 <- "UKN11"
pesticide_table <- rbind(pesticide_table,temporary_row)
temporary_row <- pesticide_table[which(pesticide_table$NUTS3=="UKN16"),]
temporary_row$NUTS3 <- "UKN08"
pesticide_table <- rbind(pesticide_table,temporary_row)
temporary_row <- pesticide_table[which(pesticide_table$NUTS3=="UKN16"),]
temporary_row$NUTS3 <- "UKN07"
pesticide_table <- rbind(pesticide_table,temporary_row)

pesticide_table <- merge(pesticide_table,euro_agri_surf, by.x="NUTS3",by.y="geo", all.x=TRUE)
pesticide_table$nodu_ha <- pesticide_table$nodu/pesticide_table$agri_surf
pesticide_table$kg_ha <- pesticide_table$KG_TOT/pesticide_table$agri_surf

pesticide_table_melt <- as.data.table(pesticide_table %>% group_by(COUNTRY,NUTS3,Categories_of_products) %>% summarize(total_kg=sum(kg_ha),total_nodu_ha=sum(nodu_ha)))
pesticide_table_wide_kg <- dcast(pesticide_table_melt,COUNTRY + NUTS3 ~ Categories_of_products, value.var="total_kg")
pesticide_table_wide_nodu <- dcast(pesticide_table_melt,COUNTRY + NUTS3 ~ Categories_of_products, value.var="total_nodu_ha")

pesticide_table_melt <- as.data.table(pesticide_table %>% group_by(COUNTRY,NUTS3) %>% summarize(total_kg=sum(KG_TOT),total_kg_ha=sum(kg_ha),total_nodu_ha=sum(nodu_ha)))
pesticide_table_melt$id <- pesticide_table_melt$NUTS3

pesticide_sf <- merge(euro_nuts3_sf,pesticide_table_melt)

ggplot(pesticide_sf) +
  geom_sf(aes(fill = log(total_kg_ha)))
ggplot(pesticide_sf) +
  geom_sf(aes(fill = log(total_nodu_ha)))
