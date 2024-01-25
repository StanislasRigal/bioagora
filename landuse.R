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

# pesticide

pesticide_table <- read.csv("raw_data/pesticide/pesticideActiveSubtances.csv")
pesticide_table <- pesticide_table[which(!(pesticide_table$COUNTRY %in% c("1-","2-","3-","4-","5-","6-"))),]

### from quantity to unit dose

pesticide_dose <- read.csv2("raw_data/pesticide/pesticide_fr/agence_eau/usage_pesticide.csv", header=TRUE)

pesticide_name <- data.frame(Substances_common_names=unique(pesticide_table$Substances_common_names),Dose=NA, Unit=NA)
pesticide_name$Substances_common_names2 <- pesticide_name$Substances_common_names
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="FOSETYL-AL")] <- "FOSETYL d'aluminium"
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="IMAZALIL (ENILCONAZOLE)")] <- "IMAZALIL"
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="BROMOXYNIL OCTANOATE AND/OR HEPTANOATE")] <- "BROMOXYNIL"
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="CLOPYRALID MONOETHANOLAMIN SALT")] <- "CLOPYRALID"
pesticide_name$Substances_common_names[which(pesticide_name$Substances_common_names=="PROPOXYCARBAZONE-SODIUM")] <- "PROPOXYCARBAZONE"

for(i in 1:nrow(pesticide_name)){
  if(!(i %in% c(25,39,44,49,63,64,70,71,91,97,125,129,130,131,132,134,137,143,150))){
    pest_name_dose <- grep(pesticide_name$Substances_common_names[i],unique(pesticide_dose$sa),ignore.case=TRUE,value=TRUE)[which.min(nchar(grep(pesticide_name$Substances_common_names[i],unique(pesticide_dose$sa),ignore.case=TRUE,value=TRUE)))]
    pest_dose <- pesticide_dose[which(pesticide_dose$sa==pest_name_dose),]
    pest_dose$dose <- as.numeric(pest_dose$dose)
    product_dose <- pest_dose$dose[grepl("ha",pest_dose$unite,ignore.case=TRUE)]
    product_unit <- pest_dose$unite[grepl("ha",pest_dose$unite,ignore.case=TRUE)]
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
    }
    
    if(all_same(nodu_unit)){
      pesticide_name$Dose[i] <- mean(nodu, na.rm=TRUE)
      pesticide_name$Unit[i] <- nodu_unit[1]
    }else{
      pesticide_name$Dose[i] <- mean(nodu, na.rm=TRUE)
      pesticide_name$Unit[i] <- NA
    }
  }
  if(i==25){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/1109
    pesticide_name$Dose[i] <- 0.300
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==39){ # https://www.fao.org/fileadmin/templates/agphome/documents/Pests_Pesticides/JMPR/Report2017/5.12_FENPROPIMORPH__188_.pdf
    pesticide_name$Dose[i] <- 0.750
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==44){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/75
    pesticide_name$Dose[i] <- 0.250
    pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==49){ # https://www.fao.org/fileadmin/templates/agphome/documents/Pests_Pesticides/JMPR/Evaluation96/thiram.pdf
    pesticide_name$Dose[i] <- 2.4
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
  if(i==137){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/205
    pesticide_name$Dose[i] <- 0.24
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==143){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/796
    pesticide_name$Dose[i] <- 0.094
      pesticide_name$Unit[i] <- "kg/ha"
  }
  if(i==150){ # https://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/start/screen/active-substances/details/380
    pesticide_name$Dose[i] <- 0.034
      pesticide_name$Unit[i] <- "kg/ha"
  }
}


pesticide_table <- as.data.table(pesticide_table %>% group_by(COUNTRY,NUTS3,Categories_of_products) %>% summarize(total=sum(KG_TOT)))
pesticide_table <- dcast(pesticide_table,COUNTRY + NUTS3 ~ Categories_of_products, value.var="total")
pesticide_table$total <- apply(pesticide_table[,3:ncol(pesticide_table)],1,sum)
pesticide_table$id <- pesticide_table$NUTS3

### update NUTS names
pesticide_table$id <- sub("FR21","FRF2",pesticide_table$id)
pesticide_table$id <- sub("FR22","FRE2",pesticide_table$id)
pesticide_table$id <- sub("FR23","FRD2",pesticide_table$id)
pesticide_table$id <- sub("FR24","FRB0",pesticide_table$id)
pesticide_table$id <- sub("FR25","FRD1",pesticide_table$id)
pesticide_table$id <- sub("FR26","FRC1",pesticide_table$id)
pesticide_table$id <- sub("FR30","FRE1",pesticide_table$id)
pesticide_table$id <- sub("FR41","FRF3",pesticide_table$id)
pesticide_table$id <- sub("FR42","FRF1",pesticide_table$id)
pesticide_table$id <- sub("FR43","FRC2",pesticide_table$id)
pesticide_table$id <- sub("FR51","FRG0",pesticide_table$id)
pesticide_table$id <- sub("FR52","FRH0",pesticide_table$id)
pesticide_table$id <- sub("FR53","FRI3",pesticide_table$id)
pesticide_table$id <- sub("FR61","FRI1",pesticide_table$id)
pesticide_table$id <- sub("FR62","FRJ2",pesticide_table$id)
pesticide_table$id <- sub("FR63","FRI2",pesticide_table$id)
pesticide_table$id <- sub("FR71","FRK2",pesticide_table$id)
pesticide_table$id <- sub("FR72","FRK1",pesticide_table$id)
pesticide_table$id <- sub("FR81","FRJ1",pesticide_table$id)
pesticide_table$id <- sub("FR82","FRL0",pesticide_table$id)
pesticide_table$id <- sub("FR83","FRM0",pesticide_table$id)
#pesticide_table$id <- sub("FRA","FRY",pesticide_table$id)
pesticide_table$id <- sub("DE915","DE91C",pesticide_table$id)
pesticide_table$id <- sub("DE919","DE91C",pesticide_table$id)
pesticide_table$id <- sub("DEB16","DEB1C",pesticide_table$id)
pesticide_table$id <- sub("DEB19","DEB1D",pesticide_table$id)
pesticide_table$id <- sub("FI1D4","FI1D8",pesticide_table$id)
pesticide_table$id <- sub("FI1D6","FI1D9",pesticide_table$id)
pesticide_table$id <- sub("HU101","HU110",pesticide_table$id)
pesticide_table$id <- sub("HU102","HU120",pesticide_table$id)
pesticide_table$id <- sub("IE011","IE041",pesticide_table$id)
pesticide_table$id <- sub("IE012","IE063",pesticide_table$id)
pesticide_table$id <- sub("IE013","IE042",pesticide_table$id)
pesticide_table$id <- sub("IE021","IE061",pesticide_table$id)
pesticide_table$id <- sub("IE022","IE062",pesticide_table$id)
pesticide_table$id <- sub("IE023","IE051",pesticide_table$id)
pesticide_table$id <- sub("IE024","IE052",pesticide_table$id)
pesticide_table$id <- sub("IE025","IE053",pesticide_table$id)
pesticide_table$id <- sub("LT00A","LT011",pesticide_table$id)
pesticide_table$id <- sub("LT001","LT021",pesticide_table$id)
pesticide_table$id <- sub("LT002","LT022",pesticide_table$id)
pesticide_table$id <- sub("LT003","LT023",pesticide_table$id)
pesticide_table$id <- sub("LT004","LT024",pesticide_table$id)
pesticide_table$id <- sub("LT005","LT025",pesticide_table$id)
pesticide_table$id <- sub("LT006","LT026",pesticide_table$id)
pesticide_table$id <- sub("LT007","LT027",pesticide_table$id)
pesticide_table$id <- sub("LT008","LT028",pesticide_table$id)
pesticide_table$id <- sub("LT009","LT029",pesticide_table$id)
pesticide_table$id <- sub("NL121","NL124",pesticide_table$id)
pesticide_table$id <- sub("NL122","NL125",pesticide_table$id)
pesticide_table$id <- sub("NL123","NL126",pesticide_table$id)
pesticide_table$id <- sub("NL322","NL328",pesticide_table$id)
pesticide_table$id <- sub("NL326","NL329",pesticide_table$id)
pesticide_table$id <- sub("NL338","NL33B",pesticide_table$id)
pesticide_table$id <- sub("NL339","NL33C",pesticide_table$id)
pesticide_table$id <- sub("NO061","NO060",pesticide_table$id)
pesticide_table$id <- sub("NO062","NO060",pesticide_table$id)
pesticide_table$id <- sub("PL113","PL711",pesticide_table$id)
pesticide_table$id <- sub("PL114","PL712",pesticide_table$id)
pesticide_table$id <- sub("PL115","PL713",pesticide_table$id)
pesticide_table$id <- sub("PL116","PL714",pesticide_table$id)
pesticide_table$id <- sub("PL117","PL715",pesticide_table$id)
pesticide_table$id <- sub("PL127","PL911",pesticide_table$id)
pesticide_table$id <- sub("PL128","PL921",pesticide_table$id)
pesticide_table$id <- sub("PL129","PL912",pesticide_table$id)
pesticide_table$id <- sub("PL12A","PL913",pesticide_table$id)
pesticide_table$id <- sub("PL12B","PL922",pesticide_table$id)
pesticide_table$id <- sub("PL12C","PL923",pesticide_table$id)
pesticide_table$id <- sub("PL12D","PL924",pesticide_table$id)
pesticide_table$id <- sub("PL12E","PL925",pesticide_table$id)
pesticide_table$id <- sub("PL311","PL811",pesticide_table$id)
pesticide_table$id <- sub("PL312","PL812",pesticide_table$id)
pesticide_table$id <- sub("PL314","PL814",pesticide_table$id)
pesticide_table$id <- sub("PL315","PL815",pesticide_table$id)
pesticide_table$id <- sub("PL323","PL821",pesticide_table$id)
pesticide_table$id <- sub("PL324","PL822",pesticide_table$id)
pesticide_table$id <- sub("PL325","PL823",pesticide_table$id)
pesticide_table$id <- sub("PL326","PL824",pesticide_table$id)
pesticide_table$id <- sub("PL331","PL721",pesticide_table$id)
pesticide_table$id <- sub("PL332","PL722",pesticide_table$id)
pesticide_table$id <- sub("PL343","PL841",pesticide_table$id)
pesticide_table$id <- sub("PL344","PL842",pesticide_table$id)
pesticide_table$id <- sub("PL345","PL843",pesticide_table$id)
temporary_row <- pesticide_table[which(pesticide_table$id=="PL913"),]
temporary_row$id <- "PL926"
pesticide_table <- rbind(pesticide_table,temporary_row)
pesticide_table$id <- sub("UKM21","UKM71",pesticide_table$id)
pesticide_table$id <- sub("UKM22","UKM72",pesticide_table$id)
pesticide_table$id <- sub("UKM23","UKM73",pesticide_table$id)
pesticide_table$id <- sub("UKM24","UKM91",pesticide_table$id)
pesticide_table$id <- sub("UKM25","UKM75",pesticide_table$id)
pesticide_table$id <- sub("UKM26","UKM76",pesticide_table$id)
pesticide_table$id <- sub("UKM27","UKM77",pesticide_table$id)
pesticide_table$id <- sub("UKM28","UKM78",pesticide_table$id)
pesticide_table$id <- sub("UKM31","UKM81",pesticide_table$id)
pesticide_table$id <- sub("UKM32","UKM92",pesticide_table$id)
pesticide_table$id <- sub("UKM33","UKM93",pesticide_table$id)
pesticide_table$id <- sub("UKM34","UKM82",pesticide_table$id)
pesticide_table$id <- sub("UKM35","UKM83",pesticide_table$id)
pesticide_table$id <- sub("UKM36","UKM84",pesticide_table$id)
pesticide_table$id <- sub("UKM37","UKM94",pesticide_table$id)
pesticide_table$id <- sub("UKM38","UKM95",pesticide_table$id)
pesticide_table$id <- sub("UKN01","UKN06",pesticide_table$id)
pesticide_table$id <- sub("UKN02","UKN14",pesticide_table$id)
pesticide_table$id <- sub("UKN03","UKN13",pesticide_table$id) # UKN15 UKN09
temporary_row <- pesticide_table[which(pesticide_table$id=="UKN13"),]
temporary_row$id <- "UKN15"
pesticide_table <- rbind(pesticide_table,temporary_row)
temporary_row <- pesticide_table[which(pesticide_table$id=="UKN13"),]
temporary_row$id <- "UKN09"
pesticide_table <- rbind(pesticide_table,temporary_row)
pesticide_table$id <- sub("UKN04","UKN10",pesticide_table$id) # UKN12
temporary_row <- pesticide_table[which(pesticide_table$id=="UKN10"),]
temporary_row$id <- "UKN12"
pesticide_table <- rbind(pesticide_table,temporary_row)
pesticide_table$id <- sub("UKN05","UKN16",pesticide_table$id) # UKN11 UKN08 UKN07
temporary_row <- pesticide_table[which(pesticide_table$id=="UKN16"),]
temporary_row$id <- "UKN11"
pesticide_table <- rbind(pesticide_table,temporary_row)
temporary_row <- pesticide_table[which(pesticide_table$id=="UKN16"),]
temporary_row$id <- "UKN08"
pesticide_table <- rbind(pesticide_table,temporary_row)
temporary_row <- pesticide_table[which(pesticide_table$id=="UKN16"),]
temporary_row$id <- "UKN07"
pesticide_table <- rbind(pesticide_table,temporary_row)

### download eurostat data of population counts by NUTS-3 region
#euro_pop <- get_eurostat('demo_r_pjanaggr3', stringsAsFactors = FALSE) %>%  filter(sex == 'T', str_length(geo) == 5, age == 'TOTAL') # NUTS-3

### download geospatial data for NUTS-3 regions

euro_nuts3_sf <- get_eurostat_geospatial(output_class = 'sf', 
                                         resolution ='60', nuts_level = "3") %>%
  st_transform(crs = 3035)

pesticide_sf <- merge(euro_nuts3_sf,pesticide_table)

ggplot(pesticide_sf) +
  geom_sf(aes(fill = total))
