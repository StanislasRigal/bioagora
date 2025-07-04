# Loading pressure data and preparing them for a unique grid

# Land cover

## Imperviousness

impervious_2006 <- rast(raster(x = "raw_data/Imperviousness/IMD_2006_100m_eu_03035_d03_Full/IMD_2006_100m_eu_03035_d03_full.tif"))
impervious_2018 <- rast(raster(x = "raw_data/Imperviousness/IMD_2018_100m_eu_03035_v020/DATA/IMD_2018_100m_eu_03035_V2_0.tif"))

## Tree density

treedensity_2012 <- rast(raster(x = "raw_data/tree_cover/TCD_2012_100m_eu_03035_d04_Full/TCD_2012_100m_eu_03035_d04_full.tif"))
treedensity_2018 <- rast(raster(x = "raw_data/tree_cover/TCD_2018_100m_eu_03035_v020/DATA/TCD_2018_100m_eu_03035_V2_0.tif"))

# Land use intensity

## Example of land use intensity tile for example from https://doi.org/10.7910/DVN/86M4PO and https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/86M4PO

ex_tile <- raster(x = "raw_data/landuse_intensity/Cropping_Intensity_30m_2016_2018_N50E000.tif")

## European land system intensity

eu_land_system <- rast(raster(x = "raw_data/land_system/EU_landSystem.tif"))

## Protected areas 

protected_area_shp0 <- read_sf(dsn = "raw_data/protected_area/WDPA_shp_0/", layer = "WDPA_WDOECM_Jan2024_Public_EU_shp-polygons")
protected_area_shp0 <- protected_area_shp0[,c("IUCN_CAT","PARENT_ISO","GIS_AREA","DESIG_TYPE","DESIG_ENG")]
protected_area_shp0 <- protected_area_shp0[which(protected_area_shp0$PARENT_ISO %in% c("AND","AUT","BEL","BGR","HRV","CZE","DNK","EST","FIN","FRA",
                                                                        "DEU","GRC","HUN","IRL","ITA","LVA","LIE","LTU","LUX","MLT",
                                                                        "MCO","NDL","NOR","POL","PRT","ROU","SVK","SVN","ESP","SWE","CHE","GBR")),]
protected_area_shp1 <- read_sf(dsn = "raw_data/protected_area/WDPA_shp_1/", layer = "WDPA_WDOECM_Jan2024_Public_EU_shp-polygons")
protected_area_shp1 <- protected_area_shp1[,c("IUCN_CAT","PARENT_ISO","GIS_AREA","DESIG_TYPE","DESIG_ENG")]
protected_area_shp1 <- protected_area_shp1[which(protected_area_shp1$PARENT_ISO %in% c("AND","AUT","BEL","BGR","HRV","CZE","DNK","EST","FIN","FRA",
                                                                                       "DEU","GRC","HUN","IRL","ITA","LVA","LIE","LTU","LUX","MLT",
                                                                                       "MCO","NDL","NOR","POL","PRT","ROU","SVK","SVN","ESP","SWE","CHE","GBR")),]
protected_area_shp2 <- read_sf(dsn = "raw_data/protected_area/WDPA_shp_2/", layer = "WDPA_WDOECM_Jan2024_Public_EU_shp-polygons")
protected_area_shp2 <- protected_area_shp2[,c("IUCN_CAT","PARENT_ISO","GIS_AREA","DESIG_TYPE","DESIG_ENG")]
protected_area_shp2 <- protected_area_shp2[which(protected_area_shp2$PARENT_ISO %in% c("AND","AUT","BEL","BGR","HRV","CZE","DNK","EST","FIN","FRA",
                                                                                       "DEU","GRC","HUN","IRL","ITA","LVA","LIE","LTU","LUX","MLT",
                                                                                       "MCO","NDL","NOR","POL","PRT","ROU","SVK","SVN","ESP","SWE","CHE","GBR")),]
protected_area <- rbind(protected_area_shp0, protected_area_shp1)
protected_area <- rbind(protected_area, protected_area_shp2)

## Light pollution

lightpollution_1992 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_1992_calDMSP.tif")
lightpollution_1993 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_1993_calDMSP.tif")
lightpollution_1994 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_1994_calDMSP.tif")
lightpollution_1995 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_1995_calDMSP.tif")
lightpollution_1996 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_1996_calDMSP.tif")
lightpollution_1997 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_1997_calDMSP.tif")
lightpollution_1998 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_1998_calDMSP.tif")
lightpollution_1999 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_1999_calDMSP.tif")
lightpollution_2000 <- rast(raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2000_calDMSP.tif"))
lightpollution_2001 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2001_calDMSP.tif")
lightpollution_2002 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2002_calDMSP.tif")
lightpollution_2003 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2003_calDMSP.tif")
lightpollution_2004 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2004_calDMSP.tif")
lightpollution_2005 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2005_calDMSP.tif")
lightpollution_2006 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2006_calDMSP.tif")
lightpollution_2007 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2007_calDMSP.tif")
lightpollution_2008 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2008_calDMSP.tif")
lightpollution_2009 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2009_calDMSP.tif")
lightpollution_2010 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2010_calDMSP.tif")
lightpollution_2011 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2011_calDMSP.tif")
lightpollution_2012 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2012_calDMSP.tif")
lightpollution_2013 <- rast(raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2013_calDMSP.tif"))
#lightpollution_2014 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2014_simVIIRS.tif")
#lightpollution_2015 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2015_simVIIRS.tif")
#lightpollution_2016 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2016_simVIIRS.tif")
#lightpollution_2017 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2017_simVIIRS.tif")
#lightpollution_2018 <- raster(x = "raw_data/light_pollution/9828827/Harmonized_DN_NTL_2018_simVIIRS.tif")


## Pesticides

### Get NUTS3 surface and agricultural areas

#### Load land cover layer

clc_land_cover <- raster(x = "raw_data/land_cover/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")

temp1 <- exact_extract(clc_land_cover,grid_eu_mainland_outline,fun="frac")
clc_table <- data.frame(value=t(temp1),type=c(rep("Urban",11),rep("Agricultural land",11),rep("Forest",7),rep("Other natural land",15),NA,NA))
clc_table <- na.omit(clc_table)
clc_table2 <- data.frame(clc_table %>% group_by(type) %>% summarize(percent=sum(value)))


#### Example to download eurostat data of population counts by NUTS-3 region
#euro_pop <- get_eurostat('demo_r_pjanaggr3', stringsAsFactors = FALSE) %>%  filter(sex == 'T', str_length(geo) == 5, age == 'TOTAL') # NUTS-3

#### Load geospatial data for NUTS-3 regions

euro_nuts3_sf <- get_eurostat_geospatial(output_class = 'sf', 
                                         resolution ='01', nuts_level = "3") %>%
  st_transform(crs = 3035)

#### Get percentage of agricultural surface by NUTS3

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


#### Download eurostat data of surface by NUTS-3 region

euro_surface_nuts <- get_eurostat('demo_r_d3area', stringsAsFactors = FALSE) %>%  filter(TIME_PERIOD=="2015-01-01", str_length(geo) == 5, landuse=="TOTAL") # NUTS-3

#### Update NUTS3 codes

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

#### Calculate number of agricultural hectars per NUTS3

euro_agri_surf <- merge(euro_agri_surf,euro_surface_nuts, by="geo", all.x=TRUE)
euro_agri_surf$value_ha <- euro_agri_surf$values*100
euro_agri_surf$agri_surf <- euro_agri_surf$perc_agri*euro_agri_surf$value_ha

### From pesticide quantity to number of unit dose

#### Load quantity data per NUTS3

pesticide_table <- read.csv("raw_data/pesticide/pesticideActiveSubtances.csv")
pesticide_table <- pesticide_table[which(!(pesticide_table$COUNTRY %in% c("1-","2-","3-","4-","5-","6-"))),]

### Retrieve the NODU per active substance

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

#### Update NUTS names

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

#### Use the NODU per ha to compare between NUTS3

pesticide_table <- merge(pesticide_table,euro_agri_surf, by.x="NUTS3",by.y="geo", all.x=TRUE)
pesticide_table$nodu_ha <- pesticide_table$nodu/pesticide_table$agri_surf
pesticide_table$kg_ha <- pesticide_table$KG_TOT/pesticide_table$agri_surf

pesticide_table$nodu_ha[which(pesticide_table$agri_surf==0)] <- NA
pesticide_table$kg_ha[which(pesticide_table$agri_surf==0)] <- NA

saveRDS(pesticide_table,"output/pesticide_table.rds")

#### Plot pesticides quantity and NODU per ha

pesticide_table_melt <- as.data.table(pesticide_table %>% group_by(COUNTRY,NUTS3,Categories_of_products) %>% summarize(total_kg=sum(kg_ha,na.rm=TRUE),total_nodu_ha=sum(nodu_ha,na.rm = TRUE)))
pesticide_table_wide_kg <- dcast(pesticide_table_melt,COUNTRY + NUTS3 ~ Categories_of_products, value.var="total_kg")
pesticide_table_wide_nodu <- dcast(pesticide_table_melt,COUNTRY + NUTS3 ~ Categories_of_products, value.var="total_nodu_ha")

pesticide_table_melt <- as.data.table(pesticide_table %>% group_by(COUNTRY,NUTS3,value_ha) %>% summarize(total_kg=sum(KG_TOT),total_kg_ha=sum(kg_ha,na.rm=TRUE),total_nodu_ha=sum(nodu_ha,na.rm=TRUE)))
pesticide_table_melt$value_ha[which(is.na(pesticide_table_melt$value_ha))] <- 0
pesticide_table_melt$total_nodu_ha[which(pesticide_table_melt$total_kg_ha > 1000)] <- NA
pesticide_table_melt$total_kg_ha[which(pesticide_table_melt$total_kg_ha > 1000)] <- NA
pesticide_table_melt$total_nodu_ha[which(pesticide_table_melt$total_nodu_ha == 0 & pesticide_table_melt$total_kg>0)] <- NA
pesticide_table_melt$total_kg_ha[which(pesticide_table_melt$total_kg_ha == 0 & pesticide_table_melt$total_kg>0)] <- NA
pesticide_table_melt$total_nodu_ha[which(pesticide_table_melt$COUNTRY == "NO")] <- NA
pesticide_table_melt$total_kg_ha[which(pesticide_table_melt$COUNTRY == "NO")] <- NA
pesticide_table_melt <- data.frame(pesticide_table_melt)
pesticide_table_melt$id <- pesticide_table_melt$NUTS3

pesticide_sf <- merge(euro_nuts3_sf,pesticide_table_melt[])

ggplot(pesticide_sf) +
  geom_sf(aes(fill = log(total_kg_ha)))
ggplot(pesticide_sf) +
  geom_sf(aes(fill = log(total_nodu_ha)))

st_write(pesticide_sf,"output/pesticide_sf.shp")
pesticide_sf <- st_read("output/pesticide_sf.shp")
names(pesticide_sf)[which(names(pesticide_sf)=="totl_kg")] <- "total_kg"
names(pesticide_sf)[which(names(pesticide_sf)=="ttl_kg_")] <- "total_kg_ha"
names(pesticide_sf)[which(names(pesticide_sf)=="ttl_nd_")] <- "total_nodu_ha"

## Wood production

woodprod_2000 <- rast(raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2000.tif"))
woodprod_2001 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2001.tif")
woodprod_2002 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2002.tif")
woodprod_2003 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2003.tif")
woodprod_2004 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2004.tif")
woodprod_2005 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2005.tif")
woodprod_2006 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2006.tif")
woodprod_2007 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2007.tif")
woodprod_2008 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2008.tif")
woodprod_2009 <- raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2009.tif")
woodprod_2010 <- rast(raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_2010.tif"))
woodprod_average <- rast(raster(x = "raw_data/Wood_prod/WoodProductionMaps/woodprod_average.tif"))

## Habitat diversity (simpson diversity of habitat proportion)

clc_land_cover <- raster(x = "raw_data/land_cover/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
clc_land_cover_2000 <- raster(x = "raw_data/land_cover/u2006_clc2000_v2020_20u1_raster100m/DATA/U2006_CLC2000_V2020_20u1.tif")


# Productivity

## Dry matter productivity

drymatter_19990310 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199903100000_GLOBE_VGT_V2.0.1/19990310/c_gls_DMP-DMP_199903100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990320 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199903200000_GLOBE_VGT_V2.0.1/19990320/c_gls_DMP-DMP_199903200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990331 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199903310000_GLOBE_VGT_V2.0.1/19990331/c_gls_DMP-DMP_199903310000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990410 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199904100000_GLOBE_VGT_V2.0.1/19990410/c_gls_DMP-DMP_199904100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990420 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199904200000_GLOBE_VGT_V2.0.1/19990420/c_gls_DMP-DMP_199904200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990430 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199904300000_GLOBE_VGT_V2.0.1/19990430/c_gls_DMP-DMP_199904300000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990510 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199905100000_GLOBE_VGT_V2.0.1/19990510/c_gls_DMP-DMP_199905100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990520 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199905200000_GLOBE_VGT_V2.0.1/19990520/c_gls_DMP-DMP_199905200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990531 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199905310000_GLOBE_VGT_V2.0.1/19990531/c_gls_DMP-DMP_199905310000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990610 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199906100000_GLOBE_VGT_V2.0.1/19990610/c_gls_DMP-DMP_199906100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990620 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199906200000_GLOBE_VGT_V2.0.1/19990620/c_gls_DMP-DMP_199906200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_19990630 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_199906300000_GLOBE_VGT_V2.0.1/19990630/c_gls_DMP-DMP_199906300000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000310 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200003100000_GLOBE_VGT_V2.0.1/20000310/c_gls_DMP-DMP_200003100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000320 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200003200000_GLOBE_VGT_V2.0.1/20000320/c_gls_DMP-DMP_200003200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000331 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200003310000_GLOBE_VGT_V2.0.1/20000331/c_gls_DMP-DMP_200003310000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000410 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200004100000_GLOBE_VGT_V2.0.1/20000410/c_gls_DMP-DMP_200004100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000420 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200004200000_GLOBE_VGT_V2.0.1/20000420/c_gls_DMP-DMP_200004200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000430 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200004300000_GLOBE_VGT_V2.0.1/20000430/c_gls_DMP-DMP_200004300000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000510 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200005100000_GLOBE_VGT_V2.0.1/20000510/c_gls_DMP-DMP_200005100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000520 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200005200000_GLOBE_VGT_V2.0.1/20000520/c_gls_DMP-DMP_200005200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000531 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200005310000_GLOBE_VGT_V2.0.1/20000531/c_gls_DMP-DMP_200005310000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000610 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200006100000_GLOBE_VGT_V2.0.1/20000610/c_gls_DMP-DMP_200006100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000620 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200006200000_GLOBE_VGT_V2.0.1/20000620/c_gls_DMP-DMP_200006200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20000630 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200006300000_GLOBE_VGT_V2.0.1/20000630/c_gls_DMP-DMP_200006300000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010310 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200103100000_GLOBE_VGT_V2.0.1/20010310/c_gls_DMP-DMP_200103100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010320 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200103200000_GLOBE_VGT_V2.0.1/20010320/c_gls_DMP-DMP_200103200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010331 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200103310000_GLOBE_VGT_V2.0.1/20010331/c_gls_DMP-DMP_200103310000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010410 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200104100000_GLOBE_VGT_V2.0.1/20010410/c_gls_DMP-DMP_200104100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010420 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200104200000_GLOBE_VGT_V2.0.1/20010420/c_gls_DMP-DMP_200104200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010430 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200104300000_GLOBE_VGT_V2.0.1/20010430/c_gls_DMP-DMP_200104300000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010510 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200105100000_GLOBE_VGT_V2.0.1/20010510/c_gls_DMP-DMP_200105100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010520 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200105200000_GLOBE_VGT_V2.0.1/20010520/c_gls_DMP-DMP_200105200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010531 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200105310000_GLOBE_VGT_V2.0.1/20010531/c_gls_DMP-DMP_200105310000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010610 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200106100000_GLOBE_VGT_V2.0.1/20010610/c_gls_DMP-DMP_200106100000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010620 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200106200000_GLOBE_VGT_V2.0.1/20010620/c_gls_DMP-DMP_200106200000_CUSTOM_VGT_V2.0.1.tiff"))
drymatter_20010630 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP_200106300000_GLOBE_VGT_V2.0.1/20010630/c_gls_DMP-DMP_200106300000_CUSTOM_VGT_V2.0.1.tiff"))

drymatter_2000 <- (drymatter_19990310 + drymatter_19990320 + drymatter_19990331 +
                   drymatter_19990410 + drymatter_19990420 + drymatter_19990430 +
                   drymatter_19990510 + drymatter_19990520 + drymatter_19990531 +
                   drymatter_19990610 + drymatter_19990620 + drymatter_19990630 +
                   drymatter_20000310 + drymatter_20000320 + drymatter_20000331 +
                   drymatter_20000410 + drymatter_20000420 + drymatter_20000430 +
                   drymatter_20000510 + drymatter_20000520 + drymatter_20000531 +
                   drymatter_20000610 + drymatter_20000620 + drymatter_20000630 +
                   drymatter_20010310 + drymatter_20010320 + drymatter_20010331 +
                   drymatter_20010410 + drymatter_20010420 + drymatter_20010430 +
                   drymatter_20010510 + drymatter_20010520 + drymatter_20010531 +
                   drymatter_20010610 + drymatter_20010620 + drymatter_20010630)/36
writeRaster(drymatter_2000,'raw_data/drymatter/drymatter_2000.tif')

drymatter_20170310 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201703100000_GLOBE_PROBAV_V2.0.1/20170310/c_gls_DMP-RT6-DMP_201703100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170320 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201703200000_GLOBE_PROBAV_V2.0.1/20170320/c_gls_DMP-RT6-DMP_201703200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170331 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201703310000_GLOBE_PROBAV_V2.0.1/20170331/c_gls_DMP-RT6-DMP_201703310000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170410 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201704100000_GLOBE_PROBAV_V2.0.1/20170410/c_gls_DMP-RT6-DMP_201704100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170420 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201704200000_GLOBE_PROBAV_V2.0.1/20170420/c_gls_DMP-RT6-DMP_201704200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170430 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201704300000_GLOBE_PROBAV_V2.0.1/20170430/c_gls_DMP-RT6-DMP_201704300000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170510 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201705100000_GLOBE_PROBAV_V2.0.1/20170510/c_gls_DMP-RT6-DMP_201705100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170520 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201705200000_GLOBE_PROBAV_V2.0.1/20170520/c_gls_DMP-RT6-DMP_201705200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170531 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201705310000_GLOBE_PROBAV_V2.0.1/20170531/c_gls_DMP-RT6-DMP_201705310000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170610 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201706100000_GLOBE_PROBAV_V2.0.1/20170610/c_gls_DMP-RT6-DMP_201706100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170620 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201706200000_GLOBE_PROBAV_V2.0.1/20170620/c_gls_DMP-RT6-DMP_201706200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20170630 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201706300000_GLOBE_PROBAV_V2.0.1/20170630/c_gls_DMP-RT6-DMP_201706300000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180310 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201803100000_GLOBE_PROBAV_V2.0.1/20180310/c_gls_DMP-RT6-DMP_201803100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180320 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201803200000_GLOBE_PROBAV_V2.0.1/20180320/c_gls_DMP-RT6-DMP_201803200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180331 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201803310000_GLOBE_PROBAV_V2.0.1/20180331/c_gls_DMP-RT6-DMP_201803310000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180410 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201804100000_GLOBE_PROBAV_V2.0.1/20180410/c_gls_DMP-RT6-DMP_201804100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180420 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201804200000_GLOBE_PROBAV_V2.0.1/20180420/c_gls_DMP-RT6-DMP_201804200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180430 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201804300000_GLOBE_PROBAV_V2.0.1/20180430/c_gls_DMP-RT6-DMP_201804300000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180510 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201805100000_GLOBE_PROBAV_V2.0.1/20180510/c_gls_DMP-RT6-DMP_201805100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180520 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201805200000_GLOBE_PROBAV_V2.0.1/20180520/c_gls_DMP-RT6-DMP_201805200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180531 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201805310000_GLOBE_PROBAV_V2.0.1/20180531/c_gls_DMP-RT6-DMP_201805310000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180610 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201806100000_GLOBE_PROBAV_V2.0.1/20180610/c_gls_DMP-RT6-DMP_201806100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180620 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201806200000_GLOBE_PROBAV_V2.0.1/20180620/c_gls_DMP-RT6-DMP_201806200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20180630 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201806300000_GLOBE_PROBAV_V2.0.1/20180630/c_gls_DMP-RT6-DMP_201806300000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190310 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201903100000_GLOBE_PROBAV_V2.0.1/20190310/c_gls_DMP-RT6-DMP_201903100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190320 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201903200000_GLOBE_PROBAV_V2.0.1/20190320/c_gls_DMP-RT6-DMP_201903200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190331 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201903310000_GLOBE_PROBAV_V2.0.1/20190331/c_gls_DMP-RT6-DMP_201903310000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190410 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201904100000_GLOBE_PROBAV_V2.0.1/20190410/c_gls_DMP-RT6-DMP_201904100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190420 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201904200000_GLOBE_PROBAV_V2.0.1/20190420/c_gls_DMP-RT6-DMP_201904200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190430 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201904300000_GLOBE_PROBAV_V2.0.1/20190430/c_gls_DMP-RT6-DMP_201904300000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190510 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201905100000_GLOBE_PROBAV_V2.0.1/20190510/c_gls_DMP-RT6-DMP_201905100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190520 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201905200000_GLOBE_PROBAV_V2.0.1/20190520/c_gls_DMP-RT6-DMP_201905200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190531 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201905310000_GLOBE_PROBAV_V2.0.1/20190531/c_gls_DMP-RT6-DMP_201905310000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190610 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201906100000_GLOBE_PROBAV_V2.0.1/20190610/c_gls_DMP-RT6-DMP_201906100000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190620 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201906200000_GLOBE_PROBAV_V2.0.1/20190620/c_gls_DMP-RT6-DMP_201906200000_CUSTOM_PROBAV_V2.0.1.tiff"))
drymatter_20190630 <- rast(raster(x = "raw_data/drymatter/c_gls_DMP-RT6_201906300000_GLOBE_PROBAV_V2.0.1/20190630/c_gls_DMP-RT6-DMP_201906300000_CUSTOM_PROBAV_V2.0.1.tiff"))

drymatter_2018 <- (drymatter_20170310 + drymatter_20170320 + drymatter_20170331 +
                     drymatter_20170410 + drymatter_20170420 + drymatter_20170430 +
                     drymatter_20170510 + drymatter_20170520 + drymatter_20170531 +
                     drymatter_20170610 + drymatter_20170620 + drymatter_20170630 +
                     drymatter_20180310 + drymatter_20180320 + drymatter_20180331 +
                     drymatter_20180410 + drymatter_20180420 + drymatter_20180430 +
                     drymatter_20180510 + drymatter_20180520 + drymatter_20180531 +
                     drymatter_20180610 + drymatter_20180620 + drymatter_20180630 +
                     drymatter_20190310 + drymatter_20190320 + drymatter_20190331 +
                     drymatter_20190410 + drymatter_20190420 + drymatter_20190430 +
                     drymatter_20190510 + drymatter_20190520 + drymatter_20190531 +
                     drymatter_20190610 + drymatter_20190620 + drymatter_20190630)/36
writeRaster(drymatter_2018,'raw_data/drymatter/drymatter_2018.tif')

drymatter_2000 <- rast(raster(x = "raw_data/drymatter/drymatter_2000.tif"))
drymatter_2018 <- rast(raster(x = "raw_data/drymatter/drymatter_2018.tif"))

## Decline in productivity

decline_productivity <- read_sf(dsn = "raw_data/decline_productivity/", layer = "lpd_int2")


# Landscape

## Small wood features

smallwoodyfeatures <- rast(raster(x = "raw_data/small_woody_feature/SWF_2018_100m_eu_03035_V1_0/Data/SWF_2018_100m_eu_03035_V1_0.tif"))

## Landscape fragmentation

fragmentation <- rast(raster(x = "raw_data/fragmentation/eea_r_3035_100_m_fga2-s-2012_p_2012-2016_v01_r00/FGA2_S_2012_v3.tif"))

## Forest intergrity

forestintegrity <- rast(raster(x = "raw_data/forest_integrity/flii_Europe.tif"))


# Climate

## Mean temperature

mean_temperature <- rast(raster(x = "raw_data/climate/tg_ens_mean_0.1deg_reg_v28.0e.nc"))

r_temp <- brick("raw_data/climate/tg_ens_mean_0.1deg_reg_v28.0e.nc")

### Average daily data by year

end_year <- 0
for(i in 1:(2023-1950)){
  print(i)
  beg_year <- end_year+1
  end_year <- end_year+365
  year <- paste0("temp_",1949+i)
  if(i %in% c(seq(1952,2023,4)-1949)){
    end_year <- end_year+1 # account for bisextil years
  }
  map_year <- assign(year, mean(r_temp[[beg_year:end_year]], na.rm=T))
  path <- paste0("output/temp_",1949+i,".tif")
  writeRaster(x=map_year, filename=path, overwrite=T)
}

for(i in 1:(2023-1950)){
  print(i)
  year <- paste0("temp_",1949+i)
  path <- paste0("output/temp_",1949+i,".tif")
  assign(year, rast(raster(path)))
}

temp_2000_average <- (temp_1998 + temp_1999 + temp_2000 + temp_2001 + temp_2002)/5
temp_2020_average <- (temp_2018 + temp_2019 + temp_2020 + temp_2021 + temp_2022)/5
writeRaster(temp_2000_average,'raw_data/climate/temp_2000_average.tif')
writeRaster(temp_2020_average,'raw_data/climate/temp_2020_average.tif')

temp_2000_average <- rast(raster(x = "raw_data/climate/temp_2000_average.tif"))
temp_2020_average <- rast(raster(x = "raw_data/climate/temp_2020_average.tif"))

### Average daily data by spring

end_year <- 0
for(i in 1:(2023-1950)){
  print(i)
  beg_year <- end_year+1
  beg_spring <- beg_year + 60
  end_spring <- beg_spring + 122
  end_year <- end_year+365
  year <- paste0("temp_",1949+i)
  if(i %in% c(seq(1952,2023,4)-1949)){
    end_year <- end_year+1 # account for bisextil years
    beg_spring <- beg_spring + 1
    end_spring <- end_spring + 1
  }
  map_year <- assign(year, mean(r_temp[[beg_spring:end_spring]], na.rm=T))
  path <- paste0("output/temp_spring_",1949+i,".tif")
  writeRaster(x=map_year, filename=path, overwrite=T)
  map_year_var <- assign(year, calc(r_temp[[beg_spring:end_spring]], var))
  path <- paste0("output/temp_spring_var_",1949+i,".tif")
  writeRaster(x=map_year_var, filename=path, overwrite=T)
}

for(i in 1:(2023-1950)){
  print(i)
  year <- paste0("temp_spring_",1949+i)
  path <- paste0("output/temp_spring_",1949+i,".tif")
  year_var <- paste0("temp_spring_var_",1949+i)
  path_var <- paste0("output/temp_spring_var_",1949+i,".tif")
  assign(year, rast(raster(path)))
  assign(year_var, rast(raster(path_var)))
}

temp_spring_2000_average <- (temp_spring_1998 + temp_spring_1999 + temp_spring_2000 + temp_spring_2001 + temp_spring_2002)/5
temp_spring_2020_average <- (temp_spring_2018 + temp_spring_2019 + temp_spring_2020 + temp_spring_2021 + temp_spring_2022)/5
writeRaster(temp_spring_2000_average,'raw_data/climate/temp_spring_2000_average.tif')
writeRaster(temp_spring_2020_average,'raw_data/climate/temp_spring_2020_average.tif')

temp_spring_var_2000_average <- (temp_spring_var_1998 + temp_spring_var_1999 + temp_spring_var_2000 + temp_spring_var_2001 + temp_spring_var_2002)/5
temp_spring_var_2020_average <- (temp_spring_var_2018 + temp_spring_var_2019 + temp_spring_var_2020 + temp_spring_var_2021 + temp_spring_var_2022)/5
writeRaster(temp_spring_var_2000_average,'raw_data/climate/temp_spring_var_2000_average.tif')
writeRaster(temp_spring_var_2020_average,'raw_data/climate/temp_spring_var_2020_average.tif')

temp_spring_2000_average <- rast(raster(x = "raw_data/climate/temp_spring_2000_average.tif"))
temp_spring_2020_average <- rast(raster(x = "raw_data/climate/temp_spring_2020_average.tif"))

temp_spring_var_2000_average <- rast(raster(x = "raw_data/climate/temp_spring_var_2000_average.tif"))
temp_spring_var_2020_average <- rast(raster(x = "raw_data/climate/temp_spring_var_2020_average.tif"))

## Min temperature

min_temperature <- rast(raster(x = "raw_data/climate/tn_ens_mean_0.1deg_reg_v28.0e.nc"))

r_temp <- brick("raw_data/climate/tn_ens_mean_0.1deg_reg_v28.0e.nc")

### Average daily data by spring

end_year <- 0
for(i in 1:(2023-1950)){
  print(i)
  beg_year <- end_year+1
  beg_spring <- beg_year + 60
  end_spring <- beg_spring + 122
  end_year <- end_year+365
  year <- paste0("temp_min_",1949+i)
  if(i %in% c(seq(1952,2023,4)-1949)){
    end_year <- end_year+1 # account for bisextil years
    beg_spring <- beg_spring + 1
    end_spring <- end_spring + 1
  }
  map_year <- assign(year, mean(r_temp[[beg_spring:end_spring]], na.rm=T))
  path <- paste0("output/temp_min_spring_",1949+i,".tif")
  writeRaster(x=map_year, filename=path, overwrite=T)
  map_year_var <- assign(year, calc(r_temp[[beg_spring:end_spring]], var))
  path <- paste0("output/temp_min_spring_var_",1949+i,".tif")
  writeRaster(x=map_year_var, filename=path, overwrite=T)
}

for(i in 1:(2023-1950)){
  print(i)
  year <- paste0("temp_min_spring_",1949+i)
  path <- paste0("output/temp_min_spring_",1949+i,".tif")
  year_var <- paste0("temp_min_spring_var_",1949+i)
  path_var <- paste0("output/temp_min_spring_var_",1949+i,".tif")
  assign(year, rast(raster(path)))
  assign(year_var, rast(raster(path_var)))
}

temp_min_spring_2000_average <- (temp_min_spring_1998 + temp_min_spring_1999 + temp_min_spring_2000 + temp_min_spring_2001 + temp_min_spring_2002)/5
temp_min_spring_2020_average <- (temp_min_spring_2018 + temp_min_spring_2019 + temp_min_spring_2020 + temp_min_spring_2021 + temp_min_spring_2022)/5
writeRaster(temp_min_spring_2000_average,'raw_data/climate/temp_min_spring_2000_average.tif')
writeRaster(temp_min_spring_2020_average,'raw_data/climate/temp_min_spring_2020_average.tif')

temp_min_spring_var_2000_average <- (temp_min_spring_var_1998 + temp_min_spring_var_1999 + temp_min_spring_var_2000 + temp_min_spring_var_2001 + temp_min_spring_var_2002)/5
temp_min_spring_var_2020_average <- (temp_min_spring_var_2018 + temp_min_spring_var_2019 + temp_min_spring_var_2020 + temp_min_spring_var_2021 + temp_min_spring_var_2022)/5
writeRaster(temp_min_spring_var_2000_average,'raw_data/climate/temp_min_spring_var_2000_average.tif')
writeRaster(temp_min_spring_var_2020_average,'raw_data/climate/temp_min_spring_var_2020_average.tif')

temp_min_spring_2000_average <- rast(raster(x = "raw_data/climate/temp_min_spring_2000_average.tif"))
temp_min_spring_2020_average <- rast(raster(x = "raw_data/climate/temp_min_spring_2020_average.tif"))

temp_min_spring_var_2000_average <- rast(raster(x = "raw_data/climate/temp_min_spring_var_2000_average.tif"))
temp_min_spring_var_2020_average <- rast(raster(x = "raw_data/climate/temp_min_spring_var_2020_average.tif"))


## Max temperature

max_temperature <- rast(raster(x = "raw_data/climate/tx_ens_mean_0.1deg_reg_v28.0e.nc"))

r_temp <- brick("raw_data/climate/tx_ens_mean_0.1deg_reg_v28.0e.nc")

### Average daily data by spring

end_year <- 0
for(i in 1:(2023-1950)){
  print(i)
  beg_year <- end_year+1
  beg_spring <- beg_year + 60
  end_spring <- beg_spring + 122
  end_year <- end_year+365
  year <- paste0("temp_max_",1949+i)
  if(i %in% c(seq(1952,2023,4)-1949)){
    end_year <- end_year+1 # account for bisextil years
    beg_spring <- beg_spring + 1
    end_spring <- end_spring + 1
  }
  map_year <- assign(year, mean(r_temp[[beg_spring:end_spring]], na.rm=T))
  path <- paste0("output/temp_max_spring_",1949+i,".tif")
  writeRaster(x=map_year, filename=path, overwrite=T)
  map_year_var <- assign(year, calc(r_temp[[beg_spring:end_spring]], var))
  path <- paste0("output/temp_max_spring_var_",1949+i,".tif")
  writeRaster(x=map_year_var, filename=path, overwrite=T)
}

for(i in 1:(2023-1950)){
  print(i)
  year <- paste0("temp_max_spring_",1949+i)
  path <- paste0("output/temp_max_spring_",1949+i,".tif")
  year_var <- paste0("temp_max_spring_var_",1949+i)
  path_var <- paste0("output/temp_max_spring_var_",1949+i,".tif")
  assign(year, rast(raster(path)))
  assign(year_var, rast(raster(path_var)))
}

temp_max_spring_2000_average <- (temp_max_spring_1998 + temp_max_spring_1999 + temp_max_spring_2000 + temp_max_spring_2001 + temp_max_spring_2002)/5
temp_max_spring_2020_average <- (temp_max_spring_2018 + temp_max_spring_2019 + temp_max_spring_2020 + temp_max_spring_2021 + temp_max_spring_2022)/5
writeRaster(temp_max_spring_2000_average,'raw_data/climate/temp_max_spring_2000_average.tif')
writeRaster(temp_max_spring_2020_average,'raw_data/climate/temp_max_spring_2020_average.tif')

temp_max_spring_var_2000_average <- (temp_max_spring_var_1998 + temp_max_spring_var_1999 + temp_max_spring_var_2000 + temp_max_spring_var_2001 + temp_max_spring_var_2002)/5
temp_max_spring_var_2020_average <- (temp_max_spring_var_2018 + temp_max_spring_var_2019 + temp_max_spring_var_2020 + temp_max_spring_var_2021 + temp_max_spring_var_2022)/5
writeRaster(temp_max_spring_var_2000_average,'raw_data/climate/temp_max_spring_var_2000_average.tif')
writeRaster(temp_max_spring_var_2020_average,'raw_data/climate/temp_max_spring_var_2020_average.tif')

temp_max_spring_2000_average <- rast(raster(x = "raw_data/climate/temp_max_spring_2000_average.tif"))
temp_max_spring_2020_average <- rast(raster(x = "raw_data/climate/temp_max_spring_2020_average.tif"))

temp_max_spring_var_2000_average <- rast(raster(x = "raw_data/climate/temp_max_spring_var_2000_average.tif"))
temp_max_spring_var_2020_average <- rast(raster(x = "raw_data/climate/temp_max_spring_var_2020_average.tif"))


## Sum of precipitation

sum_precipitation <- rast(raster(x = "raw_data/climate/rr_ens_mean_0.1deg_reg_v28.0e.nc"))


r_prec <- brick("raw_data/climate/rr_ens_mean_0.1deg_reg_v28.0e.nc")

### Average daily data by year

end_year <- 0
for(i in 1:(2023-1950)){
  print(i)
  beg_year <- end_year+1
  end_year <- end_year+365
  year <- paste0("prec_",1949+i)
  if(i %in% c(seq(1952,2023,4)-1949)){
    end_year <- end_year+1 # account for bisextil years
  }
  map_year <- assign(year, mean(r_prec[[beg_year:end_year]], na.rm=T))
  path <- paste0("output/prec_",1949+i,".tif")
  writeRaster(x=map_year, filename=path, overwrite=T)
}

for(i in 1:(2023-1950)){
  print(i)
  year <- paste0("prec_",1949+i)
  path <- paste0("output/prec_",1949+i,".tif")
  assign(year, rast(raster(path)))
}

prec_2000_average <- (prec_1998 + prec_1999 + prec_2000 + prec_2001 + prec_2002)/5
prec_2020_average <- (prec_2018 + prec_2019 + prec_2020 + prec_2021 + prec_2022)/5
writeRaster(prec_2000_average,'raw_data/climate/prec_2000_average.tif')
writeRaster(prec_2020_average,'raw_data/climate/prec_2020_average.tif')

prec_2000_average <- rast(raster(x = "raw_data/climate/prec_2000_average.tif"))
prec_2020_average <- rast(raster(x = "raw_data/climate/prec_2020_average.tif"))

### Average daily data by spring

end_year <- 0
for(i in 1:(2023-1950)){
  print(i)
  beg_year <- end_year+1
  beg_spring <- beg_year + 60
  end_spring <- beg_spring + 122
  end_year <- end_year+365
  year <- paste0("prec_",1949+i)
  if(i %in% c(seq(1952,2023,4)-1949)){
    end_year <- end_year+1 # account for bisextil years
    beg_spring <- beg_spring + 1
    end_spring <- end_spring + 1
  }
  map_year <- assign(year, mean(r_prec[[beg_spring:end_spring]], na.rm=T))
  path <- paste0("output/prec_spring_",1949+i,".tif")
  writeRaster(x=map_year, filename=path, overwrite=T)
  map_year_var <- assign(year, calc(r_prec[[beg_spring:end_spring]], var))
  path <- paste0("output/prec_spring_var_",1949+i,".tif")
  writeRaster(x=map_year_var, filename=path, overwrite=T)
}

for(i in 1:(2023-1950)){
  print(i)
  year <- paste0("prec_spring_",1949+i)
  path <- paste0("output/prec_spring_",1949+i,".tif")
  year_var <- paste0("prec_spring_var_",1949+i)
  path_var <- paste0("output/prec_spring_var_",1949+i,".tif")
  assign(year, rast(raster(path)))
  assign(year_var, rast(raster(path_var)))
}

prec_spring_2000_average <- (prec_spring_1998 + prec_spring_1999 + prec_spring_2000 + prec_spring_2001 + prec_spring_2002)/5
prec_spring_2020_average <- (prec_spring_2018 + prec_spring_2019 + prec_spring_2020 + prec_spring_2021 + prec_spring_2022)/5
writeRaster(prec_spring_2000_average,'raw_data/climate/prec_spring_2000_average.tif')
writeRaster(prec_spring_2020_average,'raw_data/climate/prec_spring_2020_average.tif')

prec_spring_var_2000_average <- (prec_spring_var_1998 + prec_spring_var_1999 + prec_spring_var_2000 + prec_spring_var_2001 + prec_spring_var_2002)/5
prec_spring_var_2020_average <- (prec_spring_var_2018 + prec_spring_var_2019 + prec_spring_var_2020 + prec_spring_var_2021 + prec_spring_var_2022)/5
writeRaster(prec_spring_var_2000_average,'raw_data/climate/prec_spring_var_2000_average.tif')
writeRaster(prec_spring_var_2020_average,'raw_data/climate/prec_spring_var_2020_average.tif')

prec_spring_2000_average <- rast(raster(x = "raw_data/climate/prec_spring_2000_average.tif"))
prec_spring_2020_average <- rast(raster(x = "raw_data/climate/prec_spring_2020_average.tif"))

prec_spring_var_2000_average <- rast(raster(x = "raw_data/climate/prec_spring_var_2000_average.tif"))
prec_spring_var_2020_average <- rast(raster(x = "raw_data/climate/prec_spring_var_2020_average.tif"))


## Relative humidity

relative_humidity <- rast(raster(x = "raw_data/climate/hu_ens_mean_0.1deg_reg_v28.0e.nc"))


r_humidity <- brick("raw_data/climate/hu_ens_mean_0.1deg_reg_v28.0e.nc")

### Average daily data by year

end_year <- 0
for(i in 1:(2023-1950)){
  print(i)
  beg_year <- end_year+1
  end_year <- end_year+365
  year <- paste0("humidity_",1949+i)
  if(i %in% c(seq(1952,2023,4)-1949)){
    end_year <- end_year+1 # account for bisextil years
  }
  map_year <- assign(year, mean(r_humidity[[beg_year:end_year]], na.rm=T))
  path <- paste0("output/humidity_",1949+i,".tif")
  writeRaster(x=map_year, filename=path, overwrite=T)
}

for(i in 1:(2023-1950)){
  print(i)
  year <- paste0("humidity_",1949+i)
  path <- paste0("output/humidity_",1949+i,".tif")
  assign(year, rast(raster(path)))
}

humidity_2000_average <- (humidity_1998 + humidity_1999 + humidity_2000 + humidity_2001 + humidity_2002)/5
humidity_2020_average <- (humidity_2018 + humidity_2019 + humidity_2020 + humidity_2021 + humidity_2022)/5
writeRaster(humidity_2000_average,'raw_data/climate/humidity_2000_average.tif')
writeRaster(humidity_2020_average,'raw_data/climate/humidity_2020_average.tif')

humidity_2000_average <- rast(raster(x = "raw_data/climate/humidity_2000_average.tif"))
humidity_2020_average <- rast(raster(x = "raw_data/climate/humidity_2020_average.tif"))

### Average daily data by spring

end_year <- 0
for(i in 1:(2023-1950)){
  print(i)
  beg_year <- end_year+1
  beg_spring <- beg_year + 60
  end_spring <- beg_spring + 122
  end_year <- end_year+365
  year <- paste0("humidity_",1949+i)
  if(i %in% c(seq(1952,2023,4)-1949)){
    end_year <- end_year+1 # account for bisextil years
    beg_spring <- beg_spring + 1
    end_spring <- end_spring + 1
  }
  map_year <- assign(year, mean(r_humidity[[beg_spring:end_spring]], na.rm=T))
  path <- paste0("output/humidity_spring_",1949+i,".tif")
  writeRaster(x=map_year, filename=path, overwrite=T)
  map_year_var <- assign(year, calc(r_humidity[[beg_spring:end_spring]], var))
  path <- paste0("output/humidity_spring_var_",1949+i,".tif")
  writeRaster(x=map_year_var, filename=path, overwrite=T)
}

for(i in 1:(2023-1950)){
  print(i)
  year <- paste0("humidity_spring_",1949+i)
  path <- paste0("output/humidity_spring_",1949+i,".tif")
  year_var <- paste0("humidity_spring_var_",1949+i)
  path_var <- paste0("output/humidity_spring_var_",1949+i,".tif")
  assign(year, rast(raster(path)))
  assign(year_var, rast(raster(path_var)))
}

humidity_spring_2000_average <- (humidity_spring_1998 + humidity_spring_1999 + humidity_spring_2000 + humidity_spring_2001 + humidity_spring_2002)/5
humidity_spring_2020_average <- (humidity_spring_2018 + humidity_spring_2019 + humidity_spring_2020 + humidity_spring_2021 + humidity_spring_2022)/5
writeRaster(humidity_spring_2000_average,'raw_data/climate/humidity_spring_2000_average.tif')
writeRaster(humidity_spring_2020_average,'raw_data/climate/humidity_spring_2020_average.tif')

humidity_spring_var_2000_average <- (humidity_spring_var_1998 + humidity_spring_var_1999 + humidity_spring_var_2000 + humidity_spring_var_2001 + humidity_spring_var_2002)/5
humidity_spring_var_2020_average <- (humidity_spring_var_2018 + humidity_spring_var_2019 + humidity_spring_var_2020 + humidity_spring_var_2021 + humidity_spring_var_2022)/5
writeRaster(humidity_spring_var_2000_average,'raw_data/climate/humidity_spring_var_2000_average.tif')
writeRaster(humidity_spring_var_2020_average,'raw_data/climate/humidity_spring_var_2020_average.tif')

humidity_spring_2000_average <- rast(raster(x = "raw_data/climate/humidity_spring_2000_average.tif"))
humidity_spring_2020_average <- rast(raster(x = "raw_data/climate/humidity_spring_2020_average.tif"))

humidity_spring_var_2000_average <- rast(raster(x = "raw_data/climate/humidity_spring_var_2000_average.tif"))
humidity_spring_var_2020_average <- rast(raster(x = "raw_data/climate/humidity_spring_var_2020_average.tif"))


# Socio-economic

## GDP

### per capita

r_gdp_cap <- brick("raw_data/GDP/GDP_per_capita_PPP_1990_2015_v2.nc")

writeRaster(x=r_gdp_cap[["X1998"]], filename="raw_data/GDP/GDP_pc_1998.tif")
GDP_pc_1998 <- rast(raster(x = "raw_data/GDP/GDP_pc_1998.tif"))
writeRaster(x=crop(GDP_pc_1998, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_1998.tif", overwrite = T)
GDP_pc_1998 <- rast(raster(x = "raw_data/GDP/GDP_pc_1998.tif"))

writeRaster(x=r_gdp_cap[["X1999"]], filename="raw_data/GDP/GDP_pc_1999.tif")
GDP_pc_1999 <- rast(raster(x = "raw_data/GDP/GDP_pc_1999.tif"))
writeRaster(x=crop(GDP_pc_1999, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_1999.tif", overwrite = T)
GDP_pc_1999 <- rast(raster(x = "raw_data/GDP/GDP_pc_1999.tif"))

writeRaster(x=r_gdp_cap[["X2000"]], filename="raw_data/GDP/GDP_pc_2000.tif")
GDP_pc_2000 <- rast(raster(x = "raw_data/GDP/GDP_pc_2000.tif"))
writeRaster(x=crop(GDP_pc_2000, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_2000.tif", overwrite = T)
GDP_pc_2000 <- rast(raster(x = "raw_data/GDP/GDP_pc_2000.tif"))

writeRaster(x=r_gdp_cap[["X2001"]], filename="raw_data/GDP/GDP_pc_2001.tif")
GDP_pc_2001 <- rast(raster(x = "raw_data/GDP/GDP_pc_2001.tif"))
writeRaster(x=crop(GDP_pc_2001, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_2001.tif", overwrite = T)
GDP_pc_2001 <- rast(raster(x = "raw_data/GDP/GDP_pc_2001.tif"))

writeRaster(x=r_gdp_cap[["X2002"]], filename="raw_data/GDP/GDP_pc_2002.tif")
GDP_pc_2002 <- rast(raster(x = "raw_data/GDP/GDP_pc_2002.tif"))
writeRaster(x=crop(GDP_pc_2002, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_2002.tif", overwrite = T)
GDP_pc_2002 <- rast(raster(x = "raw_data/GDP/GDP_pc_2002.tif"))


GDP_pc_2000_average <- (GDP_pc_1998 + GDP_pc_1999 + GDP_pc_2000 + GDP_pc_2001 + GDP_pc_2002)/5

writeRaster(x=GDP_pc_2000_average, filename="raw_data/GDP/GDP_pc_2000_average.tif")


writeRaster(x=r_gdp_cap[["X2011"]], filename="raw_data/GDP/GDP_pc_2011.tif")
GDP_pc_2011 <- rast(raster(x = "raw_data/GDP/GDP_pc_2011.tif"))
writeRaster(x=crop(GDP_pc_2011, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_2011.tif", overwrite = T)
GDP_pc_2011 <- rast(raster(x = "raw_data/GDP/GDP_pc_2011.tif"))

writeRaster(x=r_gdp_cap[["X2012"]], filename="raw_data/GDP/GDP_pc_2012.tif")
GDP_pc_2012 <- rast(raster(x = "raw_data/GDP/GDP_pc_2012.tif"))
writeRaster(x=crop(GDP_pc_2012, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_2012.tif", overwrite = T)
GDP_pc_2012 <- rast(raster(x = "raw_data/GDP/GDP_pc_2012.tif"))

writeRaster(x=r_gdp_cap[["X2013"]], filename="raw_data/GDP/GDP_pc_2013.tif")
GDP_pc_2013 <- rast(raster(x = "raw_data/GDP/GDP_pc_2013.tif"))
writeRaster(x=crop(GDP_pc_2013, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_2013.tif", overwrite = T)
GDP_pc_2013 <- rast(raster(x = "raw_data/GDP/GDP_pc_2013.tif"))

writeRaster(x=r_gdp_cap[["X2014"]], filename="raw_data/GDP/GDP_pc_2014.tif")
GDP_pc_2014 <- rast(raster(x = "raw_data/GDP/GDP_pc_2014.tif"))
writeRaster(x=crop(GDP_pc_2014, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_2014.tif", overwrite = T)
GDP_pc_2014 <- rast(raster(x = "raw_data/GDP/GDP_pc_2014.tif"))

writeRaster(x=r_gdp_cap[["X2015"]], filename="raw_data/GDP/GDP_pc_2015.tif")
GDP_pc_2015 <- rast(raster(x = "raw_data/GDP/GDP_pc_2015.tif"))
writeRaster(x=crop(GDP_pc_2015, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_pc_2015.tif", overwrite = T)
GDP_pc_2015 <- rast(raster(x = "raw_data/GDP/GDP_pc_2015.tif"))


GDP_pc_2013_average <- (GDP_pc_2011 + GDP_pc_2012 + GDP_pc_2013 + GDP_pc_2014 + GDP_pc_2015)/5

writeRaster(x=GDP_pc_2013_average, filename="raw_data/GDP/GDP_pc_2013_average.tif")

### per area

r_gdp <- brick("raw_data/GDP/GDP_PPP_30arcsec_v3.nc")

writeRaster(x=r_gdp[["X2000"]], filename="raw_data/GDP/GDP_2000.tif")
GDP_2000 <- rast(raster(x = "raw_data/GDP/GDP_2000.tif"))
writeRaster(x=crop(GDP_2000, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_2000.tif", overwrite = T)
GDP_2000 <- rast(raster(x = "raw_data/GDP/GDP_2000.tif"))

writeRaster(x=r_gdp[["X2015"]], filename="raw_data/GDP/GDP_2015.tif")
GDP_2015 <- rast(raster(x = "raw_data/GDP/GDP_2015.tif"))
writeRaster(x=crop(GDP_2015, ext(-25, 45, 26, 76)), filename="raw_data/GDP/GDP_2015.tif", overwrite = T)
GDP_2015 <- rast(raster(x = "raw_data/GDP/GDP_2015.tif"))

## Population

### 1995

pop_r2_c17 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C17/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C17.tif"))
pop_r2_c18 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C18/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C18.tif"))
pop_r2_c19 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C19/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C19.tif"))
pop_r2_c20 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C20/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C20.tif"))
pop_r2_c21 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C21/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R2_C21.tif"))
pop_r3_c18 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R3_C18/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif"))
pop_r3_c19 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R3_C19/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"))
pop_r3_c20 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R3_C20/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R3_C20.tif"))
pop_r3_c21 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R3_C21/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R3_C21.tif"))
pop_r4_c18 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R4_C18/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R4_C18.tif"))
pop_r4_c19 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R4_C19/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))
pop_r4_c20 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R4_C20/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif"))
pop_r4_c21 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R4_C21/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R4_C21.tif"))
pop_r5_c18 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C18/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C18.tif"))
pop_r5_c19 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C19/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C19.tif"))
pop_r5_c20 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C20/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C20.tif"))
pop_r5_c21 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C21/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif"))
pop_r5_c22 <- rast(raster(x = "raw_data/population/1995/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C22/GHS_POP_E1995_GLOBE_R2023A_54009_100_V1_0_R5_C22.tif"))

pop_all <- sprc(pop_r2_c17,pop_r2_c18,pop_r2_c19,pop_r2_c20,pop_r2_c21,
                pop_r3_c18,pop_r3_c19,pop_r3_c20,pop_r3_c21,
                pop_r4_c18,pop_r4_c19,pop_r4_c20,pop_r4_c21,
                pop_r5_c18,pop_r5_c19,pop_r5_c20,pop_r5_c21,pop_r5_c22)
population_1995 <- merge(pop_all)

writeRaster(population_1995,'raw_data/population/population_1995.tif')

population_1995 <- raster(x = "raw_data/population/population_1995.tif")

### 2000

pop_r2_c17 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C17/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C17.tif"))
pop_r2_c18 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C18/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C18.tif"))
pop_r2_c19 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C19/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C19.tif"))
pop_r2_c20 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C20/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C20.tif"))
pop_r2_c21 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C21/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R2_C21.tif"))
pop_r3_c18 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R3_C18/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif"))
pop_r3_c19 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R3_C19/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"))
pop_r3_c20 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R3_C20/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R3_C20.tif"))
pop_r3_c21 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R3_C21/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R3_C21.tif"))
pop_r4_c18 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R4_C18/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R4_C18.tif"))
pop_r4_c19 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R4_C19/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))
pop_r4_c20 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R4_C20/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif"))
pop_r4_c21 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R4_C21/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R4_C21.tif"))
pop_r5_c18 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C18/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C18.tif"))
pop_r5_c19 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C19/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C19.tif"))
pop_r5_c20 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C20/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C20.tif"))
pop_r5_c21 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C21/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif"))
pop_r5_c22 <- rast(raster(x = "raw_data/population/2000/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C22/GHS_POP_E2000_GLOBE_R2023A_54009_100_V1_0_R5_C22.tif"))

pop_all <- sprc(pop_r2_c17,pop_r2_c18,pop_r2_c19,pop_r2_c20,pop_r2_c21,
                pop_r3_c18,pop_r3_c19,pop_r3_c20,pop_r3_c21,
                pop_r4_c18,pop_r4_c19,pop_r4_c20,pop_r4_c21,
                pop_r5_c18,pop_r5_c19,pop_r5_c20,pop_r5_c21,pop_r5_c22)
population_2000 <- merge(pop_all)

writeRaster(population_2000,'raw_data/population/population_2000.tif')

population_2000 <- rast(raster(x = "raw_data/population/population_2000.tif"))

### 2005

pop_r2_c17 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C17/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C17.tif"))
pop_r2_c18 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C18/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C18.tif"))
pop_r2_c19 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C19/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C19.tif"))
pop_r2_c20 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C20/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C20.tif"))
pop_r2_c21 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C21/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R2_C21.tif"))
pop_r3_c18 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R3_C18/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif"))
pop_r3_c19 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R3_C19/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"))
pop_r3_c20 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R3_C20/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R3_C20.tif"))
pop_r3_c21 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R3_C21/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R3_C21.tif"))
pop_r4_c18 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R4_C18/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R4_C18.tif"))
pop_r4_c19 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R4_C19/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))
pop_r4_c20 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R4_C20/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif"))
pop_r4_c21 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R4_C21/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R4_C21.tif"))
pop_r5_c18 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C18/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C18.tif"))
pop_r5_c19 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C19/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C19.tif"))
pop_r5_c20 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C20/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C20.tif"))
pop_r5_c21 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C21/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif"))
pop_r5_c22 <- rast(raster(x = "raw_data/population/2005/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C22/GHS_POP_E2005_GLOBE_R2023A_54009_100_V1_0_R5_C22.tif"))

pop_all <- sprc(pop_r2_c17,pop_r2_c18,pop_r2_c19,pop_r2_c20,pop_r2_c21,
                pop_r3_c18,pop_r3_c19,pop_r3_c20,pop_r3_c21,
                pop_r4_c18,pop_r4_c19,pop_r4_c20,pop_r4_c21,
                pop_r5_c18,pop_r5_c19,pop_r5_c20,pop_r5_c21,pop_r5_c22)
population_2005 <- merge(pop_all)

writeRaster(population_2005,'raw_data/population/population_2005.tif')

population_2005 <- raster(x = "raw_data/population/population_2005.tif")

### 2010

pop_r2_c17 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C17/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C17.tif"))
pop_r2_c18 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C18/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C18.tif"))
pop_r2_c19 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C19/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C19.tif"))
pop_r2_c20 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C20/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C20.tif"))
pop_r2_c21 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C21/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R2_C21.tif"))
pop_r3_c18 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R3_C18/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif"))
pop_r3_c19 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R3_C19/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"))
pop_r3_c20 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R3_C20/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R3_C20.tif"))
pop_r3_c21 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R3_C21/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R3_C21.tif"))
pop_r4_c18 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R4_C18/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R4_C18.tif"))
pop_r4_c19 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R4_C19/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))
pop_r4_c20 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R4_C20/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif"))
pop_r4_c21 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R4_C21/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R4_C21.tif"))
pop_r5_c18 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C18/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C18.tif"))
pop_r5_c19 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C19/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C19.tif"))
pop_r5_c20 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C20/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C20.tif"))
pop_r5_c21 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C21/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif"))
pop_r5_c22 <- rast(raster(x = "raw_data/population/2010/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C22/GHS_POP_E2010_GLOBE_R2023A_54009_100_V1_0_R5_C22.tif"))

pop_all <- sprc(pop_r2_c17,pop_r2_c18,pop_r2_c19,pop_r2_c20,pop_r2_c21,
                pop_r3_c18,pop_r3_c19,pop_r3_c20,pop_r3_c21,
                pop_r4_c18,pop_r4_c19,pop_r4_c20,pop_r4_c21,
                pop_r5_c18,pop_r5_c19,pop_r5_c20,pop_r5_c21,pop_r5_c22)
population_2010 <- merge(pop_all)

writeRaster(population_2010,'raw_data/population/population_2010.tif')

population_2010 <- raster(x = "raw_data/population/population_2010.tif")

### 2015

pop_r2_c17 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C17/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C17.tif"))
pop_r2_c18 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C18/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C18.tif"))
pop_r2_c19 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C19/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C19.tif"))
pop_r2_c20 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C20/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C20.tif"))
pop_r2_c21 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C21/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R2_C21.tif"))
pop_r3_c18 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R3_C18/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif"))
pop_r3_c19 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R3_C19/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"))
pop_r3_c20 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R3_C20/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R3_C20.tif"))
pop_r3_c21 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R3_C21/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R3_C21.tif"))
pop_r4_c18 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C18/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C18.tif"))
pop_r4_c19 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C19/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))
pop_r4_c20 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C20/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif"))
pop_r4_c21 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C21/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C21.tif"))
pop_r5_c18 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C18/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C18.tif"))
pop_r5_c19 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C19/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C19.tif"))
pop_r5_c20 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C20/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C20.tif"))
pop_r5_c21 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C21/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif"))
pop_r5_c22 <- rast(raster(x = "raw_data/population/2015/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C22/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R5_C22.tif"))

pop_all <- sprc(pop_r2_c17,pop_r2_c18,pop_r2_c19,pop_r2_c20,pop_r2_c21,
                pop_r3_c18,pop_r3_c19,pop_r3_c20,pop_r3_c21,
                pop_r4_c18,pop_r4_c19,pop_r4_c20,pop_r4_c21,
                pop_r5_c18,pop_r5_c19,pop_r5_c20,pop_r5_c21,pop_r5_c22)
population_2015 <- merge(pop_all)

writeRaster(population_2015,'raw_data/population/population_2015.tif')

population_2015 <- raster(x = "raw_data/population/population_2015.tif")

### 2020

pop_r2_c17 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C17/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C17.tif"))
pop_r2_c18 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C18/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C18.tif"))
pop_r2_c19 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C19/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C19.tif"))
pop_r2_c20 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C20/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C20.tif"))
pop_r2_c21 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C21/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R2_C21.tif"))
pop_r3_c18 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R3_C18/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif"))
pop_r3_c19 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R3_C19/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"))
pop_r3_c20 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R3_C20/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R3_C20.tif"))
pop_r3_c21 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R3_C21/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R3_C21.tif"))
pop_r4_c18 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C18/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C18.tif"))
pop_r4_c19 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C19/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))
pop_r4_c20 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C20/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif"))
pop_r4_c21 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C21/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R4_C21.tif"))
pop_r5_c18 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C18/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C18.tif"))
pop_r5_c19 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C19/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C19.tif"))
pop_r5_c20 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C20/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C20.tif"))
pop_r5_c21 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C21/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif"))
pop_r5_c22 <- rast(raster(x = "raw_data/population/2020/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C22/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R5_C22.tif"))

pop_all <- sprc(pop_r2_c17,pop_r2_c18,pop_r2_c19,pop_r2_c20,pop_r2_c21,
                pop_r3_c18,pop_r3_c19,pop_r3_c20,pop_r3_c21,
                pop_r4_c18,pop_r4_c19,pop_r4_c20,pop_r4_c21,
                pop_r5_c18,pop_r5_c19,pop_r5_c20,pop_r5_c21,pop_r5_c22)
population_2020 <- merge(pop_all)

writeRaster(population_2020,'raw_data/population/population_2020.tif')

population_2020 <- rast(raster(x = "raw_data/population/population_2020.tif"))

### 2025

pop_r2_c17 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C17/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C17.tif"))
pop_r2_c18 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C18/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C18.tif"))
pop_r2_c19 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C19/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C19.tif"))
pop_r2_c20 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C20/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C20.tif"))
pop_r2_c21 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C21/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R2_C21.tif"))
pop_r3_c18 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R3_C18/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif"))
pop_r3_c19 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R3_C19/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"))
pop_r3_c20 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R3_C20/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R3_C20.tif"))
pop_r3_c21 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R3_C21/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R3_C21.tif"))
pop_r4_c18 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C18/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C18.tif"))
pop_r4_c19 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C19/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))
pop_r4_c20 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C20/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif"))
pop_r4_c21 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C21/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C21.tif"))
pop_r5_c18 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C18/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C18.tif"))
pop_r5_c19 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C19/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C19.tif"))
pop_r5_c20 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C20/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C20.tif"))
pop_r5_c21 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C21/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif"))
pop_r5_c22 <- rast(raster(x = "raw_data/population/2025/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C22/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R5_C22.tif"))

pop_all <- sprc(pop_r2_c17,pop_r2_c18,pop_r2_c19,pop_r2_c20,pop_r2_c21,
                pop_r3_c18,pop_r3_c19,pop_r3_c20,pop_r3_c21,
                pop_r4_c18,pop_r4_c19,pop_r4_c20,pop_r4_c21,
                pop_r5_c18,pop_r5_c19,pop_r5_c20,pop_r5_c21,pop_r5_c22)
population_2025 <- merge(pop_all)

writeRaster(population_2025,'raw_data/population/population_2025.tif')

population_2025 <- raster(x = "raw_data/population/population_2025.tif")

### 2030

pop_r2_c17 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C17/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C17.tif"))
pop_r2_c18 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C18/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C18.tif"))
pop_r2_c19 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C19/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C19.tif"))
pop_r2_c20 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C20/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C20.tif"))
pop_r2_c21 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C21/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R2_C21.tif"))
pop_r3_c18 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C18/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif"))
pop_r3_c19 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C19/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"))
pop_r3_c20 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C20/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C20.tif"))
pop_r3_c21 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C21/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C21.tif"))
pop_r4_c18 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C18/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C18.tif"))
pop_r4_c19 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C19/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"))
pop_r4_c20 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C20/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C20.tif"))
pop_r4_c21 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C21/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R4_C21.tif"))
pop_r5_c18 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C18/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C18.tif"))
pop_r5_c19 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C19/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C19.tif"))
pop_r5_c20 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C20/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C20.tif"))
pop_r5_c21 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C21/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C21.tif"))
pop_r5_c22 <- rast(raster(x = "raw_data/population/2030/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C22/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R5_C22.tif"))


pop_all <- sprc(pop_r2_c17,pop_r2_c18,pop_r2_c19,pop_r2_c20,pop_r2_c21,
          pop_r3_c18,pop_r3_c19,pop_r3_c20,pop_r3_c21,
          pop_r4_c18,pop_r4_c19,pop_r4_c20,pop_r4_c21,
          pop_r5_c18,pop_r5_c19,pop_r5_c20,pop_r5_c21,pop_r5_c22)
population_2030 <- merge(pop_all)

writeRaster(population_2030,'raw_data/population/population_2030.tif')

population_2030 <- raster(x = "raw_data/population/population_2030.tif")


# Merge all layers at same scale

## Load 1km grid for EU from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/grids

grid_eu <- st_read("raw_data/grid_eu/grid_1km_surf.gpkg")

## Test function extraction

grid_eu_test <- grid_eu[which(grid_eu$NUTS2021_1=="FR1"),]
test1 <- project(population_2000, crs(grid_eu))
test1b <- crop(test1,ext(grid_eu_test))
test2 <- project(population_2020, crs(grid_eu))
test2b <- crop(test2,ext(grid_eu_test))

grid_eu_test$pop2000 <- excat_extract(test1,grid_eu_test, fun="sum")

ggplot(grid_eu_test) +
  geom_sf(aes(fill = sqrt(pop2000)))

## Apply on all datasets

### Population

population_2000_reproj <- project(population_2000, crs(grid_eu))
writeRaster(population_2000_reproj,'output/population_2000_reproj.tif')

population_2020_reproj <- project(population_2020, crs(grid_eu))
writeRaster(population_2020_reproj,'output/population_2020_reproj.tif')

population_2000_reproj <- rast(raster(x = "output/population_2000_reproj.tif"))
population_2020_reproj <- rast(raster(x = "output/population_2020_reproj.tif"))
population_2000_reproj <- crop(population_2000_reproj,ext(grid_eu))
population_2020_reproj <- crop(population_2020_reproj,ext(grid_eu))

grid_eu$pop2000 <- exact_extract(population_2000_reproj,grid_eu, fun="sum")
grid_eu$pop2020 <- exact_extract(population_2020_reproj,grid_eu, fun="sum")

grid_eu$diff_pop_perc <- (grid_eu$pop2020 - grid_eu$pop2000)/grid_eu$pop2000

st_write(grid_eu,"output/grid_eu_pop.gpkg")

### Imperviousness

grid_eu <- st_read("output/grid_eu_pop.gpkg")

impervious_2006_reproj <- project(impervious_2006, crs(grid_eu))
writeRaster(impervious_2006_reproj,'output/impervious_2006_reproj.tif')

impervious_2018_reproj <- project(impervious_2018, crs(grid_eu))
writeRaster(impervious_2018_reproj,'output/impervious_2018_reproj.tif')

impervious_2006_reproj <- rast(raster(x = "output/impervious_2006_reproj.tif"))
impervious_2018_reproj <- rast(raster(x = "output/impervious_2018_reproj.tif"))
impervious_2006_reproj <- crop(impervious_2006_reproj,ext(grid_eu))
impervious_2018_reproj <- crop(impervious_2018_reproj,ext(grid_eu))

temp1 <- exact_extract(impervious_2006_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(impervious_2018_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp1$mean[which(temp1$mean>100)] <- NA
temp2$mean <- temp2$sum/temp2$count

grid_eu$impervious2006 <- temp1$mean
grid_eu$impervious2018 <- temp2$mean

grid_eu$diff_impervious <- (grid_eu$impervious2018 - grid_eu$impervious2006)/grid_eu$impervious2006

st_write(grid_eu,"output/grid_eu_impervious.gpkg")

### Tree density

grid_eu <- st_read("output/grid_eu_impervious.gpkg")

treedensity_2012_reproj <- project(treedensity_2012, crs(grid_eu))
writeRaster(treedensity_2012_reproj,'output/treedensity_2012_reproj.tif')

treedensity_2018_reproj <- project(treedensity_2018, crs(grid_eu))
writeRaster(treedensity_2018_reproj,'output/treedensity_2018_reproj.tif')

treedensity_2012_reproj <- rast(raster(x = "output/treedensity_2012_reproj.tif"))
treedensity_2018_reproj <- rast(raster(x = "output/treedensity_2018_reproj.tif"))
treedensity_2012_reproj <- crop(treedensity_2012_reproj,ext(grid_eu))
treedensity_2018_reproj <- crop(treedensity_2018_reproj,ext(grid_eu))

temp1 <- exact_extract(treedensity_2012_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(treedensity_2018_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp1$mean[which(temp1$mean>100)] <- NA
temp2$mean <- temp2$sum/temp2$count

grid_eu$treedensity2012 <- temp1$mean
grid_eu$treedensity2018 <- temp2$mean

grid_eu$diff_treedensity <- (grid_eu$treedensity2018 - grid_eu$treedensity2012)/grid_eu$treedensity2012

st_write(grid_eu,"output/grid_eu_treedensity.gpkg")

### European land system intensity

grid_eu <- st_read("output/grid_eu_treedensity.gpkg")

eu_land_system_reproj <- project(eu_land_system, crs(grid_eu))
writeRaster(eu_land_system_reproj,'output/eu_land_system_reproj.tif')

eu_land_system_reproj <- rast(raster(x = "output/eu_land_system_reproj.tif"))
eu_land_system_reproj <- crop(eu_land_system_reproj,ext(grid_eu))

temp1 <- exact_extract(eu_land_system_reproj,grid_eu, fun="mode")

grid_eu$eulandsystem <- temp1

st_write(grid_eu,"output/grid_eu_eulandsystem.gpkg")


### Light pollution

grid_eu <- st_read("output/grid_eu_eulandsystem.gpkg")

lightpollution_2000_reproj <- crop(lightpollution_2000,ext(c(-25,45,26,76)))
lightpollution_2000_reproj <- project(lightpollution_2000_reproj, crs(grid_eu))
writeRaster(lightpollution_2000_reproj,'output/lightpollution_2000_reproj.tif')

lightpollution_2013_reproj <- crop(lightpollution_2013,ext(c(-25,45,26,76)))
lightpollution_2013_reproj <- project(lightpollution_2013_reproj, crs(grid_eu))
writeRaster(lightpollution_2013_reproj,'output/lightpollution_2013_reproj.tif')

lightpollution_2000_reproj <- rast(raster(x = "output/lightpollution_2000_reproj.tif"))
lightpollution_2013_reproj <- rast(raster(x = "output/lightpollution_2013_reproj.tif"))
lightpollution_2000_reproj <- crop(lightpollution_2000_reproj,ext(grid_eu))
lightpollution_2013_reproj <- crop(lightpollution_2013_reproj,ext(grid_eu))

temp1 <- exact_extract(lightpollution_2000_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(lightpollution_2013_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count

grid_eu$lightpollution2000 <- temp1$mean
grid_eu$lightpollution2013 <- temp2$mean

grid_eu$diff_lightpollution <- (grid_eu$lightpollution2013 - grid_eu$lightpollution2000)/grid_eu$lightpollution2000

st_write(grid_eu,"output/grid_eu_lightpollution.gpkg")

ggplot(grid_eu) +
  geom_sf(aes(fill = lightpollution2013), colour=NA) +
  coord_sf(
    xlim = c(2834303, 7323799),
    ylim = c(1570352, 5418000)
  )

ggplot(grid_eu [which(grid_eu$NUTS2021_1=="FR1"),]) +
  geom_sf(aes(fill = lightpollution2013), colour=NA)

## Protected areas 

grid_eu <- st_read("output/grid_eu_lightpollution.gpkg")

sf::sf_use_s2(FALSE)

protected_area_reproj <- st_crop(protected_area, xmin = -25, xmax = 45, ymin = 26, ymax = 76)

saveRDS(protected_area_reproj,"output/protected_area_reproj.rds")

protected_area_reproj <- st_transform(protected_area_reproj, crs(grid_eu))

raster_template <- rast(raster(x = "output/eu_land_system_reproj.tif"))

grid_eu_test <- grid_eu[which(grid_eu$NUTS2021_1=="FR1"),]
raster_template_test <- crop(raster_template,ext(grid_eu_test))
raster_template_test[] <- NA
test <-  st_intersection(grid_eu_test, protected_area_reproj)
test$IUCN_CAT <- as.numeric(as.factor(test$IUCN_CAT))
test$TYPE_AREA <- "0Other"
test$TYPE_AREA[which(test$DESIG_ENG %in% c("Special Areas of Conservation (Habitats Directive)","Special Protection Area (Birds Directive)",
                                                                             "Natura 2000 SCI","Sites of Community Importance (Habitats Directive)","Natura 2000 SPA","Natura 2000",
                                                                             "ZSC - Natura 2000","Special Areas of Conservation - International Importance",'Special Protection Areas',
                                                                             "ZPS - Natura 2000","Special Areas of Conservation - National Importance","National Nature Reserve, Wetland of International Importance, Site of Community Importance, Special Protection Area Site of Community Importance",
                                                                             "Area of Special Protection for Birds under Wildlife Act 1990"))] <- "2Natura2000"
test$TYPE_AREA <- as.numeric(as.factor(test$TYPE_AREA))
test_rast <- st_rasterize(test %>% dplyr::select(IUCN_CAT, geometry), template=st_as_stars(raster_template_test), field = "IUCN_CAT")
test_rast <- st_rasterize(test %>% dplyr::select(TYPE_AREA, geometry), template=st_as_stars(raster_template_test), field = "TYPE_AREA")
test_rast <- st_rasterize(test %>% dplyr::select(GIS_AREA, geometry), template=st_as_stars(raster_template_test), field = "GIS_AREA")

raster_template[] <- NA
# levels(as.factor(protected_area_reproj$IUCN_CAT))  "Ia","Ib","II","III","IV","Not Applicable","Not Assigned","Not Reported","V","VI"
protected_area_reproj$TYPE_AREA <- "0Other"
protected_area_reproj$TYPE_AREA[which(protected_area_reproj$DESIG_ENG %in% c("Special Areas of Conservation (Habitats Directive)","Special Protection Area (Birds Directive)",
                                                                             "Natura 2000 SCI","Sites of Community Importance (Habitats Directive)","Natura 2000 SPA","Natura 2000",
                                                                             "ZSC - Natura 2000","Special Areas of Conservation - International Importance",'Special Protection Areas',
                                                                             "ZPS - Natura 2000","Special Areas of Conservation - National Importance","National Nature Reserve, Wetland of International Importance, Site of Community Importance, Special Protection Area Site of Community Importance",
                                                                             "Area of Special Protection for Birds under Wildlife Act 1990"))] <- "2Natura2000"
# levels(as.factor(protected_area_reproj$TYPE_AREA))
protected_area_reproj$IUCN_CAT <- as.numeric(as.factor(protected_area_reproj$IUCN_CAT))
protected_area_reproj$TYPE_AREA <- as.numeric(as.factor(protected_area_reproj$TYPE_AREA))
protected_area_rast <- st_rasterize(protected_area_reproj %>% dplyr::select(IUCN_CAT, geometry), template=st_as_stars(raster_template), field = "IUCN_CAT")
protected_area_rast2 <- st_rasterize(protected_area_reproj %>% dplyr::select(TYPE_AREA, geometry), template=st_as_stars(raster_template), field = "TYPE_AREA")
protected_area_rast3 <- st_rasterize(protected_area_reproj %>% dplyr::select(GIS_AREA, geometry), template=st_as_stars(raster_template), field = "GIS_AREA")

write_stars(protected_area_rast,layer="IUCN_CAT","output/protected_area_rast.tif")
write_stars(protected_area_rast2, layer="TYPE_AREA","output/protected_area_rast2.tif")
write_stars(protected_area_rast3,layer="GIS_AREA","output/protected_area_rast3.tif")

protected_area_rast_reproj <- rast(raster(x = "output/protected_area_rast.tif"))
protected_area_rast_reproj2 <- rast(raster(x = "output/protected_area_rast2.tif"))
protected_area_rast_reproj3 <- rast(raster(x = "output/protected_area_rast3.tif"))
temp1 <- exact_extract(protected_area_rast_reproj,grid_eu, fun=c("mode"))
temp2 <- exact_extract(protected_area_rast_reproj2,grid_eu, fun=c("mode"))
temp3 <- exact_extract(protected_area_rast_reproj3,grid_eu, fun=c("mode"))

grid_eu$protectedarea <- temp1
grid_eu$protectedarea_type <- temp2
grid_eu$protectedarea_size <- temp3

st_write(grid_eu,"output/grid_eu_protectedarea.gpkg")

### pesticides

grid_eu <- st_read("output/grid_eu_protectedarea.gpkg")

raster_template <- rast(raster(x = "output/eu_land_system_reproj.tif"))
raster_template[] <- NA
pesticide_rast <- st_rasterize(pesticide_sf %>% dplyr::select(total_kg, total_kg_ha, total_nodu_ha, geometry), template=st_as_stars(raster_template), field = c("total_kg", "total_kg_ha", "total_nodu_ha"))

write_stars(pesticide_rast,"output/pesticide_rast_kg.tif", layer = 1)
write_stars(pesticide_rast,"output/pesticide_rast_kg_ha.tif", layer = 2)
write_stars(pesticide_rast,"output/pesticide_rast_nodu_ha.tif", layer = 3)

pesticide_rast_kg <- rast(raster(x = "output/pesticide_rast_kg.tif"))
pesticide_rast_kg_ha <- rast(raster(x = "output/pesticide_rast_kg_ha.tif"))
pesticide_rast_nodu_ha <- rast(raster(x = "output/pesticide_rast_nodu_ha.tif"))

ggplot() + tidyterra::geom_spatraster(data=pesticide_rast_kg,aes(fill=pesticide_rast_kg))
ggplot() + tidyterra::geom_spatraster(data=pesticide_rast_kg_ha,aes(fill=pesticide_rast_kg_ha))
ggplot() + tidyterra::geom_spatraster(data=pesticide_rast_nodu_ha,aes(fill=pesticide_rast_nodu_ha))

temp1 <- exact_extract(pesticide_rast_kg,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(pesticide_rast_kg_ha,grid_eu, fun=c("sum","count"))
temp3 <- exact_extract(pesticide_rast_nodu_ha,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count
temp3$mean <- temp3$sum/temp3$count

grid_eu$pesticide_kg <- temp1$mean
grid_eu$pesticide_kg_ha <- temp2$mean
grid_eu$pesticide_nodu_kg <- temp3$mean

st_write(grid_eu,"output/grid_eu_pesticide.gpkg")

### woodproduction

grid_eu <- st_read("output/grid_eu_pesticide.gpkg")

woodprod_2000_reproj <- project(woodprod_2000, crs(grid_eu))
writeRaster(woodprod_2000_reproj,'output/woodprod_2000_reproj.tif')

woodprod_2010_reproj <- project(woodprod_2010, crs(grid_eu))
writeRaster(woodprod_2010_reproj,'output/woodprod_2010_reproj.tif')

woodprod_average_reproj <- project(woodprod_average, crs(grid_eu))
writeRaster(woodprod_average_reproj,'output/woodprod_average_reproj.tif')

woodprod_2000_reproj <- rast(raster(x = "output/woodprod_2000_reproj.tif"))
woodprod_2010_reproj <- rast(raster(x = "output/woodprod_2010_reproj.tif"))
woodprod_average_reproj <- rast(raster(x = "output/woodprod_average_reproj.tif"))
woodprod_2000_reproj <- crop(woodprod_2000_reproj,ext(grid_eu))
woodprod_2010_reproj <- crop(woodprod_2010_reproj,ext(grid_eu))
woodprod_average_reproj <- crop(woodprod_average_reproj,ext(grid_eu))

temp1 <- exact_extract(woodprod_2000_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(woodprod_2010_reproj,grid_eu, fun=c("sum","count"))
temp3 <- exact_extract(woodprod_average_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count
temp3$mean <- temp3$sum/temp3$count

grid_eu$woodprod2000 <- temp1$mean
grid_eu$woodprod2010 <- temp2$mean
grid_eu$woodprodaverage <- temp3$mean

grid_eu$diff_woodprod <- (grid_eu$woodprod2010 - grid_eu$woodprod2000)/grid_eu$woodprod2000

st_write(grid_eu,"output/grid_eu_woodprod.gpkg")

### productivity

grid_eu <- st_read("output/grid_eu_woodprod.gpkg")

drymatter_2000_reproj <- project(drymatter_2000, crs(grid_eu))
writeRaster(drymatter_2000_reproj,'output/drymatter_2000_reproj.tif')

drymatter_2018_reproj <- project(drymatter_2018, crs(grid_eu))
writeRaster(drymatter_2018_reproj,'output/drymatter_2018_reproj.tif')

drymatter_2000_reproj <- rast(raster(x = "output/drymatter_2000_reproj.tif"))
drymatter_2018_reproj <- rast(raster(x = "output/drymatter_2018_reproj.tif"))
drymatter_2000_reproj <- crop(drymatter_2000_reproj,ext(grid_eu))
drymatter_2018_reproj <- crop(drymatter_2018_reproj,ext(grid_eu))

temp1 <- exact_extract(drymatter_2000_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(drymatter_2018_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count

grid_eu$drymatter2000 <- temp1$mean
grid_eu$drymatter2018 <- temp2$mean

grid_eu$diff_drymatter <- (grid_eu$drymatter2018 - grid_eu$drymatter2000)/grid_eu$drymatter2000

st_write(grid_eu,"output/grid_eu_drymatter.gpkg")

### decline in productivity

grid_eu <- st_read("output/grid_eu_drymatter.gpkg")

decline_productivity_reproj <- decline_productivity[which(decline_productivity$name_iso31 %in% c("Albania","Andorra","Austria","Belgium",
                                                                                                 "Bulgaria","Bosnia and Herzegovina","Belarus","Switzerland",
                                                                                                 "Croatia","Hungary","Cyprus","Czechia",
                                                                                                 "Germany","Denmark","Spain","Estonia",
                                                                                                 "Finland","France","Faroe Islands","United Kingdom of Great Britain and Northern Ireland",
                                                                                                 "Georgia","Guernsey","Greece","Isle of Man",
                                                                                                 "Ireland","Iceland","Italy","Jersey",
                                                                                                 "Kuwait","Lithuania","Luxembourg","Latvia",
                                                                                                 "Moldova (Republic of)","Macedonia (the former Yugoslav Republic of)","Malta","Montenegro",
                                                                                                 "Poland","Netherlands","Norway","Portugal",
                                                                                                 "Romania","Serbia","San Marino","Slovakia",
                                                                                                 "Slovenia","Sweden","Turkey","Ukraine")),]
decline_productivity_reproj <- st_transform(decline_productivity_reproj, crs(grid_eu))

raster_template <- rast(raster(x = "output/eu_land_system_reproj.tif"))
raster_template[] <- NA
decline_productivity_rast <- st_rasterize(decline_productivity_reproj %>% dplyr::select(lpd_p, geometry), template=st_as_stars(raster_template), field = c("lpd_p"))
write_stars(decline_productivity_rast,"output/decline_productivity_rast.tif", layer = 1)

decline_productivity_rast <- rast(raster(x = "output/decline_productivity_rast.tif"))
temp1 <- exact_extract(decline_productivity_rast,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count

grid_eu$declineproductivity <- temp1$mean

st_write(grid_eu,"output/grid_eu_declineproductivity.gpkg")


### small woody feature

grid_eu <- st_read("output/grid_eu_declineproductivity.gpkg")

smallwoodyfeatures_reproj <- project(smallwoodyfeatures, crs(grid_eu))
writeRaster(smallwoodyfeatures_reproj,'output/smallwoodyfeatures_reproj.tif')

smallwoodyfeatures_reproj <- rast(raster(x = "output/smallwoodyfeatures_reproj.tif"))
smallwoodyfeatures_reproj <- crop(smallwoodyfeatures_reproj,ext(grid_eu))

temp1 <- exact_extract(smallwoodyfeatures_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count

grid_eu$smallwoodyfeatures <- temp1$mean

st_write(grid_eu,"output/grid_eu_smallwoodyfeatures.gpkg")


## fragmentation

grid_eu <- st_read("output/grid_eu_smallwoodyfeatures.gpkg")

fragmentation_reproj <- project(fragmentation, crs(grid_eu))
writeRaster(fragmentation_reproj,'output/fragmentation_reproj.tif')

fragmentation_reproj <- rast(raster(x = "output/fragmentation_reproj.tif"))
fragmentation_reproj <- crop(fragmentation_reproj,ext(grid_eu))

temp1 <- exact_extract(fragmentation_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count

grid_eu$fragmentation <- temp1$mean

st_write(grid_eu,"output/grid_eu_fragmentation.gpkg")


## forestintegrity

grid_eu <- st_read("output/grid_eu_fragmentation.gpkg")

forestintegrity[forestintegrity == -9999] <- NA

forestintegrity_reproj <- crop(forestintegrity, ext(-25, 45, 26, 76))

forestintegrity_reproj <- project(forestintegrity_reproj, crs(grid_eu))
writeRaster(forestintegrity_reproj,'output/forestintegrity_reproj.tif')

forestintegrity_reproj <- rast(raster(x = "output/forestintegrity_reproj.tif"))
forestintegrity_reproj <- crop(forestintegrity_reproj,ext(grid_eu))

forestintegrity_reproj <- forestintegrity_reproj/1000

temp1 <- exact_extract(forestintegrity_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp1$mean_cat <- NA
temp1$mean_cat[which(temp1$mean<=6)] <- "low"
temp1$mean_cat[which(temp1$mean>6 & temp1$mean<9.6)] <- "medium"
temp1$mean_cat[which(temp1$mean>=9.6)] <- "high"

grid_eu$forestintegrity <- temp1$mean
grid_eu$forestintegrity_cat <- temp1$mean_cat

st_write(grid_eu,"output/grid_eu_forestintegrity.gpkg")


## Mean temperature

grid_eu <- st_read("output/grid_eu_forestintegrity.gpkg")

temp_2000_average_reproj <- project(temp_2000_average, crs(grid_eu))
writeRaster(temp_2000_average_reproj,'output/temp_2000_average_reproj.tif')
temp_2020_average_reproj <- project(temp_2020_average, crs(grid_eu))
writeRaster(temp_2020_average_reproj,'output/temp_2020_average_reproj.tif')
temp_spring_2000_average_reproj <- project(temp_spring_2000_average, crs(grid_eu))
writeRaster(temp_spring_2000_average_reproj,'output/temp_spring_2000_average_reproj.tif')
temp_spring_2020_average_reproj <- project(temp_spring_2020_average, crs(grid_eu))
writeRaster(temp_spring_2020_average_reproj,'output/temp_spring_2020_average_reproj.tif')
temp_spring_var_2000_average_reproj <- project(temp_spring_var_2000_average, crs(grid_eu))
writeRaster(temp_spring_var_2000_average_reproj,'output/temp_spring_var_2000_average_reproj.tif')
temp_spring_var_2020_average_reproj <- project(temp_spring_var_2020_average, crs(grid_eu))
writeRaster(temp_spring_var_2020_average_reproj,'output/temp_spring_var_2020_average_reproj.tif')

temp_2000_average_reproj <- rast(raster(x = "output/temp_2000_average_reproj.tif"))
temp_2020_average_reproj <- rast(raster(x = "output/temp_2020_average_reproj.tif"))
temp_spring_2000_average_reproj <- rast(raster(x = "output/temp_spring_2000_average_reproj.tif"))
temp_spring_2020_average_reproj <- rast(raster(x = "output/temp_spring_2020_average_reproj.tif"))
temp_spring_var_2000_average_reproj <- rast(raster(x = "output/temp_spring_var_2000_average_reproj.tif"))
temp_spring_var_2020_average_reproj <- rast(raster(x = "output/temp_spring_var_2020_average_reproj.tif"))
temp_2000_average_reproj <- crop(temp_2000_average_reproj,ext(grid_eu))
temp_2020_average_reproj <- crop(temp_2020_average_reproj,ext(grid_eu))
temp_spring_2000_average_reproj <- crop(temp_spring_2000_average_reproj,ext(grid_eu))
temp_spring_2020_average_reproj <- crop(temp_spring_2020_average_reproj,ext(grid_eu))
temp_spring_var_2000_average_reproj <- crop(temp_spring_var_2000_average_reproj,ext(grid_eu))
temp_spring_var_2020_average_reproj <- crop(temp_spring_var_2020_average_reproj,ext(grid_eu))

temp1 <- exact_extract(temp_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(temp_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp3 <- exact_extract(temp_spring_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp4 <- exact_extract(temp_spring_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp5 <- exact_extract(temp_spring_var_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp6 <- exact_extract(temp_spring_var_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count
temp3$mean <- temp3$sum/temp3$count
temp4$mean <- temp4$sum/temp4$count
temp5$mean <- temp5$sum/temp5$count
temp6$mean <- temp6$sum/temp6$count

grid_eu$temp2000 <- temp1$mean
grid_eu$temp2020 <- temp2$mean
grid_eu$tempspring2000 <- temp3$mean
grid_eu$tempspring2020 <- temp4$mean
grid_eu$tempspringvar2000 <- temp5$mean
grid_eu$tempspringvar2020 <- temp6$mean

st_write(grid_eu,"output/grid_eu_temp.gpkg")


## Minimum and maximum temperature

grid_eu <- st_read("output/grid_eu_temp.gpkg")

temp_min_spring_2000_average_reproj <- project(temp_min_spring_2000_average, crs(grid_eu))
writeRaster(temp_min_spring_2000_average_reproj,'output/temp_min_spring_2000_average_reproj.tif')
temp_min_spring_2020_average_reproj <- project(temp_min_spring_2020_average, crs(grid_eu))
writeRaster(temp_min_spring_2020_average_reproj,'output/temp_min_spring_2020_average_reproj.tif')
temp_min_spring_var_2000_average_reproj <- project(temp_min_spring_var_2000_average, crs(grid_eu))
writeRaster(temp_min_spring_var_2000_average_reproj,'output/temp_min_spring_var_2000_average_reproj.tif')
temp_min_spring_var_2020_average_reproj <- project(temp_min_spring_var_2020_average, crs(grid_eu))
writeRaster(temp_min_spring_var_2020_average_reproj,'output/temp_min_spring_var_2020_average_reproj.tif')

temp_min_spring_2000_average_reproj <- rast(raster(x = "output/temp_min_spring_2000_average_reproj.tif"))
temp_min_spring_2020_average_reproj <- rast(raster(x = "output/temp_min_spring_2020_average_reproj.tif"))
temp_min_spring_var_2000_average_reproj <- rast(raster(x = "output/temp_min_spring_var_2000_average_reproj.tif"))
temp_min_spring_var_2020_average_reproj <- rast(raster(x = "output/temp_min_spring_var_2020_average_reproj.tif"))
temp_min_spring_2000_average_reproj <- crop(temp_min_spring_2000_average_reproj,ext(grid_eu))
temp_min_spring_2020_average_reproj <- crop(temp_min_spring_2020_average_reproj,ext(grid_eu))
temp_min_spring_var_2000_average_reproj <- crop(temp_min_spring_var_2000_average_reproj,ext(grid_eu))
temp_min_spring_var_2020_average_reproj <- crop(temp_min_spring_var_2020_average_reproj,ext(grid_eu))

temp1 <- exact_extract(temp_min_spring_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(temp_min_spring_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp3 <- exact_extract(temp_min_spring_var_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp4 <- exact_extract(temp_min_spring_var_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count
temp3$mean <- temp3$sum/temp3$count
temp4$mean <- temp4$sum/temp4$count

grid_eu$tempspringmin2000 <- temp1$mean
grid_eu$tempspringmin2020 <- temp2$mean
grid_eu$tempspringminvar2000 <- temp3$mean
grid_eu$tempspringminvar2020 <- temp4$mean


temp_max_spring_2000_average_reproj <- project(temp_max_spring_2000_average, crs(grid_eu))
writeRaster(temp_max_spring_2000_average_reproj,'output/temp_max_spring_2000_average_reproj.tif')
temp_max_spring_2020_average_reproj <- project(temp_max_spring_2020_average, crs(grid_eu))
writeRaster(temp_max_spring_2020_average_reproj,'output/temp_max_spring_2020_average_reproj.tif')
temp_max_spring_var_2000_average_reproj <- project(temp_max_spring_var_2000_average, crs(grid_eu))
writeRaster(temp_max_spring_var_2000_average_reproj,'output/temp_max_spring_var_2000_average_reproj.tif')
temp_max_spring_var_2020_average_reproj <- project(temp_max_spring_var_2020_average, crs(grid_eu))
writeRaster(temp_max_spring_var_2020_average_reproj,'output/temp_max_spring_var_2020_average_reproj.tif')

temp_max_spring_2000_average_reproj <- rast(raster(x = "output/temp_max_spring_2000_average_reproj.tif"))
temp_max_spring_2020_average_reproj <- rast(raster(x = "output/temp_max_spring_2020_average_reproj.tif"))
temp_max_spring_var_2000_average_reproj <- rast(raster(x = "output/temp_max_spring_var_2000_average_reproj.tif"))
temp_max_spring_var_2020_average_reproj <- rast(raster(x = "output/temp_max_spring_var_2020_average_reproj.tif"))
temp_max_spring_2000_average_reproj <- crop(temp_max_spring_2000_average_reproj,ext(grid_eu))
temp_max_spring_2020_average_reproj <- crop(temp_max_spring_2020_average_reproj,ext(grid_eu))
temp_max_spring_var_2000_average_reproj <- crop(temp_max_spring_var_2000_average_reproj,ext(grid_eu))
temp_max_spring_var_2020_average_reproj <- crop(temp_max_spring_var_2020_average_reproj,ext(grid_eu))

temp1 <- exact_extract(temp_max_spring_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(temp_max_spring_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp3 <- exact_extract(temp_max_spring_var_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp4 <- exact_extract(temp_max_spring_var_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count
temp3$mean <- temp3$sum/temp3$count
temp4$mean <- temp4$sum/temp4$count

grid_eu$tempspringmax2000 <- temp1$mean
grid_eu$tempspringmax2020 <- temp2$mean
grid_eu$tempspringmaxvar2000 <- temp3$mean
grid_eu$tempspringmaxvar2020 <- temp4$mean

st_write(grid_eu,"output/grid_eu_temp_min_max.gpkg")

## Mean temperature

grid_eu <- st_read("output/grid_eu_temp_min_max.gpkg")

prec_2000_average <- rast(raster(x = "raw_data/climate/prec_2000_average.tif"))
prec_2020_average <- rast(raster(x = "raw_data/climate/prec_2020_average.tif"))

prec_spring_2000_average <- rast(raster(x = "raw_data/climate/prec_spring_2000_average.tif"))
prec_spring_2020_average <- rast(raster(x = "raw_data/climate/prec_spring_2020_average.tif"))

prec_spring_var_2000_average <- rast(raster(x = "raw_data/climate/prec_spring_var_2000_average.tif"))
prec_spring_var_2020_average <- rast(raster(x = "raw_data/climate/prec_spring_var_2020_average.tif"))

humidity_2000_average <- rast(raster(x = "raw_data/climate/humidity_2000_average.tif"))
humidity_2020_average <- rast(raster(x = "raw_data/climate/humidity_2020_average.tif"))

humidity_spring_2000_average <- rast(raster(x = "raw_data/climate/humidity_spring_2000_average.tif"))
humidity_spring_2020_average <- rast(raster(x = "raw_data/climate/humidity_spring_2020_average.tif"))

humidity_spring_var_2000_average <- rast(raster(x = "raw_data/climate/humidity_spring_var_2000_average.tif"))
humidity_spring_var_2020_average <- rast(raster(x = "raw_data/climate/humidity_spring_var_2020_average.tif"))

prec_2000_average_reproj <- project(prec_2000_average, crs(grid_eu))
writeRaster(prec_2000_average_reproj,'output/prec_2000_average_reproj.tif')
prec_2020_average_reproj <- project(prec_2020_average, crs(grid_eu))
writeRaster(prec_2020_average_reproj,'output/prec_2020_average_reproj.tif')
prec_spring_2000_average_reproj <- project(prec_spring_2000_average, crs(grid_eu))
writeRaster(prec_spring_2000_average_reproj,'output/prec_spring_2000_average_reproj.tif')
prec_spring_2020_average_reproj <- project(prec_spring_2020_average, crs(grid_eu))
writeRaster(prec_spring_2020_average_reproj,'output/prec_spring_2020_average_reproj.tif')
prec_spring_var_2000_average_reproj <- project(prec_spring_var_2000_average, crs(grid_eu))
writeRaster(prec_spring_var_2000_average_reproj,'output/prec_spring_var_2000_average_reproj.tif')
prec_spring_var_2020_average_reproj <- project(prec_spring_var_2020_average, crs(grid_eu))
writeRaster(prec_spring_var_2020_average_reproj,'output/prec_spring_var_2020_average_reproj.tif')

prec_2000_average_reproj <- rast(raster(x = "output/prec_2000_average_reproj.tif"))
prec_2020_average_reproj <- rast(raster(x = "output/prec_2020_average_reproj.tif"))
prec_spring_2000_average_reproj <- rast(raster(x = "output/prec_spring_2000_average_reproj.tif"))
prec_spring_2020_average_reproj <- rast(raster(x = "output/prec_spring_2020_average_reproj.tif"))
prec_spring_var_2000_average_reproj <- rast(raster(x = "output/prec_spring_var_2000_average_reproj.tif"))
prec_spring_var_2020_average_reproj <- rast(raster(x = "output/prec_spring_var_2020_average_reproj.tif"))
prec_2000_average_reproj <- crop(prec_2000_average_reproj,ext(grid_eu))
prec_2020_average_reproj <- crop(prec_2020_average_reproj,ext(grid_eu))
prec_spring_2000_average_reproj <- crop(prec_spring_2000_average_reproj,ext(grid_eu))
prec_spring_2020_average_reproj <- crop(prec_spring_2020_average_reproj,ext(grid_eu))
prec_spring_var_2000_average_reproj <- crop(prec_spring_var_2000_average_reproj,ext(grid_eu))
prec_spring_var_2020_average_reproj <- crop(prec_spring_var_2020_average_reproj,ext(grid_eu))

temp1 <- exact_extract(prec_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(prec_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp3 <- exact_extract(prec_spring_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp4 <- exact_extract(prec_spring_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp5 <- exact_extract(prec_spring_var_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp6 <- exact_extract(prec_spring_var_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count
temp3$mean <- temp3$sum/temp3$count
temp4$mean <- temp4$sum/temp4$count
temp5$mean <- temp5$sum/temp5$count
temp6$mean <- temp6$sum/temp6$count

grid_eu$prec2000 <- temp1$mean
grid_eu$prec2020 <- temp2$mean
grid_eu$precspring2000 <- temp3$mean
grid_eu$precspring2020 <- temp4$mean
grid_eu$precspringvar2000 <- temp5$mean
grid_eu$precspringvar2020 <- temp6$mean



humidity_2000_average_reproj <- project(humidity_2000_average, crs(grid_eu))
writeRaster(humidity_2000_average_reproj,'output/humidity_2000_average_reproj.tif')
humidity_2020_average_reproj <- project(humidity_2020_average, crs(grid_eu))
writeRaster(humidity_2020_average_reproj,'output/humidity_2020_average_reproj.tif')
humidity_spring_2000_average_reproj <- project(humidity_spring_2000_average, crs(grid_eu))
writeRaster(humidity_spring_2000_average_reproj,'output/humidity_spring_2000_average_reproj.tif')
humidity_spring_2020_average_reproj <- project(humidity_spring_2020_average, crs(grid_eu))
writeRaster(humidity_spring_2020_average_reproj,'output/humidity_spring_2020_average_reproj.tif')
humidity_spring_var_2000_average_reproj <- project(humidity_spring_var_2000_average, crs(grid_eu))
writeRaster(humidity_spring_var_2000_average_reproj,'output/humidity_spring_var_2000_average_reproj.tif')
humidity_spring_var_2020_average_reproj <- project(humidity_spring_var_2020_average, crs(grid_eu))
writeRaster(humidity_spring_var_2020_average_reproj,'output/humidity_spring_var_2020_average_reproj.tif')

humidity_2000_average_reproj <- rast(raster(x = "output/humidity_2000_average_reproj.tif"))
humidity_2020_average_reproj <- rast(raster(x = "output/humidity_2020_average_reproj.tif"))
humidity_spring_2000_average_reproj <- rast(raster(x = "output/humidity_spring_2000_average_reproj.tif"))
humidity_spring_2020_average_reproj <- rast(raster(x = "output/humidity_spring_2020_average_reproj.tif"))
humidity_spring_var_2000_average_reproj <- rast(raster(x = "output/humidity_spring_var_2000_average_reproj.tif"))
humidity_spring_var_2020_average_reproj <- rast(raster(x = "output/humidity_spring_var_2020_average_reproj.tif"))
humidity_2000_average_reproj <- crop(humidity_2000_average_reproj,ext(grid_eu))
humidity_2020_average_reproj <- crop(humidity_2020_average_reproj,ext(grid_eu))
humidity_spring_2000_average_reproj <- crop(humidity_spring_2000_average_reproj,ext(grid_eu))
humidity_spring_2020_average_reproj <- crop(humidity_spring_2020_average_reproj,ext(grid_eu))
humidity_spring_var_2000_average_reproj <- crop(humidity_spring_var_2000_average_reproj,ext(grid_eu))
humidity_spring_var_2020_average_reproj <- crop(humidity_spring_var_2020_average_reproj,ext(grid_eu))

temp1 <- exact_extract(humidity_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(humidity_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp3 <- exact_extract(humidity_spring_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp4 <- exact_extract(humidity_spring_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp5 <- exact_extract(humidity_spring_var_2000_average_reproj,grid_eu, fun=c("sum","count"))
temp6 <- exact_extract(humidity_spring_var_2020_average_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count
temp3$mean <- temp3$sum/temp3$count
temp4$mean <- temp4$sum/temp4$count
temp5$mean <- temp5$sum/temp5$count
temp6$mean <- temp6$sum/temp6$count

grid_eu$humidity2000 <- temp1$mean
grid_eu$humidity2020 <- temp2$mean
grid_eu$humidityspring2000 <- temp3$mean
grid_eu$humidityspring2020 <- temp4$mean
grid_eu$humidityspringvar2000 <- temp5$mean
grid_eu$humidityspringvar2020 <- temp6$mean

st_write(grid_eu,"output/grid_eu_prec_hum.gpkg")

## Landscape diversity for 1 km (grid_eu) and 3 km

grid_eu <- st_read("output/grid_eu_prec_hum.gpkg")

clc_land_cover2 <- classify(rast(clc_land_cover),cbind(c(1:44,48),
                                                       c(rep(11,2),rep(12,4),rep(13,3),rep(14,2),
                                                         rep(21,3),rep(22,3),rep(23,1),rep(24,4),
                                                         rep(31,3),rep(32,4),rep(33,5),rep(41,2),
                                                         rep(42,3),rep(51,2),rep(52,3),NA)))
writeRaster(clc_land_cover2,'raw_data/land_cover/u2018_clc2018_v2020_20u1_raster100m/DATA/clc_land_cover2.tif')
clc_land_cover2 <- rast(raster(x = "raw_data/land_cover/u2018_clc2018_v2020_20u1_raster100m/DATA/clc_land_cover2.tif"))


clc_land_cover_20002 <- classify(rast(clc_land_cover_2000),cbind(c(1:44,48),
                                                       c(rep(11,2),rep(12,4),rep(13,3),rep(14,2),
                                                         rep(21,3),rep(22,3),rep(23,1),rep(24,4),
                                                         rep(31,3),rep(32,4),rep(33,5),rep(41,2),
                                                         rep(42,3),rep(51,2),rep(52,3),NA)))
writeRaster(clc_land_cover_20002,'raw_data/land_cover/u2006_clc2000_v2020_20u1_raster100m/DATA/clc_land_cover_20002.tif')
clc_land_cover_20002 <- rast(raster(x = "raw_data/land_cover/u2006_clc2000_v2020_20u1_raster100m/DATA/clc_land_cover_20002.tif"))

### test

grid_eu_test <- grid_eu[which(grid_eu$NUTS2021_1=="FRD"),]

sp_grid_eu <- st_as_sf(grid_eu_test$geom)
sp_grid_eu_buffer <- st_buffer(sp_grid_eu,3000) # from https://link.springer.com/chapter/10.1007/978-3-319-43314-1_8

temp1 <- exact_extract(clc_land_cover2,sp_grid_eu_buffer,fun="frac")
temp1$shannon <- apply(temp1,1, # from https://doi.org/10.1038/s41467-023-37027-5
                       function(x){
                         x_nnull <- x[which(x>0)]
                         shannon_i <- -sum(x_nnull*log(x_nnull))
                         return(shannon_i)
                         })
temp2 <- exact_extract(clc_land_cover2,grid_eu_test,fun="frac")
temp2$shannon <- apply(temp2,1,
                       function(x){
                         x_nnull <- x[which(x>0)]
                         shannon_i <- -sum(x_nnull*log(x_nnull))
                         return(shannon_i)
                       })

grid_eu_test$buffer3 <- temp1$shannon
grid_eu_test$buffer1 <- temp2$shannon

ggplot(grid_eu_test) +
  geom_sf(aes(fill = buffer1), colour=NA)

### apply

temp2 <- exact_extract(clc_land_cover2,grid_eu,fun="frac")
temp2$shannon <- apply(temp2,1,
                       function(x){
                         x_nnull <- x[which(x>0)]
                         shannon_i <- -sum(x_nnull*log(x_nnull))
                         return(shannon_i)
                       })
temp2$agri <- temp2$frac_21 + temp2$frac_22 + temp2$frac_23 + temp2$frac_24 + temp2$frac_32


temp3 <- exact_extract(clc_land_cover_20002,grid_eu,fun="frac")
temp3$shannon <- apply(temp3,1,
                       function(x){
                         x_nnull <- x[which(x>0)]
                         shannon_i <- -sum(x_nnull*log(x_nnull))
                         return(shannon_i)
                       })
temp3$agri <- temp3$frac_21 + temp3$frac_22 + temp3$frac_23 + temp3$frac_24 + temp3$frac_32

grid_eu$shannon_2018 <- temp2$shannon
grid_eu$shannon_2000 <- temp3$shannon

grid_eu$DIST_BORD <- grid_eu$TOT_P_2011 <- grid_eu$TOT_P_2018 <-
  grid_eu$Y_LLC <- grid_eu$X_LLC <- NULL

st_write(grid_eu,"output/grid_eu_shannon.gpkg")

## GDP

grid_eu <- st_read("output/grid_eu_shannon.gpkg")

GDP_2000_reproj <- project(GDP_2000, crs(grid_eu))
writeRaster(GDP_2000_reproj,'output/GDP_2000_reproj.tif')
GDP_2015_reproj <- project(GDP_2015, crs(grid_eu))
writeRaster(GDP_2015_reproj,'output/GDP_2015_reproj.tif')

GDP_2000_reproj <- rast(raster(x = "output/GDP_2000_reproj.tif"))
GDP_2015_reproj <- rast(raster(x = "output/GDP_2015_reproj.tif"))
GDP_2000_reproj <- crop(GDP_2000_reproj,ext(grid_eu))
GDP_2015_reproj <- crop(GDP_2015_reproj,ext(grid_eu))


temp1 <- exact_extract(GDP_2000_reproj,grid_eu, fun=c("sum","count"))
temp2 <- exact_extract(GDP_2015_reproj,grid_eu, fun=c("sum","count"))
temp1$mean <- temp1$sum/temp1$count
temp2$mean <- temp2$sum/temp2$count

grid_eu$GDP2000 <- temp1$mean
grid_eu$GDP2015 <- temp2$mean

st_write(grid_eu,"output/grid_eu_gdp.gpkg")
