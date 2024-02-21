# Make bioregion from EVA

## Load EVA data

header_eva <- read.csv("raw_data/EVA/197_Bioregions20240212_notJUICE/197_Bioregions20240212_notJUICE_header.csv", sep="\t", header=TRUE)

### Data too large for R, subsection for France to test

header_fr <- header_eva[which(header_eva$Country=="France"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_fr$PlotObservationID))
species_eva1 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240212_notJUICE/197_Bioregions20240212_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

### Prepare data for bioregion

vegedf_fr <- species_eva1[which(species_eva1$Match==1),c("PlotObservationID","Turboveg2 concept","Cover %")]
set.seed(123)
vegedf_fr <- vegedf_fr[which(vegedf_fr$PlotObservationID %in% sample(unique(vegedf_fr$PlotObservationID),2000)),]
vegedf_fr$species <- as.numeric(as.factor(vegedf_fr$`Turboveg2 concept`))
names(vegedf_fr)[c(1,3)] <- c("site","cover")
vegedf_fr$`Turboveg2 concept` <- NULL
vegedf_fr <- vegedf_fr[,c("site","species","cover")]
vegedf_fr <- vegedf_fr[!duplicated(vegedf_fr[,c(1,2)]),]

vegemat_fr <- net_to_mat(as.data.frame(vegedf_fr), weight = TRUE, squared = FALSE, symmetrical = FALSE, missing_value = 0)

dissim <- na.omit(dissimilarity(vegemat_fr))

tree4 <- hclu_hierarclust(dissim,
                          n_clust = 2:100)

eval_tree4 <- partition_metrics(tree4, 
                                dissimilarity = dissim, # Provide distances to compute the metrics
                                eval_metric = "pc_distance")

opti_n_tree4 <- find_optimal_n(eval_tree4)

K_name <- opti_n_tree4$evaluation_df$K[opti_n_tree4$evaluation_df$optimal_n_pc_distance]

### Make a map of the clusters
centroid_coord <- data.frame(header_fr[which(header_fr$PlotObservationID %in% unique(vegedf_fr$site)),c("Longitude","Latitude")])
centroid_coord$Longitude <- as.numeric(centroid_coord$Longitude)
vegesf_fr <- st_geometry(st_as_sf(centroid_coord,coords = c("Longitude","Latitude")))
st_crs(vegesf_fr) <- 4326
vegesf_fr <- st_transform(vegesf_fr,crs = 3035)
vegesf_fr <- as.data.frame(st_coordinates(vegesf_fr))
vegesf_fr <- pts2poly_centroids(vegesf_fr,
                                3000, crs = 3035)
vegesf_fr <- st_sf(vegesf_fr)
vegesf_fr$ID <- header_fr$PlotObservationID[which(header_fr$PlotObservationID %in% unique(vegedf_fr$site))]
vegesf_fr$geometry <- vegesf_fr$vegesf_fr
vegesf_fr$vegesf_fr <- NULL
vegesf_fr <- st_sf(vegesf_fr)


map_clusters(tree4$clusters[, c("ID", K_name)],
             vegesf_fr)

### Use EU grid as site 

grid_eu <- st_read("raw_data/grid_eu/grid_50km_surf.gpkg")

grid_eu_fr <- grid_eu[which(grid_eu$NUTS2016_1 %in% unique(grid_eu$NUTS2016_1)[grep("FR",unique(grid_eu$NUTS2016_1))][-1]),]

centroid_coord <- data.frame(header_fr[,c("Longitude","Latitude")])
centroid_coord$Longitude <- as.numeric(centroid_coord$Longitude)
vegesf_fr <- st_geometry(st_as_sf(centroid_coord,coords = c("Longitude","Latitude")))
st_crs(vegesf_fr) <- 4326
vegesf_fr <- st_transform(vegesf_fr,crs = 3035)
vegesf_fr <- as.data.frame(st_coordinates(vegesf_fr))
vegesf_fr <- pts2poly_centroids(vegesf_fr,
                                3000, crs = 3035)

### Find site in each EU cell

vegesf_fr_grid <- st_intersects(grid_eu_fr,vegesf_fr)

grid_eu_fr$first_site_id <- unlist(lapply(vegesf_fr_grid, `[`, 1))
grid_eu_fr$nb_site <- lengths(vegesf_fr_grid)

ggplot(grid_eu_fr) +
  geom_sf(aes(fill = nb_site))

### Calculate  cover of each species in each cell

vegedf_fr <- species_eva1[which(species_eva1$Match==1),c("PlotObservationID","Turboveg2 concept","Cover %")]

vegedf_fr$grid <- NA
for(i in 1:length(vegesf_fr_grid)){
  site_id <- header_fr$PlotObservationID[vegesf_fr_grid[[i]]]
  vegedf_fr$grid[which(vegedf_fr$PlotObservationID %in% site_id)] <- i
}

vegedf_fr_grid2 <- vegedf_fr %>% group_by(grid,`Turboveg2 concept`) %>% summarize(sum_cover=sum(`Cover %`))
nb_tot_site_per_cell <- vegedf_fr %>% group_by(grid,PlotObservationID) %>% summarize(count=n())
nb_tot_site_per_cell <- nb_tot_site_per_cell %>% group_by(grid) %>% summarize(count=n())

vegedf_fr_grid2 <- merge(vegedf_fr_grid2, nb_tot_site_per_cell, by="grid", all.x=TRUE)
vegedf_fr_grid2$perc_cover <- vegedf_fr_grid2$sum_cover/vegedf_fr_grid2$count

### Select the five most important species in each cell

vegedf_fr_grid2 <- vegedf_fr_grid2 %>%  group_by(grid) %>%  arrange(desc(perc_cover), .by_group=TRUE)

vegedf_fr_grid_5_sp <- vegedf_fr_grid2 %>% group_by(grid)  %>% slice(1:5)

### apply bioregion

vegedf_fr <- vegedf_fr_grid_5_sp

vegedf_fr$species <- as.numeric(as.factor(vegedf_fr$`Turboveg2 concept`))
names(vegedf_fr)[c(1,3)] <- c("site","cover")
vegedf_fr$`Turboveg2 concept` <- NULL
vegedf_fr <- vegedf_fr[,c("site","species","cover")]
vegedf_fr <- na.omit(vegedf_fr[!duplicated(vegedf_fr[,c(1,2)]),])

vegemat_fr <- net_to_mat(as.data.frame(vegedf_fr), weight = TRUE, squared = FALSE, symmetrical = FALSE, missing_value = 0)

dissim <- na.omit(dissimilarity(vegemat_fr))

tree4 <- hclu_hierarclust(dissim,
                          n_clust = 2:100)

eval_tree4 <- partition_metrics(tree4, 
                                dissimilarity = dissim, # Provide distances to compute the metrics
                                eval_metric = "pc_distance")

opti_n_tree4 <- find_optimal_n(eval_tree4)

K_name <- opti_n_tree4$evaluation_df$K[opti_n_tree4$evaluation_df$optimal_n_pc_distance]

### Make a map of the clusters

grid_eu_fr$ID <- 1:nrow(grid_eu_fr)
grid_eu_fr <- st_sf(grid_eu_fr)
map_clusters(tree4$clusters[, c("ID", K_name)],
             grid_eu_fr[,c("ID","geometry")])
