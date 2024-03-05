# Make bioregion from EVA

## Load EVA data

header_eva <- read.csv("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_header.csv", sep="\t", header=TRUE)

### Data too large for R, subsection for France to test

header_fr <- header_eva[which(header_eva$Country=="France"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_fr$PlotObservationID))
species_eva1 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")
saveRDS(species_eva1,"output/species_eva1.rds")
species_eva1 <- readRDS("output/species_eva1.rds")

header_al <- header_eva[which(header_eva$Country=="Albania"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_al$PlotObservationID))
species_eva2 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_it <- header_eva[which(header_eva$Country=="Italy"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_it$PlotObservationID))
species_eva3 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_mal <- header_eva[which(header_eva$Country=="Malta"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_mal$PlotObservationID))
species_eva4 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_au <- header_eva[which(header_eva$Country=="Austria"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_au$PlotObservationID))
species_eva5 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_slv <- header_eva[which(header_eva$Country=="Slovenia"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_slv$PlotObservationID))
species_eva6 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_gr <- header_eva[which(header_eva$Country=="Greece"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_gr$PlotObservationID))
species_eva7 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_sp <- header_eva[which(header_eva$Country=="Spain"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_sp$PlotObservationID))
species_eva8 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_swi <- header_eva[which(header_eva$Country=="Switzerland"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_swi$PlotObservationID))
species_eva9 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_ko <- header_eva[which(header_eva$Country=="Kosovo"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_ko$PlotObservationID))
species_eva10 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_se <- header_eva[which(header_eva$Country=="Serbia"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_se$PlotObservationID))
species_eva11 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                   DataFrameCallback$new(f),
                                   delim="\t")

header_bu <- header_eva[which(header_eva$Country=="Bulgaria"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_bu$PlotObservationID))
species_eva12 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_mac <- header_eva[which(header_eva$Country=="Macedonia"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_mac$PlotObservationID))
species_eva13 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_ro <- header_eva[which(header_eva$Country=="Romania"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_ro$PlotObservationID))
species_eva14 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_bo <- header_eva[which(header_eva$Country=="Bosnia-Herzegovina"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_bo$PlotObservationID))
species_eva15 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_mo <- header_eva[which(header_eva$Country=="Montenegro"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_mo$PlotObservationID))
species_eva16 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_cr <- header_eva[which(header_eva$Country=="Croatia"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_cr$PlotObservationID))
species_eva17 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_cz <- header_eva[which(header_eva$Country=="Czech Republic"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_cz$PlotObservationID))
species_eva18 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_ge <- header_eva[which(header_eva$Country=="Germany"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_ge$PlotObservationID))
species_eva19 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_po <- header_eva[which(header_eva$Country=="Poland"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_po$PlotObservationID))
species_eva20 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_slk <- header_eva[which(header_eva$Country=="Slovak Republic"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_slk$PlotObservationID))
species_eva21 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_no <- header_eva[which(header_eva$Country=="Norway"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_no$PlotObservationID))
species_eva22 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_swe <- header_eva[which(header_eva$Country=="Sweden"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_swe$PlotObservationID))
species_eva23 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_hu <- header_eva[which(header_eva$Country=="Hungary"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_hu$PlotObservationID))
species_eva24 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_fi <- header_eva[which(header_eva$Country=="Finland"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_fi$PlotObservationID))
species_eva25 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_de <- header_eva[which(header_eva$Country=="Denmark"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_de$PlotObservationID))
species_eva26 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_be <- header_eva[which(header_eva$Country=="Belgium"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_be$PlotObservationID))
species_eva27 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_uk <- header_eva[which(header_eva$Country=="United Kingdom"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_uk$PlotObservationID))
species_eva28 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_lu <- header_eva[which(header_eva$Country=="Luxembourg"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_lu$PlotObservationID))
species_eva29 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_null <- header_eva[which(header_eva$Country==""),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_null$PlotObservationID))
species_eva30 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_ir <- header_eva[which(header_eva$Country=="Ireland"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_ir$PlotObservationID))
species_eva31 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_lu <- header_eva[which(header_eva$Country=="Lithuania"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_lu$PlotObservationID))
species_eva32 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_es <- header_eva[which(header_eva$Country=="Estonia"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_es$PlotObservationID))
species_eva33 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_cy <- header_eva[which(header_eva$Country=="Cyprus"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_cy$PlotObservationID))
species_eva34 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
                                    DataFrameCallback$new(f),
                                    delim="\t")

header_pt <- header_eva[which(header_eva$Country=="Portugal"),]
f <- function(x, pos) subset(x, PlotObservationID %in% unique(header_pt$PlotObservationID))
species_eva35 <- read_delim_chunked("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_species.csv",
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

# for France

### Use EU grid as site 

grid_eu <- st_read("raw_data/grid_eu/grid_50km_surf.gpkg")
grid_eu <- st_read("raw_data/grid_eu/grid_20km_surf.gpkg")

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

#vegedf_fr$grid <- NA
#for(i in 1:length(vegesf_fr_grid)){
#  site_id <- header_fr$PlotObservationID[vegesf_fr_grid[[i]]]
#  vegedf_fr$grid[which(vegedf_fr$PlotObservationID %in% site_id)] <- i
#}

nb_site_per_grid <- sapply(vegesf_fr_grid, length)
vegesf_grid_10 <- vegesf_fr_grid[which(nb_site_per_grid>9)]

vegesf_grid_list <- as.list(vegesf_fr_grid[1:length(vegesf_fr_grid)])

grid_plot <- ldply(vegesf_grid_list, .fun=function(x){
  if(rlang::is_empty(x)){
    y <- data.frame(plot_num=NA)
  }else{
    y <- data.frame(plot_num=header_fr$PlotObservationID[x])
  }
  return(y)
})

grid_plot$grid_num <- NA
j <- 1
for(i in 1:length(nb_site_per_grid)){
  if(nb_site_per_grid[i]<2){
    grid_plot$grid_num[j] <- i
    j <- j + 1
  }else{
    grid_plot$grid_num[j:(j+nb_site_per_grid[i]-1)] <- i
    j <- j + nb_site_per_grid[i]
  }
}

grid_plot_nan <- na.omit(grid_plot)
names(grid_plot_nan) <- c("plot_num","grid")
vegedf_fr$PlotObservationID <- as.integer(vegedf_fr$PlotObservationID)
vegedf_fr <- merge(vegedf_fr, grid_plot_nan, by.x="PlotObservationID", by.y="plot_num", all.x=TRUE)



vegedf_fr_grid2 <- vegedf_fr %>% group_by(grid,`Turboveg2 concept`) %>% summarize(sum_cover=sum(`Cover %`))
nb_tot_site_per_cell <- vegedf_fr %>% group_by(grid,PlotObservationID) %>% summarize(count=n())
nb_tot_site_per_cell <- nb_tot_site_per_cell %>% group_by(grid) %>% summarize(count=n())

vegedf_fr_grid2 <- merge(vegedf_fr_grid2, nb_tot_site_per_cell, by="grid", all.x=TRUE)
vegedf_fr_grid2$perc_cover <- vegedf_fr_grid2$sum_cover/vegedf_fr_grid2$count

### Select the cells with enough sites

nb_select_site <- 10
vegedf_fr_grid_10site <- vegedf_fr_grid2[which(vegedf_fr_grid2$grid %in% which(nb_site_per_grid>=nb_select_site)),]

### Select the five most important species in each cell

vegedf_fr_grid_10site <- vegedf_fr_grid_10site %>%  group_by(grid) %>%  arrange(desc(perc_cover), .by_group=TRUE)

vegedf_fr_grid_5_sp <- vegedf_fr_grid_10site %>% group_by(grid)  %>% slice(1:10)

### apply bioregion

vegedf_fr <- vegedf_fr_grid_5_sp

vegedf_fr$species <- as.numeric(as.factor(vegedf_fr$`Turboveg2 concept`))
names(vegedf_fr)[c(1,3)] <- c("site","cover")
vegedf_fr$`Turboveg2 concept` <- NULL
vegedf_fr <- vegedf_fr[,c("site","species","cover")]
vegedf_fr <- na.omit(vegedf_fr[!duplicated(vegedf_fr[,c(1,2)]),])

vegemat_fr <- net_to_mat(as.data.frame(vegedf_fr), weight = TRUE, squared = FALSE, symmetrical = FALSE, missing_value = 0)

#### hierarchical clustering

dissim <- na.omit(dissimilarity(vegemat_fr))

tree4 <- hclu_hierarclust(dissim,
                          n_clust = 2:100)

eval_tree4 <- partition_metrics(tree4, 
                                dissimilarity = dissim, # Provide distances to compute the metrics
                                eval_metric = "pc_distance")

opti_n_tree4 <- find_optimal_n(eval_tree4)

K_name <- opti_n_tree4$evaluation_df$K[opti_n_tree4$evaluation_df$optimal_n_pc_distance]

#### Make a map of the clusters

grid_eu_fr$ID <- 1:nrow(grid_eu_fr)
grid_eu_fr <- st_sf(grid_eu_fr)
map_clusters(tree4$clusters[, c("ID", K_name)],
             grid_eu_fr[,c("ID","geometry")])


#### network clustering

vegemat_fr_simil <- similarity(vegemat_fr, metric = "Simpson")

install_binaries(binpath = "tempdir", infomap_version = c("2.1.0", "2.6.0"))

set.seed(1)
ex_infomap <- netclu_infomap(vegemat_fr_simil,
                             weight = TRUE,
                             index = names(vegemat_fr_simil)[3],
                             nbmod = 0,
                             markovtime = 1,
                             seed = 0,
                             numtrials = 1,
                             twolevel = FALSE,
                             show_hierarchy = FALSE,
                             directed = FALSE,
                             bipartite_version = FALSE,
                             bipartite = FALSE,
                             site_col = 1,
                             species_col = 2,
                             return_node_type = "both",
                             version = "2.6.0",
                             binpath = "tempdir",
                             path_temp = "infomap_temp",
                             delete_temp = TRUE)

table(ex_infomap$clusters[,2])
K_name <- names(ex_infomap$clusters)[2]

#### Make a map of the clusters

grid_eu_fr$ID <- 1:nrow(grid_eu_fr)
grid_eu_fr <- st_sf(grid_eu_fr)
map_clusters(ex_infomap$clusters[, c("ID", K_name)],
             grid_eu_fr[,c("ID","geometry")])

# for Europe

species_eva <- rbind(species_eva1,species_eva2,species_eva3,species_eva4,species_eva5,
                     species_eva6,species_eva7,species_eva8,species_eva9,species_eva10,
                     species_eva11,species_eva12,species_eva13,species_eva14,species_eva15,
                     species_eva16,species_eva17,species_eva18,species_eva19,species_eva20,
                     species_eva21,species_eva22,species_eva23,species_eva24,species_eva25,
                     species_eva26,species_eva27,species_eva28,species_eva29,species_eva31,
                     species_eva32,species_eva33,species_eva34,species_eva35)

saveRDS(species_eva,"output/species_eva.rds")

species_eva <- readRDS("output/species_eva.rds")

grid_eu <- st_read("raw_data/grid_eu/grid_50km_surf.gpkg")
grid_eu <- st_read("raw_data/grid_eu/grid_20km_surf.gpkg")

centroid_coord <- data.frame(header_eva[,c("Longitude","Latitude")])
centroid_coord$Longitude <- as.numeric(centroid_coord$Longitude)
vegesf <- st_geometry(st_as_sf(na.omit(centroid_coord),coords = c("Longitude","Latitude")))
st_crs(vegesf) <- 4326
vegesf <- st_transform(vegesf,crs = 3035)
vegesf <- as.data.frame(st_coordinates(vegesf))
vegesf <- vegesf[which(!is.na(vegesf$X)),]
vegesf <- pts2poly_centroids(vegesf,3000, crs = 3035)
vegesf <- st_sf(vegesf)

### Find site in each EU cell

vegesf_grid <- st_intersects(grid_eu,vegesf)

grid_eu$first_site_id <- unlist(lapply(vegesf_grid, `[`, 1))
grid_eu$nb_site <- lengths(vegesf_grid)

ggplot(grid_eu) +
  geom_sf(aes(fill = sqrt(nb_site)), colour=NA) +
  coord_sf(
    xlim = c(2834303, 7323799),
    ylim = c(1570352, 5418000)
  )

nb_site_per_grid <- sapply(vegesf_grid, length)
vegesf_grid_10 <- vegesf_grid[which(nb_site_per_grid>9)]

vegesf_grid_list <- as.list(vegesf_grid[1:length(vegesf_grid)])

grid_plot <- ldply(vegesf_grid_list, .fun=function(x){
  if(rlang::is_empty(x)){
    y <- data.frame(plot_num=NA)
  }else{
    y <- data.frame(plot_num=header_eva$PlotObservationID[x])
  }
  return(y)
})

grid_plot$grid_num <- NA
j <- 1
for(i in 1:length(nb_site_per_grid)){
  if(nb_site_per_grid[i]<2){
    grid_plot$grid_num[j] <- i
    j <- j + 1
  }else{
    grid_plot$grid_num[j:(j+nb_site_per_grid[i]-1)] <- i
    j <- j + nb_site_per_grid[i]
  }
}


### If grid 20 Calculate  cover of each species in each cell

vegedf <- species_eva[which(species_eva$Match==1),c("PlotObservationID","Turboveg2 concept","Cover %")]

grid_plot_nan <- na.omit(grid_plot)
names(grid_plot_nan) <- c("plot_num","grid") 
vegedf <- merge(vegedf, grid_plot_nan, by.x="PlotObservationID", by.y="plot_num", all.x=TRUE)


vegedf_grid2 <- vegedf %>% group_by(grid,`Turboveg2 concept`) %>% summarize(sum_cover=sum(`Cover %`))
nb_tot_site_per_cell <- vegedf %>% group_by(grid,PlotObservationID) %>% summarize(count=n())
nb_tot_site_per_cell <- nb_tot_site_per_cell %>% group_by(grid) %>% summarize(count=n())

vegedf_grid2 <- merge(vegedf_grid2, nb_tot_site_per_cell, by="grid", all.x=TRUE)
vegedf_grid2$perc_cover <- vegedf_grid2$sum_cover/vegedf_grid2$count

vegedf_grid_saturation <- vegedf %>% group_by(grid,`Turboveg2 concept`) %>% summarize(count=n())
vegedf_grid_saturation <- vegedf_grid_saturation %>% group_by(grid) %>% summarize(count=n())
vegedf_grid_saturation <- merge(vegedf_grid_saturation,nb_tot_site_per_cell, by="grid",all.x=TRUE)
names(vegedf_grid_saturation)[2:3] <- c("nb_species","nb_sites")
plot(nb_species~nb_sites,vegedf_grid_saturation[which(vegedf_grid_saturation$nb_sites<100),])

### Select the cells with enough sites

nb_select_site <- 10
vegedf_grid_10site <- vegedf_grid2[which(vegedf_grid2$grid %in% which(nb_site_per_grid>=nb_select_site)),]

### Select the n most important species in each cell

vegedf_grid_10site <- vegedf_grid_10site %>%  group_by(grid) %>%  arrange(desc(perc_cover), .by_group=TRUE)
nb_select_sp <- 100
vegedf_grid_10site <- vegedf_grid_10site %>% group_by(grid)  %>% slice(1:nb_select_sp)

### apply bioregion

vegedf_eu <- vegedf_grid_10site

vegedf_eu$species <- as.numeric(as.factor(vegedf_eu$`Turboveg2 concept`))
names(vegedf_eu)[c(1,3)] <- c("site","cover")
vegedf_eu$`Turboveg2 concept` <- NULL
vegedf_eu <- vegedf_eu[,c("site","species","cover")]
vegedf_eu <- na.omit(vegedf_eu[!duplicated(vegedf_eu[,c(1,2)]),])

vegemat <- net_to_mat(as.data.frame(vegedf_eu), weight = TRUE, squared = FALSE, symmetrical = FALSE, missing_value = 0)

#### hierarchical clustering

dissim <- na.omit(dissimilarity(vegemat))

tree4 <- hclu_hierarclust(dissim,
                          n_clust = 2:100)

eval_tree4 <- partition_metrics(tree4, 
                                dissimilarity = dissim, # Provide distances to compute the metrics
                                eval_metric = "pc_distance")

opti_n_tree4 <- find_optimal_n(eval_tree4)

K_name <- opti_n_tree4$evaluation_df$K[opti_n_tree4$evaluation_df$optimal_n_pc_distance]

#### Make a map of the clusters

grid_eu$ID <- 1:nrow(grid_eu)
grid_eu <- st_sf(grid_eu)
map_clusters(tree4$clusters[, c("ID", K_name)],
             grid_eu[,c("ID","geometry")])

#### network clustering

vegemat_simil <- similarity(vegemat, metric = "Simpson")
vegemat_simil <- similarity(vegemat, metric = "Bray")

install_binaries(binpath = "tempdir", infomap_version = c("2.1.0", "2.6.0"))

set.seed(1)#un
ex_infomap <- netclu_infomap(na.omit(vegemat_simil),
                             weight = TRUE,
                             index = names(vegemat_simil)[3],
                             nbmod = 0,
                             markovtime = 1,
                             seed = 0,
                             numtrials = 10,
                             twolevel = FALSE,
                             show_hierarchy = TRUE,
                             directed = FALSE,
                             bipartite_version = FALSE,
                             bipartite = FALSE,
                             site_col = 1,
                             species_col = 2,
                             return_node_type = "both",
                             version = "2.6.0",
                             binpath = "tempdir",
                             path_temp = "infomap_temp",
                             delete_temp = TRUE)
table(ex_infomap$clusters[,2])
K_name_info <- names(ex_infomap$clusters)[2]


set.seed(1)
ex_louvain <- netclu_louvain(na.omit(vegemat_simil),
                             weight = TRUE,
                             index = names(vegemat_simil)[3],
                             lang = "Cpp",
                             q = 0,
                             c = 0.5,
                             k = 1,
                             bipartite = FALSE,
                             site_col = 1,
                             species_col = 2,
                             return_node_type = "both",
                             binpath = "tempdir",
                             path_temp = "louvain_temp",
                             delete_temp = TRUE,
                             algorithm_in_output = TRUE)
table(ex_louvain$clusters[,2])
K_name_louv <- names(ex_louvain$clusters)[2]

set.seed(1)
ex_greedy <- netclu_greedy(na.omit(vegemat_simil),
                           weight = FALSE,
                           index = names(vegemat_simil)[3],
                           bipartite = FALSE,
                           site_col = 1,
                           species_col = 2,
                           return_node_type = "both",
                           algorithm_in_output = TRUE)
table(ex_greedy$clusters[,2])
K_name_gree <- names(ex_greedy$clusters)[2]

set.seed(1) #un
ex_labelprop <- netclu_labelprop(na.omit(vegemat_simil),
                                 weight = TRUE,
                                 index = names(vegemat_simil)[3],
                                 bipartite = FALSE,
                                 site_col = 1,
                                 species_col = 2,
                                 return_node_type = "both",
                                 algorithm_in_output = TRUE)
table(ex_labelprop$clusters[,2])
K_name_labe <- names(ex_labelprop$clusters)[2]

set.seed(1) # trop nombreux
ex_leiden <- netclu_leiden(na.omit(vegemat_simil),
                           weight = FALSE,
                           index = names(vegemat_simil)[3],
                           objective_function = c("CPM", "modularity"),
                           resolution_parameter = 1,
                           beta = 0.01,
                           initial_membership = NULL,
                           n_iterations = 2,
                           vertex_weights = NULL,
                           bipartite = FALSE,
                           site_col = 1,
                           species_col = 2,
                           return_node_type = "both",
                           algorithm_in_output = TRUE)
table(ex_leiden$clusters[,2])
K_name_leid <- names(ex_leiden$clusters)[2]

set.seed(1)#un
ex_leadingeigen <- netclu_leadingeigen(na.omit(vegemat_simil),
                                       weight = FALSE,
                                       index = names(vegemat_simil)[3],
                                       bipartite = FALSE,
                                       site_col = 1,
                                       species_col = 2,
                                       return_node_type = "both",
                                       algorithm_in_output = TRUE)
table(ex_leadingeigen$clusters[,2])
K_name_lead <- names(ex_leadingeigen$clusters)[2]


#### Make a map of the clusters

grid_eu$ID <- 1:nrow(grid_eu)
grid_eu <- st_sf(grid_eu)
map_clusters(ex_infomap$clusters[, c("ID", K_name_info)],
             grid_eu[,c("ID","geometry")])
map_clusters(ex_louvain$clusters[, c("ID", K_name_louv)],
             grid_eu[,c("ID","geometry")])
map_clusters(ex_greedy$clusters[, c("ID", K_name_gree)],
             grid_eu[,c("ID","geometry")])
map_clusters(ex_labelprop$clusters[, c("ID", K_name_labe)],
             grid_eu[,c("ID","geometry")])
map_clusters(ex_leiden$clusters[, c("ID", K_name_labe)],
             grid_eu[,c("ID","geometry")])
map_clusters(ex_leadingeigen$clusters[, c("ID", K_name_lead)],
             grid_eu[,c("ID","geometry")])



### If grid 50 Calculate  cover of each species in each cell

vegedf <- species_eva[which(species_eva$Match==1),c("PlotObservationID","Turboveg2 concept","Cover %")]

vegedf$grid <- NA
for(i in 1:length(vegesf_grid)){
  print(i)
  site_id <- header_eva$PlotObservationID[vegesf_grid[[i]]]
  vegedf$grid[which(vegedf$PlotObservationID %in% site_id)] <- i
}

vegedf_grid2 <- vegedf %>% group_by(grid,`Turboveg2 concept`) %>% summarize(sum_cover=sum(`Cover %`))
nb_tot_site_per_cell <- vegedf %>% group_by(grid,PlotObservationID) %>% summarize(count=n())
nb_tot_site_per_cell <- nb_tot_site_per_cell %>% group_by(grid) %>% summarize(count=n())

vegedf_grid2 <- merge(vegedf_grid2, nb_tot_site_per_cell, by="grid", all.x=TRUE)
vegedf_grid2$perc_cover <- vegedf_grid2$sum_cover/vegedf_grid2$count

### Select the n most important species in each cell

vegedf_grid2 <- vegedf_grid2 %>%  group_by(grid) %>%  arrange(desc(perc_cover), .by_group=TRUE)
nb_select_sp <- 10
vegedf_grid_5_sp <- vegedf_grid2 %>% group_by(grid)  %>% slice(1:nb_select_sp)

### apply bioregion

vegedf_eu <- vegedf_grid_5_sp

vegedf_eu$species <- as.numeric(as.factor(vegedf_eu$`Turboveg2 concept`))
names(vegedf_eu)[c(1,3)] <- c("site","cover")
vegedf_eu$`Turboveg2 concept` <- NULL
vegedf_eu <- vegedf_eu[,c("site","species","cover")]
vegedf_eu <- na.omit(vegedf_eu[!duplicated(vegedf_eu[,c(1,2)]),])

vegemat <- net_to_mat(as.data.frame(vegedf_eu), weight = TRUE, squared = FALSE, symmetrical = FALSE, missing_value = 0)

dissim <- na.omit(dissimilarity(vegemat))

tree4 <- hclu_hierarclust(dissim,
                          n_clust = 2:100)

eval_tree4 <- partition_metrics(tree4, 
                                dissimilarity = dissim, # Provide distances to compute the metrics
                                eval_metric = "pc_distance")

opti_n_tree4 <- find_optimal_n(eval_tree4)

K_name <- opti_n_tree4$evaluation_df$K[opti_n_tree4$evaluation_df$optimal_n_pc_distance]

### Make a map of the clusters

grid_eu$ID <- 1:nrow(grid_eu)
grid_eu <- st_sf(grid_eu)
map_clusters(tree4$clusters[, c("ID", K_name)],
             grid_eu[,c("ID","geometry")])
