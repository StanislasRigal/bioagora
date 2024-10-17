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

### Continue with all countries

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

### Prepare data to calculate bioregions for France to test from sparse site data

#### subsample for computation time and get initial dataset

vegedf_fr <- species_eva1[which(species_eva1$Match==1),c("PlotObservationID","Turboveg2 concept","Cover %")]

set.seed(123) 
vegedf_fr <- vegedf_fr[which(vegedf_fr$PlotObservationID %in% sample(unique(vegedf_fr$PlotObservationID),2000)),]
vegedf_fr$species <- as.numeric(as.factor(vegedf_fr$`Turboveg2 concept`))
names(vegedf_fr)[c(1,3)] <- c("site","cover")
vegedf_fr$`Turboveg2 concept` <- NULL
vegedf_fr <- vegedf_fr[,c("site","species","cover")]
vegedf_fr <- vegedf_fr[!duplicated(vegedf_fr[,c(1,2)]),]

#### Transform initial dataset into ready to use dataset as explain here https://biorgeo.github.io/bioregion/articles/bioregion.html

vegemat_fr <- net_to_mat(as.data.frame(vegedf_fr), weight = TRUE, squared = FALSE,
                         symmetrical = FALSE, missing_value = 0)

dissim <- na.omit(dissimilarity(vegemat_fr))

tree4 <- hclu_hierarclust(dissim,
                          n_clust = 2:100)

eval_tree4 <- partition_metrics(tree4, 
                                dissimilarity = dissim, # Provide distances to compute the metrics
                                eval_metric = "pc_distance")

opti_n_tree4 <- find_optimal_n(eval_tree4) # optimal number of bioregion

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

# bioregion for France with European grid to test

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

### Calculate cover of each species in each cell

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

# get bioregions for Europe

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

#### network clustering (test different clustering technics)

vegemat_simil <- similarity(vegemat, metric = "Simpson")
vegemat_simil <- similarity(vegemat, metric = "Bray")

saveRDS(vegemat_simil,"output/vegemat_simil_bray.rds")

install_binaries(binpath = "tempdir", infomap_version = c("2.1.0", "2.6.0"))

set.seed(1) # one group
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

set.seed(1) # one group
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

set.seed(1) # too many groups
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

set.seed(1) # one group
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

### Louvain seems to be the best compromise speed, segregation so its repeated several times to assess the stability of clusters

n_repet_clustering <- 20

for(i in 1:n_repet_clustering){
  set.seed(i+1)
  print(i)
  ex_louvain_repet <- netclu_louvain(na.omit(vegemat_simil),
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
  ex_louvain$clusters[,(i+2)] <- ex_louvain_repet$clusters[,2]
  names(ex_louvain$clusters[,(i+2)]) <- paste0(names(ex_louvain_repet$clusters)[2],sep="_",i+1)
}

saveRDS(ex_louvain,"output/ex_louvain.rds")



# bootstrap clusters


for(i in 2:ncol(ex_louvain$clusters)){
  singleton <- as.numeric(names(table(ex_louvain$clusters[,i])[which(table(ex_louvain$clusters[,i])==1)]))
  ex_louvain$clusters[which(ex_louvain$clusters[,i] %in% singleton),i] <- NA
  ex_louvain$clusters[,i] <- as.numeric(ex_louvain$clusters[,i])
  for(j in 1:length(unique(na.omit(ex_louvain$clusters[,i])))){
    rep_val <- sort(unique(na.omit(ex_louvain$clusters[,i])))[j]
    ex_louvain$clusters[,i] <- sapply(ex_louvain$clusters[,i],  function(x) replace(x, x == rep_val, j))
  }
}

for(i in 3:ncol(ex_louvain$clusters)){
    
    all_partition2 <- ex_louvain$clusters
    
    temp_partition <- all_partition2[,i]
    
    # Compute Jaccard similarity to relabel clusters as in the original clustering
    
    jac_sim_res <- matrix(NA, ncol=length(unique(na.omit(all_partition2[,2]))),
                          nrow=length(unique(na.omit(temp_partition))))
    for(k in sort(unique(all_partition2[,2]))){
      for(l in sort(unique(temp_partition))){
        jac_sim_mat <- all_partition2[,c(2,i)]
        jac_sim_mat[,1] <- as.numeric(jac_sim_mat[,1])
        jac_sim_mat[,2] <- as.numeric(jac_sim_mat[,2])
        jac_sim_mat[,1][which(jac_sim_mat[,1]!=k | is.na(jac_sim_mat[,1]))] <- 0
        jac_sim_mat[,2][which(jac_sim_mat[,2]!=l | is.na(jac_sim_mat[,2]))] <- 0
        jac_sim_mat[jac_sim_mat>0] <- 1
        jac_sim_mat <- t(jac_sim_mat)
        jac_sim <- c(1 - vegan::vegdist(jac_sim_mat, method="jaccard"))
        jac_sim_res[l,k] <- jac_sim
      }
    }
    
    # If same number of clusters
    
    if(length(unique(all_partition2[,2]))==length(unique(temp_partition))){
      for(l in sort(unique(temp_partition))){
        all_partition2[,i][which(temp_partition==l)] <- which.max(jac_sim_res[l,])
      }
    }
    
    # If more clusters in the bootstrap clustering
    
    if(length(unique(all_partition2[,2]))<length(unique(temp_partition))){
      l_data <- c()
      for(k in sort(unique(all_partition2[,2]))){
        l_data <- c(l_data,which.max(jac_sim_res[,k]))
      }
      k <- 0
      for(l in l_data){
        k <- k+1
        all_partition2[,i][which(temp_partition==l)] <- k
      }
      extra_clus <- sort(unique(all_partition2[,i]))[which(!(sort(unique(temp_partition)) %in% l_data))]
      for(g_sup in 1:length(extra_clus)){
        k <- k +1
        all_partition2[,i][which(temp_partition==extra_clus[g_sup])] <- k
      }
      
    }
    
    # If less clusters in the bootstrap clustering
    
    if(length(unique(all_partition2[,2]))>length(unique(temp_partition))){
      k_data <- c()
      for(l in sort(unique(temp_partition))){
        k_data <- c(k_data,which.max(jac_sim_res[l,]))
      }
      l <- 0
      for(k in k_data){
        l <- l+1
        all_partition2[,i][which(temp_partition==l)] <- k
      }
    }
    ex_louvain$clusters <- all_partition2
}

ex_louvain$clusters[is.na(ex_louvain$clusters)] <- 0

ex_louvain$clusters$max <- unlist(apply(ex_louvain$clusters[,2:ncol(ex_louvain$clusters)],1,function(x){as.numeric(names(which.max(table(as.numeric(x)))))}))


grid_bioregion <- merge(ex_louvain$clusters[, c("ID", "max")],grid_eu[,c("ID","geometry")], by="ID", all.x=TRUE)

saveRDS(grid_bioregion,"output/grid_bioregion.rds")

# create polygons from sparse raster

point_bioregion <- st_centroid(grid_bioregion$geometry)
coord_point_bioregion <- do.call(rbind, st_geometry(point_bioregion)) %>% 
  as.data.frame() %>% setNames(c("lon","lat"))
alphahull_bioregion <- ahull(coord_point_bioregion,alpha = 1)
plot(alphahull_bioregion)



##### Bioregion inside biogeographical areas

biogeo_area <- read_sf(dsn = "raw_data/biogeography/eea_v_3035_1_mio_biogeo-regions_p_2016_v01_r00/", layer = "BiogeoRegions2016")

grid_eu <- st_read("raw_data/grid_eu/grid_20km_surf.gpkg")

biogeo_area_reproj <- st_transform(biogeo_area, crs(grid_eu))

raster_template <- rast(raster(x = "output/eu_land_system_reproj.tif"))
raster_template[] <- NA
biogeo_area_reproj$num_name<- as.numeric(as.factor(biogeo_area_reproj$name))
biogeo_area_reproj_rast <- st_rasterize(biogeo_area_reproj %>% dplyr::select(num_name, geometry), template=st_as_stars(raster_template), field = "num_name")

write_stars(biogeo_area_reproj_rast,"output/biogeo_area_reproj_rast.tif")

biogeo_area_reproj_rast <- rast(raster(x = "output/biogeo_area_reproj_rast.tif"))
temp1 <- exact_extract(biogeo_area_reproj_rast,grid_eu, fun=c("mode"))

grid_eu$biogeo_area <- temp1
ggplot(grid_eu) +
  geom_sf(aes(fill = biogeo_area), colour=NA) +
  coord_sf(
    xlim = c(2834303, 7323799),
    ylim = c(1570352, 5418000)
  )

#st_write(grid_eu,"output/grid_eu_biogeo_area.gpkg")

# create a grid for each bioregion

grid_eu <- st_read("output/grid_eu_biogeo_area.gpkg")
grid_eu$ID <- 1:nrow(grid_eu)

grid_eu_alpine <- grid_eu[which(grid_eu$biogeo_area==1),]
grid_eu_atlantic <- grid_eu[which(grid_eu$biogeo_area==4),]
grid_eu_boreal <- grid_eu[which(grid_eu$biogeo_area==6),]
grid_eu_continental <- grid_eu[which(grid_eu$biogeo_area==7),]
grid_eu_mediterranean <- grid_eu[which(grid_eu$biogeo_area==9),]
grid_eu_pannonian <- grid_eu[which(grid_eu$biogeo_area==11),]

# load plant data

header_eva <- read.csv("raw_data/EVA/197_Bioregions20240222_notJUICE/197_Bioregions20240222_notJUICE_header.csv", sep="\t", header=TRUE)

species_eva <- readRDS("output/species_eva.rds")

# prepare plant data

centroid_coord <- data.frame(header_eva[,c("Longitude","Latitude")])
centroid_coord$Longitude <- as.numeric(centroid_coord$Longitude)
vegesf <- st_geometry(st_as_sf(na.omit(centroid_coord),coords = c("Longitude","Latitude")))
st_crs(vegesf) <- 4326
vegesf <- st_transform(vegesf,crs = 3035)
vegesf <- as.data.frame(st_coordinates(vegesf))
vegesf <- vegesf[which(!is.na(vegesf$X)),]
vegesf <- pts2poly_centroids(vegesf,3000, crs = 3035)
vegesf <- st_sf(vegesf)

# load functions

source(file = "functions.R")

# apply the bioregion function in each biogeographical area

bioregion_alpine <- bioregion_plant(grid_eu_tempo=grid_eu_alpine,vegesf_tempo=vegesf,
                                    species_eva_tempo=species_eva,header_eva_tempo=header_eva,
                                    nb_select_site = 10,
                                    nb_select_sp = 100,n_repet_clustering = 20)

ggplot(st_as_sf(bioregion_alpine)) +
  geom_sf(aes(fill = max), colour=NA) + scale_fill_viridis_b()

bioregion_atlantic <- bioregion_plant(grid_eu_tempo=grid_eu_atlantic,vegesf_tempo=vegesf,
                                      species_eva_tempo=species_eva,header_eva_tempo=header_eva,
                                      nb_select_site = 10,
                                      nb_select_sp = 100,n_repet_clustering = 20)

ggplot(st_as_sf(bioregion_atlantic)) +
  geom_sf(aes(fill = max), colour=NA) + scale_fill_viridis_b()

bioregion_boreal <- bioregion_plant(grid_eu_tempo=grid_eu_boreal,vegesf_tempo=vegesf,
                                      species_eva_tempo=species_eva,header_eva_tempo=header_eva,
                                      nb_select_site = 10,
                                      nb_select_sp = 100,n_repet_clustering = 20)

ggplot(st_as_sf(bioregion_boreal)) +
  geom_sf(aes(fill = max), colour=NA) + scale_fill_viridis_b()

bioregion_continental <- bioregion_plant(grid_eu_tempo=grid_eu_continental,vegesf_tempo=vegesf,
                                    species_eva_tempo=species_eva,header_eva_tempo=header_eva,
                                    nb_select_site = 10,
                                    nb_select_sp = 100,n_repet_clustering = 20)

ggplot(st_as_sf(bioregion_continental)) +
  geom_sf(aes(fill = max), colour=NA) + scale_fill_viridis_b()

bioregion_mediterranean <- bioregion_plant(grid_eu_tempo=grid_eu_mediterranean,vegesf_tempo=vegesf,
                                    species_eva_tempo=species_eva,header_eva_tempo=header_eva,
                                    nb_select_site = 10,
                                    nb_select_sp = 100,n_repet_clustering = 20)

ggplot(st_as_sf(bioregion_mediterranean)) +
  geom_sf(aes(fill = max), colour=NA) + scale_fill_viridis_b()

bioregion_pannonian <- bioregion_plant(grid_eu_tempo=grid_eu_pannonian,vegesf_tempo=vegesf,
                                           species_eva_tempo=species_eva,header_eva_tempo=header_eva,
                                           nb_select_site = 10,
                                           nb_select_sp = 100,n_repet_clustering = 20)

ggplot(st_as_sf(bioregion_pannonian)) +
  geom_sf(aes(fill = max), colour=NA) + scale_fill_viridis_b()

# merge the result (sparse as only grid cells with more than 10 monitoring sites)

bioregion_alpine$biogeo_max <- paste0("alpine",sep="_",bioregion_alpine$max) 
bioregion_atlantic$biogeo_max <- paste0("atlantic",sep="_",bioregion_atlantic$max) 
bioregion_boreal$biogeo_max <- paste0("boreal",sep="_",bioregion_boreal$max) 
bioregion_continental$biogeo_max <- paste0("continental",sep="_",bioregion_continental$max) 
bioregion_mediterranean$biogeo_max <- paste0("mediterranean",sep="_",bioregion_mediterranean$max) 
bioregion_pannonian$biogeo_max <- paste0("pannonian",sep="_",bioregion_pannonian$max) 

bioregion_all <- bind_rows(bioregion_alpine, bioregion_atlantic, bioregion_boreal,
                       bioregion_continental, bioregion_mediterranean, bioregion_pannonian)

# merge with empty grid cells

grid_bioregion_all <- merge(bioregion_all[,c("ID","biogeo_max")],grid_eu[,c("ID","geom")], by="ID", all.y=TRUE)

#saveRDS(grid_bioregion_all,"output/grid_bioregion_all.rds")
grid_bioregion_all <- readRDS("output/grid_bioregion_all.rds")

ggplot(st_as_sf(grid_bioregion_all)) +
  geom_sf(aes(fill = biogeo_max), colour=NA) +
  scale_fill_viridis_d(na.value = "grey50") +
  coord_sf(
    xlim = c(2834303, 7323799),
    ylim = c(1570352, 5418000)
  )

# complete empty cells by proximity

index <- st_touches(st_as_sf(grid_bioregion_all),st_as_sf(grid_bioregion_all))

output <- st_as_sf(grid_bioregion_all) %>% 
  mutate(biogeo_max = ifelse(is.na(biogeo_max),
                       apply(index, 1, function(i){names(which.max(table(.$biogeo_max[i])))}),
                       biogeo_max))
output$biogeo_max <- sapply(output$biogeo_max, function(x) ifelse(is.null(x), NA, x))

while (length(which(is.na(output$biogeo_max)))>4000) { # 4000 because countries without any site (turkey, ukraine, iceland)
  print(length(which(is.na(output$biogeo_max))))
  index <- st_touches(st_as_sf(output),st_as_sf(output))
  output <- st_as_sf(output) %>% 
    mutate(biogeo_max = ifelse(is.na(biogeo_max),
                               apply(index, 1, function(i){names(which.max(table(.$biogeo_max[i])))}),
                               biogeo_max))
  output$biogeo_max <- sapply(output$biogeo_max, function(x) ifelse(is.null(x), NA, x))
}

# some bioregion are very small (< 1% of the 15 000 full cells) so remove and try again

table(output$biogeo_max)

grid_bioregion_all$biogeo_max[which(grid_bioregion_all$biogeo_max %in% names(table(output$biogeo_max)[which(table(output$biogeo_max)<150)]) &
                           !(grid_bioregion_all$biogeo_max %in% unique(grep('pann', grid_bioregion_all$biogeo_max, value=TRUE))))] <- NA
grid_bioregion_all$biogeo_max[which(grid_bioregion_all$biogeo_max %in% names(table(output$biogeo_max)[which(table(output$biogeo_max)<69)]))] <- NA


index <- st_touches(st_as_sf(grid_bioregion_all),st_as_sf(grid_bioregion_all))

output2 <- st_as_sf(grid_bioregion_all) %>% 
  mutate(biogeo_max = ifelse(is.na(biogeo_max),
                             apply(index, 1, function(i){names(which.max(table(.$biogeo_max[i])))}),
                             biogeo_max))
output2$biogeo_max <- sapply(output2$biogeo_max, function(x) ifelse(is.null(x), NA, x))

while (length(which(is.na(output2$biogeo_max)))>4000) { # 4000 because countries without any site (turkey, ukraine, iceland)
  print(length(which(is.na(output2$biogeo_max))))
  index <- st_touches(st_as_sf(output2),st_as_sf(output2))
  output2 <- st_as_sf(output2) %>% 
    mutate(biogeo_max = ifelse(is.na(biogeo_max),
                               apply(index, 1, function(i){names(which.max(table(.$biogeo_max[i])))}),
                               biogeo_max))
  output2$biogeo_max <- sapply(output2$biogeo_max, function(x) ifelse(is.null(x), NA, x))
}

table(output2$biogeo_max)

# merge grid cell according to bioregion

poly_biogeo <- output2 %>%
  group_by(biogeo_max) %>% 
  summarize(geom = st_union(geom))

ggplot(poly_biogeo) +
  geom_sf(aes(fill = biogeo_max), alpha=0.5) + scale_fill_viridis_d() +
  coord_sf(
    xlim = c(2834303, 7323799),
    ylim = c(1570352, 5418000)
  )

saveRDS(poly_biogeo,"output/poly_biogeo.rds")

# some cells are isolated, so we dissolve them into the neighbourging polygon, to do so we start by disaggregating bioregion to identify small areas

poly_biogeo_cast <- st_disaggregate(poly_biogeo)
poly_biogeo_cast$biogeo_max[which(is.na(poly_biogeo_cast$biogeo_max))] <- "outside"
poly_biogeo_cast$area <- st_area(poly_biogeo_cast)
threshold <- 2500000000
units(threshold) <- as_units("m^2")
poly_biogeo_cast$biogeo_max[which(poly_biogeo_cast$area < threshold)] <- NA

index <- st_touches(st_as_sf(poly_biogeo_cast),st_as_sf(poly_biogeo_cast))

output3 <- st_as_sf(poly_biogeo_cast) %>% 
  mutate(biogeo_max = ifelse(is.na(biogeo_max) ,
                             apply(index, 1, function(i){names(which.max(table(.$biogeo_max[i])))}),
                             biogeo_max))
output3$biogeo_max <- sapply(output3$biogeo_max, function(x) ifelse(is.null(x), NA, x))

# merge cleaned grid cells 

poly_biogeo_clean <- output3 %>%
  group_by(biogeo_max) %>% 
  summarize(geom = st_union(geom))

ggplot(poly_biogeo_clean) +
  geom_sf(aes(fill = biogeo_max), alpha=0.5) + scale_fill_viridis_d() +
  coord_sf(
    xlim = c(2834303, 7323799),
    ylim = c(1570352, 5418000)
  )

poly_biogeo_clean$biogeo_max[which(poly_biogeo_clean$biogeo_max == "outside")] <- NA

saveRDS(poly_biogeo_clean,"output/poly_biogeo_clean.rds")

grid_eu_1km <- st_read("raw_data/grid_eu/grid_1km_surf.gpkg")

raster_template <- rast(raster(x = "output/eu_land_system_reproj.tif"))
raster_template[] <- NA
poly_biogeo_clean$num_biogeo_max <- as.numeric(as.factor(poly_biogeo_clean$biogeo_max))
poly_biogeo_clean_rast <- st_rasterize(poly_biogeo_clean %>% dplyr::select(num_biogeo_max, geom), template=st_as_stars(raster_template), field = "num_biogeo_max")

write_stars(poly_biogeo_clean_rast,"output/poly_biogeo_clean_rast.tif")

poly_biogeo_clean_rast <- rast(raster(x = "output/poly_biogeo_clean_rast.tif"))
temp1 <- exact_extract(poly_biogeo_clean_rast,grid_eu_1km, fun=c("mode"))

grid_eu_1km$biogeo_area <- temp1
ggplot(grid_eu_1km) +
  geom_sf(aes(fill = biogeo_area), colour=NA) +
  coord_sf(
    xlim = c(2834303, 7323799),
    ylim = c(1570352, 5418000)
  )

st_write(grid_eu_1km,"output/grid_eu_bioregion.gpkg")


######## SDM

## Test from https://rspatial.org/raster/sdm/6_sdm_methods.html

library(dismo)
library(maptools)
data(wrld_simpl)
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
file <- file.path(system.file(package="dismo"), "ex/bradypus.csv")
bradypus <- read.table(file,  header=TRUE,  sep=',')
bradypus <- bradypus[,-1]
presvals <- extract(predictors, bradypus)
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] <- as.factor(sdmdata[,'biome'])

pred_nf <- dropLayer(predictors, 'biome')

set.seed(0)
group <- kfold(bradypus, 5)
pres_train <- bradypus[group != 1, ]
pres_test <- bradypus[group == 1, ]
ext <- extent(-90, -32, -33, 23)

set.seed(10)
backg <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

maxent()
xm <- maxent(predictors, pres_train, factors='biome')
plot(xm)
response(xm)

e <- evaluate(pres_test, backg_test, xm, predictors)
px <- predict(predictors, xm, ext=ext, progress='')
par(mfrow=c(1,2))
plot(px, main='Maxent, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
plot(px > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')

## Do the same with EVA






### using bioclimatic regions (Primaru Landscape Structure) from A Spatial Regional Reference Framework for Sustainability Assessment in Europe Renetzeder


grid_eu_1km <- st_read("raw_data/grid_eu/grid_1km_surf.gpkg")
NUTS3_PLS <- read.csv("raw_data/biogeography/map_bioclimatic_data_long.csv", header=TRUE)

NUTS3_PLS$NUTS3[which(!(NUTS3_PLS$NUTS3 %in% unique(grid_eu_1km$NUTS2021_3)))]

grid_eu_1km$PLS <- NA

for(i in 1:25){
  grid_eu_1km$PLS[which(substr(grid_eu_1km$NUTS2021_3,1,5) %in% NUTS3_PLS$NUTS3[which(NUTS3_PLS$PLS_region==i)])] <- i
}

grid_eu_1km_biogeo <- grid_eu_1km %>% group_by(PLS) %>% summarise(id="PLS_region") 
st_write(grid_eu_1km_biogeo,"output/grid_eu_1km_biogeo.gpkg")

column_to_add <- grid_eu_1km$PLS
write.csv(column_to_add,"output/column_to_add.csv",row.names = FALSE)
saveRDS(column_to_add,"output/column_to_add.rds")
