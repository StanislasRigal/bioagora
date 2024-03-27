bioregion_plant <- function(
    grid_eu_tempo,
    vegesf_tempo,
    species_eva_tempo,
    header_eva_tempo,
    nb_select_site = 10,
    nb_select_sp = 100,
    n_repet_clustering = 20){
  
  # find id and number of monitoring sites in each European grid cell
  
  vegesf_grid_tempo <- st_intersects(grid_eu_tempo,vegesf_tempo)
  
  grid_eu_tempo$first_site_id <- unlist(lapply(vegesf_grid_tempo, `[`, 1))
  grid_eu_tempo$nb_site <- lengths(vegesf_grid_tempo)
  
  map_nb_site <- ggplot(grid_eu_tempo) +
    geom_sf(aes(fill = sqrt(nb_site)), colour=NA) +
    coord_sf(
      xlim = c(2834303, 7323799),
      ylim = c(1570352, 5418000)
    )
  
  # link grid cell id and monitoring site id
  
  nb_site_per_grid <- sapply(vegesf_grid_tempo, length)
  
  vegesf_grid_tempo_list <- as.list(vegesf_grid_tempo[1:length(vegesf_grid_tempo)])
  
  grid_plot_tempo <- ldply(vegesf_grid_tempo_list, .fun=function(x){
    if(rlang::is_empty(x)){
      y <- data.frame(plot_num=NA)
    }else{
      y <- data.frame(plot_num=header_eva_tempo$PlotObservationID[x])
    }
    return(y)
  })
  
  grid_plot_tempo$grid_num <- NA
  j <- 1
  for(i in 1:length(nb_site_per_grid)){
    if(nb_site_per_grid[i]<2){
      grid_plot_tempo$grid_num[j] <- i
      j <- j + 1
    }else{
      grid_plot_tempo$grid_num[j:(j+nb_site_per_grid[i]-1)] <- i
      j <- j + nb_site_per_grid[i]
    }
  }
  
  vegedf_tempo <- species_eva_tempo[which(species_eva_tempo$Match==1),c("PlotObservationID","Turboveg2 concept","Cover %")]
  
  grid_plot_nan <- na.omit(grid_plot_tempo)
  names(grid_plot_nan) <- c("plot_num","grid") 
  vegedf_tempo <- merge(vegedf_tempo, grid_plot_nan, by.x="PlotObservationID", by.y="plot_num", all.x=TRUE)
  
  # summarise plant cover for each plant by grid cell
  
  vegedf_grid2 <- vegedf_tempo %>% group_by(grid,`Turboveg2 concept`) %>% summarize(sum_cover=sum(`Cover %`))
  nb_tot_site_per_cell <- vegedf_tempo %>% group_by(grid,PlotObservationID) %>% summarize(count=n())
  nb_tot_site_per_cell <- nb_tot_site_per_cell %>% group_by(grid) %>% summarize(count=n())
  
  vegedf_grid2 <- merge(vegedf_grid2, nb_tot_site_per_cell, by="grid", all.x=TRUE)
  vegedf_grid2$perc_cover <- vegedf_grid2$sum_cover/vegedf_grid2$count
  
  ### Select the cells with enough sites
  
  vegedf_grid_10site <- vegedf_grid2[which(vegedf_grid2$grid %in% which(nb_site_per_grid>=nb_select_site)),]
  
  ### Select the n most important species in each cell
  
  vegedf_grid_10site <- vegedf_grid_10site %>%  group_by(grid) %>%  arrange(desc(perc_cover), .by_group=TRUE)
  vegedf_grid_10site <- vegedf_grid_10site %>% group_by(grid)  %>% slice(1:nb_select_sp)
  
  ### apply bioregion
  
  vegedf_eu_tempo <- vegedf_grid_10site
  
  vegedf_eu_tempo$species <- as.numeric(as.factor(vegedf_eu_tempo$`Turboveg2 concept`))
  names(vegedf_eu_tempo)[c(1,3)] <- c("site","cover")
  vegedf_eu_tempo$`Turboveg2 concept` <- NULL
  vegedf_eu_tempo <- vegedf_eu_tempo[,c("site","species","cover")]
  vegedf_eu_tempo <- na.omit(vegedf_eu_tempo[!duplicated(vegedf_eu_tempo[,c(1,2)]),])
  
  vegemat_tempo <- net_to_mat(as.data.frame(vegedf_eu_tempo), weight = TRUE, squared = FALSE, symmetrical = FALSE, missing_value = 0)
  
  vegemat_simil_tempo <- similarity(vegemat_tempo, metric = "Bray")
  
  install_binaries(binpath = "tempdir", infomap_version = c("2.1.0", "2.6.0"))
  
  set.seed(1)
  ex_louvain <- netclu_louvain(na.omit(vegemat_simil_tempo),
                               weight = TRUE,
                               index = names(vegemat_simil_tempo)[3],
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
  
  for(i in 1:n_repet_clustering){
    set.seed(i+1)
    print(i)
    ex_louvain_repet <- netclu_louvain(na.omit(vegemat_simil_tempo),
                                       weight = TRUE,
                                       index = names(vegemat_simil_tempo)[3],
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
  
  ex_louvain$clusters$ID <- grid_eu_tempo$ID[as.numeric(ex_louvain$clusters$ID)]
  
  grid_eu_tempo <- st_sf(grid_eu_tempo)
  grid_bioregion_tempo <- merge(ex_louvain$clusters[, c("ID", "max")],grid_eu_tempo[,c("ID","geom")], by="ID", all.x=TRUE)
  
  return(grid_bioregion_tempo) #quid map_nb_site
}
