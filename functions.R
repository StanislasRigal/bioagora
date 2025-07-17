# function to get bioregion from vegetation data

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


#####
# Function to apply GWPR to bird species

gwpr_species_manual <- function(bird_data,pressure_data,site_data,bandwidth,bandwidth_auto=FALSE,
                                formula_gwpr,formula_gwpr_scheme,min_site_number_per_species,min_occurence_species=200){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","scheme_code","Long_LAEA","Lat_LAEA","pop","impervious","treedensity","lightpollution",
                                                   "woodprod","drymatter","tempspring","tempspringvar",  
                                                   "precspring","precspringvar",
                                                   "protectedarea","pesticide_nodu","smallwoodyfeatures",
                                                   "fragmentation","shannon","eulandsystem_cat")])
  
  col_names <- c("(Intercept)","year:treedensity","year:impervious","year:pop", 
                 "year:lightpollution","year:woodprod","year:drymatter","year:tempspring",
                 "year:tempspringvar","year:precspring","year:precspringvar","year:protectedarea",
                 "year:pesticide_nodu","year:smallwoodyfeatures","year:fragmentation","year:shannon",
                 "year:eulandsystem_catlow_intensity","year:eulandsystem_catmedium_intensity",
                 "year:eulandsystem_cathigh_intensity")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$eulandsystem_cat)) > 1){
      if(length(unique(poisson_df$scheme_code)) > 1){
        global_mod <- glm(formula_gwpr_scheme, family="poisson", data=poisson_df)
      }else{
        global_mod <- glm(formula_gwpr, family="poisson", data=poisson_df)
      }
    }else{
      if(length(unique(poisson_df$scheme_code)) > 1){
        global_mod <- glm(count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + scheme_code, family="poisson", data=poisson_df)
      }else{
        global_mod <- glm(count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon, family="poisson", data=poisson_df)
      }
    }
    
    if(global_mod$converged){
      
      ### autocorrelation of residuals
      
      poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = poisson_df,
                                           proj4string = CRS(crs(site_data)))
      
      nb <- dnearneigh(poisson_sf@coords, 10000,bandwidth)
      lw <- nb2listw(nb, style="W", zero.policy=TRUE)
      moran_I <- lm.morantest(global_mod,lw)
      moran_res <- c(unlist(moran_I[3])[1],unlist(moran_I[2])) # check if autocorrelated
      
      if(is.na(moran_res[2])){
        moran_res[2] <- 0
      }
      
      if(moran_res[2]<0.05){
        
        ### GWPR
        
        DM <- gw.dist(dp.locat = coordinates(poisson_sf))
        
        unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
        
        unique_poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(unique_poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = unique_poisson_df,
                                                    proj4string = CRS(crs(site_data)))
        
        unique_DM <- gw.dist(dp.locat = coordinates(unique_poisson_sf))
        
        bw.f1 <- bandwidth_test
        
        num_site_within_bw <- apply(unique_DM,2,function(x){which(x<bw.f1)})
        
        result_all_site <- daply(unique_poisson_df,.(siteID),.fun=function(x,num_site_within_bw,min_site_number_per_species){
          
          site_ID <- unique(x$siteID)
          site_ID_num <- which(unique_poisson_df$siteID == site_ID)
          
          unique_poisson_df_i <- unique_poisson_df[num_site_within_bw[[site_ID_num]],]
          
          if(nrow(unique_poisson_df_i) >= min_site_number_per_species){
            
            unique_poisson_df_i$w <- exp(-.5*(unique_DM[site_ID_num,num_site_within_bw[[site_ID_num]]]/bw.f1)^2)
            
            poisson_df_i <- poisson_df[which(poisson_df$siteID %in% unique_poisson_df_i$siteID),]
            
            weigth_i <- merge(poisson_df_i,unique_poisson_df_i[,c("siteID","w")],by="siteID")
            
            site_scheme <- weigth_i %>% group_by(scheme_code,siteID) %>% summarize(count=n())
            site_scheme <- site_scheme %>% group_by(scheme_code) %>% summarise(nb_site = n())
            
            if(length(unique(weigth_i$eulandsystem_cat)) > 1){
              if(length(unique(weigth_i$scheme_code)) > 1 && nrow(site_scheme[which(site_scheme$nb_site==1),]) == 0){
                res.poisson_i <- glm(formula_gwpr_scheme, family="poisson",
                                     data=weigth_i,
                                     weights = w) # bisquare
                result_i <- summary(res.poisson_i)$coefficients
                result_i <- result_i[grep("scheme_code",row.names(result_i),invert = TRUE),]
              }else{
                res.poisson_i <- glm(formula_gwpr, family="poisson",
                                     data=weigth_i,
                                     weights = w) # bisquare
                result_i <- summary(res.poisson_i)$coefficients
              }
            }else{
              if(length(unique(weigth_i$scheme_code)) > 1 && nrow(site_scheme[which(site_scheme$nb_site==1),]) == 0){
                res.poisson_i <- glm(count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                       year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
                                       year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                       year:shannon + scheme_code, family="poisson",
                                     data=weigth_i,
                                     weights = w) # bisquare
                result_i <- result_i[grep("scheme_code",row.names(result_i),invert = TRUE),]
              }else{
                res.poisson_i <- glm(count~year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                       year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
                                       year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                       year:shannon, family="poisson",
                                     data=weigth_i,
                                     weights = w) # bisquare
              }
            }
            
            if(nrow(result_i) == 19){
              result_site <- result_i
            }else{
              row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
              row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
              result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
              result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
              result_i_complet <- as.matrix(result_i_complet[2:5])
              result_site <- result_i_complet
            }
          }else{
            result_site <- matrix(NA,nrow=19,ncol=4)
          }
          
          return(result_site)
        },
        num_site_within_bw=num_site_within_bw,min_site_number_per_species=min_site_number_per_species,
        .progress="text")
        
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        result_all_site_scale <- result_all_site
        for(i in 1:dim(result_all_site_scale)[3]){
          result_all_site_scale[-1,1,i] <- scale(result_all_site_scale[-1,1,i],center = FALSE)
        }
        
        
        ### Remove edge effect by keeping sites with enough neighbourg
        
        site_to_keep <- unique_poisson_df$siteID[which(apply(unique_DM,1,function(x){length(which(x < bw.f1))}) > min_site_number_per_species)]
        
        res.poisson_noedge <- result_all_site[,,which(unique_poisson_sf$siteID %in% site_to_keep)]
        
        if(length(site_to_keep) > 1){
          res.poisson_noedge_df <- data.frame(res.poisson_noedge[1,1,],res.poisson_noedge[2,1,],res.poisson_noedge[3,1,],res.poisson_noedge[4,1,],
                                              res.poisson_noedge[5,1,],res.poisson_noedge[6,1,],res.poisson_noedge[7,1,],res.poisson_noedge[8,1,],
                                              res.poisson_noedge[9,1,],res.poisson_noedge[10,1,],res.poisson_noedge[11,1,],res.poisson_noedge[12,1,],
                                              res.poisson_noedge[13,1,],res.poisson_noedge[14,1,],res.poisson_noedge[15,1,],res.poisson_noedge[16,1,],
                                              res.poisson_noedge[17,1,],res.poisson_noedge[18,1,],res.poisson_noedge[19,1,])
          res.poisson_noedge_pval <- data.frame(res.poisson_noedge[1,4,],res.poisson_noedge[2,4,],res.poisson_noedge[3,4,],res.poisson_noedge[4,4,],
                                                res.poisson_noedge[5,4,],res.poisson_noedge[6,4,],res.poisson_noedge[7,4,],res.poisson_noedge[8,4,],
                                                res.poisson_noedge[9,4,],res.poisson_noedge[10,4,],res.poisson_noedge[11,4,],res.poisson_noedge[12,4,],
                                                res.poisson_noedge[13,4,],res.poisson_noedge[14,4,],res.poisson_noedge[15,4,],res.poisson_noedge[16,4,],
                                                res.poisson_noedge[17,4,],res.poisson_noedge[18,4,],res.poisson_noedge[19,4,])
        }
        if(length(site_to_keep) == 1){
          res.poisson_noedge_df <- data.frame(res.poisson_noedge[1,1],res.poisson_noedge[2,1],res.poisson_noedge[3,1],res.poisson_noedge[4,1],
                                              res.poisson_noedge[5,1],res.poisson_noedge[6,1],res.poisson_noedge[7,1],res.poisson_noedge[8,1],
                                              res.poisson_noedge[9,1],res.poisson_noedge[10,1],res.poisson_noedge[11,1],res.poisson_noedge[12,1],
                                              res.poisson_noedge[13,1],res.poisson_noedge[14,1],res.poisson_noedge[15,1],res.poisson_noedge[16,1],
                                              res.poisson_noedge[17,1],res.poisson_noedge[18,1],res.poisson_noedge[19,1])
          res.poisson_noedge_pval <- data.frame(res.poisson_noedge[1,4],res.poisson_noedge[2,4],res.poisson_noedge[3,4],res.poisson_noedge[4,4],
                                                res.poisson_noedge[5,4],res.poisson_noedge[6,4],res.poisson_noedge[7,4],res.poisson_noedge[8,4],
                                                res.poisson_noedge[9,4],res.poisson_noedge[10,4],res.poisson_noedge[11,4],res.poisson_noedge[12,4],
                                                res.poisson_noedge[13,4],res.poisson_noedge[14,4],res.poisson_noedge[15,4],res.poisson_noedge[16,4],
                                                res.poisson_noedge[17,4],res.poisson_noedge[18,4],res.poisson_noedge[19,4])
        }
        if(length(site_to_keep) == 0){
          res.poisson_noedge_df <- data.frame(matrix(NA,nrow=1,ncol=19))
          res.poisson_noedge_pval <- matrix(1,nrow=1,ncol=19)
        }
        
        
        
        res.poisson_noedge_df[res.poisson_noedge_pval > 0.05] <- NA 
        
        names(res.poisson_noedge_df) <- col_names
        
        if(length(site_to_keep) == 0){
          
          res.poisson_noedge_df$siteID <- NA
          res.poisson_noedge_df <- res.poisson_noedge_df[rowSums(is.na(res.poisson_noedge_df)) != (ncol(res.poisson_noedge_df)-1), ]
          
        }else{
          res.poisson_noedge_df$siteID <-  unique_poisson_sf$siteID[which(unique_poisson_sf$siteID %in% site_to_keep)]
          
          res.poisson_noedge_df <- res.poisson_noedge_df[rowSums(is.na(res.poisson_noedge_df)) != (ncol(res.poisson_noedge_df)-1), ]
        }
        
        
        
        #res.poisson_noedge_sf <- merge(unique_poisson_sf[,c("siteID")],res.poisson_noedge_df)
        #res_plot <- st_as_sf(res.poisson_noedge_sf)
        #ggplot(grid_eu_spafra_outline) + geom_sf() +  geom_sf(data=res_plot, aes(col=exp(`year:treedensity`))) + scale_color_gradientn(colors = sf.colors(20))
        
      }else{
        
        result_i <- summary(global_mod)$coef
        
        if(nrow(result_i) != 19){
          row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
          row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
          result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
          result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
          result_i_complet <- as.matrix(result_i_complet[2:5])
          result_i <- result_i_complet
        }
        
        res.poisson_noedge_df <- data.frame(t(result_i[,1]))
        names(res.poisson_noedge_df) <- col_names
        res.poisson_noedge_df$siteID <- NA
        
      }
      
    }else{
      res.poisson_noedge_df <- data.frame(t(rep(NA,19)))
      names(res.poisson_noedge_df) <- col_names
      res.poisson_noedge_df$siteID <- NA
    }
    
  }else{
    res.poisson_noedge_df <- data.frame(t(rep(NA,19)))
    names(res.poisson_noedge_df) <- col_names
    res.poisson_noedge_df$siteID <- NA
  }
  
  return(res.poisson_noedge_df)
}


##### 
# Function to apply GMLP to bird species

glm_species_biogeo <- function(bird_data,pressure_data,site_data,
                               formula_glmp,formula_glmp_scheme,min_site_number_per_species,
                               min_occurence_species=200,family){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","time_effort","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   "pop","impervious","treedensity","lightpollution",
                                                   "woodprod","drymatter","tempspring","tempspringvar",  
                                                   "precspring","precspringvar","humidityspring",
                                                   "protectedarea","pesticide_nodu","smallwoodyfeatures",
                                                   "fragmentation","shannon","eulandsystem_cat","biogeo_area")])
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$time_effort)) > length(unique(poisson_df$scheme_code)) & length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$time_effort <- scale(poisson_df$time_effort)
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }

  poisson_df$count_scale_all <- scales::rescale(poisson_df$count)
  
  
  col_names <- c("(Intercept)","year","year:treedensity","year:impervious","year:pop", 
                 "year:lightpollution","year:woodprod","year:drymatter","year:tempspring",
                 "year:tempspringvar","year:precspring","year:precspringvar","year:humidityspring","year:protectedarea",
                 "year:pesticide_nodu","year:smallwoodyfeatures","year:fragmentation","year:shannon",
                 "year:eulandsystem_catlow_intensity","year:eulandsystem_catmedium_intensity",
                 "year:eulandsystem_cathigh_intensity")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$eulandsystem_cat)) > 1){
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
        global_mod <- glm(formula_glmp_scheme, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
        global_mod <- glm(formula_glmp, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat, family=family, data=poisson_df)
      }
    }else{
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + time_effort + area_sampled_m2 + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + time_effort + area_sampled_m2, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon, family=family, data=poisson_df)
      }
    }
    
    if(global_mod$converged){
      
      ### autocorrelation of residuals
      
      poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = poisson_df,
                                           proj4string = CRS(crs(site_data)))
      
      ### GLMP
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      unique_poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(unique_poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = unique_poisson_df,
                                                  proj4string = CRS(crs(site_data)))
      
      
      result_all_site <- daply(unique_poisson_df,.(biogeo_area),.fun=function(x,min_site_number_per_species,poisson_df){
        
        if(nrow(x) >= min_site_number_per_species){
          
          poisson_df_i <- poisson_df[which(poisson_df$biogeo_area == unique(x$biogeo_area)),]
          
          #poisson_df_i$count_scale_all <- scales::rescale(poisson_df_i$count)
          
          if(length(table(poisson_df_i$time_effort)) > length(unique(poisson_df_i$scheme_code)) & length(table(poisson_df_i$area_sampled_m2)) > length(unique(poisson_df_i$scheme_code))){
            one_scheme_time_area <- 0 
            poisson_df_i$time_effort <- scale(poisson_df_i$time_effort)
            poisson_df_i$area_sampled_m2 <- scale(poisson_df_i$area_sampled_m2)
          }else{
            one_scheme_time_area <- 1
          }
          
          if(length(unique(poisson_df_i$eulandsystem_cat)) > 1){
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(formula_glmp_scheme, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(formula_glmp, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + year:eulandsystem_cat + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + year:eulandsystem_cat, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
          }else{
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(count_scale_all~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + time_effort + area_sampled_m2 + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(count_scale_all~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + time_effort + area_sampled_m2, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
          }
          
          if(nrow(result_i) == length(col_names)){
            result_site <- result_i
          }else{
            row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
            row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
            result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
            result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
            result_i_complet <- as.matrix(result_i_complet[2:5])
            result_site <- result_i_complet
          }
        }else{
          result_site <- matrix(NA,nrow=length(col_names),ncol=4)
        }
        
        return(result_site)
      },
      min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
      .progress="text")
      
      result_all_site <- aperm(result_all_site, c(2,3,1))
      
      if(dim(result_all_site)[3] > 1){
        res.poisson_df <- data.frame(result_all_site[1,1,],result_all_site[2,1,],result_all_site[3,1,],result_all_site[4,1,],
                                     result_all_site[5,1,],result_all_site[6,1,],result_all_site[7,1,],result_all_site[8,1,],
                                     result_all_site[9,1,],result_all_site[10,1,],result_all_site[11,1,],result_all_site[12,1,],
                                     result_all_site[13,1,],result_all_site[14,1,],result_all_site[15,1,],result_all_site[16,1,],
                                     result_all_site[17,1,],result_all_site[18,1,],result_all_site[19,1,],result_all_site[20,1,],result_all_site[21,1,])
        res.poisson_pval <- data.frame(result_all_site[1,4,],result_all_site[2,4,],result_all_site[3,4,],result_all_site[4,4,],
                                       result_all_site[5,4,],result_all_site[6,4,],result_all_site[7,4,],result_all_site[8,4,],
                                       result_all_site[9,4,],result_all_site[10,4,],result_all_site[11,4,],result_all_site[12,4,],
                                       result_all_site[13,4,],result_all_site[14,4,],result_all_site[15,4,],result_all_site[16,4,],
                                       result_all_site[17,4,],result_all_site[18,4,],result_all_site[19,4,],result_all_site[20,4,],result_all_site[21,4,])
      }
      if(dim(result_all_site)[3] == 1){
        res.poisson_df <- data.frame(result_all_site[1,1],result_all_site[2,1],result_all_site[3,1],result_all_site[4,1],
                                     result_all_site[5,1],result_all_site[6,1],result_all_site[7,1],result_all_site[8,1],
                                     result_all_site[9,1],result_all_site[10,1],result_all_site[11,1],result_all_site[12,1],
                                     result_all_site[13,1],result_all_site[14,1],result_all_site[15,1],result_all_site[16,1],
                                     result_all_site[17,1],result_all_site[18,1],result_all_site[19,1],result_all_site[20,1],result_all_site[21,1])
        res.poisson_pval <- data.frame(result_all_site[1,4],result_all_site[2,4],result_all_site[3,4],result_all_site[4,4],
                                       result_all_site[5,4],result_all_site[6,4],result_all_site[7,4],result_all_site[8,4],
                                       result_all_site[9,4],result_all_site[10,4],result_all_site[11,4],result_all_site[12,4],
                                       result_all_site[13,4],result_all_site[14,4],result_all_site[15,4],result_all_site[16,4],
                                       result_all_site[17,4],result_all_site[18,4],result_all_site[19,4],result_all_site[20,4],result_all_site[21,4])
      }
      if(dim(result_all_site)[3] == 0){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=length(col_names)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=length(col_names))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      names(res.poisson_df) <- col_names
      
      if(dim(result_all_site)[3] == 0){
        
        res.poisson_df$biogeo_area <- NA
        
      }else{
        
        res.poisson_df$biogeo_area <- row.names(res.poisson_df)
        
      }
      
      global_mod_coef <- summary(global_mod)$coefficient[grep("scheme_code|area_sampled_m2|time_effort",row.names(summary(global_mod)$coefficient),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- col_names
      global_mod_df$biogeo_area <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="biogeo_area")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`year:treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,length(col_names))))
      names(res.poisson_df) <- col_names
      res.poisson_df$biogeo_area <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,length(col_names))))
    names(res.poisson_df) <- col_names
    res.poisson_df$biogeo_area <- NA
  }
  
  return(res.poisson_df)
}



#### GLMM

glmm_species_biogeo <- function(bird_data,pressure_data,site_data,
                               formula_glmp,formula_glmp_scheme,min_site_number_per_species,min_occurence_species=200){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","scheme_code","Long_LAEA","Lat_LAEA","pop","impervious","treedensity","lightpollution",
                                                   "woodprod","drymatter","tempspring","tempspringvar",  
                                                   "precspring","precspringvar",
                                                   "protectedarea","pesticide_nodu","smallwoodyfeatures",
                                                   "fragmentation","shannon","eulandsystem_cat","biogeo_area")])
  
  col_names <- c("(Intercept)","year","year:treedensity","year:impervious","year:pop", 
                 "year:lightpollution","year:woodprod","year:drymatter","year:tempspring",
                 "year:tempspringvar","year:precspring","year:precspringvar","year:protectedarea",
                 "year:pesticide_nodu","year:smallwoodyfeatures","year:fragmentation","year:shannon",
                 "year:eulandsystem_catlow_intensity","year:eulandsystem_catmedium_intensity",
                 "year:eulandsystem_cathigh_intensity")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$eulandsystem_cat)) > 1){
      if(length(unique(poisson_df$scheme_code)) > 1){
        global_mod <- glmer(formula_glmp_scheme, family="poisson", data=poisson_df)
      }else{
        global_mod <- glmer(formula_glmp, family="poisson", data=poisson_df)
      }
    }else{
      if(length(unique(poisson_df$scheme_code)) > 1){
        global_mod <- glmer(count~year+year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + (1|scheme_code) + (1|siteID), family="poisson", data=poisson_df)
      }else{
        global_mod <- glmer(count~year+year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+#year:GDP_percap+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + (1|siteID), family="poisson", data=poisson_df)
      }
    }
    
    grep("failed to converge",global_mod@optinfo$conv$lme4$messages)

    poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = poisson_df,
                                         proj4string = CRS(crs(site_data)))
    
    ### GLMMP
    
    unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
    
    unique_poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(unique_poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = unique_poisson_df,
                                                proj4string = CRS(crs(site_data)))
    
    
    result_all_site <- daply(unique_poisson_df,.(biogeo_area),.fun=function(x,min_site_number_per_species,poisson_df){
      
      if(nrow(x) >= min_site_number_per_species){
        
        poisson_df_i <- poisson_df[which(poisson_df$biogeo_area == unique(x$biogeo_area)),]
        
        site_scheme <- poisson_df_i %>% group_by(scheme_code,siteID) %>% summarize(count=n())
        site_scheme <- site_scheme %>% group_by(scheme_code) %>% summarise(nb_site = n())
        
        if(length(unique(poisson_df_i$eulandsystem_cat)) > 1){
          if(length(unique(poisson_df_i$scheme_code)) > 1 && nrow(site_scheme[which(site_scheme$nb_site==1),]) == 0){
            res.poisson_i <- glmer(formula_glmp_scheme, family="poisson",
                                   data=poisson_df_i)
            result_i <- summary(res.poisson_i)$coefficients
          }else{
            res.poisson_i <- glmer(formula_glmp, family="poisson",
                                   data=poisson_df_i)
            result_i <- summary(res.poisson_i)$coefficients
          }
        }else{
          if(length(unique(poisson_df_i$scheme_code)) > 1 && nrow(site_scheme[which(site_scheme$nb_site==1),]) == 0){
            res.poisson_i <- glmer(count~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + (1|scheme_code) + (1|siteID), family="poisson",
                                   data=poisson_df_i)
            result_i <- summary(res.poisson_i)$coefficients
          }else{
            res.poisson_i <- glmer(count~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + (1|siteID), family="poisson",
                                   data=poisson_df_i)
            result_i <- summary(res.poisson_i)$coefficients
          }
        }
        
        if(nrow(result_i) == 20){
          result_site <- result_i
        }else{
          row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
          row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
          result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
          result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
          result_i_complet <- as.matrix(result_i_complet[2:5])
          result_site <- result_i_complet
        }
      }else{
        result_site <- matrix(NA,nrow=20,ncol=4)
      }
      
      return(result_site)
    },
    min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
    .progress="text")
    
    result_all_site <- aperm(result_all_site, c(2,3,1))
    
    result_all_site_scale <- result_all_site
    for(i in 1:dim(result_all_site_scale)[3]){
      result_all_site_scale[-1,1,i] <- scale(result_all_site_scale[-1,1,i],center = FALSE)
    }
    
    
    ### Remove edge effect by keeping sites with enough neighbourg
    
    if(dim(result_all_site)[3] > 1){
      res.poisson_df <- data.frame(result_all_site[1,1,],result_all_site[2,1,],result_all_site[3,1,],result_all_site[4,1,],
                                   result_all_site[5,1,],result_all_site[6,1,],result_all_site[7,1,],result_all_site[8,1,],
                                   result_all_site[9,1,],result_all_site[10,1,],result_all_site[11,1,],result_all_site[12,1,],
                                   result_all_site[13,1,],result_all_site[14,1,],result_all_site[15,1,],result_all_site[16,1,],
                                   result_all_site[17,1,],result_all_site[18,1,],result_all_site[19,1,],result_all_site[20,1,])
      res.poisson_pval <- data.frame(result_all_site[1,4,],result_all_site[2,4,],result_all_site[3,4,],result_all_site[4,4,],
                                     result_all_site[5,4,],result_all_site[6,4,],result_all_site[7,4,],result_all_site[8,4,],
                                     result_all_site[9,4,],result_all_site[10,4,],result_all_site[11,4,],result_all_site[12,4,],
                                     result_all_site[13,4,],result_all_site[14,4,],result_all_site[15,4,],result_all_site[16,4,],
                                     result_all_site[17,4,],result_all_site[18,4,],result_all_site[19,4,],result_all_site[20,4,])
    }
    if(dim(result_all_site)[3] == 1){
      res.poisson_df <- data.frame(result_all_site[1,1],result_all_site[2,1],result_all_site[3,1],result_all_site[4,1],
                                   result_all_site[5,1],result_all_site[6,1],result_all_site[7,1],result_all_site[8,1],
                                   result_all_site[9,1],result_all_site[10,1],result_all_site[11,1],result_all_site[12,1],
                                   result_all_site[13,1],result_all_site[14,1],result_all_site[15,1],result_all_site[16,1],
                                   result_all_site[17,1],result_all_site[18,1],result_all_site[19,1],result_all_site[20,1])
      res.poisson_pval <- data.frame(result_all_site[1,4],result_all_site[2,4],result_all_site[3,4],result_all_site[4,4],
                                     result_all_site[5,4],result_all_site[6,4],result_all_site[7,4],result_all_site[8,4],
                                     result_all_site[9,4],result_all_site[10,4],result_all_site[11,4],result_all_site[12,4],
                                     result_all_site[13,4],result_all_site[14,4],result_all_site[15,4],result_all_site[16,4],
                                     result_all_site[17,4],result_all_site[18,4],result_all_site[19,4],result_all_site[20,4])
    }
    if(dim(result_all_site)[3] == 0){
      res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=20))
      res.poisson_pval <- matrix(1,nrow=1,ncol=20)
    }
    
    
    
    res.poisson_df[res.poisson_pval > 0.05] <- NA 
    
    names(res.poisson_df) <- col_names
    
    if(dim(result_all_site)[3] == 0){
      
      res.poisson_df$biogeo_area <- NA
      
    }else{
      
      res.poisson_df$biogeo_area <- row.names(res.poisson_df)
      
    }
    
    global_mod_coef <- summary(global_mod)$coef[,1]
    global_mod_coef[which(summary(global_mod)$coef[,4] > 0.05)] <- NA
    global_mod_df <- data.frame(t(global_mod_coef))
    names(global_mod_df) <- col_names
    global_mod_df$biogeo_area <- "europe"
    
    res.poisson_df <- rbind(res.poisson_df,global_mod_df)
    
    #res.poisson_sf <- merge(grid_eu_spafra_biogeo,res.poisson_df,by="biogeo_area")
    #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`year:treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,20)))
    names(res.poisson_df) <- col_names
    res.poisson_df$biogeo_area <- NA
  }
  
  return(res.poisson_df)
}


### PLS

glm_species_PLS <- function(bird_data,pressure_data,site_data,
                               formula_glmp,formula_glmp_scheme,min_site_number_per_species,
                               min_occurence_species=200,family){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","time_effort","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   "pop","impervious","treedensity","lightpollution",
                                                   "woodprod","drymatter","tempspring","tempspringvar",  
                                                   "precspring","precspringvar","humidityspring",
                                                   "protectedarea","pesticide_nodu","smallwoodyfeatures",
                                                   "fragmentation","shannon","eulandsystem_cat","PLS")])
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$time_effort)) > length(unique(poisson_df$scheme_code)) & length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$time_effort <- scale(poisson_df$time_effort)
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- scales::rescale(poisson_df$count)
  
  
  col_names <- c("(Intercept)","year","year:treedensity","year:impervious","year:pop", 
                 "year:lightpollution","year:woodprod","year:drymatter","year:tempspring",
                 "year:tempspringvar","year:precspring","year:precspringvar","year:humidityspring","year:protectedarea",
                 "year:pesticide_nodu","year:smallwoodyfeatures","year:fragmentation","year:shannon",
                 "year:eulandsystem_catlow_intensity","year:eulandsystem_catmedium_intensity",
                 "year:eulandsystem_cathigh_intensity")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$eulandsystem_cat)) > 1){
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
        global_mod <- glm(formula_glmp_scheme, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
        global_mod <- glm(formula_glmp, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat, family=family, data=poisson_df)
      }
    }else{
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + time_effort + area_sampled_m2 + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + time_effort + area_sampled_m2, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon, family=family, data=poisson_df)
      }
    }
    
    if(global_mod$converged){
      
      ### autocorrelation of residuals
      
      poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = poisson_df,
                                           proj4string = CRS(crs(site_data)))
      
      ### GLMP
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      unique_poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(unique_poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = unique_poisson_df,
                                                  proj4string = CRS(crs(site_data)))
      
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=function(x,min_site_number_per_species,poisson_df){
        
        if(nrow(x) >= min_site_number_per_species){
          
          poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
          
          #poisson_df_i$count_scale_all <- scales::rescale(poisson_df_i$count)
          
          if(length(table(poisson_df_i$time_effort)) > length(unique(poisson_df_i$scheme_code)) & length(table(poisson_df_i$area_sampled_m2)) > length(unique(poisson_df_i$scheme_code))){
            one_scheme_time_area <- 0 
            poisson_df_i$time_effort <- scale(poisson_df_i$time_effort)
            poisson_df_i$area_sampled_m2 <- scale(poisson_df_i$area_sampled_m2)
          }else{
            one_scheme_time_area <- 1
          }
          
          if(length(unique(poisson_df_i$eulandsystem_cat)) > 1){
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(formula_glmp_scheme, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(formula_glmp, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + year:eulandsystem_cat + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + year:eulandsystem_cat, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
          }else{
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(count_scale_all~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + time_effort + area_sampled_m2 + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(count_scale_all~ year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + time_effort + area_sampled_m2, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                                     year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                                     year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                                     year:shannon, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
          }
          
          if(nrow(result_i) == length(col_names)){
            result_site <- result_i
          }else{
            row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
            row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
            result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
            result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
            result_i_complet <- as.matrix(result_i_complet[2:5])
            result_site <- result_i_complet
          }
        }else{
          result_site <- matrix(NA,nrow=length(col_names),ncol=4)
        }
        
        return(result_site)
      },
      min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
      .progress="text")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- data.frame(result_all_site[1,1,],result_all_site[2,1,],result_all_site[3,1,],result_all_site[4,1,],
                                       result_all_site[5,1,],result_all_site[6,1,],result_all_site[7,1,],result_all_site[8,1,],
                                       result_all_site[9,1,],result_all_site[10,1,],result_all_site[11,1,],result_all_site[12,1,],
                                       result_all_site[13,1,],result_all_site[14,1,],result_all_site[15,1,],result_all_site[16,1,],
                                       result_all_site[17,1,],result_all_site[18,1,],result_all_site[19,1,],result_all_site[20,1,],result_all_site[21,1,])
          res.poisson_pval <- data.frame(result_all_site[1,4,],result_all_site[2,4,],result_all_site[3,4,],result_all_site[4,4,],
                                         result_all_site[5,4,],result_all_site[6,4,],result_all_site[7,4,],result_all_site[8,4,],
                                         result_all_site[9,4,],result_all_site[10,4,],result_all_site[11,4,],result_all_site[12,4,],
                                         result_all_site[13,4,],result_all_site[14,4,],result_all_site[15,4,],result_all_site[16,4,],
                                         result_all_site[17,4,],result_all_site[18,4,],result_all_site[19,4,],result_all_site[20,4,],result_all_site[21,4,])
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- data.frame(result_all_site[1,1],result_all_site[2,1],result_all_site[3,1],result_all_site[4,1],
                                       result_all_site[5,1],result_all_site[6,1],result_all_site[7,1],result_all_site[8,1],
                                       result_all_site[9,1],result_all_site[10,1],result_all_site[11,1],result_all_site[12,1],
                                       result_all_site[13,1],result_all_site[14,1],result_all_site[15,1],result_all_site[16,1],
                                       result_all_site[17,1],result_all_site[18,1],result_all_site[19,1],result_all_site[20,1],result_all_site[21,1])
          res.poisson_pval <- data.frame(result_all_site[1,4],result_all_site[2,4],result_all_site[3,4],result_all_site[4,4],
                                         result_all_site[5,4],result_all_site[6,4],result_all_site[7,4],result_all_site[8,4],
                                         result_all_site[9,4],result_all_site[10,4],result_all_site[11,4],result_all_site[12,4],
                                         result_all_site[13,4],result_all_site[14,4],result_all_site[15,4],result_all_site[16,4],
                                         result_all_site[17,4],result_all_site[18,4],result_all_site[19,4],result_all_site[20,4],result_all_site[21,4])
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=length(col_names)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=length(col_names))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      names(res.poisson_df) <- col_names
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- row.names(res.poisson_df)
        
      }
      
      global_mod_coef <- summary(global_mod)$coefficient[grep("scheme_code|area_sampled_m2|time_effort",row.names(summary(global_mod)$coefficient),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- col_names
      global_mod_df$PLS <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`year:treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,length(col_names))))
      names(res.poisson_df) <- col_names
      res.poisson_df$PLS <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,length(col_names))))
    names(res.poisson_df) <- col_names
    res.poisson_df$PLS <- NA
  }
  
  return(res.poisson_df)
}



glm_species_PLS_trend <- function(bird_data,pressure_data,site_data,
                            min_site_number_per_species,
                            min_occurence_species=200,family){
  
  formula_glmp <- count_scale_all ~ year + time_effort + area_sampled_m2
  formula_glmp_scheme <- count_scale_all ~ year + time_effort + area_sampled_m2 + scheme_code
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","time_effort","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   "pop","impervious","treedensity","lightpollution",
                                                   "woodprod","drymatter","tempspring","tempspringvar",  
                                                   "precspring","precspringvar","humidityspring",
                                                   "protectedarea","pesticide_nodu","smallwoodyfeatures",
                                                   "fragmentation","shannon","eulandsystem_cat","PLS")])
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$time_effort)) > length(unique(poisson_df$scheme_code)) & length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$time_effort <- scale(poisson_df$time_effort)
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- scales::rescale(poisson_df$count)
  
  
  col_names <- c("(Intercept)","year")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$eulandsystem_cat)) > 1){
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
        global_mod <- glm(formula_glmp_scheme, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
        global_mod <- glm(formula_glmp, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year, family=family, data=poisson_df)
      }
    }else{
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
        global_mod <- glm(count_scale_all~year + time_effort + area_sampled_m2 + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
        global_mod <- glm(count_scale_all~year + time_effort + area_sampled_m2, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year + scheme_code, family=family, data=poisson_df)
      }
      if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
        global_mod <- glm(count_scale_all~year, family=family, data=poisson_df)
      }
    }
    
    if(global_mod$converged){
      
      ### autocorrelation of residuals
      
      poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = poisson_df,
                                           proj4string = CRS(crs(site_data)))
      
      ### GLMP
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      unique_poisson_sf <- SpatialPointsDataFrame(coords = as.matrix(unique_poisson_df[,c("Long_LAEA","Lat_LAEA")]), data = unique_poisson_df,
                                                  proj4string = CRS(crs(site_data)))
      
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=function(x,min_site_number_per_species,poisson_df){
        
        if(nrow(x) >= min_site_number_per_species){
          
          poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
          
          #poisson_df_i$count_scale_all <- scales::rescale(poisson_df_i$count)
          
          if(length(table(poisson_df_i$time_effort)) > length(unique(poisson_df_i$scheme_code)) & length(table(poisson_df_i$area_sampled_m2)) > length(unique(poisson_df_i$scheme_code))){
            one_scheme_time_area <- 0 
            poisson_df_i$time_effort <- scale(poisson_df_i$time_effort)
            poisson_df_i$area_sampled_m2 <- scale(poisson_df_i$area_sampled_m2)
          }else{
            one_scheme_time_area <- 1
          }
          
          if(length(unique(poisson_df_i$eulandsystem_cat)) > 1){
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(formula_glmp_scheme, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(formula_glmp, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
          }else{
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(count_scale_all~ year + time_effort + area_sampled_m2 + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
              res.poisson_i <- glm(count_scale_all~ year + time_effort + area_sampled_m2, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year + scheme_code, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
            if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
              res.poisson_i <- glm(count_scale_all~year, family=family,
                                   data=poisson_df_i)
              result_i <- summary(res.poisson_i)$coefficients
              result_i <- result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),]
            }
          }
          
          if(nrow(result_i) == length(col_names)){
            result_site <- result_i
          }else{
            row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
            row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
            result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
            result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
            result_i_complet <- as.matrix(result_i_complet[2:5])
            result_site <- result_i_complet
          }
        }else{
          result_site <- matrix(NA,nrow=length(col_names),ncol=4)
        }
        
        return(result_site)
      },
      min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
      .progress="text")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- data.frame(result_all_site[1,1,],result_all_site[2,1,])
          res.poisson_pval <- data.frame(result_all_site[1,4,],result_all_site[2,4,])
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- data.frame(result_all_site[1,1],result_all_site[2,1])
          res.poisson_pval <- data.frame(result_all_site[1,4],result_all_site[2,4])
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=length(col_names)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=length(col_names))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      names(res.poisson_df) <- col_names
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- row.names(res.poisson_df)
        
      }
      
      global_mod_coef <- summary(global_mod)$coefficient[grep("scheme_code|area_sampled_m2|time_effort",row.names(summary(global_mod)$coefficient),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- col_names
      global_mod_df$PLS <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`year:treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,length(col_names))))
      names(res.poisson_df) <- col_names
      res.poisson_df$PLS <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,length(col_names))))
    names(res.poisson_df) <- col_names
    res.poisson_df$PLS <- NA
  }
  
  return(res.poisson_df)
}



### GAM

bird_data <- droplevels(subsite_data_mainland_trend[which(subsite_data_mainland_trend$sci_name_out == "Alauda arvensis"),])
pressure_data <- press_mainland_trend_scale
site_data <- site_mainland_sf_reproj
min_site_number_per_species <- 60
min_occurence_species <- 200
family <- "quasipoisson"

gam_species_PLS1 <- function(bird_data,pressure_data,site_data,
                             pressure_name = c("d_impervious","d_treedensity","d_agri",
                                               "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                               "d_shannon","shannon","drymatter","protectedarea_perc",
                                               "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                               "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                             min_site_number_per_species = 60,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- scales::rescale(poisson_df$count)
  
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
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model (gamm too resource consumming over the whole Europe)
    
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    #global_vif <- max(car::vif(lm(as.formula(formula_gam), data=poisson_df), type="predictor")$GVIF)
    
    #res_data <- merge(site_data,data.frame(siteID=poisson_df$siteID,year=poisson_df$year,res=residuals(global_mod,type="deviance")),by="siteID")
    #gam.check(global_mod)
    #ggplot(grid_eu_mainland_outline) +  geom_sf() + geom_sf(data=res_data,aes(col=res), size=0.5) + scale_color_gradient2() + theme_minimal()
    
    if(global_mod$converged){
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      if_fail <- rbind(matrix(NA,(nrow=length(col_names)+1),ncol=4),c(0,rep(0,3)))
      row.names(if_fail) <- c(col_names,"dev_exp","n_obs")
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=purrr::possibly(otherwise=if_fail,
                                                                      .f=function(x,min_site_number_per_species,poisson_df){
        
        if(nrow(x) >= min_site_number_per_species){
          
          poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
          
          if(length(table(poisson_df_i$area_sampled_m2)) > length(unique(poisson_df_i$scheme_code))){
            one_scheme_time_area <- 0 
            poisson_df_i$area_sampled_m2 <- scale(poisson_df_i$area_sampled_m2)
          }else{
            one_scheme_time_area <- 1
          }
          
          if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
            res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i,random=list(siteID=~1))
            result_i <- summary(res.poisson_i$gam)$p.table
            dev_exp <- summary(res.poisson_i$gam)$r.sq
            n_obs <- summary(res.poisson_i$gam)$n
            result_i <- as.matrix(result_i[grep("scheme_code|area_sampled_m2",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
            res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i,random=list(siteID=~1))
            result_i <- summary(res.poisson_i$gam)$p.table
            dev_exp <- summary(res.poisson_i$gam)$r.sq
            n_obs <- summary(res.poisson_i$gam)$n
            result_i <- as.matrix(result_i[grep("area_sampled_m2",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
            res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i,random=list(siteID=~1))
            result_i <- summary(res.poisson_i$gam)$p.table
            dev_exp <- summary(res.poisson_i$gam)$r.sq
            n_obs <- summary(res.poisson_i$gam)$n
            result_i <- as.matrix(result_i[grep("scheme_code",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
            res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i,random=list(siteID=~1))
            result_i <- summary(res.poisson_i$gam)$p.table
            dev_exp <- summary(res.poisson_i$gam)$r.sq
            n_obs <- summary(res.poisson_i$gam)$n
            result_i <- as.matrix(result_i[grep("no_",row.names(result_i),invert = TRUE),])
          }
          
          if(nrow(result_i) == length(col_names)){
            result_site <- result_i
          }else{
            row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
            row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
            result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
            result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
            result_i_complet <- as.matrix(result_i_complet[2:5])
            result_site <- result_i_complet
          }
          result_site <- rbind(result_site,c(dev_exp,rep(0,3)))
          result_site <- rbind(result_site,c(n_obs,rep(0,3)))
          
        }else{
          n_obs <- nrow(poisson_df[which(poisson_df$PLS == unique(x$PLS)),])
          result_site <- matrix(NA,(nrow=length(col_names)+1),ncol=4)
          result_site <- rbind(result_site,c(n_obs,rep(0,3)))
        }
        
        row.names(result_site) <- c(col_names,"dev_exp","n_obs")
        
        return(result_site)
      }),
      min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
      .progress="text")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1,])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4,])))
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4])))
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=(length(col_names)+2)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=(length(col_names)+2))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- gsub("X","",row.names(res.poisson_df))
        
      }
      
      global_mod_coef <- summary(global_mod)$p.table[grep("scheme_code|area_sampled_m2|time_effort|no_",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef <- rbind(global_mod_coef,c(summary(global_mod)$dev.expl,rep(0,3)),c(summary(global_mod)$n,rep(0,3)))
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
      global_mod_df$PLS <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
      names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
      res.poisson_df$PLS <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
    names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
    res.poisson_df$PLS <- NA
  }
  
  return(res.poisson_df)
}



gam_species_PLS2 <- function(bird_data,pressure_data,site_data,
                             pressure_name = c("d_impervious","d_treedensity","d_agri",
                                               "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                               "d_shannon","shannon","drymatter","protectedarea_perc",
                                               "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                               "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                             min_site_number_per_species = 60,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - min(poisson_df$year)
  
  if(length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- poisson_df$count#scales::rescale(poisson_df$count)
  
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
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model (gamm too resource consumming over the whole Europe)
    
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    #global_vif <- max(car::vif(lm(as.formula(formula_gam), data=poisson_df), type="predictor")$GVIF)
    #res_data <- data.frame(data.frame(siteID=poisson_df$siteID,year=poisson_df$year,res=residuals(global_mod,type="deviance")) %>% group_by(siteID) %>% summarise(res=mean(res)))
    #res_data <- merge(site_data,res_data,by="siteID")
    #gam.check(global_mod)
    #ggplot(grid_eu_mainland_outline) +  geom_sf() + geom_sf(data=res_data,aes(col=res), size=0.5) + scale_color_gradient2() + theme_minimal()
    
    if(global_mod$converged){
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      if_fail <- rbind(matrix(NA,(nrow=length(col_names)+1),ncol=4),c(0,rep(0,3)))
      row.names(if_fail) <- c(col_names,"dev_exp","n_obs")
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=purrr::possibly(otherwise=if_fail,
                                                                             .f=function(x,min_site_number_per_species,poisson_df){
                                                                               
                                                                               if(nrow(x) >= min_site_number_per_species){
                                                                                 
                                                                                 poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
                                                                                 #poisson_df_i$year <- poisson_df_i$year - min(poisson_df_i$year)
                                                                                 
                                                                                 if(length(table(poisson_df_i$area_sampled_m2)) > length(unique(poisson_df_i$scheme_code))){
                                                                                   one_scheme_time_area <- 0 
                                                                                   poisson_df_i$area_sampled_m2 <- scale(poisson_df_i$area_sampled_m2)
                                                                                 }else{
                                                                                   one_scheme_time_area <- 1
                                                                                 }
                                                                                 
                                                                                 if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(siteID=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("scheme_code|area_sampled_m2",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(siteID=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("area_sampled_m2",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(siteID=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("scheme_code",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(siteID=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("no_",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 
                                                                                 if(nrow(result_i) == length(col_names)){
                                                                                   result_site <- result_i
                                                                                 }else{
                                                                                   row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
                                                                                   row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
                                                                                   result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
                                                                                   result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
                                                                                   result_i_complet <- as.matrix(result_i_complet[2:5])
                                                                                   result_site <- result_i_complet
                                                                                 }
                                                                                 result_site <- rbind(result_site,c(dev_exp,rep(0,3)))
                                                                                 result_site <- rbind(result_site,c(n_obs,rep(0,3)))
                                                                                 
                                                                               }else{
                                                                                 n_obs <- nrow(poisson_df[which(poisson_df$PLS == unique(x$PLS)),])
                                                                                 result_site <- matrix(NA,(nrow=length(col_names)+1),ncol=4)
                                                                                 result_site <- rbind(result_site,c(n_obs,rep(0,3)))
                                                                               }
                                                                               
                                                                               row.names(result_site) <- c(col_names,"dev_exp","n_obs")
                                                                               
                                                                               return(result_site)
                                                                             }),
                               min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
                               .progress="none")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1,])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4,])))
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4])))
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=(length(col_names)+2)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=(length(col_names)+2))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- gsub("X","",row.names(res.poisson_df))
        
      }
      
      global_mod_coef <- summary(global_mod)$p.table[grep("scheme_code|area_sampled_m2|time_effort|no_",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef <- rbind(global_mod_coef,c(summary(global_mod)$dev.expl,rep(0,3)),c(summary(global_mod)$n,rep(0,3)))
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
      global_mod_df$PLS <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
      names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
      res.poisson_df$PLS <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
    names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
    res.poisson_df$PLS <- NA
  }
  
  return(res.poisson_df)
}


gam_species_PLS3 <- function(bird_data,pressure_data,site_data,
                             pressure_name = c("d_impervious","d_treedensity","d_agri",
                                               "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                               "d_shannon","shannon","drymatter","protectedarea_perc","protectedarea_type",
                                               "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                               "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                             min_site_number_per_species = 60,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- scales::rescale(poisson_df$count)
  
  if(length(pressure_name) > 1){
    formula_gam <- "count_scale_all ~ year + year:d_impervious + year:d_treedensity:eulandsystem_forest_lowmedium + year:d_treedensity:eulandsystem_forest_high +
    year:d_agri:eulandsystem_farmland_low + year:d_agri:eulandsystem_farmland_medium + year:d_agri:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc + year:protectedarea_perc:protectedarea_type +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
  }else{
    formula_gam <- paste("count_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
  }
  
  col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
                 "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_tempsrping",
                 "year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc",
                 "year:d_treedensity:eulandsystem_forest_lowmedium","year:d_treedensity:eulandsystem_forest_high",
                 "year:d_agri:eulandsystem_farmland_low","year:d_agri:eulandsystem_farmland_medium",
                 "year:d_agri:eulandsystem_farmland_high","year:protectedarea_perc:protectedarea_type")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    #global_vif <- max(car::vif(lm(as.formula(formula_gam), data=poisson_df), type="predictor")$GVIF)
    
    #res_data <- merge(site_data,data.frame(siteID=poisson_df$siteID,year=poisson_df$year,res=residuals(global_mod,type="deviance")),by="siteID")
    #gam.check(global_mod)
    #ggplot(grid_eu_mainland_outline) +  geom_sf() + geom_sf(data=res_data,aes(col=res), size=0.5) + scale_color_gradient2() + theme_minimal()
    
    if(global_mod$converged){
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=function(x,min_site_number_per_species,poisson_df){
        
        if(nrow(x) >= min_site_number_per_species){
          
          poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
          
          if(length(table(poisson_df_i$area_sampled_m2)) > length(unique(poisson_df_i$scheme_code))){
            one_scheme_time_area <- 0 
            poisson_df_i$area_sampled_m2 <- scale(poisson_df_i$area_sampled_m2)
          }else{
            one_scheme_time_area <- 1
          }
          
          if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("scheme_code|area_sampled_m2",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("area_sampled_m2",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("scheme_code",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("no_",row.names(result_i),invert = TRUE),])
          }
          
          if(nrow(result_i) == length(col_names)){
            result_site <- result_i
          }else{
            row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
            row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
            result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
            result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
            result_i_complet <- as.matrix(result_i_complet[2:5])
            result_site <- result_i_complet
          }
          result_site <- rbind(result_site,c(dev_exp,rep(0,3)))
          result_site <- rbind(result_site,c(n_obs,rep(0,3)))
          
        }else{
          n_obs <- nrow(poisson_df[which(poisson_df$PLS == unique(x$PLS)),])
          result_site <- matrix(NA,(nrow=length(col_names)+1),ncol=4)
          result_site <- rbind(result_site,c(n_obs,rep(0,3)))
        }
        
        row.names(result_site) <- c(col_names,"dev_exp","n_obs")
        
        return(result_site)
      },
      min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
      .progress="text")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1,])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4,])))
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4])))
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=(length(col_names)+2)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=(length(col_names)+2))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- gsub("X","",row.names(res.poisson_df))
        
      }
      
      global_mod_coef <- summary(global_mod)$p.table[grep("scheme_code|area_sampled_m2|time_effort|no_",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef <- rbind(global_mod_coef,c(summary(global_mod)$dev.expl,rep(0,3)),c(summary(global_mod)$n,rep(0,3)))
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
      global_mod_df$PLS <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
      names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
      res.poisson_df$PLS <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
    names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
    res.poisson_df$PLS <- NA
  }
  
  return(res.poisson_df)
}


gam_species_PLS2_check <- function(bird_data,pressure_data,site_data,
                             pressure_name = c("d_impervious","d_treedensity","d_agri",
                                               "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                               "d_shannon","shannon","drymatter","protectedarea_perc",
                                               "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                               "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                             min_site_number_per_species = 60,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - min(poisson_df$year)
  
  if(length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- poisson_df$count#scales::rescale(poisson_df$count)
  
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
  
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model (gamm too resource consumming over the whole Europe)
    
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    

    fv <- predict(global_mod, type = "response")
    res_data <- data.frame(data.frame(siteID=poisson_df$siteID,year=poisson_df$year,res=residuals(global_mod,type="deviance")) %>% group_by(siteID) %>% summarise(res=mean(res)))
    res_data <- merge(site_data,res_data,by="siteID")
    
    p1 <- ggplot(data.frame(x = as.numeric(fv), y=napredict(global_mod$na.action, global_mod$y)), aes(x,y)) + geom_point() + theme_minimal() + labs(x = "Fitted Values", y = paste0("Response ",unique(bird_data$sci_name_out)), title = "Response vs. Fitted Values")
    
    p2 <- ggplot(data.frame(y=residuals(global_mod, type = "deviance")), aes(sample = y)) + stat_qq() + theme_minimal() + labs(x="Theoretical quantile",y="deviance residuals",title = "Normal Q-Q plot")
    
    p3 <- ggplot(grid_eu_mainland_outline) +  geom_sf() + geom_sf(data=res_data,aes(col=res), size=0.5) + scale_color_gradient2() + theme_void() + theme(legend.position = c(0.1,0.8), legend.title = element_blank())
    
    figure <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
  }
  
  return(figure)
  
}


gam_species_year <- function(bird_data,pressure_data,site_data,
                             min_site_number_per_species = 60,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   "PLS")])
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - min(poisson_df$year)
  
  if(length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- poisson_df$count#scales::rescale(poisson_df$count)
  
  formula_gam <- "count_scale_all ~ year"
  
  col_names <- c("(Intercept)","year")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model (gamm too resource consumming over the whole Europe)
    
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    

      
      global_mod_coef <- summary(global_mod)$p.table[grep("scheme_code|area_sampled_m2|time_effort|no_",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
      global_mod_coef <- rbind(global_mod_coef,c(summary(global_mod)$dev.expl,rep(0,3)),c(summary(global_mod)$n,rep(0,3)))
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
      global_mod_df$PLS <- "europe"
    
  }else{
    global_mod_df <- data.frame(t(rep(NA,(length(col_names)+2))))
    names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
    global_mod_df$PLS <- NA
  }
  
  return(global_mod_df)
}


butterfly_data <- droplevels(subsite_data_mainland_trend_butterfly[which(subsite_data_mainland_trend_butterfly$species_name == "Aglais io"),])
pressure_data <- press_mainland_trend_butterfly_scale
site_data <- site_mainland_sf_reproj_butterfly
min_site_number_per_species <- 60
min_occurence_species <- 200
family <- "quasipoisson"
pressure_name <- c("impervious","treedensity","drymatter",
                   "tempspring","tempspringvar","precspring",
                   "protectedarea_perc","protectedarea_type","shannon",
                   "eulandsystem_cat","farmland")

gam_species_PLS1b <- function(butterfly_data,pressure_data,site_data,
                             pressure_name = c("d_impervious","d_treedensity","d_agri",
                                               "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                               "d_shannon","shannon","drymatter","protectedarea_perc",
                                               "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                               "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                             min_site_number_per_species = 60,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(butterfly_data, pressure_data[which(pressure_data$transect_id %in% unique(butterfly_data$transect_id) & pressure_data$year %in% unique(butterfly_data$year)),], by =c("transect_id","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("transect_id","count_corrected","year","transect_length","bms_id","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$transect_length)) > length(unique(poisson_df$bms_id))){
    one_scheme_time_area <- 0 
    poisson_df$transect_length <- scale(poisson_df$transect_length)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_corrected_scale_all <- scales::rescale(poisson_df$count_corrected)
  
  if(length(pressure_name) > 1){
    formula_gam <- "count_corrected_scale_all ~ year + year:d_impervious + year:d_treedensity:eulandsystem_forest_lowmedium + year:d_treedensity:eulandsystem_forest_high +
    year:d_agri:eulandsystem_farmland_low + year:d_agri:eulandsystem_farmland_medium + year:d_agri:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
  }else{
    formula_gam <- paste("count_corrected_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
  }
  
  col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
                 "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_tempsrping",
                 "year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc",
                 "year:d_treedensity:eulandsystem_forest_lowmedium","year:d_treedensity:eulandsystem_forest_high",
                 "year:d_agri:eulandsystem_farmland_low","year:d_agri:eulandsystem_farmland_medium",
                 "year:d_agri:eulandsystem_farmland_high")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model (gamm too resource consumming over the whole Europe)
    
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    #global_vif <- max(car::vif(lm(as.formula(formula_gam), data=poisson_df), type="predictor")$GVIF)
    
    #res_data <- merge(site_data,data.frame(transect_id=poisson_df$transect_id,year=poisson_df$year,res=residuals(global_mod,type="deviance")),by="transect_id")
    #gam.check(global_mod)
    #ggplot(grid_eu_mainland_outline) +  geom_sf() + geom_sf(data=res_data,aes(col=res), size=0.5) + scale_color_gradient2() + theme_minimal()
    
    if(global_mod$converged){
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      if_fail <- rbind(matrix(NA,(nrow=length(col_names)+1),ncol=4),c(0,rep(0,3)))
      row.names(if_fail) <- c(col_names,"dev_exp","n_obs")
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=purrr::possibly(otherwise=if_fail,
                                                                             .f=function(x,min_site_number_per_species,poisson_df){
                                                                               
                                                                               if(nrow(x) >= min_site_number_per_species){
                                                                                 
                                                                                 poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
                                                                                 
                                                                                 if(length(table(poisson_df_i$transect_length)) > length(unique(poisson_df_i$bms_id))){
                                                                                   one_scheme_time_area <- 0 
                                                                                   poisson_df_i$transect_length <- scale(poisson_df_i$transect_length)
                                                                                 }else{
                                                                                   one_scheme_time_area <- 1
                                                                                 }
                                                                                 
                                                                                 if(length(unique(poisson_df_i$bms_id)) > 1 && one_scheme_time_area == 0){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("bms_id|transect_length",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$bms_id)) == 1 && one_scheme_time_area == 0){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("transect_length",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$bms_id)) > 1 && one_scheme_time_area == 1){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("bms_id",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$bms_id)) == 1 && one_scheme_time_area == 1){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("no_",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 
                                                                                 if(nrow(result_i) == length(col_names)){
                                                                                   result_site <- result_i
                                                                                 }else{
                                                                                   row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
                                                                                   row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
                                                                                   result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
                                                                                   result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
                                                                                   result_i_complet <- as.matrix(result_i_complet[2:5])
                                                                                   result_site <- result_i_complet
                                                                                 }
                                                                                 result_site <- rbind(result_site,c(dev_exp,rep(0,3)))
                                                                                 result_site <- rbind(result_site,c(n_obs,rep(0,3)))
                                                                                 
                                                                               }else{
                                                                                 n_obs <- nrow(poisson_df[which(poisson_df$PLS == unique(x$PLS)),])
                                                                                 result_site <- matrix(NA,(nrow=length(col_names)+1),ncol=4)
                                                                                 result_site <- rbind(result_site,c(n_obs,rep(0,3)))
                                                                               }
                                                                               
                                                                               row.names(result_site) <- c(col_names,"dev_exp","n_obs")
                                                                               
                                                                               return(result_site)
                                                                             }),
                               min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
                               .progress="text")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1,])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4,])))
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4])))
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=(length(col_names)+2)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=(length(col_names)+2))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- gsub("X","",row.names(res.poisson_df))
        
      }
      
      global_mod_coef <- summary(global_mod)$p.table[grep("bms_id|transect_length|time_effort|no_",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef <- rbind(global_mod_coef,c(summary(global_mod)$dev.expl,rep(0,3)),c(summary(global_mod)$n,rep(0,3)))
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
      global_mod_df$PLS <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
      names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
      res.poisson_df$PLS <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
    names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
    res.poisson_df$PLS <- NA
  }
  
  return(res.poisson_df)
}


gam_species_PLS2b <- function(butterfly_data,pressure_data,site_data,
                              pressure_name = c("d_impervious","d_treedensity","d_agri",
                                                "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                                "d_shannon","shannon","drymatter","protectedarea_perc",
                                                "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                                "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                              min_site_number_per_species = 60,
                              min_occurence_species=200,
                              family="quasipoisson"){
  
  species_press_data_year <- merge(butterfly_data, pressure_data[which(pressure_data$transect_id %in% unique(butterfly_data$transect_id) & pressure_data$year %in% unique(butterfly_data$year)),], by =c("transect_id","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("transect_id","count_corrected","year","transect_length","bms_id","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - min(poisson_df$year)
  
  if(length(table(poisson_df$transect_length)) > length(unique(poisson_df$bms_id))){
    one_scheme_time_area <- 0 
    poisson_df$transect_length <- scale(poisson_df$transect_length)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_corrected_scale_all <- poisson_df$count_corrected#scales::rescale(poisson_df$count_corrected)
  
  if(length(pressure_name) > 1){
    formula_gam <- "count_corrected_scale_all ~ year + year:d_impervious + year:d_treedensity +
    year:eulandsystem_forest_lowmedium + year:eulandsystem_forest_high +
    year:d_agri + year:eulandsystem_farmland_low + year:eulandsystem_farmland_medium + year:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
  }else{
    formula_gam <- paste("count_corrected_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
  }
  
  col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
                 "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
                 "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
                 "year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium","year:eulandsystem_farmland_high",
                 "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model (gamm too resource consumming over the whole Europe)
    
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    #global_vif <- max(car::vif(lm(as.formula(formula_gam), data=poisson_df), type="predictor")$GVIF)
    
    #res_data <- merge(site_data,data.frame(transect_id=poisson_df$transect_id,year=poisson_df$year,res=residuals(global_mod,type="deviance")),by="transect_id")
    #gam.check(global_mod)
    #ggplot(grid_eu_mainland_outline) +  geom_sf() + geom_sf(data=res_data,aes(col=res), size=0.5) + scale_color_gradient2() + theme_minimal()
    
    if(global_mod$converged){
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      if_fail <- rbind(matrix(NA,(nrow=length(col_names)+1),ncol=4),c(0,rep(0,3)))
      row.names(if_fail) <- c(col_names,"dev_exp","n_obs")
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=purrr::possibly(otherwise=if_fail,
                                                                             .f=function(x,min_site_number_per_species,poisson_df){
                                                                               
                                                                               if(nrow(x) >= min_site_number_per_species){
                                                                                 
                                                                                 poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
                                                                                 #poisson_df_i$year <- poisson_df_i$year - min(poisson_df_i$year)
                                                                                 
                                                                                 if(length(table(poisson_df_i$transect_length)) > length(unique(poisson_df_i$bms_id))){
                                                                                   one_scheme_time_area <- 0 
                                                                                   poisson_df_i$transect_length <- scale(poisson_df_i$transect_length)
                                                                                 }else{
                                                                                   one_scheme_time_area <- 1
                                                                                 }
                                                                                 
                                                                                 if(length(unique(poisson_df_i$bms_id)) > 1 && one_scheme_time_area == 0){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("bms_id|transect_length",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$bms_id)) == 1 && one_scheme_time_area == 0){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("transect_length",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$bms_id)) > 1 && one_scheme_time_area == 1){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("bms_id",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 if(length(unique(poisson_df_i$bms_id)) == 1 && one_scheme_time_area == 1){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   result_i <- summary(res.poisson_i$gam)$p.table
                                                                                   dev_exp <- summary(res.poisson_i$gam)$r.sq
                                                                                   n_obs <- summary(res.poisson_i$gam)$n
                                                                                   result_i <- as.matrix(result_i[grep("no_",row.names(result_i),invert = TRUE),])
                                                                                 }
                                                                                 
                                                                                 if(nrow(result_i) == length(col_names)){
                                                                                   result_site <- result_i
                                                                                 }else{
                                                                                   row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
                                                                                   row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
                                                                                   result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
                                                                                   result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
                                                                                   result_i_complet <- as.matrix(result_i_complet[2:5])
                                                                                   result_site <- result_i_complet
                                                                                 }
                                                                                 result_site <- rbind(result_site,c(dev_exp,rep(0,3)))
                                                                                 result_site <- rbind(result_site,c(n_obs,rep(0,3)))
                                                                                 
                                                                               }else{
                                                                                 n_obs <- nrow(poisson_df[which(poisson_df$PLS == unique(x$PLS)),])
                                                                                 result_site <- matrix(NA,(nrow=length(col_names)+1),ncol=4)
                                                                                 result_site <- rbind(result_site,c(n_obs,rep(0,3)))
                                                                               }
                                                                               
                                                                               row.names(result_site) <- c(col_names,"dev_exp","n_obs")
                                                                               
                                                                               return(result_site)
                                                                             }),
                               min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
                               .progress="none")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1,])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4,])))
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4])))
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=(length(col_names)+2)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=(length(col_names)+2))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- gsub("X","",row.names(res.poisson_df))
        
      }
      
      global_mod_coef <- summary(global_mod)$p.table[grep("bms_id|transect_length|time_effort|no_",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef <- rbind(global_mod_coef,c(summary(global_mod)$dev.expl,rep(0,3)),c(summary(global_mod)$n,rep(0,3)))
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
      global_mod_df$PLS <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
      names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
      res.poisson_df$PLS <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
    names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
    res.poisson_df$PLS <- NA
  }
  
  return(res.poisson_df)
}


gam_species_PLS2b_check <- function(butterfly_data,pressure_data,site_data,
                              pressure_name = c("d_impervious","d_treedensity","d_agri",
                                                "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                                "d_shannon","shannon","drymatter","protectedarea_perc",
                                                "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                                "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                              min_site_number_per_species = 60,
                              min_occurence_species=200,
                              family="quasipoisson"){
  
  species_press_data_year <- merge(butterfly_data, pressure_data[which(pressure_data$transect_id %in% unique(butterfly_data$transect_id) & pressure_data$year %in% unique(butterfly_data$year)),], by =c("transect_id","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("transect_id","count_corrected","year","transect_length","bms_id","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - min(poisson_df$year)
  
  if(length(table(poisson_df$transect_length)) > length(unique(poisson_df$bms_id))){
    one_scheme_time_area <- 0 
    poisson_df$transect_length <- scale(poisson_df$transect_length)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_corrected_scale_all <- poisson_df$count_corrected#scales::rescale(poisson_df$count_corrected)
  
  if(length(pressure_name) > 1){
    formula_gam <- "count_corrected_scale_all ~ year + year:d_impervious + year:d_treedensity +
    year:eulandsystem_forest_lowmedium + year:eulandsystem_forest_high +
    year:d_agri + year:eulandsystem_farmland_low + year:eulandsystem_farmland_medium + year:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
  }else{
    formula_gam <- paste("count_corrected_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
  }
  
  col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
                 "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
                 "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
                 "year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium","year:eulandsystem_farmland_high",
                 "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model (gamm too resource consumming over the whole Europe)
    
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    
    fv <- predict(global_mod, type = "response")
    res_data <- data.frame(data.frame(transect_id=poisson_df$transect_id,year=poisson_df$year,res=residuals(global_mod,type="deviance")) %>% group_by(transect_id) %>% summarise(res=mean(res)))
    res_data <- merge(site_data,res_data,by="transect_id")
    
    p1 <- ggplot(data.frame(x = as.numeric(fv), y=napredict(global_mod$na.action, global_mod$y)), aes(x,y)) + geom_point() + theme_minimal() + labs(x = "Fitted Values", y = paste0("Response ",unique(butterfly_data$species_name)), title = "Response vs. Fitted Values")
    
    p2 <- ggplot(data.frame(y=residuals(global_mod, type = "deviance")), aes(sample = y)) + stat_qq() + theme_minimal() + labs(x="Theoretical quantile",y="deviance residuals",title = "Normal Q-Q plot")
    
    p3 <- ggplot(grid_eu_mainland_outline) +  geom_sf() + geom_sf(data=res_data,aes(col=res), size=0.5) + scale_color_gradient2() + theme_void() + theme(legend.position = c(0.1,0.8), legend.title = element_blank())
    
    figure <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
  }
  
  return(figure)
  
}



gam_species_PLS3b <- function(butterfly_data,pressure_data,site_data,
                             pressure_name = c("d_impervious","d_treedensity","d_agri",
                                               "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                               "d_shannon","shannon","drymatter","protectedarea_perc","protectedarea_type",
                                               "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                               "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                             min_site_number_per_species = 60,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(butterfly_data, pressure_data[which(pressure_data$transect_id %in% unique(butterfly_data$transect_id) & pressure_data$year %in% unique(butterfly_data$year)),], by =c("transect_id","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("transect_id","count_corrected","year","transect_length","bms_id","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$transect_length)) > length(unique(poisson_df$bms_id))){
    one_scheme_time_area <- 0 
    poisson_df$transect_length <- scale(poisson_df$transect_length)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- scales::rescale(poisson_df$count_corrected)
  
  if(length(pressure_name) > 1){
    formula_gam <- "count_scale_all ~ year + year:d_impervious + year:d_treedensity:eulandsystem_forest_lowmedium + year:d_treedensity:eulandsystem_forest_high +
    year:d_agri:eulandsystem_farmland_low + year:d_agri:eulandsystem_farmland_medium + year:d_agri:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc + year:protectedarea_perc:protectedarea_type +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
  }else{
    formula_gam <- paste("count_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
  }
  
  col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
                 "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_tempsrping",
                 "year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc",
                 "year:d_treedensity:eulandsystem_forest_lowmedium","year:d_treedensity:eulandsystem_forest_high",
                 "year:d_agri:eulandsystem_farmland_low","year:d_agri:eulandsystem_farmland_medium",
                 "year:d_agri:eulandsystem_farmland_high","year:protectedarea_perc:protectedarea_type")
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    #res_data <- merge(site_data,data.frame(transect_id=poisson_df$transect_id,year=poisson_df$year,res=residuals(global_mod,type="deviance")),by="transect_id")
    #gam.check(global_mod)
    
    if(global_mod$converged){
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=function(x,min_site_number_per_species,poisson_df){
        
        if(nrow(x) >= min_site_number_per_species){
          
          poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
          
          if(length(table(poisson_df_i$transect_length)) > length(unique(poisson_df_i$bms_id))){
            one_scheme_time_area <- 0 
            poisson_df_i$transect_length <- scale(poisson_df_i$transect_length)
          }else{
            one_scheme_time_area <- 1
          }
          
          if(length(unique(poisson_df_i$bms_id)) > 1 && one_scheme_time_area == 0){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("bms_id|transect_length|no_",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$bms_id)) == 1 && one_scheme_time_area == 0){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("transect_length|no_",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$bms_id)) > 1 && one_scheme_time_area == 1){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("bms_id|no_",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$bms_id)) == 1 && one_scheme_time_area == 1){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("no_",row.names(result_i),invert = TRUE),])
          }
          
          if(nrow(result_i) == length(col_names)){
            result_site <- result_i
          }else{
            row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_i)))), ncol=1)
            row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_i)))]
            result_i_complet <- merge(result_i,row_to_add,by="row.names",all=TRUE)
            result_i_complet <- result_i_complet[match(col_names, result_i_complet$Row.names),]
            result_i_complet <- as.matrix(result_i_complet[2:5])
            result_site <- result_i_complet
          }
          result_site <- rbind(result_site,c(dev_exp,rep(0,3)))
          result_site <- rbind(result_site,c(n_obs,rep(0,3)))
          
        }else{
          n_obs <- nrow(poisson_df[which(poisson_df$PLS == unique(x$PLS)),])
          result_site <- matrix(NA,(nrow=length(col_names)+1),ncol=4)
          result_site <- rbind(result_site,c(n_obs,rep(0,3)))
        }
        
        row.names(result_site) <- c(col_names,"dev_exp","n_obs")
        
        return(result_site)
      },
      min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
      .progress="text")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1,])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4,])))
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),1])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:(length(col_names)+2),4])))
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=(length(col_names)+2)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=(length(col_names)+2))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- gsub("X","",row.names(res.poisson_df))
        
      }
      
      global_mod_coef <- summary(global_mod)$p.table[grep("bms_id|transect_length|no_",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef <- rbind(global_mod_coef,c(summary(global_mod)$dev.expl,rep(0,3)),c(summary(global_mod)$n,rep(0,3)))
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
      global_mod_df$PLS <- "europe"
      
      res.poisson_df <- rbind(res.poisson_df,global_mod_df)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,res.poisson_df,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
      names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
      res.poisson_df$PLS <- NA
    }
    
  }else{
    res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+2))))
    names(res.poisson_df) <- c(col_names,"dev_exp","n_obs")
    res.poisson_df$PLS <- NA
  }
  
  return(res.poisson_df)
}

######## Community analysis functions

#CXI_data=community_data_SXI;pressure_data=press_mainland_trend_scale;site_data=site_mainland_sf_reproj;min_site_number=40

lm_CXI_biogeo <- function(CXI_data,pressure_data,site_data,min_site_number=40){
  
  CXI_press_data_year <- merge(CXI_data, pressure_data[which(pressure_data$siteID %in% unique(CXI_data$siteID) & pressure_data$year %in% unique(CXI_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  model_df <- na.omit(CXI_press_data_year[,c("siteID","CTI","CSI","year","time_effort","area_sampled_m2","scheme_code",
                                             "Long_LAEA","Lat_LAEA","pop","impervious","treedensity","lightpollution",
                                             "woodprod","drymatter","tempspring","tempspringvar",  
                                             "precspring","precspringvar","humidityspring",
                                             "protectedarea","pesticide_nodu","smallwoodyfeatures",
                                             "fragmentation","shannon","eulandsystem_cat","biogeo_area")])
  model_df$year <- model_df$year - 2000
  
  model_df$CTI_scale_all <- scales::rescale(model_df$CTI)
  model_df$CSI_scale_all <- scales::rescale(model_df$CSI)
  
  col_names <- c("(Intercept)","year","year:treedensity","year:impervious","year:pop", 
                 "year:lightpollution","year:woodprod","year:drymatter","year:tempspring",
                 "year:tempspringvar","year:precspring","year:precspringvar","year:humidityspring","year:protectedarea",
                 "year:pesticide_nodu","year:smallwoodyfeatures","year:fragmentation","year:shannon",
                 "year:eulandsystem_catlow_intensity","year:eulandsystem_catmedium_intensity",
                 "year:eulandsystem_cathigh_intensity")
  
  ### global model
  
  global_mod_CTI <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                         year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                         year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                         year:shannon + year:eulandsystem_cat + time_effort + area_sampled_m2 + scheme_code, data=model_df)
  global_mod_CSI <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                         year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                         year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                         year:shannon + year:eulandsystem_cat + time_effort + area_sampled_m2 + scheme_code, data=model_df)
  
  model_sf <- SpatialPointsDataFrame(coords = as.matrix(model_df[,c("Long_LAEA","Lat_LAEA")]), data = model_df,
                                     proj4string = CRS(crs(site_data)))
  
  unique_model_df <- distinct(model_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
  
  unique_model_sf <- SpatialPointsDataFrame(coords = as.matrix(unique_model_df[,c("Long_LAEA","Lat_LAEA")]), data = unique_model_df,
                                            proj4string = CRS(crs(site_data)))
  
  result_all_site <- ddply(unique_model_df,.(biogeo_area),.fun=function(x,min_site_number,model_df){
    
    if(nrow(x) >= min_site_number){
      
      model_df_i <- model_df[which(model_df$biogeo_area == unique(x$biogeo_area)),]
      
      if(length(table(model_df_i$time_effort)) > length(unique(model_df_i$scheme_code)) & length(table(model_df_i$area_sampled_m2)) > length(unique(model_df_i$scheme_code))){
        one_scheme_time_area <- 0 
        model_df_i$time_effort <- scale(model_df_i$time_effort)
        model_df_i$area_sampled_m2 <- scale(model_df_i$area_sampled_m2)
      }else{
        one_scheme_time_area <- 1
      }
      
      if(length(unique(model_df_i$eulandsystem_cat)) > 1){
        if(length(unique(model_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
          mod_CTI_i <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat + time_effort + area_sampled_m2 + scheme_code, data=model_df_i)
          mod_CSI_i <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat + time_effort + area_sampled_m2 + scheme_code, data=model_df_i)
          result_CTI_i <- summary(mod_CTI_i)$coefficients
          result_CTI_i <- result_CTI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CTI_i),invert = TRUE),]
          result_CSI_i <- summary(mod_CSI_i)$coefficients
          result_CSI_i <- result_CSI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CSI_i),invert = TRUE),]
        }
        if(length(unique(model_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
          mod_CTI_i <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat + time_effort + area_sampled_m2, data=model_df_i)
          mod_CSI_i <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat + time_effort + area_sampled_m2, data=model_df_i)
          result_CTI_i <- summary(mod_CTI_i)$coefficients
          result_CTI_i <- result_CTI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CTI_i),invert = TRUE),]
          result_CSI_i <- summary(mod_CSI_i)$coefficients
          result_CSI_i <- result_CSI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CSI_i),invert = TRUE),]
        }
        if(length(unique(model_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
          mod_CTI_i <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat  + scheme_code, data=model_df_i)
          mod_CSI_i <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat + scheme_code, data=model_df_i)
          result_CTI_i <- summary(mod_CTI_i)$coefficients
          result_CTI_i <- result_CTI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CTI_i),invert = TRUE),]
          result_CSI_i <- summary(mod_CSI_i)$coefficients
          result_CSI_i <- result_CSI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CSI_i),invert = TRUE),]
        }
        if(length(unique(model_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
          mod_CTI_i <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat, data=model_df_i)
          mod_CSI_i <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + year:eulandsystem_cat, data=model_df_i)
          result_CTI_i <- summary(mod_CTI_i)$coefficients
          result_CTI_i <- result_CTI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CTI_i),invert = TRUE),]
          result_CSI_i <- summary(mod_CSI_i)$coefficients
          result_CSI_i <- result_CSI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CSI_i),invert = TRUE),]
        }
      }else{
        if(length(unique(model_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
          mod_CTI_i <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + time_effort + area_sampled_m2 + scheme_code, data=model_df_i)
          mod_CSI_i <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + time_effort + area_sampled_m2 + scheme_code, data=model_df_i)
          result_CTI_i <- summary(mod_CTI_i)$coefficients
          result_CTI_i <- result_CTI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CTI_i),invert = TRUE),]
          result_CSI_i <- summary(mod_CSI_i)$coefficients
          result_CSI_i <- result_CSI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CSI_i),invert = TRUE),]
        }
        if(length(unique(model_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
          mod_CTI_i <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + time_effort + area_sampled_m2, data=model_df_i)
          mod_CSI_i <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + time_effort + area_sampled_m2, data=model_df_i)
          result_CTI_i <- summary(mod_CTI_i)$coefficients
          result_CTI_i <- result_CTI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CTI_i),invert = TRUE),]
          result_CSI_i <- summary(mod_CSI_i)$coefficients
          result_CSI_i <- result_CSI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CSI_i),invert = TRUE),]
        }
        if(length(unique(model_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
          mod_CTI_i <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + scheme_code, data=model_df_i)
          mod_CSI_i <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon + scheme_code, data=model_df_i)
          result_CTI_i <- summary(mod_CTI_i)$coefficients
          result_CTI_i <- result_CTI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CTI_i),invert = TRUE),]
          result_CSI_i <- summary(mod_CSI_i)$coefficients
          result_CSI_i <- result_CSI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CSI_i),invert = TRUE),]
        }
        if(length(unique(model_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
          mod_CTI_i <- lm(CTI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon, data=model_df_i)
          mod_CSI_i <- lm(CSI_scale_all~year + year:treedensity+year:impervious+year:pop+year:lightpollution+year:woodprod+
                            year:drymatter+year:tempspring+year:tempspringvar+year:precspring+year:precspringvar+year:humidityspring+
                            year:protectedarea+year:pesticide_nodu+year:smallwoodyfeatures+year:fragmentation+
                            year:shannon, data=model_df_i)
          result_CTI_i <- summary(mod_CTI_i)$coefficients
          result_CTI_i <- result_CTI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CTI_i),invert = TRUE),]
          result_CSI_i <- summary(mod_CSI_i)$coefficients
          result_CSI_i <- result_CSI_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_CSI_i),invert = TRUE),]
        }
      }
      
      if(nrow(result_CTI_i) == length(col_names)){
        result_site_CTI <- result_CTI_i
      }else{
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_CTI_i)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_CTI_i)))]
        result_CTI_i_complet <- merge(result_CTI_i,row_to_add,by="row.names",all=TRUE)
        result_CTI_i_complet <- result_CTI_i_complet[match(col_names, result_CTI_i_complet$Row.names),]
        result_CTI_i_complet <- as.matrix(result_CTI_i_complet[2:5])
        result_site_CTI <- result_CTI_i_complet
      }
      
      if(nrow(result_CSI_i) == length(col_names)){
        result_site_CSI <- result_CSI_i
      }else{
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(result_CSI_i)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(result_CSI_i)))]
        result_CSI_i_complet <- merge(result_CSI_i,row_to_add,by="row.names",all=TRUE)
        result_CSI_i_complet <- result_CSI_i_complet[match(col_names, result_CSI_i_complet$Row.names),]
        result_CSI_i_complet <- as.matrix(result_CSI_i_complet[2:5])
        result_site_CSI <- result_CSI_i_complet
      }
      
    }else{
      result_site_CSI <- result_site_CTI <- matrix(NA,nrow=length(col_names),ncol=4)
    }
    
    result_site <- rbind(data.frame(result_CTI_i,cxi="CTI"),data.frame(result_CSI_i,cxi="CSI"))
    
    result_site$variable <- col_names
    
    return(result_site)
  },
  min_site_number=min_site_number,model_df=model_df,
  .progress="text")
  
  global_mod_coef_CTI <- summary(global_mod_CTI)$coefficient[grep("scheme_code|area_sampled_m2|time_effort",row.names(summary(global_mod_CTI)$coefficient),invert = TRUE),]
  global_mod_coef_CSI <- summary(global_mod_CSI)$coefficient[grep("scheme_code|area_sampled_m2|time_effort",row.names(summary(global_mod_CSI)$coefficient),invert = TRUE),]
  
  result_site_all <- rbind(data.frame(global_mod_coef_CTI,cxi="CTI"),data.frame(global_mod_coef_CSI,cxi="CSI"))
  
  result_site_all$variable <- col_names
  
  result_site_all$biogeo_area <- "europe"
  
  res.model_df <- rbind(result_all_site,result_site_all)
  
  #res.model_df2 <- dcast(res.model_df[,c("Estimate","biogeo_area","cxi","variable")], biogeo_area + cxi ~ variable, value.var = "Estimate")
  #res.model_sf <- merge(grid_eu_mainland_biogeo,res.model_df2[which(res.model_df2$cxi=="CTI"),],by="biogeo_area")
  #ggplot() + geom_sf() +  geom_sf(data=res.model_sf, aes(fill=exp(`year:treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
  
  return(res.model_df)
  
}


predict_trend <- function(mod,
                          pressure_data_unscale,
                          poisson_df_unscale,
                          poisson_df,
                          lulc_pls_short,
                          climate_pls,
                          pa_pls_short,
                          PLS,
                          nb_rep=1000,
                          pressure_remove = NULL){
  
  mod_coef <- summary(mod)$p.table[grep("year",row.names(summary(mod)$p.table)),]
  
  if(!is.null(pressure_remove)){
    mod_coef[pressure_remove,c("Estimate","Std. Error")] <- 0
    pressure_removed <- c("year","d_impervious","d_tempsrping","d_tempsrpingvar","d_precspring",
    "d_shannon","protectedarea_perc","d_treedensity","eulandsystem_forest_lowmedium","eulandsystem_forest_high",
    "d_agri","eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high")[pressure_remove]
  }else{pressure_removed <- "none"}
  
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
  
  beta1_past <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*mean(poisson_df$d_impervious) +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*mean(poisson_df$d_tempsrping) +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*mean(poisson_df$d_tempsrpingvar) +
    mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*mean(poisson_df$d_precspring) +
    mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*mean(poisson_df$d_shannon) +
    mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*mean(poisson_df$protectedarea_perc) +
    mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*mean(poisson_df$d_treedensity) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*mean(poisson_df$eulandsystem_forest_lowmedium) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*mean(poisson_df$eulandsystem_forest_high) +
    mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*mean(poisson_df$d_agri) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_low) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_medium) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_high)
  
  beta1_past_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*mean(poisson_df$d_impervious) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*mean(poisson_df$d_tempsrping) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*mean(poisson_df$d_tempsrpingvar) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*mean(poisson_df$d_precspring) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*mean(poisson_df$d_shannon) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*mean(poisson_df$protectedarea_perc) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*mean(poisson_df$d_treedensity) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_lowmedium) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_high) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*mean(poisson_df$d_agri) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_low) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_medium) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_high)
  
  
  d_impervious_ssp1 <- mean(poisson_df_unscale$impervious_2018)*
    (lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
  d_shannon_ssp1 <- mean(poisson_df_unscale$shannon_2018)*
    (lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
  d_tempspring_ssp1 <- mean(poisson_df_unscale$tempspring_2020)*
    (climate_pls$mean_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$mean_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_tempspringvar_ssp1 <- mean(poisson_df_unscale$tempspringvar_2020)*
    (climate_pls$var_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$var_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_precspring_ssp1 <- mean(poisson_df_unscale$precspring_2020)*
    (climate_pls$sum_p_4_5[which(climate_pls$PLS==PLS)]/climate_pls$sum_p_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_agri_ssp1 <- mean(poisson_df_unscale$agri_2018)*
    (sum(lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
  agri_low_ssp1 <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]
  agri_medium_ssp1 <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]
  agri_high_ssp1 <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]
  d_treedensity_ssp1 <- mean(poisson_df_unscale$treedensity_2018)*
    (sum(lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
  forest_lowmedium_ssp1 <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]
  forest_high_ssp1 <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$ssp1[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]
  protectedarea_perc_ssp1 <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$ssp1[which(pa_pls_short$PLS==PLS)]/pa_pls_short$initial[which(pa_pls_short$PLS==PLS)]
  
  beta1_SSP1 <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_ssp1 - d_impervious_mu)/d_impervious_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_ssp1 - d_shannon_mu)/d_shannon_si +
    mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_ssp1 - d_treedensity_mu)/d_treedensity_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_ssp1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_ssp1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_ssp1 - d_agri_mu)/d_agri_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_ssp1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_ssp1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_ssp1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_SSP1_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_ssp1 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_ssp1 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_ssp1 - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_ssp1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_ssp1 +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_ssp1 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_ssp1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_ssp1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_ssp1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  beta1_BAU <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*mean(poisson_df$d_impervious) +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*mean(poisson_df$d_shannon) +
    mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*mean(poisson_df$d_treedensity) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*mean(poisson_df$eulandsystem_forest_lowmedium) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*mean(poisson_df$eulandsystem_forest_high) +
    mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*mean(poisson_df$d_agri) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_low) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_medium) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_high)
  
  beta1_BAU_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*mean(poisson_df$d_impervious) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*mean(poisson_df$d_shannon) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*mean(poisson_df$d_treedensity) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_lowmedium) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_high) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*mean(poisson_df$d_agri) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_low) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_medium) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_high)
  
  
  
  d_impervious_ssp3 <- mean(poisson_df_unscale$impervious_2018)*
    (lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
  d_shannon_ssp3 <- mean(poisson_df_unscale$shannon_2018)*
    (lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
  d_tempspring_ssp3 <- mean(poisson_df_unscale$tempspring_2020)*
    (climate_pls$mean_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$mean_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_tempspringvar_ssp3 <- mean(poisson_df_unscale$tempspringvar_2020)*
    (climate_pls$var_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$var_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_precspring_ssp3 <- mean(poisson_df_unscale$precspring_2020)*
    (climate_pls$sum_p_4_5[which(climate_pls$PLS==PLS)]/climate_pls$sum_p_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_agri_ssp3 <- mean(poisson_df_unscale$agri_2018)*
    (sum(lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
  agri_low_ssp3 <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]
  agri_medium_ssp3 <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]
  agri_high_ssp3 <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]
  d_treedensity_ssp3 <- mean(poisson_df_unscale$treedensity_2018)*
    (sum(lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
  forest_lowmedium_ssp3 <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]
  forest_high_ssp3 <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$ssp3[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]
  protectedarea_perc_ssp3 <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$ssp3[which(pa_pls_short$PLS==PLS)]/pa_pls_short$initial[which(pa_pls_short$PLS==PLS)]
  
  beta1_SSP3 <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_ssp3 - d_impervious_mu)/d_impervious_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp3 - d_tempspring_mu)/d_tempspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp3 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp3 - d_precspring_mu)/d_precspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_ssp3 - d_shannon_mu)/d_shannon_si +
    mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp3 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_ssp3 - d_treedensity_mu)/d_treedensity_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_ssp3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_ssp3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_ssp3 - d_agri_mu)/d_agri_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_ssp3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_ssp3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_ssp3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_SSP3_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_ssp3 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp3 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp3 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp3 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_ssp3 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp3 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_ssp3 - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_ssp3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_ssp3 +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_ssp3 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_ssp3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_ssp3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_ssp3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  d_impervious_nac <- mean(poisson_df_unscale$impervious_2018)*
    (lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
  d_shannon_nac <- mean(poisson_df_unscale$shannon_2018)*
    (lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
  d_tempspring_nac <- mean(poisson_df_unscale$tempspring_2020)*
    (climate_pls$mean_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$mean_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_tempspringvar_nac <- mean(poisson_df_unscale$tempspringvar_2020)*
    (climate_pls$var_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$var_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_precspring_nac <- mean(poisson_df_unscale$precspring_2020)*
    (climate_pls$sum_p_4_5[which(climate_pls$PLS==PLS)]/climate_pls$sum_p_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_agri_nac <- mean(poisson_df_unscale$agri_2018)*
    (sum(lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
  agri_low_nac <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]
  agri_medium_nac <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]
  agri_high_nac <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]
  d_treedensity_nac <- mean(poisson_df_unscale$treedensity_2018)*
    (sum(lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
  forest_lowmedium_nac <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]
  forest_high_nac <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$nac[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]
  protectedarea_perc_nac <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$nac[which(pa_pls_short$PLS==PLS)]/pa_pls_short$initial[which(pa_pls_short$PLS==PLS)]
  
  beta1_nac <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_nac - d_impervious_mu)/d_impervious_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nac - d_tempspring_mu)/d_tempspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nac - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_nac - d_precspring_mu)/d_precspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_nac - d_shannon_mu)/d_shannon_si +
    mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nac - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nac - d_treedensity_mu)/d_treedensity_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nac - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nac - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_nac - d_agri_mu)/d_agri_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nac - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nac - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nac - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nac_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_nac - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nac - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nac - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_nac - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_nac - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nac - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nac - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nac - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nac +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_nac - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nac - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nac - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nac - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  d_impervious_nfn <- mean(poisson_df_unscale$impervious_2018)*
    (lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
  d_shannon_nfn <- mean(poisson_df_unscale$shannon_2018)*
    (lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
  d_tempspring_nfn <- mean(poisson_df_unscale$tempspring_2020)*
    (climate_pls$mean_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$mean_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_tempspringvar_nfn <- mean(poisson_df_unscale$tempspringvar_2020)*
    (climate_pls$var_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$var_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_precspring_nfn <- mean(poisson_df_unscale$precspring_2020)*
    (climate_pls$sum_p_4_5[which(climate_pls$PLS==PLS)]/climate_pls$sum_p_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_agri_nfn <- mean(poisson_df_unscale$agri_2018)*
    (sum(lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
  agri_low_nfn <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]
  agri_medium_nfn <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]
  agri_high_nfn <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]
  d_treedensity_nfn <- mean(poisson_df_unscale$treedensity_2018)*
    (sum(lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
  forest_lowmedium_nfn <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]
  forest_high_nfn <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$nfn[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]
  protectedarea_perc_nfn <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$nfn[which(pa_pls_short$PLS==PLS)]/pa_pls_short$initial[which(pa_pls_short$PLS==PLS)]
  
  beta1_nfn <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_nfn - d_impervious_mu)/d_impervious_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nfn - d_tempspring_mu)/d_tempspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nfn - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_nfn - d_precspring_mu)/d_precspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_nfn - d_shannon_mu)/d_shannon_si +
    mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nfn - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nfn - d_treedensity_mu)/d_treedensity_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nfn - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nfn - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_nfn - d_agri_mu)/d_agri_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nfn - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nfn - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nfn - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nfn_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_nfn - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nfn - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nfn - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_nfn - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_nfn - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nfn - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nfn - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nfn - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nfn +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_nfn - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nfn - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nfn - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nfn - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  d_impervious_nfs <- mean(poisson_df_unscale$impervious_2018)*
    (lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("urban"))]-1)/(2050-2018) 
  d_shannon_nfs <- mean(poisson_df_unscale$shannon_2018)*
    (lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("landscape_div"))]-1)/(2050-2018) 
  d_tempspring_nfs <- mean(poisson_df_unscale$tempspring_2020)*
    (climate_pls$mean_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$mean_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_tempspringvar_nfs <- mean(poisson_df_unscale$tempspringvar_2020)*
    (climate_pls$var_t_4_5[which(climate_pls$PLS==PLS)]/climate_pls$var_t_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_precspring_nfs <- mean(poisson_df_unscale$precspring_2020)*
    (climate_pls$sum_p_4_5[which(climate_pls$PLS==PLS)]/climate_pls$sum_p_2016[which(climate_pls$PLS==PLS)]-1)/(2050-2018)
  d_agri_nfs <- mean(poisson_df_unscale$agri_2018)*
    (sum(lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low","farmland_medium","farmland_high"))])-1)/(2050-2018)
  agri_low_nfs <- mean(poisson_df_unscale$eulandsystem_farmland_low)*lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_low"))]
  agri_medium_nfs <- mean(poisson_df_unscale$eulandsystem_farmland_medium)*lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_medium"))]
  agri_high_nfs <- mean(poisson_df_unscale$eulandsystem_farmland_high)*lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("farmland_high"))]
  d_treedensity_nfs <- mean(poisson_df_unscale$treedensity_2018)*
    (sum(lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])/sum(lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium","forest_high"))])-1)/(2050-2018)
  forest_lowmedium_nfs <- mean(poisson_df_unscale$eulandsystem_forest_lowmedium)*lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_lowmedium"))]
  forest_high_nfs <- mean(poisson_df_unscale$eulandsystem_forest_high)*lulc_pls_short$nfs[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]/lulc_pls_short$initial[which(lulc_pls_short$PLS==PLS & lulc_pls_short$variable %in% c("forest_high"))]
  protectedarea_perc_nfs <- mean(poisson_df_unscale$protectedarea_perc)*pa_pls_short$nfs[which(pa_pls_short$PLS==PLS)]/pa_pls_short$initial[which(pa_pls_short$PLS==PLS)]
  
  beta1_nfs <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*(d_impervious_nfs - d_impervious_mu)/d_impervious_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nfs - d_tempspring_mu)/d_tempspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nfs - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*(d_precspring_nfs - d_precspring_mu)/d_precspring_si +
    mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*(d_shannon_nfs - d_shannon_mu)/d_shannon_si +
    mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nfs - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nfs - d_treedensity_mu)/d_treedensity_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nfs - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nfs - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*(d_agri_nfs - d_agri_mu)/d_agri_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nfs - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nfs - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nfs - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nfs_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*(d_impervious_nfs - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nfs - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nfs - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*(d_precspring_nfs - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*(d_shannon_nfs - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nfs - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nfs - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nfs - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nfs +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*(d_agri_nfs - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nfs - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nfs - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nfs - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  mod_coef_signif <- mod_coef
  mod_coef_signif[which(mod_coef_signif[,c("Pr(>|t|)")] > 0.05),c("Estimate","Std. Error")] <- 0
  
  beta1_past_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*mean(poisson_df$d_impervious) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*mean(poisson_df$d_tempsrping) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*mean(poisson_df$d_tempsrpingvar) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*mean(poisson_df$d_precspring) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*mean(poisson_df$d_shannon) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*mean(poisson_df$protectedarea_perc) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*mean(poisson_df$d_treedensity) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*mean(poisson_df$eulandsystem_forest_lowmedium) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*mean(poisson_df$eulandsystem_forest_high) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*mean(poisson_df$d_agri) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_low) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_medium) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_high)
  
  beta1_past_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*mean(poisson_df$d_impervious) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*mean(poisson_df$d_tempsrping) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*mean(poisson_df$d_tempsrpingvar) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*mean(poisson_df$d_precspring) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*mean(poisson_df$d_shannon) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*mean(poisson_df$protectedarea_perc) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*mean(poisson_df$d_treedensity) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_lowmedium) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_high) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*mean(poisson_df$d_agri) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_low) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_medium) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_high)
  
  beta1_SSP1_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_ssp1 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_ssp1 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_ssp1 - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_ssp1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_ssp1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_ssp1 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_ssp1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_ssp1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_ssp1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_SSP1_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_ssp1 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_ssp1 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_ssp1 - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_ssp1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_ssp1 +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_ssp1 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_ssp1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_ssp1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_ssp1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  beta1_BAU_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*mean(poisson_df$d_impervious) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*mean(poisson_df$d_shannon) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*mean(poisson_df$d_treedensity) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*mean(poisson_df$eulandsystem_forest_lowmedium) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*mean(poisson_df$eulandsystem_forest_high) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*mean(poisson_df$d_agri) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_low) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_medium) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*mean(poisson_df$eulandsystem_farmland_high)
  
  beta1_BAU_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*mean(poisson_df$d_impervious) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp1 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp1 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp1 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*mean(poisson_df$d_shannon) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp1 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*mean(poisson_df$d_treedensity) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_lowmedium) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_high) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*mean(poisson_df$d_agri) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_low) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_medium) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*mean(poisson_df$eulandsystem_farmland_high)
  
  
  beta1_SSP3_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_ssp3 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_ssp3 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_ssp3 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_ssp3 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_ssp3 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_ssp3 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_ssp3 - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_ssp3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_ssp3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_ssp3 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_ssp3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_ssp3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_ssp3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_SSP3_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_ssp3 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_ssp3 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_ssp3 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_ssp3 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_ssp3 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_ssp3 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_ssp3 - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_ssp3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_ssp3 +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_ssp3 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_ssp3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_ssp3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_ssp3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nac_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_nac - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nac - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nac - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_nac - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_nac - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nac - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nac - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nac - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nac - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_nac - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nac - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nac - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nac - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nac_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_nac - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nac - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nac - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_nac - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_nac - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nac - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nac - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nac - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nac +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_nac - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nac - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nac - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nac - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nfn_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_nfn - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nfn - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nfn - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_nfn - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_nfn - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nfn - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nfn - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nfn - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nfn - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_nfn - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nfn - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nfn - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nfn - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nfn_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_nfn - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nfn - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nfn - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_nfn - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_nfn - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nfn - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nfn - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nfn - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nfn +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_nfn - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nfn - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nfn - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nfn - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nfs_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_nfs - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_nfs - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_nfs - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_nfs - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_nfs - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_nfs - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_nfs - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_nfs - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_nfs - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_nfs - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_nfs - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_nfs - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_nfs - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  beta1_nfs_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_nfs - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_nfs - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_nfs - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_nfs - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_nfs - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_nfs - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_nfs - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_nfs - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*forest_high_nfs +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_nfs - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_nfs - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_nfs - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_nfs - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  return(data.frame(intercept = summary(mod)$p.table[1,1], trend_past=beta1_past,sd_past=sd(beta1_past_sample), trend_BAU=beta1_BAU,sd_BAU=sd(beta1_BAU_sample),trend_SSP1=beta1_SSP1,sd_SSP1=sd(beta1_SSP1_sample),
                    trend_SSP3=beta1_SSP3,sd_SSP3=sd(beta1_SSP3_sample),trend_nac=beta1_nac,sd_nac=sd(beta1_nac_sample),
                    trend_nfn=beta1_nfn,sd_nfn=sd(beta1_nfn_sample),trend_nfs=beta1_nfs,sd_nfs=sd(beta1_nfs_sample),
                    trend_past_signif=beta1_past_signif,sd_past_signif=sd(beta1_past_sample_signif),
                    trend_BAU_signif=beta1_BAU_signif,sd_BAU_signif=sd(beta1_BAU_sample_signif),trend_SSP1_signif=beta1_SSP1_signif,sd_SSP1_signif=sd(beta1_SSP1_sample_signif),
                    trend_SSP3_signif=beta1_SSP3_signif,sd_SSP3_signif=sd(beta1_SSP3_sample_signif),trend_nac_signif=beta1_nac_signif,sd_nac_signif=sd(beta1_nac_sample_signif),
                    trend_nfn_signif=beta1_nfn_signif,sd_nfn_signif=sd(beta1_nfn_sample_signif),trend_nfs_signif=beta1_nfs_signif,sd_nfs_signif=sd(beta1_nfs_sample_signif),PLS=PLS, pressure_removed))
}

bird_data <- droplevels(subsite_data_mainland_trend[which(subsite_data_mainland_trend$sci_name_out == "Alauda arvensis"),])
pressure_data <- press_mainland_trend_scale
pressure_data_unscale <-  press_mainland_trend
site_data <- site_mainland_sf_reproj
min_site_number_per_species <- 60
min_occurence_species <- 200
family <- "quasipoisson"

predict_trend_bird <- function(bird_data,pressure_data,pressure_data_unscale,site_data,
                               lulc_pls_short,climate_pls,pa_pls_short,
                             pressure_name = c("d_impervious","d_treedensity","d_agri",
                                               "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                               "d_shannon","shannon","drymatter","protectedarea_perc",
                                               "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                               "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                             min_site_number_per_species = 60,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  species_press_data_year_unscale <- merge(bird_data, pressure_data_unscale[which(pressure_data_unscale$siteID %in% unique(bird_data$siteID) & pressure_data_unscale$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df_unscale <- na.omit(species_press_data_year_unscale[,c("siteID","count","year","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                                   pressure_name,"tempspring_2020","tempspringvar_2020","precspring_2020","agri_2018","shannon_2018","impervious_2018","treedensity_2018","PLS")])
  
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - 2000
  
  if(length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- poisson_df$count#scales::rescale(poisson_df$count)
  
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
  
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    if(global_mod$converged){
      
      predict_trend_europe <- predict_trend(mod=global_mod,
                                            pressure_data_unscale,
                                            poisson_df_unscale,
                                            poisson_df,
                                            lulc_pls_short,
                                            climate_pls,
                                            pa_pls_short,
                                            PLS="europe")
      
      for(i in 1:14){
        predict_trend_europe_temp <- predict_trend(mod=global_mod,
                                                   pressure_data_unscale,
                                                   poisson_df_unscale,
                                                   poisson_df,
                                                   lulc_pls_short,
                                                   climate_pls,
                                                   pa_pls_short,
                                                   PLS="europe",
                                                   pressure_remove = i)
        
        predict_trend_europe <- rbind(predict_trend_europe,predict_trend_europe_temp)
      }
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      if_fail <- data.frame(intercept=NA,trend_past=NA,sd_past=NA,trend_BAU=NA,sd_BAU=NA,trend_SSP1=NA,sd_SSP1=NA,
                            trend_SSP3=NA,sd_SSP3=NA,trend_nac=NA,sd_nac=NA,
                            trend_nfn=NA,sd_nfn=NA,trend_nfs=NA,sd_nfs=NA,
                            trend_past_signif=NA,sd_past_signif=NA,
                            trend_BAU_signif=NA,sd_BAU_signif=NA,trend_SSP1_signif=NA,sd_SSP1_signif=NA,
                            trend_SSP3_signif=NA,sd_SSP3_signif=NA,trend_nac_signif=NA,sd_nac_signif=NA,
                            trend_nfn_signif=NA,sd_nfn_signif=NA,trend_nfs_signif=NA,sd_nfs_signif=NA,
                            PLS=NA,pressure_removed=NA)
      
      predict_trend_pls <- ddply(unique_poisson_df,.(PLS),.fun=purrr::possibly(otherwise=if_fail,
                                                                             .f=function(x,min_site_number_per_species,poisson_df){
                                                                               
                                                                               if(nrow(x) >= min_site_number_per_species){
                                                                                 
                                                                                 poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
                                                                                 poisson_df_unscale_i <- poisson_df_unscale[which(poisson_df_unscale$PLS == unique(x$PLS)),]
                                                                                 
                                                                                 if(length(table(poisson_df_i$area_sampled_m2)) > length(unique(poisson_df_i$scheme_code))){
                                                                                   one_scheme_time_area <- 0 
                                                                                   poisson_df_i$area_sampled_m2 <- scale(poisson_df_i$area_sampled_m2)
                                                                                 }else{
                                                                                   one_scheme_time_area <- 1
                                                                                 }
                                                                                 
                                                                                 if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2:scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(siteID=~1))
                                                                                   }
                                                                                 if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(siteID=~1))
                                                                                   }
                                                                                 if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(siteID=~1))
                                                                                   }
                                                                                 if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
                                                                                   res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                         family=family, data=poisson_df_i,random=list(siteID=~1))
                                                                                   }
                                                                                 
                                                                                 predict_trend_i <- predict_trend(mod=res.poisson_i$gam,
                                                                                                                  pressure_data_unscale,
                                                                                                                  poisson_df_unscale_i,
                                                                                                                  poisson_df_i,
                                                                                                                  lulc_pls_short,
                                                                                                                  climate_pls,
                                                                                                                  pa_pls_short,
                                                                                                                  PLS=unique(x$PLS))
                                                                                 
                                                                               }else{
                                                                                 predict_trend_i <- data.frame(intercept=NA,trend_past=NA,sd_past=NA,trend_BAU=NA,sd_BAU=NA,trend_SSP1=NA,sd_SSP1=NA,
                                                                                                               trend_SSP3=NA,sd_SSP3=NA,trend_nac=NA,sd_nac=NA,
                                                                                                               trend_nfn=NA,sd_nfn=NA,trend_nfs=NA,sd_nfs=NA,
                                                                                                               trend_past_signif=NA,sd_past_signif=NA,
                                                                                                               trend_BAU_signif=NA,sd_BAU_signif=NA,trend_SSP1_signif=NA,sd_SSP1_signif=NA,
                                                                                                               trend_SSP3_signif=NA,sd_SSP3_signif=NA,trend_nac_signif=NA,sd_nac_signif=NA,
                                                                                                               trend_nfn_signif=NA,sd_nfn_signif=NA,trend_nfs_signif=NA,sd_nfs_signif=NA,
                                                                                                               PLS=unique(x$PLS),pressure_removed=NA)
                                                                               }
                                                                                 
                                                                               return(predict_trend_i)
                                                                             }),
                               min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
                               .progress="none")
      
      predict_trend_all <- rbind(predict_trend_pls,predict_trend_europe)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,predict_trend_all,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=trend_BAU)) + scale_fill_gradient2()
      
    }else{
      predict_trend_all <- data.frame(intercept=NA,trend_past=NA,sd_past=NA,trend_BAU=NA,sd_BAU=NA,trend_SSP1=NA,sd_SSP1=NA,
                                      trend_SSP3=NA,sd_SSP3=NA,trend_nac=NA,sd_nac=NA,
                                      trend_nfn=NA,sd_nfn=NA,trend_nfs=NA,sd_nfs=NA,
                                      trend_past_signif=NA,sd_past_signif=NA,
                                      trend_BAU_signif=NA,sd_BAU_signif=NA,trend_SSP1_signif=NA,sd_SSP1_signif=NA,
                                      trend_SSP3_signif=NA,sd_SSP3_signif=NA,trend_nac_signif=NA,sd_nac_signif=NA,
                                      trend_nfn_signif=NA,sd_nfn_signif=NA,trend_nfs_signif=NA,sd_nfs_signif=NA,
                                      PLS=NA,pressure_removed=NA)
    }
    
  }else{
    predict_trend_all <- data.frame(intercept=NA,trend_past=NA,sd_past=NA,trend_BAU=NA,sd_BAU=NA,trend_SSP1=NA,sd_SSP1=NA,
                                    trend_SSP3=NA,sd_SSP3=NA,trend_nac=NA,sd_nac=NA,
                                    trend_nfn=NA,sd_nfn=NA,trend_nfs=NA,sd_nfs=NA,
                                    trend_past_signif=NA,sd_past_signif=NA,
                                    trend_BAU_signif=NA,sd_BAU_signif=NA,trend_SSP1_signif=NA,sd_SSP1_signif=NA,
                                    trend_SSP3_signif=NA,sd_SSP3_signif=NA,trend_nac_signif=NA,sd_nac_signif=NA,
                                    trend_nfn_signif=NA,sd_nfn_signif=NA,trend_nfs_signif=NA,sd_nfs_signif=NA,
                                    PLS=NA,pressure_removed=NA)
  }
  
  return(predict_trend_all)
}




butterfly_data <- droplevels(subsite_data_mainland_trend_butterfly[which(subsite_data_mainland_trend_butterfly$species_name == "Aglais io"),])
pressure_data <- press_mainland_trend_butterfly_scale
pressure_data_unscale <- press_mainland_trend_butterfly
site_data <- site_mainland_sf_reproj_butterfly
min_site_number_per_species <- 60
min_occurence_species <- 200
family <- "quasipoisson"


predict_trend_butterfly <- function(butterfly_data,pressure_data,pressure_data_unscale,site_data,
                               lulc_pls_short,climate_pls,pa_pls_short,
                               pressure_name = c("d_impervious","d_treedensity","d_agri",
                                                 "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                                 "d_shannon","shannon","drymatter","protectedarea_perc",
                                                 "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high",
                                                 "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                               min_site_number_per_species = 60,
                               min_occurence_species=200,
                               family="quasipoisson"){
  
  species_press_data_year <- merge(butterfly_data, pressure_data[which(pressure_data$transect_id %in% unique(butterfly_data$transect_id) & pressure_data$year %in% unique(butterfly_data$year)),], by =c("transect_id","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("transect_id","count_corrected","year","transect_length","bms_id","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  species_press_data_year_unscale <- merge(butterfly_data, pressure_data_unscale[which(pressure_data_unscale$transect_id %in% unique(butterfly_data$transect_id) & pressure_data_unscale$year %in% unique(butterfly_data$year)),], by =c("transect_id","year"), all.x=TRUE)
  
  poisson_df_unscale <- na.omit(species_press_data_year_unscale[,c("transect_id","count_corrected","year","transect_length","bms_id","Long_LAEA","Lat_LAEA",
                                                                   pressure_name,"tempspring_2020","tempspringvar_2020","precspring_2020","agri_2018","shannon_2018","impervious_2018","treedensity_2018","PLS")])
  
  
  poisson_df$year <- scale(poisson_df$year)#poisson_df$year - 2000
  
  if(length(table(poisson_df$transect_length)) > length(unique(poisson_df$bms_id))){
    one_scheme_time_area <- 0 
    poisson_df$transect_length <- scale(poisson_df$transect_length)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_corrected_scale_all <- poisson_df$count_corrected#scales::rescale(poisson_df$count_corrected)
  
  if(length(pressure_name) > 1){
    formula_gam <- "count_corrected_scale_all ~ year + year:d_impervious + year:d_treedensity +
    year:eulandsystem_forest_lowmedium + year:eulandsystem_forest_high +
    year:d_agri + year:eulandsystem_farmland_low + year:eulandsystem_farmland_medium + year:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
  }else{
    formula_gam <- paste("count_corrected_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
  }
  
  col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
                 "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
                 "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
                 "year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium","year:eulandsystem_farmland_high",
                 "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")
  
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$bms_id)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    if(global_mod$converged){
      
      predict_trend_europe <- predict_trend(mod=global_mod,
                                            pressure_data_unscale,
                                            poisson_df_unscale,
                                            poisson_df,
                                            lulc_pls_short,
                                            climate_pls,
                                            pa_pls_short,
                                            PLS="europe")
      
      for(i in 1:14){
        predict_trend_europe_temp <- predict_trend(mod=global_mod,
                                              pressure_data_unscale,
                                              poisson_df_unscale,
                                              poisson_df,
                                              lulc_pls_short,
                                              climate_pls,
                                              pa_pls_short,
                                              PLS="europe",
                                              pressure_remove = i)
        
        predict_trend_europe <- rbind(predict_trend_europe,predict_trend_europe_temp)
      }
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      if_fail <- data.frame(intercept=NA,trend_past=NA,sd_past=NA,trend_BAU=NA,sd_BAU=NA,trend_SSP1=NA,sd_SSP1=NA,
                            trend_SSP3=NA,sd_SSP3=NA,trend_nac=NA,sd_nac=NA,
                            trend_nfn=NA,sd_nfn=NA,trend_nfs=NA,sd_nfs=NA,
                            trend_past_signif=NA,sd_past_signif=NA,
                            trend_BAU_signif=NA,sd_BAU_signif=NA,trend_SSP1_signif=NA,sd_SSP1_signif=NA,
                            trend_SSP3_signif=NA,sd_SSP3_signif=NA,trend_nac_signif=NA,sd_nac_signif=NA,
                            trend_nfn_signif=NA,sd_nfn_signif=NA,trend_nfs_signif=NA,sd_nfs_signif=NA,
                            PLS=NA,pressure_removed=NA)
      
      predict_trend_pls <- ddply(unique_poisson_df,.(PLS),.fun=purrr::possibly(otherwise=if_fail,
                                                                               .f=function(x,min_site_number_per_species,poisson_df){
                                                                                 
                                                                                 if(nrow(x) >= min_site_number_per_species){
                                                                                   
                                                                                   poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
                                                                                   poisson_df_unscale_i <- poisson_df_unscale[which(poisson_df_unscale$PLS == unique(x$PLS)),]
                                                                                   
                                                                                   if(length(table(poisson_df_i$transect_length)) > length(unique(poisson_df_i$bms_id))){
                                                                                     one_scheme_time_area <- 0 
                                                                                     poisson_df_i$transect_length <- scale(poisson_df_i$transect_length)
                                                                                   }else{
                                                                                     one_scheme_time_area <- 1
                                                                                   }
                                                                                   
                                                                                   if(length(unique(poisson_df_i$bms_id)) > 1 && one_scheme_time_area == 0){
                                                                                     res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length:bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                           family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   }
                                                                                   if(length(unique(poisson_df_i$bms_id)) == 1 && one_scheme_time_area == 0){
                                                                                     res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("transect_length","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                           family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   }
                                                                                   if(length(unique(poisson_df_i$bms_id)) > 1 && one_scheme_time_area == 1){
                                                                                     res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("bms_id","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                           family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   }
                                                                                   if(length(unique(poisson_df_i$bms_id)) == 1 && one_scheme_time_area == 1){
                                                                                     res.poisson_i <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                                                                                                           family=family, data=poisson_df_i,random=list(transect_id=~1))
                                                                                   }
                                                                                   
                                                                                   predict_trend_i <- predict_trend(mod=res.poisson_i$gam,
                                                                                                                    pressure_data_unscale,
                                                                                                                    poisson_df_unscale_i,
                                                                                                                    poisson_df_i,
                                                                                                                    lulc_pls_short,
                                                                                                                    climate_pls,
                                                                                                                    pa_pls_short,
                                                                                                                    PLS=unique(x$PLS))
                                                                                   
                                                                                 }else{
                                                                                   predict_trend_i <- data.frame(intercept=NA,trend_past=NA,sd_past=NA,trend_BAU=NA,sd_BAU=NA,trend_SSP1=NA,sd_SSP1=NA,
                                                                                                                 trend_SSP3=NA,sd_SSP3=NA,trend_nac=NA,sd_nac=NA,
                                                                                                                 trend_nfn=NA,sd_nfn=NA,trend_nfs=NA,sd_nfs=NA,
                                                                                                                 trend_past_signif=NA,sd_past_signif=NA,
                                                                                                                 trend_BAU_signif=NA,sd_BAU_signif=NA,trend_SSP1_signif=NA,sd_SSP1_signif=NA,
                                                                                                                 trend_SSP3_signif=NA,sd_SSP3_signif=NA,trend_nac_signif=NA,sd_nac_signif=NA,
                                                                                                                 trend_nfn_signif=NA,sd_nfn_signif=NA,trend_nfs_signif=NA,sd_nfs_signif=NA,
                                                                                                                 PLS=unique(x$PLS),pressure_removed=NA)
                                                                                 }
                                                                                 
                                                                                 return(predict_trend_i)
                                                                               }),
                                 min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
                                 .progress="none")
      
      predict_trend_all <- rbind(predict_trend_pls,predict_trend_europe)
      
      #res.poisson_sf <- merge(grid_eu_mainland_biogeo,predict_trend_all,by="PLS")
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=trend_BAU)) + scale_fill_gradient2()
      
    }else{
      predict_trend_all <- data.frame(intercept=NA,trend_past=NA,sd_past=NA,trend_BAU=NA,sd_BAU=NA,trend_SSP1=NA,sd_SSP1=NA,
                                      trend_SSP3=NA,sd_SSP3=NA,trend_nac=NA,sd_nac=NA,
                                      trend_nfn=NA,sd_nfn=NA,trend_nfs=NA,sd_nfs=NA,
                                      trend_past_signif=NA,sd_past_signif=NA,
                                      trend_BAU_signif=NA,sd_BAU_signif=NA,trend_SSP1_signif=NA,sd_SSP1_signif=NA,
                                      trend_SSP3_signif=NA,sd_SSP3_signif=NA,trend_nac_signif=NA,sd_nac_signif=NA,
                                      trend_nfn_signif=NA,sd_nfn_signif=NA,trend_nfs_signif=NA,sd_nfs_signif=NA,
                                      PLS=NA,pressure_removed=NA)
    }
    
  }else{
    predict_trend_all <- data.frame(intercept=NA,trend_past=NA,sd_past=NA,trend_BAU=NA,sd_BAU=NA,trend_SSP1=NA,sd_SSP1=NA,
                                    trend_SSP3=NA,sd_SSP3=NA,trend_nac=NA,sd_nac=NA,
                                    trend_nfn=NA,sd_nfn=NA,trend_nfs=NA,sd_nfs=NA,
                                    trend_past_signif=NA,sd_past_signif=NA,
                                    trend_BAU_signif=NA,sd_BAU_signif=NA,trend_SSP1_signif=NA,sd_SSP1_signif=NA,
                                    trend_SSP3_signif=NA,sd_SSP3_signif=NA,trend_nac_signif=NA,sd_nac_signif=NA,
                                    trend_nfn_signif=NA,sd_nfn_signif=NA,trend_nfs_signif=NA,sd_nfs_signif=NA,
                                    PLS=NA,pressure_removed=NA)
  }
  
  return(predict_trend_all)
}



overall_mean_sd_trend <- function(data){
  n <- length(na.omit(data$trend_BAU))
  mu_past <- exp(mean(data$trend_past,na.rm=TRUE))
  var_past <- (sum(data$sd_past^2 + data$trend_past^2, na.rm = TRUE))/n - mean(data$trend_past,na.rm=TRUE)^2
  sd_past <- sqrt(mu_past^2*var_past)
  se_past <- mu_past/sqrt(n)*sd(data$trend_past)
  mu_bau <- exp(mean(data$trend_BAU,na.rm=TRUE))
  var_bau <- (sum(data$sd_BAU^2 + data$trend_BAU^2, na.rm = TRUE))/n - mean(data$trend_BAU,na.rm=TRUE)^2
  sd_bau <- sqrt(mu_bau^2*var_bau)
  se_bau <- mu_bau/sqrt(n)*sd(data$trend_BAU)
  mu_ssp1 <- exp(mean(data$trend_SSP1,na.rm=TRUE))
  var_ssp1 <- (sum(data$sd_SSP1^2 + data$trend_SSP1^2, na.rm = TRUE))/n - mean(data$trend_SSP1,na.rm=TRUE)^2
  sd_ssp1 <- sqrt(mu_ssp1^2*var_ssp1)
  se_ssp1 <- mu_ssp1/sqrt(n)*sd(data$trend_SSP1)
  mu_ssp3 <- exp(mean(data$trend_SSP3,na.rm=TRUE))
  var_ssp3 <- (sum(data$sd_SSP3^2 + data$trend_SSP3^2, na.rm = TRUE))/n - mean(data$trend_SSP3,na.rm=TRUE)^2
  sd_ssp3 <- sqrt(mu_ssp3^2*var_ssp3)
  se_ssp3 <- mu_ssp3/sqrt(n)*sd(data$trend_SSP3)
  mu_nac <- exp(mean(data$trend_nac,na.rm=TRUE))
  var_nac <- (sum(data$sd_nac^2 + data$trend_nac^2, na.rm = TRUE))/n - mean(data$trend_nac,na.rm=TRUE)^2
  sd_nac <- sqrt(mu_nac^2*var_nac)
  se_nac <- mu_nac/sqrt(n)*sd(data$trend_nac)
  mu_nfn <- exp(mean(data$trend_nfn,na.rm=TRUE))
  var_nfn <- (sum(data$sd_nfn^2 + data$trend_nfn^2, na.rm = TRUE))/n - mean(data$trend_nfn,na.rm=TRUE)^2
  sd_nfn <- sqrt(mu_nfn^2*var_nfn)
  se_nfn <- mu_nfn/sqrt(n)*sd(data$trend_nfn)
  mu_nfs <- exp(mean(data$trend_nfs,na.rm=TRUE))
  var_nfs <- (sum(data$sd_nfs^2 + data$trend_nfs^2, na.rm = TRUE))/n - mean(data$trend_nfs,na.rm=TRUE)^2
  sd_nfs <- sqrt(mu_nfs^2*var_nfs)
  se_nfs <- mu_nfs/sqrt(n)*sd(data$trend_nfs)
  
  mu_past_signif <- exp(mean(data$trend_past_signif,na.rm=TRUE))
  var_past_signif <- (sum(data$sd_past_signif^2 + data$trend_past_signif^2, na.rm = TRUE))/n - mean(data$trend_past_signif,na.rm=TRUE)^2
  sd_past_signif <- sqrt(mu_past_signif^2*var_past_signif)
  se_past_signif <- mu_past_signif/sqrt(n)*sd(data$trend_past_signif)
  mu_bau_signif <- exp(mean(data$trend_BAU_signif,na.rm=TRUE))
  var_bau_signif <- (sum(data$sd_BAU_signif^2 + data$trend_BAU_signif^2, na.rm = TRUE))/n - mean(data$trend_BAU_signif,na.rm=TRUE)^2
  sd_bau_signif <- sqrt(mu_bau_signif^2*var_bau_signif)
  se_bau_signif <- mu_bau_signif/sqrt(n)*sd(data$trend_BAU_signif)
  mu_ssp1_signif <- exp(mean(data$trend_SSP1_signif,na.rm=TRUE))
  var_ssp1_signif <- (sum(data$sd_SSP1_signif^2 + data$trend_SSP1_signif^2, na.rm = TRUE))/n - mean(data$trend_SSP1_signif,na.rm=TRUE)^2
  sd_ssp1_signif <- sqrt(mu_ssp1_signif^2*var_ssp1_signif)
  se_ssp1_signif <- mu_ssp1_signif/sqrt(n)*sd(data$trend_SSP1_signif)
  mu_ssp3_signif <- exp(mean(data$trend_SSP3_signif,na.rm=TRUE))
  var_ssp3_signif <- (sum(data$sd_SSP3_signif^2 + data$trend_SSP3_signif^2, na.rm = TRUE))/n - mean(data$trend_SSP3_signif,na.rm=TRUE)^2
  sd_ssp3_signif <- sqrt(mu_ssp3_signif^2*var_ssp3_signif)
  se_ssp3_signif <- mu_ssp3_signif/sqrt(n)*sd(data$trend_SSP3_signif)
  mu_nac_signif <- exp(mean(data$trend_nac_signif,na.rm=TRUE))
  var_nac_signif <- (sum(data$sd_nac_signif^2 + data$trend_nac_signif^2, na.rm = TRUE))/n - mean(data$trend_nac_signif,na.rm=TRUE)^2
  sd_nac_signif <- sqrt(mu_nac_signif^2*var_nac_signif)
  se_nac_signif <- mu_nac_signif/sqrt(n)*sd(data$trend_nac_signif)
  mu_nfn_signif <- exp(mean(data$trend_nfn_signif,na.rm=TRUE))
  var_nfn_signif <- (sum(data$sd_nfn_signif^2 + data$trend_nfn_signif^2, na.rm = TRUE))/n - mean(data$trend_nfn_signif,na.rm=TRUE)^2
  sd_nfn_signif <- sqrt(mu_nfn_signif^2*var_nfn_signif)
  se_nfn_signif <- mu_nfn_signif/sqrt(n)*sd(data$trend_nfn_signif)
  mu_nfs_signif <- exp(mean(data$trend_nfs_signif,na.rm=TRUE))
  var_nfs_signif <- (sum(data$sd_nfs_signif^2 + data$trend_nfs_signif^2, na.rm = TRUE))/n - mean(data$trend_nfs_signif,na.rm=TRUE)^2
  sd_nfs_signif <- sqrt(mu_nfs_signif^2*var_nfs_signif)
  se_nfs_signif <- mu_nfs_signif/sqrt(n)*sd(data$trend_nfs_signif)
  
  return(data.frame(mu_past,sd_past,se_past,mu_bau,sd_bau,se_bau,mu_ssp1,sd_ssp1,se_ssp1,mu_ssp3,sd_ssp3,se_ssp3,
                    mu_nac,sd_nac,se_nac,mu_nfn,sd_nfn,se_nfn,mu_nfs,sd_nfs,se_nfs,
                    mu_past_signif,sd_past_signif,se_past_signif,mu_bau_signif,sd_bau_signif,se_bau_signif,mu_ssp1_signif,sd_ssp1_signif,se_ssp1_signif,mu_ssp3_signif,sd_ssp3_signif,se_ssp3_signif,
                    mu_nac_signif,sd_nac_signif,se_nac_signif,mu_nfn_signif,sd_nfn_signif,se_nfn_signif,mu_nfs_signif,sd_nfs_signif,se_nfs_signif,n))
}


overall_mean_sd_trend_weigthed <- function(data){
  n <- length(na.omit(data$trend_BAU))
  mu_bau <- weighted.mean(exp(data$trend_BAU),na.rm=TRUE, w = data$ab_tot)
  var_bau <- (sum(data$sd_BAU^2*exp(data$trend_BAU)^2 + exp(data$trend_BAU)^2, na.rm = TRUE))/n - mu_bau^2
  sd_bau <- sqrt(mu_bau^2*var_bau)
  mu_ssp1 <- weighted.mean(exp(data$trend_SSP1),na.rm=TRUE, w = data$ab_tot)
  var_ssp1 <- (sum(data$sd_SSP1^2*exp(data$trend_SSP1)^2 + exp(data$trend_SSP1)^2, na.rm = TRUE))/n - mu_ssp1^2
  sd_ssp1 <- sqrt(var_ssp1)
  mu_ssp3 <- weighted.mean(exp(data$trend_SSP3),na.rm=TRUE, w = data$ab_tot)
  var_ssp3 <- (sum(data$sd_SSP3^2*exp(data$trend_SSP3)^2 + exp(data$trend_SSP3)^2, na.rm = TRUE))/n - mu_ssp3^2
  sd_ssp3 <- sqrt(var_ssp3)
  mu_nac <- weighted.mean(exp(data$trend_nac),na.rm=TRUE, w = data$ab_tot)
  var_nac <- (sum(data$sd_nac^2*exp(data$trend_nac)^2 + exp(data$trend_nac)^2, na.rm = TRUE))/n - mu_nac^2
  sd_nac <- sqrt(var_nac)
  mu_nfn <- weighted.mean(exp(data$trend_nfn),na.rm=TRUE, w = data$ab_tot)
  var_nfn <- (sum(data$sd_nfn^2*exp(data$trend_nfn)^2 + exp(data$trend_nfn)^2, na.rm = TRUE))/n - mu_nfn^2
  sd_nfn <- sqrt(var_nfn)
  mu_nfs <- weighted.mean(exp(data$trend_nfs),na.rm=TRUE, w = data$ab_tot)
  var_nfs <- (sum(data$sd_nfs^2*exp(data$trend_nfs)^2 + exp(data$trend_nfs)^2, na.rm = TRUE))/n - mu_nfs^2#(sum((data$sd_nfs)^2 + (data$trend_nfs)^2, na.rm = TRUE))/n - mu_nfs^2
  sd_nfs <- sqrt(var_nfs)
  mu_bau_signif <- weighted.mean(exp(data$trend_BAU_signif),na.rm=TRUE, w = data$ab_tot)
  var_bau_signif <- (sum(data$sd_BAU_signif^2*exp(data$trend_BAU_signif)^2 + exp(data$trend_BAU_signif)^2, na.rm = TRUE))/n - mu_bau_signif^2
  sd_bau_signif <- sqrt(var_bau_signif)
  mu_ssp1_signif <- weighted.mean(exp(data$trend_SSP1_signif),na.rm=TRUE, w = data$ab_tot)
  var_ssp1_signif <- (sum(data$sd_SSP1_signif^2*exp(data$trend_SSP1_signif)^2 + exp(data$trend_SSP1_signif)^2, na.rm = TRUE))/n - mu_ssp1_signif^2
  sd_ssp1_signif <- sqrt(var_ssp1_signif)
  mu_ssp3_signif <- weighted.mean(exp(data$trend_SSP3_signif),na.rm=TRUE, w = data$ab_tot)
  var_ssp3_signif <- (sum(data$sd_SSP3_signif^2*exp(data$trend_SSP3_signif)^2 + exp(data$trend_SSP3_signif)^2, na.rm = TRUE))/n - mu_ssp3_signif^2
  sd_ssp3_signif <- sqrt(var_ssp3_signif)
  mu_nac_signif <- weighted.mean(exp(data$trend_nac_signif),na.rm=TRUE, w = data$ab_tot)
  var_nac_signif <- (sum(data$sd_nac_signif^2*exp(data$trend_nac_signif)^2 + exp(data$trend_nac_signif)^2, na.rm = TRUE))/n - mu_nac_signif^2
  sd_nac_signif <- sqrt(var_nac_signif)
  mu_nfn_signif <- weighted.mean(exp(data$trend_nfn_signif),na.rm=TRUE, w = data$ab_tot)
  var_nfn_signif <- (sum(data$sd_nfn_signif^2*exp(data$trend_nfn_signif)^2 + exp(data$trend_nfn_signif)^2, na.rm = TRUE))/n - mu_nfn_signif^2
  sd_nfn_signif <- sqrt(var_nfn_signif)
  mu_nfs_signif <- weighted.mean(exp(data$trend_nfs_signif),na.rm=TRUE, w = data$ab_tot)
  var_nfs_signif <- (sum(data$sd_nfs_signif^2*exp(data$trend_nfs_signif)^2 + exp(data$trend_nfs_signif)^2, na.rm = TRUE))/n - mu_nfs_signif^2
  sd_nfs_signif <- sqrt(var_nfs_signif)
  return(data.frame(mu_bau,sd_bau,mu_ssp1,sd_ssp1,mu_ssp3,sd_ssp3,
                    mu_nac,sd_nac,mu_nfn,sd_nfn,mu_nfs,sd_nfs,
                    mu_bau_signif,sd_bau_signif,mu_ssp1_signif,sd_ssp1_signif,mu_ssp3_signif,sd_ssp3_signif,
                    mu_nac_signif,sd_nac_signif,mu_nfn_signif,sd_nfn_signif,mu_nfs_signif,sd_nfs_signif,n))
}


### France


bird_data <- droplevels(subsite_data_mainland_trend_fr[which(subsite_data_mainland_trend_fr$sci_name_out == "Alauda arvensis"),])
pressure_data <- press_mainland_trend_scale_FR
pressure_data_unscale <- press_mainland_trend_FR
pressure_change <- pressure_change
site_data <- site_mainland_sf_reproj
min_site_number_per_species <- 60
min_occurence_species <- 200
family <- "quasipoisson"


gam_species_FR <- function(bird_data,pressure_data,pressure_data_unscale,pressure_change,site_data,
                             pressure_name = c("d_impervious","d_treedensity","d_agri",
                                               "d_tempsrping","tempsrping","d_tempsrpingvar","d_precspring","precspring",
                                               "d_shannon","shannon","drymatter","protectedarea_perc",
                                               "eulandsystem_farmland_low","eulandsystem_farmland_medium","eulandsystem_farmland_high","CPE_mean",
                                               "eulandsystem_forest_lowmedium","eulandsystem_forest_high","milieu_cat"),
                             min_site_number_per_species = 60,
                             min_occurence_species=300,
                           pest_landsystem = c("pest","landsystem"),
                             family="quasipoisson"){
  
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
  
  if(pest_landsystem == "landsystem"){
    formula_gam <- "count_scale_all ~ year + year:d_impervious + year:d_treedensity +
    year:eulandsystem_forest_lowmedium + year:eulandsystem_forest_high +
    year:d_agri + year:eulandsystem_farmland_low + year:eulandsystem_farmland_medium + year:eulandsystem_farmland_high +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
    col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
                   "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
                   "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
                   "year:eulandsystem_farmland_low","year:eulandsystem_farmland_medium","year:eulandsystem_farmland_high",
                   "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")
    predict_trend_FR <- predict_trend_landsystem_FR
  }
  if(pest_landsystem == "pest"){
    formula_gam <- "count_scale_all ~ year + year:d_impervious + year:d_treedensity +
    year:eulandsystem_forest_lowmedium + year:eulandsystem_forest_high +
    year:d_agri + year:CPE_mean +
    year:d_tempsrping + year:d_tempsrpingvar + year:d_precspring + year:d_shannon + year:protectedarea_perc +
    milieu_cat + tempsrping + precspring + shannon + drymatter"
    col_names <- c("(Intercept)","year","milieu_catopenland","milieu_catothers","milieu_caturban",
                   "tempsrping","precspring","shannon","drymatter","year:d_impervious","year:d_treedensity",
                   "year:eulandsystem_forest_lowmedium","year:eulandsystem_forest_high","year:d_agri",
                   "year:CPE_mean",
                   "year:d_tempsrping","year:d_tempsrpingvar","year:d_precspring","year:d_shannon","year:protectedarea_perc")
    predict_trend_FR <- predict_trend_pest_FR
  }
  
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
      global_mod <- gamm(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=4)"), collapse = " + "))),
                        family=family, data=poisson_df,random=list(siteID=~1|PLS))
      
      predict_trend_FR_res <- predict_trend_FR(mod=global_mod,
                                               pressure_data_unscale,
                                               poisson_df,
                                               poisson_df_unscale,
                                               pressure_change)
      
      global_mod_coef <- summary(global_mod$gam)$p.table
      
      if(nrow(global_mod_coef) < length(col_names)){
        row_to_add <- matrix(NA,nrow=length(which(!(col_names %in% row.names(global_mod_coef)))), ncol=1)
        row.names(row_to_add) <- col_names[which(!(col_names %in% row.names(global_mod_coef)))]
        global_mod_coef_complet <- merge(global_mod_coef,row_to_add,by="row.names",all=TRUE)
        global_mod_coef_complet <- global_mod_coef_complet[match(col_names, global_mod_coef_complet$Row.names),]
        global_mod_coef_complet <- as.matrix(global_mod_coef_complet[2:5])
        global_mod_coef <- global_mod_coef_complet
      }
      
      global_mod_coef <- rbind(global_mod_coef,c(summary(global_mod$gam)$r.sq,rep(0,3)),c(summary(global_mod$gam)$n,rep(0,3)))
      
      global_mod_coef1 <- global_mod_coef[,1]
      global_mod_coef1[which(global_mod_coef[,4] > 0.05)] <- NA
      global_mod_df <- data.frame(t(global_mod_coef1))
      names(global_mod_df) <- c(col_names,"dev_exp","n_obs")
      global_mod_df$PLS <- "france"
      
      res.poisson_df <- cbind(global_mod_df,predict_trend_FR_res)
      
    }else{
      res.poisson_df <- data.frame(t(rep(NA,(length(col_names)+23))))
      names(res.poisson_df) <- c(col_names,"dev_exp","n_obs","PLS","trend_past","sd_past",
                                 "trend_tend","sd_tend","trend_s1","sd_s1",
                                 "trend_s2","sd_s2","trend_s3","sd_s3",
                                 "trend_s4","sd_s4",
                                 "trend_tend_signif","sd_tend_signif","trend_s1_signif","sd_s1_signif",
                                 "trend_s2_signif","sd_s2_signif","trend_s3_signif","sd_s3_signif",
                                 "trend_s4_signif","sd_s4_signif")
    }
    
  return(res.poisson_df)
}



predict_trend_landsystem_FR <- function(mod,
                          pressure_data_unscale,
                          poisson_df_unscale,
                          pressure_change,
                          nb_rep=1000){
  
  mod_coef <- summary(mod$gam)$p.table[grep("year",row.names(summary(mod$gam)$p.table)),]
  
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
  
  
  mod_coef_signif <- mod_coef
  mod_coef_signif[which(mod_coef_signif[,c("Pr(>|t|)")] > 0.05),c("Estimate","Std. Error")] <- 0
  
  
  beta1_tend_signif <- mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_tend - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_tend - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_tend - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_tend - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_tend - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_tend - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_tend  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_tend - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_tend - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_tend - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_tend - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_tend - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_tend - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  beta1_tend_sample_signif <- rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_tend - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_tend - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_tend - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_tend - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_tend - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_tend - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_tend  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_tend - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_tend - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_tend - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_tend - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_tend - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_tend - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  

  beta1_s1_signif <- mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_s1 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s1 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s1 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_s1 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_s1 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s1 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s1  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_s1 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_s1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_s1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_s1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  beta1_s1_sample_signif <- rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_s1 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s1 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s1 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_s1 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_s1 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s1 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s1  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_s1 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_s1 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_s1 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_s1 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  

  beta1_s2_signif <- mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_s2 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s2 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s2 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_s2 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_s2 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s2 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s2  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s2 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s2 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_s2 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_s2 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_s2 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_s2 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  beta1_s2_sample_signif <- rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_s2 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s2 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s2 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_s2 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_s2 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s2 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s2  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s2 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s2 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_s2 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_s2 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_s2 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_s2 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  

  beta1_s3_signif <- mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_s3 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s3 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s3 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_s3 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_s3 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s3 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s3  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_s3 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_s3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_s3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_s3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  beta1_s3_sample_signif <- rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_s3 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s3 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s3 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_s3 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_s3 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s3 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s3  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_s3 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_s3 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_s3 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_s3 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  

  beta1_s4_signif <- mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_s4 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s4 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s4 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_s4 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_s4 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s4 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s4  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s4 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s4 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_s4 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"]*(agri_low_s4 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"]*(agri_medium_s4 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"]*(agri_high_s4 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  beta1_s4_sample_signif <- rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_s4 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s4 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s4 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_s4 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_s4 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s4 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s4  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s4 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s4 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_s4 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_low"),"Std. Error"])*(agri_low_s4 - eulandsystem_farmland_low_mu)/eulandsystem_farmland_low_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_medium"),"Std. Error"])*(agri_medium_s4 - eulandsystem_farmland_medium_mu)/eulandsystem_farmland_medium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_farmland_high"),"Std. Error"])*(agri_high_s4 - eulandsystem_farmland_high_mu)/eulandsystem_farmland_high_si
  
  
  return(data.frame(trend_tend=beta1_tend,sd_tend=sd(beta1_tend_sample),trend_s1=beta1_s1,sd_s1=sd(beta1_s1_sample),
                    trend_s2=beta1_s2,sd_s2=sd(beta1_s2_sample),trend_s3=beta1_s3,sd_s3=sd(beta1_s3_sample),
                    trend_s4=beta1_s4,sd_s4=sd(beta1_s4_sample),
                    trend_tend_signif=beta1_tend_signif,sd_tend_signif=sd(beta1_tend_sample_signif),trend_s1_signif=beta1_s1_signif,sd_s1_signif=sd(beta1_s1_sample_signif),
                    trend_s2_signif=beta1_s2_signif,sd_s2_signif=sd(beta1_s2_sample_signif),trend_s3_signif=beta1_s3_signif,sd_s3_signif=sd(beta1_s3_sample_signif),
                    trend_s4_signif=beta1_s4_signif,sd_s4_signif=sd(beta1_s4_sample_signif)))
}


predict_trend_pest_FR <- function(mod,
                             pressure_data_unscale,
                             poisson_df,
                             poisson_df_unscale,
                             pressure_change,
                             nb_rep=1000){
  
  mod_coef <- summary(mod$gam)$p.table[grep("year",row.names(summary(mod$gam)$p.table)),]
  
  year_si <- sd(na.omit(pressure_data_unscale$year))
  d_impervious_si <- sd(na.omit(pressure_data_unscale$d_impervious))
  d_tempspring_si <- sd(na.omit(pressure_data_unscale$d_tempsrping))
  d_tempspringvar_si <- sd(na.omit(pressure_data_unscale$d_tempsrpingvar))
  d_precspring_si <- sd(na.omit(pressure_data_unscale$d_precspring))
  d_shannon_si <- sd(na.omit(pressure_data_unscale$d_shannon))
  protectedarea_perc_si <- sd(na.omit(pressure_data_unscale$protectedarea_perc))
  d_treedensity_si <- sd(na.omit(pressure_data_unscale$d_treedensity))
  d_agri_si <- sd(na.omit(pressure_data_unscale$d_agri))
  CPE_mean_si <- sd(na.omit(pressure_data_unscale$CPE_mean))
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
  CPE_mean_mu <- mean(na.omit(pressure_data_unscale$CPE_mean))
  eulandsystem_forest_lowmedium_mu <- mean(na.omit(pressure_data_unscale$eulandsystem_forest_lowmedium))
  eulandsystem_forest_high_mu <- mean(na.omit(pressure_data_unscale$eulandsystem_forest_high))
  
  nb_rep <- 1000
  
  beta1_past <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"]*mean(poisson_df$d_impervious) +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"]*mean(poisson_df$d_tempsrping) +
    mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"]*mean(poisson_df$d_tempsrpingvar) +
    mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"]*mean(poisson_df$d_precspring) +
    mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"]*mean(poisson_df$d_shannon) +
    mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"]*mean(poisson_df$protectedarea_perc) +
    mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"]*mean(poisson_df$d_treedensity) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*mean(poisson_df$eulandsystem_forest_lowmedium) +
    mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"]*mean(poisson_df$eulandsystem_forest_high) +
    mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"]*mean(poisson_df$d_agri) +
    mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"]*mean(poisson_df$CPE_mean)
  
  beta1_past_sample <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_impervious"),"Std. Error"])*mean(poisson_df$d_impervious) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrping"),"Std. Error"])*mean(poisson_df$d_tempsrping) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_tempsrpingvar"),"Std. Error"])*mean(poisson_df$d_tempsrpingvar) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_precspring"),"Std. Error"])*mean(poisson_df$d_precspring) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_shannon"),"Std. Error"])*mean(poisson_df$d_shannon) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:protectedarea_perc"),"Std. Error"])*mean(poisson_df$protectedarea_perc) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_treedensity"),"Std. Error"])*mean(poisson_df$d_treedensity) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_lowmedium) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:eulandsystem_forest_high"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_high) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:d_agri"),"Std. Error"])*mean(poisson_df$d_agri) +
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Std. Error"])*mean(poisson_df$CPE_mean)
  
  
  d_impervious_tend <- mean(poisson_df_unscale$impervious_2018)*
    (pressure_change$tend[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
  d_shannon_tend <- mean(poisson_df_unscale$shannon_2018)*
    (pressure_change$tend[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
  d_agri_tend <- mean(poisson_df_unscale$agri_2018)*
    (pressure_change$tend[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
  agri_high_tend <- mean(poisson_df_unscale$CPE_mean)*pressure_change$tend[which(pressure_change$variable %in% c("NODU"))]/pressure_change$initial[which(pressure_change$variable %in% c("NODU"))]
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
    mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"]*(agri_high_tend - CPE_mean_mu)/CPE_mean_si
  
  
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
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Std. Error"])*(agri_high_tend - CPE_mean_mu)/CPE_mean_si
  
  
  d_impervious_s1 <- mean(poisson_df_unscale$impervious_2018)*
    (pressure_change$s1[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
  d_shannon_s1 <- mean(poisson_df_unscale$shannon_2018)*
    (pressure_change$s1[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
  d_agri_s1 <- mean(poisson_df_unscale$agri_2018)*
    (pressure_change$s1[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
  agri_high_s1 <- mean(poisson_df_unscale$CPE_mean)*pressure_change$s1[which(pressure_change$variable %in% c("NODU"))]/pressure_change$initial[which(pressure_change$variable %in% c("NODU"))]
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
    mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"]*(agri_high_s1 - CPE_mean_mu)/CPE_mean_si
  
  
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
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Std. Error"])*(agri_high_s1 - CPE_mean_mu)/CPE_mean_si
  
  d_impervious_s2 <- mean(poisson_df_unscale$impervious_2018)*
    (pressure_change$s2[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
  d_shannon_s2 <- mean(poisson_df_unscale$shannon_2018)*
    (pressure_change$s2[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
  d_agri_s2 <- mean(poisson_df_unscale$agri_2018)*
    (pressure_change$s2[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
  agri_high_s2 <- mean(poisson_df_unscale$CPE_mean)*pressure_change$s2[which(pressure_change$variable %in% c("NODU"))]/pressure_change$initial[which(pressure_change$variable %in% c("NODU"))]
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
    mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"]*(agri_high_s2 - CPE_mean_mu)/CPE_mean_si
  
  
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
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Std. Error"])*(agri_high_s2 - CPE_mean_mu)/CPE_mean_si
  
  d_impervious_s3 <- mean(poisson_df_unscale$impervious_2018)*
    (pressure_change$s3[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
  d_shannon_s3 <- mean(poisson_df_unscale$shannon_2018)*
    (pressure_change$s3[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
  d_agri_s3 <- mean(poisson_df_unscale$agri_2018)*
    (pressure_change$s3[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
  agri_high_s3 <- mean(poisson_df_unscale$CPE_mean)*pressure_change$s3[which(pressure_change$variable %in% c("NODU"))]/pressure_change$initial[which(pressure_change$variable %in% c("NODU"))]
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
    mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"]*(agri_high_s3 - CPE_mean_mu)/CPE_mean_si
  
  
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
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Std. Error"])*(agri_high_s3 - CPE_mean_mu)/CPE_mean_si
  
  d_impervious_s4 <- mean(poisson_df_unscale$impervious_2018)*
    (pressure_change$s4[which(pressure_change$variable %in% c("Urban cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Urban cover"))]-1)/(2050-2020) 
  d_shannon_s4 <- mean(poisson_df_unscale$shannon_2018)*
    (pressure_change$s4[which(pressure_change$variable %in% c("Hedge"))]/pressure_change$initial[which(pressure_change$variable %in% c("Hedge"))]-1)/(2050-2020) 
  d_agri_s4 <- mean(poisson_df_unscale$agri_2018)*
    (pressure_change$s4[which(pressure_change$variable %in% c("Agricultural cover"))]/pressure_change$initial[which(pressure_change$variable %in% c("Agricultural cover"))]-1)/(2050-2020) 
  agri_high_s4 <- mean(poisson_df_unscale$CPE_mean)*pressure_change$s4[which(pressure_change$variable %in% c("NODU"))]/pressure_change$initial[which(pressure_change$variable %in% c("NODU"))]
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
    mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"]*(agri_high_s4 - CPE_mean_mu)/CPE_mean_si
  
  
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
    rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year:CPE_mean"),"Std. Error"])*(agri_high_s4 - CPE_mean_mu)/CPE_mean_si
  
  
  mod_coef_signif <- mod_coef
  mod_coef_signif[which(mod_coef_signif[,c("Pr(>|t|)")] > 0.05),c("Estimate","Std. Error")] <- 0
  
  beta1_past_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*mean(poisson_df$d_impervious) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*mean(poisson_df$d_tempsrping) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*mean(poisson_df$d_tempsrpingvar) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*mean(poisson_df$d_precspring) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*mean(poisson_df$d_shannon) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*mean(poisson_df$protectedarea_perc) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*mean(poisson_df$d_treedensity) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*mean(poisson_df$eulandsystem_forest_lowmedium) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*mean(poisson_df$eulandsystem_forest_high) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*mean(poisson_df$d_agri) +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"]*mean(poisson_df$CPE_mean)
  
  beta1_past_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*mean(poisson_df$d_impervious) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*mean(poisson_df$d_tempsrping) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*mean(poisson_df$d_tempsrpingvar) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*mean(poisson_df$d_precspring) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*mean(poisson_df$d_shannon) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*mean(poisson_df$protectedarea_perc) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*mean(poisson_df$d_treedensity) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_lowmedium) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*mean(poisson_df$eulandsystem_forest_high) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*mean(poisson_df$d_agri) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Std. Error"])*mean(poisson_df$CPE_mean)
  
  
  beta1_tend_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_tend - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_tend - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_tend - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_tend - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_tend - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_tend - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_tend  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_tend - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_tend - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_tend - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"]*(agri_high_tend - CPE_mean_mu)/CPE_mean_si
  
  
  beta1_tend_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_tend - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_tend - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_tend - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_tend - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_tend - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_tend - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_tend  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_tend - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_tend - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_tend - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Std. Error"])*(agri_high_tend - CPE_mean_mu)/CPE_mean_si
  
  
  beta1_s1_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_s1 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s1 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s1 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_s1 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_s1 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s1 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s1  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_s1 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"]*(agri_high_s1 - CPE_mean_mu)/CPE_mean_si
  
  
  beta1_s1_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_s1 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s1 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s1 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_s1 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_s1 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s1 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s1  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s1 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s1 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_s1 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Std. Error"])*(agri_high_s1 - CPE_mean_mu)/CPE_mean_si
  
 beta1_s2_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_s2 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s2 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s2 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_s2 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_s2 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s2 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s2  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s2 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s2 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_s2 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"]*(agri_high_s2 - CPE_mean_mu)/CPE_mean_si
  
  
  beta1_s2_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_s2 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s2 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s2 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_s2 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_s2 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s2 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s2  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s2 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s2 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_s2 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Std. Error"])*(agri_high_s2 - CPE_mean_mu)/CPE_mean_si
  
  beta1_s3_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_s3 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s3 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s3 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_s3 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_s3 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s3 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s3  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_s3 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"]*(agri_high_s3 - CPE_mean_mu)/CPE_mean_si
  
  
  beta1_s3_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_s3 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s3 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s3 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_s3 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_s3 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s3 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s3  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s3 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s3 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_s3 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Std. Error"])*(agri_high_s3 - CPE_mean_mu)/CPE_mean_si
  
 beta1_s4_signif <- mod_coef[which(row.names(mod_coef)=="year"),"Estimate"] +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"]*(d_impervious_s4 - d_impervious_mu)/d_impervious_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"]*(d_tempspring_s4 - d_tempspring_mu)/d_tempspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"]*(d_tempspringvar_s4 - d_tempspringvar_mu)/d_tempspringvar_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"]*(d_precspring_s4 - d_precspring_mu)/d_precspring_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"]*(d_shannon_s4 - d_shannon_mu)/d_shannon_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"]*(protectedarea_perc_s4 - protectedarea_perc_mu)/protectedarea_perc_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"]*(d_treedensity_s4  - d_treedensity_mu)/d_treedensity_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"]*(forest_lowmedium_s4 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"]*(forest_high_s4 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"]*(d_agri_s4 - d_agri_mu)/d_agri_si +
    mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"]*(agri_high_s4 - CPE_mean_mu)/CPE_mean_si
  
  
  beta1_s4_sample_signif <- rnorm(nb_rep,mod_coef[which(row.names(mod_coef)=="year"),"Estimate"], sd=mod_coef[which(row.names(mod_coef)=="year"),"Std. Error"]) +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_impervious"),"Std. Error"])*(d_impervious_s4 - d_impervious_mu)/d_impervious_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrping"),"Std. Error"])*(d_tempspring_s4 - d_tempspring_mu)/d_tempspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_tempsrpingvar"),"Std. Error"])*(d_tempspringvar_s4 - d_tempspringvar_mu)/d_tempspringvar_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_precspring"),"Std. Error"])*(d_precspring_s4 - d_precspring_mu)/d_precspring_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_shannon"),"Std. Error"])*(d_shannon_s4 - d_shannon_mu)/d_shannon_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:protectedarea_perc"),"Std. Error"])*(protectedarea_perc_s4 - protectedarea_perc_mu)/protectedarea_perc_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_treedensity"),"Std. Error"])*(d_treedensity_s4  - d_treedensity_mu)/d_treedensity_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_lowmedium"),"Std. Error"])*(forest_lowmedium_s4 - eulandsystem_forest_lowmedium_mu)/eulandsystem_forest_lowmedium_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:eulandsystem_forest_high"),"Std. Error"])*(forest_high_s4 - eulandsystem_forest_high_mu)/eulandsystem_forest_high_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:d_agri"),"Std. Error"])*(d_agri_s4 - d_agri_mu)/d_agri_si +
    rnorm(nb_rep,mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Estimate"], sd=mod_coef_signif[which(row.names(mod_coef_signif)=="year:CPE_mean"),"Std. Error"])*(agri_high_s4 - CPE_mean_mu)/CPE_mean_si
  
  
  return(data.frame(trend_past=beta1_past, sd_past=sd(beta1_past_sample), trend_tend=beta1_tend,sd_tend=sd(beta1_tend_sample),trend_s1=beta1_s1,sd_s1=sd(beta1_s1_sample),
                    trend_s2=beta1_s2,sd_s2=sd(beta1_s2_sample),trend_s3=beta1_s3,sd_s3=sd(beta1_s3_sample),
                    trend_s4=beta1_s4,sd_s4=sd(beta1_s4_sample),
                    trend_past_signif=beta1_past_signif, sd_past_signif=sd(beta1_past_sample_signif),trend_tend_signif=beta1_tend_signif,sd_tend_signif=sd(beta1_tend_sample_signif),trend_s1_signif=beta1_s1_signif,sd_s1_signif=sd(beta1_s1_sample_signif),
                    trend_s2_signif=beta1_s2_signif,sd_s2_signif=sd(beta1_s2_sample_signif),trend_s3_signif=beta1_s3_signif,sd_s3_signif=sd(beta1_s3_sample_signif),
                    trend_s4_signif=beta1_s4_signif,sd_s4_signif=sd(beta1_s4_sample_signif)))
}


overall_mean_sd_trend_FR <- function(data){
  n <- length(na.omit(data$trend_s1))
  mu_past <- exp(mean(data$trend_past,na.rm=TRUE))
  var_past <- (sum(data$sd_past^2 + data$trend_past^2, na.rm = TRUE))/n - mean(data$trend_past,na.rm=TRUE)^2
  sd_past <- sqrt(mu_past^2*var_past)
  se_past <- mu_past/sqrt(n)*sd(data$trend_past)
  mu_tend <- exp(mean(data$trend_tend,na.rm=TRUE))
  var_tend <- (sum(data$sd_tend^2 + data$trend_tend^2, na.rm = TRUE))/n - mean(data$trend_tend,na.rm=TRUE)^2
  sd_tend <- sqrt(mu_tend^2*var_tend)
  se_tend <- mu_tend/sqrt(n)*sd(data$trend_tend)
  mu_s1 <- exp(mean(data$trend_s1,na.rm=TRUE))
  var_s1 <- (sum(data$sd_s1^2 + data$trend_s1^2, na.rm = TRUE))/n - mean(data$trend_s1,na.rm=TRUE)^2
  sd_s1 <- sqrt(mu_s1^2*var_s1)
  se_s1 <- mu_s1/sqrt(n)*sd(data$trend_s1)
  mu_s2 <- exp(mean(data$trend_s2,na.rm=TRUE))
  var_s2 <- (sum(data$sd_s2^2 + data$trend_s2^2, na.rm = TRUE))/n - mean(data$trend_s2,na.rm=TRUE)^2
  sd_s2 <- sqrt(mu_s2^2*var_s2)
  se_s2 <- mu_s2/sqrt(n)*sd(data$trend_s2)
  mu_s3 <- exp(mean(data$trend_s3,na.rm=TRUE))
  var_s3 <- (sum(data$sd_s3^2 + data$trend_s3^2, na.rm = TRUE))/n - mean(data$trend_s3,na.rm=TRUE)^2
  sd_s3 <- sqrt(mu_s3^2*var_s3)
  se_s3 <- mu_s3/sqrt(n)*sd(data$trend_s3)
  mu_s4 <- exp(mean(data$trend_s4,na.rm=TRUE))
  var_s4 <- (sum(data$sd_s4^2 + data$trend_s4^2, na.rm = TRUE))/n - mean(data$trend_s4,na.rm=TRUE)^2
  sd_s4 <- sqrt(mu_s4^2*var_s4)
  se_s4 <- mu_s4/sqrt(n)*sd(data$trend_s4)
 
  mu_past_signif <- exp(mean(data$trend_past_signif,na.rm=TRUE))
  var_past_signif <- (sum(data$sd_past_signif^2 + data$trend_past_signif^2, na.rm = TRUE))/n - mean(data$trend_past_signif,na.rm=TRUE)^2
  sd_past_signif <- sqrt(mu_past_signif^2*var_past_signif)
  se_past_signif <- mu_past_signif/sqrt(n)*sd(data$trend_past_signif)
  mu_tend_signif <- exp(mean(data$trend_tend_signif,na.rm=TRUE))
  var_tend_signif <- (sum(data$sd_tend_signif^2 + data$trend_tend_signif^2, na.rm = TRUE))/n - mean(data$trend_tend_signif,na.rm=TRUE)^2
  sd_tend_signif <- sqrt(mu_tend_signif^2*var_tend_signif)
  se_tend_signif <- mu_tend_signif/sqrt(n)*sd(data$trend_tend_signif)
  mu_s1_signif <- exp(mean(data$trend_s1_signif,na.rm=TRUE))
  var_s1_signif <- (sum(data$sd_s1_signif^2 + data$trend_s1_signif^2, na.rm = TRUE))/n - mean(data$trend_s1_signif,na.rm=TRUE)^2
  sd_s1_signif <- sqrt(mu_s1_signif^2*var_s1_signif)
  se_s1_signif <- mu_s1_signif/sqrt(n)*sd(data$trend_s1_signif)
  mu_s2_signif <- exp(mean(data$trend_s2_signif,na.rm=TRUE))
  var_s2_signif <- (sum(data$sd_s2_signif^2 + data$trend_s2_signif^2, na.rm = TRUE))/n - mean(data$trend_s2_signif,na.rm=TRUE)^2
  sd_s2_signif <- sqrt(mu_s2_signif^2*var_s2_signif)
  se_s2_signif <- mu_s2_signif/sqrt(n)*sd(data$trend_s2_signif)
  mu_s3_signif <- exp(mean(data$trend_s3_signif,na.rm=TRUE))
  var_s3_signif <- (sum(data$sd_s3_signif^2 + data$trend_s3_signif^2, na.rm = TRUE))/n - mean(data$trend_s3_signif,na.rm=TRUE)^2
  sd_s3_signif <- sqrt(mu_s3_signif^2*var_s3_signif)
  se_s3_signif <- mu_s3_signif/sqrt(n)*sd(data$trend_s3_signif)
  mu_s4_signif <- exp(mean(data$trend_s4_signif,na.rm=TRUE))
  var_s4_signif <- (sum(data$sd_s4_signif^2 + data$trend_s4_signif^2, na.rm = TRUE))/n - mean(data$trend_s4_signif,na.rm=TRUE)^2
  sd_s4_signif <- sqrt(mu_s4_signif^2*var_s4_signif)
  se_s4_signif <- mu_s4_signif/sqrt(n)*sd(data$trend_s4_signif)
  
  return(data.frame(mu_past,sd_past,se_past,mu_tend,sd_tend,se_tend,mu_s1,sd_s1,se_s1,mu_s2,sd_s2,se_s2,mu_s3,sd_s3,se_s3,
                    mu_s4,sd_s4,se_s4,
                    mu_past_signif,sd_past_signif,se_past_signif,mu_tend_signif,sd_tend_signif,se_tend_signif,mu_s1_signif,sd_s1_signif,se_s1_signif,mu_s2_signif,sd_s2_signif,se_s2_signif,mu_s3_signif,sd_s3_signif,se_s3_signif,
                    mu_s4_signif,sd_s4_signif,se_s4_signif,n))
}
