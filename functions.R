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
min_site_number_per_species <- 80
min_occurence_species <- 200
family <- "quasipoisson"
pressure_name <- c("impervious","treedensity","woodprod","drymatter",
                   "tempspring","tempspringvar","precspring",#"precspringvar","humidityspring",
                   "protectedarea_perc",#"pesticide_nodu",
                   "smallwoodyfeatures","shannon","eulandsystem_cat",
                   "grassland","farmland","low_farmland","high_farmland")

gam_species_PLS <- function(bird_data,pressure_data,site_data,
                             pressure_name = c("impervious","treedensity","woodprod","drymatter",
                                               "tempspring","tempspringvar","precspring",#"precspringvar","humidityspring",
                                               "protectedarea_perc",#"pesticide_nodu",
                                               "smallwoodyfeatures","shannon","eulandsystem_cat",
                                               "grassland","farmland","low_farmland","high_farmland"),
                             min_site_number_per_species = 80,
                             min_occurence_species=200,
                             family="quasipoisson"){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","time_effort","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$time_effort)) > length(unique(poisson_df$scheme_code)) & length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$time_effort <- scale(poisson_df$time_effort)
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- scales::rescale(poisson_df$count)
  
  if(length(pressure_name) > 1){
    formula_gam <- paste("count_scale_all ~", paste("year:",pressure_name,sep="", collapse = " + "))
  }else{
    formula_gam <- paste("count_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
  }
  
  col_names <- c("(Intercept)",pressure_name)
  
  if(nrow(poisson_df) >= min_occurence_species){
    
    ### global poisson model
    
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    

    if(global_mod$converged){
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
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
          
          if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            result_i <- as.matrix(result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            result_i <- as.matrix(result_i[grep("area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            result_i <- as.matrix(result_i[grep("scheme_code",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
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
        
        row.names(result_site) <- col_names
        
        return(result_site)
      },
      min_site_number_per_species=min_site_number_per_species,poisson_df=poisson_df,
      .progress="text")
      
      if(!is.na(dim(result_all_site)[3])){
        result_all_site <- aperm(result_all_site, c(2,3,1))
        
        if(dim(result_all_site)[3] > 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:length(col_names),1,])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:length(col_names),4,])))
        }
        if(dim(result_all_site)[3] == 1){
          res.poisson_df <- as.data.frame(t(data.frame(result_all_site[1:length(col_names),1])))
          res.poisson_pval <- as.data.frame(t(data.frame(result_all_site[1:length(col_names),4])))
        }
      }
      
      if(is.na(dim(result_all_site)[3])){
        res.poisson_df <- data.frame(matrix(NA,nrow=1,ncol=length(col_names)))
        res.poisson_pval <- matrix(1,nrow=1,ncol=length(col_names))
      }
      
      
      
      res.poisson_df[res.poisson_pval > 0.05] <- NA 
      
      if(is.na(dim(result_all_site)[3])){
        
        res.poisson_df$PLS <- NA
        
      }else{
        
        res.poisson_df$PLS <- gsub("X","",row.names(res.poisson_df))
        
      }
      
      global_mod_coef <- summary(global_mod)$p.table[grep("scheme_code|area_sampled_m2|time_effort",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
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
      #ggplot() + geom_sf() +  geom_sf(data=res.poisson_sf, aes(fill=exp(`treedensity`))) + scale_fill_gradientn(colors = sf.colors(20))
      
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



bird_data <- droplevels(subsite_data_mainland_trend[which(subsite_data_mainland_trend$sci_name_out == "Alauda arvensis"),])
pressure_data <- press_mainland_trend_scale
site_data <- site_mainland_sf_reproj
min_site_number_per_species <- 80
min_occurence_species <- 200
family <- "quasipoisson"
pressure_name <- c("impervious","treedensity","drymatter",
                   "tempspring","tempspringvar","precspring",
                   "protectedarea_perc","smallwoodyfeatures","shannon",
                   "eulandsystem_cat","grassland","farmland")

gam_species_PLS <- function(bird_data,pressure_data,site_data,
                            pressure_name = c("impervious","treedensity","drymatter",
                                              "tempspring","tempspringvar","precspring",
                                              "protectedarea_perc","smallwoodyfeatures","shannon",
                                              "eulandsystem_cat","grassland","farmland"),
                            min_site_number_per_species = 80,
                            min_occurence_species=200,
                            family="quasipoisson"){
  
  species_press_data_year <- merge(bird_data, pressure_data[which(pressure_data$siteID %in% unique(bird_data$siteID) & pressure_data$year %in% unique(bird_data$year)),], by =c("siteID","year"), all.x=TRUE)
  
  poisson_df <- na.omit(species_press_data_year[,c("siteID","count","year","time_effort","area_sampled_m2","scheme_code","Long_LAEA","Lat_LAEA",
                                                   pressure_name,"PLS")])
  
  
  poisson_df$year <- poisson_df$year - 2000
  
  if(length(table(poisson_df$time_effort)) > length(unique(poisson_df$scheme_code)) & length(table(poisson_df$area_sampled_m2)) > length(unique(poisson_df$scheme_code))){
    one_scheme_time_area <- 0 
    poisson_df$time_effort <- scale(poisson_df$time_effort)
    poisson_df$area_sampled_m2 <- scale(poisson_df$area_sampled_m2)
  }else{
    one_scheme_time_area <- 1
  }
  
  poisson_df$count_scale_all <- scales::rescale(poisson_df$count)
  
  if(length(pressure_name) > 1){
    formula_gam <- "count_scale_all ~ year:impervious + year:impervious:eulandsystem_cat + year:treedensity + year:treedensity:eulandsystem_cat + 
    year:drymatter + year:tempspring + year:tempspringvar + year:precspring + year:protectedarea_perc + year:smallwoodyfeatures + year:shannon +
    year:grassland + year:grassland:eulandsystem_cat + year:farmland + year:farmland:eulandsystem_cat"
  }else{
    formula_gam <- paste("count_scale_all ~", paste(pressure_name,sep="", collapse = " + "))
  }
  
  col_names <- c("(Intercept)","year:impervious","year:treedensity","year:drymatter","year:tempspring","year:tempspringvar",
                 "year:precspring","year:protectedarea_perc","year:smallwoodyfeatures","year:shannon",                                     
                 "year:grassland","year:farmland","year:impervious:eulandsystem_catmedium_intensity","year:impervious:eulandsystem_cathigh_intensity",
                 "year:treedensity:eulandsystem_catmedium_intensity","year:treedensity:eulandsystem_cathigh_intensity",
                 "year:grassland:eulandsystem_catmedium_intensity","year:grassland:eulandsystem_cathigh_intensity",
                 "year:farmland:eulandsystem_catmedium_intensity","year:farmland:eulandsystem_cathigh_intensity")
  
  if(nrow(poisson_df) >= min_occurence_species & length(table(poisson_df$eulandsystem_cat)) > 1){
    
    ### global poisson model
    
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 0){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) > 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    if(length(unique(poisson_df$scheme_code)) == 1 && one_scheme_time_area == 1){
      global_mod <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                        family=family, data=poisson_df)
    }
    
    
    if(global_mod$converged){
      
      unique_poisson_df <- distinct(poisson_df, Long_LAEA, Lat_LAEA,.keep_all = TRUE)
      
      result_all_site <- daply(unique_poisson_df,.(PLS),.fun=function(x,min_site_number_per_species,poisson_df){
        
        if(nrow(x) >= min_site_number_per_species & length(table(x$eulandsystem_cat)) > 1){
          
          poisson_df_i <- poisson_df[which(poisson_df$PLS == unique(x$PLS)),]
          
          if(length(table(poisson_df_i$time_effort)) > length(unique(poisson_df_i$scheme_code)) & length(table(poisson_df_i$area_sampled_m2)) > length(unique(poisson_df_i$scheme_code))){
            one_scheme_time_area <- 0 
            poisson_df_i$time_effort <- scale(poisson_df_i$time_effort)
            poisson_df_i$area_sampled_m2 <- scale(poisson_df_i$area_sampled_m2)
          }else{
            one_scheme_time_area <- 1
          }
          
          if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 0){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("scheme_code|area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 0){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("time_effort","area_sampled_m2","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("area_sampled_m2|time_effort",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) > 1 && one_scheme_time_area == 1){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("scheme_code","te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            result_i <- summary(res.poisson_i)$p.table
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- as.matrix(result_i[grep("scheme_code",row.names(result_i),invert = TRUE),])
          }
          if(length(unique(poisson_df_i$scheme_code)) == 1 && one_scheme_time_area == 1){
            res.poisson_i <- gam(as.formula(paste(formula_gam,sep=" + ",paste(c("te(Long_LAEA,Lat_LAEA,bs='tp',fx=TRUE,k=3)"), collapse = " + "))),
                                 family=family, data=poisson_df_i)
            dev_exp <- summary(res.poisson_i)$dev.expl
            n_obs <- summary(res.poisson_i)$n
            result_i <- summary(res.poisson_i)$p.table
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
      
      global_mod_coef <- summary(global_mod)$p.table[grep("scheme_code|area_sampled_m2|time_effort",row.names(summary(global_mod)$p.table),invert = TRUE),]
      
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
