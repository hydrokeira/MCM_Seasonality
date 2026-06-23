setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")
mcm_flow<-read.csv("MCM_FlowPeriods_Cleaned.csv")

years<-mcm_flow %>%
  group_by(stream) %>%
  summarise(min_year=min(variable), max_year=max(variable), num_years=n_distinct(variable))

# complete_years<-mcm_flow %>%
#   group_by(stream, variable) %>%
#   filter(!is.na(conc_clean)) %>%
#   summarise(num_obs=n()) %>%
#   filter(num_obs == 12) %>%
#   group_by(stream) %>%
#   mutate(comp_years=n(), unique=paste(stream, variable)) 

complete_years <- mcm_flow %>%
  filter(!is.na(conc_clean)) %>%
  group_by(stream, variable) %>%
  summarise(
    n_required = length(intersect(unique(Period), 2:11)),
    .groups = "drop"
  ) %>%
  filter(n_required == 10) %>%
  mutate(unique2=paste(stream, variable))

complete_years<-mcm_flow %>%
  mutate(unique2=paste(stream, variable)) %>%
  filter(unique2 %in% complete_years$unique2) %>%
  group_by(stream, variable) %>%
  mutate(norm_conc = scale(conc_clean, center = T))

complete_years %>%
  ggplot(aes(Period, conc_clean))+geom_line(aes(group = variable))+facet_wrap(~stream, scales="free_y")+
  theme_bw()+theme(text = element_text(size = 15))+
  labs(x="Time", y="Concentration (mg/L)")

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")

avg_flow_periods<-complete_years %>%
  group_by(stream) %>%
  mutate(norm_conc=scale(conc_clean, center = T)) %>%
  ungroup() %>%
  group_by(stream, Period) %>%
  summarise(mean_conc=mean(conc_clean, na.rm=T), mean_norm_conc=mean(norm_conc, na.rm=T))

ggplot(avg_flow_periods, aes(Period, mean_norm_conc))+geom_line()+
  facet_wrap(~stream, scale="free_y")+theme_classic()+
  labs(x="Flow Period", y="Concentration (mg/L)")+
  theme(text = element_text(size = 15))

avg_wide<-avg_flow_periods %>%
  select(-mean_conc) %>%
  pivot_wider(names_from = Period, values_from = mean_norm_conc)

dist_mat_all<-dtwDist(avg_wide[2:13], avg_wide[2:13])

# 1. Perform hierarchical clustering
hc_result <- hclust(as.dist(dist_mat_all), method = "complete")

# 2. Plot the dendrogram to visualize the trees
plot(hc_result, hang = -1)

# 3. Cut the tree into a specific number of clusters (e.g., k = 3)
cluster_assignments <- cutree(hc_result, k = 4)

# 4. View cluster assignment for each observation
print(cluster_assignments)

avg_wide$cluster<-cluster_assignments

avg_wide %>%
  pivot_longer(cols = 2:13, names_to = "month", values_to = "value") %>%
  ggplot(aes(as.numeric(month), value))+geom_line(aes(group = stream, col=stream), lwd=1)+facet_wrap(~cluster)+
  theme_bw()+theme(text = element_text(size = 20))

centroids<-avg_wide %>%
  pivot_longer(cols = 2:13, names_to = "period", values_to = "value") %>%
  group_by(cluster, period) %>%
  summarise(centroid_value=mean(value)) %>%
  mutate(period=as.numeric(period))

centroids %>%
  ggplot(aes(as.numeric(period), centroid_value))+geom_line(lwd=1)+facet_wrap(~cluster)+
  theme_bw()+theme(text = element_text(size = 20))

centroids_wide<-centroids %>%
  pivot_wider(names_from = period, values_from = centroid_value)

centroids_wide<-centroids_wide[,c(1:2, 6:13,3:5)]

complete_years_norm_wide<-complete_years %>%
  select(Period, variable, stream, norm_conc) %>%
  pivot_wider(names_from = Period, values_from = norm_conc)

complete_years_norm_wide<-complete_years_norm_wide[,c(1:3,7:14,4:6)]

dtw_dist<-list()

for (i in 1:nrow(complete_years_norm_wide)) {
  
  print(i)
  
  centroid_line<-complete_years_norm_wide[i,3:14]
  centroid_line <- centroid_line %>%
    select(where(~ !any(is.na(.))))
  
  centroid_line<-unlist(as.vector(centroid_line))
  
  distance<-list()
  
  for (k in 1:4) {
    
    centroid_line2<-unlist(as.vector(centroids_wide[k,2:13]))
    
    distance[[k]]<-dtw(x=centroid_line, y=centroid_line2, 
                       window.type = "sakoechiba",
                       window.size=length(centroid_line2)-length(centroid_line))$distance
  }
  
  dtw_dist[[i]]<-do.call(cbind, distance)
  
}

dtw_df<-data.frame(do.call(rbind, dtw_dist))

dtw_df$clust <- apply(dtw_df[, 1:4], 1, which.min)
dtw_df$stream<-complete_years_norm_wide$stream
dtw_df$year<-complete_years_norm_wide$variable

complete_years_norm_wide$clust<-dtw_df$clust

ggplot(dtw_df, aes(year, stream, fill=as.factor(clust)))+geom_tile()+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="Year", y="", fill="Cluster")

complete_years_norm_wide %>%
  pivot_longer(cols = 3:14, names_to = "period", values_to = "norm_conc") %>%
  mutate(period=as.numeric(period)) %>%
  ggplot(aes(period, norm_conc, col=as.factor(clust)))+
  geom_line(aes(group=variable), lwd=0.8)+facet_wrap(~stream)+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="Period", y="Z-Score Normalized Concentration", col="Cluster")
