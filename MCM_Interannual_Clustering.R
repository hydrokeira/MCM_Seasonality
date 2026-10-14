require(dtw)
require(ggpubr)
require(dplyr)
require(ggplot2)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")
mcm_flow<-read.csv("MCM_FlowPeriods_Cleaned_GenConc.csv")

mcm_flow<-mcm_flow %>%
  filter(!stream %like% "Harnish")

sum(mcm_flow$Q_min_Q=="yes", na.rm=T)
sum(mcm_flow$Q_min_Q=="no", na.rm=T)

648/(3144+648)

stream_order<-c("Lawson Creek at B3", "Andersen Creek at H1", "Onyx River at Lower Wright Weir",
                "Priscu Stream at B1", "Onyx River at Lake Vanda Weir", "Commonwealth Stream at C1",
                "Von Guerard Stream at F6", "Crescent Stream at F8", "Delta Stream at F10",
                "Green Creek at F9", "Canada Stream at F1")

# Define the start and end dates using as.Date()
start_date <- as.Date("2025-12-05")
end_date   <- as.Date("2026-01-31")

# Generate the sequence jumping by 3 days
date_sequence <- seq(from = start_date, to = end_date, by = 5)

# 3. Format the sequence to hide the year
dates_no_year <- format(date_sequence, format = "%b %d")

# Print the result
print(dates_no_year)

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
    n_required = length(intersect(unique(Period), 3:11)),
    total_num=n(),
    .groups = "drop"
  ) %>%
  filter(n_required == 9) %>%
  mutate(unique2=paste(stream, variable)) %>%
  mutate(stream=factor(stream, levels = stream_order))

pdf("Years_Included_Tile.pdf", width = 16, height = 4)

ggplot(complete_years, aes(variable, stream))+geom_tile(aes(fill=as.factor(total_num)), col="white")+theme_classic()+
  theme(text = element_text(size = 20))+labs(y="", x="Year",fill="Number of\nFlow Periods")+
  scale_fill_manual(values = grey.colors(n=4, rev=T))+
  scale_y_discrete(limits=rev)

dev.off()

years_summary<-complete_years %>%
  group_by(stream) %>%
  summarise(num_years=n(), min_year=min(variable), max_year=max(variable))

write.csv(years_summary, "Streams_Year_Summary_Updated.csv")

complete_years<-mcm_flow %>%
  mutate(unique2=paste(stream, variable)) %>%
  filter(unique2 %in% complete_years$unique2) %>%
  group_by(stream, variable) %>%
  mutate(norm_conc = scale(conc_clean, center = T))

pdf("AllYears_ConcCurves.pdf", width = 16, height = 9)

complete_years %>%
  mutate(stream=factor(stream, levels = stream_order)) %>%
  ggplot(aes(as.factor(Period), conc_clean))+geom_line(aes(group = variable))+facet_wrap(~stream, scales="free_y")+
  theme_classic()+theme(text = element_text(size = 20), axis.text.x = element_text(angle = 55, hjust=1))+
  labs(x="Date", y="Concentration (mg/L)")+scale_x_discrete(labels = dates_no_year)

dev.off()

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")

avg_flow_periods<-complete_years %>%
  dplyr::group_by(stream) %>%
  mutate(norm_conc=scale(conc_clean, center = T)) %>%
  ungroup() %>%
  group_by(stream, Period) %>%
  summarise(mean_conc=mean(conc_clean, na.rm=T), mean_norm_conc=mean(norm_conc, na.rm=T))

pdf("Avg_Flow_Periods_Normalized.pdf", width = 14, height = 9)

ggplot(avg_flow_periods, aes(Period, mean_norm_conc))+geom_line()+
  facet_wrap(~stream)+theme_classic()+
  labs(x="Flow Period", y="Concentration (mg/L)")+
  theme(text = element_text(size = 15))

dev.off()

avg_wide<-avg_flow_periods %>%
  dplyr::select(-mean_conc) %>%
  pivot_wider(names_from = Period, values_from = mean_norm_conc)

dist_mat_all<-dtwDist(avg_wide[2:13], avg_wide[2:13])

# 1. Perform hierarchical clustering
hc_result <- hclust(as.dist(dist_mat_all), method = "complete")

pdf("HClust_Tree.pdf", width = 8, height = 6)

# 2. Plot the dendrogram to visualize the trees
plot(hc_result, hang = -1)

dev.off()

# 3. Cut the tree into a specific number of clusters (e.g., k = 3)
cluster_assignments <- cutree(hc_result, k = 3)

# 4. View cluster assignment for each observation
print(cluster_assignments)

avg_wide$cluster<-cluster_assignments

stats<-read.csv("MCM_StreamLength_Biomass.csv")

stats<-stats %>%
  mutate(stream = case_when(
    Stream=="Canada"~"Canada Stream at F1",
    Stream=="Green"~"Green Creek at F9",
    Stream=="Delta"~"Delta Stream at F10",
    Stream=="Crescent"~"Crescent Stream at F8",
    Stream=="Harnish"~"Harnish Creek at F7",
    Stream=="Von Guerard"~"Von Guerard Stream at F6",
    Stream=="Andersen"~"Andersen Creek at H1",
    Stream=="Priscu"~"Priscu Stream at B1",
    Stream=="Lawson"~"Lawson Creek at B3",
    Stream=="Commonwealth"~"Commonwealth Stream at C1",
    Stream=="Onyx_LVW"~"Onyx River at Lake Vanda Weir",
    Stream=="Onyx_LWW"~"Onyx River at Lower Wright Weir",
  ))

simple_stream<-c("Lawson", "Andersen", "Onyx_LWW", "Priscu", "Onyx_LVW",
                 "Commonwealth", "Von Guerard", "Crescent", "Harnish",
                 "Delta", "Green", "Canada")

complete_years_stats<-complete_years %>%
  left_join(stats)

avg_Si_Q<-complete_years %>%
  dplyr::group_by(stream) %>%
  summarise(mean_conc=mean(conc_clean, na.rm=T), mean_Q=mean(Q, na.rm=T),cv_q=sd(Q, na.rm = T)/mean_Q)

pdf("Clusters_Avg_Biomass.pdf", width = 17, height = 6)

avg_wide %>%
  pivot_longer(cols = 2:13, names_to = "month", values_to = "value") %>%
  left_join(stats) %>%
  mutate(stream=factor(stream, levels = stream_order),
         Biomass_Class=factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  mutate(month=as.character(month), month=factor(month, levels = seq(1,12,1))) %>%
  mutate(cluster=as.character(cluster), cluster=factor(cluster, levels = c(2,1,3,4))) %>%
  ggplot(aes(month, value))+
  #geom_line(aes(group = stream, col=Biomass_Class), lwd=1)+
  geom_line(aes(group = stream, col=stream), lwd=1)+
  facet_wrap(~cluster, nrow=1)+
  theme_classic()+
  theme(text=element_text(size=20),legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  # scale_color_manual(values = c(
  # "low" = "#c7e9c0",
  # "moderate" = "#74c476",
  # "high" = "#238b45"))+
  scale_x_discrete(labels = dates_no_year, breaks=seq(1,12,1))+
  labs(x="", y="Z-Score Normalized Concentration", col="Biomass\nClass")

dev.off()

#### now do annual clustering####

centroids<-avg_wide %>%
  pivot_longer(cols = 2:13, names_to = "period", values_to = "value") %>%
  group_by(cluster, period) %>%
  summarise(centroid_value=mean(value)) %>%
  mutate(period=as.numeric(period))

new_facet_names <- c(
  "2" = "flat",
  "1" = "increasing",
  "3" = "U-shaped"
)

pdf("Cluster_Centroids.pdf", width = 10, height = 4)

centroids %>%
  mutate(cluster=as.character(cluster), cluster=factor(cluster, levels = c(2,1,3))) %>%
  ggplot(aes(as.factor(period), centroid_value))+geom_line(lwd=1, group=1)+
  facet_wrap(~cluster, labeller = as_labeller(new_facet_names))+
  theme_classic()+
  theme(text = element_text(size = 15),axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = dates_no_year, breaks=seq(1,12,1))+
  labs(x="", y="Z-Score Normalized Concentration")

dev.off()

centroids_wide<-centroids %>%
  pivot_wider(names_from = period, values_from = centroid_value)

centroids_wide<-centroids_wide[,c(1:2, 6:13,3:5)]

complete_years_norm_wide<-complete_years %>%
  dplyr::select(Period, variable, stream, norm_conc) %>%
  pivot_wider(names_from = Period, values_from = norm_conc)

complete_years_norm_wide<-complete_years_norm_wide[,c(1:3,7:14,4:6)]

dtw_dist<-list()

for (i in 1:nrow(complete_years_norm_wide)) {
  
  print(i)
  
  centroid_line<-complete_years_norm_wide[i,3:14]
  centroid_line <- centroid_line %>%
    dplyr::select(where(~ !any(is.na(.))))
  
  centroid_line<-unlist(as.vector(centroid_line))
  
  distance<-list()
  
  for (k in 1:3) {
    
    centroid_line2<-unlist(as.vector(centroids_wide[k,2:13]))
    
    distance[[k]]<-dtw(x=centroid_line, y=centroid_line2, 
                       window.type = "sakoechiba",
                       window.size=length(centroid_line2)-length(centroid_line))$distance
  }
  
  dtw_dist[[i]]<-do.call(cbind, distance)
  
}

dtw_df<-data.frame(do.call(rbind, dtw_dist))

table(dtw_df$clust)

dtw_df$clust <- apply(dtw_df[, 1:3], 1, which.min)
dtw_df$stream<-complete_years_norm_wide$stream
dtw_df$year<-complete_years_norm_wide$variable

write.csv(dtw_df, "Annaul_Cluster_Membership.csv")

complete_years_norm_wide$clust<-dtw_df$clust

pdf("Cluster_Tile.pdf", width = 11, height = 4)

p1<-dtw_df %>%
  mutate(clust=as.character(clust), clust=factor(clust, levels = c(2,1,3))) %>%
  mutate(stream=factor(stream, levels = stream_order)) %>%
  ggplot(aes(year, stream, fill=as.factor(clust)))+geom_tile()+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "right")+
  labs(x="Year", y="", fill="Cluster")+
  scale_fill_manual(values = c("#66C5CC","#F6CF71","#B497E7"))+
  scale_y_discrete(limits=rev)

dev.off()

p1

p2<-complete_years_norm_wide %>%
  pivot_longer(cols = 3:14, names_to = "period", values_to = "norm_conc") %>%
  mutate(period=as.numeric(period)) %>%
  mutate(clust=as.character(clust), clust=factor(clust, levels = c(2,1,3,4))) %>%
  mutate(period=as.character(period), period=factor(period, levels = seq(1,12,1))) %>%
  mutate(stream=factor(stream, levels = stream_order)) %>%
  ggplot(aes(period, norm_conc, col=as.factor(clust)))+
  geom_line(aes(group=variable), lwd=0.8)+facet_wrap(~stream)+
  theme_classic()+theme(text = element_text(size = 20), axis.text.x = element_text(angle = 55, hjust = 1))+
  labs(x="Period", y="Z-Score Normalized Concentration", col="Cluster")+
  scale_color_manual(values = c("#66C5CC","#F6CF71","#B497E7","#B3B3B3"))+
  scale_x_discrete(labels = dates_no_year, breaks=seq(1,12,1))

p2

pdf("Annual_Cluster_Tile.pdf", width = 16, height = 13)

ggarrange(p2, p1, nrow = 2, heights = c(3, 1.7))

dev.off()

annual_Q<-complete_years %>%
  dplyr::group_by(stream, variable) %>%
  summarise(mean_Q=mean(Q, na.rm=T), max_Q=max(Q, na.rm=T)) %>%
  mutate(year=variable) 

pdf("Annual_Cluster_Discharge.pdf", width = 18, height = 10)

dtw_df %>%
  mutate(clust=as.character(clust), clust=factor(clust, levels = c(2,1,3))) %>%
  left_join(annual_Q) %>%
  mutate(stream=factor(stream, levels = stream_order)) %>%
  ggplot(aes(clust, mean_Q, fill=clust))+geom_boxplot(outliers = F)+
  geom_jitter(col="black", width = .2)+
  theme_classic()+theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="", y="Mean Annual Q (cms)", fill="Cluster")+
  scale_fill_manual(values = c("#66C5CC","#F6CF71","#B497E7"))+
  scale_color_manual(values = c("#66C5CC","#F6CF71","#B497E7"))+
  scale_alpha_manual(values = c(0.3, 1))+
  facet_wrap(~stream, scales="free_y")+
  scale_x_discrete(labels=c("flat","increasing", "U-shaped", "decreasing"))

dev.off()

table(dtw_df$clust)

110+37+72

72/219

get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}


modal_cluster<-dtw_df %>%
  group_by(stream) %>%
  summarise(mode_clust=get_mode(clust))

write.csv(modal_cluster, "Modal_Cluster.csv")  

modal_cluster<-read.csv("Modal_Cluster.csv")
  
stats<-read.csv("MCM_StreamLength_Biomass.csv")

stats<-stats %>%
  mutate(stream = case_when(
    Stream=="Canada"~"Canada Stream at F1",
    Stream=="Green"~"Green Creek at F9",
    Stream=="Delta"~"Delta Stream at F10",
    Stream=="Crescent"~"Crescent Stream at F8",
    Stream=="Harnish"~"Harnish Creek at F7",
    Stream=="Von Guerard"~"Von Guerard Stream at F6",
    Stream=="Andersen"~"Andersen Creek at H1",
    Stream=="Priscu"~"Priscu Stream at B1",
    Stream=="Lawson"~"Lawson Creek at B3",
    Stream=="Commonwealth"~"Commonwealth Stream at C1",
    Stream=="Onyx_LVW"~"Onyx River at Lake Vanda Weir",
    Stream=="Onyx_LWW"~"Onyx River at Lower Wright Weir",
  ))

simple_stream<-c("Lawson", "Andersen", "Onyx_LWW", "Priscu", "Onyx_LVW",
                 "Commonwealth", "Von Guerard", "Crescent", "Harnish",
                 "Delta", "Green", "Canada")

modal_stats<-modal_cluster %>%
  left_join(stats)

avg_Si_Q<-complete_years %>%
  dplyr::group_by(stream) %>%
  summarise(mean_conc=mean(conc_clean, na.rm=T), mean_Q=mean(Q, na.rm=T))

k1<-modal_stats %>%
  mutate(stream=factor(stream, levels = stream_order),
         Biomass_Class=factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  mutate(cluster=as.character(mode_clust), cluster=factor(mode_clust, levels = c(2,1,3,4))) %>%
  ggplot(aes(cluster, Length.to.gauge..km.))+geom_boxplot(aes(fill=cluster), outliers = F)+
  geom_jitter(width=0.2)+theme_classic()+
  scale_fill_manual(values = c("#F6CF71","#B497E7","#B3B3B3"))+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="", y="Length to Gauge (km)")+
  scale_x_discrete(labels=c("increasing", "U-shaped", "decreasing"))

k1

k2<-modal_stats %>%
  left_join(avg_Si_Q) %>%
  mutate(cluster=as.character(mode_clust), cluster=factor(mode_clust, levels = c(2,1,3,4))) %>%
  ggplot(aes(cluster, mean_conc))+geom_boxplot(aes(fill=cluster), outliers = F)+
  geom_jitter(width=0.2)+theme_classic()+
  scale_fill_manual(values = c("#F6CF71","#B497E7","#B3B3B3"))+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="", y="Mean DSi (mg/L)")+
  scale_x_discrete(labels=c("increasing", "U-shaped", "decreasing"))
k2

k3<-modal_stats %>%
  left_join(avg_Si_Q) %>%
  mutate(norm_Q=mean_Q/Length.to.gauge..km./1000) %>%
  mutate(cluster=as.character(mode_clust), cluster=factor(mode_clust, levels = c(2,1,3,4))) %>%
  ggplot(aes(cluster, log(norm_Q)))+geom_boxplot(aes(fill=cluster), outliers = F)+
  geom_jitter(width=0.2)+theme_classic()+
  scale_fill_manual(values = c("#F6CF71","#B497E7","#B3B3B3"))+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="", y="Log(Normalized Q (m2/s))")+
  scale_x_discrete(labels=c("increasing", "U-shaped", "decreasing"))
k3

dat <- modal_stats %>%
  left_join(avg_Si_Q) %>%
  mutate(
    norm_Q = mean_Q / Length.to.gauge..km. / 1000,
    cluster = factor(as.character(mode_clust))
  ) %>%
  filter(!is.na(norm_Q), !is.na(cluster))

wilcox.test(log(Length.to.gauge..km.) ~ cluster, data = dat)

pdf("Modal_Cluster_Distribution.pdf", width = 14, height = 4)

ggarrange(k1, k2, k3, nrow = 1)

dev.off()
