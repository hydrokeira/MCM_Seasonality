annual<-read.csv("Annaul_Cluster_Membership.csv")
modal<-read.csv("Modal_Cluster.csv")

clust_all<-annual %>%
  left_join(modal[c(2,3)])

clust_all<-clust_all %>%
  mutate(modal_same = ifelse(clust==mode_clust, "yes", "no")) %>%
  group_by(stream) %>%
  mutate(num_years=n(), years_modal = length(which(modal_same=="yes")),clust_unique=length(unique(clust)))

clust_all_sum<-clust_all %>%
  group_by(stream) %>%
  reframe(st=(num_years-years_modal)/num_years, sc=1+((4-clust_unique)/4), s_tot=1-(st/sc)) %>%
  distinct()

write.csv(clust_all_sum, "MCM_Stability.csv")

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

pdf("Regime_Stability.pdf", width = 4, height = 4)

ggplot(clust_stats, aes(Biomass_Class, s_tot))+geom_boxplot(outliers = F, fill=NA)+geom_jitter(size=2, width = 0.15)+
  theme_classic()+labs(x="", y="DSi Regime Stability")+theme(text=element_text(size = 20))

dev.off()
