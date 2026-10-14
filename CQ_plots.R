setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")
mcm_flow<-read.csv("MCM_FlowPeriods_Cleaned.csv")

complete_years <- mcm_flow %>%
  filter(!is.na(conc_clean)) %>%
  group_by(stream, variable) %>%
  summarise(
    n_required = length(intersect(unique(Period), 3:11)),
    total_num=n(),
    .groups = "drop"
  ) %>%
  filter(n_required == 9) %>%
  mutate(unique2=paste(stream, variable))

onyx_years<-complete_years %>%
  filter(stream=="Onyx River at Lake Vanda Weir")

onyx<-mcm_flow %>%
  filter(stream=="Onyx River at Lake Vanda Weir") %>%
  filter(variable %in% onyx_years$variable)

pdf("Onyx_CQ.pdf", width = 12, height = 10)
ggplot(onyx, aes(y=log10(Q), x=log10(conc_clean), col=Period))+
  geom_path(lwd=1)+
  geom_point(size=3)+
  facet_wrap(~variable)+
  scale_color_gradientn(colors = c("lightblue", "yellowgreen", "darkblue"))+
  theme_classic()+
  theme(text = element_text(size=20))+
  labs(x="log(DSi [mg/L])", y="log(Q)")+
  ggtitle("Onyx River @ Lake Vanda")
dev.off()

green_years<-complete_years %>%
  filter(stream=="Green Creek at F9")

green<-mcm_flow %>%
  filter(stream=="Green Creek at F9") %>%
  filter(variable %in% green_years$variable)

pdf("Green_CQ.pdf", width = 12, height = 10)

ggplot(green, aes(y=log10(Q), x=log10(conc_clean), col=Period))+
  geom_path(lwd=1)+
  geom_point(size=3)+
  facet_wrap(~variable)+
  scale_color_gradientn(colors = c("lightblue", "yellowgreen", "darkblue"))+
  theme_classic()+
  theme(text = element_text(size=20))+
  labs(x="log(DSi [mg/L])", y="log(Q)")+
  ggtitle("Green Creek")

dev.off()
