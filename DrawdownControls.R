setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")

mcm<-read.csv("MCM_FlowPeriods_Cleaned.csv")

mcm_avg<-mcm %>% 
  dplyr::group_by(stream, Period) %>%
  dplyr::summarise(mean_conc=mean(conc_clean, na.rm = TRUE))

#pull flow period 8 to look at drawdown
mcm_avg<-mcm_avg %>%
  group_by(stream) %>%
  mutate(intial_si = mean_conc[Period==1], lowest_si = mean_conc[Period==8], diff=lowest_si-intial_si,
         mean_si=mean(mean_conc), prop=(diff/intial_si)*100)

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

pdf("Si_Drawdown_Percent.pdf", width = 10, height = 6)

mcm_avg %>%
  filter(!duplicated(diff)) %>%
  left_join(stats) %>%
  mutate(Biomass_Class = factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  ggplot(aes(x=Stream, y=prop))+geom_bar(stat = "identity", aes(fill=Length.to.gauge..km.))+theme_classic()+
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="", y="Drawdown (% of Initial Concentration)", fill="Length to\nGauge (km)")+
  facet_wrap(~Biomass_Class, scales="free_x", space="free_x")+
  scale_fill_binned(breaks = c(1, 2, 5, 10), palette = "oranges")

dev.off()

pdf("Si_Drawdown_Mean.pdf", width = 10, height = 6)

mcm_avg %>%
  filter(!duplicated(diff)) %>%
  left_join(stats) %>%
  mutate(Biomass_Class = factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  ggplot(aes(x=Stream, y=diff))+geom_bar(stat = "identity", aes(fill=Length.to.gauge..km.))+theme_classic()+
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="", y="Mean Si Drawdown (mg Si/L)", fill="Length to\nGauge (km)")+
  facet_wrap(~Biomass_Class, scales="free_x", space="free_x")+
  scale_fill_binned(breaks = c(1, 2, 5, 10), palette = "oranges")

dev.off()  

biomass<-read.csv("Biomass_Clean.csv")

biomass <- biomass %>%
  mutate(stream = case_when(
    STREAM.NAME=="Canada Gauge"~"Canada Stream at F1",
    STREAM.NAME=="Green Creek"~"Green Creek at F9",
    STREAM.NAME=="Delta Gauge"~"Delta Stream at F10",
    STREAM.NAME=="Crescent"~"Crescent Stream at F8",
    STREAM.NAME=="Harnish"~"Harnish Creek at F7",
    STREAM.NAME=="Von Guerard Gauge"~"Von Guerard Stream at F6",
    STREAM.NAME=="Andersen"~"Andersen Creek at H1",
    STREAM.NAME=="Priscu"~"Priscu Stream at B1",
    STREAM.NAME=="Lawson"~"Lawson Creek at B3",
    STREAM.NAME=="Commonwealth"~"Commonwealth Stream at C1",
    STREAM.NAME=="Onyx at Vanda"~"Onyx River at Lake Vanda Weir",
    STREAM.NAME=="Onyx at Lower"~"Onyx River at Lower Wright Weir",
  )) %>%
  filter(!is.na(stream))

biomass_avg<-biomass %>%
  group_by(STREAM.NAME, MAT.TYPE) %>%
  summarise(mean_afdm=mean(AFDM..mg.cm2., na.rm=T), mean_chl=mean(CHL.A..ug.cm2., na.rm=T))

biomass_avg <- biomass_avg %>%
  mutate(stream = case_when(
    STREAM.NAME=="Canada Gauge"~"Canada Stream at F1",
    STREAM.NAME=="Green Creek"~"Green Creek at F9",
    STREAM.NAME=="Delta Gauge"~"Delta Stream at F10",
    STREAM.NAME=="Crescent"~"Crescent Stream at F8",
    STREAM.NAME=="Harnish"~"Harnish Creek at F7",
    STREAM.NAME=="Von Guerard Gauge"~"Von Guerard Stream at F6",
    STREAM.NAME=="Andersen"~"Andersen Creek at H1",
    STREAM.NAME=="Priscu"~"Priscu Stream at B1",
    STREAM.NAME=="Lawson"~"Lawson Creek at B3",
    STREAM.NAME=="Commonwealth"~"Commonwealth Stream at C1",
    STREAM.NAME=="Onyx at Vanda"~"Onyx River at Lake Vanda Weir",
    STREAM.NAME=="Onyx at Lower"~"Onyx River at Lower Wright Weir",
  )) %>%
  filter(!is.na(stream))

p1<-mcm_avg %>%
  filter(!duplicated(diff)) %>%
  left_join(stats) %>%
  left_join(biomass) %>%
  mutate(Biomass_Class = factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  ggplot(aes(x=Stream, y=AFDM..mg.cm2.))+geom_boxplot(aes(fill=MAT.TYPE), position = "dodge", outliers = F)+theme_classic()+
  theme(text = element_text(size = 20), axis.text.x = element_blank(), legend.position = "null")+
  labs(x="", y="Mean Annual AFDM", fill="Mat Type")+
  facet_wrap(~Biomass_Class, scales="free_x", space="free_x")+
  scale_fill_manual(values = c("black", "orange"))

p2<-mcm_avg %>%
  filter(!duplicated(diff)) %>%
  left_join(stats) %>%
  left_join(biomass) %>%
  mutate(Biomass_Class = factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  ggplot(aes(x=Stream, y=CHL.A..ug.cm2.))+geom_boxplot(aes(fill=MAT.TYPE), position = "dodge", outliers = F)+theme_classic()+
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1), , legend.position = "null")+
  labs(x="", y="Mean Annual Chl-A", fill="Mat Type")+
  facet_wrap(~Biomass_Class, scales="free_x", space="free_x")+
  scale_fill_manual(values = c("black", "orange"))

pdf("Biomass_Class_MCM.pdf", width = 10, height = 8)

ggarrange(p1, p2, nrow = 2, heights = c(0.35, 0.5))

dev.off()

mcm_avg %>%
  filter(!duplicated(diff)) %>%
  left_join(stats) %>%
  left_join(biomass_avg) %>%
  mutate(Biomass_Class = factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  ggplot(aes(x=diff, y=mean_chl))+geom_point(aes(col=stream, shape=MAT.TYPE), size=4)

mcm_avg %>%
  filter(!duplicated(diff)) %>%
  left_join(stats) %>%
  left_join(biomass_avg) %>%
  mutate(Biomass_Class = factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  ggplot(aes(x=diff, y=mean_afdm))+geom_point(aes(col=stream, shape=MAT.TYPE), size=4)

#### Canada Stream ####
biomass_canada<-biomass %>%
  filter(STREAM.NAME=="Canada Gauge") %>%
  mutate(stream="Canada Stream at F1", variable=SEASON) %>%
  group_by(STREAM.NAME, MAT.TYPE, variable) %>%
  summarise(mean_afdm=mean(AFDM..mg.cm2., na.rm=T), mean_chl=mean(CHL.A..ug.cm2., na.rm=T))
#pull flow period 8 to look at drawdown
mcm %>%
  filter(stream=="Canada Stream at F1") %>%
  group_by(variable) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  ggplot(aes(variable, prop))+geom_point()

p1<-mcm %>%
  filter(stream=="Canada Stream at F1") %>%
  group_by(variable) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  left_join(biomass_canada) %>%
  ggplot(aes(mean_afdm, diff, col=MAT.TYPE))+geom_point()+geom_smooth(method = "lm", se=F)+
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  scale_color_manual(values = c("black", "orange"))+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Mean AFDM (mg/cm2)", y="Si Drawdown (mg Si/L)")+ggtitle("Canada Stream")


p2<-mcm %>%
  filter(stream=="Canada Stream at F1") %>%
  group_by(variable) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  left_join(biomass_canada) %>%
  ggplot(aes(mean_chl, diff, col=MAT.TYPE))+geom_point()+geom_smooth(method = "lm", se=F)+
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  scale_color_manual(values = c("black", "orange"))+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Mean Chl-A (ug/cm2)", y="Si Drawdown (mg Si/L)")
  

p3<-mcm %>%
  filter(stream == "Canada Stream at F1") %>%
  group_by(variable) %>%
  arrange(Period, .by_group = TRUE) %>%
  summarise(
    mean_Q=mean(Q),
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  ggplot(aes(mean_Q, diff))+geom_point()+geom_smooth(method = "lm", se=F, col="black")+
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Mean Discharge (cms)", y="Si Drawdown (mg Si/L)")

p7<-mcm %>%
  filter(stream == "Canada Stream at F1") %>%
  group_by(variable) %>%
  arrange(Period, .by_group = TRUE) %>%
  summarise(
    mean_Q=mean(Q),
    lowest_q=Q[Period == 8],
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  ggplot(aes(lowest_q, diff))+geom_point()+geom_smooth(method = "lm", se=F, col="black")+
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Period 8 Discharge (cms)", y="Si Drawdown (mg Si/L)")

#### Green Creek ####
biomass_green<-biomass %>%
  filter(STREAM.NAME=="Green Creek") %>%
  mutate(stream="Green Creek at F9", variable=SEASON) %>%
  group_by(STREAM.NAME, MAT.TYPE, variable) %>%
  summarise(mean_afdm=mean(AFDM..mg.cm2., na.rm=T), mean_chl=mean(CHL.A..ug.cm2., na.rm=T))
#pull flow period 8 to look at drawdown
mcm %>%
  filter(stream=="Green Creek at F9") %>%
  group_by(variable) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  ggplot(aes(variable, diff))+geom_point()

p4<-mcm %>%
  filter(stream == "Green Creek at F9") %>%
  group_by(variable) %>%
  arrange(Period, .by_group = TRUE) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  left_join(biomass_green) %>%
  ggplot(aes(mean_afdm, diff, col = MAT.TYPE)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  scale_color_manual(values = c("black", "orange"))+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Mean AFDM (mg/cm2)", y="Si Drawdown (mg Si/L)")+ggtitle("Green Creek")


p5<-mcm %>%
  filter(stream == "Green Creek at F9") %>%
  group_by(variable) %>%
  arrange(Period, .by_group = TRUE) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  left_join(biomass_green) %>%
  ggplot(aes(mean_chl, diff, col=MAT.TYPE))+geom_point()+geom_smooth(method = "lm", se=F)+
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  scale_color_manual(values = c("black", "orange"))+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Mean Chl-A (ug/cm2)", y="Si Drawdown (mg Si/L)")

p6<-mcm %>%
  filter(stream == "Green Creek at F9") %>%
  group_by(variable) %>%
  arrange(Period, .by_group = TRUE) %>%
  summarise(
    mean_Q=mean(Q),
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  left_join(biomass_green) %>%
  ggplot(aes(mean_Q, diff))+geom_point()+geom_smooth(method = "lm", se=F, col="black")+
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Mean Discharge (cms)", y="Si Drawdown (mg Si/L)")

p8<-mcm %>%
  filter(stream == "Green Creek at F9") %>%
  group_by(variable) %>%
  arrange(Period, .by_group = TRUE) %>%
  summarise(
    mean_Q=mean(Q),
    lowest_q=Q[Period == 8],
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  left_join(biomass_green) %>%
  ggplot(aes(lowest_q, diff))+geom_point()+geom_smooth(method = "lm", se=F, col="black")+
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Period 8 Discharge (cms)", y="Si Drawdown (mg Si/L)")
p8

mcm %>%
  filter(stream == "Green Creek at F9") %>%
  group_by(variable) %>%
  arrange(Period, .by_group = TRUE) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  left_join(biomass_green) %>%
  ggplot(aes(mean_chl, mean_afdm, col=MAT.TYPE))+geom_point()+geom_smooth(method = "lm", se=F)+
  theme_classic()+theme(text = element_text(size = 20))+
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) +
  scale_color_manual(values = c("black", "orange"))


pdf("Drawdown_Controls_Canada_Green.pdf", width = 10, height = 16)

ggarrange(p4, p1, p5, p2, p6, p3, p8, p7, nrow = 4, ncol=2, heights = c(0.4, 0.32, 0.32, 0.32))

dev.off()


p1<-mcm %>%
  filter(stream=="Canada Stream at F1") %>%
  group_by(variable) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  ggplot(aes(variable, diff))+geom_bar(stat="identity", fill="black")+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Year", y="Si Drawdown (mg Si/L)")+ggtitle("Canada Stream")


p2<-mcm %>%
  filter(stream=="Green Creek at F9") %>%
  group_by(variable) %>%
  summarise(
    initial_si = first(conc_clean, na_rm = TRUE),
    lowest_si  = conc_clean[Period == 8],
    diff = lowest_si - initial_si,
    mean_si = mean(conc_clean, na.rm = TRUE),
    prop = (diff / mean_si) * 100
  ) %>%
  ggplot(aes(variable, diff))+geom_bar(stat="identity", fill="black")+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Year", y="Si Drawdown (mg Si/L)")+ggtitle("Green Creek")

ggarrange(p1, p2, nrow = 2, align = "v")

