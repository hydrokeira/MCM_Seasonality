###MCM Biomass

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/MCMSeasonality")

biomass<-read.csv("STREAMS_MICROBIAL_BIOMASS_3.csv")

unique(biomass$MAT.TYPE)

biomass<-subset(biomass, biomass$MAT.TYPE %in% c("black", "orange", "black ", "orange ", "Black",
                                                 "Orange ", "orange/filamentous", "Orange", "Black "))
biomass<-biomass %>%
  mutate(MAT.TYPE = case_when(
    MAT.TYPE == "black " ~ "black",
    MAT.TYPE == "Black" ~ "black",
    MAT.TYPE == "orange " ~ "orange",
    MAT.TYPE == "Orange" ~ "orange",
    MAT.TYPE == "orange/filamentous" ~ "orange",
    .default = MAT.TYPE
  ))

unique(biomass$MAT.TYPE)

#standardize names
biomass<-biomass %>%
  mutate(STREAM.NAME = case_when(
    STREAM.NAME == "Canada Stream - Delta" ~ "Canada Delta",
    STREAM.NAME == "Canada Stream - Gage" ~ "Canada Gauge",
    STREAM.NAME == "Canada Stream Gage" ~ "Canada Gauge",
    STREAM.NAME == "Canada Stream " ~ "Canada Gauge",
    STREAM.NAME == "Canada gage" ~ "Canada Gauge",
    STREAM.NAME == "Commonwealth Stream" ~ "Commonwealth",
    STREAM.NAME == "Crescent Stream" ~ "Crescent",
    STREAM.NAME == "Delta Stream - Gage" ~ "Delta Gauge",
    STREAM.NAME == "Delta Stream - Upper" ~ "Delta Upper",
    STREAM.NAME == "Delta Stream Gauge" ~ "Delta Gauge",
    STREAM.NAME == "Delta Stream Upper" ~ "Delta Upper",
    STREAM.NAME == "Delta Upper " ~ "Delta Upper",
    STREAM.NAME == "Delta Gauge " ~ "Delta Gauge",
    STREAM.NAME == "Upper Delta " ~ "Delta Upper",
    STREAM.NAME == "Upper Delta" ~ "Delta Upper",
    STREAM.NAME == "Delta gage" ~ "Delta Gauge",
    STREAM.NAME == "Green" ~ "Green Creek",
    STREAM.NAME == "Green " ~ "Green Creek",
    STREAM.NAME == "Green Creek " ~ "Green Creek",
    STREAM.NAME == "Green Creek Gauge" ~ "Green Creek",
    STREAM.NAME == "Delta Stream Gauge" ~ "Delta Gauge",
    STREAM.NAME == "VG Upper" ~ "Von Guerard Upper",
    STREAM.NAME == "VG gage" ~ "Von Guerard Gauge",
    STREAM.NAME == "VG middle" ~ "Von Guerard Lower",
    STREAM.NAME == "Von Guerard Stream - Gage" ~ "Von Guerard Gauge",
    STREAM.NAME == "Von Guerard Stream - Lower" ~ "Von Guerard Lower",
    STREAM.NAME == "Von Guerard Lower " ~ "Von Guerard Lower",
    STREAM.NAME == "Von Guerard Gauge " ~ "Von Guerard Gauge",
    STREAM.NAME == "Von Guerard Stream - Upper" ~ "Von Guerard Upper",
    STREAM.NAME == "Von Guerard @ Gauge " ~ "Von Guerard Gauge",
    STREAM.NAME == "Onyx River - Lower Wright" ~ "Onyx at Lower",
    STREAM.NAME == "Onyx River - Vanda" ~ "Onyx at Vanda",
    STREAM.NAME == "Lawson Creek" ~ "Lawson",
    STREAM.NAME == "Lawson Stream" ~ "Lawson",
    STREAM.NAME == "Von Guerard Stream Gage" ~ "Von Guerard Gauge",
    STREAM.NAME == "Von Guerard Stream Lower" ~ "Von Guerard Lower",
    STREAM.NAME == "Von Guerard Middle" ~ "Von Guerard Lower",
    STREAM.NAME == "Von Guerard Stream Upper" ~ "Von Guerard Upper",
    STREAM.NAME == "Von Guerard Upper " ~ "Von Guerard Upper",
    STREAM.NAME == "Commonwealth " ~ "Commonwealth",
    STREAM.NAME == "Lawson " ~ "Lawson",
    STREAM.NAME == "Lawson Upstream" ~ "Lawson Upper",
    STREAM.NAME == "Lawson Creek - Upper" ~ "Lawson Upper",
    STREAM.NAME == "Lawson Creek - Gage" ~ "Lawson",
    STREAM.NAME == "Harnish Creek at lower site" ~ "Harnish",
    .default = STREAM.NAME
  ))

unique(biomass$STREAM.NAME)

#remove sites that are not relavent to analysis
biomass<-biomass %>%
  filter(!(STREAM.NAME %in% c("Lawson Upper", "Harnish Creek at upper site", "Canada Delta", "Green creek above gage",
                              "Canada stream near glacier", "Green Lake")))


biomass_table<-biomass %>%
  group_by(STREAM.NAME, SEASON, MAT.TYPE) %>%
  tally()

write.csv(biomass_table, "Biomass_Summary_Table.csv")

p1<-ggplot(biomass, aes(as.character(SEASON), AFDM..mg.cm2., col=MAT.TYPE))+geom_boxplot()+
  facet_wrap(~STREAM.NAME)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(values = c("black", "orange"))+
  labs(x="Season", y="AFDM (mg/cm2)", col="Mat Type")
  
long_records<-c("Canada Gauge","Delta Gauge", "Delta Upper", "Green Creek", "Von Guerard Gauge","Von Guerard Lower")

p2<-ggplot(biomass[biomass$STREAM.NAME %in% long_records, ], aes(as.character(SEASON), AFDM..mg.cm2., col=MAT.TYPE))+geom_boxplot()+
  facet_wrap(~STREAM.NAME)+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(values = c("black", "orange"))+
  labs(x="Season", y="AFDM (mg/cm2)", col="Mat Type")

pdf("MCM_Biomass_Boxplots.pdf", width = 12, height = 8)

print(p1)
print(p2)

dev.off()
