###mcm drawdown
require(zoo)
require(ggpubr)
require(dataRetrieval)
require(EflowStats)
require(lubridate)
require(data.table)
require(dplyr)
require(tidyr)

### si conversion uM -> mg/L = x 0.028
### k conversion uM -> mg/L = x 0.039

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")

chem<-read.csv("20260105_masterdata_chem.csv")
chem<-chem %>%
  filter(!Stream_Name %like% "Harnish")

sites<-c("Andersen Creek at H1", "Priscu Stream at B1","Canada Stream at F1", 
         "Green Creek at F9", "Lawson Creek at B3", "Delta Stream at F10",
         "Crescent Stream at F8", "Von Guerard Stream at F6", "Commonwealth Stream at C1",
         "Onyx River at Lake Vanda Weir","Onyx River at Lower Wright Weir")

solutes<-c("Ca", "Cl", "DSi", "K", "Mg", "Na", "SO4")

# Define the start and end dates using as.Date()
start_date <- as.Date("2025-12-05")
end_date   <- as.Date("2026-01-31")

# Generate the sequence jumping by 3 days
date_sequence <- seq(from = start_date, to = end_date, by = 5)

# 3. Format the sequence to hide the year
dates_no_year <- format(date_sequence, format = "%b %d")

MCM<-chem %>%
  filter(LTER=="MCM") %>%
  mutate(date=as.Date(date), day=get_waterYearDay(date)) %>%
  filter(day > 61) %>%
  filter(variable %in% solutes) %>%
  filter(!Stream_Name %in% "Harnish Creek at F7") 

ggplot(MCM, aes(day, value))+geom_point(aes(col=year(date)))+
  facet_wrap(~variable, scales="free_y")+
  scale_color_gradient(low="lightgrey", high = "black")+
  geom_smooth()+
  theme_classic()+theme(text=element_text(size=20))+
  labs(x="Day of WY", y="Concentration (uM)")

MCM<-chem %>%
  filter(LTER=="MCM") %>%
  mutate(date=as.Date(date), day=get_waterYearDay(date)) %>%
  filter(day > 61) %>%
  filter(variable %in% solutes) %>%
  filter(Stream_Name %in% sites) %>%
  pivot_wider(names_from = variable, values_from = value, values_fn = mean) %>%
  pivot_longer(cols = c(8, 10:14), names_to = "solute", values_to = "value") %>%
  mutate(si_ratio=DSi/value)

biomass_manual <- data.frame(unique(MCM$Stream_Name))
biomass_manual$biomass_class <- c("low","high","high","low","low","low","low","moderate","moderate",
                                  "moderate","moderate")
colnames(biomass_manual)[1]<-"Stream_Name"

modal_clust<-read.csv("Modal_Cluster.csv")
colnames(modal_clust)[2]<-"Stream_Name"

pdf("Si_K_ratio_cluster.pdf", width = 14, height = 4)

p1<-MCM %>%
  filter(solute %in% c("K")) %>%
  group_by(Stream_Name, solute) %>%
  mutate(sd_3 = sd(value, na.rm=T)*3, mean_val=mean(value, na.rm=T), threshold=mean_val+sd_3) %>%
  mutate(remove = ifelse(value < threshold, "no", "yes")) %>%
  filter(remove=="no") %>%
  left_join(biomass_manual) %>%
  mutate(biomass_class=factor(biomass_class, levels = c("low", "moderate", "high"))) %>%
  left_join(modal_clust) %>%
  ggplot(aes(day, si_ratio, col=as.character(mode_clust)))+
  geom_smooth(aes(group=Stream_Name), se=F)+
  facet_wrap(~biomass_class)+
  theme_classic()+
  theme(text=element_text(size=20), legend.position = "right", 
        axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x="", y="Si:K Ratio", col="Modal\nCluster", lty="")+
  scale_color_manual(values = c("#F6CF71","#B497E7"))+
  scale_x_continuous(breaks = seq(65,122,5), labels = dates_no_year)

p1

dev.off()

pdf("Si_K_concentrations_together.pdf", width = 16, height = 10)

MCM %>%
  filter(solute %in% c("K")) %>%
  left_join(biomass_manual) %>%
  left_join(modal_clust) %>%
  mutate(Stream_Name=factor(Stream_Name, levels = sites)) %>%
  ggplot(aes(day))+
  geom_smooth(mapping=aes(y=DSi*0.028), col="black", se=F)+
  geom_smooth(mapping=aes(y=value*0.039), col="grey", se=F)+
  facet_wrap(~Stream_Name, scales="free_y")+
  theme_classic()+
  theme(text=element_text(size=20), legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x="", y="Concentration (mg/L)", col="Modal\nCluster", lty="")+
  scale_x_continuous(breaks = seq(65,122,5), labels = dates_no_year)

dev.off()

pdf("Si_K_ratio_stream.pdf", width = 14, height = 4)

p1<-MCM %>%
  filter(solute %in% c("K")) %>%
  group_by(Stream_Name, solute) %>%
  mutate(sd_3 = sd(value, na.rm=T)*3, mean_val=mean(value, na.rm=T), threshold=mean_val+sd_3) %>%
  mutate(remove = ifelse(value < threshold, "no", "yes")) %>%
  filter(remove=="no") %>%
  left_join(biomass_manual) %>%
  mutate(biomass_class=factor(biomass_class, levels = c("low", "moderate", "high"))) %>%
  left_join(modal_clust) %>%
  ggplot(aes(day, si_ratio, col=as.character(Stream_Name)))+
  geom_smooth(aes(group=Stream_Name), se=F)+
  facet_wrap(~biomass_class)+
  theme_classic()+
  theme(text=element_text(size=20), legend.position = "right", 
        axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x="", y="Si:K Ratio", col="Site", lty="")+
  scale_x_continuous(breaks = seq(65,122,5), labels = dates_no_year)

p1

dev.off()
