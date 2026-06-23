###mcm drawdown
require(zoo)
require(ggpubr)
require(dataRetrieval)
require(EflowStats)
require(lubridate)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")

chem<-read.csv("20260105_masterdata_chem.csv")

mcm<-read.csv("MCM_FlowPeriods_Cleaned.csv")

unique(mcm$stream)

sites<-c("Andersen Creek at H1", "Priscu Stream at B1","Canada Stream at F1", 
         "Green Creek at F9", "Lawson Creek at B3", "Delta Stream at F10", "Harnish Creek at F7",
         "Crescent Stream at F8", "Von Guerard Stream at F6", "Commonwealth Stream at C1",
         "Onyx River at Lake Vanda Weir","Onyx River at Lower Wright Weir")

MCM<-chem %>%
  filter(LTER=="MCM") %>%
  filter(variable %in% c("DSi", "K")) %>%
  mutate(date=as.Date(date), day=get_waterYearDay(date)) %>%
  filter(day > 61) %>%
  pivot_wider(names_from = variable, values_from = value, values_fn = mean) %>%
  mutate(Si_K=DSi/K) %>%
  filter(Stream_Name %in% sites) %>%
  mutate(Si_K_avg=rollmean(Si_K, k=5, fill = NA))

x3<-ggplot(MCM, aes(date, Si_K, col=year(date)))+geom_point()+
  facet_wrap(~Stream_Name)+
  theme_classic()+geom_smooth(se=F, col="black")+
  labs(x="Time", y="Si:K", col="Water Year")+
  geom_vline(xintercept = as.Date("2001-06-01"))+
  theme(text = element_text(size = 15))+
  scale_color_viridis_c(option = "A", direction = -1)

x3

x1<-ggplot(MCM, aes(date, DSi, col=year(date)))+geom_point()+facet_wrap(~Stream_Name)+
  theme_classic()+geom_smooth(se=F, col="black")+
  labs(x="Time", y="Si", col="Water Year")+
  geom_vline(xintercept = as.Date("2001-06-01"))+
  theme(text = element_text(size = 15))+
  scale_color_viridis_c(option = "A", direction = -1)
x1

x2<-ggplot(MCM, aes(date, K, col=year(date)))+geom_point()+facet_wrap(~Stream_Name)+
  theme_classic()+geom_smooth(se=F, col="black")+
  labs(x="Time", y="K", col="Water Year")+
  geom_vline(xintercept = as.Date("2001-06-01"))+
  theme(text = element_text(size = 15))+
  scale_color_viridis_c(option = "A", direction = -1)
x2

pdf("Si_K_LongTerm_FixedAxes.pdf", width = 15, height = 10)

x1
x2
x3

dev.off()

p3<-ggplot(MCM, aes(day, Si_K, col=year(date)))+geom_point()+facet_wrap(~Stream_Name)+
  geom_smooth(se=F, col="black")+theme_classic()+
  labs(x="Day of Water Year", y="Si:K", col="Water Year")+
  scale_color_viridis_c(option = "A", direction = -1)+
  theme(text = element_text(size = 15))

p3

p1<-ggplot(MCM, aes(day, DSi, col=year(date)))+geom_point()+facet_wrap(~Stream_Name)+
  geom_smooth(se=F, col="black")+theme_classic()+
  labs(x="Day of Water Year", y="Si", col="Water Year")+
  scale_color_viridis_c(option = "A", direction = -1)+
  theme(text = element_text(size = 15))
p1

p2<-ggplot(MCM, aes(day, K, col=year(date)))+geom_point()+facet_wrap(~Stream_Name)+
  geom_smooth(se=F, col="black")+theme_classic()+
  labs(x="Day of Water Year", y="K", col="Water Year")+
  scale_color_viridis_c(option = "A", direction = -1)+
  theme(text = element_text(size = 15))
p2

pdf("Si_K_Seasonal_FixedAxes.pdf", width = 15, height = 10)

p1
p2
p3

dev.off()

MCM %>%
  filter(Stream_Name=="Green Creek at F9") %>%
  ggplot(aes(day, Si_K))+geom_point()+facet_wrap(~year(date))+
  theme_classic()+geom_smooth(se=F)+
  labs(x="Day of Water Year", y="Si:K", col="Water Year")+
  theme(text = element_text(size = 20))+
  scale_color_viridis_c(option = "A", direction = -1)

MCM %>%
  filter(Stream_Name=="Canada Stream at F1") %>%
  ggplot(aes(day, Si_K))+geom_point()+facet_wrap(~year(date))+
  theme_classic()+geom_smooth(se=F)+
  labs(x="Day of Water Year", y="Si:K", col="Water Year")+
  theme(text = element_text(size = 20))+
  scale_color_viridis_c(option = "A", direction = -1)


###mcm drawdown
require(zoo)
require(ggpubr)
require(dataRetrieval)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")

chem<-read.csv("20260105_masterdata_chem.csv")

sites<-c("Andersen Creek at H1", "Priscu Stream at B1","Canada Stream at F1", 
         "Green Creek at F9", "Lawson Creek at B3", "Delta Stream at F10", "Harnish Creek at F7",
         "Crescent Stream at F8", "Von Guerard Stream at F6", "Commonwealth Stream at C1",
         "Onyx River at Lake Vanda Weir","Onyx River at Lower Wright Weir")

solutes<-c("Ca", "Cl", "DSi", "K", "Mg", "Na", "SO4")

MCM<-chem %>%
  filter(LTER=="MCM") %>%
  mutate(date=as.Date(date), day=get_waterYearDay(date)) %>%
  filter(day > 61) %>%
  filter(variable %in% solutes)
  #filter(Stream_Name %in% "Green Creek at F9") 

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
                                  "moderate","moderate","moderate")
colnames(biomass_manual)[1]<-"Stream_Name"

pdf("Si_ratios_K_Ca.pdf", width = 18, height = 9)

MCM %>%
  filter(solute %in% c("K", "Ca")) %>%
  left_join(biomass_manual) %>%
  mutate(biomass_class=factor(biomass_class, levels = c("high", "moderate", "low"))) %>%
  mutate(solute=factor(solute, levels = c("K","Ca"))) %>%
  ggplot(aes(day, si_ratio))+
  #geom_point(aes(col=Stream_Name))+
  geom_smooth(aes(col=Stream_Name, lty=Stream_Name), se=F)+
  facet_grid(rows=vars(solute), cols=vars(biomass_class), scales="free_y")+
  theme_bw()+
  theme(text=element_text(size=20), legend.key.width = unit(2, "cm"), legend.position = "left")+
  labs(x="Day of WY", y="Si:Solute Ratio", col="", lty="")+
  scale_color_manual(values = carto_pal(n=12, "Bold"))

dev.off()
