setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")

chem<-read.csv("20260105_masterdata_chem.csv")
chem<-chem %>%
  filter(!Stream_Name %like% "Harnish")

sites<-c("Andersen Creek at H1", "Priscu Stream at B1","Canada Stream at F1", 
         "Green Creek at F9", "Lawson Creek at B3", "Delta Stream at F10",
         "Crescent Stream at F8", "Von Guerard Stream at F6", "Commonwealth Stream at C1",
         "Onyx River at Lake Vanda Weir","Onyx River at Lower Wright Weir")

MCM_DSi<-chem %>%
  filter(LTER=="MCM") %>%
  mutate(date=as.Date(date), day=get_waterYearDay(date)) %>%
  filter(day > 61) %>%
  filter(variable %in% c("DSi")) %>%
  filter(!Stream_Name %in% "Harnish Creek at F7") %>%
  group_by(date, Stream_Name) %>%
  summarise(mean_DSi=mean(value))

MCM_Ca<-chem %>%
  filter(LTER=="MCM") %>%
  mutate(date=as.Date(date), day=get_waterYearDay(date)) %>%
  filter(day > 61) %>%
  filter(variable %in% c("Ca", "Mg")) %>%
  filter(!Stream_Name %in% "Harnish Creek at F7") %>%
  group_by(date, Stream_Name) %>%
  summarise(mean_Ca=mean(sum(value)))

pH<-read.csv("MCM_pH.csv")

pH <- pH %>%
  mutate(date=as.Date(Sampling.Date, format="%m/%d/%y")) %>%
  filter(!is.na(pH)) %>%
  mutate(Stream_Name=Site.Stream.Name) %>%
  group_by(date, Stream_Name) %>%
  summarise(mean_pH=mean(pH))

pH %>%
  dplyr::filter(!is.na(Stream_Name)) %>%
  dplyr::filter(!is.na(mean_pH)) %>%
  dplyr::filter(!Stream_Name=="Bohner Stream") %>%
  filter(Stream_Name=="Andersen Creek at H1") %>%
  filter(get_waterYear(date) > 2009 & get_waterYear(date) < 2017) %>%
  ggplot(aes(date, mean_pH))+
  geom_point()+
  geom_line()+
  facet_wrap(~Stream_Name, scales="free")+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="pH", y="DSi (mg/L)")

pH %>%
  dplyr::filter(!is.na(Stream_Name)) %>%
  dplyr::filter(!is.na(mean_pH)) %>%
  left_join(MCM_DSi) %>%
  dplyr::filter(!Stream_Name=="Bohner Stream") %>%
  #filter(Stream_Name=="Andersen Creek at H1") %>%
  #filter(get_waterYear(date) > 2009 & get_waterYear(date) < 2017) %>%
  ggplot(aes(y=mean_DSi*0.028, x=mean_pH))+
  geom_point()+
  #geom_line()+
  facet_wrap(~Stream_Name, scales="free")+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(y="DSi (mg/L)", x="pH")
  #facet_wrap(~get_waterYear(date))

MCM %>%
  left_join(pH) %>%
  dplyr::filter(!is.na(Stream_Name)) %>%
  dplyr::filter(!is.na(mean_pH)) %>%
  dplyr::filter(!Stream_Name=="Bohner Stream") %>%
  #filter(Stream_Name=="Priscu Stream at B1") %>%
  ggplot(aes(mean_pH, mean_DSi))+
  geom_point()+
  facet_wrap(~Stream_Name, scales="free")+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="pH", y="DSi (mg/L)")
  facet_wrap(~get_waterYear(date), scales="free")


MCM %>%
  left_join(pH) %>%
  dplyr::filter(!is.na(Stream_Name)) %>%
  dplyr::filter(!is.na(mean_pH)) %>%
  dplyr::filter(!Stream_Name=="Bohner Stream") %>%
  #filter(Stream_Name=="Andersen Creek at H1") %>%
  filter(Stream_Name=="Green Creek at F9") %>%
  #filter(get_waterYear(date) > 2009 & get_waterYear(date) < 2013) %>%
  ggplot(aes(y=log(mean_DSi), x=mean_pH))+
  geom_point()+
  #geom_point(aes(col=as.factor(get_waterYear(date))))+
  #geom_line()+
  facet_wrap(~Stream_Name, scales="free")+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="pH", y="DSi (mg/L)")
  #facet_wrap(~get_waterYear(date))

MCM_alk %>%
  left_join(MCM_Ca) %>%
  dplyr::filter(!is.na(Stream_Name)) %>%
  dplyr::filter(!Stream_Name=="Bohner Stream") %>%
  ggplot(aes(y=mean_alk*0.05, x=mean_Ca))+
  geom_point()+
  #geom_point(aes(col=as.factor(get_waterYear(date))))+
  #geom_line()+
  facet_wrap(~Stream_Name, scales="free")+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="pH", y="DSi (mg/L)")


MCM_DSi_NOx<-chem %>%
  filter(LTER=="MCM") %>%
  mutate(date=as.Date(date), day=get_waterYearDay(date)) %>%
  filter(day > 61) %>%
  filter(variable %in% c("DSi", "NOx")) %>%
  filter(!Stream_Name %in% c("Harnish Creek at F7", "Bohner Stream")) %>%
  group_by(date, Stream_Name, variable) %>%
  summarise(mean_value=mean(value)) %>%
  pivot_wider(values_from = mean_value, names_from = variable) %>%
  mutate(Si_N=DSi/NOx) %>%
  filter(!is.na(Si_N))


ggplot(MCM_DSi_NOx, aes(DSi, Si_N))+
  geom_point()+  
  facet_wrap(~Stream_Name, scales="free")+
  geom_smooth(se=F)+
  theme_classic()+theme(text = element_text(size = 15))

ggplot(MCM_DSi_NOx, aes(DSi, NOx))+
  geom_point()+  
  facet_wrap(~Stream_Name, scales="free")+
  #geom_smooth(se=F)+
  theme_classic()+theme(text = element_text(size = 15))+
  labs(x="pH", y="DSi (mg/L)")

ggplot(MCM_DSi_NOx, aes(Stream_Name, NOx))+geom_boxplot()


