require(dplyr)
require(ggplot2)
require(lubridate)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")

chem<-read.csv("20260105_masterdata_chem.csv")

chem<-chem %>%
  filter(LTER == "MCM", variable =="DSi")

sites<-c("Andersen Creek at H1", "Priscu Stream at B1","Canada Stream at F1", 
         "Green Creek at F9", "Lawson Creek at B3", "Delta Stream at F10",
         "Crescent Stream at F8", "Von Guerard Stream at F6", "Commonwealth Stream at C1",
         "Onyx River at Lake Vanda Weir","Onyx River at Lower Wright Weir")

pdf("MCM_Monthly_Observation_Count.pdf", width = 12, height = 8)

chem %>%
  filter(Stream_Name %in% sites) %>%
  mutate(month=as.character(month(as.Date(date)))) %>%
  mutate(month=factor(month, levels = c("11", "12", "1", "2"))) %>%
  ggplot(aes(month))+geom_bar(stat = "count", fill="black")+
  facet_wrap(~Stream_Name)+
  theme_classic()+
  theme(text = element_text(size = 15))+
  labs(x="Month", y="Number of Observations")

dev.off()

chem %>%
  filter(Stream_Name %in% sites) %>%
  mutate(month=as.character(month(as.Date(date)))) %>%
  mutate(month=factor(month, levels = c("11", "12", "1", "2"))) %>%
  group_by(month) %>%
  tally()

(1181+1270)/(1181+1270+93+8)

