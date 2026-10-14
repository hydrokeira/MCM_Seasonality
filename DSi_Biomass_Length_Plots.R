require(dplyr)
require(ggplot2)
require(data.table)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/MCMSeasonality")
mcm_flow<-read.csv("MCM_FlowPeriods_Cleaned.csv")

mcm_flow<-mcm_flow %>%
  filter(!stream %like% "Harnish")

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

complete_years<-mcm_flow %>%
  mutate(unique2=paste(stream, variable)) %>%
  filter(unique2 %in% complete_years$unique2) %>%
  group_by(stream, variable) %>%
  mutate(norm_conc = scale(conc_clean, center = T))

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

avg_si<-

pdf("DSi_Conc_Boxplots.pdf", width = 10, height = 6)

complete_years_stats %>%
  mutate(Stream=factor(Stream, levels = simple_stream),
         Biomass_Class=factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  ggplot(aes(Stream, conc_clean))+geom_boxplot(aes(fill=Length.to.gauge..km.))+
  facet_wrap(~Biomass_Class, scales="free_x", space="free_x")+
  theme_classic()+theme(text = element_text(size = 20), 
                        axis.text.x = element_text(angle = 45, hjust = 1),
                        legend.position = "null")+
  labs(x="", y="DSi Concentration (mg/L)")+
  scale_fill_binned(breaks = c(1, 2, 5, 10), palette = "oranges")

dev.off()

pdf("Biomass_Length_NoLegend.pdf", width = 5, height = 5)

complete_years_stats %>%
  mutate(stream=factor(stream, levels = stream_order),
         Biomass_Class=factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  group_by(stream, Length.to.gauge..km., Biomass_Class) %>%
  summarise(mean_si=mean(conc_clean, na.rm=T)) %>%
  ggplot(aes(Length.to.gauge..km., mean_si))+
  geom_point(aes(col=Biomass_Class), size=4)+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = c(
    "low" = "#c7e9c0",
    "moderate" = "#74c476",
    "high" = "#238b45"
  ))+labs(x="Length to Gage (km)", y="Mean DSi (mg/L)", col="Biomass Class")

dev.off()

avg<-complete_years_stats %>%
  mutate(stream=factor(stream, levels = stream_order),
         Biomass_Class=factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  group_by(stream, Length.to.gauge..km., Biomass_Class) %>%
  summarise(mean_si=mean(conc_clean, na.rm=T))

write.csv(avg, "Mean_Si_Conc.csv")

pdf("Avg_DSi_Regime.pdf", width = 15, height = 6)

complete_years_stats %>%
  mutate(stream=factor(stream, levels = stream_order),
         Biomass_Class=factor(Biomass_Class, levels = c("low", "moderate", "high"))) %>%
  group_by(stream, Period, Biomass_Class, Length.to.gauge..km.) %>%
  summarise(mean_conc=mean(conc_clean, na.rm=T)) %>%
  ggplot(aes(as.factor(Period), mean_conc))+
  geom_line(aes(col=Length.to.gauge..km., group=stream), size=1)+
  facet_wrap(~Biomass_Class)+theme_classic()+
  scale_color_binned(breaks = c(1, 2, 5, 10), palette = "oranges")+
  theme(text = element_text(size = 20), legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="", y="Mean DSi Concentration (mg/L)", col="Length to Gage (km)")+
  scale_x_discrete(labels = dates_no_year)

dev.off()

