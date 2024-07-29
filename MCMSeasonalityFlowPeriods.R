library(zoo)

setwd("C:/Users/johnkeir/Box/Keira_Johnson/SiSyn/MCMSeasonality")

#this is to the daily WRTDS output
link<-"https://drive.google.com/file/d/11-nv9g8w1oUjGg_vkhl6ZcSQUSo_Di_7/view?usp=drive_link"

file = drive_get(as_id(link))

drive_download(file$drive_resource,  overwrite=T)

daily_results<-read.csv("Full_Results_WRTDS_kalman_daily.csv")

#subset to Si and columns of interest
daily_results<-subset(daily_results, daily_results$chemical=="DSi")
daily_results<-daily_results[,c("LTER", "Stream_Name", "Date", "Month", "waterYear", "ConcDay", "Q")]

#only keep MCM streams - should be 12
daily_results_MCM<-subset(daily_results, daily_results$LTER=="MCM")

daily_results_MCM<-subset(daily_results_MCM, daily_results_MCM$Month %in% c(12,1))

daily_results_avg<-daily_results_MCM %>%
  group_by(Stream_Name) %>%
  summarise(mean_si=mean(ConcDay))

#make list of streams in MCM LTER
stream_list<-unique(daily_results_MCM$Stream_Name)

#all streams have 61 days of data - to get 12 equal periods we want to pull every 5 days from the 61 days
seq_list<-seq(5, 61, 5)

#open lists to append new dfs to
streams_df_Si<-list()
streams_df_Q<-list()
WY_length<-list()

#the outer loop loops through each stream in MCM
for (i in 1:length(stream_list)) {
  
  #pull out one stream
  one_stream<-subset(daily_results_MCM, daily_results_MCM$Stream_Name==stream_list[i])
  
  #remove all NA
  one_stream<-one_stream[complete.cases(one_stream$ConcDay),]
  
  #pull out water years
  WY_list<-unique(one_stream$waterYear)
  
  #append to list - this will be used for column names later
  WY_length[[i]]<-WY_list
  
  #create dataframe of "months" to append new data to
  months_df_Si<-as.data.frame(seq(1,12,1))
  
  months_df_Q<-as.data.frame(seq(1,12,1))
  
  #loop through each water year to get rolling average for each "month"
  for (k in 1:length(WY_list)) {
    
    #pull out one year of data
    one_year_stream <-subset(one_stream, one_stream$waterYear==WY_list[k])
    
    #rolling average
    mov_avg_Si<- rollmean(one_year_stream$ConcDay, k=5)
    
    mov_avg_Q<- rollmean(one_year_stream$Q, k=5)
    
    #append NA to front, values dont exist until "k", in this case 5
    mov_avg_Si<-c(NA, NA, NA, NA, mov_avg_Si)
    
    mov_avg_Q<-c(NA, NA, NA, NA, mov_avg_Q)
    
    #pull out "month" days from seq list (every 5 days during 61 day period)
    months_avg_Si<-mov_avg_Si[seq_list]
    
    months_avg_Q<-mov_avg_Q[seq_list]
    
    #bind to dataframe
    months_df_Si<-cbind(months_df_Si, months_avg_Si)
    
    months_df_Q<-cbind(months_df_Q, months_avg_Q)
    
  }
  
  #make list of dfs
  streams_df_Si[[i]]<-months_df_Si
  
  streams_df_Q[[i]]<-months_df_Q
  
}

#open list
MCM_stream_list_Si<-list()

#this loop takes the list of dfs and turns into a melted df similar format to the original monthly df
for (i in 1:length(stream_list)) {
  
  #get stream df
  onestream_df<-streams_df_Si[[i]]
  
  colnames(onestream_df)[1]<-"Period"
  
  onestream_df_melt<-melt(onestream_df, id.vars="Period")
  
  #rename columns
  names(onestream_df)<-c("Period", WY_length[[i]])
  
  #melt
  stream_melt<-melt(onestream_df, id.vars ="Period", value.name="ConcDay")
  
  #add column for stream name
  stream_melt$stream<-stream_list[i]
  
  #add to new list to combine
  MCM_stream_list_Si[[i]]<-stream_melt
  
}

#combine into long, melted df
MCM_allstreams_Si<-bind_rows(MCM_stream_list_Si)

#open list
MCM_stream_list_Q<-list()

#this loop takes the list of dfs and turns into a melted df similar format to the original monthly df
for (i in 1:length(stream_list)) {
  
  #get stream df
  onestream_df<-streams_df_Q[[i]]
  
  colnames(onestream_df)[1]<-"Period"
  
  onestream_df_melt<-melt(onestream_df, id.vars="Period")
  
  #rename columns
  names(onestream_df)<-c("Period", WY_length[[i]])
  
  #melt
  stream_melt<-melt(onestream_df, id.vars ="Period", value.name="Q")
  
  #add column for stream name
  stream_melt$stream<-stream_list[i]
  
  #add to new list to combine
  MCM_stream_list_Q[[i]]<-stream_melt
  
}


#combine into long, melted df
MCM_allstreams_Q<-bind_rows(MCM_stream_list_Q)

MCM_allstreams<-merge(MCM_allstreams_Si, MCM_allstreams_Q, by=c("Period","variable","stream"))


ID=seq(1,12,1)

#check
ggplot(MCM_allstreams, aes(Period, ConcDay))+
  geom_line(aes(col=variable))+facet_wrap(~stream, scales = "free")+theme_classic()+
  scale_x_continuous(breaks = ID)+theme(legend.position = "null")+
  labs(y="Si Concentration")

write.csv(MCM_allstreams, "MCM_Interpolation.csv")

MCM_allstreams<-MCM_allstreams %>%
  dplyr::group_by(stream) %>%
  dplyr::mutate(min_Q=min(Q, na.rm = T))

mcm_years<-mcm %>%
  dplyr::group_by(stream) %>%
  dplyr::summarise(min_year=min(variable), max_year=max(variable))

MCM_allstreams$min_Q_10<-MCM_allstreams$min_Q*1.1

MCM_allstreams<-MCM_allstreams %>%
  mutate(conc_clean=case_when(Q > min_Q_10 ~ ConcDay,
                              .default=NA))

MCM_allstreams$Q_min_Q<-ifelse(MCM_allstreams$Q > MCM_allstreams$min_Q_10, "no", "yes")

write.csv(MCM_allstreams, "MCM_FlowPeriods_Cleaned.csv")

#mcm_remove<-c("Andersen Creek at H1","Onyx River at Lake Vanda Weir","Onyx River at Lower Wright Weir", "Lawson Creek at B3")

#mcm<-mcm[-c(which(mcm$stream %in% mcm_remove)),]

mcm_avg<-MCM_allstreams %>% 
  dplyr::group_by(stream, Period) %>%
  dplyr::summarise(mean_conc=mean(conc_clean, na.rm = TRUE))

mcm_avg<-mcm_avg %>%
  mutate(location = case_when(stream %in% c("Crescent Stream at F8","Delta Stream at F10","Harnish Creek at F7",
                                            "Von Guerard Stream at F6") ~ "Fryxell Basin",
                              stream %like% "Onyx" ~ "Wright Valley",
                              .default = "Taylor Valley"))


pdf("MCM_Avg_Si_07292024.pdf", width = 18, height = 10, family = "Times")

ggplot(mcm_avg, aes(Period, mean_conc))+geom_line(aes(group=stream, col=stream), size=3)+
  theme_bw()+theme(text = element_text(size = 25))+scale_x_continuous(breaks = seq(1,12,1))+
  labs(x="Flow Period (~5 days)", y="Si Concentration (mg/L)", col="Stream")+facet_wrap(~location, scales = "free_y")

dev.off()

pdf("MCM_Annual_Si_07292024.pdf", width = 18, height = 10)

ggplot(MCM_allstreams, aes(Period, conc_clean))+geom_line(aes(group=as.numeric(variable), col=as.numeric(variable)))+
  theme_bw()+theme(text = element_text(size = 25))+scale_x_continuous(breaks = seq(1,12,1))+
  labs(x="Flow Period (~5 days)", y="Si Concentration (mg/L)", col="Year")+
  facet_wrap(~stream, scales = "free_y")+
  scale_color_gradientn(colors = c("firebrick", "goldenrod", "seagreen", "dodgerblue", "purple3"))

dev.off()





