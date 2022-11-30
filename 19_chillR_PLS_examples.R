library(chillR)
library(dormancyR)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(patchwork)


hourly_models <- list(Chilling_units = chilling_units,
     Low_chill = low_chill_model,
     Modified_Utah = modified_utah_model,
     North_Carolina = north_carolina_model,
     Positive_Utah = positive_utah_model,
     Chilling_Hours = Chilling_Hours,
     Utah_Chill_Units = Utah_Model,
     Chill_Portions = Dynamic_Model)
daily_models<-list(Rate_of_Chill = rate_of_chill, 
    Exponential_Chill = exponential_chill,
    Triangula_Chill_Haninnen = triangular_chill_1,
    Triangular_Chill_Legave = triangular_chill_2)

metrics<-c(names(daily_models),names(hourly_models))

model_labels=c("Rate of Chill",
               "Exponential Chill",
               "Triangular Chill (Häninnen)",
               "Triangular Chill (Legave)",
               "Chilling Units",
               "Low-Chill Chill Units",
               "Modified Utah Chill Units",
               "North Carolina Chill Units",
               "Positive Utah Chill Units",
               "Chilling Hours",
               "Utah Chill Units",
               "Chill Portions")



for(T in -20:30)
 {hourly<-sapply(hourly_models, function(x) x(rep(T,1000)))[1000,]
  temp_frame<-data.frame(Tmin=rep(T,1000),
                         Tmax=rep(T,1000),
                         Tmean=rep(T,1000))
  daily<-sapply(daily_models, function(x) x(temp_frame))[1000,]
 
  if(T==-20) sensitivity<-c(T=T,daily,hourly) else
    sensitivity<-rbind(sensitivity,c(T=T,daily,hourly))
}

sensitivity_normal<-
  as.data.frame(cbind(sensitivity[,1],
                      sapply(2:ncol(sensitivity),
                             function(x)
                               sensitivity[,x]/max(sensitivity[,x]))))
colnames(sensitivity_normal)<-colnames(sensitivity)
sensitivity_gg<-melt(sensitivity_normal,id.vars="T")
sensitivity_gg$value[which(sensitivity_gg$value<=0.001)]<-NA


chill<-
  ggplot(sensitivity_gg,aes(x=T,y=factor(variable),size=value)) +
  geom_point(col="light blue") +
  scale_y_discrete(labels= model_labels) +
  ylab("Chill model") +
  xlab("Temperature (assumed constant, °C)") +
  xlim(c(-30,40)) +
  theme_bw(base_size=15) +
  labs(size = "Chill \nWeight")

chill

KA_temps_JD<-make_JDay(read_tab("data/TMaxTMin1958-2019_patched.csv"))
temps<-stack_hourly_temps(
  KA_temps_JD[which(KA_temps_JD$JDay>305|KA_temps_JD$JDay<90),],
  latitude=50.6)
hh<-hist(temps$hourtemps$Temp,breaks=c(-30:30), plot=FALSE)
hh_df<-data.frame(
  T=hh$mids,
  variable="Klein-Altendorf, Germany",
  value=hh$counts/max(hh$counts))
hh_df$value[which(hh_df$value==0)]<-NA

Beijing_temps_JD<-make_JDay(read_tab("data/Beijing_weather.csv"))
temps<-stack_hourly_temps(
  Beijing_temps_JD[which(Beijing_temps_JD$JDay>305|Beijing_temps_JD$JDay<90),]
  ,latitude=39.9)
hh<-hist(temps$hourtemps$Temp,breaks=c(-30:30), plot=FALSE)
hh_df_2<-data.frame(T=hh$mids,
                    variable="Beijing, China",
                    value=hh$counts/max(hh$counts))
hh_df_2$value[which(hh_df_2$value==0)]<-NA

Davis_temps_JD<-make_JDay(read_tab("data/Davis_weather.csv"))
temps<-stack_hourly_temps(
  Davis_temps_JD[which(Davis_temps_JD$JDay>305|Davis_temps_JD$JDay<90),],
  latitude=38.5)
hh<-hist(temps$hourtemps$Temp,breaks=c(-30:40), plot=FALSE)
hh_df_3<-data.frame(T=hh$mids,
                    variable="Davis, California",
                    value=hh$counts/max(hh$counts))
hh_df_3$value[which(hh_df_3$value==0)]<-NA

hh_df<-rbind(hh_df,hh_df_2,hh_df_3)

locations<-
  ggplot(data=hh_df,aes(x=T,y=variable,size=value)) +
  geom_point(col="coral2") +
  ylab("Location") +
  xlab("Temperature (between November and March, °C)") + 
  xlim(c(-30,40)) +
  theme_bw(base_size=15) +
  labs(size = "Relative \nfrequency")


locations



  plot<- (chill +
            locations +
            plot_layout(guides = "collect",
                        heights = c(1,0.4))
        ) & theme(legend.position = "right",
                  legend.text = element_text(size=10),
                  legend.title = element_text(size=12))

plot


chill<-
  ggplot(sensitivity_gg[which(sensitivity_gg$variable=="Chill_Portions"),],
         aes(x=T,y=factor(variable),
             size=value)) +
  geom_point(col="light blue") +
  scale_y_discrete(labels= "Chill Portions") +
  ylab("Chill model") +
  xlab("Temperature (assumed constant, °C)") +
  xlim(c(-30,40)) +
  theme_bw(base_size=15) +
  labs(size = "Chill \nWeight")

  plot<- (chill +
            locations +
            plot_layout(guides = "collect",
                        heights = c(0.5,1))
        ) & theme(legend.position = "right",
                  legend.text = element_text(size=10),
                  legend.title = element_text(size=12))

plot

