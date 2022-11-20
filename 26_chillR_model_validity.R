require(ggplot2)
dat<-data.frame(x=c(1,2,3,4),y=c(2.3,2.5,2.7,2.7))

ggplot(dat,aes(x=x,y=y)) +
  geom_smooth(method="lm",fullrange = TRUE) +
  geom_smooth(method="lm",fullrange = FALSE,col="dark green") +
  geom_point() +
  xlim(c(0,10)) +
  geom_vline(xintercept = 8, col="red") +
  theme_bw(base_size = 15)


require(chillR)
past_weather<-read_tab("data/TMaxTMin1958-2019_patched.csv")
past_weather$RCP_Time<-"Past"

temps_50_45<-load_temperature_scenarios("data/Weather","Bonn_2050_rcp45")
temps_50_85<-load_temperature_scenarios("data/Weather","Bonn_2050_rcp85")
temps_85_45<-load_temperature_scenarios("data/Weather","Bonn_2085_rcp45")
temps_85_85<-load_temperature_scenarios("data/Weather","Bonn_2085_rcp85")

for (tt in names(temps_50_45))
{temp<-temps_50_45[[tt]]
 temp$GCM<-tt
 temp$Time<-2050
 temp$RCP<-"RCP4.5"
 if(tt==names(temps_50_45)[1])
   results<-temp else
     results<-rbind(results,temp)
  }

for (tt in names(temps_50_85))
{temp<-temps_50_85[[tt]]
 temp$GCM<-tt
 temp$Time<-2050
 temp$RCP<-"RCP8.5"
 results<-rbind(results,temp)
}
for (tt in names(temps_85_45))
{temp<-temps_85_45[[tt]]
 temp$GCM<-tt
 temp$Time<-2085
 temp$RCP<-"RCP4.5"
 results<-rbind(results,temp)
}
for (tt in names(temps_85_85))
{temp<-temps_85_85[[tt]]
 temp$GCM<-tt
 temp$Time<-2085
 temp$RCP<-"RCP8.5"
 results<-rbind(results,temp)
}
results$RCP_Time<-paste0(results$RCP,"_",results$Time)

future_months<-
  aggregate(results[,c("Tmin","Tmax")],
            by=list(results$RCP_Time,
                    results$Year,
                    results$Month),
            FUN=mean)
colnames(future_months)[1:3]<-c("RCP_Time","Year","Month")

past_months<-
  aggregate(past_weather[,c("Tmin","Tmax")],
            by=list(past_weather$RCP_Time,
                    past_weather$Year,
                    past_weather$Month),
            FUN=mean)
colnames(past_months)[1:3]<-c("RCP_Time","Year","Month")

all_months<-rbind(past_months,future_months)

all_months$month_name<-factor(all_months$Month,
                                       levels=c(6:12,1:5),
                                       labels=month.name[c(6:12,1:5)])

library(tidyverse)

# Calculate the hulls for each group
hull_temps <- all_months %>%
  group_by(RCP_Time,month_name) %>%
  slice(chull(Tmin, Tmax))

ggplot(hull_temps, aes(Tmin, Tmax, fill = factor(RCP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past",
                             "RCP4.5_2050",
                             "RCP4.5_2085",
                             "RCP8.5_2050",
                             "RCP8.5_2085"),
                    values=c("black",
                             alpha("light green",0.3),
                             alpha("dark green",0.3),
                             alpha("coral",0.3),
                             alpha("dark red",0.3))) +
  theme_bw(base_size = 15)



ggplot(hull_temps[which(hull_temps$Month %in% c(10,11,12,1,2,3)),],
       aes(Tmin, Tmax, fill = factor(RCP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past",
                             "RCP4.5_2050",
                             "RCP4.5_2085",
                             "RCP8.5_2050",
                             "RCP8.5_2085"),
                    values=c("black",
                             alpha("light green",0.3),
                             alpha("dark green",0.3),
                             alpha("coral",0.3),
                             alpha("dark red",0.3))) +
  theme_bw(base_size = 15)




enhanced<-read_tab("data/final_weather_data_S1_S2.csv")
enhanced$Year<-enhanced$Treatment
enhanced$RCP_Time<-"Past_enhanced"


enhanced_months<-aggregate(enhanced[,c("Tmin","Tmax")],
                           by=list(enhanced$RCP_Time,
                                   enhanced$Year,
                                   enhanced$Month),FUN=mean)
colnames(enhanced_months)[1:3]<-c("RCP_Time","Year","Month")

all_months_enhanced<-rbind(enhanced_months,future_months)

all_months_enhanced$month_name<-factor(all_months_enhanced$Month,
                                       levels=c(6:12,1:5),
                                       labels=month.name[c(6:12,1:5)])

library(tidyverse)

# Calculate the hulls for each group
hull_temps_enhanced <- all_months_enhanced %>%
  group_by(RCP_Time,month_name) %>%
  slice(chull(Tmin, Tmax))

ggplot(hull_temps_enhanced[which(hull_temps_enhanced$Month %in% c(10,11,12,1,2,3)),],
       aes(Tmin, Tmax, fill = factor(RCP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past_enhanced",
                             "RCP4.5_2050",
                             "RCP4.5_2085",
                             "RCP8.5_2050",
                             "RCP8.5_2085"),
                    values=c("black",
                             alpha("light green",0.3),
                             alpha("dark green",0.3),
                             alpha("coral",0.3),
                             alpha("dark red",0.3))) +
  theme_bw(base_size = 15)



past_months$RCP_Time<-"Past_combined"
enhanced_months$RCP_Time<-"Past_combined"

all_months_both<-rbind(enhanced_months,past_months,future_months)

all_months_both$month_name<-factor(all_months_both$Month,
                                   levels=c(6:12,1:5),
                                   labels=month.name[c(6:12,1:5)])

hull_temps_both <- all_months_both %>%
  group_by(RCP_Time,month_name) %>%
  slice(chull(Tmin, Tmax))

ggplot(hull_temps_both[which(hull_temps_both$Month %in% c(10,11,12,1,2,3)),],
       aes(Tmin, Tmax, fill = factor(RCP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past_combined",
                             "RCP4.5_2050",
                             "RCP4.5_2085",
                             "RCP8.5_2050",
                             "RCP8.5_2085"),
                    values=c("black",
                             alpha("light green",0.3),
                             alpha("dark green",0.3),
                             alpha("coral",0.3),
                             alpha("dark red",0.3))) +
  theme_bw(base_size = 15)



