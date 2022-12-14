library(chillR)
library(dplyr)
library(lubridate)

mon<-1 # Month
ndays<-31 # Number of days per month
tmin<-1
tmax<-8
latitude<-50


hourly_temps<-make_all_day_table(
  data.frame(Year=c(2001,2001),
             Month=c(mon,mon),
             Day=c(1,ndays),
             Tmin=c(0,0),
             Tmax=c(0,0))) %>%
  mutate(Tmin=tmin,
         Tmax=tmax) %>% 
  stack_hourly_temps(latitude=latitude)

# weather$Tmin<-tmin
# weather$Tmax<-tmax
# hourly_temps<-stack_hourly_temps(weather,latitude=latitude)

CPs<-Dynamic_Model(hourly_temps$hourtemps$Temp)
daily_CPs<-tail(CPs,1)/(length(CPs)/24)

daily_CPs


library(lubridate) # to get convenient date functionality
library(purrr) # to get the pluck function

latitude<-50.6

month_range<-c(10,11,12,1,2,3)

Tmins=c(-20:20)
Tmaxs=c(-15:30)

temp_model<-Dynamic_Model

first_run<-TRUE
for(mon in month_range)
    {weather<-make_all_day_table(
      data.frame(Year=c(2001,2001),
                 Month=c(mon,mon),
                 Day=c(1,days_in_month(mon)),
                 Tmin=c(0,0),
                 Tmax=c(0,0)))
    for(tmin in Tmins)
      for(tmax in Tmaxs)
        if(tmax>=tmin)
          {
          hourtemps <- weather %>%
            mutate(Tmin=tmin,
                   Tmax=tmax) %>% 
            stack_hourly_temps(latitude=latitude) %>% 
            pluck("hourtemps","Temp")
          day_CP<-do.call(temp_model,
                          list(hourtemps)) %>% 
            tail(1)/days_in_month(mon)
          
          if(first_run)
            {CP<-day_CP
            mins<-tmin
            maxs<-tmax
            month<-mon
            first_run<-FALSE} else
              {CP<-append(CP,day_CP)
              mins<-append(mins,tmin)
              maxs<-append(maxs,tmax)
              month<-append(month,mon)}
        }
    }

results<-data.frame(Month=month,Tmin=mins,Tmax=maxs,CP)

write.csv(results,"data/model_sensitivity_development.csv",row.names = FALSE)

results<-read.csv("data/model_sensitivity_development.csv")

head(results)



library(ggplot2)
library(colorRamps)

results$Month_names<- factor(results$Month, levels=month_range,
                             labels=month.name[month_range])  

DM_sensitivity<-ggplot(results,
                       aes(x=Tmin,y=Tmax,fill=CP)) +
  geom_tile() +
  scale_fill_gradientn(colours=alpha(matlab.like(15),
                                     alpha = .5),
                       name="Chill/day (CP)") +
  ylim(min(results$Tmax),
       max(results$Tmax)) +
  ylim(min(results$Tmin),
       max(results$Tmin))

DM_sensitivity

DM_sensitivity <- DM_sensitivity +
  facet_wrap(vars(Month_names)) +
  ylim(min(results$Tmax),max(results$Tmax)) +
  ylim(min(results$Tmin),max(results$Tmin))

DM_sensitivity

temperatures<-read_tab("data/TMaxTMin1958-2019_patched.csv") %>% 
  filter(Month %in% month_range)

temperatures[which(temperatures$Tmax<temperatures$Tmin),
             c("Tmax","Tmin")]<-NA

temperatures$Month_names = factor(temperatures$Month,
                                  levels=c(10,11,12,1,2,3),
                                  labels=c("October","November",
                                           "December","January",
                                           "February","March")) 

DM_sensitivity +
  geom_point(data=temperatures,aes(x=Tmin,y=Tmax,fill=NULL,
                                   color="Temperature"),size=0.2) +
  facet_wrap(vars(Month_names)) +
  scale_color_manual(values = "black",
                     labels = "Daily temperature \nextremes (°C)",
                     name="Observed at site" ) +
  guides(fill = guide_colorbar(order = 1),
         color = guide_legend(order = 2)) +
  ylab("Tmax (°C)") +
  xlab("Tmin (°C)") + 
  theme_bw(base_size=12) 

# continue here next time

Chill_model_sensitivity<-
  function(latitude,
           temp_models=list(Dynamic_Model=Dynamic_Model,GDH=GDH),
           month_range=c(10,11,12,1,2,3),
           Tmins=c(-10:20),
           Tmaxs=c(-5:30))
  {
  metrics<-as.list(rep(NA,length(temp_models)))
  names(metrics)<-names(temp_models)

  first_run<-TRUE
  for(mon in month_range)
  {weather<-make_all_day_table(
    data.frame(Year=c(2001,2001),
               Month=c(mon,mon),
               Day=c(1,days_in_month(mon)),
               Tmin=c(0,0),
               Tmax=c(0,0)))
  for(tmin in Tmins)
    for(tmax in Tmaxs)
      if(tmax>=tmin)
      {
        hourtemps <- weather %>%
          mutate(Tmin=tmin, Tmax=tmax) %>% 
          stack_hourly_temps(latitude=latitude) %>% 
          pluck(1,"Temp")         
          
           metric <- lapply(temp_models,
                           function(x) do.call(x,list(hourtemps)) %>% 
                             tail(1)/(length(hourtemps)/24))
                 
          if(first_run)
            {for(tm in 1:length(temp_models))
              metrics[[tm]]<-metric[[tm]]
            mins<-tmin
            maxs<-tmax
            month<-mon
            first_run<-FALSE} else
              {for(tm in 1:length(temp_models))
                metrics[[tm]]<-append(metrics[[tm]],metric[[tm]])
              mins<-append(mins,tmin)
              maxs<-append(maxs,tmax)
              month<-append(month,mon)}
      }
  }
  results<-cbind(data.frame(Month=month,Tmin=mins,Tmax=maxs),
                 as.data.frame(metrics))
  results
}


Chill_sensitivity_temps<-function(chill_model_sensitivity_table,
                                  temperatures,
                                  temp_model,
                                  month_range=c(10,11,12,1,2,3),
                                  Tmins=c(-10:20),
                                  Tmaxs=c(-5:30),
                                  legend_label="Chill/day (CP)")
{
  library(ggplot2)
  library(colorRamps)

  cmst<-filter(chill_model_sensitivity_table,Month %in% month_range) %>% 
    mutate(Month_names = factor(Month,
                                levels=month_range,
                                labels=month.name[month_range]))
  
 
  DM_sensitivity<-
    ggplot(cmst,
           aes(x=Tmin,y=Tmax,fill=.data[[temp_model]])) +
    geom_tile() +
    scale_fill_gradientn(colours=alpha(matlab.like(15), alpha = .5),
                         name=legend_label) +
    coord_cartesian(xlim=range(Tmins),
                    ylim=range(Tmaxs))
  
  temperatures<-temperatures %>% 
    filter(Month %in% month_range) %>% 
    filter(Tmax>=Tmin) %>% 
    mutate(Month_names=factor(Month,
                              levels=month_range,
                              labels=month.name[month_range])  )
  

  DM_sensitivity +
    geom_point(data=temperatures,
               aes(x=Tmin,y=Tmax,fill=NULL,color="Temperature"),
               size=0.2) +
    facet_wrap(vars(Month_names)) +
    scale_color_manual(values = "black",
                       labels = "Daily temperature \nextremes (°C)",
                       name="Observed at site" ) +
    guides(fill = guide_colorbar(order = 1),
           color = guide_legend(order = 2)) +
    ylab("Tmax (°C)") +
    xlab("Tmin (°C)") + 
    theme_bw(base_size=12)

}
  


Model_sensitivities_CKA<-
  Chill_model_sensitivity(latitude=50,
                          temp_models=list(Dynamic_Model=Dynamic_Model,
                                           GDH=GDH),
                          month_range=c(10:12,1:5))
write.csv(Model_sensitivities_CKA,
          "data/Model_sensitivities_CKA.csv",row.names = FALSE)

Model_sensitivities_Davis<-
  Chill_model_sensitivity(latitude=38.5,
                          temp_models=list(Dynamic_Model=Dynamic_Model,
                                           GDH=GDH),
                          month_range=c(10:12,1:5))
write.csv(Model_sensitivities_Davis,
          "data/Model_sensitivities_Davis.csv",row.names = FALSE)

Model_sensitivities_Beijing<-
  Chill_model_sensitivity(latitude=39.9,
                          temp_models=list(Dynamic_Model=Dynamic_Model,
                                           GDH=GDH),
                          month_range=c(10:12,1:5))
write.csv(Model_sensitivities_Beijing,
          "data/Model_sensitivities_Beijing.csv",row.names = FALSE)

Model_sensitivities_Sfax<-
  Chill_model_sensitivity(latitude=35,
                          temp_models=list(Dynamic_Model=Dynamic_Model,
                                           GDH=GDH),
                          month_range=c(10:12,1:5))
write.csv(Model_sensitivities_Sfax,
          "data/Model_sensitivities_Sfax.csv",row.names = FALSE)


Model_sensitivities_CKA<-read.csv("data/Model_sensitivities_CKA.csv")
Model_sensitivities_Davis<-read.csv("data/Model_sensitivities_Davis.csv")
Model_sensitivities_Beijing<-read.csv("data/Model_sensitivities_Beijing.csv")
Model_sensitivities_Sfax<-read.csv("data/Model_sensitivities_Sfax.csv")


Beijing_weather<-read_tab("data/Beijing_weather.csv")
CKA_temperatures<-read_tab("data/TMaxTMin1958-2019_patched.csv")
Davis_weather<-read_tab("data/Davis_weather.csv")
Sfax_weather<-read_tab("data/Sfax_weather.csv")


Chill_sensitivity_temps(Model_sensitivities_Beijing,
                        Beijing_weather,
                        temp_model="Dynamic_Model",
                        month_range=c(10,11,12,1,2,3),
                        legend_label="Chill per day \n(Chill Portions)") +
  ggtitle("Chill model sensitivity at Beijing, China")

Chill_sensitivity_temps(Model_sensitivities_CKA,
                        CKA_temperatures,
                        temp_model="Dynamic_Model",
                        month_range=c(10,11,12,1,2,3),
                        legend_label="Chill per day \n(Chill Portions)") +
  ggtitle("Chill model sensitivity at Klein-Altendorf, Germany")

Chill_sensitivity_temps(Model_sensitivities_Davis,
                        Davis_weather,
                        temp_model="Dynamic_Model",
                        month_range=c(10,11,12,1,2,3),
                        legend_label="Chill per day \n(Chill Portions)") +
  ggtitle("Chill model sensitivity at Davis, California")

Chill_sensitivity_temps(Model_sensitivities_Sfax,
                        Sfax_weather,
                        temp_model="Dynamic_Model",
                        month_range=c(10,11,12,1,2,3),
                        legend_label="Chill per day \n(Chill Portions)") +
  ggtitle("Chill model sensitivity near Sfax, Tunisia")



Chill_sensitivity_temps(Model_sensitivities_Davis,
                        Beijing_weather,
                        temp_model="GDH",
                        month_range=c(12,1:5),
                        legend_label="Heat per day \n(GDH)") +
  ggtitle("Heat model sensitivity at Beijing, China")

Chill_sensitivity_temps(Model_sensitivities_CKA,
                        CKA_temperatures,
                        temp_model="GDH",
                        month_range=c(12,1:5),
                        legend_label="Heat per day \n(GDH)") +
  ggtitle("Heat model sensitivity at Klein-Altendorf, Germany")

Chill_sensitivity_temps(Model_sensitivities_Davis,
                        Davis_weather,
                        temp_model="GDH",
                        month_range=c(12,1:5),
                        legend_label="Heat per day \n(GDH)") +
  ggtitle("Heat model sensitivity at Davis, California")

Chill_sensitivity_temps(Model_sensitivities_Sfax,
                        Sfax_weather,
                        temp_model="GDH",
                        month_range=c(12,1:5),
                        legend_label="Heat per day \n(GDH)") +
  ggtitle("Heat model sensitivity near Sfax, Tunisia")

