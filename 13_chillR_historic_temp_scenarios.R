require(chillR)
require(ggplot2)
require(reshape2)
require(kableExtra)

## Let's first make a temperature scenario with 100 synthetic years that represents
## the weather at Klein-Altendorf from 1998 to 2005.

Temp<-temperature_generation(KA_weather,years=c(1998,2005), sim_years = c(2001,2100))

## # Now we make a temperature scenario that raises all temperatures by 2°C

change_scenario<-data.frame(Tmin=rep(2,12),
                            Tmax=rep(2,12))

change_scenario

Temp_2<-temperature_generation(KA_weather,years=c(1998,2005),
                                sim_years = c(2001,2100),
                                temperature_scenario = change_scenario)
 
# As before, we'll make a data.frame that contains all
# our data, so we can take a look at it.
 
Temperature_scenarios<-cbind(
   KA_weather[which(KA_weather$Year %in% 1998:2005),],
   Data_source="observed")
 
Temperature_scenarios<-rbind(
   Temperature_scenarios,
   cbind(Temp[[1]][,c("Year","Month","Day","Tmin","Tmax")],
         Data_source="simulated"))
 
Temperature_scenarios<-rbind(
   Temperature_scenarios,
   cbind(Temp_2[[1]][,c("Year","Month","Day","Tmin","Tmax")],
         Data_source="Warming_2C"))

Temperature_scenarios[,"Date"]<-
  as.Date(ISOdate(2000,
                  Temperature_scenarios$Month,
                  Temperature_scenarios$Day))

ggplot(data=Temperature_scenarios, aes(Date,Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

ggplot(data=Temperature_scenarios, aes(Date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")


# download weather station list for the vicinity of Bonn
station_list<-handle_dwd(action="list_stations",
                         location=c(7.1,50.8),
                         time_interval = c(19730101,20191231))

station_list

# download weather data for Cologne/Bonn airport
Bonn_weather_raw<-handle_dwd(action="download_weather",
                              location=station_list$Station_ID[1],
                              time_interval = c(19730101,20191231),
                              station_list = station_list)
 
# convert weather data to chillR format
Bonn_weather<-handle_dwd(Bonn_weather_raw)
 
# check record for missing data
fix_weather(Bonn_weather[[1]])$QC
 
# (if we had gaps, we'd now have to patch them. The KA_weather dataset could be a
# possible source of auxiliary data)
## Bonn_patched<-patch_daily_temperatures(
##   weather=Bonn_weather[[1]]$weather,
##  patch_weather=list(KA_weather,Auxiliary_weather$weather))
 
Bonn_temps<-Bonn_weather[[1]]

scenario_1980<-temperature_scenario_from_records(weather=Bonn_temps,year=1980)

scenario_1980$'1980'$data

scenario_1980

temps_1980<-temperature_generation(weather=Bonn_temps, years=c(1973,2019),
                                   sim_years=c(2001,2100),
                                   temperature_scenario = scenario_1980)

scenario_1996<-temperature_scenario_from_records(weather=Bonn_temps,year=1996)

 
relative_scenario<-temperature_scenario_baseline_adjustment(
  baseline=scenario_1996,
  temperature_scenario = scenario_1980)



temps_1980<-temperature_generation(weather=Bonn_temps, years=c(1973,2019),
                                   sim_years=c(2001,2100),
                                   temperature_scenario = relative_scenario)
 

all_past_scenarios<-temperature_scenario_from_records(
  weather=Bonn_temps,
  year=c(1980,1990,2000,2010))
 
adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_1996,
  temperature_scenario = all_past_scenarios)
 
all_past_scenario_temps<-temperature_generation(
  weather=Bonn_temps,
  years=c(1973,2019),
  sim_years=c(2001,2100),
  temperature_scenario = adjusted_scenarios)
 
all_past_scenario_temps

?tempResponse_daily_list

chill_hist_scenario_list<-tempResponse_daily_list(all_past_scenario_temps,
                                                  latitude=50.9,
                                                  Start_JDay = 305,
                                                  End_JDay = 59)

chill_hist_scenario_list

# so this was a list. For easy plotting, we have to convert this to a data.frame.

scenarios<-names(chill_hist_scenario_list)[1:4]

all_scenarios<-chill_hist_scenario_list[[scenarios[1]]]
all_scenarios[,"scenario"]<-as.numeric(scenarios[1])


for (sc in scenarios[2:4])
 all_scenarios<-rbind(all_scenarios,
                      cbind(chill_hist_scenario_list[[sc]],scenario=as.numeric(sc)))

all_scenarios<-all_scenarios[which(all_scenarios$Perc_complete==100),]

# Let's compute the actual 'observed' chill for comparison
actual_chill<-tempResponse_daily_list(Bonn_temps,latitude=50.9,
                        Start_JDay = 305,
                        End_JDay = 59)[[1]]
actual_chill<-actual_chill[which(actual_chill$Perc_complete==100),]

actual_chill

ggplot(data=all_scenarios,aes(scenario,Chill_Portions,
                              fill=factor(scenario))) +
  geom_violin() +
  ylab("Chill accumulation (Chill Portions)") +
  xlab("Scenario year") +
  theme_bw(base_size=15) +
  ylim(c(0,90)) +
  geom_point(data=actual_chill,
             aes(End_year,Chill_Portions,fill="blue"),
             col="blue",show.legend = FALSE) +
  scale_fill_discrete(name="Scenario",
                      breaks = unique(all_scenarios$scenario)) 


?runn_mean

temperature_means<-data.frame(Year=min(Bonn_temps$Year):max(Bonn_temps$Year),
                                Tmin=aggregate(Bonn_temps$Tmin,FUN="mean",
                                               by=list(Bonn_temps$Year))[,2],
                              Tmax=aggregate(Bonn_temps$Tmax,FUN="mean",
                                             by=list(Bonn_temps$Year))[,2])
temperature_means[,"runn_mean_Tmin"]<-runn_mean(temperature_means$Tmin,15)
temperature_means[,"runn_mean_Tmax"]<-runn_mean(temperature_means$Tmax,15)

Tmin_regression<-lm(Tmin~Year, temperature_means)
temperature_means[,"regression_Tmin"]<-Tmin_regression$coefficients[1]+
  Tmin_regression$coefficients[2]*temperature_means$Year

Tmax_regression<-lm(Tmax~Year, temperature_means)
temperature_means[,"regression_Tmax"]<-Tmax_regression$coefficients[1]+
  Tmax_regression$coefficients[2]*temperature_means$Year


ggplot(temperature_means,aes(Year, Tmin)) + 
  geom_point() + 
  geom_line(data=temperature_means,aes(Year, runn_mean_Tmin),lwd=2,col="blue") + 
  geom_line(data=temperature_means,aes(Year, regression_Tmin),lwd=2,col="red") +
  theme_bw(base_size=15) +
  ylab("Mean monthly minimum temperature (°C)")

ggplot(temperature_means,aes(Year, Tmax)) + 
  geom_point() + 
  geom_line(data=temperature_means,aes(Year, runn_mean_Tmax),lwd=2,col="blue") + 
  geom_line(data=temperature_means,aes(Year, regression_Tmax),lwd=2,col="red") +
  theme_bw(base_size=15) +
  ylab("Mean monthly maximum temperature (°C)")

