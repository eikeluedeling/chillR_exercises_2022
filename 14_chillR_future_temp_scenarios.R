require(chillR)
require(ggplot2)
require(reshape2)
require(kableExtra)
#Temperature_scenarios<-read_tab("data/Temperature_scenarios.csv")
Temperature_scenarios[,"Date"]<-as.Date(ISOdate(2000, Temperature_scenarios$Month, Temperature_scenarios$Day))
Bonn_temps<-read_tab("data/Bonn_temps.csv")
#chill_hist_scenario_list<-load_temperature_scenarios("data","chill_hist_scenario_list_")


getClimateWizardData(coordinates=c(longitude=10.61,latitude=34.93),
    scenario="rcp45", start_year=2020, end_year=2050,
    metric=c("CD18","R02"), GCMs=c("bcc-csm1-1","BNU-ESM"))
 

 
RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)
 
for(RCP in RCPs)
   for(Time in Times)
     {start_year <- Time-15
      end_year <- Time+15
      clim_scen <-getClimateWizardData(
      c(longitude = 7.143,latitude = 50.866),
      RCP,
      start_year,
      end_year,
      temperature_generation_scenarios = TRUE,
      baseline =c(1975, 2005),
      metric = "monthly_min_max_temps",
      GCMs = "all")
    save_temperature_scenarios(clim_scen,
                               "data/ClimateWizard",
                               paste0("Bonn_futures_",Time,"_",RCP))}


scenario_1990<-temperature_scenario_from_records(Bonn_temps[,3:8],1990)
scenario_1996<-temperature_scenario_from_records(Bonn_temps,1996)
adjustment_scenario<-temperature_scenario_baseline_adjustment(scenario_1996,scenario_1990)

adjustment_scenario


RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)


for(RCP in RCPs)
  for(Time in Times)
    {
     clim_scen<-load_ClimateWizard_scenarios(
       "data/climateWizard",
       paste0("Bonn_futures_",Time,"_",RCP))
     clim_scen_adjusted<-
       temperature_scenario_baseline_adjustment(
         baseline_temperature_scenario=adjustment_scenario,
         temperature_scenario=clim_scen)
     Temps<-temperature_generation(
       weather=Bonn_temps,
       years=c(1973,2019),
       sim_years=c(2001,2101),
       temperature_scenario = clim_scen_adjusted)

     save_temperature_scenarios(
       Temps,
       "data/Weather",
       paste0("Bonn_",Time,"_",RCP))
  }



all_past_scenarios<-temperature_scenario_from_records(
  weather=Bonn_temps,
  year=c(1980,1990,2000,2010))

adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_1996,
  temperature_scenario = all_past_scenarios)

all_past_scenario_temps<-temperature_generation(
  weather=Bonn_temps,
  years=c(1973,2019),
  sim_years=c(2001,2101),
  temperature_scenario = adjusted_scenarios)

save_temperature_scenarios(
  all_past_scenario_temps,
  "data/Weather",
  "Bonn_historic")



frost_model<-function(x) step_model(x,data.frame(
  lower=c(-1000,0),
  upper=c(0,1000),
  weight=c(1,0)))

models<-list(Chill_CP=Dynamic_Model,Heat_GDH=GDH,Frost_H=frost_model)



Temps<-load_temperature_scenarios("data/Weather","Bonn_historic")
chill_past_scenarios<-tempResponse_daily_list(
  Temps,
  latitude=50.866,
  Start_JDay = 305,
  End_JDay = 59,
  models=models,
  misstolerance = 10)
chill_observed<-tempResponse_daily_list(
  Bonn_temps,
  latitude=50.866,
  Start_JDay = 305,
  End_JDay = 59,
  models=models,
  misstolerance = 10)

save_temperature_scenarios(chill_past_scenarios,
                           "data/chill",
                           "Bonn_historic")
save_temperature_scenarios(chill_observed,
                           "data/chill",
                           "Bonn_observed")


chill_past_scenarios<-load_temperature_scenarios(
  "data/chill",
  "Bonn_historic")
chill_observed<-load_temperature_scenarios(
  "data/chill",
  "Bonn_observed")

chills <-make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE)

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)")


for(RCP in RCPs)
  for(Time in Times)
    {
    Temps<-load_temperature_scenarios(
      "data/Weather",
      paste0("Bonn_",Time,"_",RCP))
    chill<-tempResponse_daily_list(
      Temps,
      latitude=50.866,
      Start_JDay = 305,
      End_JDay = 59,
      models=models,
      misstolerance = 10)
    save_temperature_scenarios(
      chill,
      "data/chill",
      paste0("Bonn_",Time,"_",RCP))
}




for(RCP in RCPs)
  for(Time in Times)
    {
    chill<-load_temperature_scenarios(
      "data/chill",
      paste0("Bonn_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    chills <-make_climate_scenario(
      chill,
      caption =c(RCPcaption, Time_caption),
      add_to = chills)
}



plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)",
  texcex=1.5)

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Heat_GDH",
  metric_label="Heat (Growing Degree Hours)",
  texcex=1.5)

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Frost_H",
  metric_label="Frost hours",
  texcex=1.5)


