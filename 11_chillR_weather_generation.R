library(chillR)
library(ggplot2)

# Temperatures<-read.csv("data/generated_temperatures_example_1.csv")
# Temperatures[,"Date"]<-as.Date(ISOdate(2000, Temperatures$Month, Temperatures$Day))

# chill_comparison_full_seasons<-read.csv("data/chill_comparison_full_seasons.csv")


 Temp<-temperature_generation(KA_weather,
                              years=c(1998,2005),
                              sim_years = c(2001,2100))
 
 Temperatures<-cbind(KA_weather[
   which(KA_weather$Year %in% 1998:2005),] ,Data_source="observed")
 
 Temperatures<-rbind(Temperatures,
                     cbind(Temp[[1]][,c("Year","Month","Day","Tmin","Tmax")],
                           Data_source="simulated"))
 Temperatures[,"Date"]<-as.Date(ISOdate(2000,
                                        Temperatures$Month,
                                        Temperatures$Day))

ggplot(data=Temperatures, aes(Date,Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

ggplot(data=Temperatures, aes(Date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")


 chill_observed<-chilling(
   stack_hourly_temps(
     Temperatures[which(Temperatures$Data_source=="observed"),],
     latitude = 50.4),
   Start_JDay = 305,
   End_JDay = 59)
 
 chill_simulated<-chilling(
   stack_hourly_temps(
     Temperatures[which(Temperatures$Data_source=="simulated"),],
     latitude = 50.4),
   Start_JDay = 305,
   End_JDay = 59)
 
 chill_comparison<-cbind(chill_observed ,Data_source="observed")
 chill_comparison<-rbind(chill_comparison,
                         cbind(chill_simulated ,Data_source="simulated"))
 
 chill_comparison_full_seasons<-chill_comparison[
   which(chill_comparison$Perc_complete==100),]

ggplot(chill_comparison_full_seasons, aes(x=Chill_portions)) + 
  geom_histogram(binwidth=1,aes(fill = factor(Data_source))) +
  theme_bw(base_size = 20) +
  labs(fill = "Data source") +
  xlab("Chill accumulation (Chill Portions)") +
  ylab("Frequency")



chill_simulations<-chill_comparison_full_seasons[
  which(chill_comparison_full_seasons$Data_source=="simulated"),]

ggplot(chill_simulations, aes(x=Chill_portions)) +
  stat_ecdf(geom = "step",lwd=1.5,col="blue") +
  ylab("Cumulative probability") +
  xlab("Chill accumulation (in Chill Portions)") +
  theme_bw(base_size = 20)

# Here's the amount of chill that is exceeded in 90% of all years.
quantile(chill_simulations$Chill_portions, 0.1)

# and here's the 50% confidence interval (25th to 75th percentile)
quantile(chill_simulations$Chill_portions, c(0.25,0.75))


