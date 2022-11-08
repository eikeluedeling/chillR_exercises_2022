## station_list<-handle_gsod(action="list_stations",
##                           location=c(7.10,50.73),
##                           time_interval=c(1990,2020))

station_list<-read.csv("data/station_list.csv")
weather<-list(database="GSOD",weather=read.csv("data/Bonn_weather.csv"))
cleaned_weather<-list(database="GSOD",weather=read.csv("data/Bonn_chillR_weather.csv"))

require(kableExtra)

kable(station_list) %>%
  kable_styling("striped", position = "left", font_size = 8)


 weather<-handle_gsod(action="download_weather",
                      location=station_list$chillR_code[4],
                      time_interval=c(1990,2020))

kable(weather[[1]]$weather[1:20,]) %>%
  kable_styling("striped", position = "left", font_size = 8)


cleaned_weather<-handle_gsod(weather)

kable(cleaned_weather[[1]]$weather[1:20,]) %>%
  kable_styling("striped", position = "left", font_size = 8)


write.csv(station_list,"data/station_list.csv",row.names=FALSE)
write.csv(weather,"data/Bonn_weather.csv",row.names=FALSE)
write.csv(cleaned_weather$weather,"data/Bonn_chillR_weather.csv",row.names=FALSE)
 
