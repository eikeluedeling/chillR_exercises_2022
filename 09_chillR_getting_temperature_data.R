station_list<-handle_gsod(action="list_stations",
                           location=c(7.10,50.73),
                           time_interval=c(1990,2020))

head(station_list)


weather<-handle_gsod(action="download_weather",
                     location=station_list$chillR_code[4],
                     time_interval=c(1990,2020))

weather[[1]]$weather[1:20,]


cleaned_weather<-handle_gsod(weather)

cleaned_weather[[1]]$weather[1:20,]

dir.create("data")
write.csv(station_list,"data/station_list.csv",row.names=FALSE)
write.csv(weather,"data/Bonn_weather.csv",row.names=FALSE)
write.csv(cleaned_weather$weather,"data/Bonn_chillR_weather.csv",row.names=FALSE)
 
### handle DWD

station_list_dwd<-handle_dwd(action="list_stations",
                          location=c(7.10,50.73),
                          time_interval=c(19900101,20201231))
head(station_list_dwd)
weather_dwd<-handle_dwd(action="download_weather",
                     location=station_list_dwd$Station_ID[2],
                     time_interval=c(19900101,20201231))
head(weather_dwd$`Königswinter-Heiderhof`$weather)
cleaned_weather_dwd<-handle_dwd(weather_dwd)

cleaned_weather_dwd$`Königswinter-Heiderhof`


write.csv(station_list_dwd,"data/station_list_dwd.csv",row.names=FALSE)
write.csv(weather_dwd,"data/KoeWiHeiderhof_weather.csv",row.names=FALSE)
write.csv(cleaned_weather_dwd$`Königswinter-Heiderhof`,"data/KoeWiHeiderhof_chillR_weather.csv",row.names=FALSE)
