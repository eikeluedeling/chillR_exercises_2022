require(tidyr)
require(reshape2)
require(dplyr)
require(kableExtra)

library(chillR)
Chilling_Hours

?Chilling_Hours

weather<-fix_weather(KA_weather[which(KA_weather$Year>2006),])
hourtemps<-stack_hourly_temps(weather,latitude=50.4)
Chilling_Hours(hourtemps$hourtemps$Temp)



Chilling_Hours(Winters_hours_gaps$Temp)[1:100]

Utah_Model

?step_model

Utah_Model(Winters_hours_gaps$Temp)[1:100]

step_model

?step_model

df<-data.frame(
  lower=c(-1000,1,2,3,4,5,6),
  upper=c(1,2,3,4,5,6,1000),
  weight=c(0,1,2,3,2,1,0))

kable(df) %>%
  kable_styling("striped", position = "left", font_size = 10)

custom<-function(x) {step_model(x,df)}

custom(Winters_hours_gaps$Temp)[1:100]


Dynamic_Model(Winters_hours_gaps$Temp)[1:100]

?Dynamic_Model

Dynamic_Model

Winter_JDay<-make_JDay(Winters_hours_gaps)

output<-chilling(make_JDay(Winters_hours_gaps),
                 Start_JDay = 90,
                 End_JDay = 100)

kable(output) %>%
  kable_styling("striped", position = "left", font_size = 10)


output<-tempResponse(make_JDay(Winters_hours_gaps),
                     Start_JDay = 90,
                     End_JDay = 100,
                     models=list(Chill_Portions=Dynamic_Model, GDH=GDH))

kable(output) %>%
  kable_styling("striped", position = "left", font_size = 10)

output<-tempResponse(make_JDay(Winters_hours_gaps),
                     Start_JDay = 90,
                     End_JDay = 100,
                     models=list(Chill_Portions=Dynamic_Model, GDH=GDH, Elvis=custom))
