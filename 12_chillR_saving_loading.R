library(chillR)
library(ggplot2)
library(kableExtra)
Temperatures<-read_tab("data/Temperatures.csv")

kable(head(Temperatures))  %>%
  kable_styling("striped", position = "left",font_size = 10)

write.csv(Temperatures, file="data/Temperatures.csv", row.names = FALSE)

Temperatures<-read_tab("data/Temperatures.csv")

kable(head(Temperatures)) %>%
  kable_styling("striped", position = "left",font_size = 10)

test_list<-list(Number=1,
                String="Thanks for using chillR!",
                DateFrame=data.frame(a=c(1,2,3),b=c(3,2,1),c=c(5,4,3)))
 
save_temperature_scenarios(test_list,"data","test_list")
 

test_list<-load_temperature_scenarios("data","test_list")
 

