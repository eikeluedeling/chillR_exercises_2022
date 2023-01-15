library(chillR)
library(leaflet)
library(dplyr)
library(reshape2)
library(kableExtra)
library(ggplot2)
library(Kendall)

## library(leaflet)
## library(dplyr)

leaflet() %>%
  setView(lng=6.99, lat=50.625, zoom=12) %>%
  addTiles() %>%
  addMarkers(lng=6.99, lat=50.625, popup="Campus Klein-Altendorf")


CKA_Alexander_Lucas<-read.csv("data/Alexander_Lucas_bloom_1958_2019.csv")
CKA_weather<-read.csv("data/TMaxTMin1958-2019_patched.csv")


head(CKA_Alexander_Lucas)


Alexander_Lucas<-melt(CKA_Alexander_Lucas,
                      id.vars = "Pheno_year",
                      value.name="YEARMODA")
Alexander_Lucas$Year<-as.numeric(substr(Alexander_Lucas$YEARMODA,1,4))
Alexander_Lucas$Month<-as.numeric(substr(Alexander_Lucas$YEARMODA,5,6))
Alexander_Lucas$Day<-as.numeric(substr(Alexander_Lucas$YEARMODA,7,8))
Alexander_Lucas<-make_JDay(Alexander_Lucas)

head(Alexander_Lucas)



ggplot(data=Alexander_Lucas,aes(Pheno_year,JDay,col=variable)) +
  geom_line() +
  theme_bw(base_size=15) +
  scale_color_discrete(
    name="Phenological event",
    labels=c("First bloom", "Full bloom", "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)")


ggplot(data=Alexander_Lucas,aes(Pheno_year,JDay,col=variable)) +
  geom_line() +
  theme_bw(base_size=15) +
  scale_color_discrete(name="Phenological event", labels=c("First bloom", "Full bloom", "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_smooth(method = "lm")

ggplot(data=Alexander_Lucas,aes(Pheno_year,JDay,col=variable)) +
  geom_smooth() +
  theme_bw(base_size=15) +
  scale_color_discrete(
    name="Phenological event",
    labels=c("First bloom", "Full bloom", "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") 

require(Kendall)
Kendall_first<-Kendall(x=Alexander_Lucas$Pheno_year[which(Alexander_Lucas$variable=="First_bloom")], y=Alexander_Lucas$JDay[which(Alexander_Lucas$variable=="First_bloom")])
Kendall_full<-Kendall(x=Alexander_Lucas$Pheno_year[which(Alexander_Lucas$variable=="Full_bloom")], y=Alexander_Lucas$JDay[which(Alexander_Lucas$variable=="Full_bloom")])
Kendall_last<-Kendall(x=Alexander_Lucas$Pheno_year[which(Alexander_Lucas$variable=="Last_bloom")], y=Alexander_Lucas$JDay[which(Alexander_Lucas$variable=="Last_bloom")])

Kendall_first
Kendall_full
Kendall_last


linear_trend_first<-lm(
  Alexander_Lucas$JDay[
    which(Alexander_Lucas$variable=="First_bloom")]~
    Alexander_Lucas$Pheno_year[
      which(Alexander_Lucas$variable=="First_bloom")])
linear_trend_full<-lm(
  Alexander_Lucas$JDay[
    which(Alexander_Lucas$variable=="Full_bloom")]~
    Alexander_Lucas$Pheno_year[
      which(Alexander_Lucas$variable=="First_bloom")])
linear_trend_last<-lm(
  Alexander_Lucas$JDay[
    which(Alexander_Lucas$variable=="Last_bloom")]~
    Alexander_Lucas$Pheno_year[
      which(Alexander_Lucas$variable=="First_bloom")])

linear_trend_first
linear_trend_full
linear_trend_last

phenology_trends<-
  data.frame(Stage=c("First bloom", "Full bloom", "Last bloom"),
             Kendall_tau=
               c(Kendall_first[[1]][1],
                 Kendall_full[[1]][1],
                 Kendall_last[[1]][1]),
             Kendall_p  =
               c(Kendall_first[[2]][1],
                 Kendall_full[[2]][1],
                 Kendall_last[[2]][1]),
             Linear_trend=
               c(linear_trend_first[[1]][2],
                 linear_trend_full[[1]][2],
                 linear_trend_last[[1]][2])
             )

phenology_trends


frost_df=data.frame(
  lower=c(-1000,0),
  upper=c(0,1000),
  weight=c(1,0))

frost_model<-function(x) step_model(x,frost_df)



hourly<-stack_hourly_temps(CKA_weather,latitude=50.625)
frost<-tempResponse(hourly,models=c(frost=frost_model))

ggplot(frost,aes(End_year,frost)) +
  geom_smooth() +
  geom_point() +
  ylim(c(0,NA)) +
  ylab("Frost hours per year") +
  xlab("Year")


Kendall(x=frost$End_year,y=frost$frost)
lm(frost$frost~frost$End_year)


frost_model_no_summ<-function(x) step_model(x, frost_df, summ=FALSE)

hourly$hourtemps[,"frost"]<-frost_model_no_summ(hourly$hourtemps$Temp)

Daily_frost_hours<-aggregate(hourly$hourtemps$frost,
                             by=list(hourly$hourtemps$YEARMODA),
                             FUN=sum)

Daily_frost<-make_JDay(CKA_weather)

Daily_frost[,"Frost_hours"]<-Daily_frost_hours$x


Daily_frost$Frost_hours[which(Daily_frost$Frost_hours==0)]<-NA

ggplot(data=Daily_frost,aes(Year,JDay,size=Frost_hours)) +
  geom_point(col="light blue",alpha = 0.8) + 
  scale_size(range = c(0, 3),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name="Frost hours") +
  theme_bw(base_size=15)

ggplot(data=Alexander_Lucas,
       aes(Pheno_year,JDay,col=variable)) +
  geom_line() +
  theme_bw(base_size=15) +
  scale_color_discrete(
    name="Phenological event",
    labels=c("First bloom", "Full bloom", "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_point(data=Daily_frost,
             aes(Year,JDay,size=Frost_hours),
             col="light blue",alpha = 0.8) + 
  scale_size(range = c(0, 3),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name="Frost hours") +
  theme_bw(base_size=15)



ggplot(data=Alexander_Lucas,
       aes(Pheno_year,JDay,col=variable)) +
  geom_line() +
  theme_bw(base_size=15) +
  scale_color_discrete(
    name="Phenological event",
    labels=c("First bloom", "Full bloom", "Last bloom")) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_point(data=Daily_frost,
             aes(Year,JDay,size=Frost_hours),
             col="light blue",
             alpha = 0.8) + 
  scale_size(range = c(0, 3),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name="Frost hours") +
  theme_bw(base_size=15) +
  ylim(c(80,140))



Ribbon_Lucas<-dcast(
  Alexander_Lucas,Pheno_year ~ variable, value.var = "JDay")

ggplot(data=Ribbon_Lucas,aes(Pheno_year)) +
  geom_ribbon(aes(ymin = First_bloom, ymax = Last_bloom),
              fill = "light gray") +
  geom_line(aes(y = Full_bloom)) +
  theme_bw(base_size=15) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_point(data=Daily_frost,
             aes(Year,JDay,size=Frost_hours),
             col="light blue",
             alpha = 0.8) + 
  scale_size(range = c(0, 3),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name="Frost hours") +
  theme_bw(base_size=15) +
  ylim(c(77,140))


# identify frost events that overlap with bloom
lookup_dates<-Ribbon_Lucas
row.names(lookup_dates)<-lookup_dates$Pheno_year

Daily_frost[,"First_bloom"]<-
  lookup_dates[as.character(Daily_frost$Year),"First_bloom"]
Daily_frost[,"Last_bloom"]<-
  lookup_dates[as.character(Daily_frost$Year),"Last_bloom"]

Daily_frost[
  which(!is.na(Daily_frost$Frost_hours)),"Bloom_frost"]<-
  "Before bloom"
Daily_frost[
  which(Daily_frost$JDay>=Daily_frost$First_bloom),"Bloom_frost"]<-
  "During bloom"
Daily_frost[
  which(Daily_frost$JDay>Daily_frost$Last_bloom),"Bloom_frost"]<-
  "After bloom"
Daily_frost[
  which(Daily_frost$JDay>180),"Bloom_frost"]<-
  "Before bloom"

ggplot(data=Ribbon_Lucas,aes(Pheno_year)) +
  geom_ribbon(aes(ymin = First_bloom, ymax = Last_bloom),
              fill = "light gray") +
  geom_line(aes(y = Full_bloom)) +
  theme_bw(base_size=15) +
  xlab("Phenological year") +
  ylab("Julian date (day of the year)") +
  geom_point(data=Daily_frost,
             aes(Year,JDay,size=Frost_hours,col=Bloom_frost),
             alpha = 0.8) + 
  scale_size(range = c(0, 5),
             breaks = c(1, 5, 10, 15, 20),
             labels = c("1", "5", "10", "15", "20"),
             name="Frost hours") +
  scale_color_manual(
    breaks=c("Before bloom", "During bloom", "After bloom"),
    values=c("light green","red","light blue"),
    name="Frost timing") +
  theme_bw(base_size=15) +
  ylim(c(77,140))


Bloom_frost_trend<-aggregate(
  Daily_frost$Frost_hours,
  by=list(Daily_frost$Year,Daily_frost$Bloom_frost),
  FUN=function(x) sum(x,na.rm=TRUE))
colnames(Bloom_frost_trend)<-c("Year","Frost_timing","Frost_hours")

DuringBloom<-
  Bloom_frost_trend[which(Bloom_frost_trend$Frost_timing=="During bloom"),]

ggplot(data=DuringBloom,aes(Year,Frost_hours)) +
  geom_col() 


Kendall(x=DuringBloom$Year,y=DuringBloom$Frost_hours)
lm(DuringBloom$Frost_hours~DuringBloom$Year)
