require(chillR)
require(ggplot2)
require(reshape2)
require(kableExtra)


Alex<-read.csv("data/Alexander_Lucas_bloom_1958_2019.csv")
Alex<-melt(Alex, id.vars = "Pheno_year", value.name="YEARMODA")
Alex$Year<-as.numeric(substr(Alex$YEARMODA,1,4))
Alex$Month<-as.numeric(substr(Alex$YEARMODA,5,6))
Alex$Day<-as.numeric(substr(Alex$YEARMODA,7,8))
Alex<-make_JDay(Alex)

Alex_first<-Alex[which(Alex$variable=="First_bloom"),]

ggplot(Alex_first,aes(Pheno_year,JDay)) +
  geom_point() +
  ylab("First bloom date (day of the year)") +
  xlab ("Year") +
  theme_bw(base_size=12)

library(Kendall)
Kendall(x=Alex_first$Pheno_year,y=Alex_first$JDay)

x <- Alex_first$Pheno_year
y <- Alex_first$JDay

summary(lm(y ~ x))

ggplot(Alex_first,aes(Year,JDay)) +
  geom_point() +
  geom_smooth(method='lm', formula = y~x) +
  ylab("First bloom date (day of the year)") +
  xlab ("Year") +
  theme_bw(base_size=12)


summary(lm(y ~ poly(x,25)))

ggplot(Alex_first,aes(Year,JDay)) +
  geom_point() +
  geom_smooth(method='lm', formula = y ~ poly(x,25)) +
  ylab("First bloom date (day of the year)") +
  xlab ("Year") +
  theme_bw(base_size=12)

temperature<-read.csv("data/TMaxTMin1958-2019_patched.csv")

Tmin<-aggregate(temperature$Tmin,by=list(temperature$Year),FUN=mean)
Tmax<-aggregate(temperature$Tmax,by=list(temperature$Year),FUN=mean)
Annual_means<-Tmin %>% cbind(Tmax[,2]) %>% cbind((Tmin[,2]+Tmax[,2])/2)
colnames(Annual_means)<-c("Year","Tmin","Tmax","Tmean")
Annual_means<-merge(Annual_means,Alex_first)

Annual_means_melted<-melt(Annual_means[,c(1:4,10)],id=c("Year","JDay"))

ggplot(Annual_means_melted,aes(x=value,y=JDay)) + geom_point() +
  geom_smooth(method="lm",formula=y~x) + facet_wrap("variable")

summary(lm(Annual_means$JDay ~ Annual_means$Tmin))
summary(lm(Annual_means$JDay ~ Annual_means$Tmax))
summary(lm(Annual_means$JDay ~ Annual_means$Tmean))


temps_JDays<-make_JDay(temperature)

corr_temp_pheno<-function(start_JDay, # the start JDay of the period
                          end_JDay, # the start JDay of the period
                          temps_JDay=temps_JDays, # the temperature dataset
                          bloom=Alex_first) # a data.frame with bloom dates
{
  temps_JDay[,"Season"]<-temps_JDay$Year
  if(start_JDay>end_JDay) temps_JDay$Season[which(temps_JDay$JDay>=start_JDay)]<-
      temps_JDay$Year[which(temps_JDay$JDay>=start_JDay)]+1
  if(start_JDay>end_JDay) sub_temps<-subset(temps_JDay,JDay<=end_JDay|JDay>=start_JDay)
  if(start_JDay<=end_JDay) sub_temps<-subset(temps_JDay,JDay<=end_JDay&JDay>=start_JDay)
  mean_temps<-aggregate(sub_temps[,c("Tmin","Tmax")],by=list(sub_temps$Season),FUN=mean)
  mean_temps[,"Tmean"]<-(mean_temps$Tmin+mean_temps$Tmax)/2
  colnames(mean_temps)[1]<-c("Pheno_year")
  temps_bloom<-merge(mean_temps,bloom[c("Pheno_year","JDay")])
  # Let's just extract the slopes of the regression model for now
  slope_Tmin<-summary(lm(temps_bloom$JDay~temps_bloom$Tmin))$coefficients[2,1]
  slope_Tmean<-summary(lm(temps_bloom$JDay~temps_bloom$Tmean))$coefficients[2,1]
  slope_Tmax<-summary(lm(temps_bloom$JDay~temps_bloom$Tmax))$coefficients[2,1]
  
  c(start_JDay=start_JDay,
    end_JDay=end_JDay,
    length=length(unique(sub_temps$JDay)),
    slope_Tmin=slope_Tmin,
    slope_Tmean=slope_Tmean,
    slope_Tmax=slope_Tmax)
}

corr_temp_pheno(305,29,temps_JDays,Alex_first)

corr_temp_pheno(305,45)


library(colorRamps) # for the color scheme we'll use in the plot

stJDs<-seq(1,366,10)
eJDs<-seq(1,366,10)

for(stJD in stJDs)
  for(eJD in eJDs)
    {correlations<-corr_temp_pheno(stJD,eJD)
    if(stJD==1&eJD==1) corrs<-correlations else
      corrs<-rbind(corrs,correlations)
}


slopes<-melt(as.data.frame(corrs),id.vars=c("start_JDay","end_JDay","length"))

ggplot(data=slopes, aes(x=start_JDay,y=length,fill=value)) +
  geom_tile() +
  facet_wrap(vars(variable)) +
  scale_fill_gradientn(colours=matlab.like(15)) +
  ylab("Interval duration (days)") + 
  xlab("Start date of temperature summary interval (Day of year)") +
  theme_bw(base_size = 12)

