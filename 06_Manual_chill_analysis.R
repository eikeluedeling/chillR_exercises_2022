#install.packages("chillR")
library(chillR)
#library(knitr)
#library(pander)
#library(kableExtra)

?"chillR-package"
?chilling

?Winters_hours_gaps

Winters_hours_gaps

Winters_hours_gaps[3,4]
Winters_hours_gaps[3,"Year"]
Winters_hours_gaps$Temp[8]

Winters_hours_gaps[3,]
Winters_hours_gaps[,4]
Winters_hours_gaps[,]

c(1,2,3,6,7,9)

Winters_hours_gaps[c(1,2,3,6,7,9),]
Winters_hours_gaps[c(1,2,3,6,7,9),c("Year","Month","Day","Hour","Temp")]

a<-1
b<-2

hourtemps <- Winters_hours_gaps[,c("Year","Month","Day","Hour","Temp")]



hourtemps[3,]
hourtemps[3,"Temp"]
hourtemps$Temp[1:5]
hourtemps[1:5,]

1>4
1<4
1<=4
1<=1
1==1
1==3
!1==3

c(1,2,3,4,5,6)<4

c(1,2,3,4,5,6)<4&c(1,2,3,4,5,6)>2


hourtemps$Temp >= 0 & hourtemps$Temp <= 7.2


hourtemps[,"Chilling_Hour"] <- hourtemps$Temp >= 0 & hourtemps$Temp <= 7.2
hourtemps

hourtemps[13:20,]

sum(hourtemps$Chilling_Hour[13:20])

which(hourtemps$Year==2008)
which(hourtemps$Month==10)

which(hourtemps$Year==2008 & hourtemps$Month==10 & hourtemps$Day==1 & hourtemps$Hour==12)


Start_Date<-which(hourtemps$Year==2008 & hourtemps$Month==10 &
                    hourtemps$Day==1 & hourtemps$Hour==0)

End_Date<-which(hourtemps$Year==2008 & hourtemps$Month==10 &
                  hourtemps$Day==31 & hourtemps$Hour==23)

sum(hourtemps$Chilling_Hour[Start_Date:End_Date])


CH <- function(hour_temps)
  {hour_temps}

CH(hourtemps)
CH("Good morning")

CH <- function(hour_temps)
{hour_temps[,"Chilling_Hour"] <- hour_temps$Temp >= 0 & hour_temps$Temp <= 7.2
 return(hour_temps)
}

CH("Good morning")
CH(hourtemps)

chill<-CH(hourtemps)

CH_sum <- function(hour_temps)
{hour_temps[,"Chilling_Hour"] <- hour_temps$Temp >= 0 & hour_temps$Temp <= 7.2
 CHs<-sum(hour_temps[,"Chilling_Hour"])
return(CHs)
}

CH_sum(hourtemps)



CH_sum_dates <- function(hour_temps,Start_year, Start_month, Start_day, Start_hour,
                         End_year, End_month, End_day, End_hour)
  
{
  Start_Date<-which(hourtemps$Year==Start_year &
                    hourtemps$Month==Start_month &
                    hourtemps$Day==Start_day &
                    hourtemps$Hour==Start_hour)
  
  End_Date<-which(hourtemps$Year==End_year &
                  hourtemps$Month==End_month &
                  hourtemps$Day==End_day &
                  hourtemps$Hour==End_hour)
  
  
  hour_temps[,"Chilling_Hour"] <- hour_temps$Temp >= 0 & hour_temps$Temp <= 7.2
  CHs<-sum(hour_temps[Start_Date:End_Date,"Chilling_Hour"])
  return(CHs)
}

CH_sum_dates(hourtemps, Start_year=2008, Start_month=3, 15, 0, 2008, 11, 1, 12)

YEARMODAHO
2008031504


CH_sum_dates <- function(hour_temps,
                         Start_YEARMODAHO,
                         End_YEARMODAHO)
{
  Start_year<-as.numeric(substr(Start_YEARMODAHO,1,4))
  Start_month<-as.numeric(substr(Start_YEARMODAHO,5,6))
  Start_day<-as.numeric(substr(Start_YEARMODAHO,7,8))
  Start_hour<-as.numeric(substr(Start_YEARMODAHO,9,10))
  
  End_year<-as.numeric(substr(End_YEARMODAHO,1,4))
  End_month<-as.numeric(substr(End_YEARMODAHO,5,6))
  End_day<-as.numeric(substr(End_YEARMODAHO,7,8))
  End_hour<-as.numeric(substr(End_YEARMODAHO,9,10))
  
  Start_Date<-which(hourtemps$Year==Start_year &
                      hourtemps$Month==Start_month &
                      hourtemps$Day==Start_day &
                      hourtemps$Hour==Start_hour)
  
  End_Date<-which(hourtemps$Year==End_year &
                    hourtemps$Month==End_month &
                    hourtemps$Day==End_day &
                    hourtemps$Hour==End_hour)

  hour_temps[,"Chilling_Hour"] <- hour_temps$Temp >= 0 & hour_temps$Temp <= 7.2
  CHs<-sum(hour_temps[Start_Date:End_Date,"Chilling_Hour"])
  return(CHs)
}

CH_sum_dates(hourtemps, 2008040103, 2008111002)

