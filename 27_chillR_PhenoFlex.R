library(chillR)
library(ggplot2)
CKA_weather<-read_tab("data/TMaxTMin1958-2019_patched.csv")
hourtemps <- stack_hourly_temps(CKA_weather, latitude=50.6)

yc <- 40
zc <- 190
iSeason <- genSeason(hourtemps,
                     mrange = c(8, 6),
                     years=c(2009))

season_data<-hourtemps$hourtemps[iSeason[[1]],]

res <- PhenoFlex(temp=season_data$Temp,
                 times=c(1: length(season_data$Temp)),
                 zc=zc, stopatzc=TRUE, yc=yc, basic_output=FALSE)

DBreakDay <- res$bloomindex
seasontemps<-hourtemps$hourtemps[iSeason[[1]],]
seasontemps[,"x"]<-res$x
seasontemps[,"y"]<-res$y
seasontemps[,"z"]<-res$z
seasontemps<-add_date(seasontemps)

CR_full<-seasontemps$Date[which(seasontemps$y>=yc)[1]]
Bloom<-seasontemps$Date[which(seasontemps$z>=zc)[1]]

chillplot<-ggplot(data=seasontemps[1:DBreakDay,],aes(x=Date,y=y)) +
  geom_line(col="blue",lwd=1.5) +
  theme_bw(base_size=15) +
  geom_hline(yintercept=yc,lty=2,col="blue",lwd=1.2) +
  geom_vline(xintercept=CR_full,lty=3,col="blue",lwd=1.2) +
  ylab("Chill accumulation (y)") +
  labs(title="Chilling") +
  annotate("text",label="Chill req. (yc)", 
            x=ISOdate(2008,10,01),
            y=yc*1.1, col="blue",lwd=5)

heatplot<-ggplot(data=seasontemps[1:DBreakDay,],aes(x=Date,y=z)) +
  geom_line(col="red",lwd=1.5) +
  theme_bw(base_size=15) +
  scale_y_continuous(position = "right") +
  geom_hline(yintercept=zc,lty=2,col="red",lwd=1.2) +
  geom_vline(xintercept=CR_full,lty=3,col="blue",lwd=1.2) +
  geom_vline(xintercept=Bloom,lty=3,col="red",lwd=1.2) +
  ylab("Heat accumulation (z)") +
  labs(title="Forcing")  +
  annotate("text",label="Heat req. (zc)", 
            x=ISOdate(2008,10,01),
            y=zc*0.95, col="red",lwd=5)


library(patchwork)
chillplot + heatplot


yc <- 40
zc <- 190
seasons<-1959:2019

iSeason <- genSeason(hourtemps,
                     mrange = c(8, 6),
                     years=seasons)

for (sea in 1:length(seasons))
{season_data<-hourtemps$hourtemps[iSeason[[sea]],]
 res <- PhenoFlex(temp=season_data$Temp,
                 times=c(1: length(season_data$Temp)),
                 zc=zc, stopatzc=TRUE, yc=yc, basic_output=FALSE)
 if(sea==1)
    results<-season_data$DATE[res$bloomindex] else
      results<-c(results,season_data$DATE[res$bloomindex])}

predictions<-data.frame(Season=seasons,Prediction=results)
predictions$Prediction<-ISOdate(2001,
                                substr(predictions$Prediction,4,5),
                                substr(predictions$Prediction,1,2))

ggplot(data=predictions,aes(x=Season,y=Prediction)) +
  geom_smooth() +
  geom_point() +
  ylab("Predicted bloom date") +
  theme_bw(base_size=15)


CKA_weather<-read_tab("data/TMaxTMin1958-2019_patched.csv")
Alex<-read_tab("data/Alexander_Lucas_bloom_1958_2019.csv")
Alex_first<-Alex[,1:2]
Alex_first[,"Year"]<-substr(Alex_first$First_bloom,1,4)
Alex_first[,"Month"]<-substr(Alex_first$First_bloom,5,6)
Alex_first[,"Day"]<-substr(Alex_first$First_bloom,7,8)
Alex_first<-make_JDay(Alex_first)
Alex_first<-Alex_first[,c("Pheno_year","JDay")]
colnames(Alex_first)<-c("Year","pheno")
hourtemps <- stack_hourly_temps(CKA_weather, latitude=50.6)



# here's the order of the parameters (from the helpfile of the
# PhenoFlex_GDHwrapper function)
#          yc,  zc,  s1, Tu,    E0,      E1,     A0,         A1,   Tf, Tc, Tb,  slope
par <-   c(40, 190, 0.5, 25, 3372.8,  9900.3, 6319.5, 5.939917e13,  4, 36,  4,  1.60)
upper <- c(41, 200, 1.0, 30, 4000.0, 10000.0, 7000.0,       6.e13, 10, 40, 10, 50.00)
lower <- c(38, 180, 0.1, 0 , 3000.0,  9000.0, 6000.0,       5.e13,  0,  0,  0,  0.05)



SeasonList <- genSeasonList(hourtemps$hourtemps, mrange = c(8, 6), years=c(1959:2019))

Fit_res <- phenologyFitter(par.guess=par,
                           modelfn = PhenoFlex_GDHwrapper,
                           bloomJDays=Alex_first$pheno[which(Alex_first$Year>1958)],
                           SeasonList=SeasonList,
                           lower=lower,
                           upper=upper,
                           control=list(smooth=FALSE, verbose=FALSE, maxit=1000,
                                        nb.stop.improvement=5))


Alex_par<-Fit_res$par

write.csv(Alex_par,"data/PhenoFlex_parameters_Alexander_Lucas.csv")



Alex_par<-read_tab("data/PhenoFlex_parameters_Alexander_Lucas.csv")[,2]

SeasonList <- genSeasonList(hourtemps$hourtemps, mrange = c(8, 6), years=c(1959:2019))

Alex_PhenoFlex_predictions<-Alex_first[which(Alex_first$Year>1958),]

for(y in 1:length(Alex_PhenoFlex_predictions$Year))
   Alex_PhenoFlex_predictions$predicted[y]<-PhenoFlex_GDHwrapper(SeasonList[[y]],Alex_par)

Alex_PhenoFlex_predictions$Error<-
  Alex_PhenoFlex_predictions$predicted-Alex_PhenoFlex_predictions$pheno

RMSEP(Alex_PhenoFlex_predictions$predicted,Alex_PhenoFlex_predictions$pheno)
RPIQ(Alex_PhenoFlex_predictions$predicted,Alex_PhenoFlex_predictions$pheno)
mean(Alex_PhenoFlex_predictions$Error)
mean(abs(Alex_PhenoFlex_predictions$Error))

ggplot(Alex_PhenoFlex_predictions,aes(x=pheno,y=predicted)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  theme_bw(base_size = 15) +
  xlab("Observed bloom date (Day of the year)") +
  ylab("Predicted bloom date (Day of the year)") +
  ggtitle("Predicted vs. observed bloom dates")

ggplot(Alex_PhenoFlex_predictions,aes(Error)) +
  geom_histogram() +
  ggtitle("Distribution of prediction errors")

