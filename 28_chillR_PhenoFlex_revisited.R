library(tufte)


library(ggplot2)
library(tidyr)
require(reshape2)
library(chillR)


apply_const_temp <-
  function(temp, A0, A1, E0, E1, Tf, slope, portions=1200, deg_celsius=TRUE)
    {
  temp_vector <- rep(temp, times=portions)
  res <- chillR::DynModel_driver(temp=temp_vector,
                         A0=A0, A1=A1,
                         E0=E0, E1=E1,
                         Tf=Tf,
                         slope=slope,
                         deg_celsius=deg_celsius)
  return(res$y[length(res$y)])
}

gen_bell <- function(par, temp_values=seq(-5, 20, 0.1)) {
  E0 <- par[5]
  E1 <- par[6]
  A0 <- par[7]
  A1 <- par[8]
  Tf <- par[9]
  slope <- par[12]

  y <- c()
  for(i in seq_along(temp_values)) {
    y[i] <- apply_const_temp(temp=temp_values[i],
                             A0=A0, A1=A1, E0=E0, E1=E1, Tf=Tf, slope=slope)
  }
  return(invisible(y))
}

GDH_response<-function(T, par)
{Tb<-par[11]
 Tu<-par[4]
 Tc<-par[10]
 GDH_weight <- rep(0, length(T))
 GDH_weight[which(T >= Tb & T <= Tu)] <-
   1/2 * (1 + cos(pi + pi * (T[which(T >= Tb & T <= Tu)] - Tb)/(Tu - Tb)))
 GDH_weight[which(T > Tu & T <= Tc)] <-
   (1 + cos(pi/2 + pi/2 * (T[which(T >  Tu & T <= Tc)] -Tu)/(Tc - Tu)))
  return(GDH_weight)
}



Alex_par<-read_tab("data/PhenoFlex_parameters_Alexander_Lucas.csv")[,2]

temp_values=seq(-5, 30, 0.1)

temp_response<-data.frame(Temperature=temp_values,
                          Chill_response=gen_bell(Alex_par, temp_values),
                          Heat_response=GDH_response(temp_values,Alex_par))

melted_response<-melt(temp_response,id.vars="Temperature")

ggplot(melted_response,aes(x=Temperature,y=value)) +
  geom_line(size=2,aes(col=variable)) +
  ylab("Temperature response (arbitrary units)") +
  xlab("Temperature (°C)") +
  facet_wrap(vars(variable),scales="free",
             labeller = labeller(variable=c(Chill_response=c("Chill response"),
                                 Heat_response=c("Heat response")))) +
  scale_color_manual(values = c("Chill_response" = "blue", "Heat_response" = "red")) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")


## latitude<-50.6
## 
## month_range<-c(10,11,12,1,2,3)
## 
## Tmins=c(-20:20)
## Tmaxs=c(-15:30)
## 
## mins<-NA
## maxs<-NA
## chill_eff<-NA
## heat_eff<-NA
## month<-NA
## 
## simulation_par<-Alex_par
## 
## for(mon in month_range)
##     {days_month<-as.numeric(difftime( ISOdate(2002,mon+1,1),
##                                            ISOdate(2002,mon,1) ))
##     if(mon==12) days_month<-31
##     weather<-make_all_day_table(data.frame(Year=c(2001,2002),
##                                          Month=c(mon,mon),
##                                          Day=c(1,days_month),Tmin=c(0,0),Tmax=c(0,0)))
##     for(tmin in Tmins)
##       for(tmax in Tmaxs)
##         if(tmax>=tmin)
##           {
##           weather$Tmin<-tmin
##           weather$Tmax<-tmax
##           hourtemps<-stack_hourly_temps(weather,
##                                         latitude=latitude)$hourtemps$Temp
##           chill_eff<-c(chill_eff,
##                        PhenoFlex(temp=hourtemps,
##                                  times=c(1: length(hourtemps)),
##                                  A0=simulation_par[7],
##                                  A1=simulation_par[8],
##                                  E0=simulation_par[5],
##                                  E1=simulation_par[6],
##                                  Tf=simulation_par[9],
##                                  slope=simulation_par[12],
##                                  deg_celsius=TRUE,
##                                  basic_output=FALSE)$y[length(hourtemps)]/
##                                          (length(hourtemps)/24))
##           heat_eff<-c(heat_eff,
##                       cumsum(GDH_response(hourtemps,
##                                           simulation_par))[length(hourtemps)]/
##                                                  (length(hourtemps)/24))
##           mins<-c(mins,tmin)
##           maxs<-c(maxs,tmax)
##           month<-c(month,mon)
##         }
##     }
## results<-data.frame(Month=month,Tmin=mins,Tmax=maxs,Chill_eff=chill_eff,Heat_eff=heat_eff)
## results<-results[!is.na(results$Month),]
## 
## 
## write.csv(results,"data/model_sensitivity_PhenoFlex.csv")
## 


Chill_sensitivity_temps<-function(chill_model_sensitivity_table,
                                  temperatures,
                                  temp_model,
                                  month_range=c(10,11,12,1,2,3),
                                  Tmins=c(-20:20),
                                  Tmaxs=c(-15:30),
                                  legend_label="Chill/day (CP)")
{
  library(ggplot2)
  library(colorRamps)

  cmst<-chill_model_sensitivity_table
  cmst<-cmst[which(cmst$Month %in% month_range),]
  cmst$Month_names<- factor(cmst$Month, levels=month_range,
                            labels=month.name[month_range])  
  
  DM_sensitivity<-ggplot(cmst,aes_string(x="Tmin",y="Tmax",fill=temp_model)) +
    geom_tile() +
    scale_fill_gradientn(colours=alpha(matlab.like(15), alpha = .5),
                         name=legend_label) +
    xlim(Tmins[1],Tmins[length(Tmins)]) +
    ylim(Tmaxs[1],Tmaxs[length(Tmaxs)])
  
  temperatures<-
    temperatures[which(temperatures$Month %in% month_range),]
  temperatures[which(temperatures$Tmax<temperatures$Tmin),
               c("Tmax","Tmin")]<-NA
  temperatures$Month_names <- factor(temperatures$Month,
                                     levels=month_range, labels=month.name[month_range])  
  
  DM_sensitivity +
    geom_point(data=temperatures,
               aes(x=Tmin,y=Tmax,fill=NULL,color="Temperature"),
               size=0.2) +
    facet_wrap(vars(Month_names)) +
    scale_color_manual(values = "black",
                       labels = "Daily temperature \nextremes (°C)",
                       name="Observed at site" ) +
    guides(fill = guide_colorbar(order = 1),
           color = guide_legend(order = 2)) +
    ylab("Tmax (°C)") +
    xlab("Tmin (°C)") + 
    theme_bw(base_size=15)

}




Model_sensitivities_PhenoFlex<-
  read.csv("data/model_sensitivity_PhenoFlex.csv")
CKA_weather<-read_tab("data/TMaxTMin1958-2019_patched.csv")


Chill_sensitivity_temps(Model_sensitivities_PhenoFlex,
                        CKA_weather,
                        temp_model="Chill_eff",
                        month_range=c(10,11,12,1,2,3),
                        Tmins=c(-20:20),
                        Tmaxs=c(-15:30),
                        legend_label="Chill per day \n(arbitrary)") +
  ggtitle("PhenoFlex chill efficiency ('Alexander Lucas')")




Chill_sensitivity_temps(Model_sensitivities_PhenoFlex,
                        CKA_weather,
                        temp_model="Heat_eff",
                        month_range=c(10,11,12,1,2,3),
                        Tmins=c(-20:20),
                        Tmaxs=c(-15:30),
                        legend_label="Heat per day \n(arbitrary)") +
  ggtitle("PhenoFlex heat efficiency ('Alexander Lucas')")


