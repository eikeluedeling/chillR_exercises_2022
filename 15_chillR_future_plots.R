library(chillR)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(ggpmisc)
library(patchwork)

chill_past_scenarios<-load_temperature_scenarios(
  "data/chill",
  "Bonn_historic")
chill_observed<-load_temperature_scenarios(
  "data/chill",
  "Bonn_observed")

chills <-make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE)

RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)

for(RCP in RCPs)
  for(Time in Times)
  { chill<-load_temperature_scenarios(
    "data/chill",
    paste0("Bonn_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    chills <-make_climate_scenario(
      chill,
      caption =c(RCPcaption, Time_caption),
      add_to = chills)
  }

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Frost_H",
  metric_label="Frost hours",
  texcex=1.5)

# We'll first process the past scenarios (element 1 of the chills list).
# Within the data element, we have a list of multiple data.frames for
# the various past scenarios.
# Using a 'for' loop, we cycle through all these data.frames.

for(nam in names(chills[[1]]$data))
  {
   # Extract the data frame.
   ch<-chills[[1]]$data[[nam]]
   # Add columns for the new information we have to add and fill them.
   ch[,"GCM"]<-"none"
   ch[,"RCP"]<-"none"
   ch[,"Year"]<-as.numeric(nam)
   
   # Now check if this is the first time we've gone through this loop.
   # If this is the first time, the ch data.frame becomes the output
   # object (past_simulated).
   # If it is not the first time ('else'), we add the current data.frame
   # to the 'past_simulated' object
  if(nam==names(chills[[1]]$data)[1])
    past_simulated<-ch else
      past_simulated<-rbind(past_simulated,ch)
  }

# We add another column called 'Scenario' and label all rows as 'Historic' 
past_simulated["Scenario"] <- "Historic"

kable(past_simulated[1:5,])  %>%
  kable_styling("striped", position = "left",font_size = 8)

# We'll want to add the historic observation too, so let's simplify the
# pointer to this information for easier use later

past_observed <- chills[[1]][["historic_data"]]

kable(past_observed[1:5,])  %>%
  kable_styling("striped", position = "left",font_size = 8)


# Extract future data
for(i in 2:length(chills))
  for(nam in names(chills[[i]]$data))
    {ch<-chills[[i]]$data[[nam]]
     ch[,"GCM"]<-nam
     ch[,"RCP"]<-chills[[i]]$caption[1]
     ch[,"Year"]<-chills[[i]]$caption[2]
     if(i==2&nam==names(chills[[i]]$data)[1])
       future_data<-ch else
         future_data<-rbind(future_data,ch)
  }

kable(future_data[1:5,])  %>%
  kable_styling("striped", position = "left",font_size = 8)


# Extract the model names
Models <- unique(future_data$GCM)
  
metric<-"Heat_GDH"
axis_label<-"Heat (in GDH)"

# get extreme values for the axis scale

rng<-range(past_observed[[metric]],
           past_simulated[[metric]],
           future_data[[metric]])  
rng


past_plot<-ggplot() +
  geom_boxplot(data = past_simulated,
               aes_string("as.numeric(Year)",metric,group="Year"),
               fill="skyblue")

past_plot



past_plot<-past_plot +
  scale_y_continuous(limits = c(0, round(rng[2] + rng[2]/10))) +
  labs(x = "Year", y = axis_label)

past_plot


past_plot<-past_plot +
  facet_grid(~ Scenario) +
  theme_bw(base_size = 15) 
  
past_plot

  
past_plot<-past_plot +  
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle=45, hjust=1)) 

past_plot



# add historic data
past_plot <- past_plot +
  geom_point(data = past_observed,
             aes_string("End_year",metric),
             col="blue")

past_plot



y<-2050

future_2050<-ggplot(data= future_data[which(future_data$Year==y),]) +
      geom_boxplot(aes_string("GCM", metric, fill="GCM"))

future_2050



future_2050<-future_2050 +
  facet_wrap(vars(RCP)) +
   scale_x_discrete(labels = NULL, expand = expansion(add = 1)) 



future_2050<-future_2050 +
  scale_y_continuous(limits = c(0, round(1.1*rng[2]))) +
    geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5)

future_2050


future_2050<-future_2050 +
  theme_bw(base_size = 15) +
  theme(axis.ticks.y = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
                 legend.position = "bottom",
                 legend.margin = margin(0, 0, 0, 0, "cm"),
                 legend.background = element_rect(),
                 strip.background = element_blank(),
                 strip.text = element_text(face = "bold"),
                 legend.box.spacing = unit(0, "cm"),
                 plot.subtitle = element_text(hjust = 0.5, vjust = -1, size = 15 * 1.05,
                                                       face = "bold")) 

future_2050


future_plot_list<-list()

for(y in c(2050,2085))
{
  future_plot_list[[which(y == c(2050,2085))]] <-
    ggplot(data= future_data[which(future_data$Year==y),]) +
    geom_boxplot(aes_string("GCM", metric, fill="GCM")) +
    facet_wrap(vars(RCP)) +
    scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
    scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
    geom_text_npc(aes(npcx = "center", npcy = "top", label = Year),
                  size = 5) +
    theme_bw(base_size = 15) +
    theme(axis.ticks.y = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom",
          legend.margin = margin(0, 0, 0, 0, "cm"),
          legend.background = element_rect(),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.box.spacing = unit(0, "cm"),
          plot.subtitle = element_text(
            hjust = 0.5,
            vjust = -1,
            size = 15 * 1.05,
            face = "bold")) 
}

future_plot_list

library(patchwork)

both_plots<-past_plot + future_plot_list

both_plots



plot <- both_plots +
           plot_layout(guides = "collect",
                       widths = c(1,rep(1.8,length(future_plot_list))))
         


plot<-plot & theme(legend.position = "bottom",
                 legend.text = element_text(size=8),
                 legend.title = element_text(size=10),
                 axis.title.x=element_blank())


plot


         
metric<-"Chill_CP"
axis_label<-"Chill (in CP)"

# get extreme values for the axis scale

rng<-range(past_observed[[metric]],past_simulated[[metric]],future_data[[metric]])  
past_plot<-ggplot() +
  geom_boxplot(data = past_simulated, aes_string("as.numeric(Year)",metric,group="Year"),fill="skyblue") +
  scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
  labs(x = "Year", y = axis_label) +
  facet_grid(~ Scenario) +
  theme_bw(base_size = 15) +  
  theme(strip.background = element_blank(),
           strip.text = element_text(face = "bold"),
           axis.text.x = element_text(angle=45, hjust=1)) +
  geom_point(data = past_observed,aes_string("End_year",metric),col="blue")

future_plot_list<-list()

for(y in c(2050,2085))
{
  future_plot_list[[which(y == c(2050,2085))]] <- ggplot(data= future_data[which(future_data$Year==y),]) +
      geom_boxplot(aes_string("GCM", metric, fill="GCM")) +
  facet_wrap(vars(RCP)) +
   scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
  scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
    geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) +
  theme_bw(base_size = 15) +
  theme(axis.ticks.y = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
                 legend.position = "bottom",
                 legend.margin = margin(0, 0, 0, 0, "cm"),
                 legend.background = element_rect(),
                 strip.background = element_blank(),
                 strip.text = element_text(face = "bold"),
                 legend.box.spacing = unit(0, "cm"),
                 plot.subtitle = element_text(hjust = 0.5, vjust = -1, size = 15 * 1.05,
                                                       face = "bold")) 
}

plot<- (past_plot +
        future_plot_list +
        plot_layout(guides = "collect",
                       widths = c(1,rep(1.8,length(future_plot_list))))
       ) & theme(legend.position = "bottom",
                 legend.text = element_text(size=8),
                 legend.title = element_text(size=10),
                 axis.title.x=element_blank())
plot


         
metric<-"Frost_H"
axis_label<-"Frost duration (in hours)"

# get extreme values for the axis scale

rng<-range(past_observed[[metric]],past_simulated[[metric]],future_data[[metric]])  
past_plot<-ggplot() +
  geom_boxplot(data = past_simulated, aes_string("as.numeric(Year)",metric,group="Year"),fill="skyblue") +
  scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
  labs(x = "Year", y = axis_label) +
  facet_grid(~ Scenario) +
  theme_bw(base_size = 15) +  
  theme(strip.background = element_blank(),
           strip.text = element_text(face = "bold"),
           axis.text.x = element_text(angle=45, hjust=1)) +
  geom_point(data = past_observed,aes_string("End_year",metric),col="blue")

future_plot_list<-list()

for(y in c(2050,2085))
{
  future_plot_list[[which(y == c(2050,2085))]] <- ggplot(data= future_data[which(future_data$Year==y),]) +
      geom_boxplot(aes_string("GCM", metric, fill="GCM")) +
  facet_wrap(vars(RCP)) +
   scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
  scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
    geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) +
  theme_bw(base_size = 15) +
  theme(axis.ticks.y = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
                 legend.position = "bottom",
                 legend.margin = margin(0, 0, 0, 0, "cm"),
                 legend.background = element_rect(),
                 strip.background = element_blank(),
                 strip.text = element_text(face = "bold"),
                 legend.box.spacing = unit(0, "cm"),
                 plot.subtitle = element_text(hjust = 0.5, vjust = -1, size = 15 * 1.05,
                                                       face = "bold")) 
}

plot<- (past_plot +
        future_plot_list +
        plot_layout(guides = "collect",
                       widths = c(1,rep(1.8,length(future_plot_list))))
       ) & theme(legend.position = "bottom",
                 legend.text = element_text(size=8),
                 legend.title = element_text(size=10),
                 axis.title.x=element_blank())
plot



plot_scenarios_gg<-function(past_observed,
                            past_simulated,
                            future_data,
                            metric,
                            axis_label)
{
  rng<-range(past_observed[[metric]],
             past_simulated[[metric]],
             future_data[[metric]])  
  past_plot<-ggplot() +
    geom_boxplot(data = past_simulated,
                 aes_string("as.numeric(Year)",metric,group="Year"),
                 fill="skyblue") +
    scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
    labs(x = "Year", y = axis_label) +
    facet_grid(~ Scenario) +
    theme_bw(base_size = 15) +  
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle=45, hjust=1)) +
    geom_point(data = past_observed,
               aes_string("End_year",metric),
               col="blue")
  
  future_plot_list<-list()
  
  for(y in c(2050,2085))
  {
    future_plot_list[[which(y == c(2050,2085))]] <-
      ggplot(data= future_data[which(future_data$Year==y),]) +
      geom_boxplot(aes_string("GCM", metric, fill="GCM")) +
      facet_wrap(vars(RCP)) +
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
      scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year),
                    size = 5) +
      theme_bw(base_size = 15) +
      theme(axis.ticks.y = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.margin = margin(0, 0, 0, 0, "cm"),
            legend.background = element_rect(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold"),
            legend.box.spacing = unit(0, "cm"),
            plot.subtitle = element_text(hjust = 0.5,
                                         vjust = -1,
                                         size = 15 * 1.05,
                                         face = "bold")) 
  }
  
  plot<- (past_plot +
            future_plot_list +
            plot_layout(guides = "collect",
                        widths = c(1,rep(1.8,length(future_plot_list))))
        ) & theme(legend.position = "bottom",
                  legend.text = element_text(size=8),
                  legend.title = element_text(size=10),
                  axis.title.x=element_blank())
  plot
  
}






plot_scenarios_gg(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Heat_GDH",
                  axis_label="Heat (in Growing Degree Hours)")
plot_scenarios_gg(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Chill_CP",
                  axis_label="Chill (in Chill Portions)")
plot_scenarios_gg(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Frost_H",
                  axis_label="Frost duration (in hours)")

ggsave("plots/Frost_H.png", width = 20,
       height = 10, units = "cm",
       dpi = 600)

