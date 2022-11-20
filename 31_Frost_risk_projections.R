# Load all the required packages for this lesson
library(chillR)
library(tidyverse)
library(patchwork)
library(cowplot)

# Import the past weather dataset to build the hull plots for model validity domains
# Note: data from 01.01.1958 to 20.06.2019. Not complete season for bloom in 1958 (missing Oct-Dec 1957). Barely passing the threshold for bloom in 2019 (need to set mrange to c(9, 5) in genSeasonList for PhenoFlex calibration)
past_weather <- read_tab("data/TMaxTMin1958-2019_patched.csv")

# Add the column RCP_Time for further discrimination with future scenarios
past_weather$RCP_Time <- "Past"

# Summarize the data by month. Compute the mean of Tmin and Tmax by RCP_Time, Year, and Month
past_months <- past_weather %>% group_by(RCP_Time, Year, Month) %>% summarize(Tmin = mean(Tmin, na.rm = TRUE),
                                                                              Tmax = mean(Tmax, na.rm = TRUE))


# Load all data for future scenarios (combination of Time and RCP)
future_temps <- list("Bonn_2050_rcp45" = load_temperature_scenarios("data/Weather_2", "Bonn_2050_rcp45"),
                     "Bonn_2050_rcp85" = load_temperature_scenarios("data/Weather_2", "Bonn_2050_rcp85"),
                     "Bonn_2085_rcp45" = load_temperature_scenarios("data/Weather_2", "Bonn_2085_rcp45"),
                     "Bonn_2085_rcp85" = load_temperature_scenarios("data/Weather_2", "Bonn_2085_rcp85"))

# Combine the datesets of each list into a single dataset per scenario
future_temps <- data.frame(bind_rows(lapply(future_temps, function (x) bind_rows(x, .id = "GCM")), .id = "List"))

# Re-structure the data. Add the RCP, Time and RCP_Time columns
future_temps <- future_temps %>% mutate(RCP = if_else(substr(.$List, 11, 16) == "rcp45", "RCP4.5", "RCP8.5"),
                                        Time = substr(.$List, 6, 9)) %>% 
  select(DATE, Year, Month, Day, Tmin, Tmax, RCP, Time, GCM) %>% 
  mutate(RCP_Time = paste0(.$RCP, "_", .$Time))
  
# Summarize the data by month. Compute the mean of Tmin and Tmax by RCP_Time, Year, and Month
future_months <- future_temps %>% group_by(RCP_Time, Year, Month) %>%  summarize(Tmin = mean(Tmin, na.rm = TRUE),
                                                                                 Tmax = mean(Tmax, na.rm = TRUE))
  

# Merge the past and future months to plot them together
all_months <- rbind(past_months, future_months)

# Add a column for the name of the month 
all_months$month_name <- factor(all_months$Month, levels = c(6 : 12, 1 : 5), labels = month.name[c(6 : 12, 1 : 5)])

# Calculate the hulls for each group
hull_temps <- all_months %>% group_by(RCP_Time, month_name) %>% slice(chull(Tmin, Tmax))


# Load the weather data from the experimental seasons to generate an "Enhanced" temps category
enhanced <- read.csv("data/final_weather_data_S1_S2_pear_hourly.csv")

# Summarize the data. Compute the minimum and maximum records in a daily basis
enhanced <- enhanced %>% group_by(YEARMODA, Treatment, Year, Month, Day, JDay) %>% 
  summarize(Tmin = min(Temp, na.rm = TRUE),
            Tmax = max(Temp, na.rm = TRUE))

# Summarize the data by month now. Compute mean across minimum and maximum records
enhanced <- enhanced %>% group_by(Treatment, Month) %>% summarize(Tmin = mean(Tmin),
                                                                  Tmax = mean(Tmax))

# Re-format the column Year and add the column RCP_Time
enhanced$Year <- enhanced$Treatment
enhanced$RCP_Time <- "Past_enhanced"

# Combining the past and the enhanced temps
past_months$RCP_Time <- "Past_combined"
enhanced$RCP_Time <- "Past_combined"

# Merge all the data. Simulated scenarios, observed scenarios, and enhanced temps
all_months_both <- rbind(enhanced, past_months, future_months)

# Add the labels for the month no
all_months_both$month_name <- factor(all_months_both$Month, levels = c(6 : 12, 1 : 5), labels = month.name[c(6 : 12, 1 : 5)])

# Create the hull
hull_temps_both <- all_months_both %>% group_by(RCP_Time, month_name) %>% slice(chull(Tmin, Tmax))

# Implement the plot showing the overlap among conditions
ggplot(hull_temps_both[which(hull_temps_both$Month %in% c(10,11,12,1,2,3)),],
       aes(Tmin, Tmax, fill = factor(RCP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past_combined",
                             "RCP4.5_2050",
                             "RCP4.5_2085",
                             "RCP8.5_2050",
                             "RCP8.5_2085"),
                    values=c("black",
                             alpha("light green",0.3),
                             alpha("dark green",0.3),
                             alpha("coral",0.3),
                             alpha("dark red",0.3))) +
  theme_bw(base_size = 15)

# Remove the temperature for the treatments excluded from the analysis. These treatments may be a bit unrealistic and therefore
# difficult to be explained by the model
all_months_both_conference <- filter(all_months_both, !(Year %in% c(3, 8, 9, 12, 13, 14, 15, 19,
                                                                    23, 25, 26, 27, 28, 29, 32)))

# Create the hull again
hull_temps_both_conference <- all_months_both_conference %>%
  group_by(RCP_Time,month_name) %>%
  slice(chull(Tmin, Tmax))

# Plot the model validity domain
ggplot(hull_temps_both_conference[hull_temps_both_conference$Month %in% c(10,11,12,1,2,3),],
       aes(Tmin, Tmax, fill = factor(RCP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past_combined",
                             "RCP4.5_2050",
                             "RCP4.5_2085",
                             "RCP8.5_2050",
                             "RCP8.5_2085"),
                    values=c("black",
                             alpha("light green",0.3),
                             alpha("dark green",0.3),
                             alpha("coral",0.3),
                             alpha("dark red",0.3))) +
  theme_bw(base_size = 15)

# Save the plot to the folder
# ggsave("model_validity_domain.png", width = 16, height = 15, units = "cm", dpi = 600)

# Import the phenology data for the historic period 1958-2019
# Note that we are selecting only the Year and full bloom columns
historic_pheno_conference <- read.csv("data/Pheno_pear_conference_1958_2019.csv",fileEncoding="UTF-8-BOM")[c("Year", "Full_bloom")]

# Remove missing years
historic_pheno_conference <- historic_pheno_conference[which(!historic_pheno_conference$Full_bloom==""),]

# Add a column for the JDay
historic_pheno_conference$Full_bloom <- dormancyR::date_to_JDay(date = as.Date(historic_pheno_conference$Full_bloom, format = "%d.%m.%Y"),
                                                                format = "%Y-%m-%d")

# Rename the columns
colnames(historic_pheno_conference) <- c("Year", "pheno")

# Do the same for the weather data
past_weather <- read_tab("data/TMaxTMin1958-2019_patched.csv")

# Create hourly temps for compatibility with the experimental data set
past_weather <- stack_hourly_temps(weather = past_weather, latitude = 50.4)[["hourtemps"]]


# Load the experimental data
# Load the data from the folder
exp_weather <- read.csv("data/final_weather_data_S1_S2_pear_hourly.csv")

# Generate a new column (Year_2) to simulate the year and comply with the format of PhenoFlex functions
exp_weather["Year_2"] <- exp_weather$Treatment + exp_weather$Year + 3

# Since this experiment was conducted during two consecutive seasons, the next step will fix a small continuity issue
# generated during the season 2
exp_weather[exp_weather$Treatment >= 17, "Year_2"] <- exp_weather[exp_weather$Treatment >= 17, "Year_2"] - 1

# For further compatibility, I will now select the columns needed and will drop "Year" (the original one)
exp_weather <- exp_weather[c("YEARMODA", "Year_2", "Month", "Day", "Hour", "JDay", "Temp")]

# To replace the missing "Year" column, I will now change the name of the column
colnames(exp_weather)[which(colnames(exp_weather) == "Year_2")] <- "Year"


# Import the phenology data from the repository
exp_pheno <- read.csv("data/final_bio_data_S1_S2_pear.csv")

exp_pheno["Treatment"] <- exp_pheno$Treatment + 2019 + 3

# Remove conflictive treatments
exp_pheno <- exp_pheno[!(exp_pheno$Treatment %in% c(2025, 2030, 2031, 2034, 2035, 2036, 2037, 2041,
                                                    2045, 2047, 2048, 2049, 2050, 2051, 2054)),
                       c("Treatment", "pheno")]

# Rename the columns to match the names of the historic dataset
colnames(exp_pheno) <- c("Year", "pheno")


# Merge the historic and experimental phenology data
pheno_merged <- bind_rows(filter(historic_pheno_conference, Year != 1958),
                          exp_pheno)


# Merge the historic and experimental phenology data
weather_merged <- bind_rows(past_weather[, colnames(past_weather) %in% names(exp_weather)],
                            exp_weather)

## # Define the season used for calibration and validation in the PhenoFlex modelling approach
## calibration_seasons <- sort(sample(pheno_merged$Year, 50, replace = FALSE))
## validation_seasons <- sort(pheno_merged[!(pheno_merged$Year %in% calibration_seasons), "Year"])
## 
## # Define the list of seasons (weather data)
## weather_season_list <- genSeasonList(weather_merged, mrange = c(9, 5), years = calibration_seasons)

## # Set the initial parameters (wide ranges)
## #          yc,  zc,  s1, Tu,     E0,      E1,     A0,          A1,   Tf, Tc, Tb, slope
## lower <- c(20, 100, 0.1,  0, 3000.0,  9000.0, 6000.0,       5.e13,    0,  0,  0,  0.05)
## par   <- c(40, 190, 0.5, 25, 3372.8,  9900.3, 6319.5, 5.939917e13,    4, 36,  4,  1.60)
## upper <- c(80, 500, 1.0, 30, 4000.0, 10000.0, 7000.0,       6.e13,   10, 40, 10, 50.00)
## 
## # Run the fitter
## pheno_fit <- phenologyFitter(par.guess = par,
##                              modelfn = PhenoFlex_GDHwrapper,
##                              bloomJDays = pheno_merged[pheno_merged$Year %in%
##                                                          calibration_seasons, "pheno"],
##                              SeasonList = weather_season_list,
##                              lower = lower,
##                              upper = upper,
##                              control = list(smooth = FALSE,
##                                             verbose = FALSE,
##                                             maxit = 2000,
##                                             nb.stop.improvement = 20))
## 
## # Save the resulting parameters to folder (to avoid running the phenology fitter again)
## write.csv(pheno_fit$par, "data/PhenoFlex_hist_exp_pear.csv", row.names = FALSE)
## 
## # Save the results of the predicted phenology for the calibration seasons
## write.csv(data.frame(pheno_merged[pheno_merged$Year %in% calibration_seasons, ],
##                      "Predicted" = pheno_fit$pbloomJDays), "data/PhenoFlex_hist_exp_predicted_bloom_pear.csv",
##           row.names = FALSE)

# Read the parameters
params <- read.csv("data/PhenoFlex_hist_exp_pear.csv")[[1]]

# Generate a data set to collect the outputs of the fitting for the calibration data 
out_df <- read.csv("data/PhenoFlex_hist_exp_predicted_bloom_pear.csv")

# Compute the error (observed - predicted)
out_df[["Error"]] <- out_df$pheno - out_df$Predicted

# Define the season used for calibration and validation in the PhenoFlex modelling approach
calibration_seasons <- out_df$Year
validation_seasons <- sort(pheno_merged[!(pheno_merged$Year %in% calibration_seasons), "Year"])

# Define the list of seasons (weather data)
weather_season_list <- genSeasonList(weather_merged, mrange = c(9, 5), years = calibration_seasons)

# Compute some metrics for the calibration procedure
calibration_metrics <- data.frame("Metric" = c("RMSEP", "RPIQ"),
                                  "Calibration" = c(RMSEP(out_df$Predicted,
                                                          out_df$pheno, na.rm = TRUE),
                                                    RPIQ(out_df$Predicted,
                                                         out_df$pheno)))

kableExtra::kable(calibration_metrics)

# Plot the results to see the overall fitting
ggplot(out_df, aes(pheno, Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Observed") +
  theme_bw()

# Generate a validation data set with phenology data
valid_df <- pheno_merged[pheno_merged$Year %in% validation_seasons, ]

# Generate a list of seasons with weather data for the validation procedure
valid_season_list <- genSeasonList(weather_merged, mrange = c(9, 7), years = validation_seasons)

# Estimate the bloom dates with PhenoFlexGDHwrapper
for (i in 1 : nrow(valid_df)) {
  
  valid_df[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list[[i]],
                                                   params)
}

# Compute the error (observed - predicted)
valid_df[["Error"]] <- valid_df$pheno - valid_df$Predicted

# Compute some metrics for the validation dataset
validation_metrics <- data.frame(calibration_metrics,
                                 "Validation" = c(RMSEP(valid_df$Predicted,
                                                        valid_df$pheno, na.rm = TRUE),
                                                  RPIQ(valid_df$Predicted,
                                                       valid_df$pheno, na.rm = TRUE)))

kableExtra::kable(validation_metrics)



# Plot the validation and calibration results (highlight seasons with 10 or more days of error)
ggplot() +
  geom_point(data = out_df, aes(pheno, Predicted, color = "Calibration")) +
  geom_point(data = valid_df, aes(pheno, Predicted, color = "Validation")) + 
  ggrepel::geom_label_repel(aes(pheno, Predicted, label = Year),
                            data = filter(out_df, abs(Error) > 10), nudge_y = 2, nudge_x = 6) +
  ggrepel::geom_label_repel(aes(pheno, Predicted, label = Year),
                            data = filter(valid_df, abs(Error) > 10), nudge_y = -1, nudge_x = 9, force = 4) +
  scale_color_manual(values = c("cadetblue", "firebrick")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Observed",
       color = "Dataset") +
  theme_bw()


# Create a primer data frame to allocate the results for future bloom
future_bloom <- future_temps %>% group_by(RCP_Time, GCM, Year) %>% summarise(Pheno = NA)

# Define the scenarios to be used in the for loop
scenarios <- unique(future_temps$RCP_Time)

# Define the climate models to be used in the for loop
climate_models <- unique(future_temps$GCM)

# Implement the for loop
for (scenario in scenarios){
  for (climate_model in climate_models){
    
    # Subset a temporary data frame according to scenario and climate model
    temp_df <- filter(future_temps, RCP_Time == scenario & GCM == climate_model)
    
    # Generate hourly temperatures in the temporary data frame
    temp_df <- stack_hourly_temps(temp_df, latitude = 50.4)
    
    # Define the saeasons to be used for predicting the phenology
    temp_seasons_list <- genSeasonList(temp_df$hourtemps, mrange = c(9, 7), years = c(2002 : 2101))
    
    # Change the names of the list of seasons to be used as index in the next for loop
    names(temp_seasons_list) <- 2002 : 2101
    
    # Implement a for loop that runs over the list of seasons to estimate the bloom date using the parameters fitted by the
    # model
    for (i in 1 : length(temp_seasons_list)){
      
      # Add the bloom date to the primer data set
      future_bloom[future_bloom$RCP_Time == scenario & 
                     future_bloom$GCM == climate_model & 
                     future_bloom$Year == names(temp_seasons_list)[i], "Pheno"] <- PhenoFlex_GDHwrapper(temp_seasons_list[[i]],
                                                                                                        params)
    }
  }
}



# Bloom for past simulated scenarios ####
# Load the historic simulated scenarios
temps_past_scenarios <- load_temperature_scenarios("data/Weather/", "Bonn_historic")

# Create a single dataset for all the simulated years
temps_past_scenarios <- bind_rows(temps_past_scenarios, .id = "Scen_year")

# Make a primer dataset to allocate the results of the bloom projection
simulated_bloom <- temps_past_scenarios %>% group_by(Scen_year, Year) %>% summarise(Pheno = NA)

# Define the scenario years to be used in the loop
scen_years <- unique(simulated_bloom$Scen_year)

# Implement the for loop
for (scen_year in scen_years){
  
  # Subset a temporary data frame according to scenario and climate model
  temp_df <- filter(temps_past_scenarios, Scen_year == scen_year)
  
  # Generate hourly temperatures in the temporary data frame
  temp_df <- stack_hourly_temps(temp_df, latitude = 50.4)
  
  # Define the saeasons to be used for predicting the phenology
  temp_seasons_list <- genSeasonList(temp_df$hourtemps, mrange = c(9, 7), years = c(2002 : 2101))
  
  # Change the names of the list of seasons to be used as index in the next for loop
  names(temp_seasons_list) <- 2002 : 2101
  
  # Implement a for loop that runs over the list of seasons to estimate the bloom date using the parameters fitted by the
  # model
  for (i in 1 : length(temp_seasons_list)){
    
    # Add the bloom date to the primer data set
    simulated_bloom[simulated_bloom$Scen_year == scen_year & 
                      simulated_bloom$Year == names(temp_seasons_list)[i], "Pheno"] <- PhenoFlex_GDHwrapper(temp_seasons_list[[i]],
                                                                                                            params)
  }
}


# Plot all bloom prediction results
# Base plot to escape the issue with plotting the density and the violins in the same plot
base_plot <- ggplot(simulated_bloom, aes(Scen_year, Pheno)) +
  scale_y_continuous(limits = c(10, 160),
                     labels = function (x) format(dormancyR::JDay_to_date(x, 2001, na.rm = TRUE), "%b %d")) +
  scale_x_discrete(labels = c(1975, "", "", "", "", 2000, "", "", "", 2019)) +
  labs(x = "Scenario year",
       y = "Bloom date") +
  facet_grid(~ "Historic") +
  theme_bw() +
  theme(aspect.ratio = 4)

# Plot showing the violins for historic simulated scenarios
past_simulated_plot <- ggplot(simulated_bloom, aes(Scen_year, Pheno)) +
  geom_violin(size = 0.35, draw_quantiles = c(0.25, 0.75), alpha = 0.5) +
  stat_summary(fun = "median", geom = "point", shape = 4, size = 0.8) +
  scale_y_continuous(limits = c(10, 160),
                     labels = function (x) format(dormancyR::JDay_to_date(x, 2001, na.rm = TRUE), "%b %d")) +
  scale_x_discrete(labels = c(1975, "", "", "", "", 2000, "", "", "", 2019)) +
  labs(x = "",
       y = "") +
  facet_grid(~ "Historic") +
  theme_bw() +
  theme(plot.background = element_rect(fill = "transparent"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        aspect.ratio = 4)

# Make a density plot to show the distribution of the seasons used for calibration of the PhenoFlex modelling framework
# Create a dataframe with columns to be used in the plot
observed_bloom_calibration <- data.frame(out_df,
                                         Facet = "Past scenarios",
                                         Dataset = if_else(out_df$Year > 2010, "Experimental", "Historic"))

past_observed_plot <- ggplot(observed_bloom_calibration,
                             aes(y = pheno, fill = Dataset)) +
  geom_density(alpha = 0.75, size = 0.25) +
  scale_y_continuous(limits = c(10, 160),
                     labels = function (x) format(dormancyR::JDay_to_date(x, 2001, na.rm = TRUE), "%b %d")) +
  scale_x_continuous(breaks = 0, labels = "",
                     expand = expansion(mult = 0.01)) +
  labs(x = "",
       y = "") +
  facet_grid(~ "Historic") +
  theme_bw() +
  theme(plot.background = element_rect(fill = "transparent"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.5, 0.1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        aspect.ratio = 4)


# Create a violin plot to show future bloom dates
future_bloom_plot <- ggplot(na.omit(future_bloom), aes(GCM, Pheno, fill = GCM)) +
  geom_violin(size = 0.35, draw_quantiles = c(0.25, 0.75)) +
  stat_summary(fun = "median", geom = "point", shape = 4, size = 0.8) +
  scale_y_continuous(limits = c(10, 160)) +
  facet_grid(~ RCP_Time) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 7),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Use cowplot to merge all plot together. Not an easy and intuitive solution but works quite well if you have
# highly customized ideas. Be careful that the final plot (the one obtained after saving) meets your requirements (alignement, font size and so on).
# The plot window in RStudio is distorting the final resolution of the plot
ggdraw() + draw_plot(future_bloom_plot, x = 0.28, y = 0, width = 0.7, height = 1, scale = 1) + 
  draw_plot(base_plot, x = -0.36, y = 0.0775, width = 1, height = 0.9225, scale = 1) + 
  draw_plot(past_observed_plot, x = -0.36, y = 0.0775, width = 1, height = 0.9225, scale = 1) +
  draw_plot(past_simulated_plot, x = -0.36, y = 0.0775, width = 1, height = 0.9225, scale = 1)

# Save the plot to the folder (check this one and adjust the parameters in the cowplot call - try and error approach)
ggsave("pictures/future_pheno_pear.png", device = "png", width = 18.6, height = 14.7, units = "cm", dpi = 600)


# Frost risk part ####
# Add the "buffer" around the estimated bloom dates to compute the number of hours below 0 °C in this period. Hypothetically, this represents the
# period of bloom in all scenarios

# Define the buffer only once
bloom_buffer <- 5

# Mutate the future bloom dataset to add the beggining and end for bloom
future_bloom <- future_bloom %>% mutate(Beginning = Pheno - bloom_buffer,
                                        End = Pheno + bloom_buffer)

# Do the same with the observed bloom dataset (i.e. calibration)
observed_bloom_calibration <- observed_bloom_calibration %>% mutate(Beginning = pheno - bloom_buffer,
                                                                    End = pheno + bloom_buffer)

# Same with the past simulated data from historic scenarios
simulated_bloom <- simulated_bloom %>% mutate(Beginning = Pheno - bloom_buffer,
                                              End = Pheno + bloom_buffer)

for (i in 1 : nrow(observed_bloom_calibration)){
  
  # Extract the beginning and end date for observed bloom
  beg_bloom <- observed_bloom_calibration[i, "Beginning"]
  
  end_bloom <- observed_bloom_calibration[i, "End"]
  
  # Subset a temporary dataframe of weather records for the year of interest
  temp_df <- filter(weather_merged, Year == observed_bloom_calibration[i, "Year"])
  
  # Add the Julian date
  temp_df <- make_JDay(temp_df)
  
  # Filter only the period for bloom
  temp_df <- filter(temp_df, JDay %in% c(beg_bloom : end_bloom))
  
  # Calculate the number of hours below 0 °C
  frost_hours <- max(dormancyR::frost_risk(temp_df$Temp, threshold = 0))
  
  # Add the number of hours to the original dataframe
  observed_bloom_calibration[i, "Frost"] <- frost_hours
  
}


# Make a primer dataset to allocate the results of the frost projections
simulated_frost <- temps_past_scenarios %>% group_by(Scen_year, Year) %>% summarise(Frost = NA)

# Implement the loop over scenario years and simulated years 
for (scen_year in scen_years)
  for (year in c(2002 : 2101)){
    
    # Extract the beginning and end of the blooming period
    beg_bloom <- filter(na.omit(simulated_bloom), Scen_year == scen_year & Year == year)[["Beginning"]]
    
    beg_bloom <- trunc(beg_bloom)
    
    end_bloom <- filter(na.omit(simulated_bloom), Scen_year == scen_year, Year == year)[["End"]]
    
    end_bloom <- trunc(end_bloom)
    
    # Filter the weather data according to the relevant scenario year and simulated year
    temp_df <- filter(temps_past_scenarios, Scen_year == scen_year & Year == year)
    
    # Derive hourly temperatures based on the latitude of Campus Klein-Altendorf
    temp_df <- stack_hourly_temps(temp_df, latitude = 50.4)[["hourtemps"]]
    
    # Filter the relevant period when bloom is likely to occurs
    temp_df <- filter(temp_df, JDay %in% c(beg_bloom : end_bloom))
    
    # Compute the number of frost hours
    frost_hours <- max(dormancyR::frost_risk(temp_df$Temp, threshold = 0))
    
    # Add the number of frost events to the primer dataset
    simulated_frost[simulated_frost$Scen_year == scen_year & 
                      simulated_frost$Year == year, "Frost"] <- frost_hours

}

# Remove NA cells since the first year has no bloom
simulated_frost <- na.omit(simulated_frost)

# Create a primer data frame to allocate the results for future bloom
future_frost <- future_temps %>% group_by(RCP_Time, GCM, Year) %>% summarise(Frost = NA)

# Implement the for loop
for (scenario in scenarios)
  for (climate_model in climate_models)
    for (year in c(2002 : 2101)){
      
      beg_bloom <- filter(na.omit(future_bloom), RCP_Time == scenario & GCM == climate_model & Year == year)[["Beginning"]]
      
      beg_bloom <- trunc(beg_bloom)
      
      end_bloom <- filter(na.omit(future_bloom), RCP_Time == scenario & GCM == climate_model & Year == year)[["End"]]
      
      end_bloom <- trunc(end_bloom)
      
      # Derive hourly temperatures based on the latitude of Campus Klein-Altendorf
      temp_df <- filter(future_temps, RCP_Time == scenario & GCM == climate_model & Year == year)
      
      temp_df <- stack_hourly_temps(temp_df, latitude = 50.4)[["hourtemps"]]
      
      temp_df <- filter(temp_df, JDay %in% c(beg_bloom : end_bloom))
      
      frost_hours <- max(dormancyR::frost_risk(temp_df$Temp, threshold = 0))
      
      future_frost[future_frost$RCP_Time == scenario & 
                     future_frost$GCM == climate_model &
                     future_frost$Year == year, "Frost"] <- frost_hours
  
    
    }

# Remove NA cells since the first year has no bloom
future_frost <- na.omit(future_frost)

# Plots
past_frost_plot <- ggplot(simulated_frost, aes(Scen_year, Frost)) +
  geom_boxplot(fill = "cadetblue", outlier.size = 0.8, outlier.alpha = 0.5, outlier.shape = 1) +
  #geom_point(data = observed_bloom_calibration, aes(as.character(Year), Frost)) +
  scale_y_continuous(limits = c(0, 90), expand = expansion(mult = 0)) +
  labs(x = "Year",
       y = "Spring frost (hours <0 °C)") +
  facet_grid(~ "Historic") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, angle = 60, hjust = 1))

future_frost_plot <- ggplot(future_frost, aes(GCM, Frost, fill = GCM)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.5, outlier.shape = 1) +
  scale_y_continuous(limits = c(0, 90), expand = expansion(mult = 0)) +
  facet_grid(~ RCP_Time) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 7.5),
        legend.key.size = unit(0.3, "cm"))

past_frost_plot + future_frost_plot + plot_layout(widths = c(0.25, 1))

