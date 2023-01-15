library(chillR)
library(tidyverse)

# Load the data from the folder
data <- read.csv("data/final_weather_data_S1_S2_apple_hourly.csv")

# Generate a new column (Year_2) to simulate the year and comply with the format of PhenoFlex functions
data["Year_2"] <- data$Treatment + data$Year 

# Since this experiment was conducted during two consecutive seasons, the next step will fix a small continuity issue
# generated during the season 2
data[data$Treatment >= 34, "Year_2"] <- data[data$Treatment >= 34, "Year_2"] - 1

# For further compatibility, I will now select the columns needed and will drop "Year" (the original one)
data <- data[c("YEARMODA", "Year_2", "Month", "Day", "Hour", "JDay", "Temp")]

# To replace the missing "Year" column, I will now change the name of the column
colnames(data)[which(colnames(data) == "Year_2")] <- "Year"


# Import the phenology data from the repository
pheno <- read.csv("data/final_bio_data_S1_S2_apple.csv")

# Remove troubling treatments
pheno <- pheno[!(pheno$Treatment %in% c(36, 3, 23, 24, 17, 18, 61)), c("Treatment", "pheno")]

pheno["Treatment"] <- pheno$Treatment + 2019

colnames(pheno) <- c("Year", "pheno")


head(data)

head(pheno)

# Now we define two subsets: one with all season, and one that excludes seasons with marginal chill

pheno_marginal <- pheno
pheno_normal <- pheno[!(pheno$Year %in% c(2032, 2061, 2065, 2077, 2081)), ]

# Define a vector of calibration and validation seasons. Marginal includes the marginal seasons
calibration_seasons <- sort(sample(pheno_normal$Year, 40, replace = FALSE))
calibration_seasons_marginal <- sort(c(sample(calibration_seasons, 35, replace = FALSE),
                                       pheno_marginal$Year[which(!(pheno_marginal$Year %in% pheno_normal$Year))]))
calibration_seasons_normal <- calibration_seasons

# Common validation seasons
validation_seasons <- sort(pheno_normal[!(pheno_normal$Year %in% calibration_seasons), "Year"])

# Define the list of seasons (weather data)
season_list_marginal <- genSeasonList(data, mrange = c(9, 7), years = calibration_seasons_marginal)
season_list_normal <- genSeasonList(data, mrange = c(9, 7), years = calibration_seasons_normal)

# Set the initial parameters (wide ranges)
#          yc,  zc,  s1, Tu,     E0,      E1,     A0,          A1,   Tf, Tc, Tb, slope
lower <- c(20, 100, 0.1,  0, 3000.0,  9000.0, 6000.0,       5.e13,    0,  0,  0,  0.05)
par   <- c(40, 190, 0.5, 25, 3372.8,  9900.3, 6319.5, 5.939917e13,    4, 36,  4,  1.60)
upper <- c(80, 500, 1.0, 30, 4000.0, 10000.0, 7000.0,       6.e13,   10, 40, 10, 50.00)

# Run the fitter
pheno_fit_marginal <- phenologyFitter(par.guess = par,
                                      modelfn = PhenoFlex_GDHwrapper,
                                      bloomJDays = pheno_marginal[pheno_marginal$Year %in%
                                                                    calibration_seasons_marginal, "pheno"],
                                      SeasonList = season_list_marginal,
                                      lower = lower,
                                      upper = upper,
                                      control = list(smooth = FALSE,
                                                     verbose = FALSE,
                                                     maxit = 100,
                                                     nb.stop.improvement = 10))

# Same for version 2
pheno_fit_normal <- phenologyFitter(par.guess = par,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_normal[pheno_normal$Year %in%
                                                                calibration_seasons_normal, "pheno"],
                                    SeasonList = season_list_normal,
                                    lower = lower,
                                    upper = upper,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 100,
                                                   nb.stop.improvement = 10))

write.csv(pheno_fit_marginal$par, "data/PhenoFlex_marginal_params.csv", row.names = FALSE)
write.csv(pheno_fit_normal$par, "data/PhenoFlex_normal_params.csv", row.names = FALSE)

write.csv(data.frame(pheno_marginal[pheno_marginal$Year %in% calibration_seasons_marginal, ],
          "Predicted" = pheno_fit_marginal$pbloomJDays), "data/PhenoFlex_marginal_predicted_bloom.csv",
          row.names = FALSE)
write.csv(data.frame(pheno_normal[pheno_normal$Year %in% calibration_seasons_normal, ],
          "Predicted" = pheno_fit_normal$pbloomJDays), "data/PhenoFlex_normal_predicted_bloom.csv",
          row.names = FALSE)


# Read the parameters
params_marginal <- read.csv("data/PhenoFlex_marginal_params.csv")[[1]]
params_normal <- read.csv("data/PhenoFlex_normal_params.csv")[[1]]

# Generate a data set to collect the outputs of the fitting for the calibration data 
out_df_marginal <- read.csv("data/PhenoFlex_marginal_predicted_bloom.csv")
out_df_normal <- read.csv("data/PhenoFlex_normal_predicted_bloom.csv")

# Compute the error (observed - predicted)
out_df_marginal[["Error"]] <- out_df_marginal$pheno - out_df_marginal$Predicted
out_df_normal[["Error"]] <- out_df_normal$pheno - out_df_normal$Predicted

calibration_metrics <- data.frame("Metric" = c("RMSEP", "RPIQ"),
                                 "PhenoFlex_marginal" = c(RMSEP(out_df_marginal$Predicted,
                                                                out_df_marginal$pheno, na.rm = TRUE),
                                                          RPIQ(out_df_marginal$Predicted,
                                                               out_df_marginal$pheno)),
                                 "PhenoFlex_normal" = c(RMSEP(out_df_normal$Predicted,
                                                              out_df_normal$pheno, na.rm = TRUE),
                                                        RPIQ(out_df_normal$Predicted,
                                                             out_df_normal$pheno)))

calibration_metrics

out_df_all <- bind_rows("PhenoFlex marginal" = out_df_marginal,
                        "PhenoFlex normal" = out_df_normal,
                        .id = "PhenoFlex version")

# Plot the observed versus predicted values
ggplot(out_df_all, aes(pheno, Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Observed") +
  facet_grid(~ `PhenoFlex version`) +
  theme_bw()


# Generate a validation data set with phenology data
valid_df_marginal <- pheno_marginal[pheno_marginal$Year %in% validation_seasons, ]
valid_df_normal <- pheno_normal[pheno_normal$Year %in% validation_seasons, ]

# Generate a list of seasons with weather data for the validation procedure
valid_season_list <- genSeasonList(data, mrange = c(9, 7), years = validation_seasons)

# Estimate the bloom dates with PhenoFlexGDHwrapper
for (i in 1 : nrow(valid_df_marginal)) {
  
  valid_df_marginal[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list[[i]],
                                                            params_marginal)
}

# The same for the second version
for (i in 1 : nrow(valid_df_normal)) {
  
  valid_df_normal[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list[[i]],
                                                          params_normal)
}

# Compute the error (observed - predicted)
valid_df_marginal[["Error"]] <- valid_df_marginal$pheno - valid_df_marginal$Predicted
valid_df_normal[["Error"]] <- valid_df_normal$pheno - valid_df_normal$Predicted

validation_metrics <- data.frame("Metric" = c("RMSEP", "RPIQ"),
                                 "PhenoFlex_marginal" = c(RMSEP(valid_df_marginal$Predicted,
                                                                valid_df_marginal$pheno, na.rm = TRUE),
                                                          RPIQ(valid_df_marginal$Predicted,
                                                               valid_df_marginal$pheno)),
                                 "PhenoFlex_normal" = c(RMSEP(valid_df_normal$Predicted,
                                                              valid_df_normal$pheno, na.rm = TRUE),
                                                        RPIQ(valid_df_normal$Predicted,
                                                             valid_df_normal$pheno)))

validation_metrics


# Create a unique data set
valid_df_all <- bind_rows("PhenoFlex marginal" = valid_df_marginal,
                          "PhenoFlex normal" = valid_df_normal,
                          .id = "PhenoFlex version")

# Plot the calibrated and validated 
ggplot(out_df_all, aes(pheno, Predicted, color = "Calibration")) +
  geom_point() +
  geom_point(data = valid_df_all, aes(pheno, Predicted, color = "Validation")) + 
  scale_color_manual(values = c("cadetblue", "firebrick")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Observed",
       color = "Dataset") +
  facet_grid(~ `PhenoFlex version`) +
  theme_bw()

apply_const_temp <- function(temp, A0, A1, E0, E1, Tf, slope, portions=1200, deg_celsius=TRUE){
  temp_vector <- rep(temp, times=portions)
  res <- chillR::DynModel_driver(temp=temp_vector,
                         A0=A0, A1=A1,
                         E0=E0, E1=E1,
                         Tf=Tf,
                         slope=slope,
                         deg_celsius=deg_celsius)
  return(invisible(res$y[length(res$y)]))
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

GDH_response<-function(par, T)
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

# Create a data set with theoretical temperatures and heat and chill responses
temp_response_marginal <- data.frame(Temp = seq(-5, 60, 0.1),
                                     Chill_res = gen_bell(params_marginal, temp_values = seq(-5, 60, 0.1)),
                                     Heat_res = GDH_response(params_marginal, seq(-5, 60, 0.1)),
                                     Version = "PhenoFlex_marginal")

temp_response_normal <- data.frame(Temp = seq(-5, 60, 0.1),
                                   Chill_res = gen_bell(params_normal, temp_values = seq(-5, 60, 0.1)),
                                   Heat_res = GDH_response(params_normal, seq(-5, 60, 0.1)),
                                   Version = "PhenoFlex_normal")


# Generate a single data set
temp_response <- bind_rows(temp_response_marginal, temp_response_normal)

# Plotting
ggplot(temp_response, aes(Temp)) +
  geom_line(aes(y = Chill_res, color = "Chill"),lwd=1.3) +
  geom_line(aes(y = Heat_res * 25, color = "Heat"),lwd=1.3) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.01)),
                     sec.axis = sec_axis(~ . / 25, name = "Arbitrary heat units")) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_color_manual(values = c("blue4", "firebrick")) +
  labs(x = "Temperature (°C)",
       y = "Arbitrary chill units",
       color = NULL) +
  facet_grid(Version ~ .) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85))
