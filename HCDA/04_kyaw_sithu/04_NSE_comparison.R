library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(trend)
library(Kendall)

#Create function for reading data
read_daily_out <- function(path) {
  df <- read.table(path, header = TRUE)
  
  df <- df %>%
    mutate(
      date = as.Date(paste(Year, Mon, Day, sep = '-'))
    )
  
  names(df) <- gsub("\\.", "_", names(df))
  
  sim_col <- grep("Qsim", names(df), ignore.case = TRUE, value = TRUE)
  obs_col <- grep("Qobs", names(df), ignore.case = TRUE, value = TRUE)
  
  df <- df %>%
    select(date, Qsim = all_of(sim_col), Qobs = all_of(obs_col))
  
  df$Qobs[df$Qobs == -9999] <- NA
  df$Qsim[df$Qsim == -9999] <- NA
  return(df)
}

#load data
daily_default <- read_daily_out("outputs_5710001/daily_discharge.out")
daily_calib   <- read_daily_out("outputs_5710001_cali/daily_discharge.out")

head(daily_default)
head(daily_calib)

#create function for NSE
NSE <- function(Qsim, Qobs) {
  1 - sum((Qsim - Qobs)^2, na.rm = TRUE) /
  sum((Qobs - mean(Qobs, na.rm = TRUE))^2, na.rm = TRUE)
}

NSE_default <- NSE(daily_default$Qsim, daily_default$Qobs)
NSE_calib   <- NSE(daily_calib$Qsim, daily_calib$Qobs)

cat("NSE (Default):     ", NSE_default, "\n")
cat("NSE (Calibrated): ", NSE_calib, "\n")

#create NSE table for comparison
NSE_table <- data.frame(
  Model = c("Default", "Calibrated"),
  NSE   = c(NSE_default, NSE_calib)
)

print(NSE_table)

improvement <- NSE_calib - NSE_default
cat("NSE Improvement:", improvement, "\n")
#prepare for plotting
NSE_label <- paste0("NSE_Default = ", round(NSE_table[1,2], 5), "   ",
                    "NSE_Calibrated = ", round(NSE_table[2,2], 5), "   ",
                    "NSE_Improved = ", round(improvement, 5))

daily_default$type <- "Default"
daily_calib$type   <- "Calibrated"

combined <- rbind(daily_default, daily_calib)

CP <- ggplot(combined, aes(x = date, y = Qsim)) +
  geom_line(color = "blue") +
  facet_wrap(~ type, ncol = 1) +
  labs(
    title = "Daily Simulated Discharge: Default vs Calibrated",
    x = "Date",
    y = "Q [m³/s]"
  ) +
  annotate(
    "text",
    x=min(daily_default$date),
    y=750,
    label=NSE_label,
    hjust=0)+
  theme_bw()

daily_calib <- daily_calib %>%
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(10,11,12,1,2,3) ~ "summer",
      month %in% c(4,5,6,7,8,9) ~ "winter"
    )
  )

seasonal_Q <- daily_calib %>%
  group_by(year, season) %>%
  summarise(
    Qobs = mean(Qobs, na.rm = TRUE),
    Qsim = mean(Qsim, na.rm = TRUE))

summer <- seasonal_Q %>% filter(season=="summer")
winter <- seasonal_Q %>% filter(season=="winter")

mk_s_obs <- MannKendall(summer$Qobs)
mk_s_sim <- MannKendall(summer$Qsim)

mk_w_obs <- MannKendall(winter$Qobs)
mk_w_sim <- MannKendall(winter$Qsim)

mk_tests <- list(
  s_Qobs = mk_s_obs,
  s_Qsim   = mk_s_sim,
  w_Qobs = mk_w_obs,
  w_Qsim   = mk_w_sim
)
print(mk_tests)

s_long <- summer %>%
  pivot_longer(cols = c(Qsim, Qobs),
               names_to = "variable",
               values_to = "value")

w_long <- winter %>%
  pivot_longer(cols = c(Qsim, Qobs),
               names_to = "variable",
               values_to = "value")

SP <- ggplot(s_long, aes(x = year, y = value, color = variable)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  labs(
    title = "Observed vs Simulated Discharge in Summer (with Linear Trend)",
    x = "Date",
    y = "Q [m³/s]",
    color = ""
  ) +
  scale_color_manual(values = c("Qsim" = "blue", "Qobs" = "red")) +
  theme_bw()

WP <- ggplot(w_long, aes(x = year, y = value, color = variable)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  labs(
    title = "Observed vs Simulated Discharge in Winter (with Linear Trend)",
    x = "Date",
    y = "Q [m³/s]",
    color = ""
  ) +
  scale_color_manual(values = c("Qsim" = "blue", "Qobs" = "red")) +
  theme_bw()
save = c(CP, SP, WP)
ggsave("./seasonal_trend.pdf", width =  12, height = 6, save)
