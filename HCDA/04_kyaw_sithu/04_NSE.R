library(dplyr)
library(ggplot2)
library(lubridate)

#load the data
daily <- read.table("outputs_5710001/daily_discharge.out", header = TRUE)
print(daily)

#prepare data for ploting
dates <- daily %>%
  mutate(
    date = as.Date(paste(Year, Mon, Day, sep = '-'))
  )
daily_Q <- dates[,c(7,5,6)]
min(daily_Q$Qobs_0005710001, na.rm = TRUE)
min(daily_Q$Qsim_0005710001, na.rm = TRUE)

daily_Q$Qobs_0005710001[daily_Q$Qobs_0005710001 == -9999] <- NA

#prepare to find NSE daily
dQobs <- daily_Q$Qobs_0005710001
dQsim <- daily_Q$Qsim_0005710001

NSE_day <-  1 - sum((dQsim - dQobs)^2, na.rm = TRUE) /
  sum((dQobs - mean(dQobs, na.rm = TRUE))^2, na.rm = TRUE)
print(NSE_day)

NSE_D_label <- paste0("NSE_Day = ", round(NSE_day, 5))

#plot the timeseries with daily NSE value
nse_day <- ggplot(daily_Q, aes(x = date)) +
  geom_line(aes(y = Qobs_0005710001), color = "red", size = 0.5) +
  geom_line(aes(y = Qsim_0005710001), color = "blue", size = 0.5) +
  labs(title = "Observed vs Simulated Discharge",
       y = "Q [m³/s]",
       x = "Date") +
  annotate(
    "text",
    x=min(daily_Q$date),
    y=1200,
    label=NSE_D_label,
    hjust=0)+
  theme_bw()

#prepare data for monthly discharge
monthly_Q <- daily_Q %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(Qsim = mean(Qsim_0005710001),
            Qobs = mean(Qobs_0005710001)) %>%
  mutate(date = as.Date(sprintf("%d-%02d-15", year, month)))
min(monthly_Q$Qsim, na.rm = TRUE)
min(monthly_Q$Qobs, na.rm = TRUE)

#prepare for monthly NSE
mQobs <- monthly_Q$Qobs
mQsim <- monthly_Q$Qsim

NSE_mon <-  1 - sum((mQsim - mQobs)^2, na.rm = TRUE) /
  sum((mQobs - mean(mQobs, na.rm = TRUE))^2, na.rm = TRUE)
print(NSE_mon)

NSE_M_label <- paste0("NSE_Mon = ", round(NSE_mon, 5))

#plot the timeseries with monthly NSE value
nse_mon <- ggplot(monthly_Q, aes(x = date)) +
  geom_line(aes(y = Qobs), color = "red", size = 0.5) +
  geom_line(aes(y = Qsim), color = "blue", size = 0.5) +
  labs(title = "Monthly Discharge (Observed vs Simulated)",
       y = "Q [m³/s]",
       x = "Date") +
  annotate(
    "text",
    x=min(monthly_Q$date),
    y=700,
    label=NSE_M_label,
    hjust=0) +
  theme_bw()

#save the plots
save = c(nse_day, nse_mon)
ggsave("./NSE.pdf", width =  12, height = 6, save)
