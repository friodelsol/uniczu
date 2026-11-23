library(ncdf4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Kendall)
library(trend)

#Load netcdf files
pre <- nc_open("pre_1980_2020_monthly_fieldmean.nc")
tavg  <- nc_open("tavg_1980_2020_monthly_fieldmean.nc")

#Extract time
time <- ncvar_get(pre, "time")

#Convert time(hours) to date
time_dates <- as.Date(time / 24, origin = "1900-01-01")
time_dates

#Extract pre and tavg values
pr <- ncvar_get(pre, "pre")
tavg <- ncvar_get(tavg, "tavg")

#Create data frame
df <- data.frame(
  date = time_dates,
  precip = pr,
  temp = tavg
)

#Define seasons
df <- df %>%
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(10,11,12,1,2,3) ~ "summer",
      month %in% c(4,5,6,7,8,9) ~ "winter"
    )
  )

#Calculate seasonal average
seasonal <- df %>%
  group_by(year, season) %>%
  summarise(
    precip = mean(precip, na.rm=TRUE),
    temp = mean(temp, na.rm=TRUE)
  )

tail(seasonal)

#Split the dataset according to seasons
summer <- seasonal %>% filter(season=="summer")
winter <- seasonal %>% filter(season=="winter")

#Mann-Kendall test
mk_s_pr <- MannKendall(summer$precip)
mk_s_temp <- MannKendall(summer$temp)

mk_w_pr <- MannKendall(winter$precip)
mk_w_temp <- MannKendall(winter$temp)

#Preparation for plots
mk_tests <- list(
  summer_precip = mk_s_pr,
  summer_temp   = mk_s_temp,
  winter_precip = mk_w_pr,
  winter_temp   = mk_w_temp
)

mk_labels <- list()

for(i in names(mk_tests)) {
  
  tau <- round(mk_tests[[i]]$tau, 3)
  pval <- round(mk_tests[[i]]$sl, 5)
  
  mk_labels[[i]] <- paste0(
    "tau = ", tau,
    ", 2-sided pvalue = ", pval
  )
}

#Plot trend
s_pr <- ggplot(summer, aes(year, precip)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", se=FALSE) +
  labs(
    x ="Year",
    y ="PRE (mm/day)",
    title ="LON = -70.5 -69.75, LAT = -34.5 -33.0; summer",
  )+
  annotate(
    "text",
    x=min(summer$year),
    y=max(summer$precip),
    label=mk_labels$summer_precip,
    hjust=0
  )+
  theme_bw()

s_temp <- ggplot(summer, aes(year, temp)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", se=FALSE) +
  labs(
    x ="Year",
    y ="T (deg C)",
    title ="LON = -70.5 -69.75, LAT = -34.5 -33.0; summer",
  )+
  annotate(
    "text",
    x=min(summer$year),
    y=max(summer$temp),
    label=mk_labels$summer_temp,
    hjust=0
  )+
  theme_bw()

w_pr <- ggplot(winter, aes(year, precip)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", se=FALSE) +
  labs(
    x ="Year",
    y ="PRE (mm/day)",
    title ="LON = -70.5 -69.75, LAT = -34.5 -33.0; winter",
  )+
  annotate(
    "text",
    x=min(winter$year),
    y=max(winter$precip),
    label=mk_labels$winter_precip,
    hjust=0
  )+
  theme_bw()

w_temp <- ggplot(winter, aes(year, temp)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", se=FALSE) +
  labs(
    x ="Year",
    y ="T (deg C)",
    title ="LON = -70.5 -69.75, LAT = -34.5 -33.0; winter",
  )+
  annotate(
    "text",
    x=min(winter$year),
    y=max(winter$temp),
    label=mk_labels$winter_temp,
    hjust=0
  )+
  theme_bw()

#Save the trend in pdf
save = c(s_pr, s_temp, w_pr, w_temp)
ggsave("./seasonal_trend.pdf", width =  12, height = 6, save)
