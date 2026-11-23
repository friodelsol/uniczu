library(data.table)
library(dplyr)
library(ggplot2)

#read data into data table
dt_time <- fread("file.txt")
dt_time
str(dt_time)

#Convert day, month and year to date format
dt_time <- dt_time %>%
  mutate(
    Date = as.Date(paste(Year, Mon, Day, sep = '-'))
  )

#Prepare data
dt_time <- dt_time[,c(7,5,6)]
dt_time_long <- melt(data = dt_time, id.vars = 'Date',
                     variable.name = 'variable',
                     value.name = 'Q_value')

#plot time series
p1 <- ggplot(data = dt_time_long) +
  geom_line(aes(x = Date, y = Q_value, colour = variable))+
  labs(title = "Observed and simulated discharge")+
  theme_bw()

#Save plot as pdf
ggsave("./timeseries.pdf", width =  12, height = 6, p1)
