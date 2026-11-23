library(ncdf4)
library(ggplot2)
library(dplyr)

#Load .nc file
array <- nc_open("array.nc")
array

#Extract coordinate and time
lon <- ncvar_get(array, "longitude")
lat <- ncvar_get(array, "latitude")
time <- ncvar_get(array, "time")

#Convert time to date
time_dates <- as.Date(time, origin = "1950-01-01")
time_dates

#Extract rainfall data
rr_raw <- ncvar_get(array, "rr")

# Get scaling from attributes
scale <- ncatt_get(array, "rr", "scale_factor")$value
offset <- ncatt_get(array, "rr", "add_offset")$value
fill <- ncatt_get(array, "rr", "_FillValue")$value

#Prepare data to plot
rr <- rr_raw
rr[rr_raw == fill] <- NA
rr <- rr * scale + offset
rr_annual <- apply(rr, c(1, 2), sum)

#Convert to data frame
lon_lat <- expand.grid(lon = lon, lat = lat)
df <- cbind(lon_lat, value = as.vector(rr_annual))
max(df$value, na.rm = TRUE)

#Plot spatial map
p2 <- ggplot(df, aes(x = lon, y = lat, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_gradientn(
    colours = c(
      rgb(0, 0, 128, maxColorValue = 255),     
      rgb(0, 0, 255, maxColorValue = 255),     
      rgb(0, 255, 255, maxColorValue = 255),   
      rgb(255, 255, 0, maxColorValue = 255),   
      rgb(255, 165, 0, maxColorValue = 255),   
      rgb(255, 0, 0, maxColorValue = 255)
      ),
    name = "[mm]",
    na.value = "white"
    ) +
  labs(
    title = "Annual Rainfall of 2020",
    x = "lon",
    y = "lat"
  ) +
  theme_classic()+
  theme(legend.position = "right")

#Save plot as pdf
ggsave("./spatial_map.pdf", width =  12, height = 8, p2)
