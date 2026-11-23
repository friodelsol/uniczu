library(ncdf4)
library(ggplot2)
library(dplyr)

#Load .nc files
wettest <- nc_open("rr_wettest_UK.nc")
driest <- nc_open("rr_driest_UK.nc")

#Extract coordinate and time
lon <- ncvar_get(wettest, "longitude")
lat <- ncvar_get(wettest, "latitude")

#Extract rainfall data
rr_wettest <- ncvar_get(wettest, "rr")
rr_driest <- ncvar_get(driest, "rr")

#Get scaling from attributes
scale <- ncatt_get(wettest, "rr", "scale_factor")$value
offset <- ncatt_get(wettest, "rr", "add_offset")$value
fill <- ncatt_get(wettest, "rr", "_FillValue")$value

#Prepare data to plot
rr_dry <- rr_driest
rr_dry[rr_driest == fill] <- NA
rr_wet <- rr_wettest
rr_wet[rr_wettest == fill] <- NA

#Convert to data frame
lon_lat <- expand.grid(lon = lon, lat = lat)
df_driest <- cbind(lon_lat, value = as.vector(rr_dry))
df_wettest <- cbind(lon_lat, value = as.vector(rr_wet))
max(df_wettest$value, na.rm = TRUE)

#Plot spatial map
p1 <- ggplot(df_driest, aes(x = lon, y = lat, fill = value)) +
  geom_raster(interpolate = FALSE) +
  coord_equal() +
  scale_fill_gradientn(
    colours = c(
      rgb(255, 0, 0, maxColorValue = 255),
      rgb(255, 165, 0, maxColorValue = 255),   
      rgb(255, 255, 0, maxColorValue = 255),     
      rgb(0, 255, 255, maxColorValue = 255),     
      rgb(0, 0, 255, maxColorValue = 255), 
      rgb(0, 0, 128, maxColorValue = 255)
    ),
    name = "[mm]",
    na.value = "white"
  ) +
  labs(
    title = "Spatial Map of Driest year(1933) in UK",
    x = "lon",
    y = "lat"
  ) +
  theme_classic()+
  theme(legend.position = "right")

p2 <- ggplot(df_wettest, aes(x = lon, y = lat, fill = value)) +
  geom_raster(interpolate = FALSE) +
  coord_equal() +
  scale_fill_gradientn(
    colours = c(
      rgb(255, 0, 0, maxColorValue = 255),
      rgb(255, 165, 0, maxColorValue = 255),   
      rgb(255, 255, 0, maxColorValue = 255),     
      rgb(0, 255, 255, maxColorValue = 255),     
      rgb(0, 0, 255, maxColorValue = 255), 
      rgb(0, 0, 128, maxColorValue = 255)
    ),
    name = "[mm]",
    na.value = "white"
  ) +
  labs(
    title = "Spatial Map of Wettest year(2000) in UK",
    x = "lon",
    y = "lat"
  ) +
  theme_classic()+
  theme(legend.position = "right")

#Save plot as pdf
ggsave("./Driest_year.pdf", width =  12, height = 8, p1)
ggsave("./Wettest_year.pdf", width =  12, height = 8, p2)
