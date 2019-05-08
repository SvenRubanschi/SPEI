library(raster)
library(sp)
library(dplyr)


# Coordination ------------------------------------------------------------

coords <- readRDS("data/dummy.rds") %>%
  select(-date_OBS)

# Annual Mean Temp --------------------------------------------------------

fl <- list.files('tavg','*.tif',full.names=TRUE)
d <- list()
for (i in 1:length(fl)) {
  d[[i]] <- raster(fl[i])
}

for (i in 1:length(fl)) {
  points <- SpatialPoints(coords[c(2,3)], proj4string = d[[i]]@crs)
  values <- extract(d[[i]],points)
  if(i == 1) {
    df_tavg <- cbind.data.frame(coords,values)
  } else {
    df_tavg <- cbind(df_tavg,values)
  }
}

tavg_data <- data.frame(plotcode = df_tavg[,1], mean_tavg = rowMeans(df_tavg[,4:15]))


# Precipitation ----------------------------------------------------------

fl <- list.files('prec','*.tif',full.names=TRUE)
d <- list()
for (i in 1:length(fl)) {
  d[[i]] <- raster(fl[i])
}

for (i in 1:length(fl)) {
  points <- SpatialPoints(coords[c(2,3)], proj4string = d[[i]]@crs)
  values <- extract(d[[i]],points)
  if(i == 1) {
    df_prec <- cbind.data.frame(coords,values)
  } else {
    df_prec <- cbind(df_prec,values)
  }
}

prec_data <- data.frame(plotcode = df_prec[,1], mean_prec = rowMeans(df_prec[,4:15]))

# Solar radiation ---------------------------------------------------------

fl <- list.files('srad','*.tif',full.names=TRUE)
d <- list()
for (i in 1:length(fl)) {
  d[[i]] <- raster(fl[i])
}

for (i in 1:length(fl)) {
  points <- SpatialPoints(coords[c(2,3)], proj4string = d[[i]]@crs)
  values <- extract(d[[i]],points)
  if(i == 1) {
    df_srad <- cbind.data.frame(coords,values)
  } else {
    df_srad <- cbind(df_srad,values)
  }
}

srad_data <- data.frame(plotcode = df_srad[,1], mean_srad = rowMeans(df_srad[,4:15]))


# Water vapor pressure ----------------------------------------------------

fl <- list.files('vapr','*.tif',full.names=TRUE)
d <- list()
for (i in 1:length(fl)) {
  d[[i]] <- raster(fl[i])
}

for (i in 1:length(fl)) {
  points <- SpatialPoints(coords[c(2,3)], proj4string = d[[i]]@crs)
  values <- extract(d[[i]],points)
  if(i == 1) {
    df_vapr <- cbind.data.frame(coords, values)
  } else {
    df_vapr <- cbind(df_vapr, values)
  }
}

vapr_data <- data.frame(plotcode = df_vapr[,1], mean_vapr = rowMeans(df_vapr[,4:15]))

# Elevation ---------------------------------------------------------------

# long 30:60; lat 30:60
r1 <- getData("worldclim", var = "alt", res = 0.5, lon = 30, lat = 60)
r2 <- getData("worldclim", var = "alt", res = 0.5, lon = 0, lat = 60)
r3 <- getData("worldclim", var = "alt", res = 0.5, lon = -30, lat = 60)
r4 <- getData("worldclim", var = "alt", res = 0.5, lon = 30, lat = 90)
r5 <- getData("worldclim", var = "alt", res = 0.5, lon = 0, lat = 90)
r6 <- getData("worldclim", var = "alt", res = 0.5, lon = -30, lat = 90)

x <- list(r1, r2, r3, r4, r5, r6)
x$filename <- 'alt.tif'
x$overwrite <- TRUE
m <- do.call(merge, x)

points <- SpatialPoints(coords[c(2,3)], proj4string = m@crs)

values <- extract(m, points)

elevation <- data.frame(coords, elevation = values)