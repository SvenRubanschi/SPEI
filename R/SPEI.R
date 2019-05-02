# This script will add the SPEI for 1 , 6, 12 and 24 months

library(dplyr)
library(tidyr)
library(ncdf4)

# load dummy
df <- readRDS("data/dummy.rds")

# create the SPEI grid
longST <- seq(-180, 179.5, by = 0.5) # start value
longGR <- seq(-179.75, 179.75, by = 0.5) # group value
longRang <- data.frame(longST, longGR)

latST <- seq(-90, 89.5, by = 0.5) # start value
latGR <- seq(-89.75, 89.75, by = 0.5) # group value
latRang <- data.frame(latST, latGR)

# selecting the SPEI grid
SPEI_coor <- transform(df, lon_SPEI = longRang$longGR[findInterval(longitude, longRang$longST)]) %>% # classify the longitude
  transform(lat_SPEI = latRang$latGR[findInterval(latitude, latRang$latST)]) %>% # classify the latitude
  mutate(coor_SPEI = paste0(lon_SPEI, "_", lat_SPEI)) # creates coordinates for the later selection

# loading the files of SPEI data out of the data folder
fl <- list.files('data','*.nc',full.names=TRUE)
d <- list()
for (i in 1:length(fl)) {
  d[[i]] <- nc_open(fl[i])
}

# creates the data frame with the SPEI info
for(i in 1:nlevels(as.factor(SPEI_coor$coor_SPEI))){
  # first subseting the dataset by plot
  sub_grid <- subset(SPEI_coor, coor_SPEI == levels(as.factor(SPEI_coor$coor_SPEI))[i])
  # setting the coordinates on the grid
  lons <- d[[1]]$dim$lon$vals
  lats <- d[[1]]$dim$lat$vals
  tims <- d[[1]]$dim$time$vals
  lo <- sub_grid$lon_SPEI[1]
  la <- sub_grid$lat_SPEI[1]
  x <- which(lons==lo)
  y <- which(lats==la)
  t <- length(tims)
  # creating a matrix for the values
  a <- matrix(NA, ncol = 5, nrow = t)
  scales <- c("01","06","12","24")
  colnames(a) <- paste('SPEI_',scales,sep='')
  # extracting the data out of the .nc
  for (s in 1:length(fl)) {
    a[,s] <- ncvar_get(d[[s]],'spei',c(x,y,1),c(1,1,-1))
  }
  # adding the info of the observation date
  date <- seq(as.Date("1901/1/1"), as.Date("2015/12/1"), by = "month")
  a <- cbind(as.data.frame(a), date)
  a <- a %>%
    mutate(month = format.Date(date,'%m'), year = format.Date(date,'%Y')) %>%
    mutate(date_OBS = paste0(year, "_", month)) %>%
    select(-month, -year, -date)
  # joining the datasets by the observation date
  sub_grid <- sub_grid %>%
    left_join(a, by = "date_OBS")
  # filling up a dataframe with all subsets
  if(i == 1) {
    df <- sub_grid
  } else {
    df <- rbind(df, sub_grid)
  }
}

df <- df %>%
  select(plotcode, longitude, latitude, date_OBS, SPEI_01, SPEI_06, SPEI_12, SPEI_24)

df