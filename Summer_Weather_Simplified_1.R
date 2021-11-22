http <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/annual/hot_days/"

# Data for monthly precitpiation in August can be found on the ftp as well:
# ftp://opendata.dwd.de/climate_environment ...

# List resulting datasets of given url
# activate library to fetch url infos
library(RCurl)
result <- getURL(http, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)

# Split string into pieces by identifying certain pattern that seperates the individual filenames
library(tidyverse)

result_tidy <- str_split(result, "\n|\r\n")  # sometimes \r needed
result_tidy <- result_tidy[[1]]

# Reorder data frame to alphabetically decreasing file names
result_tidy <- sort(result_tidy, decreasing = F)

# Delete first entry which is empty because of the previously applied pattern
result_tidy <- result_tidy[3:length(result_tidy)]s

# Data can already be subsetted to desired years e.g. 1961-2018
# 1: 1881
# 80: 1980
# 138: 2018
result_tidy <- result_tidy[c(seq(1,138, by=1))]

# Define working directory

getwd()
 # Define output directory of downloads
# create one if not yet there, warning if it exists
dir.create("DWDdata/")

out_dir <- "DWDdata/"

# loop for downloading all files listed in the ftp-server if they do not yet exist in the folder
for (i in 1:length(result_tidy)) {
  if(file.exists(paste0(out_dir, result_tidy[i]))){
    print(paste0(result_tidy[i], sep=" ", "file already exists"))
  }
  else
  {
    download.file(paste0(http, result_tidy[i]), paste0(out_dir, result_tidy[i]))
  }
}


############################
### Read ASCII-Grid-File ###
############################

# ## Define file names and directory
mypath <- "DWDdata/"

# just grep all "temp" (= temperature) file, instead of "precipitation"
# check the names in the folder which pattern is appropriate
hot <- grep("*hot*", list.files(path = mypath, pattern="*.gz$"), value=T)

filenames <- paste0(mypath, hot)

# read all ascii files and convert them into a raster stack
# activate relevant raster packages
library(sp)
library(raster)

for (i in 1:length(filenames)){
  if (i == 1){
    # for the first run define our final raster file ...
    current_ascii <- read.asciigrid(filenames[i])
    # remove the raster in case it already exists to avoid duplicate entries
    rm(my_raster)
    my_raster <- raster(current_ascii)
  } else {
    # ... and fill it with each additional run with another layer
    current_ascii <- read.asciigrid(filenames[i])
    current_raster <- raster(current_ascii)
    my_raster <- stack(my_raster, current_raster)
    # Delete all variables except for the raster stack "my_raster"
    rm(i, current_ascii, current_raster)
  }
}


# optional to check the structure

my_raster

# Change names of raster layers
# adapt sequence in case you subsetted the data before

layer_names <- c(paste0(" Year ", seq(1951, 2020, by=1)))
names(my_raster) <- layer_names
layer_names
# Subset Raster-Stack into old dates and new date
# select range of historical data to subset

# time-series data, to use for temporal aggregation
# define the first and last year to grab from the time series 

rasterHist <- my_raster[[grep("1951", layer_names):grep("2020", layer_names)]]
rasterHist




# Add Coordinate Reference System to rasterstack
# information extracted from DWD webpage
# ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/monthly/air_temperature_mean/DESCRIPTION_gridsgermany_monthly_air_temperature_mean_en.pdf
my_crs <- "+init=epsg:31467"

rasterHist@crs <- sp::CRS(my_crs)
rasterHist

# for temperature only!
# do NOT use for precipitation or other datasets
# Divide by 10 to get values in C as described in the description pdf on the ftp server:
# ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/monthly/air_temperature_mean/
# DESCRIPTION_gridsgermany_monthly_air_temperature_mean_en.pdf
rasterHist <- rasterHist/10


# Calculate mean temperature between 1951 and 2020
rasterHist_mean <- mean(rasterHist)
rasterHist_mean

library(RStoolbox)
library(gridExtra)

maxVal <- max(c(unique(values(rasterComp)),unique(values(rasterHist_mean))),na.rm=T)
minVal <- min(c(unique(values(rasterComp)),unique(values(rasterHist_mean))),na.rm=T)


p1 <- ggR(rasterHist_mean, geom_raster = T)+
  scale_fill_gradient2(low="blue", mid='yellow', high="red", name ="hot days", na.value = NA, limits=c(minVal,maxVal))+
  # , guide = F
  labs(x="",y="")+
  ggtitle("hot days 1951 to 2020")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")
p1

library(raster)
library(ggplot2)
library(gganimate)


# animating rasterHist
raster::animate(rasterHist, pause=1)






# download boundary data
bnd <- raster::getData("GADM", country='DEU', level=1)
bnd.utm <- spTransform(bnd, CRS(proj4string(rasterHist)))

# visual check
plot(bnd.utm)

bnd.utm.by <- bnd.utm[bnd.utm$NAME_1=="Bayern",]

# visual check
plot(rasterHist,2)
plot(bnd.utm.by,add=T)

# crop and mask the data
my_raster.by <- crop(my_raster, bnd.utm.by)
my_raster.by <- mask(my_raster.by, bnd.utm.by)

# class of my_raster.by
class(my_raster.by)
my_raster.by

# visual check
plot(my_raster.by,1:10)

# animating bayern data
raster::animate(my_raster.by, pause = 2)




# converting raster data into data frame
by_df <- as.data.frame(my_raster.by, xy = TRUE)

# checking the classs of by_df
class(by_df)

# dropping NAs

by_df <- as.data.frame(my_raster.by, xy = TRUE) %>% drop_na()
head(by_df)

