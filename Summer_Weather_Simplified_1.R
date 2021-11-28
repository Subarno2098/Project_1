# load DWD data for hot days per year (no. of days with max. temp >= 30°C)
http <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/annual/hot_days/" 

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


# Delete first tree entries which are empty because of the previously applied pattern
result_tidy <- result_tidy[4:length(result_tidy)]


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

# just grep all "hot" (= hot days) file
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


# Calculate mean hot days between 1951 and 2020
rasterHist_mean <- mean(rasterHist)
rasterHist_mean

library(RStoolbox)
library(gridExtra)
library(viridis)

maxVal <- max(unique(values(rasterHist_mean)),na.rm=T)
minVal <- min(unique(values(rasterHist_mean)),na.rm=T)

p1 <- ggR(rasterHist_mean, geom_raster = T)+
  scale_fill_viridis_c(option="magma",direction=-1,limits=c(minVal,maxVal))+
  labs(x="",y="")+
  ggtitle("mean hot days 1951 to 2020")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")
p1



#######################################
### Crop data to the extent of Bavaria ###
#######################################

# download boundary data for germany 
bnd <- raster::getData("GADM", country='DEU', level=1)
bnd.utm <- spTransform(bnd, CRS(proj4string(rasterHist)))

# visual check
plot(bnd.utm)

#subset of Bavaria
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



#####################################
### Animation with rasterVis package ###
#####################################

library(rasterVis)
library(animation)

library(classInt)

# plotting data for year 1954

maxVal <- max(unique(values(my_raster.by)),na.rm=T)
minVal <- min(unique(values(my_raster.by)),na.rm=T)


g<-gplot(my_raster.by$Year.1951) +
  scale_fill_viridis_c(option="magma",direction=-1,limits=c(minVal,maxVal))+
  ggtitle("hot days in Bavaria in 1954")+
  geom_tile(aes(fill = value))+
  facet_wrap(~variable)+
  coord_equal()
g


# animating the data using the levelplot function:

years <- paste("Year:",c(1951:2020)) # defining year-vector for subtitles 
colkey <- list(at=seq(0,51,.5))


saveGIF({
  for(i in c(1:nlayers(my_raster.by))){
    l <- levelplot(my_raster.by[[i]],margin=F,main="hot days in Bavaria",sub=years[i],xlab='Longitude',ylab='Latitude',colorkey=colkey)
    plot(l)
  }
}, interval=0.2, movie.name="hot_animation.gif")





