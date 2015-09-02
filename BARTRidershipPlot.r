# =========================================================================
# Title:        BARTVisualization.R
# Author:       Lefteris Anastasopoulos
# Date:         September, 2015
# Description:  Updated R Code to Scrape and Visualize BART Openly Availbale BART data
#               Modified from code by Gaston Sanchez (2011)
# License:      MIT License
#               http://opensource.org/licenses/MIT
#               Copyright (c) 2015, Lefteris Anastasopoulos
#               All rights reserved
# =========================================================================


# R packages
library(sp)
library(rgeos)
library(XML)
library(ggplot2)
library(reshape)
library(RgoogleMaps)
library(maps)
library(maptools)
library(ggmap)
library(mapproj)


## First we need to get the coordinates (latitude and longitude)
## for each BART station. There are several ways to get this information
## but here I will parse the data from the google maps of each station
## that appear in the BART website

# url bart stations
bart_stations = "http://www.bart.gov/stations/index.aspx"

# parse internal document
doc_stations = htmlParse(bart_stations)

# find matching nodes
nodes = getNodeSet(doc_stations, "//div/ul/li/a")

# get bart station names
st_names = sapply(nodes, function(x) xmlValue(x, "id"))
st_names = st_names[22:66] #This was modified...info other than station names included

# get hrefs
hrefs = sapply(nodes, function(x) xmlGetAttr(x, "href"))

# get bart station hrefs
href_stations = hrefs[22:66]

# get station abbreviation
st_abb = gsub("/index.aspx", "", href_stations)

# get coordinates
bart_url = "http://www.bart.gov"
lat = rep("", length(href_stations))
lon = lat

for (i in 1:length(href_stations))
{
  st_map = paste(bart_url, href_stations[i], "/neighborhood.aspx", sep="")
  # open connection in read mode
  con_aux = url(st_map, open="r")
  # read lines
  tmp = readLines(con_aux)
  # close connection
  close(con_aux)
  # where are the longitude and latitude
  where = grep("lng", tmp)
  # split string by ',' comma
  where.split = strsplit(tmp[where], ",")
  # get third and fourth elements
  latitude = where.split[[1]][41]
  latitude.split = strsplit(latitude,":")
  latitude = latitude.split[[1]][4]
  latitude = gsub("\"", "", latitude)
  #For Longitude
  longitude = where.split[[1]][42]
  longitude = strsplit(longitude,"\"")
  longitude = longitude[[1]][4]
  longitude = gsub("\"", "", longitude)
  # store lat and lon
  lat[i] = latitude
  lon[i] = longitude
}

# convert as numeric
lat = as.numeric(lat)
lon = as.numeric(lon)

# create data frame
stations = data.frame(Name=st_names, lat=lat, lon=lon, stringsAsFactors=FALSE)


## For this portion you need to download BART Ridership data at
## "http://www.bart.gov/about/reports/ridership"
## For this example I use ridership data from April 2015

# import ridership data and rename the file accordingly
wd = "/BARTData/"
ridership.data = "Ridership_April2015_Saturday.csv"
exits = read.csv(paste(wd, ridership.data, sep=""), stringsAsFactors=FALSE)

# change name of stations with numbers in them
exits$Station[exits$Station == "19"] = "Oakland19thSt"
exits$Station[exits$Station == "12"] = "Oakland12thSt"
exits$Station[exits$Station == "16"] = "Mission16thSt"
exits$Station[exits$Station == "24"] = "Mission24thSt"

# reorder tables by station name
stations = stations[order(stations$Name),]
exits = exits[order(exits$Station),]

# add 'lat' and 'lon' to exits
exits$lat = stations$lat
exits$lon = stations$lon

# let's do a very simple plot
plot(exits$lon, exits$lat, pch=19, col=hsv(0,0,0.6,0.5), 
     cex=sqrt(exits$FY11/1000))


## Now we are ready to start visualizing the data with different maps
## First we're going to use 'RgoogleMaps'

# R Google Maps option 1 (color terrain map)
center = c(mean(exits$lat), mean(exits$lon))
zoom = min(MaxZoom(range(exits$lat), range(exits$lon)))
BayAreaMap1 = GetMap(center=center, zoom=zoom, destfile="BayAreaMap1.png")

# R Google Maps option 2 (gray hybrid map)
BayAreaMap2 = GetMap(center=center, zoom=zoom, destfile="BayAreaMap2.png", 
                     GRAYSCALE=TRUE, maptype="hybrid")

# R Google Maps option 3 (mobile map)
BayAreaMap3 = GetMap.bbox(exits$lon, exits$lat, destfile="BayAreaMap3.png", 
                          maptype="mobile")

# R Google Maps option 4 (gray mobile map)
BayAreaMap4 = GetMap.bbox(exits$lon, exits$lat, destfile="BayAreaMap4.png", 
                          GRAYSCALE=TRUE, maptype="mobile")

# plot on map 1
dev.new()
PlotOnStaticMap(BayAreaMap1, exits$lat, exits$lon, col=hsv(0.95,1,1,0.5), 
                pch=20, cex=sqrt(exits$FY11/1000))

# plot on map 2
dev.new()
PlotOnStaticMap(BayAreaMap2, exits$lat, exits$lon, col=hsv(0.65,1,1,0.5), 
                pch=20, cex=sqrt(exits$FY11/1000))

# plot on map 3
dev.new()
PlotOnStaticMap(BayAreaMap3, exits$lat, exits$lon, col=hsv(0.95,1,1,0.5), 
                pch=20, cex=sqrt(exits$FY11/1000))

# plot on map 4
dev.new()
PlotOnStaticMap(BayAreaMap4, exits$lat, exits$lon, col=hsv(0.65,1,1,0.5), 
                pch=20, cex=sqrt(exits$FY11/1000))


## We can also use the maps provided by ggmap
## get map type 'terrain'
baymap = get_map(location = c(lon=mean(exits$lon), lat=mean(exits$lat)), 
                 maptype="terrain", color="bw")

# plot with ggmap
ggmap(baymap) + 
  geom_point(data=exits, aes(x=lon, y=lat, size=FY11), colour="tomato", alpha=0.8) + 
  labs(x="", y="", size="Weekly \nRidership") +
  opts(title = "BART Weekly ridership - 2011",
       axis.text.x = theme_blank(),
       axis.text.y = theme_blank(),
       axis.ticks = theme_blank(),
       plot.title = theme_text(size=12))


## Let's try to get a representation by years
# melt data
exm = melt(exits, id.vars=c("Station", "lat", "lon"), variable_name="Year")

# change Year labels
levels(exm$Year) = 1999:2011

# define object ggmap
gg1 = ggmap(baymap) + 
  geom_point(data=subset(exm, Year!=2011), 
             aes(x=lon, y=lat, group=Year, colour=value, size=value), alpha=0.5) + 
  scale_size_continuous(breaks=c(500,1000,5000,10000,15000,20000,30000), range=c(2,8)) +
  facet_wrap(~ Year) +
  labs(x="", y="", size="weekly \nridership") +
  opts(title = "Average BART Weekly Ridership by Year",
       axis.text.x = theme_blank(),
       axis.text.y = theme_blank(),
       axis.ticks = theme_blank(),
       plot.title = theme_text(size=12))

# turn off color legend
sc = scale_colour_gradient(breaks=c(500,1000,5000,10000,15000,20000,30000), 
                           low="orange", high="red3") 
sc$legend = FALSE

# plot map
gg1 + sc


## With ggmap we have the option to plot data on stamen maps
# get stamen map
sfmap = get_map(location = c(lon=mean(exits$lon), lat=mean(exits$lat)), 
                maptype="terrain", color="bw", source="stamen")

# plot map
ggmap(sfmap) + 
  geom_point(data=exits, aes(x=lon, y=lat, size=sqrt(FY11/100), colour=FY11)) 


## Another feature of ggmap is the possibility to work with openstreet maps
# option with open street map
opmap = get_openstreetmap(bbox = c(left=min(exits$lon), bottom=min(exits$lat), 
                                   right=max(exits$lon), top=max(exits$lat)), scale=400000, color="bw")

# plot map
ggmap(opmap) + 
  geom_point(data=exits, aes(x=lon, y=lat, size=FY11), colour="red", alpha=0.5) +
  scale_size_continuous(range=c(3,8)) + 
  labs(x="", y="", size="Weekly \nRidership") +
  opts(title = "BART Stations - Average Weekly Ridership in 2011",
       axis.text.x = theme_blank(),
       axis.text.y = theme_blank(),
       axis.ticks = theme_blank(),
       plot.title = theme_text(size=12, colour="gray30"))
