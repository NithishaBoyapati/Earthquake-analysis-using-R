library(lubridate)
library(xts)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)
library(maps)
library(maptools)
library(viridisLite)
library(highcharter)
library(treemap)
library(viridisLite)
quake<- read.csv("Enter the file path/dataset.csv")
quake <- quake[,c("Date", "Time", "Latitude", "Longitude", "Type", "Depth", "Magnitude", "Magnitude.Type") ]

quake$Date <- as.Date(quake$Date, format = "%m/%d/%Y")
quake$Year<- year(quake$Date)
quake$Month <- month(quake$Date)
quake$Day <- day(quake$Date)

##Mapping 
quake %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat=quake$Latitude, lng=quake$Longitude, clusterOptions = markerClusterOptions(),
             popup= paste(quake$Type,
                          "<br><strong>Magnitude: </strong>", quake$Magnitude,
                          "<br><strong>Depth: </strong>", quake$Depth,
                          "<br><strong>Date: </strong>", quake$Date,
                          "<br><strong>Date: </strong>", quake$Time
             ))

quake %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lat=quake$Latitude, lng=quake$Longitude, weight=1, radius=1,
                   color= ifelse(quake$Magnitude>6.5,"red","yellow"),stroke=TRUE,
                   popup= paste(quake$Type,
                                "<br><strong>Magnitude: </strong>", quake$Magnitude,
                                "<br><strong>Depth: </strong>", quake$Depth,
                                "<br><strong>Date: </strong>", quake$Date,
                                "<br><strong>Date: </strong>", quake$Time)) %>%
  addLegend(labels=c("Magnitude > 6.5", "Magnitude < 6.5"), colors=c("red","yellow"))

##Affected Countries
world <- map('world', fill=TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(world$names, ":"), function(x) x[1])
world_sp <- map2SpatialPolygons(world, IDs=IDs,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
pointsSP <- SpatialPoints(cbind(x = quake$Longitude, y= quake$Latitude), 
                          proj4string=CRS("+proj=longlat +datum=WGS84"))
indices <- over(pointsSP, world_sp)
stateNames <- sapply(world_sp@polygons, function(x) x@ID)
quake$Country <- stateNames[indices]

quake_country <- quake[!is.na(quake$Country),]

##Treemap for total number of eartquakes occurred per country
##Indonesia has more earthquakes over all years
sum_country <- quake_country %>%
  group_by(Country) %>%
  summarise(Earthquakes=n())
tm <- treemap(sum_country, index = "Country",vSize = "Earthquakes", vColor ="#58ACFA", palette = viridis(6))


hc_tm <- highchart(height = 800) %>% 
  hc_add_series_treemap(tm, allowDrillToNode = TRUE,
                        layoutAlgorithm = "squarified",
                        name = "Earthquakes per Country") %>% 
  hc_title(text = "Total Earthquakes per Country") %>% 
  hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
             Earthquakes: {point.value:,.0f}")
hc_tm

##Directory for earthquakes per country
directory <- quake_country[, c("Country","Year","Month","Magnitude","Depth")] 
datatable(directory)

##Earthquakes per year and month using ggplot
quake<- quake[!is.na(quake$Date),]

##Earthquaker per year
per_year <- quake %>%
  filter(Type=="Earthquake") %>%
  group_by(Year) %>%
  summarise(Observations=n())
ggplot(per_year, aes(x=Year,y=Observations))+geom_bar(stat = "identity",fill="#58ACFA")+
  labs(y="Observations",
       x="Year",
       title="Earthquakes per Year",
       caption="Source: Significant Earthquakes, 1965-2016")+
  theme_grey()

##Earthquakes per month
per_month <- quake %>%
  filter(Type=="Earthquake") %>%
  group_by (Year, Month) %>%
  summarise(Observations=n())

per_month <- per_month %>%
  group_by (Month) %>%
  summarise(Mean=mean(Observations))

ggplot(per_month, aes(x=Month,y=Mean))+geom_bar(stat = "identity",fill="#58ACFA")+
  labs(y="Average",
       x="Month",
       title="Average Earthquakes per Month",
       caption="Source: Significant Earthquakes, 1965-2016")+
  theme_grey()

##Earthquakes per day of a month
per_day <- quake %>%
  filter(Type=="Earthquake") %>%
  group_by (Day,Year) %>%
  summarise(Observations=n())

per_day <- per_day %>%
  group_by (Day) %>%
  summarise(Mean=mean(Observations))

ggplot(per_day, aes(x=Day,y=Mean))+geom_bar(stat = "identity",fill="#58ACFA")+
  labs(y="Mean",
       x="Day",
       title="Avergae Earthquakes per Day of a Month",
       caption="Source: Significant Earthquakes, 1965-2016")+
  theme_grey()

