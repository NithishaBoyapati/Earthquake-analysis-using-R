library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

earthquake <- fread("Enter the file path/dataset.csv")
dim(earthquake)
str(earthquake)
earthquake <- earthquake[, Date := as.Date(Date, format = "%m/%d/%Y")]
earthquake <- earthquake[, Year := year(Date)]
world <- map_data("world")

sum_year <- earthquake %>% 
  select(Year)  %>% 
  group_by(Year)  %>% 
  summarize(count = n())
datatable(sum_year)
ggplot(sum_year, aes(x =  Year, y = count, colour = "blue" , alpha = 1.5))  + 
  geom_point()  + geom_line()

sum_year <- earthquake %>% 
  select(Year, Magnitude, Source)  %>% 
  group_by(Year, Source)  %>% 
  summarize(count = n(), mean = mean(Magnitude))
datatable(sum_year)

sum_magn_type <- earthquake  %>% 
  select(Year, `Magnitude Type`) %>% 
  group_by(Year, `Magnitude Type`) %>% 
  summarize(count = n())
datatable(sum_magn_type)
ggplot(sum_magn_type, aes(x =  Year, y = count, fill = `Magnitude Type`, 
                          colour = `Magnitude Type`, alpha = 1.5))  + 
  geom_point()  + geom_line()

ggplot(sum_year, aes(x =  Year, y = count, fill = Source, 
                     colour = Source, alpha = 1.5))  + 
  geom_point()  + geom_line()


freq_magn_type <- earthquake %>% 
  select(`Magnitude Type`)  %>% 
  group_by(`Magnitude Type`)  %>% 
  summarize(count = n())

datatable(freq_magn_type)
ggplot(freq_magn_type, aes(x =  `Magnitude Type`, 
                           y =  count, colour = `Magnitude Type`,
                           fill = `Magnitude Type`, 
                           alpha = 0.7))  + 
  geom_bar(stat = "identity")

ggplot(earthquake, aes(x  = Magnitude, colour = "blue",
                       fill = "blue", 
                       alpha = 0.5))  + 
  geom_density()

ggplot(earthquake, aes(x =  Magnitude, colour = `Magnitude Type`, 
                       fill = `Magnitude Type`, alpha = 0.01)) + 
  geom_density()



ggplot() + geom_polygon(data = world, aes(x =  long,  y= lat, group = group)) + 
  geom_point(data = earthquake, 
             aes(x = Longitude, y = Latitude, colour = `Magnitude Type` )) +
  stat_density2d(show.legend = FALSE) + xlab("Longitude") +ylab("Latitude") + ggtitle("Magnitude Type")+ coord_quickmap()


ggplot() + geom_polygon(data = world, aes(x =  long,  y= lat, group = group)) + 
  geom_point(data = earthquake, 
             aes(x = Longitude, y = Latitude, colour = Status , alpha = 0.3)) +
  stat_density2d(show.legend = FALSE) + xlab("Longitude") +ylab("Latitude") + ggtitle("Coloured By Status")+ coord_quickmap()


ggplot() + geom_polygon(data = world, aes(x =  long,  y= lat, group = group)) + 
  geom_point(data = earthquake[Status == "Automatic", ], 
             aes(x = Longitude, y = Latitude, colour = Status , alpha = 0.01, size = Depth)) +
  stat_density2d(show.legend = FALSE) + xlab("Longitude") +ylab("Latitude") + ggtitle("Automatic Only ")+ coord_quickmap()


ggplot() + geom_polygon(data = world, aes(x =  long,  y= lat, group = group)) + 
  geom_point(data = earthquake[Status == "Reviewed", ], 
             aes(x = Longitude, y = Latitude, colour = Status , alpha = 0.01, size = Depth)) +
  stat_density2d(show.legend = FALSE) + xlab("Longitude") +ylab("Latitude") + ggtitle("Reviewed Only  ") + coord_quickmap()

