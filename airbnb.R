setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load the libraries
library("sp")
library("rgdal")
library("rgeos")
library("tmap")
library(ggplot2)
library(dplyr)
library(plyr)
library(spatstat)  
library(maptools)

# boundary of the city
neighborhoods <- readOGR("neighborhood.shp")
plot(neighborhoods)
#f_neighborhoods <- fortify(neighborhoods)

# load the airbnbs
june2016 <- readOGR("june2016.shp")
june2017 <- readOGR("june2017.shp")
july2018 <- readOGR( "july2018.shp")
feb2019 <- readOGR( "feb2019.shp")

#reviews per month


# Set the projection 
proj4string(neighborhoods) <- CRS("+init=EPSG:4326")
proj4string(june2016) <- CRS("+init=EPSG:4326")
proj4string(june2017) <- CRS("+init=EPSG:4326")
proj4string(july2018) <- CRS("+init=EPSG:4326")
proj4string(feb2019) <- CRS("+init=EPSG:4326")
 #plot(neighborhoods)


# maps
tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey") + tm_borders(col="black") +
tm_shape(june2016) + tm_dots(col = "blue", scale = 0.5)
 
 
# maps
# tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey") + tm_borders(col="black") +
#   tm_shape(june2017) + tm_dots(col = "blue", scale = 0.5)
 
# maps
# tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey") + tm_borders(col="black") +
#   tm_shape(july2018) + tm_dots(col = "blue", scale = 0.5)
 
# maps
#tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey") + tm_borders(col="black") +
#   tm_shape(feb2019) + tm_dots(col = "blue", scale = 0.5)

 df_neighborhoods <- data.frame(neighborhoods) 
 df_june2016 <- data.frame(june2016) 
 df_june2017 <- data.frame(june2017) 
 df_july2018 <- data.frame(july2018) 
 df_feb2019 <- data.frame(feb2019) 
 
 df_june2016 <-mutate(df_june2016, year=2016)
 df_june2017<-mutate(df_june2017,year=2017)
 df_july2018<- mutate(df_july2018,year=2018)
 df_feb2019<-mutate(df_feb2019, year=2019)
 
 df_all <- rbind(df_june2016 ,df_june2017,df_july2018,df_feb2019)

 listings <- dplyr::count(group_by(df_all,year))
 plot(listings)
 plot(listings, type="h", col="red", lwd=5, xlab="year", ylab="n", main="Exponential decay")
 
ggplot(data=listings, aes(x=year, y=n, group=1)) +
geom_line(linetype = "dashed")+ geom_point()

listing_type<-dplyr::count(group_by(df_all,room_type, year))



ggplot(data = listing_type, aes( x = factor( year ), y =n, fill = room_type ) ) +    # print bar chart
  geom_bar( stat = 'identity' ,position = 'dodge')          # add a density estimate with defaults

library(sf)
#make data points df_all and each year
all.sf.point <- st_as_sf(x = df_all, 
                              coords = c("latitude", "longitude"))
june2016.sf.point <- st_as_sf(x = df_june2016, 
                         coords = c("latitude", "longitude"))
june2017.sf.point <- st_as_sf(x = df_june2017, 
                         coords = c("latitude", "longitude"))
july2018.sf.point <- st_as_sf(x = df_july2018, 
                         coords = c("latitude", "longitude"))
feb2019.sf.point <- st_as_sf(x = df_feb2019, 
                         coords = c("latitude", "longitude"))

#neighborhoods.sf <- st_as_sf(x = f_neighborhoods)
df_neighbor <- data.frame(f_neighborhoods)
#---------------June 2016---------------------------
plot(st_geometry(june2016.sf.point))

#find entire apartments
df_apartment<-subset(june2016.sf.point, room_type !='Private room' & room_type !='Shared room')
df_apartment

df_room <- subset(june2016.sf.point, room_type !='Entire home/apt' & room_type !='Shared room')
df_room

df_shared <- subset(june2016.sf.point, room_type !='Entire home/apt' & room_type !='Private room')
df_shared


apartment_area <- count(df_apartment$neighbou_1)
  
apartment_area<-rename(apartment_area, c('x'='neighbourh'))

