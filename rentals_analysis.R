setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd
# load the libraries
#install.packages ("tmap")
library(sp)
library(rgdal)
library("rgeos")
library(tmap)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plyr)
library(spatstat)
library(maptools)
library(sf)
library(tidyverse)
library(GISTools)
library(mapview)
library(RColorBrewer)
library(maps)
library(sda)
library(ggthemes)
library(geospt)
library(geosphere)

#scale_x_discrete(labels= label_neighborhood)
label_neighborhood <- c("Frederiksberg","Brønshoj-Husum","Østerbro",
                        "Indre By","Nørrebro","Bispebjerg",
                        "Valby","Vesterbro","Amager Vest",
                        "Amager Øst","Vanløse")

# reading neighbourh polygones into columns(lat, long)
neighbour_poly = readOGR(dsn=".", layer="neighborhood")
proj4string(neighbour_poly) <- CRS("+init=EPSG:25833")
neighbour_poly@data$id = rownames(neighbour_poly@data)
neighbour_poly.points = fortify(neighbour_poly, region="id")
neighbour_poly.df = join(neighbour_poly.points, neighbour_poly@data, by="id")


# simple reading neighbourhoods
neighborhoods <- readOGR("neighborhood.shp")
neighborhoods_table <- st_read("neighborhood.shp")
proj4string(neighborhoods) <- CRS("+init=EPSG:25833")

# simple plot of neighbourhood
# plot(neighborhoods_table['neighbourh'])

# calculate central points of neighbourh
centroids <- as.data.frame(centroid(neighborhoods))
colnames(centroids) <- c("lon", "lat")
neighborhoods_table$lon<-centroids$lon
neighborhoods_table$lat<-centroids$lat
neighborhoods_table

# plot simple map of neighbourhoods
pp<-ggplot(neighbour_poly.df, aes(long,lat, group=neighbourh) ) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  coord_cartesian(xlim = NULL, ylim = NULL, expand = TRUE,default = FALSE, clip = "on")+
  geom_text(data=neighborhoods_table, aes(lon, lat, label = label_neighborhood), colour="white",size=3.5);pp
pp

# rental data
rents <- readOGR("rents_withOut_outliers.shp")
rents_table <- st_read("rents_withOut_outliers.shp")
rents_table<- st_transform(rents_table,25833)
st_crs(rents_table)
proj4string(rents) <- CRS("+init=EPSG:25833")

# add column price/m3 into spatial data
rents$rentM2 <- rents$rent /rents$size

# add column price/m3 into table data
rents_table$rentM2 <- rents_table$rent /rents_table$size

#---- OUTLIER  TREATMENT ------

# filter only needed years
rents_f <- filter(rents_table, 
          (year == 2014| year == 2015| year== 2016| year == 2017| year == 2018)
          & (rooms == 1 | rooms == 2| rooms == 3 | rooms == 4 | rooms == 5))

# add column price/m3 into table data
rents_f$rentM2 <- rents_f$rent /rents_f$size


label_neighborhood <- c("Amager Øst","Amager Vest","Bispebjerg",
                        "Brønshoj-Husum","Frederiksberg","Indre By",
                        "Nørrebro","Østerbro","Valby","Vanløse",
                        "Vesterbro")

# outlieres accross the neighbourhood
geom_boxplot(aes(colour = neighbourh),
                 varwidth = TRUE,
                 outlier.colour = "black",
                 outlier.shape = 1) + coord_flip()+
  scale_x_discrete(labels= label_neighborhood)

#______C L E A N I N G ____D A T A ____

rents_f2<-rents_f[!(rents_f$rentM2<=20),]
rents_f2<-rents_f[!(rents_f$rentM2>=600),]

#_______Trying the regresion analysis____

Filter_2015<-filter(rents_f,(year == 2015))


rents_f$date1<-paste(month.abb[as.numeric(rents_f$month)], rents_f$year, sep="-" )
rents_f$date2<-paste(1,rents_f$month,rents_f$year,sep="/")
rents_f$Date <- as.Date(rents_f$date2,format = "%d/%m/%Y")
rents_f
ggplot(rents_f, aes(x=Date, y=rentM2, color=neighbourh)) + geom_point(shape=1)


# Same, but with different colors and add regression lines
ggplot(rents_f, aes(x=Date, y=rentM2, color=neighbourh)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region


c<-unique(rents_f$neighbourh, incomparables = FALSE)

Filter<-filter(rents_f,(neighbourh == (c[1])))
ggplot(Filter,aes(Date, rentM2))+geom_point(aes(colour = neighbourh))+
      geom_smooth(method='lm', aes(fill=neighbourh))

Filter<-filter(rents_f,(neighbourh == (c[2])))
ggplot(Filter,aes(Date, rentM2))+geom_point(aes(colour = neighbourh))+
  geom_smooth(method='lm', aes(fill=neighbourh))

Filter<-filter(rents_f,(neighbourh == (c[3])))
ggplot(Filter,aes(Date, rentM2))+geom_point(aes(colour = neighbourh))+
  geom_smooth(method='lm', aes(fill=neighbourh))

Filter<-filter(rents_f,(neighbourh == (c[4])))
ggplot(Filter,aes(Date, rentM2))+geom_point(aes(colour = neighbourh))+
  geom_smooth(method='lm', aes(fill=neighbourh))
    
#______________________________________________________________


blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )
# Scatter plot of x and y variables and color by groups

scatterPlot <- ggplot(rents_f,aes(Date, rentM2, color=neighbourh)) + 
  geom_point() + 
  scale_color_manual(values = c('#999999','#E69F00','green','yellow','blue','pink','red','black','#994489','#991155','#009999','#111111')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(rents_f, aes(Date, fill=neighbourh)) + 
  geom_density(alpha=.5) + 
  scale_color_manual(values = c('#999999','#E69F00','green','yellow','blue','pink','red','black','#994489','#991155','#009999','#111111')) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(rents_f, aes(rentM2, fill=neighbourh)) + 
  geom_density(alpha=.5) + 
  scale_color_manual(values = c('#999999','#E69F00','green','yellow','blue','pink','red','black','#994489','#991155','#009999','#111111')) + 
  theme(legend.position = "none")
ydensity
 
library("gridExtra")
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#__________________________________

neighbourh1<-filter(rents_f,(neighbourh == "Indre By" | neighbourh == "Frederiksberg"))

scatterPlot <- ggplot(neighbourh1,aes(Date, rentM2, color=neighbourh)) + 
  geom_point() + 
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(neighbourh1, aes(Date, color=neighbourh)) + 
  geom_density(alpha=.5) + 
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(neighbourh1, aes(rentM2, colour=neighbourh)) + 
  geom_density(alpha=.5) + 
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
ydensity
library("gridExtra")
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))


#________________________________________________

model <- lm(date2 ~ rentM2, Filter_2014)

plot(rents_f$Date,rents_f$rentM2,
     xlim=c(min(as.Date(rents_f$Date)),max(as.Date(rents_f$Date))),
     ylim=c(0, 1000),
     main="With Outliers", 
     xlab="date2", ylab="rentM2", pch="*", col="red", cex=2)
abline(lm(Date ~ rentM2, data=rents_f), col="blue", lwd=3, lty=300)


#levels = c(2014-1, 2014-2,2014-3,2014-4,2014-5,2014-6,2014-7,2014-8,2014-9,2014-10,2014-11,2014-12)
#levels = c("2014-1", "2014-2","2014-3","2014-4","2014-5","2014-6","2014-7","2014-8","2014-9","2014-10","2014-11","2014-12")


#for all years
ggplot(rents_f,aes(Date, rentM2)) + geom_point(aes(colour = neighbourh))
  

# outlieres accross the neighbourhood
p <- ggplot(rents_f, aes(neighbourh, rentM2));p
p + geom_boxplot(aes(colour = neighbourh),
                 varwidth = TRUE,
                 outlier.colour = "black",
                 outlier.shape = 1) + coord_flip()+
  scale_x_discrete(labels= label_neighborhood)


library(vegan)
# get the z-scores for each value in refunt_value
outlier_scores <- scores(rents_f$rentM2)
rents_outliers<- rents_f[outlier_scores > 400| outlier_scores < 90, ]

is_outlier <- outlier_scores > 300 | outlier_scores < 20

# add a column with info whether the refund_value is an outlier
rents_f$is_outlier <- is_outlier

#create outliers dataframe
Outliers <- rents_f[rents_f$is_outlier == T, ]

# add a column with info whether the refund_value is an outlier
ggplot(rents_f, aes(neighbourh,rentM2)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~is_outlier)+scale_x_discrete(labels= label_neighborhood)

#outlieres accross the year
outlier_values<-boxplot(rentM2 ~ neighbourh,data=rents_f,col = "skyblue",main = "RentM2 reading across year")
outlier_values

#numbers of rental apartments
p<- ggplot(neighbour_poly.df, aes(x=long,y=lat))+
  geom_polygon(aes(group=group),fill="white",colour="grey",size=1, show.legend =T);p


gg <- p + geom_point(data=rents_f, aes(x = lng, y = lat, col = rooms)) + 
  labs(title = "Scatter Plot",
       subtitle =" The Geographic Location of Room Type", 
       x = "Longitude",
       y = "Latitude",
       caption = "Source: Akutbolig"); gg


#.............S T A R T  OF   R O O M - P I E   C H A R T..........

rents_f <- subset(rents, 
                  (year == 2014| year == 2015| year== 2016| year == 2017| year == 2018)
                  & (rooms == 1 | rooms == 2| rooms == 3 | rooms == 4 | rooms == 5))

room1 <- rents_f[rents_f$rooms == '1',]
room2 <- rents_f[rents_f$rooms == '2',]
room3 <- rents_f[rents_f$rooms == '3',]
room4 <- rents_f[rents_f$rooms == '4',]
room5 <- rents_f[rents_f$rooms == '5',]

neighborhoods_table1<-neighborhoods_table

n.breach<- poly.counts(rents_f,neighborhoods)
neighborhoods_table1$total_room<-n.breach

room1<- poly.counts(room1,neighborhoods)
neighborhoods_table1$room1<-room1

room2<- poly.counts(room2,neighborhoods)
neighborhoods_table1$room2<-room2

room3<- poly.counts(room3,neighborhoods)
neighborhoods_table1$room3<-room3

room4<- poly.counts(room4,neighborhoods)
neighborhoods_table1$room4<-room4

room5<- poly.counts(room5,neighborhoods)
neighborhoods_table1$room5<-room5

neighborhoods_table1$geometry <- NULL

pp<- ggplot(neighbour_poly.df, aes(x=long,y=lat))+
  geom_polygon(aes(group=group),fill="ivory",colour="black",size=.1, show.legend =T)+
  theme_dark();pp

library(scatterpie)

pp+geom_scatterpie(aes(lon,lat,r = sqrt(total_room+2000)/10000),
                  data = neighborhoods_table1, 
                  cols = c( "room1","room2", "room3","room4","room5"), 
                  color = NA, alpha=.8) +
  theme(plot.title = element_text(hjust = 0.5))+
   labs(title = "Number of rooms in rental offers from 2014 to 2018 in Copenhagen ",
       caption = "Source: 2019 Akutbolig",
       fill = NULL)+
  scale_fill_manual(
    breaks = c("room1", "room2", "room3", "room4","room5",
               "Frederiksberg","Brnshj-Husum","sterbro",
               "Indre By","Nrrebro","Bispebjerg",
               "Valby","Vesterbro-Kongens Enghave","Amager Vest",
               "Amager st","Vanlse"),
    
    labels = c("1", "2", "3", "4","5",
               "Frederiksberg","Brønshoj-Husum","Østerbro",
               "Indre By","Nørrebro","Bispebjerg",
               "Valby","Vesterbro","Amager Vest",
               "Amager Øst","Vanløse"),
    
    values = c("room1" = "indianred","room2" = "orange","room3" = "#339999", 
               "room4" = "lightgoldenrod","room5" = "#2F4891"))


................E N D   OF   R O O M - P I E   C H A R T..........

## How many breaches of peace in each census block?
n.breach<- poly.counts(rents,neighborhoods)
neighborhoods_table$total<-n.breach
neighborhoods$total<-n.breach

# Simple densities and map
#choropleth(neighborhoods,n.breach/poly.areas(neighborhoods))



.... T O T A L...R E N T S... P E R....N E I G H B O U R H O O D S......

#install.packages("leaflet")
library(leaflet)

# Add some basemap
#m <- leaflet() %>% setView(lng = 12.5701, lat =55.6789 , zoom = 11)
#m %>% addTiles()
#m %>% addProviderTiles(providers$Stamen.Toner)

# Interactive MAP of total rents per neighbourhood 
label_neighborhood <- c("Frederiksberg","Brønshoj-Husum","Østerbro",
                        "Indre By","Nørrebro","Bispebjerg",
                        "Valby","Vesterbro","Amager Vest",
                        "Amager Øst","Vanløse")

mapviewOptions(basemaps = c("Stamen.Toner", "OpenStreetMap.DE"),
               vector.palette = colorRampPalette(brewer.pal(9, "OrRd")),
               na.color = "magenta",
               layers.control.pos = "topright")

map<-mapview(basebmap=c("OpenStreetMap"),
        addStaticLabels= "total",
        neighborhoods_table, zcol = "total", legend = TRUE,
        at = seq(700, 10000,100),
        layer.name = c("Number of rent offers"))

map_label = addStaticLabels(map,
                     data = neighborhoods_table,
                     label = label_neighborhood)
map_label

# Static MAP- Numbers of rental offers
map_total<-tm_shape(neighborhoods)+
  tm_fill(col = "total", title = "No. rents", pallete= "seq") +
  tm_borders(lwd = 0.3) +
  tm_text("neighbourh", size = 0.7)+
  tm_style("grey") +
  tm_layout(
    main.title = "Number of rents offer 2014-2019(March)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("left", "bottom"),
    aes.palette = list(seq = "PuBuGn")) +
  tm_credits("Data:\n2019 Akutbolig", position = c("right", "bottom"))

tmap_arrange (map_total)

# Static MAP1- Numbers of rental offers (lenged into continous color scale)
map_total<-tm_shape(neighborhoods) +
  tm_fill(col = "total", title = "No. rents", pallete= "seq",style = "cont") +
  tm_borders(lwd = 0.3, col="grey") +
  tm_text("neighbourh", size = 0.7)+
  tm_style("cobalt") +
  tm_layout(
    main.title = "Number of rental offers 2014-2019(March)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("left", "bottom"),
    aes.palette = list(seq = "PuBuGn")) +
  tm_credits("Data:\n2019 Akutbolig", position = c("right", "bottom"))

tmap_arrange (map_total)

# Static MAP2- Numbers of rental offers (lenged into continous color scale)
map_total<-tm_shape(neighborhoods) +
  tm_fill(col = "total", title = "No. rents", pallete= "seq",style = "cont") +
  tm_borders(lwd = 0.3, col="grey") +
  tm_text("neighbourh", size = 0.7)+
  tm_style("classic") +
  tm_layout(
    main.title = "Number of rental offers 2014-2019",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("left", "bottom"),
    aes.palette = list(seq = "PuBuGn")) +
  tm_credits("Data:\n2019 Akutbolig", position = c("right", "bottom"))

tmap_arrange (map_total)



#--------------P O I N T   M A P ------------

#filter only those year in rets
rent_2014_2019 = rents_table %>% 
  filter(year %in% c(2014, 2015, 2016, 2017, 2018))

#plot points
point_map <- ggplot(rent_2014_2019, aes(lng, lat))+
  geom_point(aes(colour = factor(year)), size = 1)
#point_map

#.......S E L E C T I O N (Y E A R) ..............

year2014 <- rents[rents$year == '2014',]
year2015 <- rents[rents$year == '2015',]
year2016 <- rents[rents$year == '2016',]
year2017 <- rents[rents$year == '2017',]
year2018 <- rents[rents$year == '2018',]
year2019 <- rents[rents$year == '2019',]

n.breach<- poly.counts(rents,neighborhoods)
neighborhoods_table$total<-n.breach


#---2014
total2014<- poly.counts(year2014,neighborhoods)
neighborhoods_table$total2014<-total2014
neighborhoods$total2014<-total2014

# mapview of neighbourhood
map2014<-mapview(neighborhoods, zcol = c("total2014"),
        map.types = "OpenStreetMap.DE",
        col.regions = mapviewGetOption ("raster.palette")(256),
        at = seq(100, 1100, 100),
        layer.name = c("Number of rent offer"),
        hide = TRUE)
#map2014

total2015<- poly.counts(year2015,neighborhoods)
neighborhoods_table$total2015<-total2015
neighborhoods$total2015<-total2015

total2016<- poly.counts(year2016,neighborhoods)
neighborhoods_table$total2016<-total2016
neighborhoods$total2016<-total2016

total2017<- poly.counts(year2017,neighborhoods)
neighborhoods_table$total2017<-total2017
neighborhoods$total2017<-total2017

total2018<- poly.counts(year2018,neighborhoods)
neighborhoods_table$total2018<-total2018
neighborhoods$total2018<-total2018

#...........P I E...C H A R T ..........

library('raster')
library('geosphere')

#print data type of values
sapply(neighborhoods_table, typeof) 

# IF IS NEEDED TO TRANSFORM TO 
neighborhoods_table<- neighborhoods_table %>%
  mutate_if(is.double, as.numeric)

# delete the geometry in 
neighborhoods_table$geometry <- NULL
neighborhoods_table

library(mapproj)
p<- ggplot(neighbour_poly.df, aes(long,lat,group=group))+
  geom_polygon(fill="808000",colour="#778899",size=1,
               show.legend =T);p

p+geom_scatterpie(aes(lon,lat,r = sqrt(total+2000)/10000),
                  data=neighborhoods_table, 
                  cols=c( "total2014",  "total2015", "total2016","total2017","total2018"), 
                  color=NA, alpha=.8)+
  labs(title = "Rental offers during 2014-2018 in Copenhagen",
       caption = "Source: 2019 Akutbolig",
       fill = NULL)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    breaks = c("total2014", "total2015", "total2016", "total2017","total2018",
               "Frederiksberg","Brnshj-Husum","sterbro",
               "Indre By","Nrrebro","Bispebjerg",
               "Valby","Vesterbro-Kongens Enghave","Amager Vest",
               "Amager st","Vanlse"),
    
    labels = c("2014", "2015", "2016", "2017","2018",
               "Frederiksberg","Brønshoj-Husum","Østerbro",
               "Indre By","Nørrebro","Bispebjerg",
               "Valby","Vesterbro","Amager Vest",
               "Amager Øst","Vanløse"),
    
    values = c("total2014" = "#55DDE0","total2015" = "#2F4891","total2016" = "#339999", 
               "total2017" = "#F6AE2D","total2018" = "#F26419"))
  
#--------------- E N D - P I E  C H A R T ----------------------


--------------S T A R T - RentM2 ------------
  
df_neighborhoods <- data.frame(neighborhoods) 
df_year2014 <- data.frame(year2014)
df_year2015 <- data.frame(year2015) 
df_year2016 <- data.frame(year2016) 
df_year2017 <- data.frame(year2017) 
df_year2018 <- data.frame(year2018) 
df_year2019 <- data.frame(year2019) 
df_all <- rbind(df_year2014, df_year2015, df_year2016 ,df_year2017,df_year2018)

#Airbnb Annual Growth in Copenhagen
df_all<- rbind(df_year2014, df_year2015, df_year2016 ,df_year2017,df_year2018)
avarage_neighbourh<-df_all %>% group_by(df_all$neighbourh)%>%
  summarise_at(vars(rentM2), funs(mean(., na.rm=TRUE)))
avarage_neighbourh

label_neighborhood <- c("Amager Øst","Amager Vest","Bispebjerg",
                        "Brønshoj-Husum","Frederiksberg","Indre By",
                        "Nørrebro","Østerbro","Valby","Vanløse",
                        "Vesterbro")

#avarage price per M3 FOR each neighbourhood
ggplot(df_all, aes(x=neighbourh, y=rentM2)) + stat_summary(fun.y="mean", geom="bar")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Average rental price per square meter, 2014-2018", 
       x ="Neighbourhood", y = "Price Square meter")+ coord_flip()
  #scale_x_discrete(labels= label_neighborhood)



#avarage price per M3 per each year(second way)
df_all2<- rbind(df_year2014, df_year2015, df_year2016 ,df_year2017,df_year2018)


ggplot(data = avarage_rent, aes( x = year, y =Mean))+ ylim(0, 300)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Average property price per square meter",  x ="Year", y = "Price Square meter")+
  geom_bar( stat = 'identity' ,position = 'dodge')


#Annual Growth- per M3 per each year(second way)

year_borough <- count_(df_all2, vars = c('year', 'neighbourh', 'rentM2'), sort = TRUE) %>% 
  arrange(year ,neighbourh, Mean(rentM2))

avarage_rent<-ddply(df_all2, .(year,neighbourh), summarize,  Mean=mean(rentM2))

year_growth <- avarage_rent  %>%
  ggplot(aes(year,Mean, group = neighbourh)) + geom_line(aes(colour=neighbourh)) +
  ggtitle("Annual rental Growth in CPH,2014-1018 ")  + labs(x = "Year", y = "rentM2")
year_growth


library(ggpubr)

avg_price <-df_all2 %>%
  group_by(neighbourh) %>%
  dplyr::summarize(Mean = mean(rentM2, na.rm=TRUE))
avg_price


popular<-mutate(df_all2,mean_price=(avg_price$Mean)) 

ggscatter(popular, x = "n", y = "mean_price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Count popularity", ylab = "Mean price")


# 
ggqqplot(popular$n, ylab = "Count popularity")
# 
ggqqplot(popular$mean_price, ylab = "Mean price")




---------------------------------------------
  
  new.df1$host_since <- as.character(new.df1$host_since)

new.df1$host_since_year <- str_sub(new.df1$host_since, 1, 4)
new.df1$host_since_month <- str_sub(new.df1$host_since, 6, 7)
new.df1$host_since_day <- str_sub(new.df1$host_since, -2, -1)


new.df1$host_since_year <- as.integer(new.df1$host_since_year)
new.df1$host_since_month <- as.integer(new.df1$host_since_month)
new.df1$host_since_day <- as.integer(new.df1$host_since_day)
  
year_borough <- count_(new.df1, vars = c('host_since_year', 'District'), sort = TRUE) %>% 
  arrange(host_since_year , District)

year_growth <- year_borough  %>%
  ggplot(aes(host_since_year, n, colour = District)) + geom_line() +
  ggtitle("Airbnb Annual Growth in SF ") + ylim(0, 400)  + labs(x = "Year", y = "# of Hosts"); year_growth


--------------E N D -  RentM2 ------------

tm_shape(neighborhoods) + tm_polygons() + 
  tm_shape(rent_2014_2019) + tm_symbols(col = "year", border.col = "white",
                                       size = "year") +
  tm_facets(by = "year", nrow = 5, free.coords = FALSE)

# Set the projection 
proj4string(neighborhoods) <- CRS("+init=EPSG:25833")
proj4string(year2014) <- CRS("+init=EPSG:25833")
proj4string(year2015) <- CRS("+init=EPSG:25833")
proj4string(year2016) <- CRS("+init=EPSG:25833")
proj4string(year2017) <- CRS("+init=EPSG:25833")
proj4string(year2018) <- CRS("+init=EPSG:25833")
proj4string(year2019) <- CRS("+init=EPSG:25833")

#plot neighborhood
plot(st_geometry(neighborhood))
plot(neighborhood["neighbourh"])

# maps
tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey9") + tm_borders(col="grey100") + 
tm_shape(year2014) + tm_dots(col = "red", scale = 0.5)
 # maps
tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey9") + tm_borders(col="grey100") +
  tm_shape(year2015) + tm_dots(col = "blue", scale = 0.5)
 # maps
tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey9") + tm_borders(col="grey100") +
tm_shape(year2016) + tm_dots(col = "green", scale = 0.5)
 # maps
tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey9") + tm_borders(col="grey100") +
tm_shape(year2017) + tm_dots(col = "blue", scale = 0.5)
 # maps
tm_shapeape(neighborhoods) + tm_fill(alpha=.3, col = "grey9") + tm_borders(col="grey100") +
tm_shape(year2018) + tm_dots(col = "red", scale = 0.5)
 # maps
tm_shape(neighborhoods) + tm_fill(alpha=.3, col = "grey9") + tm_borders(col="grey100") +
tm_shape(year2019) + tm_dots(col = "black", scale = 0.5)



#number of rents per year
number_rent_year<-dplyr::count(group_by(df_all, year))
ggplot(data = number_rent_year, aes( x = year, y =n) ) +
  labs(title="Rental offers in Copenhagen between 2014-2018",  x ="Year", y = "Number of listings")+
  ylim(0,15000)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar( stat = 'identity' ,position = 'dodge')+ 
  theme(plot.title = element_text(hjust = 0.5))

#number of listings per year per month
number_rent_year_month<-dplyr::count(group_by(df_all,year,month))
number_rent_year_month1<-year2014 <- rents[rents$year == '2017' & rents$month=='7',]
nrow(number_rent_year_month1)

#Number of listing over months in CPH, 2014-2018
number_rent_year_month$month <- factor(number_rent_year_month$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))
ggplot(data = number_rent_year_month, aes( x = year, y =n, fill=month, order =as.numeric(month))) +
  ylim(0,1700)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title=" Number of listing over months in CPH, 2014-2018",  x ="Year", y = "Number of listings")+
  geom_bar( stat = 'identity', position = 'dodge')


#Number of  rooms in rental offers per year
number_room_f<-dplyr::count(group_by(df_all,rooms, year)%>%filter(rooms==1|rooms==2|rooms==3|rooms==4|rooms==5))
ggplot(data = number_room_f, aes( x =year, y =n, fill = rooms) ) +
  ylim(0,5000)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Number of bedrooms in rental offers in Copenhagen, 2014-2018 ",  x ="Year", y = "Number of listings")+
  geom_bar(stat = 'identity' ,position = 'dodge')


aggregate(df_all[,12], list(df_all$year), mean)

#filter number of room per month
number_room<-dplyr::count(group_by(df_all,year,month))
number_room
number_room$month <- factor(number_room$month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))
ggplot(data = number_room, aes( x = year, y= n, fill = month))+
  ylim(0,1300)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="number of rents per a year",  x ="Year(month)", y = "rentM2")+
  geom_bar( stat = 'identity' ,position = 'dodge')


number_room<-dplyr::count(group_by(df_all,year,rentM2))
number_room
ggplot(data = number_room, aes( x = year, y=rentM2, fill = year))+
  labs(title="number of rents per a year",  x ="Year(month)", y = "rentM2")+
  geom_bar( stat = 'identity' ,position = 'dodge')


#---------Summary----------------------------------------
#summary view for all rent data
summary(df_year2014[,sapply(df_year2014[,1:14],typeof) == "double"])
summary(df_year2015[,sapply(df_year2015[,1:14],typeof) == "double"])
summary(df_year2016[,sapply(df_year2016[,1:14],typeof) == "double"])
summary(df_year2017[,sapply(df_year2017[,1:14],typeof) == "double"])
summary(df_year2018[,sapply(df_year2018[,1:14],typeof) == "double"])
summary(df_year2019[,sapply(df_year2019[,1:14],typeof) == "double"])

histogram(~df_all$rent|df_all,breaks=5)

#-----------------------------------------------------------------------------------
all.sf.point <- st_as_sf(x = df_all, 
                         coords = c("latitude", "longitude"))
year2014.sf.point <- st_as_sf(x = year2014, 
                              coords = c("latitude", "longitude"))
year2015.sf.point <- st_as_sf(x = year2015, 
                              coords = c("latitude", "longitude"))
year2016.sf.point <- st_as_sf(x = year2016, 
                              coords = c("latitude", "longitude"))
year2017.sf.point <- st_as_sf(x = year2017, 
                              coords = c("latitude", "longitude"))
year2018.sf.point <- st_as_sf(x = year2018, 
                              coords = c("latitude", "longitude"))
year2019.sf.point <- st_as_sf(x = year2019, 
                             coords = c("latitude", "longitude"))


data = as.data.frame(df_all)
data$rooms= as.numeric(data$rooms)
data = as.matrix(data) 
typeof(data$rooms)


df_all <- rbind(df_year2014, df_year2015, df_year2016 ,df_year2017,df_year2018,df_year2019)
df_all %>%mutate_all(as.numeric)


df[] <- lapply(df, as.numeric)

coor <- df_all[,c("rentM2","rent","size",)]
library(corrplot)
corrplot(cor(coor),method="circle")



