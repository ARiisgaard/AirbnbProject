
#======================AIRBNB ==========================================
# ======================================================================================
#   ============================================================================================
# --------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)

library(ggthemes)
library(rgdal)
library(sf)
library(dplyr)
library(mosaic)
library(quantmod)
library(plyr)
library(dplyr)
library(mosaic)

prices.sp <- readOGR("final_airbnb_nei.shp", 
                     layer = "final_airbnb_nei")


prices.sf <- st_read("final_airbnb_nei.shp")


prices <- st_transform(prices.sf, 4326)

plot(prices$geometry)
listing_type<-dplyr::count(group_by(prices,roomtype))


#room type for all the dataset
listingtype_plot <-  ggplot(data = listing_type, aes( x = factor( roomtype ), y =n, fill = roomtype ) ) +    # print bar chart
    geom_bar( stat = 'identity' ,position = 'dodge') + scale_fill_brewer(palette="Dark2")          # add a density estimate with defaults

listingtype_plot + geom_text(aes(label = listing_type$n), position = position_dodge(0.9),vjust = -0.5)+
  labs(title = "Listing types in Airbnb dataset for years 2015- 2018") +  #, tag = "A") +
  labs(x = "Type of listings", y = "Number") +
  labs(caption = "(based on data from Tom Slee and Inside Airbnb)") + 
  labs(fill = "Listing type") +
  theme_classic(base_size = 12, base_family = "serif")

#ggsave("listingtype_plot2.png")

prices$d_aug2018
class(as.Date(prices$d_aug2018))


# for apartments only

findapartments <- prices[, "roomtype"== "Private room"]

findapartments = prices[prices$roomtype == "Private room",]
class(findapartments)
apartments <- prices[findapartments,]


counts = (sum(!is.na(apartments$d_feb2015))) 
counts <- counts


#general counts of apartments for each district for year 2015
feb2015<-subset(apartments, !is.na(apartments$d_feb2015))

febcount2015 <- feb2015 %>% group_by(neighbou_1) %>% tally() #right

df_allcount2015 <- data.frame(febcount2015)


listingscount2015 <- mutate(df_allcount2015,final_count= df_allcount2015$n)
listingscount2015 <- mutate(listingscount2015, year=as.Date("2015", format= "%Y"))
#general counts of apartments for each district for year 2016

jan2016 <- subset(apartments, !is.na(apartments$d_jan2016))
june2016 <- subset(apartments, !is.na(apartments$d_june2016))
oct2016 <- subset(apartments, !is.na(apartments$d_oct2016))

jancount2016 <- jan2016 %>% group_by(neighbou_1) %>% tally()
junecount2016 <- june2016 %>% group_by(neighbou_1) %>% tally()
octcount2016 <- oct2016 %>% group_by(neighbou_1) %>% tally()

df_allcount2016 <- data.frame(jancount2016,junecount2016,octcount2016)
listingscount2016 <- mutate(df_allcount2016, final_count = as.integer((df_allcount2016$n+df_allcount2016$n.1+df_allcount2016$n.2)/3))

listingscount2016 <- mutate(listingscount2016, year=as.Date("2016", format= "%Y"))
#general counts of apartments for each district for year 2017
 
jan2017 <- subset(apartments, !is.na(apartments$d_jan2017))
jancount2017 <- jan2017 %>% group_by(neighbou_1) %>% tally()

feb2017 <- subset(apartments, !is.na(apartments$d_feb2017))
febcount2017 <- feb2017 %>% group_by(neighbou_1) %>% tally()

mar2017 <- subset(apartments, !is.na(apartments$d_mar2017))
marcount2017 <- mar2017 %>% group_by(neighbou_1) %>% tally()

apr2017 <- subset(apartments, !is.na(apartments$d_apr2017))
aprcount2017 <- apr2017 %>% group_by(neighbou_1) %>% tally()

june2017 <- subset(apartments, !is.na(apartments$d_june2017))
junecount2017 <- june2017 %>% group_by(neighbou_1) %>% tally()

df_allcount2017 <- data.frame(jancount2017,febcount2017,marcount2017,aprcount2017,junecount2017)
listingscount2017 <- mutate(df_allcount2017, final_count = as.integer((df_allcount2017$n+
                                                                         df_allcount2017$n.1+
                                                                         df_allcount2017$n.2+
                                                                         df_allcount2017$n.3+
                                                                         df_allcount2017$n.4)/5))

listingscount2017 <- mutate(listingscount2017, year=as.Date("2017", format= "%Y"))
#general counts of apartments for each district for year 2018

apr2018 <- subset(apartments, !is.na(apartments$d_apr2018))
aprcount2018 <- apr2018 %>% group_by(neighbou_1) %>% tally()

july2018 <- subset(apartments, !is.na(apartments$d_july2018))
julycount2018 <- july2018 %>% group_by(neighbou_1) %>% tally()

aug2018 <- subset(apartments, !is.na(apartments$d_aug2018))
augcount2018 <- aug2018 %>% group_by(neighbou_1) %>% tally()

sep2018 <- subset(apartments, !is.na(apartments$d_sep2018))
sepcount2018 <- sep2018 %>% group_by(neighbou_1) %>% tally()

oct2018 <- subset(apartments, !is.na(apartments$d_oct2018))
octcount2018 <- oct2018 %>% group_by(neighbou_1) %>% tally()

nov2018 <- subset(apartments, !is.na(apartments$d_nov2018))
novcount2018 <- nov2018 %>% group_by(neighbou_1) %>% tally()

dec2018 <- subset(apartments, !is.na(apartments$d_dec2018))
deccount2018 <- dec2018 %>% group_by(neighbou_1) %>% tally()

df_allcount2018 <- data.frame(aprcount2018,julycount2018,augcount2018,sepcount2018,octcount2018,novcount2018,deccount2018)

listingscount2018 <- mutate(df_allcount2018, final_count = as.integer((df_allcount2018$n + 
                              df_allcount2018$n.1 + 
                              df_allcount2018$n.2 + 
                              df_allcount2018$n.3 + 
                              df_allcount2018$n.4 + 
                              df_allcount2018$n.5 + 
                              df_allcount2018$n.6)/7) )

listingscount2018 <- mutate(listingscount2018, year=as.Date("2018", format= "%Y"))

listingscount2018 <- mutate(listingscount2018, years2=2018)
listingscount2016 <- mutate(listingscount2016, years2=2016)
listingscount2017 <- mutate(listingscount2017, years2=2017)
listingscount2015 <- mutate(listingscount2015, years2=2015)
# counts for every year for each district
df_allyears <- bind_rows(listingscount2015,listingscount2016,listingscount2017,listingscount2018)


neigh_years <- ggplot(data = df_allyears, aes( x = factor( years2 ), y =final_count, fill = neighbou_1 ) ) +    # print bar chart
    geom_bar( stat = 'identity' ,position = 'dodge')    

# listingtype_plot <-  ggplot(data = listing_type, aes( x = factor( roomtype ), y =n, fill = roomtype ) ) +    # print bar chart
#   geom_bar( stat = 'identity' ,position = 'dodge')    
neigh_years

neigh_years +
  labs(title = "Number of apartments in each neighbourhood in Airbnb dataset for years 2015- 2018") +  #, tag = "A") +
  labs(x = "Year", y = "Number") +
  labs(caption = "(based on data from Tom Slee and Inside Airbnb)") + 
  labs(fill = "Neighbourhood") +
  theme_classic(base_size = 12, base_family = "serif")


#ggsave('number_apart_neigh.png')

all_needed <- data.frame(neighbou_1=(df_allyears$neighbou_1), year=df_allyears$year,final_count= df_allyears$final_count )



library(stats)

library(dplyr)
# seek for counts for each neighborhood and for each year to find the percentage difference
mydata <- all_needed %>%
  arrange(
    neighbou_1,
    final_count,
    desc(year)
  )
mydata
# choropleths


#read the neighborhoods

neighborhoods.sp <- readOGR("Neighbourh_pop.shp", 
                           layer = "Neighbourh_pop")


neighborhoods.sf <- st_read("Neighbourh_pop.shp")


neighborhoods <- st_transform(neighborhoods.sf, 4326)

plot(neighborhoods$geometry)
plot(apartments$geometry)


findapartments_rooms <- prices[, "roomtype"== "Entire home/apt" | "roomtype"== "Private room"]

findapartments_rooms = prices[prices$roomtype == "Private room" | prices$roomtype == "Entire home/apt" ,]
findapartments_rooms

june2016_no_na <- subset(findapartments_rooms, !(is.na(findapartments_rooms$r_june2016)))
  
library(tmap)

  
tmap2016 <-   tm_shape(neighborhoods) +
     tm_borders(col = NA, lwd = 1, lty = "solid", alpha = NA,
                group = NA)+
    tm_fill(col = "ivory") +
    tm_shape(june2016_no_na) + 
     tm_dots("roomtype",size=0.08,  palette=c(A='darkblue', B='red', C='darkseagreen4')) +
     tm_layout(title="June 2016")
tmap2016

#tmap_save(tmap2016, "tmap2016.png", height=7)
 june2017_no_na <- subset(findapartments_rooms, !(is.na(findapartments_rooms$r_june2017)))
 
 tmap2017<-     tm_shape(neighborhoods) +
       tm_borders(col = NA, lwd = 1, lty = "solid", alpha = NA,
                  group = NA)+
       tm_fill(col = "ivory") +
       tm_shape(june2017_no_na) + 
       tm_dots("roomtype",size=0.08,  palette=c(A='darkblue', B='red', C='darkseagreen4')) +
       tm_layout(title="June 2017")
 tmap2017

 # tmap_save(tmap2017, "tmap2017.png", height=7)
 
 
 july2018_no_na <- subset(findapartments_rooms, !(is.na(findapartments_rooms$r_july2018)))
 
tmap2018 <-     tm_shape(neighborhoods) +
           tm_borders(col = NA, lwd = 1, lty = "solid", alpha = NA,
                      group = NA)+
           tm_fill(col = "ivory") +
           tm_shape(july2018_no_na) + 
           tm_dots("roomtype",size=0.08,  palette=c(A='darkblue', B='red', C='darkseagreen4')) +
           tm_layout(title="July 2018")
tmap2018
tmap_save(tmap2018, "tmap2018.png", height=7)
 #============================for apartments only===========================

listingscount2015 <- st_sf(listingscount2015)
listingscount2016 <- st_sf(listingscount2016)
listingscount2017 <-st_sf(listingscount2017)
listingscount2018 <-st_sf(listingscount2018)


st_area(neighborhoods) # in m^2 !!!

# for 2015

total_apartment2015 <-st_join(neighborhoods,listingscount2015)

density_choropleth2015 <- mutate(total_apartment2015,density=(total_apartment2015$final_count/Population))

density_choropleth2015$density = as.numeric(density_choropleth2015$density)
library(ggthemes)
density2015<-   ggplot() + 
                geom_sf(data=density_choropleth2015,aes(fill=density), lwd=0.1) +
                scale_fill_distiller(palette="Blues", direction = 1) + #, tag = "A") +
                labs(fill = "Density of apartments 
(For 1 month in 2015)") +
                theme_map(base_size = 12)
density2015

#for 2016

total_apartment2016 <-st_join(neighborhoods,listingscount2016)

density_choropleth2016 <- mutate(total_apartment2016,density=(final_count/Population))

density_choropleth2016$density = as.numeric(density_choropleth2016$density)

density2016<-   ggplot() + 
  geom_sf(data=density_choropleth2016,aes(fill=density), lwd=0.1) +
  scale_fill_distiller(palette="Blues", direction = 1) +
  labs(fill = "Density of apartments 
(For 3 months in 2016)") +
  theme_map(base_size = 12)
density2016

#for 2017 

total_apartment2017 <-st_join(neighborhoods,listingscount2017)

density_choropleth2017 <- mutate(total_apartment2017,density=(final_count/Population))

density_choropleth2017$density = as.numeric(density_choropleth2017$density)

density2017<-   ggplot() + 
  geom_sf(data=density_choropleth2017,aes(fill=density), lwd=0.1) +
  scale_fill_distiller(palette="Blues", direction = 1) +
  labs(fill = "Density of apartments 
(For 5 months in 2017)") +
  theme_map(base_size = 12) 
density2017

# for 2018

total_apartment2018 <-st_join(neighborhoods,listingscount2018)

density_choropleth2018 <- mutate(total_apartment2018 ,density=(final_count/Population))

density_choropleth2018$density = as.numeric(density_choropleth2018$density)

density2018<-   ggplot() + 
  geom_sf(data=density_choropleth2018,aes(fill=density), lwd=0.1) +
  scale_fill_distiller(palette="Blues", direction = 1) +
  labs(fill = "Density of apartments 
(For 7 months in 2018)") +
  theme_map(base_size = 12) 
density2018

library(cowplot)
plot_grid(density2015, density2018,align = "h",nrow = 2, labels="Density of Airbnb apartments for each neighborhood",
          label_y = 1, vjust = 1, hjust=-0.3,label_fontfamily="serif")

dd2018 <- data.frame(density= density_choropleth2018$density,year= density_choropleth2018$years2, geom=density_choropleth2018$geometry)
dd2018 <- st_as_sf(dd2018)

dd2017 <- data.frame(density= density_choropleth2017$density,year= density_choropleth2017$years2,geom=density_choropleth2017$geometry)
dd2017<- st_as_sf(dd2017)
dd2016 <- data.frame(density= density_choropleth2016$density,year= density_choropleth2016$years2,geom=density_choropleth2016$geometry)
dd2016 <- st_as_sf(dd2016)
dd2015 <- data.frame(density= density_choropleth2015$density,year= density_choropleth2015$years2,geom=density_choropleth2015$geometry)
dd2015 <- st_as_sf(dd2015)
all_years_density <- rbind(dd2018,dd2017,dd2016,dd2015)
#all_years_density<-data.frame(density_choropleth2018,density_choropleth2017,density_choropleth2016,density_choropleth2015)
# df_allyears <- data.frame(d_2018= density_choropleth2018$density, d_2017= density_choropleth2017$density, d_2016= density_choropleth2016$density,
#                 d_2015= density_choropleth2015$density,year2018= density_choropleth2018$years2,year2017= density_choropleth2017$years2, year2016= density_choropleth2016$years2,
#                 year2015= density_choropleth2015$years2)

gg <-  ggplot() + 
    geom_sf(data=all_years_density,aes(fill=density), lwd=0.1) +
    scale_fill_distiller(palette="Blues", direction = 1,name = "Density in neighbourhoods 
(Listings of apartments/Population)") +
  theme_map(base_size = 14, base_family = "serif")
# Use vars() to supply faceting variables:
gg+ facet_wrap(vars(year))
#ggsave("pop_density.png")

# mean price per neighborhood
#2015
mean2015 <- mean((p_feb2015) ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
df_mean2015 <- data.frame(mean2015)
mean2015 <- mutate(mean2015, mean_price=as.numeric(mean2015$mean))

#2016
mean_jan2016 <- mean(p_jan2016 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_june2016 <- mean(p_june2016 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_oct2016 <- mean(p_oct2016 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")

df_mean2016 <- data.frame(mean_jan2016,mean_june2016,mean_oct2016)
mean2016 <- mutate(df_mean2016, mean_price= as.numeric(as.numeric(mean) + as.numeric(mean.1) + as.numeric(mean.2))/3)

#2017
mean_jan2017 <- mean(p_jan2017 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_feb2017 <- mean(p_feb2017 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_mar2017 <- mean(p_mar2017 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_apr2017 <- mean(p_apr2017 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_june2017 <- mean(p_june2017 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")

df_mean2017 <- data.frame(mean_jan2017,mean_feb2017,mean_mar2017,mean_apr2017,mean_june2017)

mean2017 <- mutate(df_mean2017, mean_price= as.numeric(as.numeric(mean) + as.numeric(mean.1) + as.numeric(mean.2) +
                                              as.numeric(mean.3) +  as.numeric(mean.4))/5)

#2018
mean_apr2018 <- mean(p_apr2018 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_july2018 <- mean(p_july2018 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_aug2018 <- mean(p_aug2018 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_sep2018 <- mean(p_sep2018 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_oct2018 <- mean(p_oct2018 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_nov2018 <- mean(p_nov2018 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")
mean_dec2018 <- mean(p_dec2018 ~ neighbou_1 , na.rm=TRUE, data = prices,.format = "table")

df_mean2018 <- data.frame(mean_apr2018,mean_july2018,mean_aug2018, mean_sep2018,mean_oct2018,mean_nov2018,mean_dec2018)
mean2018 <- mutate(df_mean2018, mean_price= as.numeric(as.numeric(mean) + as.numeric(mean.1) + as.numeric(mean.2) +
                                               as.numeric(mean.3) +  as.numeric(mean.4) + as.numeric(mean.5)+
                                               as.numeric(mean.6))/7)

#percentage change 
mean2015 <- mutate(mean2015, year=as.Date("01.01.2015", format= "%d.%m.%Y"))
mean2016 <- mutate(mean2016, year=as.Date("01.01.2016", format= "%d.%m.%Y"))
mean2017 <- mutate(mean2017, year=as.Date("01.01.2017", format= "%d.%m.%Y"))
mean2018 <- mutate(mean2018, year=as.Date("01.01.2018", format= "%d.%m.%Y"))

mean2015 <- mutate(mean2015, years=2015)
mean2016 <- mutate(mean2016, years=2016)
mean2017 <- mutate(mean2017, years=2017)
mean2018 <- mutate(mean2018, years=2018)

all_prices<-bind_rows(mean2015,mean2016,mean2017,mean2018)
prices_needed <- data.frame(neighbourh=(all_prices$neighbou_1),mean_price=(all_prices$mean_price),year= as.Date(all_prices$year, "%d.%m.%Y"))
library(stats)
library(base)
library(dplyr)
ggplot(data = all_prices, aes(x=year, y=mean_price)) + geom_line(aes(colour=neighbou_1), size=2)+ xlab("Year") + ylab("Average price") +
  labs(title = "Average price of apartments in neighborhoods") + theme_classic(base_size = 16) +labs(colour="Neighborhood") 

avg_pricenei <- ggplot(data = all_prices, aes( x = factor( years ), y =mean_price, fill = neighbou_1 ) ) +    # print bar chart
        geom_bar( stat = 'identity' ,position = 'dodge') + scale_fill_brewer(palette="Spectral")+
  

avg_pricenei +
  labs(title = "Average price of apartments in each neighbourhood in Airbnb dataset for years 2015- 2018") +  #, tag = "A") +
  labs(x = "Year", y = "Average price (in DKK)") +
  labs(caption = "(based on data from Tom Slee and Inside Airbnb)") + 
  labs(fill = "Neighbourhood") +
  theme_classic(base_size = 12, base_family = "serif")

#ggsave("avg_price2.png")
library(dplyr)
# seek for counts for each neighborhood and for each year to find the percentage difference
mydata2 <- prices_needed %>%
  arrange(
    neighbourh,
    mean_price,
    desc(year)
  )
mydata2



# map the prices
neighborhoods_prices<-merge(neighborhoods,prices_needed, all=TRUE)

#for 2015
prices2015 <- filter(neighborhoods_prices, year=="2015-01-01")

pprices2015<-   ggplot() + 
  geom_sf(data=prices2015,aes(fill=mean_price), lwd=0.1) +
  scale_fill_distiller(palette="OrRd", direction = 1) +
  ggtitle("2015") + 
  labs(fill = "Apartments") +
  theme_map()
pprices2015

#for 2016

prices2016 <- filter(neighborhoods_prices, year=="2016-01-01")

pprices2016<-   ggplot() + 
  geom_sf(data=prices2016,aes(fill=mean_price), lwd=0.1) +
  scale_fill_distiller(palette="OrRd", direction = 1) +
  ggtitle("2016") + 
  labs(fill = "Apartments") +
  theme_map()
pprices2016

#for 2017

prices2017 <- filter(neighborhoods_prices, year=="2017-01-01")

pprices2017<-   ggplot() + 
  geom_sf(data=prices2017,aes(fill=mean_price), lwd=0.1) +
  scale_fill_distiller(palette="OrRd", direction = 1) +
  ggtitle("2017") + 
  labs(fill = "Apartments") +
  theme_map()
pprices2017

#for 2018

prices2018 <- filter(neighborhoods_prices, year=="2018-01-01")

pprices2018<-   ggplot() + 
  geom_sf(data=prices2018,aes(fill=mean_price), lwd=0.1) +
  scale_fill_distiller(palette="OrRd", direction = 1) +
  ggtitle("2018") + 
  labs(fill = "Apartments") +
  theme_map()
pprices2018

library(cowplot)
plot_grid(pprices2015, pprices2016,pprices2017,pprices2018, labels = "Average price of apartments for each neighbourhood")

library("ggpubr")
# maybe some correlation
library(cars)
ggscatter(prices, x = "ajan16", y = "p_jan2016", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "prices", ylab = "activity")

library(car)
scatterplot(  prices$p_jan2016~prices$ajan16)

scatterplot(  prices$adec18~prices$p_dec2018)

ggscatter(prices, x = "ajun16", y = "p_june2016", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "prices", ylab = "activity")

ggscatter(prices, x = "adec18", y = "p_dec2018", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "prices", ylab = "activity")
# 
ggqqplot(popular$n, ylab = "Count popularity")
# 
ggqqplot(popular$mean_price, ylab = "Mean price")

