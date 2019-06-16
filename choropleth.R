# --------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)

library(ggthemes)
library(rgdal)
library(sf)

neighborhood.sp <- readOGR("neighborhood.shp", 
                 layer = "neighborhood")


# ---------------------------



neighborhood.sf <- st_read("neighborhood.shp")


neighborhood <- st_transform(neighborhood.sf, 4326)


#read june 2016-------------------------
june2016.sp <- readOGR("june2016.shp", 
                           layer = "june2016")


june2016.sf <- st_read("june2016.shp")


june2016 <- st_transform(june2016.sf, 4326)

# ---------------------------
#read houses #https://www.dst.dk/en/Statistik/dokumentation/documentationofstatistics/census-of-housing
houses.sp <- readOGR("bygningb.shp", 
                       layer = "bygningb")


houses.sf <- st_read("bygningb.shp")


houses <- st_transform(houses.sf, 4326)


plot(houses)



library(dplyr)

library(tidyverse)

#============================select apartments only===========================

findapartments <- june2016[, "room_type"== "Entire home/apt"]

findapartments = june2016[june2016$room_type == "Entire home/apt",]
class(findapartments)
apartments <- june2016[findapartments,]

#plot(apartments$geometry)

apart_out <- st_intersection(neighborhood, apartments)

apartment_per_neighbour <- dplyr::count(apart_out,apart_out$neighbou_1.1)

total_apartment <-st_join(neighborhood,apartment_per_neighbour)

st_area(neighborhood)

density_choropleth <- mutate(total_apartment,density=(apartment_per_neighbour$n/st_area(neighborhood)))

density_choropleth$density = as.numeric(density_choropleth$density)

ggplot() + 
  geom_sf(data=density_choropleth,aes(fill=density), lwd=0.1) +
  scale_fill_distiller(palette="OrRd", direction = 1) +
  ggtitle("Apartments per district") + 
  labs(fill = "Apartments") +
  theme_map()


#================ratio airbnb apartments and dwellings===============

houses_neighbor <- st_intersection(neighborhood, houses)

count_houses <- dplyr::count(houses_neighbor,houses = (houses_neighbor$neighbourh))

airbnb_and_houses <- st_join(total_apartment,count_houses)

ratio_housing_units <- mutate(airbnb_and_houses, ratio=(airbnb_and_houses$n.x/airbnb_and_houses$n.y))


ggplot() + 
  geom_sf(data=ratio_housing_units,aes(fill=ratio), lwd=0.1) +
  scale_fill_distiller(palette="OrRd", direction = 1) +
  ggtitle("Ratio housing units/airbnb apartments") + 
  labs(fill = "Ratio") +
  theme_map()

df_neighbor_ratio <- data_frame(ratio_housing_units$ratio,ratio_housing_units$neighbourh)
df_neighbor_ratio


#=========average price of apartments per district
avg_price <- june2016 %>%
              group_by(neighbou_1) %>% 
              summarise(average = mean(price))

avg_price

#----population and avg_price

plot(avg_price["average"])

avgprice_nei <- st_join(neighborhood,avg_price)

avglisting <- st_join(avgprice_nei,apartment_per_neighbour)
avgprice_listing <- mutate(avglisting, p_l= avglisting$average/avglisting$n)

ggplot() + 
  geom_sf(data=avgprice_nei,aes(fill=average), lwd=0.1) +
  scale_fill_distiller(palette="Spectral", direction = 1) +
  ggtitle("Average price of Airbnb apartments per district") + 
  labs(fill = "Average price") +
  theme_map()



ggplot() + 
  geom_sf(data=avgprice_listing,aes(fill=p_l), lwd=0.1) +
  scale_fill_distiller(palette="Spectral", direction = 1) +
  ggtitle("Average price of Airbnb apartments per district divided by number of listings") + 
  labs(fill = "Average price") +
  theme_map()


#============================select private rooms only (not shared)===========================

findrooms <- june2016[, "room_type"== "Private room"]

findrooms = june2016[june2016$room_type == "Private room",]
class(findrooms)
rooms <- june2016[findrooms,]

#plot(apartments$geometry)

room_out <- st_intersection(neighborhood, rooms)

rooms_per_neighbour <- dplyr::count(room_out,room_out$neighbou_1.1)

total_rooms <-st_join(neighborhood,rooms_per_neighbour)

ggplot() + 
  geom_sf(data=total_rooms,aes(fill=n), lwd=0.1) +
  scale_fill_distiller(palette="OrRd", direction = 1) +
  ggtitle("Rooms per district") + 
  labs(fill = "Rooms") +
  theme_map()




library(dplyr)



# ---------------------------

# interactive maps!
library(mapview)

# interactive leaflet map 
mapview(apart_out["reviews_pe"])
mapview(room_out["reviews_pe"])



# -----------multiple layers:
mapview(list(apart_out["reviews_pe"],apart_out))










#-----------reviews for apartments-------------------

apart_out

#clean NA values
find_apart_reviews <- apart_out[, "reviews_pe">= 0]
find_apart_reviews = apart_out[ !(is.na(apart_out$reviews_pe)),]


class(find_apart_reviews)
apartmentreviews <- apart_out[find_apart_reviews,]

#plot(apartments$geometry)

apart_review_out <- st_intersection(neighborhood, apartmentreviews)

apart_review_neighbour <- dplyr::count(apart_review_out,apart_review_out$neighbou_1.1)

total_apart_review <-st_join(neighborhood,apart_review_neighbour)



mapview(apartmentreviews["reviews_pe"])

#----------------------------corelation ONLY FOR ARARTMENTS-------------------
#correlation reviews and price 
#assumption price depends on reviews
#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
library("ggpubr")

#only for apartments


ggscatter(find_apart_reviews, x = "reviews_pe", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Reviews per yead", ylab = "Price")


# mpg
ggqqplot(find_apart_reviews$price, ylab = "price")
# wt
ggqqplot(find_apart_reviews$reviews_pe, ylab = "reviews_pe")



res <- cor.test(find_apart_reviews$price, find_apart_reviews$reviews_pe, 
                method = "pearson")
res

# Extract the p.value
res$p.value

# Extract the correlation coefficient
res$estimate #assumption is false; price doesn't depend on reviews





#correlation reviews and neighborhood 
#assumption price depends on neighborhood
#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
library("ggpubr")

#only for apartments
find_apart_reviews

#rank neighborhoods based on popularity
popularity <- dplyr::count(find_apart_reviews,neighbourh)

popular<-mutate(popularity,rank=(rank(popularity$n, na.last = TRUE))) 


#find average price for each neighborhood
mean(find_apart_reviews$price)

avg_price <-find_apart_reviews %>%
        group_by(neighbourh) %>%
        dplyr::summarize(Mean = mean(price, na.rm=TRUE))

popular<-mutate(popular,mean_price=(avg_price$Mean)) 

ggscatter(popular, x = "n", y = "mean_price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Count popularity", ylab = "Mean price")


# 
ggqqplot(popular$n, ylab = "Count popularity")
# 
ggqqplot(popular$mean_price, ylab = "Mean price")

#https://www.dummies.com/education/math/statistics/how-to-interpret-a-correlation-coefficient-r/

res <- cor.test(popular$n, popular$mean_price, 
                method = "pearson")
res 
#cor=0.52 

# Extract the p.value
res$p.value

# Extract the correlation coefficient
res$estimate #assumption is true; price  depends on neighborhood; moderate uphill (positive) relationship




