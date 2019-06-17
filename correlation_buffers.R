# --------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(ggthemes)
library(rgdal)
library(sf)
library(dplyr)
library(mosaic)
library(quantmod)
library(Hmisc)

# -----reads rental data------------
rental_data <- read.csv('LastBuffer1250.csv')

# ------------table index begin
# reviewR = reviews for Rooms
# reviewE = reviews for Entire Appartment
# pm2y1 = average rent/m2 in the buffer for the next year
# pm2y2 = average rent/m2 in the buffer the year after
# 
# For the rm2 ones:
#   - ignore rm2 - it is leftovers from the code i reused 
# - the next letter reffers to R(oom) or E(ntire appartment)
# - The rest is criterias:
#   RAH: where hostlistin > 1 and activemont > 5
# RH: where hostlistin > 1
# RA: where activemont > 5

# pchange1y = percentage rent/m2 change between this rental and pm2y1 
# pchange2y = percentage rent/m2 change between this rental and pm2y2
# ------------table index end

rental_data$reviewr_pop <- rental_data$reviewr/ rental_data$population #reviews of rooms

rental_data$reviewe_pop <- rental_data$reviewe/ rental_data$population #reviews of apartments


rental_data$rm2era_pop <- rental_data$rm2era/ rental_data$population #reviews of apartments with activity> 5 months

rental_data$rm2rrah_pop <- rental_data$rm2rrah/ rental_data$population #reviews of rooms with activity > 5 months and multihosts

rental_data$rm2rrh_pop <- rental_data$rm2rrh/ rental_data$population #reviews of rooms with multihosts

rental_data$rm2rra_pop <- rental_data$rm2rra/ rental_data$population #reviews of rooms with activity> 5 months

rental_data$rm2erah_pop <- rental_data$rm2erah/ rental_data$population #reviews of apartments with activity > 5 months and multihosts

rental_data$rm2erh_pop <- rental_data$rm2erh/ rental_data$population #reviews of apartments with multihosts




# pmat : matrix of the correlation p-values - function which tranforms the matrix and puts everything into rows
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# choose 2016 and 2017 
copenhagen <- rental_data[!(rental_data$year == "2018" | rental_data$year == "2015" |rental_data$year == "2014"),]

#choose columns created in lines 34-49
my_data <- copenhagen[, c(12,42,43,44,45,46,47,38,39,40,41)]


#do the speaman's correlation for Copenhagen
res2<-rcorr(as.matrix(my_data[,2:10]),type="spearman")


#choose years - in this case 2016,2017
year <- unique(copenhagen$year)
for(d in seq_along(year)) { 
  
  my_data <- copenhagen[, c(12,42,43,44,45,46,47,38,39,40,41)]
  testData <- my_data[(my_data$year == year[d] ),]
  
  
  
  res2<-rcorr(as.matrix(testData[,2:11]),type="spearman")
  results <- print(flattenCorrMatrix(res2$r, res2$P)) 
  df <- cbind(year[d],results)
  write.table(df  , file = "cor_copen_1250.csv",row.names=FALSE,col.names = TRUE,
              append=TRUE,sep = ";") #be careful with append 
 
  
  
}










#use function mentioned in line 55
cormatrix_cop <- flattenCorrMatrix(res2$r, res2$P) 

# #write it in csv
#  write.table(cormatrix_cop  , file = "cor_copen_1000.csv",row.names=FALSE,col.names = TRUE,
#              append=TRUE,sep = ";")

#neighbouthood analysis

#choose names of neighbourhoods
neighborhood <- unique(copenhagen$neighbourh)
#choose years - in this case 2016,2017
year <- unique(copenhagen$year)


# run the spearman's correlation for each neighbourhood and year and print it to csv
for (i in seq_along(neighborhood)){
  
  neighborhood_f<- filter(copenhagen, (neighbourh==neighborhood[i]))   
  
  for(j in seq_along(year)) { 
      
            
            my_data <- neighborhood_f[, c(12,42,43,44,45,46,47,38,39,40,41)]
            testData <- my_data[(my_data$year == year[j] ),]
            
           
            
            res2<-rcorr(as.matrix(testData[,2:11]),type="spearman")
            results <- print(flattenCorrMatrix(res2$r, res2$P)) 
            df <- cbind(neighborhood[i],year[j],results)
             write.table(df  , file = "cor_nei_1250.csv",row.names=FALSE,col.names = TRUE,
                       append=TRUE,sep = ";") #be careful with append
            
       
  }


}

