
library(sp)
install.packages("rgdal");
library(rgdal)
install.packages("RANN")

library(RANN)


#####################################################
##Bike_id is created to store the unique citi bike station IDs
##Extracting the latitude, longitude of bike stations and storing it in the bike dataframe
bikedf=read.csv("C:/Uconn MSBA/studies/Kaggle/data and code/research project/unique_citibike_data/Citibikeinfo.csv")

Bike_id<- bikedf$end.station.id;
lat <- bikedf$end.station.latitude;
long <- bikedf$end.station.longitude;

bike <- data.frame(Bike_id,lat,long);
#####################################################
##Reading the NYC taxi data
taxidf=read.csv("C:/Uconn MSBA/studies/Kaggle/data and code/research project/taxi/tripdata_cleansed.csv");
head(taxidf)


###################################################

#To check missing value 
a=sapply(taxidf, function(x) sum(is.na(x)))
a
#


###################################################
taxidf$X= seq(1:nrow(taxidf));
taxi_id <-taxidf$X;
taxi_lat <-taxidf$pickup_latitude;
taxi_long <-taxidf$pickup_longitude;

##Extracting the latitude, longitude and the newly created taxi ID and storing it in
##taxich dataframe
taxich <- data.frame(taxi_id,taxi_lat,taxi_long)

#####################################################


##Checking the NYC coordinates and subsetting the taxich dataframe to eliminate longitude in the below range

taxich2 <- subset(taxich, taxich$taxi_long > '-70' & taxich$taxi_long < '-76');


## convert the taxi lat/long coordinates to UTM for the computed zone

long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

#################################################################################
## Assuming that all points are within a zone (within 6 degrees in longitude),
## we use the first taxi's longitude to get the zone.
z <- long2UTM(taxich[1,"taxi_long"])

z;


#################################################################################
## convert the bike lat/long coordinates to UTM for the computed zone
## using the other Josh O'Brien linked answer
bike2 <- bike
coordinates(bike2) <- c("long", "lat")
proj4string(bike2) <- CRS("+proj=longlat +datum=WGS84")

bike.xy <- spTransform(bike2, CRS(paste0("+proj=utm +zone=",z," ellps=WGS84")))
#################################################################################
## convert the Taxi lat/long coordinates to UTM for the computed zone
taxich4 <- taxich2
nrow(taxich4)
coordinates(taxich4) <- c("taxi_long", "taxi_lat")
proj4string(taxich4) <- CRS("+proj=longlat +datum=WGS84")

taxich2.xy <- spTransform(taxich4, CRS(paste0("+proj=utm +zone=",z," ellps=WGS84")))
nrow(taxich2.xy)

###################################################################
## find the nearest neighbor in Taxi.xy@coords for each bike.xy@coords


res <- nn2(data=bike.xy@coords,query=taxich2.xy@coords, k=1)

res1<- as.data.frame(res);
 nrow(res1)      


## res$nn.dist is a vector of the distance to the nearest bike.xy@coords for each Taxi.xy@coords
## res$nn.idx is a vector of indices to bus.xy of the nearest bike.xy@coords for each taxi.xy@coords

###################################################################
taxich2$near_drop_bike_station_ID <- ifelse(res$nn.dists <= 1000, bike[res1$nn.idx,"Bike_id"], NA)


taxich2$near_drop_distance <- ifelse(res$nn.dists<=1000, res1$nn.dists*0.00062137119223733 ,NA)

##taxich$count <- count;

##count <- cbind(taxich,rowSums(res$nn.idx > 0) - 1)
##remove(taxich);



###################################################################
#me=merge(taxich2,taxidf,by="taxi_id")

#writing teh file
write.csv(taxich2,"C:/Uconn MSBA/studies/Kaggle/data and code/research project/original datasets/all_neighbour.csv")


