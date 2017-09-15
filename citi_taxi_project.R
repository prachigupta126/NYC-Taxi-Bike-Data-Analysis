
#Exploratory Geo-spatial analysis

##Reading the May 2016 citibike and yellow taxi NYC trip cleansed data
##Data cleansing was done using SAS JMP

##Taxi Trip distance was present in the database 
##For citibike, trip distance is calculated by the below function
##calculate the distance between pickup and drop off points of citi bike

library(sqldf)
#################################################################################
# Reading the files
citibike=read.csv("C:/Uconn MSBA/studies/Kaggle/data and code/research project/201605-citibike-tripdata/citibike_cleansed/citibike_cleansed.csv")
taxi= read.csv("C:/Uconn MSBA/studies/Kaggle/data and code/research project/original datasets/taxi_clean_format.csv")
###########################################################################
#Checking the data
head(citibike)
head(taxi)



###########################################################################
#Faeture Engineerig

#creating new column in citibike 
citibike$age = 2017 - citibike$birth_year
head(df_citibike)


##Taxi Trip distance was present in the database 
##For citibike, trip distance is calculated by the below function
##calculate the Haversine distance between pickup and drop off points of citi bike

earthDist <- function (lon1, lat1, lon2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- round(R * c* 0.62137119,2);
  return(d)
}



##adding a column "distance(in miles)" for the citi bike data
citibike$distance= earthDist(citibike$start.station.longitude, citibike$start.station.latitude,citibike$end.station.longitude, citibike$end.station.latitude)
###########################################################################
# Data cleaning process based on distance 
##Checking the count citibike trip distance is less than or equal to 0 (Trip didn't occur)

length(which(citibike$distance<=0.00))

##Checking the count of the citibike trip distance is greater than 5 miles 

length(which(citibike$distance>=5.00))

##Eliminated over 100000 records in over 1 mil records with the below code

for (i in 1:length(citibike$distance))
{
  temp = which(citibike$distance <=0.00 | citibike$distance > 5.00)
  citibike$distance[temp] = NA
  
}

##############################################################
#Change datetime format into month, date, year and time

# Converting teh date time to POSIXlt format
taxi$tpep_pickup_datetime = as.POSIXlt(strptime(taxi$tpep_pickup_datetime,format="%Y/%m/%d %I:%M:%S %p"))
taxi$tpep_dropoff_datetime =as.POSIXlt(strptime(taxi$tpep_dropoff_datetime,format="%Y/%m/%d %I:%M:%S %p"))

taxi$tpep_pickup_date = as.Date(taxi$tpep_pickup_datetime,format="%Y/%m/%d")
taxi$tpep_drop_date = as.Date(taxi$tpep_dropoff_datetime,format="%Y/%m/%d")

# Scraping the time from the date-time format
taxi$pickup_time = format(taxi$tpep_pickup_datetime,format="%H:%M:%S")
taxi$dropup_time = format(taxi$tpep_dropoff_datetime,format="%H:%M:%S")


# Checking if it is weekday or not 
taxi$pickup_day=weekdays(as.Date(taxi$tpep_pickup_date,'%d-%m-%Y'))

# Start hour of Taxi 
taxi$start_hour = format(taxi$tpep_pickup_datetime,format="%H")

# Visualisation of Taxi data
head(taxi)
tail(taxi)


# Grouping of the weekdays together 
taxi$weekeknd_ornot <- ifelse(taxi$pickup_day=='Sunday' | taxi$pickup_day=='Saturday',"Weekend","not-Weekend")

# group_by day
library(dplyr)
a=taxi %>% group_by(pickup_day)
a

##############################################################
# Creating a new data fraame with unique bike station id from the bike data


## Using the tapply function to get the longitude, latitude and bike station name 
## from the unique end bike station id of the citibike cleansed data
long=tapply(citi_clean$start.station.longitude, citi_clean$start.station.id, FUN=min)
lat =tapply(citi_clean$start.station.latitude, citi_clean$start.station.id, FUN=min)
##Storing it in dataframe df1
df1= as.data.frame( cbind(long,lat))

##Creating a new dataframe df2 with one column

df2=data.frame()[,1];

## Storing the unique station ID in the frame
df2$citi_clean$end.station.id= data.frame(unique(citi_clean$end.station.id));

##Storing the latitude, longitude and station name 

df2$citilat=df1$lat
df2$citilong=df1$long
df2$citistationname=df1$station_name

citibikedf <- df2

##Writing the unique citibike information into a CSV file 
## with columns bike_id, station_name, Longitude, Latitude

write.csv(Citibikedf,file="C:/Users/Karpagam/Desktop/Citibikeinfo.csv")


###########################################################################
#Visualisations

tapply(df_citibike$distance_covered , df_citibike$gender, median)

a= sqldf("select min(citi_clean$end.station.longitude) from citi_clean")

taxi_bike %>%
  group_by(Pickup.Day)




##########################################
##    find the minimum distance from the end point of the taxi rode
#  Haversine distance is used in this


for (i in 1:nrow(sample2)){
  
  sample2$minimum_distance_end[i] <- 1000
  
  
  
  for (j in 1:nrow(citi_unique)){
    
    temp=(earthDist(sample2$dropoff_longitude[i],sample2$dropoff_latitude[i],citi_unique$end.station.longitude[j],citi_unique$end.station.latitude[j]));
    
    
    if(temp< sample2$minimum_distance_end[i])
    {
      sample2$minimum_distance_end[i]=temp
      
      
      sample2$closest_station_end[i] = citi_unique$end.station.id[j]
      
    }
    
  }
  
}

# This program  gives correct result but since data is huge we need to go for other implementaion
# So we went for Kd tree approach
