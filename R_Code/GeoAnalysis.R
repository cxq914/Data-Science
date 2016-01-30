library(XML)
library(bitops)
library(RCurl)
library(sp)
library(geosphere)
library(osmar)
final.data <- read.xlsx('Applebee Data/Result/Historical Data( 2015-11-23 - 2016-01-03 ).xlsx',detectDates = TRUE)

rest.dat <- unique(final.data[,c(1,3,23)])


rest.add <- read.xlsx('Restaurant Addresses.xlsx')

restaurant <- merge(rest.dat,rest.add,by.x = 'Restaurant.Code',by.y='Code')

zip.info <- read.xlsx('Census Data by Zip.xlsx')

str(zip.info)

zip.info$GEO.id2 <- as.character(zip.info$GEO.id2)

zip.info <- zip.info[,c(2,4,15,60,61,90,91,94,95,96,97,111)]


dat.zip <- merge(restaurant,zip.info,by.x = 'Zip.Code', by.y = 'GEO.id2')

addresses <- paste(dat.zip$Street.Address,',',dat.zip$City,',',dat.zip$State,',USA')

getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(278, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)

}

i=1

src <- osmsource_api()
for (i in 1401:1526){
  if (is.na(geocoded[i,2])==FALSE & is.na(geocoded[i,1])==FALSE){
    bb1 <- center_bbox(geocoded[i,2],geocoded[i,1],1000,1000)
    ua1 <- get_osm(bb1,source = src)
    nodes <- as.data.frame(summary(ua1$nodes)[6])
    #amenity <- nodes[nodes$keyval.Key=='amenity' | nodes$keyval.Key=='shop',]
    ####question here: which kinds of buildings should be considered?
    surrounding[[i]] <- nodes
  }else{
    surrounding[[i]] <- NA
  }
  
}

save(surrounding,file='surrounding.Rda')

geocoded$restaurant <- 0
geocoded$school <- 0
geocoded$theatre <- 0
geocoded$mall <- 0
geocoded$leisure <- 0



for (i in 1:1526){
  a<-as.data.frame(surrounding[[i]])
  
  if (sum(is.na(a))==0){
    #####restaurants
    if (nrow(a[a$keyval.Value=='restaurant',])==0){
      geocoded[i,8] <- 0
    }else{
      geocoded[i,8] <- sum(a[a$keyval.Value=='restaurant',3])
    }
    ####school
    if (nrow(a[a$keyval.Value=='school',])==0){
      geocoded[i,9] <- 0
    }else{
      geocoded[i,9] <- sum(a[a$keyval.Value=='school',3])
    }
    ####theatre
    if (nrow(a[a$keyval.Value=='theatre',])==0){
      geocoded[i,10] <- 0
    }else{
      geocoded[i,10] <- sum(a[a$keyval.Value=='theatre',3])
    }
    ####mall
    if (nrow(a[a$keyval.Value=='mall',])==0){
      geocoded[i,11] <- 0
    }else{
      geocoded[i,11] <- sum(a[a$keyval.Value=='mall',3])
    }
    ####leisure
    if (nrow(a[a$keyval.Key=='leisure',])==0){
      geocoded[i,12] <- 0
    }else{
      geocoded[i,12] <- sum(a[a$keyval.Key=='leisure',3])
    }
  }
  
}

save(geocoded,file='geocoded.Rda')


dat.geo <- cbind(restaurant.geo,geocoded)
