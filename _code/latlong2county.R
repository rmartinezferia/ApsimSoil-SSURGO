# https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county

latlong2county <- function(lat, long) {
  
  require(sp)
  require(maps)
  require(maptools)
  
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat"))

  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(data.frame(long,lat), 
                            proj4string=CRS("+proj=longlat"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  
  unlist(strsplit(countyNames[indices], ","))
}

# Test the function using points in Wisconsin
#latlong2county(lat=44, long=-90)
