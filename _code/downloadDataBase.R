#install.packages("FedData")
library(FedData)
source("_code/SaxtonRawls.R")

# Inputs ###########################################

SiteName <- "SorensonFarm"
FieldExtent <- c(-93.740682, -93.741712, 42.01000, 42.010759)

# Download SSURGO data from extent #################

a <- polygon_from_extent(raster::extent(FieldExtent[1],FieldExtent[2],FieldExtent[3],FieldExtent[4]),
                    proj4string="+proj=longlat")

x <- get_ssurgo(template=a, label=SiteName)

# Extract useful data #############################

component <- x$tabular$component
chorizon <- x$tabular$chorizon
mapunit <-x$tabular$mapunit
majcomp <- component[component$majcompflag == "Yes", c("compname",
                                                       "taxclname",
                                                       "drainagecl",
                                                       "mukey","cokey","slope.r","hydgrp")]

### Plot mapunits 
#plot(x$spatial)
#invisible(text(getSpPPolygonsLabptSlots(x$spatial), labels=as.character((majcomp$compname)), cex=1))

# Merge data ########################################

majcomp<- merge(data.frame(mukey=as.character(x$spatial$MUKEY),
                           musym=as.character(x$spatial$MUSYM)),
                majcomp)


# Calculate % of area
area <- data.frame(musym=x$spatial$MUSYM, area=NA)
for (i in 1:length(x$spatial$MUSYM)) area$area[i] <- x$spatial@polygons[[i]]@area
area$area <- area$area/sum(area$area)

# Merge into dataset
majcomp <- merge(majcomp, area)
majcomp <- merge(majcomp,chorizon)

# Create horizons
h <- majcomp[order(majcomp$compname, majcomp$hzdept.r),]
h <- h[,c("compname","area","slope.r", "hydgrp",
          "hzdept.r","hzdepb.r","hzthk.r",
          "sandtotal.r","claytotal.r",
          "dbthirdbar.r","dbovendry.r", 
          "om.r","ksat.r",
          "wfifteenbar.r","wthirdbar.r","ph1to1h2o.r")]

names(h) <- c("name","area","slope","hydrogroup","top","bottom","thick","sand","clay","wetbd","drybd", "om","ksat","ll","dul","ph")

# Create new variables ###########################

## Center 
h$center <- h$top + h$thick/2

# Runoff 

hydgrp <- data.frame(matrix(data=c(61,64,68,71,73,76,80,83,81,84,88,91,84,87,91,94),
                 ncol = 4,nrow = 4,
                 dimnames = list(c("0-2","2-5","5-10","10-100"),
                                 LETTERS[1:4])))


hydgrp$"A/B" <- 0.5*(hydgrp[,1]+hydgrp[,1+1])
hydgrp$"A/C" <- 0.5*(hydgrp[,1]+hydgrp[,1+2])
hydgrp$"A/D" <- 0.5*(hydgrp[,1]+hydgrp[,1+3])

hydgrp$"B/C" <- 0.5*(hydgrp[,2]+hydgrp[,2+1])
hydgrp$"B/D" <- 0.5*(hydgrp[,2]+hydgrp[,2+2])

hydgrp$"C/D" <- 0.5*(hydgrp[,3]+hydgrp[,3+1])

require(tidyr)
hydgrp$key <-row.names(hydgrp)

hydgrp %>%
  group_by(key) %>%
  gather(key="hydgrp",value="CN2",-key)


for(i in length(names(hydgrp))){
#  while(i < length(names(hydgrp)) )
  
  0.5*(hydgrp[,1]+hydgrp[,1+1])
  
}



if your soil has a slope of 2% and belongs to Hydrological group of B, then the CN2 value is 73 - 5 = 68
if your soil has a slope of 2% and belongs to Hydrological group of B/D, then you calculate 2 CN2 values and make the average, here is 73-5=68 and 84-5=79, so (68+79)/2 = 73.5


#h$bd <- SaxtonRawls(pSand=h$sand ,pClay=h$clay, pOM=h$om)$BD
h$bd <- ifelse( h$wetbd < 0.9, 0.9, ifelse(h$wetbd > 1.8, 1.8,h$wetbd))

#h$ll.sr <- SaxtonRawls(pSand=h$sand ,pClay=h$clay, pOM=h$om)$LL15
#h$dul.sr <- SaxtonRawls(pSand=h$sand ,pClay=h$clay, pOM=h$om)$DUL
h$sat <- SaxtonRawls(pSand=h$sand ,pClay=h$clay, pOM=h$om)$SAT
