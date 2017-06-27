
source("_code/SaxtonRawls.R")


Int <- function (X,Y,method) {
  if(all(is.na(X))){rep(NA, length(X))}
  else { approx(X, Y, rule=2,  xout=1:length(X)-1,method=method)$y}
}


download_SSURGO <- function(SiteName = "field2",
                            north=NULL,south=NULL,east=NULL,west=NULL,
                            map=TRUE,
                            soilLayer_breaks = c(1,5,10,20,30,50,80,120,160,200),
                            interpolation_Method = c("linear", "constant")) {
  
  # Load packages
  require(sp)
  require(FedData)
  require(raster)
  require(dplyr)
  
  a <- polygon_from_extent(extent(east, west, north, south),
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
  ### Plot make a map
  
  if(map) { 
    plot(x$spatial)
    invisible(text(getSpPPolygonsLabptSlots(x$spatial), labels=as.character((majcomp$compname)), cex=1))
  }
  
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
  
  # Bin slope groups
  hydrogroup <- readRDS("_data/hydrogroup.rds")
  h$slope_code <- .bincode(h$slope, breaks=c(0,2,5,10,100))
  h <- left_join(h,hydrogroup) # merge dataset
  
  # Expand soil layers
  h$center <- trunc(h$top + h$thick/2)
  
  h<- merge(merge(data.frame(name=rep(unique(h$name),
                                  each=max(soilLayer_breaks)+1),
                         center=0:max(soilLayer_breaks)),
              unique(h[,c("name","area","slope","hydrogroup","top","bottom","thick","slope_code","CN2")])),
        h, all=T)
  
  h <- h[h$center >= h$top & h$center < h$bottom,] 
  
  soils <- vector("list", length(unique(h$name)))
  
  for(i in 1:length(unique(h$name))){
      soil <- h[h$name == unique(h$name)[i], -1]
      
      for(j in 10:length(names(soil))){
        soil[,j] <- Int(X=soil$center, Y=soil[,j] , method = interpolation_Method[1]) 
      }
      
      # Calculate new variables ###########################
      
      #soil$bd <- SaxtonRawls(pSand=soil$sand ,pClay=soil$clay, pOM=soil$om)$BD
      soil$bd <- ifelse( soil$wetbd < 0.9, 0.9, ifelse(soil$wetbd > 1.8, 1.8,soil$wetbd))
      
      #soil$ll.sr <- SaxtonRawls(pSand=soil$sand ,pClay=soil$clay, pOM=soil$om)$LL15
      #soil$dul.sr <- SaxtonRawls(pSand=soil$sand ,pClay=soil$clay, pOM=soil$om)$DUL
      
      soil$ksat <- soil$ksat*100/1.157 # mm/day
      
      soil$sat <- SaxtonRawls(pSand=soil$sand ,pClay=soil$clay, pOM=soil$om)$SAT/100
      
      soil$ph <- 0.52+1.06*soil$ph #pH 1:5
      
      soil$OC <- soil$om/1.72 # %
      
      soil$U <- ifelse(soil$clay<=20,5+0.175*soil$clay,
                    ifelse(soil$clay<=40,7.5+0.05*soil$clay,
                           ifelse(soil$clay<=50,11.5-0.05*soil$clay,
                                  ifelse(soil$clay<=70,12.75-0.075*soil$clay,
                                         ifelse(soil$clay<=80,11-0.05*soil$clay,0))))) # mm
      
      soil$cona <- ifelse(soil$clay<=30,0.025*soil$clay+3.25,
                        ifelse(soil$clay<=50,4,
                               ifelse(soil$clay<=70,-0.025*soil$clay+5.25,
                                      ifelse(soil$clay<=80,3.5,0)))) # mm/d^5
      soil$PO <- 1-soil$bd/2.65
      
      soil$Salb <- 0.13
      
      soil$FInert <- ifelse(soil$center<=1,0.4,
                         ifelse(soil$center<=10,0.4,
                                ifelse(soil$center<60,0.008*soil$center+0.32,
                                       ifelse(soil$center<=120,0.8,
                                              ifelse(soil$center<180,0.0032*soil$center+0.42,
                                                     ifelse(soil$center<=300,0.99,0)))))) #(0-1)
      
      soil$FBiom <- ifelse(soil$center<=10,0.04,
                        ifelse(soil$center<=20,0.055-0.0015*soil$center,
                               ifelse(soil$center<=30,0.03-0.0005*soil$center,
                                      ifelse(soil$center<60,0.0216-0.0002*soil$center,
                                             ifelse(soil$center<=300,0.01,0))))) #(0-1)
      
      soil$MWCON <- 1 #(0-1)
      
      soil$CNsoil <- 13
      
      soil$rootWt <- 1000
      
      soil$DiffusConst <- 40
      
      soil$DiffusSlope <- 16
      
      soil$CNRed <- 20 #residue runoff
      
      soil$CNCov	 <- 0.8
      
      soil$EnrAcoeff	<- 7.4
      
      soil$EnrBcoeff	<- 0.2
      
      soil$XF_maize <- 1
      
      soil$KL_maize <-	ifelse(soil$center<=20,0.08,0.09*exp(-0.007*soil$center))
      
      soil$e	<- 0.5  #ifelse(F4=$BC$3,0.07,IF(F4=$BC$4,0.03,0.05))
      
      soil$dul <- soil$dul/100
      
      soil$ll <- soil$ll/100
      
      soil$SWCON <- (soil$PO-soil$dul)/soil$PO
      
      soil$AirDry <- ifelse(soil$center<=15,0.5,ifelse(soil$center<=30,0.9,1))*soil$ll
      
      
      # Bin by layer
      
      soil$layer <- .bincode(soil$center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))
      
      soil %>%
        dplyr::select(-hydrogroup) %>%
        group_by(layer) %>%
        mutate(thick = (max(center)-min(center))*10) %>%
        summarise_all("mean") -> soil
      
      
      # Save df into list ###########
      
      
      soils[[i]] <- data.frame(soil)
      
  }
  
  names(soils) <- (unique(h$name))
  
  return(list(coordinates = c(mean(north,south),mean(east,west)),
              soils = soils))
}

### Example 
soils <-  download_SSURGO(SiteName = "field2",north=42.010759,south=42.01000,east=-93.740682,west=-93.741712)
View(soils[2])
