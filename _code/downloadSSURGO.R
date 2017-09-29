
source("_code/SaxtonRawls.R")

Int <- function (X,Y,method) {
  if(all(is.na(X))){rep(NA, length(X))}
  else { approx(X, Y, rule=2,  xout=1:length(X)-1,method=method)$y}
}


downloadSSURGO <- function(SiteName = "field2",
                            north=NULL,south=NULL,east=NULL,west=NULL,
                            map=TRUE,
                            soilLayer_breaks = c(1,5,10,20,30,50,80,120,160,200),
                            interpolation_Method = c("linear", "constant")) {
  
  # Check
  
  # Load packages
  require(sp)
  require(FedData)
  require(raster)
  require(dplyr)
  require(ggplot2)
  require(ggthemes)
  
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
  
  x$spatial@data %>%
    mutate(MUKEY = as.numeric(as.character(MUKEY))) %>%
    left_join(majcomp %>%
                mutate(mukey = as.numeric(as.character(mukey))), by = c("MUKEY" = "mukey")) -> majcomp2 

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
  
  h <- merge(merge(data.frame(expand.grid(name = unique(h$name),
                                          center = 1:max(soilLayer_breaks))),
              unique(h[,c("name","area","slope","hydrogroup","top","bottom","thick","slope_code","CN2")])),
        h, all=T)
  
  h <- h[h$center >= h$top & h$center < h$bottom,] 
  
  soils <- vector("list", length(unique(h$name)))
  
  for(i in 1:length(unique(h$name))){
      horizon <- h[h$name == unique(h$name)[i], -1]
      
      for(j in 10:length(names(horizon))){
        horizon[,j] <- Int(X=horizon$center, Y=horizon[,j] , method = interpolation_Method[1]) 
      }
      
      # Calculate new variables ###########################
      
      #horizon$bd <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$BD
      horizon$bd <- ifelse( horizon$wetbd < 0.9, 0.9, ifelse(horizon$wetbd > 1.8, 1.8,horizon$wetbd))
      
      #horizon$ll.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$LL15
      #horizon$dul.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$DUL
      
      horizon$ksat <- horizon$ksat*100/1.157 # mm/day
      
      horizon$sat <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$SAT/100
      
      horizon$ph <- 0.52+1.06*horizon$ph #pH 1:5
      
      horizon$OC <- horizon$om/1.72 # %
      
      horizon$U <- ifelse(horizon$clay<=20,5+0.175*horizon$clay,
                    ifelse(horizon$clay<=40,7.5+0.05*horizon$clay,
                           ifelse(horizon$clay<=50,11.5-0.05*horizon$clay,
                                  ifelse(horizon$clay<=70,12.75-0.075*horizon$clay,
                                         ifelse(horizon$clay<=80,11-0.05*horizon$clay,0))))) # mm
      
      horizon$cona <- ifelse(horizon$clay<=30,0.025*horizon$clay+3.25,
                        ifelse(horizon$clay<=50,4,
                               ifelse(horizon$clay<=70,-0.025*horizon$clay+5.25,
                                      ifelse(horizon$clay<=80,3.5,0)))) # mm/d^5
      horizon$PO <- 1-horizon$bd/2.65
      
      horizon$Salb <- 0.13
      
      horizon$FInert <- ifelse(horizon$center<=1,0.4,
                         ifelse(horizon$center<=10,0.4,
                                ifelse(horizon$center<60,0.008*horizon$center+0.32,
                                       ifelse(horizon$center<=120,0.8,
                                              ifelse(horizon$center<180,0.0032*horizon$center+0.42,
                                                     ifelse(horizon$center<=300,0.99,0)))))) #(0-1)
      
      horizon$FBiom <- ifelse(horizon$center<=10,0.04,
                        ifelse(horizon$center<=20,0.055-0.0015*horizon$center,
                               ifelse(horizon$center<=30,0.03-0.0005*horizon$center,
                                      ifelse(horizon$center<60,0.0216-0.0002*horizon$center,
                                             ifelse(horizon$center<=300,0.01,0))))) #(0-1)
      
      horizon$MWCON <- 1 #(0-1)
      
      horizon$RootCN <- 45
      
      horizon$SoilCN <- 13
      
      horizon$RootWt <- 1000
      
      horizon$DiffusConst <- 40
      
      horizon$DiffusSlope <- 16
      
      horizon$CNRed <- 20 #residue runoff
      
      horizon$CNCov	 <- 0.8
      
      horizon$EnrAcoeff	<- 7.4
      
      horizon$EnrBcoeff	<- 0.2
      
      horizon$XF_maize <- 1
      
      horizon$KL_maize <-	ifelse(horizon$center<=20,0.08,0.09*exp(-0.007*horizon$center))
      
      horizon$e	<- 0.5  #ifelse(F4=$BC$3,0.07,IF(F4=$BC$4,0.03,0.05))
      
      horizon$dul <- horizon$dul/100
      
      horizon$ll <- horizon$ll/100
      
      horizon$SWCON <- (horizon$PO-horizon$dul)/horizon$PO
      
      horizon$AirDry <- ifelse(horizon$center<=15,0.5,ifelse(horizon$center<=30,0.9,1))*horizon$ll
      
      
      # Bin by layer
      
      horizon$layer <- .bincode(horizon$center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))
      
      horizon %>%
        select(-hydrogroup) %>%
        group_by(layer) %>%
        mutate(thick = (max(center)-min(center))*10) %>%
        summarise_all("mean") -> horizon
      
      
      # Save df into list ###########
      
      soils[[i]] <- list(area = unique(horizon$area),
                         horizon = data.frame(horizon %>% select(-area, -top, -bottom,-center,-slope_code,-slope)))
      
  }
  
  names(soils) <- (unique(h$name))
  
  ### Return with map
  if(map) { 

    x$spatial@data$id <- rownames(x$spatial@data)
    
    return(list(site_name = SiteName,
                coordinates = c(mean(north,south),mean(east,west)),
                map = ggplot(data = merge(fortify(x$spatial, region = "id"),merge(x$spatial@data,majcomp2)) , 
                             aes(x=long, y=lat, group = group, fill = paste0(compname," (",MUSYM,")"))) +
                  geom_polygon(alpha = 0.8)  +
                  geom_path(color = "white") +
                  scale_fill_hue(l = 40) +
                  coord_equal() +
                  labs(title = SiteName, fill= "Soil type:") + 
                  ggthemes::theme_foundation(),
                soils = soils))
  } else {
    return(list(site_name = SiteName,
                coordinates = c(mean(north,south),mean(east,west)),
                soils = soils))
  }
  
}

### Example 
#h <-  download_SSURGO(SiteName = "field2",north=42.010759,south=42.01000,east=-93.740682,west=-93.741712)

