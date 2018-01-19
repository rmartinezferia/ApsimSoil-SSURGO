
source("_code/SaxtonRawls.R")

Int <- function (X,Y,method) {
  if(all(is.na(X))){rep(NA, length(X))}
  else { approx(X, Y, rule=2,  xout=1:length(X)-1,method=method)$y}
}

Int <- function (Y,X,method = "linear" ){
  
  if(all(is.na(X)) | all(is.infinite(X)) | all(is.na(Y)) | all(is.infinite(Y))){
    rep(NA, length(X))
    
  }
  
  else { 
    approx(X, Y, 
           rule=2,
           xout=1:length(X)-1,
           method=method)$y
  }
}


wtmean <- function(x,w) mean(x*w,na.rm = T)


downloadSSURGO <- function(SiteName = "field",
                           north=NULL,south=NULL,east=NULL,west=NULL,
                           map=TRUE,
                           by_soil = FALSE,
                           soilLayer_breaks =c(5,10,15,30,45,60,80,100,120,160,200),
                           interpolation_Method = c("linear", "constant"),
                           drainage_parms = list(boundary_layer = TRUE,
                                                 DrainDepth  = 1200, #Depth of subsurface drain (mm)
                                                 DrainSpacing = 2000, #Distance between subsurface drains (mm)
                                                 DrainRadius  = 100, #Radius of each subsurface drain (mm)
                                                 Klat  = 1000 #Lateral saturated soil water conductivity (mm/d)
                                                 )){
  
  # Check
  
  # Load packages
  #require(sp)
  require(lubridate)
  require(FedData)
  require(raster)
  require(dplyr)
  require(ggplot2)
  require(ggthemes)
  require(maptools)
  
  a <- polygon_from_extent(extent(west,east,south,north),
                           proj4string="+proj=longlat")
  
  x <- get_ssurgo(template=a, label=SiteName)
  
  
  # Extract useful data -----------------------------------------------------------------------------
  
  component <- x$tabular$component
  chorizon <- x$tabular$chorizon
  mapunit <-x$tabular$mapunit
  watertable <- x$tabular$muaggatt[ , c("musym","wtdepaprjunmin")]
  
  majcomp <- component[component$majcompflag == "Yes", c("compname",
                                                         "taxclname",
                                                         "drainagecl",
                                                         "mukey","cokey","slope.r","hydgrp")]
  
  x$spatial@data %>%
    mutate(MUKEY = as.numeric(as.character(MUKEY))) %>%
    left_join(majcomp %>%
                mutate(mukey = as.numeric(as.character(mukey))), by = c("MUKEY" = "mukey")) -> majcomp2 
  
  # Merge data
  majcomp<- unique(merge(data.frame(mukey=as.character(x$spatial$MUKEY),
                                    musym=as.character(x$spatial$MUSYM)),majcomp))
  
  
  
  # Calculate % of area
  area <- x$spatial@data
  area$id <- rownames(x$spatial@data)
  area$area <- NA
  
  for (i in 1:length(area$id)) area$area[i] <- x$spatial@polygons[[i]]@area
  
  area %>% 
    left_join(majcomp, by = c("MUSYM" = "musym")) %>%
    group_by(MUSYM,compname) %>% 
    summarise(area=sum(area)) %>%
    group_by() %>%
    mutate(area = area/sum(area)) %>%
    as.data.frame() -> area
  names(area) <- c("musym","compname","area")
  
  
  # Merge into dataset
  majcomp <- merge(majcomp, area)
  majcomp <- merge(majcomp,chorizon)
  majcomp <- merge(majcomp,watertable)
  
  # Create horizons
  h <- majcomp[order(majcomp$compname, majcomp$hzdept.r),]
  
  h$compname <- paste0(h$compname,"_",h$musym)
  
  h <- h[,c("compname","area","slope.r", "hydgrp","hzname",
            "hzdept.r","hzdepb.r","hzthk.r", # .r means representative value
            "sandtotal.r","claytotal.r",
            "dbthirdbar.r","dbovendry.r", 
            "om.r","ksat.r",
            "wfifteenbar.r","wthirdbar.r","ph1to1h2o.r","wtdepaprjunmin")]
  
  names(h) <- c("name","area","slope","hydrogroup","hzname","top","bottom","thick","sand","clay","wetbd","drybd", "om","ksat","ll","dul","ph","watertable")
  
  h$watertable_g <- h$watertable + 50 #ave(ifelse(grepl("g",h$hzname),as.numeric(h$center),NA),h$name,FUN = min)
  
  # Bin slope groups
  hydrogroup <- readRDS("_data/hydrogroup.rds")
  h$slope_code <- .bincode(h$slope, breaks=c(0,2,5,10,100))
  h <- left_join(h,hydrogroup) # merge dataset
  
  # Expand soil layers
  h$thick <- ifelse(is.na(h$thick),h$bottom - h$top,h$thick)
  h$center <- trunc(h$top + h$thick/2)
  
  # Add extra layer to the bottom if not deep enough 
  h$maxdepth <- ave(h$bottom,h$name,FUN = max)
  h2 <- h[h$bottom == h$maxdepth,]
  
  if(any(h2$maxdepth < max(soilLayer_breaks))) {
    h2 <- h2[h2$maxdepth < max(soilLayer_breaks),]
    h2$top <- h2$bottom + 1
    h2$bottom <- max(soilLayer_breaks)
    h2$thick <- h2$bottom - h2$top
    h <- rbind(h,h2)
  }
  
  
  h <- h[order(h$name, h$center),-length(h2)]
  
  h <- merge(merge(data.frame(expand.grid(name = unique(h$name),
                                          center = 1:max(soilLayer_breaks))),
                   unique(h[,c("name","area","slope","hydrogroup","top","bottom","thick","slope_code","CN2","watertable_g")])),
             h, all = "T")
  
  h <- h[h$center > h$top & h$center <= h$bottom,] 
  
  h <- h[ , -which(names(h) %in% c("hzname","watertable"))]

  soils <- vector("list", length(unique(h$name)))
  
  for(i in 1:length(unique(h$name))){
    horizon <- h[h$name == unique(h$name)[i], -1]
    
    for(j in 11:length(names(horizon))){
      horizon[,j] <- Int(X=horizon$center, Y=horizon[,j] , method = interpolation_Method[1]) 
    }
    
    # Calculate new variables ------------------------------------------------------------------- 
    
    # Soil physical properties  
    
    horizon$bd <- ifelse( horizon$wetbd < 0.9, 0.9, ifelse(horizon$wetbd > 1.8, 1.8,horizon$wetbd))
    #horizon$bd <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$BD
    #horizon$ll.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$LL15
    #horizon$dul.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$DUL
    
    horizon$ksat <- pmin(horizon$ksat*100/1.157,SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$KSAT*24) # mm/day
    horizon$ksat <- ifelse(horizon$ksat > 500, 500, horizon$ksat) # mm/day
    
    horizon$sat <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$SAT/100
    
    horizon$PO <- 1-horizon$bd/2.65
    
    horizon$Salb <- 0.15 # Bare soil albedo
    
    horizon$MWCON <- 1 #(0-1)
    
    horizon$dul <- horizon$dul/100
    
    horizon$ll <- horizon$ll/100
    
    horizon$SWCON <- (horizon$PO-horizon$dul)/horizon$PO
    
    horizon$AirDry <- ifelse(horizon$center<=15,0.9,ifelse(horizon$center<=30,0.95,1))*horizon$ll
    
    # adjust Boundary layer conditions
    
    if(drainage_parms$boundary_layer){ 
      
      xsat <-  horizon$sat[101:(max(horizon$center) -1 )]
      
      xdul <-  horizon$dul[101:(max(horizon$center) -1 )]
      
      xksat <-  horizon$ksat[101:(max(horizon$center) -1 )]
      
      xcenter <- 1:(max(horizon$center)-101)
      
      horizon$sat <- c(horizon$sat[1:101], (xsat - xdul)*exp(xcenter*-0.01) + xdul) # exponential decay after 100cm 
      horizon$sat  <- ifelse(horizon$sat < horizon$dul + 0.001, horizon$dul + 0.001, horizon$sat)

      horizon$ksat <- c(horizon$ksat[1:101], xksat*exp(xcenter*-0.06)) # exponential decay after 100cm 
      horizon$ksat <- ifelse(horizon$ksat < 0.01, 0.01, horizon$ksat)
      
    }
    
    
    horizon$U <- ifelse(horizon$clay<=20,5+0.175*horizon$clay,
                        ifelse(horizon$clay<=40,7.5+0.05*horizon$clay,
                               ifelse(horizon$clay<=50,11.5-0.05*horizon$clay,
                                      ifelse(horizon$clay<=70,12.75-0.075*horizon$clay,
                                             ifelse(horizon$clay<=80,11-0.05*horizon$clay,0))))) # mm
    
    horizon$cona <- ifelse(horizon$clay<=30,0.025*horizon$clay+3.25,
                           ifelse(horizon$clay<=50,4,
                                  ifelse(horizon$clay<=70,-0.025*horizon$clay+5.25,
                                         ifelse(horizon$clay<=80,3.5,0)))) # mm/d^5
 
    
    horizon$DiffusConst <- 40
    
    horizon$DiffusSlope <- 16
    
    horizon$CN2 <- ifelse(is.na(horizon$CN2),80,horizon$CN2) - 2
    
    horizon$CNRed <- 20 #residue runoff
    
    horizon$CNCov	 <- 0.8
    
    horizon$EnrAcoeff	<- 7.4
    
    horizon$EnrBcoeff	<- 0.2
    
    horizon$XF_maize <- 1
    
    horizon$KL_maize <-	ifelse(horizon$center<=20,0.08,0.09*exp(-0.007*horizon$center))
    
    horizon$e	<- 0.5  #ifelse(F4=$BC$3,0.07,IF(F4=$BC$4,0.03,0.05))
    
    
    # Soil chemical properties
    
    horizon$ph <- 0.52+1.06*horizon$ph #pH 1:5
    
    horizon$OC <- horizon$om/1.72 # %
    
    horizon$OC <- c(horizon$OC[1],
                    ifelse(horizon$center[-1] >= 100 & diff(horizon$OC) == 0,
                           horizon$OC[1]*exp(horizon$center[-1]*-0.035),
                           horizon$OC)) # exponential decay below 100 cm if data is missing
    
    
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
    
    horizon$RootCN <- 45
    
    horizon$SoilCN <- 13
    
    horizon$RootWt <- 1000
    
    # SWIM Specific

    horizon$KDul <- 0.1
    horizon$PSIDul <- -300
    horizon$VC<- TRUE
    horizon$DTmin <- 0
    horizon$DTmax <- 1440
    horizon$MaxWaterIncrement <- 10
    horizon$SpaceWeightingFactor <- 0
    horizon$SoluteSpaceWeightingFactor <- 0
    horizon$Diagnostics <- FALSE

    horizon$Dis  <- 15
    horizon$Disp  <- 1
    horizon$A  <- 1
    horizon$DTHC  <- 0
    horizon$DTHP  <- 1
    horizon$WaterTableCl  <- 0
    horizon$WaterTableNO3  <- 0
    horizon$WaterTableNH4  <- 0
    horizon$WaterTableUrea  <- 0
    horizon$WaterTableTracer  <- 0
    horizon$WaterTableMineralisationInhibitor  <- 0
    horizon$WaterTableUreaseInhibitor  <- 0
    horizon$WaterTableNitrificationInhibitor  <- 0
    horizon$WaterTableDenitrificationInhibitor  <- 0
    #swim Thickness$double  <- 1000
    
    horizon$NO3Exco <- 0
    horizon$NO3FIP <-  1
    horizon$NH4Exco <- 100
    horizon$NH4FIP  <- 1
    horizon$UreaExco  <- 0
    horizon$UreaFIP <- 1
    horizon$ClExco  <- 0
    horizon$ClFIP <- 1
    horizon$DrainDepth  <- drainage_parms$DrainDepth
    horizon$DrainSpacing  <- drainage_parms$DrainSpacing
    horizon$DrainRadius  <- drainage_parms$DrainRadius
    horizon$Klat  <- drainage_parms$Klat
    horizon$ImpermDepth  <- max(horizon$bottom)*10 + 500 
    horizon$WaterTableDepth  <- horizon$watertable_g*10

    # Bin by layer
    
    horizon$layer <- .bincode(horizon$center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))
    
    horizon %>%
      dplyr::select(-hydrogroup) %>%
      group_by(layer) %>%
      mutate(thick = 10 + (max(center)-min(center))*10) %>%
      summarise_all("mean") -> horizon
    
    
    # Save df into list ###########
    
    soils[[i]] <- list(area = unique(horizon$area),
                       horizon = data.frame(horizon %>% 
                                              dplyr::select(-area, -top, -bottom,-center,-slope_code,-slope,-watertable_g)))
    
  }
  
  names(soils) <- (unique(h$name))
  
  ### Return average soil
  
  if(!by_soil) {

    soils <- list(Average_Soil =
                    list(area = 1,
                        horizon = do.call(rbind, sapply(soils, rbind)[2,]) %>%
                        mutate() %>%
                        group_by(layer, thick) %>%
                        summarise_all(weighted.mean,unlist(sapply(soils, rbind)[1,])) %>%
                        as.data.frame()))
  }
  
  ### Return with map
  if(map) { 
    
    x$spatial@data$id <- rownames(x$spatial@data)
    names(area)[1] <- "MUSYM"
    
    return(list(site_name = SiteName,
                coordinates = c(mean(north,south),mean(east,west)),
                map = ggplot(data = merge(fortify(x$spatial, region = "id"),merge(x$spatial@data,area)) , 
                             aes(x=long, y=lat, group = group, fill =  paste0(compname," (",MUSYM,")"))) +
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

