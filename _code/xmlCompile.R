source("_code/latlong2county.R")

xmlCompile <- function(data, crops = c("maize","soybean","wheat"), include_Swim = FALSE, include_tile = FALSE) {
  
  require(XML)
  require(Hmisc)
  require(lubridate)
  
  # <folder version="37" creator="Apsim 7.9-r4044" name="Soils">
  folder <- newXMLNode("folder", attrs = list(version="37",
                                              creator="Apsim 7.9-r4044",
                                              name=data$site_name))
  
  for(i in 1:length(data$soils)){
    
    print(paste0(names(data$soils)[i],": ",
                 round(unique(data$soils[[i]]$area),2)*100,
                 "% of AOI"))
    
    ## <Soil name="Default">
    Soil <- newXMLNode("Soil", attrs = list(name=paste0(data$site_name," (",names(data$soils)[i]," ",round(unique(data$soils[[i]]$area)*100),"% of AOI)")),
                       parent = folder)
    
    ### <RecordNumber>0</RecordNumber>
    RecordNumber <- newXMLNode("RecordNumber",parent = Soil)
    xmlValue(RecordNumber) <-  i
    
    ### <SoilType>Nicollet</SoilType>
    SoilType <- newXMLNode("SoilType",parent = Soil)
    xmlValue(SoilType) <- names(data$soils)[i]
    
    ### <Region>Story</Region>
    county <- capitalize(latlong2county(lat=data$coordinates[1], long=data$coordinates[2]))
    Region <- newXMLNode("Region",parent = Soil)
    xmlValue(Region) <- county[2]
    
    ### <State>Iowa</State>
    State <- newXMLNode("State",parent = Soil)
    xmlValue(State) <- county[1]
    
    ### <Country>US</Country>
    Country <- newXMLNode("Country",parent = Soil)
    xmlValue(Country) <- "USA"
    
    ### <ApsoilNumber>1</ApsoilNumber>
    ApsoilNumber <- newXMLNode("ApsoilNumber",parent = Soil)
    xmlValue(ApsoilNumber) <- 1
    
    ### <Latitude>0</Latitude>
    Latitude <- newXMLNode("Latitude",parent = Soil)
    xmlValue(Latitude) <- round(data$coordinates[1],2)
    
    ### <Longitude>-0</Longitude>
    Longitude <- newXMLNode("Longitude",parent = Soil)
    xmlValue(Longitude) <- round(data$coordinates[2],2)
    
    ### <YearOfSampling>0</YearOfSampling>
    YearOfSampling <- newXMLNode("YearOfSampling",parent = Soil)
    xmlValue(YearOfSampling) <- as.character(year(Sys.Date()))
    
    ### <DataSource>ssurgo2apsim</DataSource>
    DataSource <- newXMLNode("DataSource",parent = Soil)
    xmlValue(DataSource) <- "ssurgo2apsim"
    
    ### <Comments></Comments>
    Comments <- newXMLNode("Comments",parent = Soil)
    xmlValue(Comments) <- paste0("This soil was created with data from SSURGO Database, downloaded in R via the FedData package and parameters set using the approach described by Archontoulis et al. (2014, Agron. J. 106(3):1025-1040). This soil type represents ",
                                 round(unique(data$soils[[i]]$area)*100),"% of AOI.")
    
    
    ### <Water>
    Water <- newXMLNode("Water",parent = Soil)
    
    #### <Thickness>
    Thickness <- newXMLNode("Thickness",parent = Water)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = Thickness)
      xmlValue(double) <- data$soils[[i]]$horizon$thick[j]
    }
    
    #### <BD>
    BD <- newXMLNode("BD",parent = Water)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = BD)
      xmlValue(double) <- round(data$soils[[i]]$horizon$bd[j],2)
    }
    
    #### <AirDry>
    AirDry <- newXMLNode("AirDry",parent = Water)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = AirDry)
      xmlValue(double) <- round(data$soils[[i]]$horizon$AirDry[j],2)
    }
    
    #### <LL15>
    LL15 <- newXMLNode("LL15",parent = Water)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = LL15)
      xmlValue(double) <- round(data$soils[[i]]$horizon$ll[j],2)
    }
    
    #### <DUL>
    DUL <- newXMLNode("DUL",parent = Water)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = DUL)
      xmlValue(double) <- round(data$soils[[i]]$horizon$dul[j],2)
    }
    
    #### <SAT>
    SAT <- newXMLNode("SAT",parent = Water)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = SAT)
      xmlValue(double) <- round(data$soils[[i]]$horizon$sat[j],2)
    }
    
    #### <KS>
    KS <- newXMLNode("KS",parent = Water)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = KS)
      xmlValue(double) <- round(data$soils[[i]]$horizon$ksat[j],1)
    }
    
    #### <SoilCrop>
    for(k in 1:length(crops)){
      SoilCrop <- newXMLNode("SoilCrop", attrs = list(name = crops[k]),parent = Water)
      
      ##### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = SoilCrop)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data$soils[[i]]$horizon$thick[j]
      }
      
      ##### <LL>
      LL <- newXMLNode("LL",parent = SoilCrop)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = LL)
        xmlValue(double) <- round(data$soils[[i]]$horizon$ll[j],2)
      }
      
      ##### <KL>
      KL <- newXMLNode("KL",parent = SoilCrop)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = KL)
        xmlValue(double) <- round(data$soils[[i]]$horizon$KL_maize[j],3)
      }
      
      ##### <XF>
      XF <- newXMLNode("XF",parent = SoilCrop)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = XF)
        xmlValue(double) <- round(data$soils[[i]]$horizon$XF_maize[j],3)
      }
    }
    
    if(include_Swim) {
      
      ### <Swim>
      Swim <- newXMLNode("Swim",parent = Soil)
      
      #### <Salb>
      Salb <- newXMLNode("Salb",parent = Swim)
      xmlValue(Salb) <- unique(data$soils[[i]]$horizon$Salb)
      
      #### <CN2Bare>
      CN2Bare <- newXMLNode("CN2Bare",parent = Swim)
      xmlValue(CN2Bare) <- (round(unique(data$soils[[i]]$horizon$CN2),0))
      
      #### <CNRed>
      CNRed <- newXMLNode("CNRed",parent = Swim)
      xmlValue(CNRed) <- (unique(data$soils[[i]]$horizon$CNRed))
      
      #### <CNCov>
      CNCov <- newXMLNode("CNCov",parent = Swim)
      xmlValue(CNCov) <- (unique(data$soils[[i]]$horizon$CNCov))
      
      #### <KDul>
      KDul <- newXMLNode("KDul",parent = Swim)
      xmlValue(KDul) <- (unique(data$soils[[i]]$horizon$KDul))
      
      #### <PSIDul>
      PSIDul <- newXMLNode("PSIDul",parent = Swim)
      xmlValue(PSIDul) <- (unique(data$soils[[i]]$horizon$PSIDul))

      #### <VC>
      VC <- newXMLNode("VC",parent = Swim)
      xmlValue(VC) <- tolower(as.character(as.logical(unique(data$soils[[i]]$horizon$VC))))
      
      #### <DTmin>
      DTmin <- newXMLNode("DTmin",parent = Swim)
      xmlValue(DTmin) <- (unique(data$soils[[i]]$horizon$DTmin))
      
      #### <DTmax>
      DTmax <- newXMLNode("DTmax",parent = Swim)
      xmlValue(DTmax) <- (unique(data$soils[[i]]$horizon$DTmax))
      
      #### <MaxWaterIncrement>
      MaxWaterIncrement <- newXMLNode("MaxWaterIncrement",parent = Swim)
      xmlValue(MaxWaterIncrement) <- (unique(data$soils[[i]]$horizon$MaxWaterIncrement))
      
      #### <SpaceWeightingFactor>
      SpaceWeightingFactor <- newXMLNode("SpaceWeightingFactor",parent = Swim)
      xmlValue(SpaceWeightingFactor) <- (unique(data$soils[[i]]$horizon$SpaceWeightingFactor))

      #### <SoluteSpaceWeightingFactor>
      SoluteSpaceWeightingFactor <- newXMLNode("SoluteSpaceWeightingFactor",parent = Swim)
      xmlValue(SoluteSpaceWeightingFactor) <- (unique(data$soils[[i]]$horizon$SoluteSpaceWeightingFactor))
      
      #### <Diagnostics>
      Diagnostics <- newXMLNode("Diagnostics",parent = Swim)
      xmlValue(Diagnostics) <- tolower(as.character(as.logical(unique(data$soils[[i]]$horizon$Diagnostics))))
      
      #### <SwimSoluteParameters>
      SwimSoluteParameters <- newXMLNode("SwimSoluteParameters",parent = Swim)
      
      #####<Dis>
      Dis <- newXMLNode("Dis",parent = SwimSoluteParameters)
      xmlValue(Dis) <- (unique(data$soils[[i]]$horizon$Dis))
      
      #####<Disp>1</Disp>
      Disp <- newXMLNode("Disp",parent = SwimSoluteParameters)
      xmlValue(Disp) <- (unique(data$soils[[i]]$horizon$Disp))
      
      #####<A>1</A>
      A <- newXMLNode("A",parent = SwimSoluteParameters)
      xmlValue(A) <- (unique(data$soils[[i]]$horizon$A))
      
      #####<DTHC>0</DTHC>
      DTHC <- newXMLNode("DTHC",parent = SwimSoluteParameters)
      xmlValue(DTHC) <- (unique(data$soils[[i]]$horizon$DTHC))
      
      #####<DTHP>1</DTHP>
      DTHP <- newXMLNode("DTHP",parent = SwimSoluteParameters)
      xmlValue(DTHP) <- (unique(data$soils[[i]]$horizon$DTHP))
      
      #####<WaterTableCl>
      WaterTableCl <- newXMLNode("WaterTableCl",parent = SwimSoluteParameters)
      xmlValue(WaterTableCl) <- (unique(data$soils[[i]]$horizon$WaterTableCl))
      
      #####<WaterTableNO3>
      WaterTableNO3 <- newXMLNode("WaterTableNO3",parent = SwimSoluteParameters)
      xmlValue(WaterTableNO3) <- (unique(data$soils[[i]]$horizon$WaterTableNO3))
      
      #####<WaterTableNH4>
      WaterTableNH4 <- newXMLNode("WaterTableNH4",parent = SwimSoluteParameters)
      xmlValue(WaterTableNH4) <- (unique(data$soils[[i]]$horizon$WaterTableNH4))
      
      #####<WaterTableUrea>
      WaterTableUrea <- newXMLNode("WaterTableUrea",parent = SwimSoluteParameters)
      xmlValue(WaterTableUrea) <- (unique(data$soils[[i]]$horizon$WaterTableUrea))
      
      #####<WaterTableTracer>
      WaterTableTracer <- newXMLNode("WaterTableTracer",parent = SwimSoluteParameters)
      xmlValue(WaterTableTracer) <- (unique(data$soils[[i]]$horizon$WaterTableTracer))
      
      #####<WaterTableMineralisationInhibitor>
      WaterTableMineralisationInhibitor <- newXMLNode("WaterTableMineralisationInhibitor",parent = SwimSoluteParameters)
      xmlValue(WaterTableMineralisationInhibitor) <- (unique(data$soils[[i]]$horizon$WaterTableMineralisationInhibitor))
      
      #####<WaterTableUreaseInhibitor>
      WaterTableUreaseInhibitor <- newXMLNode("WaterTableUreaseInhibitor",parent = SwimSoluteParameters)
      xmlValue(WaterTableUreaseInhibitor) <- (unique(data$soils[[i]]$horizon$WaterTableUreaseInhibitor))
      
      #####<WaterTableNitrificationInhibitor>
      WaterTableNitrificationInhibitor <- newXMLNode("WaterTableNitrificationInhibitor",parent = SwimSoluteParameters)
      xmlValue(WaterTableNitrificationInhibitor) <- (unique(data$soils[[i]]$horizon$WaterTableNitrificationInhibitor))
      
      #####<WaterTableDenitrificationInhibitor>
      WaterTableDenitrificationInhibitor <- newXMLNode("WaterTableDenitrificationInhibitor",parent = SwimSoluteParameters)
      xmlValue(WaterTableDenitrificationInhibitor) <- (unique(data$soils[[i]]$horizon$WaterTableDenitrificationInhibitor))
      
      #####<Thickness>

      Thickness <- newXMLNode("Thickness",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data$soils[[i]]$horizon$thick[j]
      }
      
      #### <NO3Exco>
      NO3Exco <- newXMLNode("NO3Exco",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = NO3Exco)
        xmlValue(double) <- data$soils[[i]]$horizon$NO3Exco[j]
      }
      
      #### <NO3FIP>
      NO3FIP <- newXMLNode("NO3FIP",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = NO3FIP)
        xmlValue(double) <- data$soils[[i]]$horizon$NO3FIP[j]
      }
      
      #### <NH4Exco>
      NH4Exco <- newXMLNode("NH4Exco",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = NH4Exco)
        xmlValue(double) <- data$soils[[i]]$horizon$NH4Exco[j]
      }
      
      #### <NH4FIP>
      NH4FIP <- newXMLNode("NH4FIP",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = NH4FIP)
        xmlValue(double) <- data$soils[[i]]$horizon$NH4FIP[j]
      }
      
      #### <UreaExco>
      UreaExco <- newXMLNode("UreaExco",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = UreaExco)
        xmlValue(double) <- data$soils[[i]]$horizon$UreaExco[j]
      }
      
      #### <UreaFIP>
      UreaFIP <- newXMLNode("UreaFIP",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = UreaFIP)
        xmlValue(double) <- data$soils[[i]]$horizon$UreaFIP[j]
      }
      
      #### <ClExco>
      ClExco <- newXMLNode("ClExco",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = ClExco)
        xmlValue(double) <- data$soils[[i]]$horizon$ClExco[j]
      }
      
      #### <ClFIP>
      ClFIP <- newXMLNode("ClFIP",parent = SwimSoluteParameters)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = ClFIP)
        xmlValue(double) <- data$soils[[i]]$horizon$ClFIP[j]
      }
      
      if (include_tile){
        
        #### <SwimSubsurfaceDrain>
        SwimSubsurfaceDrain <- newXMLNode("SwimSubsurfaceDrain",parent = Swim)
        
        #####<DrainDepth>
        DrainDepth <- newXMLNode("DrainDepth",parent = SwimSubsurfaceDrain)
        xmlValue(DrainDepth) <- (unique(data$soils[[i]]$horizon$DrainDepth))
        
        #####<DrainSpacing>
        DrainSpacing <- newXMLNode("DrainSpacing",parent = SwimSubsurfaceDrain)
        xmlValue(DrainSpacing) <- (unique(data$soils[[i]]$horizon$DrainSpacing))
        
        #####<DrainRadius>
        DrainRadius <- newXMLNode("DrainRadius",parent = SwimSubsurfaceDrain)
        xmlValue(DrainRadius) <- (unique(data$soils[[i]]$horizon$DrainRadius))
        
        #####<Klat>
        Klat <- newXMLNode("Klat",parent = SwimSubsurfaceDrain)
        xmlValue(Klat) <- (unique(data$soils[[i]]$horizon$Klat))
        
        #####<ImpermDepth>
        ImpermDepth <- newXMLNode("ImpermDepth",parent = SwimSubsurfaceDrain)
        xmlValue(ImpermDepth) <- (unique(data$soils[[i]]$horizon$ImpermDepth))
        
      }
     
      if(!is.na(unique(h$soils[[1]]$horizon$WaterTableDepth))) {
        
        #### <SwimWaterTable>
        SwimWaterTable <- newXMLNode("SwimWaterTable",parent = Swim)
        
        #####<WaterTableDepth>
        WaterTableDepth <- newXMLNode("WaterTableDepth",parent = SwimWaterTable)
        xmlValue(WaterTableDepth) <- round(unique(data$soils[[i]]$horizon$WaterTableDepth))
        
      }
        
    } else {
      
      ### <SoilWater>
      SoilWater <- newXMLNode("SoilWater",parent = Soil)
      
      #### <SummerCona>
      SummerCona <- newXMLNode("SummerCona",parent = SoilWater)
      xmlValue(SummerCona) <-(round(data$soils[[i]]$horizon$cona,2)[1])
      
      #### <SummerU>
      SummerU <- newXMLNode("SummerU",parent = SoilWater)
      xmlValue(SummerU) <- (round(data$soils[[i]]$horizon$U,2)[1])
      
      #### <SummerDate>
      SummerDate <- newXMLNode("SummerDate",parent = SoilWater)
      xmlValue(SummerDate) <- "1-jun"
      
      #### <WinterCona>
      WinterCona <- newXMLNode("WinterCona",parent = SoilWater)
      xmlValue(WinterCona) <-as.character(round(data$soils[[i]]$horizon$cona,2)[1])
      
      #### <WinterU>
      WinterU <- newXMLNode("WinterU",parent = SoilWater)
      xmlValue(WinterU) <- as.character(round(data$soils[[i]]$horizon$U,2)[1])
      
      #### <WinterDate>
      WinterDate <- newXMLNode("WinterDate",parent = SoilWater)
      xmlValue(WinterDate) <- "1-nov"
      
      #### <DiffusConst>
      DiffusConst <- newXMLNode("DiffusConst",parent = SoilWater)
      xmlValue(DiffusConst) <-  unique(data$soils[[i]]$horizon$DiffusConst)
      
      #### <DiffusSlope>
      DiffusSlope <- newXMLNode("DiffusSlope",parent = SoilWater)
      xmlValue(DiffusSlope) <-  unique(data$soils[[i]]$horizon$DiffusSlope)
      
      #### <Salb>
      Salb <- newXMLNode("Salb",parent = SoilWater)
      xmlValue(Salb) <- unique(data$soils[[i]]$horizon$Salb)
      
      #### <CN2Bare>
      CN2Bare <- newXMLNode("CN2Bare",parent = SoilWater)
      xmlValue(CN2Bare) <- (round(unique(data$soils[[i]]$horizon$CN2),0))
      
      #### <CNRed>
      CNRed <- newXMLNode("CNRed",parent = SoilWater)
      xmlValue(CNRed) <- (unique(data$soils[[i]]$horizon$CNRed))
      
      #### <CNCov>
      CNCov <- newXMLNode("CNCov",parent = SoilWater)
      xmlValue(CNCov) <- (unique(data$soils[[i]]$horizon$CNCov))
      
      #### <Slope>NaN</Slope>
      #### <DischargeWidth>NaN</DischargeWidth>
      #### <CatchmentArea>NaN</CatchmentArea>
      #### <MaxPond>NaN</MaxPond>
      
      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = SoilWater)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data$soils[[i]]$horizon$thick[j]
      }
      
      #### <SWCON>
      SWCON <- newXMLNode("SWCON",parent = SoilWater)
      for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
        double <- newXMLNode("double",parent = SWCON)
        xmlValue(double) <- round(data$soils[[i]]$horizon$SWCON[j],3)
      }
      
    }
    
    
    ### SoilOrganicMatter
    SoilOrganicMatter <- newXMLNode("SoilOrganicMatter",parent = Soil)
    
    #### <RootCN>
    RootCN <- newXMLNode("RootCN",parent = SoilOrganicMatter)
    xmlValue(RootCN) <- unique(round(data$soils[[i]]$horizon$RootCN))
    
    #### <RootWt>
    RootWt <- newXMLNode("RootWt",parent = SoilOrganicMatter)
    xmlValue(RootWt) <- unique(round(data$soils[[i]]$horizon$RootWt))
    
    #### <SoilCN>13</SoilCN>
    SoilCN <- newXMLNode("SoilCN",parent = SoilOrganicMatter)
    xmlValue(SoilCN) <- unique(round(data$soils[[i]]$horizon$SoilCN))
    
    #### <EnrACoeff>
    EnrACoeff <- newXMLNode("EnrACoeff",parent = SoilOrganicMatter)
    xmlValue(EnrACoeff) <- unique(round(data$soils[[i]]$horizon$EnrAcoeff,2))
    
    #### <EnrBCoeff>
    EnrBCoeff <- newXMLNode("EnrBCoeff",parent = SoilOrganicMatter)
    xmlValue(EnrBCoeff) <- unique(round(data$soils[[i]]$horizon$EnrBcoeff,2))
    
    #### <Thickness>
    Thickness <- newXMLNode("Thickness",parent = SoilOrganicMatter)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = Thickness)
      xmlValue(double) <- data$soils[[i]]$horizon$thick[j]
    }
    
    #### <OC>
    OC <- newXMLNode("OC",parent = SoilOrganicMatter)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = OC)
      xmlValue(double) <- round(data$soils[[i]]$horizon$OC[j],2)
    }
    
    #### <FBiom>
    FBiom <- newXMLNode("FBiom",parent = SoilOrganicMatter)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = FBiom)
      xmlValue(double) <- round(data$soils[[i]]$horizon$FBiom[j],4)
    }
    
    #### <FInert>
    FInert <- newXMLNode("FInert",parent = SoilOrganicMatter)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = FInert)
      xmlValue(double) <- round(data$soils[[i]]$horizon$FInert[j],4)
    }
    
    #### <OCUnits>
    OCUnits <- newXMLNode("OCUnits",parent = SoilOrganicMatter)
    xmlValue(OCUnits) <- "Total"
    
    ### <Analysis>
    Analysis <- newXMLNode("Analysis",parent = Soil)
    
    #### <Thickness>
    Thickness <- newXMLNode("Thickness",parent = Analysis)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = Thickness)
      xmlValue(double) <- data$soils[[i]]$horizon$thick[j]
    }
    
    #### <PH>
    PH <- newXMLNode("PH",parent = Analysis)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = PH)
      xmlValue(double) <- round(data$soils[[i]]$horizon$ph[j],2)
    }
    
    #### <ParticleSizeSand>
    ParticleSizeSand <- newXMLNode("ParticleSizeSand",parent = Analysis)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = ParticleSizeSand)
      xmlValue(double) <- round(data$soils[[i]]$horizon$sand[j],1)
    }
    
    #### <ParticleSizeClay>
    ParticleSizeClay <- newXMLNode("ParticleSizeClay",parent = Analysis)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = ParticleSizeClay)
      xmlValue(double) <- round(data$soils[[i]]$horizon$clay[j],1)
    }
    
    #### <ParticleSizeSilt>
    ParticleSizeSilt <- newXMLNode("ParticleSizeSilt",parent = Analysis)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = ParticleSizeSilt)
      xmlValue(double) <- round(100 - data$soils[[i]]$horizon$sand[j] - data$soils[[i]]$horizon$clay[j],1)
    }
    
    ### Sample
    Sample <- newXMLNode("Sample",parent = Soil, attrs = list(name="Intial conditions"))
    
    #### <Thickness>
    Thickness <- newXMLNode("Thickness",parent = Sample)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = Thickness)
      xmlValue(double) <- data$soils[[i]]$horizon$thick[j]
    }
    
    #### <NO3>
    NO3 <- newXMLNode("NO3",parent = Sample)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = NO3)
      xmlValue(double) <- round(data$soils[[i]]$horizon$OC[j],2) # same as OC but in ppm
    }
    
    #### <NH4>
    NH4 <- newXMLNode("NH4",parent = Sample)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = NH4)
      xmlValue(double) <- round(data$soils[[i]]$horizon$OC[j],2)*0.5 # same as 1/2 oc but ppm
    }
    
    #### <SW>
    SW <- newXMLNode("SW",parent = Sample)
    for(j in 1:length(data$soils[[i]]$horizon$thick)){ 
      double <- newXMLNode("double",parent = SW)
      xmlValue(double) <- round(data$soils[[i]]$horizon$dul[j],2)
    }
    
    #### <NO3Units>
    NO3Units <- newXMLNode("NO3Units",parent = Sample)
    xmlValue(NO3Units) <- "ppm"
    
    #### <NH4Units>
    NH4Units <- newXMLNode("NH4Units",parent = Sample)
    xmlValue(NH4Units) <- "ppm"
    
    #### <SWUnits>
    SWUnits <- newXMLNode("SWUnits",parent = Sample)
    xmlValue(SWUnits) <- "Volumetric"
    
  }
  
  return(folder)
  
} 
