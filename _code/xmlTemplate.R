# Load the package required to read XML files.
#install.packages("XML")
library("XML")
library("Hmisc")
require("lubridate")

source("_code/download_ssurgo.R")
source("_code/latlong2county.R")
source("_code/expandNode.R")

# (down)load ssurgo data
data <- download_SSURGO(SiteName = "field2",
                     soilLayer_breaks = c(5,20,50,80,120,180),
                     north=42.010759,south=42.01000,east=-93.740682,west=-93.741712)

#ssurgo2apsim <- function(data, majorComponets = 1) {

out <- vector("list", length(unique(h$name)))

for(i in 1:length(h$soils)) {
  
  # Read soil templete
  template <- xmlToList(xmlRoot(xmlParse(file = "_data/defaultSoil2.xml")), addAttributes = TRUE, simplify = FALSE)
  
  # RecordNumber
  template[1] <- i
  
  # SoilType
  template$SoilType <- names(h$soils)[i]
  
  # County and State
  county <- capitalize(latlong2county(lat=h$coordinates[1], long=h$coordinates[2]))
  template$Region <- county[2]
  template$State <- county[1]
  template$Country <- "USA"
  
  # Lat & Long
  template$Latitude <- as.character(round(h$coordinates[1],2))
  template$Longitude <- as.character(round(h$coordinates[2],2))
  
  #Year
  template$YearOfSampling <- as.character(year(Sys.Date()))
  
  #SoilWater
  
  ## Cona
  template$SoilWater$SummerCona <- as.character(round(h$soils[[i]]$cona,2)[1])
  template$SoilWater$WinterCona <- as.character(round(h$soils[[i]]$cona,2)[1])
  
  ## U
  template$SoilWater$SummerU <- as.character(round(h$soils[[i]]$U,2)[1])
  template$SoilWater$WinterU <- as.character(round(h$soils[[i]]$U,2)[1])
  
  ## DiffusConst
  template$SoilWater$DiffusConst <- as.character(unique(h$soils[[i]]$DiffusConst))
  
  ## DiffusSlope
  template$SoilWater$DiffusSlope <- as.character(unique(h$soils[[i]]$DiffusSlope))
  
  ## Salb
  template$SoilWater$Salb <- as.character(unique(h$soils[[i]]$Salb))
  
  ## CN2Bare
  template$SoilWater$CN2Bare <- as.character(round(unique(h$soils[[i]]$CN2),0))
  
  ## CNRed
  template$SoilWater$CNRed <- as.character(unique(h$soils[[i]]$CNRed))
  
  ## CNCov
  template$SoilWater$CNCov <- as.character(unique(h$soils[[i]]$CNCov))
  
  ## Thickness
  template$SoilWater[17] <- as.character(expandNode(h$soils[[i]]$thick,"Thickness"))
  
  ## SWCON
  template$SoilWater[18] <- as.character(expandNode(h$soils[[i]]$SWCON,"SWCON"))
  
  # SoilOrganicMatter
  
  ## Thickness
  template$SoilOrganicMatter[6] <- as.character(expandNode(h$soils[[i]]$thick,"Thickness"))
  
  ## OC
  template$SoilOrganicMatter[7] <- as.character(expandNode(round(h$soils[[i]]$OC,2),"OC"))
  
  ## OCMetadata
  template$SoilOrganicMatter[7] <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$OC)),"OC", "string"))
  
  ## FBiom
  template$SoilOrganicMatter[9] <- as.character(expandNode(round(h$soils[[i]]$FBiom,4),"FBiom"))
  
  ## FInert
  template$SoilOrganicMatter[10] <-  as.character(expandNode(round(h$soils[[i]]$FInert,4),"FInert"))
  
  # Water
  
  ## Thickness
  template$Water[1] <- as.character(expandNode(h$soils[[i]]$thick,"Thickness"))
  
  ## BD
  template$Water[2] <- as.character(expandNode(round(h$soils[[i]]$bd,2),"BD"))
  template$Water[8] <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$BD)),"BDMetadata", "string"))
  
  ## AirDry
  template$Water[3] <- as.character(expandNode(round(h$soils[[i]]$AirDry,3),"AirDry"))
  template$Water[9] <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$AirDry)),"AirDryMetadata", "string"))
  
  ## LL15
  template$Water[4] <- as.character(expandNode(round(h$soils[[i]]$ll,3),"LL15"))
  template$Water[10] <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$ll)),"LL15Metadata", "string"))
  
  ## DUL
  template$Water[5] <- as.character(expandNode(round(h$soils[[i]]$dul,3),"DUL"))
  template$Water[11] <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$dul)),"DULMetadata", "string"))
  
  ## SAT
  template$Water[6] <- as.character(expandNode(round(h$soils[[i]]$sat,3),"SAT"))
  template$Water[12] <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$sat)),"SATMetadata", "string"))
  
  ## KS
  template$Water[7] <- as.character(expandNode(round(h$soils[[i]]$ksat,0),"KS"))
  
  ## SoilCrop
  
  ### Thickness
  template$Water$SoilCrop[1] <- as.character(expandNode(h$soils[[i]]$thick,"Thickness"))
  
  ### LL
  template$Water$SoilCrop[2] <- as.character(expandNode(round(h$soils[[i]]$ll,3),"LL"))
  
  ### KL
  template$Water$SoilCrop[3] <-  as.character(expandNode(round(h$soils[[i]]$KL_maize,3),"KL"))
  
  ### XF
  template$Water$SoilCrop[4] <-  as.character(expandNode(round(h$soils[[i]]$XF_maize,3),"XF"))
  
  
  # Analysis
  
  ## Thickness
  template$Analysis[1]  <- as.character(expandNode(h$soils[[i]]$thick,"Thickness"))
  
  ## Texture
  template$Analysis[2]  <- as.character(expandNode(rep(" ", length(h$soils[[i]]$thick)),"Texture", "string"))
  
  ## MunsellColour
  template$Analysis[3]  <- as.character(expandNode(rep(" ", length(h$soils[[i]]$thick)),"MunsellColour", "string"))
  
  ## PH
  template$Analysis[4]  <- as.character(expandNode(round(h$soils[[i]]$ph,2),"PH"))
  template$Analysis[5]  <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$ph)),"PHMetadata", "string"))
  
  ## ParticleSizeSand
  template$Analysis[6]  <- as.character(expandNode(rep(" ", length(h$soils[[i]]$thick)),"CEC"))
  template$Analysis[7]  <- as.character(expandNode(rep(" ", length(h$soils[[i]]$ph)),"CECMetadata", "string"))
  
  ## ParticleSizeSilt
  template$Analysis[8]  <- as.character(expandNode(round(h$soils[[i]]$sand,1),"ParticleSizeSand"))
  template$Analysis[9]  <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$ph)),"ParticleSizeSandMetadata", "string"))
  
  ## ParticleSizeClay
  template$Analysis[10]  <- as.character(expandNode(round(100 - h$soils[[i]]$sand - h$soils[[i]]$clay,1),"ParticleSizeSilt"))
  template$Analysis[11]  <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$ph)),"ParticleSizeSiltMetadata", "string"))
  
  ## ParticleSizeClay
  template$Analysis[12]  <- as.character(expandNode(round(h$soils[[i]]$clay,1),"ParticleSizeClay"))
  template$Analysis[13]  <- as.character(expandNode(rep("ssurgo2apsim", length(h$soils[[i]]$ph)),"ParticleSizeClayMetadata", "string"))
  
  print(names(h$soils)[i])
  #attr(template,"name") <- names(h$soils)[i]
  out[[i]] <- template
}

x <- listToXml(item=out, tag = "li")





