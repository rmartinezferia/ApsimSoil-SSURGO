# Example of function that downloads the SSURGO database for a field extent 

#install.packages("FedData")
require(tidyverse)
source("_code/download_ssurgo.R")

# (down)load ssurgo data
h <- download_SSURGO(SiteName = "Sorenson", 
                     # Set soil layer structure
                     soilLayer_breaks = c(5,20,50,80,120,180), 
                     # Set field extent (in Decimal Degrees)
                     north=42.010759, # Latitude
                     south=42.01000,  # Latitude
                     east=-93.740682, # Longitude
                     west=-93.741712) # Longitude

# Access the downloaded data
h$soils