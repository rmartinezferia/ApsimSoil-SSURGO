# Example of function that downloads the SSURGO database for a field extent 

#install.packages("FedData")
#install.packages("maptools")
#install.packages("maps")

source("_code/ssurgo2apsim.R")

# (down)load ssurgo data
h <- downloadSSURGO(SiteName = "ISU Sorenson", 
                    # Set soil layer structure
                    soilLayer_breaks = c(5,10,15,30,45,60,80,100,120,150,180), 
                    # Set field extent (in Decimal Degrees)
                    north=42.012351, # Latitude
                    south=42.010515, # Latitude
                    east=-93.737968, # Longitude
                    west=-93.742707, # Longitude
                    map = TRUE) 

# Access field info
h$site_name
h$coordinates
h$map
#ggsave("_figures/map.png", width = 8, height = 3)

# Access the downloaded data
h$soils

SSURGO2APSIM(h)



