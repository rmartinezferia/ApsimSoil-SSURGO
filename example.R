# Example of function that downloads the SSURGO database for a field extent ###
# and converts data into a APSIM toolbox ######################################

# Install packages ---------------------------------------------------------- # 
#devtools::install_github("ropensci/FedData")
#install.packages("maptools")
#install.packages("maps")
#install.packages("raster")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("XML")
#install.packages("Hmisc")
#install.packages("lubridate")

# Load functions ------------------------------------------------------------ #
source("_code/ssurgo2apsim.R")

# (down)load ssurgo data ---------------------------------------------------- #
h <- downloadSSURGO(SiteName = "ISU Sorenson", 
                    # Set soil layer structure
                    soilLayer_breaks = c(5,20,50,80,120,180), 
                    # Set field extent (in Decimal Degrees)
                    north=42.012351, # Latitude
                    south=42.010515, # Latitude
                    east=-93.737968, # Longitude
                    west=-93.742707, # Longitude
                    by_soil = TRUE,
                    map = TRUE)


# Access field info --------------------------------------------------------- #
h$soils
h$site_name
h$coordinates
h$map
ggsave("_figures/map.png", width = 8, height = 3)

# Access the downloaded data ------------------------------------------------ #

# Create APSIM toolbox with soils ------------------------------------------- #
SSURGO2APSIM(h, crops = c("maize","soybean"),
             area_threshold = 0,
             include_Swim = FALSE, include_tile = FALSE)
