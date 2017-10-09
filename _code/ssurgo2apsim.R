# SSURGO2APSIM Function

source("_code/downloadSSURGO.R")
source("_code/xmlCompile.R")

SSURGO2APSIM <- function(data,
                         area_threshold = 0,
                         crops = c("maize","soybean","wheat")) {
  
  message(paste0("Creating APSIM toolbox with soils that occupy an area greather than ",area_threshold*100,"% of the ",data$site_name)," site.")
  
  if(!(area_threshold >= 0 & area_threshold <= 1)) stop("'area_threshold' must be between 0 and 1")
  
  data$soils <- lapply(data$soils, function(x) {if(x[[1]] > area_threshold) return(x)})
  data$soils <- data$soils[!sapply(data$soils, is.null)] 
  names(data$soils)
  
  out <- xmlCompile(data, crops)
  
  writeLines(saveXML(out), paste0(data$site_name,".xml"))
}