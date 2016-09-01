library(FedData)
library(maptools)

#### Inputs

SoilName <- "SorensonFarm"
FieldExtent <- c(-93.740682, -93.741712, 42.01000, 42.010759)

### Download SSURGO data from extent
a <- polygon_from_extent(raster::extent(FieldExtent[1],FieldExtent[2],FieldExtent[3],FieldExtent[4]),
                    proj4string="+proj=longlat")

x <- get_ssurgo(template=a, label=SoilName)

component <- x$tabular$component
chorizon <- x$tabular$chorizon
mapunit <-x$tabular$mapunit
majcomp <- component[component$majcompflag == "Yes", c("compname",
                                                       "taxclname",
                                                       "drainagecl",
                                                       "mukey")]

majcomp<- merge(data.frame(mukey=as.character(x$spatial$MUKEY),
                           musym=as.character(x$spatial$MUSYM)),
                majcomp)


area <- data.frame(musym=x$spatial$MUSYM, area=NA)
for (i in 1:length(x$spatial$MUSYM)) area$area[i] <- x$spatial@polygons[[i]]@area

majcomp <- merge(majcomp, area)

### Plot data 

plot(x$spatial)
invisible(text(getSpPPolygonsLabptSlots(x$spatial), labels=as.character(majcomp$compname), cex=1))
