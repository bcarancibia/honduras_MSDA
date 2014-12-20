#Script takes in x,y location of aid projects
#Stack of arbitrary rasters
#Aggregates to any arbitrary polygone for analysis
#Outputs a Decision Tree MLA for various outcomes

library(sp)
library(maptools)
library(plyr)
library(raster)
library(rgdal)
library(leafletR)
library(RColorBrewer)
library(stringr)

wd = "/users/bcarancibia/CUNY_IS_608/final_project/"
setwd(wd) 

INPUT="Toolkit_geocoded_Honduras.csv" #cannot share this raw raw data

GADM_Aggregation = "Honduras_1990-2013.shp" #this can be downloaded from UNGADM but per licensing I cannot share this data


wd = "/users/bcarancibia/CUNY_IS_608/final_project/"
setwd(wd) 

data_dir = "/users/bcarancibia/CUNY_IS_608/final_project/honduras/data"

sector=vector()
sector[1] = "water"
sector[2] = "enviro"
sector[3] = "transport|highway|road"
sector[4] = "health"

sector_names = vector()
sector_names[1] = "Water-Related Projects"
sector_names[2] = "Environmental Projects"
sector_names[3] = "Infrastructure Projects"
sector_names[4] = "Health Projects"

#For every Donor, assign a color.
input_data = read.csv(INPUT, header=TRUE)
donors = unique(input_data$DONOR)

donor_colors = vector()
for (i in 1:length(donors))
{
donor_color = brewer.pal(10,'Set3')[i]

nm = str_replace_all(as.character(donors[i]), "[^[:alnum:]]", "")

ev_str = paste(nm,"='",donor_color,"'")

eval(parse(text=ev_str))

}


ColLookup <- function(donor)
  {
  nm = str_replace_all(as.character(donor), "[^[:alnum:]]", "")
  eval(parse(text=nm))
  }

#Create overlays for sectors.
for (i in 1:length(sector))
  {
  input_data = read.csv(INPUT, header=TRUE)
  sector_data <- input_data[grep(sector[i], input_data$LOCATION_ACTIVITY_DESCRIPTION),]
  sector_data["TITLE"] = as.data.frame(apply(sector_data["TITLE"],2,function(x)gsub("[^0-9A-Za-z///' ]", "", x)))
  geocoded_sector_data <- sector_data[rowSums(is.na(sector_data[,c("LATITUDE","LONGITUDE")]))==0,]
  GIS_sector_data <- SpatialPointsDataFrame(list(geocoded_sector_data$LONGITUDE,geocoded_sector_data$LATITUDE), sector_data)
  GIS_sector_data@data["COLOR_D"] = "#000000"
  for (j in 1:length(GIS_sector_data))
    {
    GIS_sector_data@data[,c('COLOR_D')][j] = ColLookup(GIS_sector_data@data[,c('DONOR')][j])
    }
  
  GIS_sector_data <- GIS_sector_data[,c('TITLE','LATITUDE','LONGITUDE','DONOR','COLOR_D')]


  View(GIS_sector_data)
  
  proj4string(GIS_sector_data) <- CRS("+init=epsg:4326")
  toGeoJSON(data=GIS_sector_data, name=sector_names[i], dest=data_dir) 
  }


#Create ancilliary data layers
GIS_GADM = readShapePoly(GADM_Aggregation)

GIS_GADM <- GIS_GADM[,c('ADM2_NAME')]

proj4string(Attribute_GIS) <- CRS("+init=epsg:4326")

#Raster Aggregations

#Population Density (roughly per sq. km)
Rlayer <- raster("2011ls_hond.tif")
Attribute_GIS <- extract(Rlayer,GIS_GADM, fun='mean', na.rm=TRUE, sp=TRUE, small=TRUE)
Attribute_GIS <- rename(Attribute_GIS, c("X2011ls_hond" = "PopDens"))

Attribute_GIS <- GIS_GADM[,c('PopDens')]

Attribute_GIS$PopDens = round(Attribute_GIS$PopDens)
toGeoJSON(data=Attribute_GIS, dest=data_dir)

#Create a visualizaion of the  data.


# cuts<-round(quantile(Attribute_GIS$POP_DENSITY, probs = seq(0,1,0.2), na.rm = TRUE),0)
# cuts[1] <- 0
# 
# popup <- c("ADM2_NAME", "POP_DENSITY")
# 
# sty<- styleGrad(prop="POP_DENSITY", breaks=cuts, right=FALSE, style.par="col",
#                 style.val=rev(heat.colors(6)), leg="Population Density per sq. KM (2011)", lwd=1)
# 
# popdens <- toGeoJSON(data=Attribute_GIS, dest=tempdir())
# 
# map<-leaflet(data=popdens, style=sty,
#              title="index", base.map="osm",
#              incl.data=TRUE,  popup=popup)
# 
# #browseURL(map)

DTD <- merge(Attribute_GIS, project_data)
DTD$dr_events[is.na(DTD$dr_events)] <- 0

DTD_C <- DTD[complete.cases(DTD$Freq),]

fit <- rpart(Above_med_projects ~ POP_DENSITY + dr_events + dist_road + dist_city + WS, data=DTD_C)

#to be used later, not working all the way yet
plotcp(fit)
plot(fit, uniform=TRUE, main="Classificaiton Tree for Water-related Aid")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
post(fit, file="/users/bcarancibia/CUNY_IS_608/tree.ps", title="Classification Tree for Water-related Aid in Honduras")

Attribute_GIS$DT_EST <- predict(fit, newdata = Attribute_GIS)

cuts<-round(quantile(Attribute_GIS$DT_EST, probs = seq(0.0,1.0,0.2), na.rm = TRUE),2)
cuts[1] <- 0

popup <- c("ADM2_NAME", "DT_EST")

#sty<- styleGrad(prop="DT_EST", breaks=cuts, right=FALSE, style.par="col",style.val=rev(heat.colors(6)), leg="Probability of >avg Water projects", lwd=1)
# sty <- styleGrad(prop="DT_EST", breaks=cuts, style.val=rev(heat.colors(6)))
# sty_2 <- styleGrad(prop="dist_road", breaks=cuts, style.val=rev(heat.colors(6)))
# 
# sty <- styleSingle(col="brown")
# sty_2 <- styleSingle(col="red")
# 

# 
#  map<-leaflet(data=AtrGeo, style=sty_2,
#               title="index_2", incl.data=TRUE)
# # 
# map<-leaflet(data=list(AtrGeo,AtrGeo), style=list(sty,sty_2),
#              title="index_2", incl.data=TRUE)
# # 
#  browseURL(map)


AtrGeo <- toGeoJSON(data=Attribute_GIS, dest=tempdir())