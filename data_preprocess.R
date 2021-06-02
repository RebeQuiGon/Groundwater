library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(raster)
library(sp)
library(rgdal)


#data documentation: https://data.geus.dk/tabellerkoder/index.html?tablename=WATLEVEL

mainDir<- "C:/Users/rebe_/Desktop/thesis"
dataDir<- "C:/Users/rebe_/Desktop/thesis/Data"
predDir<- "C:/Users/rebe_/Desktop/thesis/Data/Predictors_DK"


####PREPROCESS BOREHOLE DATA######################################################
#Load the point data
all_points<- st_read("C:/Users/rebe_/Desktop/thesis/data/hovedstaden_15m.shp")


##FIRST remove data too old? make timofmeas as date/time
timeclean<- all_points %>% filter(timeofmeas>1990)

#clean the data
tryouts<- timeclean %>% filter(watlevgrsu<1000 & watlevgrsu>-5) #there are some outliers, exclude them too
tryouts<- tryouts %>% filter(drilldepth<10) #take only surface boreholes
tryouts<-tryouts %>% group_by(boreholeid) %>% filter(watlevgrsu < drilldepth) #exclude measurements deeper than drill depth
tryouts<- tryouts %>% filter(!is.na(watlevgrsu)) #exclude NA values


#TIME FORMATTING
#Time from chr into date format
finalpoints<-tryouts
final_time <- finalpoints %>%
  mutate(timeofmeas = ymd_hms(timeofmeas))

final_time$date = as.Date(final_time$timeofmeas, format = "%Y/%m/%d")

st_write(final_time, file.path(mainDir, "data/boreholes/final_time.shp"), 
         check_exists=TRUE, overwrite_layer=T, append = F) # overwrites

####MONTHLY DATA################################################################
# create month and year column
final_time$month = lubridate::month(final_time$timeofmeas)
final_time$year = lubridate::year(final_time$timeofmeas)
final_time$date <- as.yearmon(final_time$timeofmeas)

#aggregate by date and boreholeid, keeping the other columns as unique values
# summarize data as needed, make date as year/month: this makes a new table, no spatial, no coords
finalsum<-final_time %>% group_by(boreholeid,year,month) %>% 
  summarise(numMean = mean(watlevgrsu), sd = sd(watlevgrsu)) %>% 
  as.data.frame

finalsum$Date <- as.yearmon(paste(finalsum$year, finalsum$month), "%Y %m")


##Get the coordinates of the points back to the summarized monthly data
pointgeom<-final_time
pointgeom<-pointgeom[!duplicated(pointgeom$boreholeid), ] #keep single boreholes

newdata <- pointgeom[c(2,11,12,14)] #delete unwanted columns, leave only boreholeid and coordinates/geometry

points<- merge(finalsum, newdata, by= "boreholeid", all.x=TRUE) #merge back to the monthly data

pointsfinal<- points %>% filter(year<2019) #precipitation goes up to 2018, so get boreholes up to that

pointsfinal<-pointsfinal[c(-5,-6)] #final points with coords and monthly watlevel averages

st_write(pointsfinal, file.path(mainDir, "data/boreholes/monthly_2018.shp"), 
         check_exists=TRUE, overwrite_layer=T, append = F) # overwrites
st_write(points, file.path(mainDir, "data/boreholes/monthly_2020.shp"), delete_layer = TRUE) # overwrites




####CLEAN DATA INDEPENDENT VARIABLES##########################################################################################
saveDir<- "C:/Users/rebe_/Desktop/thesis/Data/Predictors_DK"

#Load mask for the study area
mask_list<- list.files(file.path(dataDir, "modelomraade"), pattern="shp$", full.names = TRUE)
mask<- readOGR(mask_list) #mask with DK projection

dkcrs<- CRS('+init=EPSG:25832')

#Load a mask for DK (for the resampling, less resampling)
wmask_list<-list.files(file.path(dataDir, "Mask_world"), pattern="shp$", full.names = TRUE) 
wmask<- readOGR(wmask_list) #mask with world projection

#Load a mask for EU (shorter computation time)
eumask_list<-list.files(file.path(dataDir, "Mask_EU"), pattern="shp$", full.names = TRUE)
eumask<- readOGR(eumask_list) #mask with EU projection


####DEM########################################################################
#load DEM
dem<-raster(file.path(dataDir, "Elevation/DEM.tif"))
dem<-crop(dem,extent(eumask))
dem<-mask(dem, eumask)
dem<- projectRaster(dem, crs = crsdem, method = "ngb")

crs(dem)<-dkcrs

dem<-crop(dem,extent(mask))
dem<-mask(dem, mask)

crs(dem)<-dkcrs
writeRaster(dem, file.path(saveDir, "dem.tif"), "GTiff", overwrite=T)


####CLAY CONTENT########################################################################
#Load clay
jblist<-list.files(file.path(dataDir, "DK_data/jordbund"), pattern="tif$", full.names = TRUE)
jb2<-stack(jblist)

jb2<- projectRaster(jb2, crs = crsdem, method = "ngb")
crs(jb2)<-dkcrs

for (i in 1:4) {
  origin(jb2[[i]])<-origin(dem)
}
or<-origin(dem)
origin(jb2)<-or

for (i in seq_along(jblist)) {
  writeRaster(jb2[[i]], file.path(paste0(dataDir,"/DK_data/claycontent/clay", i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 4"))
}

jblist<-list.files(file.path(dataDir, "DK_data/claycontent"), pattern="tif$", full.names = TRUE)
jb2<-stack(jblist)

reclass_df <- matrix(c(NA,0),
                     ncol=2, byrow = T)
n<-nlayers(jb2)

jb_na<-stack(jb2[[1]])

for(i in 1:n){
  jb_na[[i]]=reclassify(jb2[[i]],reclass_df)
  print(paste0(i,"/4"))
}

jb<-crop(jb_na,extent(mask))
jb<-mask(jb, mask)

for (i in seq_along(jblist)) {
  writeRaster(jb[[i]], file.path(paste0(saveDir,"/clay", i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 4"))
}

####Drainage########################################################################
#Load drainage
dclist<-list.files(file.path(dataDir, "DK_data/Drained_v3"), pattern="tif$", full.names = TRUE)
dc2<-stack(dclist)

dc2<- projectRaster(dc2, crs = crsdem, method = "ngb")
crs(dc2)<-dkcrs

dem<-raster(file.path(saveDir, "dem.tif"))
dc<- resample(dc2, dem, method="ngb")


for (i in seq_along(dclist)) {
  writeRaster(dc[[i]], file.path(paste0(dataDir,"/DK_data/drain/",names(dc[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 3"))
}

reclass_df <- matrix(c(NA,0),
                     ncol=2, byrow = T)
n<-nlayers(dc)

dc_na<-stack(dc[[1]])

for(i in 1:n){
  dc_na[[i]]=reclassify(dc[[i]],reclass_df)
  print(paste0(i,"/",n))
}

dcfin<-crop(dc_na,extent(mask))
dcfin<-mask(dcfin, mask)

for (i in seq_along(dclist)) {
  writeRaster(dcfin[[i]], file.path(paste0(saveDir,"/",names(dcfin[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 3"))
}

####PRECIP#######################################################################
#Load precipitation for all decades, give it the masks's crs
prec1990list<-list.files(file.path(dataDir, "Precipitation/wc2.1_2.5m_prec_1990-1999"), pattern="tif$", full.names = TRUE)
prec1990<-stack(prec1990list)
p1990_mask<-crop(prec1990, extent(wmask))
p1990_mask<- mask(p1990_mask, wmask)
prec1990_crs<- projectRaster(p1990_mask, crs = crsdem, method = "ngb")

crs(prec1990_crs)<-dkcrs

for (i in seq_along(prec1990list)) {
  writeRaster(prec1990_crs[[i]], file.path(paste0(dataDir,"/DK_data/precipitation90/", names(prec1990_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

p1990<- resample(prec1990_crs, dem, method="ngb")
p1990<-crop(p1990,extent(mask))
p1990<-mask(p1990, mask)

plot(p1990[[1]])

prec2000list<-list.files(file.path(dataDir, "Precipitation/wc2.1_2.5m_prec_2000-2009"), pattern="tif$", full.names = TRUE)
prec2000<-stack(prec2000list)
p2000_mask<-crop(prec2000, extent(wmask))
p2000_mask<- mask(p2000_mask, wmask)
prec2000_crs<- projectRaster(p2000_mask, crs = crsdem, method = "ngb")

crs(prec2000_crs)<-dkcrs

for (i in seq_along(prec2000list)) {
  writeRaster(prec2000_crs[[i]], file.path(paste0(dataDir,"/DK_data/precipitation00/", names(prec2000_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

p2000<- resample(prec2000_crs, dem, method="ngb")
p2000<-crop(p2000,extent(mask))
p2000<-mask(p2000, mask)

prec2010list<-list.files(file.path(dataDir, "Precipitation/wc2.1_2.5m_prec_2010-2018"), pattern="tif$", full.names = TRUE)
prec2010<-stack(prec2010list)
p2010_mask<-crop(prec2010, extent(wmask))
p2010_mask<- mask(p2010_mask, wmask)
prec2010_crs<- projectRaster(p2010_mask, crs = crsdem, method = "ngb")

crs(prec2010_crs)<-dkcrs

for (i in seq_along(prec2010list)) {
  writeRaster(prec2010_crs[[i]], file.path(paste0(dataDir,"/DK_data/precipitation10/", names(prec2010_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

p2010<- resample(prec2010_crs, dem, method="ngb")
p2010<-crop(p2010,extent(mask))
p2010<-mask(p2010, mask)


#save the study area
for (i in seq_along(prec1990list)) {
  writeRaster(p1990[[i]], file.path(paste0(saveDir,"/precipitation90/", names(p1990[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

for (i in seq_along(prec2000list)) {
  writeRaster(p2000[[i]], file.path(paste0(saveDir,"/precipitation00/", names(p2000[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

for (i in seq_along(prec2010list)) {
  writeRaster(p2010[[i]], file.path(paste0(saveDir,"/precipitation10/", names(p2010[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

####TEMPERATURE#######################################################################
#Load tmin for all decades, give it the masks's crs
dem<-raster(file.path(saveDir, "dem.tif"))
tmin1990list<-list.files(file.path(dataDir, "tmin/wc2.1_2.5m_tmin_1990-1999"), pattern="tif$", full.names = TRUE)
tmin1990<-stack(tmin1990list)
tmin1990_mask<-crop(tmin1990, extent(wmask))
tmin1990_mask<- mask(tmin1990_mask, wmask)
tmin1990_crs<- projectRaster(tmin1990_mask, crs = crsdem, method = "ngb")

crs(tmin1990_crs)<-dkcrs

for (i in seq_along(tmin1990list)) {
  writeRaster(tmin1990_crs[[i]], file.path(paste0(dataDir,"/DK_data/tmin90/", names(tmin1990_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

tmin1990<- resample(tmin1990_crs, dem, method="ngb")
tmin1990<-crop(tmin1990,extent(mask))
tmin1990<-mask(tmin1990, mask)

plot(tmin1990[[1]])

tmin2000list<-list.files(file.path(dataDir, "tmin/wc2.1_2.5m_tmin_2000-2009"), pattern="tif$", full.names = TRUE)
tmin2000<-stack(tmin2000list)
tmin2000_mask<-crop(tmin2000, extent(wmask))
tmin2000_mask<- mask(tmin2000_mask, wmask)
tmin2000_crs<- projectRaster(tmin2000_mask, crs = crsdem, method = "ngb")

crs(tmin2000_crs)<-dkcrs

for (i in seq_along(tmin2000list)) {
  writeRaster(tmin2000_crs[[i]], file.path(paste0(dataDir,"/DK_data/tmin00/", names(tmin2000_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

tmin2000<- resample(tmin2000_crs, dem, method="ngb")
tmin2000<-crop(tmin2000,extent(mask))
tmin2000<-mask(tmin2000, mask)

tmin2010list<-list.files(file.path(dataDir, "tmin/wc2.1_2.5m_tmin_2010-2018"), pattern="tif$", full.names = TRUE)
tmin2010<-stack(tmin2010list)
tmin2010_mask<-crop(tmin2010, extent(wmask))
tmin2010_mask<- mask(tmin2010_mask, wmask)
tmin2010_crs<- projectRaster(tmin2010_mask, crs = crsdem, method = "ngb")

crs(tmin2010_crs)<-dkcrs

for (i in seq_along(tmin2010list)) {
  writeRaster(tmin2010_crs[[i]], file.path(paste0(dataDir,"/DK_data/tmin10/", names(tmin2010_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 108"))
}

tmin2010<- resample(tmin2010_crs, dem, method="ngb")
tmin2010<-crop(tmin2010,extent(mask))
tmin2010<-mask(tmin2010, mask)


#save the study area
for (i in seq_along(tmin1990list)) {
  writeRaster(tmin1990[[i]], file.path(paste0(saveDir,"/tmin/", names(tmin1990[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

for (i in seq_along(tmin2000list)) {
  writeRaster(tmin2000[[i]], file.path(paste0(saveDir,"/tmin/", names(tmin2000[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

for (i in seq_along(tmin2010list)) {
  writeRaster(tmin2010[[i]], file.path(paste0(saveDir,"/tmin/", names(tmin2010[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 108"))
}

#Load tmax for all decades, give it the masks's crs
dem<-raster(file.path(saveDir, "dem.tif"))
tmax1990list<-list.files(file.path(dataDir, "tmax/wc2.1_2.5m_tmax_1990-1999"), pattern="tif$", full.names = TRUE)
tmax1990<-stack(tmax1990list)
tmax1990_mask<-crop(tmax1990, extent(wmask))
tmax1990_mask<- mask(tmax1990_mask, wmask)
tmax1990_crs<- projectRaster(tmax1990_mask, crs = crsdem, method = "ngb")

crs(tmax1990_crs)<-dkcrs

for (i in seq_along(tmax1990list)) {
  writeRaster(tmax1990_crs[[i]], file.path(paste0(dataDir,"/DK_data/tmax90/", names(tmax1990_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

tmax1990<- resample(tmax1990_crs, dem, method="ngb")
tmax1990<-crop(tmax1990,extent(mask))
tmax1990<-mask(tmax1990, mask)

plot(tmax1990[[1]])

tmax2000list<-list.files(file.path(dataDir, "tmax/wc2.1_2.5m_tmax_2000-2009"), pattern="tif$", full.names = TRUE)
tmax2000<-stack(tmax2000list)
tmax2000_mask<-crop(tmax2000, extent(wmask))
tmax2000_mask<- mask(tmax2000_mask, wmask)
tmax2000_crs<- projectRaster(tmax2000_mask, crs = crsdem, method = "ngb")

crs(tmax2000_crs)<-dkcrs

for (i in seq_along(tmax2000list)) {
  writeRaster(tmax2000_crs[[i]], file.path(paste0(dataDir,"/DK_data/tmax00/", names(tmax2000_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

tmax2000<- resample(tmax2000_crs, dem, method="ngb")
tmax2000<-crop(tmax2000,extent(mask))
tmax2000<-mask(tmax2000, mask)

tmax2010list<-list.files(file.path(dataDir, "tmax/wc2.1_2.5m_tmax_2010-2018"), pattern="tif$", full.names = TRUE)
tmax2010<-stack(tmax2010list)
tmax2010_mask<-crop(tmax2010, extent(wmask))
tmax2010_mask<- mask(tmax2010_mask, wmask)
tmax2010_crs<- projectRaster(tmax2010_mask, crs = crsdem, method = "ngb")

crs(tmax2010_crs)<-dkcrs

for (i in seq_along(tmax2010list)) {
  writeRaster(tmax2010_crs[[i]], file.path(paste0(dataDir,"/DK_data/tmax10/", names(tmax2010_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 108"))
}

tmax2010<- resample(tmax2010_crs, dem, method="ngb")
tmax2010<-crop(tmax2010,extent(mask))
tmax2010<-mask(tmax2010, mask)


#save the study area
for (i in seq_along(tmax1990list)) {
  writeRaster(tmax1990[[i]], file.path(paste0(saveDir,"/tmax90/", names(tmax1990[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

for (i in seq_along(tmax2000list)) {
  writeRaster(tmax2000[[i]], file.path(paste0(saveDir,"/tmax00/", names(tmax2000[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

for (i in seq_along(tmax2010list)) {
  writeRaster(tmax2010[[i]], file.path(paste0(saveDir,"/tmax10/", names(tmax2010[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 108"))
}

####TEMPERATURE MEAN############################################################
tmean90<-stack(tmax1990[[1]])

for (i in seq_along(tmin1990list)) {
  tmean<-mean(tmin1990[[i]], tmax1990[[i]])
  tmean90[[i]]<-tmean
  print(paste0(i, " done"))
}

tmean00<-stack(tmax2000[[1]])

for (i in seq_along(tmin2000list)) {
  tmean<-mean(tmin2000[[i]], tmax2000[[i]])
  tmean00[[i]]<-tmean
  print(paste0(i, " done"))
}

#tmean10
tmax2010list<-list.files(file.path(saveDir, "tmax10"), pattern="tif$", full.names = TRUE)
tmax2010<-stack(tmax2010list)
tmin2010list<-list.files(file.path(saveDir, "tmin10"), pattern="tif$", full.names = TRUE)
tmin2010<-stack(tmin2010list)

tmean10<-stack(tmax2010[[1]])

for (i in seq_along(tmin2010list)) {
  tmean<-mean(tmin2010[[i]], tmax2010[[i]])
  tmean10[[i]]<-tmean
  print(paste0(i, " done"))
}

for (i in seq_along(tmin1990list)) {
  writeRaster(tmean90[[i]], file.path(paste0(saveDir,"/tmean90/", names(tmin1990[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

for (i in seq_along(tmin2000list)) {
  writeRaster(tmean00[[i]], file.path(paste0(saveDir,"/tmean00/", names(tmin2000[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 120"))
}

for (i in seq_along(tmin2010list)) {
  writeRaster(tmean10[[i]], file.path(paste0(saveDir,"/tmean10/", names(tmin2010[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 108"))
}

####LANDCOVER####################################################################
#Load land cover
corinelist<-list.files(file.path(dataDir, "Corine_lc"), pattern="tif$", full.names = TRUE)
corine<-stack(corinelist)

corine<-crop(corine,extent(eumask))
corine<-mask(corine, eumask)
corine<- projectRaster(corine, crs = crsdem, method = "ngb")

crs(corine)<-dkcrs


#save
for (i in seq_along(corinelist)) {
  writeRaster(corine[[i]], file.path(paste0(dataDir,"/DK_data/landcover/", names(corine[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 5"))
}

#save study area
corinelist<-list.files(file.path(dataDir, "DK_data/landcover"), pattern="tif$", full.names = TRUE)
corine<-stack(corinelist)

corine2<- resample(corine, dem, method="ngb")
corine2<-crop(corine2,extent(mask))
corine2<-mask(corine2, mask)

#mask water bodies
water_res<-matrix(c(41,NA),
                  ncol=2, byrow = T)

lc<-stack(corine2[[1]])

for (i in seq_along(corinelist)) {
  lc[[i]]<-reclassify(corine2[[i]],water_res)
}

for (i in seq_along(corinelist)) {
  writeRaster(lc[[i]], file.path(paste0(saveDir,"/landcover/", names(lc[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 5"))
}

####Sea level 2017-2018###################################################################
sealevel<-read.csv(file.path(dataDir, "sealevel.csv"), header = TRUE, sep = "," )

sealevel$date<- as.yearmon(paste(sealevel$Year, sealevel$Month), "%Y %m")

sea_17<-sealevel%>%filter(date>= 2017)

y<-dem
s<-stack(dem)

for (i in 1:24) {
  r<-y
  values(r)<- sea_17$Monthly_MSL[[i]]
  s[[i]]<-r
}

s<-crop(s,extent(mask))
s<-mask(s, mask)

for (i in seq_along(s)) {
  writeRaster(s[[i]], file.path(paste0(saveDir,"/datafuture/sealevel/", i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 24"))
}


####SLOPE########################################################################
#Load slope
slope<-raster(file.path(dataDir, "Slope/slope.tif"))

slope<-crop(slope,extent(eumask))
slope<-mask(slope, eumask)
slope<- projectRaster(slope, crs = crsdem, method = "ngb")

crs(slope)<-dkcrs

writeRaster(slope, file.path(dataDir, "DK_data/slope.tif"), "GTiff", overwrite=T)

slope<- resample(slope, dem, method="ngb")
slope<-crop(slope,extent(mask))
slope<-mask(slope, mask)

writeRaster(slope, file.path(saveDir, "slope.tif"), "GTiff", overwrite = T)

#Slope degrees (calculated in QGIS)
dem<- raster(file.path(saveDir, "dem.tif"))
slope<-raster(file.path(dataDir, "Saga_analyses/slope_degrees.tif"))

slope_masked<-crop(slope, extent(mask))
slope_masked<-mask(slope_masked, mask)

writeRaster(slope_masked, file.path(saveDir, "slope_degrees.tif"), "GTiff", overwrite=T)


####Flow########################################################################
flow<-raster(file.path(dataDir, "Saga_analyses/Flow Accumulation.sdat"))

flow_masked<-crop(flow, extent(mask))
flow_masked<-mask(flow_masked, mask)

writeRaster(flow_masked, file.path(saveDir, "flow.tif"), "GTiff", overwrite=T)

d<-stack(flow_masked, dem)


####Distance to water###########################################################
horizontal<-raster(file.path(dataDir, "Saga_analyses/Overland Flow Distance.sdat"))
vertical<-raster(file.path(dataDir, "Saga_analyses/Vertical Overland Flow Distance.sdat"))

reclass_df <- matrix(c(NA, 0),
                     ncol=2, byrow = T)

h_reclass<-reclassify(horizontal,reclass_df)
horizontal_masked<-crop(h_reclass, extent(mask))
horizontal_masked<-mask(horizontal_masked, mask)

v_reclass<-reclassify(vertical,reclass_df)
vertical_masked<-crop(v_reclass, extent(mask))
vertical_masked<-mask(vertical_masked, mask)

writeRaster(vertical_masked, file.path(saveDir, "vdistance.tif"), "GTiff", overwrite=T)

writeRaster(horizontal_masked, file.path(saveDir, "hdistance.tif"), "GTiff", overwrite=T)


####TWI##########################################################################
twi<-raster(file.path(dataDir, "Saga_analyses/Topographic Wetness Index.sdat"))

twi_masked<-crop(twi, extent(mask))
twi_masked<-mask(twi_masked, mask)

writeRaster(twi_masked, file.path(saveDir, "twi.tif"), "GTiff", overwrite=T)


####INSOLATION###################################################################
#Load land cover
radlist<-list.files(file.path(dataDir, "Saga_analyses/insolation"), pattern="sdat$", full.names = TRUE)
rad<-stack(radlist)

crs(rad)<-dkcrs

#save study area
rad2<-crop(rad,extent(mask))
rad2<-mask(rad2, mask)

for (i in seq_along(radlist)) {
  writeRaster(rad2[[i]], file.path(paste0(saveDir,"/insolation/", names(rad2[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


####DEPTH TO CLAY################################################################
claylist<-list.files(file.path(dataDir, "Clay"), pattern="tif$", full.names = T)

clay_top<-stack(claylist, bands=1)
clay_bot<-stack(claylist, bands=2)

crs(clay_top) <- CRS('+init=EPSG:25832')
crs(clay_bot) <- CRS('+init=EPSG:25832')

#Get thickness of clay and remove areas with no clay
clay_thickness<- stack(clay_top-clay_bot)
clay_0<-stack(clay_thickness>0.1)

#Reclass 0 to NA
clay_na<-stack(clay_0[[1]])

reclass_df <- matrix(c(0, NA,
                       1, 1),
                     ncol=2, byrow = T)

n<-nlayers(clay_0)

for(i in 1:n){
  clay_na[[i]]=reclassify(clay_0[[i]],reclass_df)
}



#Calculate the top depth of each clay layer
clay_depth<-stack(clay_na*clay_top)

#Merge the layers by keeping the highest value (top depth, closer to surface)
b <- brick(clay_depth)

clay_max<-stackApply(b, indices = rep(1, nlayers(b)), fun = max)


#Checkpoint
writeRaster(clay_max, file.path(dataDir, "clay_depth.tif"), "GTiff", overwrite=TRUE)

#Resample, clip
reclass_df <- matrix(c(NA, -999),
                     ncol=2, byrow = T)


clay_reclass=reclassify(clay_max,reclass_df)

clay_res<-resample(clay_reclass, dem, method="ngb")
clay_masked<-crop(clay_res,extent(mask))
clay_masked<-mask(clay_masked, mask)

writeRaster(clay_masked, file.path(saveDir, "clay_depth.tif"), "GTiff", overwrite=T)


####Imperviousness##############################################################
#load 2018 raster (extent of this raster is for the whole Europe, clip to the other imper extent and process all together)
im2018<-raster(file.path(dataDir, "imperviousness/Europe/IMD_2018_100m_eu_03035_V2_0.tif"))
im<-raster(file.path(dataDir, "imperviousness/IMD_2015_100m_eu_03035_d03_E40N30.tif"))

im2018<- crop(im2018, extent(im))
im2018<- mask(im2018, im)

writeRaster(im2018, file.path(dataDir, "imperviousness/im2018.tif"), "GTiff", overwrite=T)

#load all imperv rasters in a stack
imperlist<-list.files(file.path(dataDir, "imperviousness"), pattern="tif$", full.names = TRUE)
imper<-stack(imperlist)

imper<-crop(imper,extent(eumask))
imper<-mask(imper, eumask)
imper<- projectRaster(imper, crs = crsdem, method="ngb")

crs(imper)<-dkcrs

for (i in seq_along(imperlist)) {
  writeRaster(imper[[i]], file.path(paste0(dataDir,"/DK_data/imperviousness/", names(imper[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 5"))
}

#save study area
imper<- resample(imper, dem, method="ngb")
imper<-crop(imper,extent(mask))
imper<-mask(imper, mask)

for (i in seq_along(imperlist)) {
  writeRaster(imper[[i]], file.path(paste0(saveDir,"/imperviousness/", names(imper[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 5"))
}


####SOIL TYPE###########################################################################
jordlist<-list.files(file.path(dataDir, "DK_data/Jord"), pattern="shp$", full.names = TRUE)
jord<-st_read(jordlist)

jord$group_id <- jord %>% group_indices(tsym) #GIVE THE DATA AN ID BASED ON THE TYPE OF SOIL

#Rasterize
r<-raster(extent(dem), crs= '+init=EPSG:25832') ##make an empty raster as clay thickness
res(r) = res(dem) #pixel size

jordv<-jord$group_id #vector with the values that will be transferred to the raster
jordart_raster <- rasterize(jord,r, jordv) #rasterize shp to raster

#crop again to the study area
jordart <- crop(jordart_raster, extent(mask))
jordart<- mask(jordart, mask)

st_write(jord, file.path(saveDir, "jordart_shp"), "jordart.shp", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=T, append = F)
writeRaster(jordart, file.path(saveDir, "jordart.tif"), "GTiff", overwrite = T)


####Water bodies#####################################################################
dem<-raster(file.path(dataDir, "Predictors_DK/dem.tif"))
water<-raster(file.path(dataDir, "Vandkortf_10m_1bit.tif"))

water_res<- resample(water, dem, method="bilinear")

reclass_df <- matrix(c(NA, 0,
                       1, 1),
                     ncol=2, byrow = T)

water_reclass<-reclassify(water_res,reclass_df)

water_re <- crop(water_reclass, extent(mask))
water_re<- mask(water_re, mask)

writeRaster(water_re, file.path(saveDir, "water.tif"), "GTiff", overwrite = T)

#Not used
reclass_NA <- matrix(c(NA, 1),
                     ncol=2, byrow = T)
water_reclass2<-reclassify(water_re, reclass_NA) #this one has sea, use for saga analyses

reclass_0 <- matrix(c(0, NA),
                    ncol=2, byrow = T)
water_sea<-reclassify(water_reclass2, reclass_0)

writeRaster(water_reclass2, file.path(dataDir, "Saga_analyses/water_sea.tif"), "GTiff", overwrite=TRUE)
writeRaster(water_sea, file.path(dataDir, "Saga_analyses/water_sea_NA.tif"), "GTiff", overwrite=TRUE)



####X and Y utm coordinates########################################################
predictors_feb[[1]]

xmn<-xmin(dem)
xmx<-xmax(dem)
ymn<-ymin(dem)
ymx<-ymax(dem)

xm<-matrix(xFromCell(dem,c(1:5525370)),nrow=2523,byrow=TRUE)
x<-raster(xm,xmn=xmn, xmx=xmx,ymn=ymn,ymx=ymx)
x
dem
crs(x)<-dkcrs

x<-crop(x,extent(mask))
x<-mask(x, mask)

writeRaster(x, file.path(saveDir,"xutm.tif"), "GTiff", overwrite=TRUE)

ym<-matrix(yFromCell(dem,c(1:5525370)),nrow=2523,byrow=TRUE)
y<-raster(ym,xmn=xmn, xmx=xmx,ymn=ymn,ymx=ymx)
y
plot(y)
crs(y)<-dkcrs

y<-crop(y,extent(mask))
y<-mask(y, mask)

writeRaster(y, file.path(saveDir,"yutm.tif"), "GTiff", overwrite=TRUE)


####Month cumulative###########################################################
#2017
y<-dem
s<-stack(dem)

for (i in 1:12) {
  r<-y
  values(r)<- 324+i
  s[[i]]<-r
}

s<-crop(s,extent(mask))
s<-mask(s, mask)

for (i in seq_along(s)) {
  writeRaster(s[[i]], file.path(paste0(saveDir,"/datafuture/month/2017_", i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}

#2018
s<-stack(dem)

for (i in 1:12) {
  r<-y
  values(r)<- 336+i
  s[[i]]<-r
}

s<-crop(s,extent(mask))
s<-mask(s, mask)

for (i in seq_along(s)) {
  writeRaster(s[[i]], file.path(paste0(saveDir,"/datafuture/month/2018_", i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}

#2090
y<-dem
s<-stack(dem)

for (i in 1:12) {
  r<-y
  values(r)<- 1201+i
  s[[i]]<-r
}

s<-crop(s,extent(mask))
s<-mask(s, mask)

for (i in seq_along(s)) {
  writeRaster(s[[i]], file.path(paste0(saveDir,"/datafuture/2100_month/month/2090_", i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


####Future climate scenarios########################################################
####FUTURE TMIN####-------------------------------------------------------------------
#245
dem<-raster(file.path(saveDir, "dem.tif"))
tmin245<-stack(file.path(dataDir, "2100/ssp245/wc2.1_2.5m_tmin_BCC-CSM2-MR_ssp245_2081-2100.tif"))

tmin245_mask<-crop(tmin245, extent(wmask))
tmin245_mask<- mask(tmin245_mask, wmask)
tmin245_crs<- projectRaster(tmin245_mask, crs = crsdem, method = "ngb")

crs(tmin245_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(tmin245_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp245/tmin/", names(tmin245_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


#370
dem<-raster(file.path(saveDir, "dem.tif"))
tmin370<-stack(file.path(dataDir, "2100/ssp370/wc2.1_2.5m_tmin_BCC-CSM2-MR_ssp370_2081-2100.tif"))

tmin370_mask<-crop(tmin370, extent(wmask))
tmin370_mask<- mask(tmin370_mask, wmask)
tmin370_crs<- projectRaster(tmin370_mask, crs = crsdem, method = "ngb")

crs(tmin370_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(tmin370_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp370/tmin/", names(tmin370_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


#585
dem<-raster(file.path(saveDir, "dem.tif"))
tmin585<-stack(file.path(dataDir, "2100/ssp585/wc2.1_2.5m_tmin_BCC-CSM2-MR_ssp585_2081-2100.tif"))

tmin585_mask<-crop(tmin585, extent(wmask))
tmin585_mask<- mask(tmin585_mask, wmask)
tmin585_crs<- projectRaster(tmin585_mask, crs = crsdem, method = "ngb")

crs(tmin585_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(tmin585_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp585/tmin/", names(tmin585_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


####FUTURE TMAX####-------------------------------------------------------------------
#245
dem<-raster(file.path(saveDir, "dem.tif"))
tmax245<-stack(file.path(dataDir, "2100/ssp245/wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp245_2081-2100.tif"))

tmax245_mask<-crop(tmax245, extent(wmask))
tmax245_mask<- mask(tmax245_mask, wmask)
tmax245_crs<- projectRaster(tmax245_mask, crs = crsdem, method = "ngb")

crs(tmax245_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(tmax245_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp245/tmax/", names(tmax245_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


#370
dem<-raster(file.path(saveDir, "dem.tif"))
tmax370<-stack(file.path(dataDir, "2100/ssp370/wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp370_2081-2100.tif"))

tmax370_mask<-crop(tmax370, extent(wmask))
tmax370_mask<- mask(tmax370_mask, wmask)
tmax370_crs<- projectRaster(tmax370_mask, crs = crsdem, method = "ngb")

crs(tmax370_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(tmax370_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp370/tmax/", names(tmax370_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


#585
dem<-raster(file.path(saveDir, "dem.tif"))
tmax585<-stack(file.path(dataDir, "2100/ssp585/wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp585_2081-2100.tif"))

tmax585_mask<-crop(tmax585, extent(wmask))
tmax585_mask<- mask(tmax585_mask, wmask)
tmax585_crs<- projectRaster(tmax585_mask, crs = crsdem, method = "ngb")

crs(tmax585_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(tmax585_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp585/tmax/", names(tmax585_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


####FUTURE TMEAN####------------------------------------------------------
#245
tmean245<-stack(tmax245_crs[[1]])

for (i in 1:12) {
  tmean<-mean(tmin245_crs[[i]], tmax245_crs[[i]])
  tmean245[[i]]<-tmean
  print(paste0(i, " done"))
}
plot(tmean245[[1]])

for (i in 1:12) {
  writeRaster(tmean245[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp245/tmean/tmean",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


tmean245_res<- resample(tmean245, dem, method="ngb")
tmean245_m<-crop(tmean245_res,extent(mask))
tmean245_m<-mask(tmean245_m, mask)

plot(tmean245_m[[1]])


for (i in 1:12) {
  writeRaster(tmean245_m[[i]], file.path(paste0(dataDir,"/Predictors_DK/datafuture/2100_month/ssp245/tmean/tmean",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


#370
tmean370<-stack(tmax370_crs[[1]])

for (i in 1:12) {
  tmean<-mean(tmin370_crs[[i]], tmax370_crs[[i]])
  tmean370[[i]]<-tmean
  print(paste0(i, " done"))
}
plot(tmean370[[1]])

for (i in 1:12) {
  writeRaster(tmean370[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp370/tmean/tmean",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


tmean370_res<- resample(tmean370, dem, method="ngb")
tmean370_m<-crop(tmean370_res,extent(mask))
tmean370_m<-mask(tmean370_m, mask)

plot(tmean370_m[[1]])


for (i in 1:12) {
  writeRaster(tmean370_m[[i]], file.path(paste0(dataDir,"/Predictors_DK/datafuture/2100_month/ssp370/tmean/tmean",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


#585
tmean585<-stack(tmax585_crs[[1]])

for (i in 1:12) {
  tmean<-mean(tmin585_crs[[i]], tmax585_crs[[i]])
  tmean585[[i]]<-tmean
  print(paste0(i, " done"))
}
plot(tmean585[[1]])

for (i in 1:12) {
  writeRaster(tmean585[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp585/tmean/tmean",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


tmean585_res<- resample(tmean585, dem, method="ngb")
tmean585_m<-crop(tmean585_res,extent(mask))
tmean585_m<-mask(tmean585_m, mask)

plot(tmean585_m[[1]])


for (i in 1:12) {
  writeRaster(tmean585_m[[i]], file.path(paste0(dataDir,"/Predictors_DK/datafuture/2100_month/ssp585/tmean/tmean",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


####FUTURE PRECIPITATION MONTH######------------------------------------------------
#245
dem<-raster(file.path(saveDir, "dem.tif"))
prec245<-stack(file.path(dataDir, "2100/ssp245/wc2.1_2.5m_prec_BCC-CSM2-MR_ssp245_2081-2100.tif"))

prec245_mask<-crop(prec245, extent(wmask))
prec245_mask<- mask(prec245_mask, wmask)
prec245_crs<- projectRaster(prec245_mask, crs = crsdem, method = "ngb")

crs(prec245_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(prec245_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp245/precip/", names(prec245_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


prec245_res<- resample(prec245_crs, dem, method="ngb")
prec245_m<-crop(prec245_res,extent(mask))
prec245_m<-mask(prec245_m, mask)

plot(prec245_m[[1]])


for (i in 1:12) {
  writeRaster(prec245_m[[i]], file.path(paste0(dataDir,"/Predictors_DK/datafuture/2100_month/ssp245/precip/precip",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


#370
dem<-raster(file.path(saveDir, "dem.tif"))
prec370<-stack(file.path(dataDir, "2100/ssp370/wc2.1_2.5m_prec_BCC-CSM2-MR_ssp370_2081-2100.tif"))

prec370_mask<-crop(prec370, extent(wmask))
prec370_mask<- mask(prec370_mask, wmask)
prec370_crs<- projectRaster(prec370_mask, crs = crsdem, method = "ngb")

crs(prec370_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(prec370_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp370/precip/", names(prec370_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


prec370_res<- resample(prec370_crs, dem, method="ngb")
prec370_m<-crop(prec370_res,extent(mask))
prec370_m<-mask(prec370_m, mask)

plot(prec370_m[[1]])


for (i in 1:12) {
  writeRaster(prec370_m[[i]], file.path(paste0(dataDir,"/Predictors_DK/datafuture/2100_month/ssp370/precip/precip",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


#585
dem<-raster(file.path(saveDir, "dem.tif"))
prec585<-stack(file.path(dataDir, "2100/ssp585/wc2.1_2.5m_prec_BCC-CSM2-MR_ssp585_2081-2100.tif"))

prec585_mask<-crop(prec585, extent(wmask))
prec585_mask<- mask(prec585_mask, wmask)
prec585_crs<- projectRaster(prec585_mask, crs = crsdem, method = "ngb")

crs(prec585_crs)<-dkcrs

for (i in 1:12) {
  writeRaster(prec585_crs[[i]], file.path(paste0(dataDir,"/DK_data/2100_month/ssp585/precip/", names(prec585_crs[[i]]), ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


prec585_res<- resample(prec585_crs, dem, method="ngb")
prec585_m<-crop(prec585_res,extent(mask))
prec585_m<-mask(prec585_m, mask)

plot(prec585_m[[1]])


for (i in 1:12) {
  writeRaster(prec585_m[[i]], file.path(paste0(dataDir,"/Predictors_DK/datafuture/2100_month/ssp585/precip/precip",i, ".tif")), "GTiff", overwrite=TRUE)
  print(paste0("Saved ", i, " out of 12"))
}


