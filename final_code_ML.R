####MACHINE LEARNING PREP####################################################################################################
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(raster)


mainDir<- "C:/Users/rebe_/Desktop/thesis"
dataDir<- "C:/Users/rebe_/Desktop/thesis/Data"
predDir<- "C:/Users/rebe_/Desktop/thesis/Data/Predictors_DK"

p<-read_sf(file.path(mainDir, "data/boreholes/monthly_2018.shp")) #load monthly gw observations
p$date <- as.yearmon(paste(p$year, p$month), "%Y %m") #transform date to correct date format

pointsfinal<-st_cast(p, "POINT") #only to convert multipoint to point, extract doesn't work with multipoint


####Extract monthly precipitation data##########################################
dates<-subset(pointsfinal, select=c("date")) #get date column only
datelist<-dates[!duplicated(dates$date), ] #remove duplicates
datelist2<- datelist %>% filter(date>=2000)
datelist3<- datelist %>% filter(date>=2010) #up to 2018


prec1990list<-list.files(file.path(mainDir, "Data/Predictors_DK/precip90_int"), pattern="tif$", full.names = TRUE)
p1990<-stack(prec1990list)

prec2000list<-list.files(file.path(mainDir, "Data/Predictors_DK/precip00_int"), pattern="tif$", full.names = TRUE)
p2000<-stack(prec2000list)

prec2010list<-list.files(file.path(mainDir, "Data/Predictors_DK/precip10_int"), pattern="tif$", full.names = TRUE)
p2010<-stack(prec2010list)


data1990<-data.frame() #create an empty frame to add the resulting data from each iteration

datelist = datelist[order(datelist$date),] #order by date so the loop iterations go in order
head(datelist)

#loop to extract precipitation data for each month from 1990 to 1999
for (i in seq_along(prec1990list)) {
  sub<- subset(pointsfinal, date==datelist$date[[i]]) #subset gw observations by month
  sub$ID <- seq.int(nrow(sub)) #give the observations a unique ID
  data<-raster::extract(p1990[[i]], sub, df=T) #extract precipitation for each observation
  pder <- merge(sub, data, by.x="ID", by.y="ID") #merge extracted precipitation with gw observations
  names(pder)[9] <- "precip"
  data1990<-rbind(data1990, pder) #merge dataframe with all months/years
  print(paste0("Extracted ", i, " out of ", length(prec1990list)))
}

#Same as before for 2000-2009
data2000<-data.frame()
datelist2 = datelist2[order(datelist2$date),]

for (i in seq_along(prec2000list)) {
  sub<- subset(pointsfinal, date==datelist2$date[[i]])
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(p2000[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[9] <- "precip"
  data2000<-rbind(data2000, pder)
  print(paste0("Extracted ", i, " out of ", length(prec2000list)))
}

#Same as before for 2010-2018
data2010<-data.frame()
datelist3 = datelist3[order(datelist3$date),]

for (i in seq_along(datelist3$date)) {
  sub<- subset(pointsfinal, date==datelist3$date[[i]])
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(p2010[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[9] <- "precip"
  data2010<-rbind(data2010, pder)
  print(paste0("Extracted ", i, " out of ", length(prec2010list)))
}

precdata<-rbind(data1990,data2000,data2010) #points from 1990 to 2018 with monthly precipitation extracted

####CORINE Landcover############################################################
corinelist<-list.files(file.path(mainDir, "Data/Predictors_DK/landcover"), pattern="tif$", full.names = TRUE)
corine<-stack(corinelist)

lc<-data.frame()

corine1<-pointsfinal %>% filter(year <= 1999)
corine1$ID <- seq.int(nrow(corine1))
extr<-extract(corine[[1]], corine1, df=T)
pder1 <- merge(corine1, extr, by.x="ID", by.y="ID")
names(pder1)[9] <- "landcover"

corine2<-pointsfinal %>% filter(year >= 2000 & year <= 2005)
corine2$ID <- seq.int(nrow(corine2))
extr2<-extract(corine[[2]], corine2, df=T)
pder2 <- merge(corine2, extr2, by.x="ID", by.y="ID")
names(pder2)[9] <- "landcover"

corine3<-pointsfinal %>% filter(year >= 2006 & year<= 2011)
corine3$ID <- seq.int(nrow(corine3))
extr3<-extract(corine[[3]], corine3, df=T)
pder3 <- merge(corine3, extr3, by.x="ID", by.y="ID")
names(pder3)[9] <- "landcover"

corine4<-pointsfinal %>% filter(year>= 2012 & year <= 2017)
corine4$ID <- seq.int(nrow(corine4))
extr4<-extract(corine[[4]], corine4, df=T)
pder4<- merge(corine4, extr4, by.x="ID", by.y="ID")
names(pder4)[9] <- "landcover"

corine5<-pointsfinal %>% filter(year == 2018)
corine5$ID <- seq.int(nrow(corine5))
extr5<-extract(corine[[5]], corine5, df=T)
pder5<- merge(corine5, extr5, by.x="ID", by.y="ID")
names(pder5)[9] <- "landcover"

lc<-rbind(pder1, pder2, pder3, pder4, pder5)

lcdf<- lc[c(2,8,9)]
lcdf<-as.data.frame(lcdf[c(1,2,3)])
lcdf<-as.data.frame(lcdf[c(1,2,3)])

lcdata<-merge(precdata, lcdf, by= c("boreholeid","date"))


####IMPERVIOUSNESS#############################################################
implist<-list.files(file.path(mainDir, "Data/Predictors_DK/imperviousness"), pattern="tif$", full.names = TRUE)
imper<-stack(implist)

imp<-data.frame()

imper1<-pointsfinal %>% filter(year <= 2006)
imper1$ID <- seq.int(nrow(imper1))
extr<-extract(imper[[2]], imper1, df=T)
pder1 <- merge(imper1, extr, by.x="ID", by.y="ID")
names(pder1)[9] <- "imperv"

imper2<-pointsfinal %>% filter(year >= 2007 & year <= 2009)
imper2$ID <- seq.int(nrow(imper2))
extr2<-extract(imper[[3]], imper2, df=T)
pder2 <- merge(imper2, extr2, by.x="ID", by.y="ID")
names(pder2)[9] <- "imperv"

imper3<-pointsfinal %>% filter(year >= 2010 & year<= 2012)
imper3$ID <- seq.int(nrow(imper3))
extr3<-extract(imper[[4]], imper3, df=T)
pder3 <- merge(imper3, extr3, by.x="ID", by.y="ID")
names(pder3)[9] <- "imperv"

imper4<-pointsfinal %>% filter(year>= 2013 & year <= 2015)
imper4$ID <- seq.int(nrow(imper4))
extr4<-extract(imper[[5]], imper4, df=T)
pder4<- merge(imper4, extr4, by.x="ID", by.y="ID")
names(pder4)[9] <- "imperv"

imper5<-pointsfinal %>% filter(year >= 2016)
imper5$ID <- seq.int(nrow(imper5))
extr5<-extract(imper[[1]], imper5, df=T)
pder5<- merge(imper5, extr5, by.x="ID", by.y="ID")
names(pder5)[9] <- "imperv"

imp<-rbind(pder1, pder2, pder3, pder4, pder5)

imdf<- as.data.frame(imp[c(2,8,9)])
imdf<- as.data.frame(imdf[c(1,2,3)])

imdata<-merge(lcdata, imdf, by= c("boreholeid","date"))

####Extract monthly tmin data##########################################
dates<-subset(pointsfinal, select=c("date")) #get date column only
datelist<-dates[!duplicated(dates$date), ] #remove duplicates
datelist2<- datelist %>% filter(date>=2000)
datelist3<- datelist %>% filter(date>=2010) #up to 2018


tminlist<-list.files(file.path(mainDir, "Data/Predictors_DK/tmin"), pattern="tif$", full.names = TRUE)
tmin<-stack(tminlist)


tmindf<-data.frame() #create an empty frame to add the resulting data from each iteration

datelist = datelist[order(datelist$date),] #order by date so the loop iterations go in order
head(datelist)

#loop to extract data for each month from 1990 to 1999
for (i in seq_along(datelist$date)) {
  sub<- subset(pointsfinal, date==datelist$date[[i]])
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(tmin[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[9] <- "tmin"
  tmindf<-rbind(tmindf, pder)
  print(paste0("Extracted ", i, " out of ", length(datelist$date)))
}

datadf<- as.data.frame(tmindf[c(2,8,9)])
datadf<- as.data.frame(datadf[c(1,2,3)])
tmindata<-merge(imdata, datadf, by= c("boreholeid","date"))

####Extract monthly tmax data##########################################
dates<-subset(pointsfinal, select=c("date")) #get date column only
datelist<-dates[!duplicated(dates$date), ] #remove duplicates

tmaxlist<-list.files(file.path(mainDir, "Data/Predictors_DK/tmax"), pattern="tif$", full.names = TRUE)
tmax<-stack(tmaxlist)

tmaxdf<-data.frame() #create an empty frame to add the resulting data from each iteration

datelist = datelist[order(datelist$date),] #order by date so the loop iterations go in order
head(datelist)

#loop to extract data for each month from 1990 to 1999
for (i in seq_along(datelist$date)) {
  sub<- subset(pointsfinal, date==datelist$date[[i]])
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(tmax[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[9] <- "tmax"
  tmaxdf<-rbind(tmaxdf, pder)
  print(paste0("Extracted ", i, " out of ", length(datelist$date)))
}

data2df<- as.data.frame(tmaxdf[c(2,8,9)])
data2df<- as.data.frame(data2df[c(1,2,3)])
tmaxdata<-merge(tmindata, data2df, by= c("boreholeid","date"))

####Extract monthly tmean data##########################################
dates<-subset(pointsfinal, select=c("date")) #get date column only
datelist<-dates[!duplicated(dates$date), ] #remove duplicates
datelist2<- datelist %>% filter(date>=2000)
datelist3<- datelist %>% filter(date>=2010) #up to 2018


tmean90list<-list.files(file.path(mainDir, "Data/Predictors_DK/tmean90"), pattern="tif$", full.names = TRUE)
tmean90<-stack(tmean90list)

tmean00list<-list.files(file.path(mainDir, "Data/Predictors_DK/tmean00"), pattern="tif$", full.names = TRUE)
tmean00<-stack(tmean00list)

tmean10list<-list.files(file.path(mainDir, "Data/Predictors_DK/tmean10"), pattern="tif$", full.names = TRUE)
tmean10<-stack(tmean10list)


data1990<-data.frame() #create an empty frame to add the resulting data from each iteration

datelist = datelist[order(datelist$date),] #order by date so the loop iterations go in order
head(datelist)

#loop to extract data for each month from 1990 to 1999
for (i in seq_along(tmean90list)) {
  sub<- subset(pointsfinal, date==datelist$date[[i]])
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(tmean90[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[9] <- "tmean"
  data1990<-rbind(data1990, pder)
  print(paste0("Extracted ", i, " out of ", length(tmean90list)))
}

#Same as before for 2000-2009
data2000<-data.frame()
datelist2 = datelist2[order(datelist2$date),]

for (i in seq_along(tmean00list)) {
  sub<- subset(pointsfinal, date==datelist2$date[[i]])
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(tmean00[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[9] <- "tmean"
  data2000<-rbind(data2000, pder)
  print(paste0("Extracted ", i, " out of ", length(tmean00list)))
}

#Same as before for 2010-2018
data2010<-data.frame()
datelist3 = datelist3[order(datelist3$date),]

for (i in seq_along(datelist3$date)) {
  sub<- subset(pointsfinal, date==datelist3$date[[i]])
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(tmean10[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[9] <- "tmean"
  data2010<-rbind(data2010, pder)
  print(paste0("Extracted ", i, " out of ", length(tmean10list)))
}

tmeandf<-rbind(data1990,data2000,data2010) #points from 1990 to 2018 with monthly temp extracted

data3df<- as.data.frame(tmeandf[c(2,8,9)])
data3df<- as.data.frame(data3df[c(1,2,3)])
tmeandata<-merge(tmaxdata, data3df, by= c("boreholeid","date"))


####INSOLATION################################################################################
inslist<-list.files(file.path(mainDir, "Data/Predictors_DK/insolation"), pattern="tif$", full.names = TRUE)
ins<-stack(inslist)

insdf<-data.frame() #create an empty frame to add the resulting data from each iteration

#loop to extract data for each month from 1990 to 1999
for (i in 1:12) {
  sub<- subset(pointsfinal, month==i)
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(ins[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[9] <- "ins"
  insdf<-rbind(insdf, pder)
  print(paste0("Extracted ", i, " out of 12"))
}

data4df<- as.data.frame(insdf[c(2,8,9)])
data4df<- as.data.frame(data4df[c(1,2,3)])
insdata<-merge(tmeandata, data4df, by= c("boreholeid","date"))


####SLOPE,DEM, single predictors##############################################################
#extract the rest of the data
slopedemlist<-list.files(file.path(mainDir, "Data/Predictors_DK"), pattern="tif$", full.names = TRUE)
slopedem<-stack(slopedemlist)

datextract<-insdata
datextract$ID<-seq.int(nrow(datextract))
sdemex<-extract(slopedem,datextract, df=T)
pfin<-merge(datextract,sdemex, by.x="ID", by.y="ID")


####Add cumulative month#######################################################
#use the date list and give it an ID, then merge back to the df by date
datelist = datelist[order(datelist$date),] #order by date so the loop iterations go in order
datelist$monthcum<-seq.int(nrow(datelist))

datelistdf<-as.data.frame(datelist[c(1,3)])
datelistdf<-as.data.frame(datelistdf[c(1,2)])

pfin_date<-merge(pfin,datelistdf, by=c("date"))


####Sea level###################################################################
sealevel<-read.csv(file.path(dataDir, "sealevel.csv"), header = TRUE, sep = "," )

sealevel$date<- as.yearmon(paste(sealevel$Year, sealevel$Month), "%Y %m")
sea_fin<-sealevel

sea_fin[,c(1,2,4,5,6)]<-NULL

#change name of sea level column 
names(sea_fin)[1] <- "sealevel"

#merge to the borehole data by date
pfin_sea<-merge(pfin_date,sea_fin, by="date", all.x=T)

st_write(pfin_sea, file.path(mainDir, "data/boreholes/extract_sea_temp2.shp"), delete_layer = TRUE) #final shapefile


####ML ALGORITHMS###############################################################
rm(list=ls()) #clean environment

library(caret)
library(factoextra)
library("corrplot")

mainDir<- "C:/Users/rebe_/Desktop/thesis"
dataDir<- "C:/Users/rebe_/Desktop/thesis/Data"
predDir<- "C:/Users/rebe_/Desktop/thesis/Data/Predictors_DK"

#Load the shapefile and make the date field again
pfin<-read_sf(file.path(dataDir, "boreholes/extract_sea_temp.shp"))
pfin$date <- as.yearmon(paste(pfin$year, pfin$month), "%Y %m")
pder_clean <- na.omit(pfin)  #omit rows with NA values in them - necessary for the models
pder_clean<-pder_clean%>%filter(numMean>-5) #outliers



####Partition of the data and delete extra columns
trainDat<-pder_clean%>% filter(date<=2016)
trainfactor<-trainDat
trainfactor[,c("ID", "boreholeid", "year", "month","geometry", "date","water","tmin","tmax")]<-NULL
trainfactor<-as.data.frame(trainfactor)
cor.factor<-as.data.frame(trainDat[,c(1:30)])

testDat<-pder_clean%>% filter(date>=2017 & date<= "Dec 2018")
testfactor<-testDat
testfactor[,c("ID", "boreholeid", "year", "month","geometry", "date","water","tmin","tmax")]<-NULL
testfactor<-as.data.frame(testfactor)


predictors <- c("xutm","yutm","precip","landcover","imperv","tmean","ins",
                "cly_dpt","clay1","clay2","clay3","clay4","dc","dem","hdistance","jordart","slp_dgr","flow","twi","vdistance",
                "monthcum","sealevel")



####PCA/Correlation, importance of the variables#################################

c<-cor(cor.factor[,c(6:30)]) #check correlation 
corrplot(c, is.corr = T)

#pca
samp2 <- trainfactor[,-1]
pcadat<-trainfactor

#PCA
pcadat[,c("landcover", "dc", "jordart")]<-NULL #exclude categorical variables

data.pca<- prcomp(pcadat[,c(2:20)], center = T, scale. = T) #perform pca

#visualize pca results
eig.val <- get_eigenvalue(data.pca) #get the eigenvalues (pcs)/ can also be seen with summary(data.pca)

fviz_eig(data.pca, addlabels = TRUE, ylim = c(0, 50)) #plot the eigenvalues (only first 10)

var <- get_pca_var(data.pca) #extract results for variables

var$contrib

corrplot(var$cos2, is.corr=FALSE) #plots quality of representation of each var to the pcs

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(data.pca, choice = "var", axes = 1:5)

#Contribution of vars
var$contrib

corrplot(var$contrib, is.corr=FALSE)

fviz_contrib(data.pca, choice = "var", axes = 1:3) #contribution to pc1 and pc2, which are the most important

#plot the contribution to pc1 and pc2
fviz_pca_var(data.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


################################################################################
#post-pca processing
pcatrain<-trainfactor
pcatest<-testfactor

pcatrain[,c("sealevel", "flow")]<-NULL
pcatest[,c("sealevel", "flow")]<-NULL

pcapredictors <- c("xutm","yutm","precip","landcover","imperv","tmean","ins",
                   "cly_dpt","clay1","clay2","clay3","clay4","dc","dem","hdistance",
                   "jordart","slp_dgr","twi","vdistance","monthcum")



################################################################################
#Model excluding pca vars
set.seed(10)

set.seed(10)
pcarfmodel <- caret::train(pcatrain[,pcapredictors],pcatrain$numMean,
                            method="rf",
                            metric="Rsquared",
                            ntree=1000,
                            tuneLength=10,
                            importance=TRUE,
                            trControl=trainControl(method="cv",number=5, search = "random"))

print(pcarfmodel)
plot(varImp(pcarfmodel))

#evaluation
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

predf$pred = pcarfmodel %>% predict(pcatest[,-1])

predf$error<-predf$numMean-predf$pred
predf$aerror<-abs(predf$error) #MAE
mean(predf2$aerror)
RSQUARE(predf$numMean,predf$pred) #R2

histogram(predf$error)

st_write(predf, file.path(mainDir, "Data/Results/pcarfmodel.shp"), delete_layer = TRUE) # overwrites



#------------------------------------------------------------------------------------------#

#SVM model#----------------------------------------------------------------------
set.seed(10)
pcasvmmodel<-caret::train(pcatrain[,pcapredictors],pcatrain$numMean,
                          method="svmRadial",
                          metric="Rsquared",
                          tuneLength=10,
                          importance=TRUE,
                          trControl=trainControl(method="cv",search="random",number=5))
pcasvmmodel

#evaluation
svmpred<-pcatest

svmpred$pred = pcasvmmodel %>% predict(pcatest[,-1])


svmpred$error<-svmpred$numMean-svmpred$pred
svmpred$aerror<-abs(svmpred$error)
mean(svmpred$aerror)

RSQUARE(svmpred$numMean,svmpred$pred)


#NN model#-----------------------------------------------------------------------
set.seed(10)
pcannetmodel<-caret::train(pcatrain[,pcapredictors],pcatrain$numMean,
                           method="avNNet",
                           preProcess = c('center', 'scale'),
                           metric="Rsquared",
                           tuneLength=10,
                           importance=TRUE,
                           trControl=trainControl(method="cv",search="random",number=5),
                           linout = TRUE,
                           maxit = 1000)

pcannetmodel

plot(varImp(pcannetmodel))


annpred<-pcatest

annpred$pred = pcannetmodel %>% predict(pcatest[,-1])


annpred$error<-annpred$numMean-annpred$pred
annpred$aerror<-abs(annpred$error)
mean(annpred$aerror)

RSQUARE(annpred$numMean,annpred$pred)


##RF by landcover
#Agricultural
trainagric<- pcatrain%>% filter(landcover>=12 & landcover<=22)

set.seed(10)
agrimodel<-caret::train(trainagric[,pcapredictors],trainagric$numMean,
                        method="rf",
                        metric="Rsquared",
                        tuneLength=10,
                        importance=TRUE,
                        trControl=trainControl(method="cv",search="random",number=5))

plot(varImp(agrimodel))

#Urban
trainurban<- pcatrain%>% filter(landcover<=11)

set.seed(10)
urbanmodel<-caret::train(trainurban[,pcapredictors],trainurban$numMean,
                         method="rf",
                         metric="Rsquared",
                         tuneLength=10,
                         importance=TRUE,
                         trControl=trainControl(method="cv",search="random",number=5))

plot(varImp(urbanmodel))


#Nature
trainnature<- pcatrain%>% filter(landcover>=23 & landcover<=29)

set.seed(10)
naturemodel<-caret::train(trainnature[,pcapredictors],trainnature$numMean,
                          method="rf",
                          metric="Rsquared",
                          tuneLength=10,
                          importance=TRUE,
                          trControl=trainControl(method="cv",search="random",number=5))

plot(varImp(naturemodel))


#Predict maps 2018
predictorslist18 <- list.files(file.path(mainDir, "Data/Predictors_DK/datafuture"), pattern="tif$", full.names = TRUE)
preciplist18 <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/precip"), pattern="tif$", full.names = TRUE)
tmeanlist18 <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/tmean"), pattern="tif$", full.names = TRUE)
monthlist18 <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/month"), pattern="tif$", full.names = TRUE)
inslist18<-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/insolation"), pattern="tif$", full.names = TRUE)

predictors18<-stack(predictorslist18)
names(predictors18)
names(predictors18[[1]])<-"cly_dpt"
names(predictors18[[12]])<-"slp_dgr"

precip18<-stack(preciplist18)
tmean18<-stack(tmeanlist18)
month18<-stack(monthlist18)
ins18<-stack(inslist18)


#Make all the predictions for 2017-2018
all_preds18<-stack(predictors18[[1]]) #make a stack to use later

for (i in 1:24) {
  names(precip18[[i]])<-"precip"
  names(tmean18[[i]])<-"tmean"
  names(ins18[[i]])<-"ins"
  names(month18[[i]])<-"monthcum"
  p<-stack(predictors18, precip18[[i]], tmean18[[i]],ins18[[i]],month18[[i]])
  prediction<-predict(p, pcarfmodel)
  all_preds18[[i]]<-prediction
  writeRaster(prediction, file.path(paste0(mainDir, "/Data/Results/Monthly/2017-2018/", i, ".tif")), "GTiff", overwrite=T)
  print(paste0(i," / 24"))
}



p18.2<- rasterToPoints(all_preds18[[14]])
df18.2<-as.data.frame(p18.2)
mean(df18.2$layer)
median(df18.2$layer)
histogram(df18.2$layer)

over18.2<-df18.2 %>% filter (layer.2.1.2.2<=0.5)
over1.18.2<- df18.2 %>% filter (layer.2.1.2.2<=1)


p18.8<- rasterToPoints(all_preds18[[20]])
df18.8<-as.data.frame(p18.8)
mean(df18.8$layer)
median(df18.8$layer)
histogram(df18.8$layer)

over18.8<-df18.8 %>% filter (layer.2.2<=0.5)
over1.18.8<- df18.8 %>% filter (layer.2.2<=1)




#Now extract the results to the points between 2017-2018 (testdat)
dates<-subset(testDat, select=c("date")) #get date column only
datelist<-dates[!duplicated(dates$date), ] #remove duplicates
datelist17<- datelist %>% filter(date>=2017) #up to 2018

predf18<-data.frame() #create an empty frame to add the resulting data from each iteration

datelist = datelist[order(datelist$date),] #order by date so the loop iterations go in order
head(datelist)

for (i in seq_along(datelist$date)) {
  sub<- subset(testDat, date==datelist$date[[i]])
  sub$ID <- seq.int(nrow(sub))
  data<-raster::extract(all_preds18[[i]], sub, df=T)
  pder <- merge(sub, data, by.x="ID", by.y="ID")
  names(pder)[32] <- "predicted"
  predf18<-rbind(predf18, pder)
  print(paste0("Extracted ", i, " out of ", length(datelist$date)))
}

predf18$error<-predf18$numMean-predf18$predicted
predf18$aerror<-abs(predf18$error)
mean(predf18$aerror)

histogram(predf18$error)

mae_month<-predf18 %>% group_by(date) %>% 
  summarise(mae = mean(aerror)) %>% 
  as.data.frame

under<-subset(predf18, predf18$error<0) #under surface, dryer
over<-subset(predf18, predf18$error>=0) #over surface, flooded

mean(over$error)
mean(under$error)

mae_over<-over %>% group_by(date) %>% 
  summarise(mae = mean(aerror)) %>% 
  as.data.frame
mae_under<-under %>% group_by(date) %>% 
  summarise(mae = mean(aerror)) %>% 
  as.data.frame
mae_borehole<-predf %>% group_by(boreholeid) %>%
  summarise(mae = mean(aerror)) %>% 
  as.data.frame()
mae_borehole[,c(3)]<-NULL
mae_borehole<-as.data.frame(mae_borehole)
help(summarise)
mean(mae_over$mae)
mean(mae_under$mae)

st_write(predf18, file.path(mainDir, "Data/Results/Monthly/error.shp"), delete_layer = TRUE) # overwrites
errors<-read_sf(file.path(mainDir, "Data/Results/Monthly/error.shp"))

under<-subset(errors, errors$aerror>=3) #under surface, dryer
e<-sum(errors$aerror)
e2<-sum(under$aerror)
total<-100*(e2/e)

RSQUARE(predf18$numMean,predf18$predicted)
RMSE(errors$numMean,errors$predicted)
MAE(predf18$numMean,predf18$predicted)




####PREDICTIONS 2100 seasonal#######################################################################################
predictorslist <- list.files(file.path(mainDir, "Data/Predictors_DK/datafuture"), pattern="tif$", full.names = TRUE)
monthlist <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/2100_month/month"), pattern="tif$", full.names = TRUE)
inslist<-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/2100_month"), pattern="tif$", full.names = TRUE)


predictors2<-stack(predictorslist)
month<-stack(monthlist)
ins<-stack(inslist)

insf<-ins[[1]]
monthf<-month[[2]]

insa<-ins[[2]]
montha<-month[[8]]


#245 winter
precip245list <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/2100_month/ssp245/precip"), pattern="tif$", full.names = TRUE)
precip245<-stack(precip245list)
fprec245<-precip245list[[2]]

temp245list <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/2100_month/ssp245/tmean"), pattern="tif$", full.names = TRUE)
temp245<-stack(temp245list)
ftemp245<-temp245list[[2]]

names(monthf)<-"monthcum"
names(predictors2)[1]<-"cly_dpt"
names(predictors2)[12]<-"slp_dgr"
names(insf)<-"ins"
names(fprec245)<-"precip"
names(ftemp245)<-"tmean"

predict245f<-stack(predictors2,monthf,insf,fprec245,ftemp245) #all predictors winter SSP245
prediction245f<-predict(predict245f, pcarfmodel) #ssp245 winter

names(predict245f)[[19]]<-"precip"
names(predict245f)[[20]]<-"tmean"
names(predict245f)

prediction245f<-predict(predict245f, pcarfmodel) #ssp585
plot(prediction245f)
writeRaster(prediction245f, file.path(mainDir, "Data/Results/Monthly/245feb.tif"), "GTiff", overwrite=F)


#245 summer
aprec245<-precip245list[[8]]
atemp245<-temp245list[[8]]

names(montha)<-"monthcum"
names(insa)<-"ins"
names(aprec245)<-"precip"
names(atemp245)<-"tmean"

predict245a<-stack(predictors2,montha,insa,aprec245,atemp245)
names(predict245a)[[19]]<-"precip"
names(predict245a)[[20]]<-"tmean"
names(predict245a)


prediction245a<-predict(predict245a, pcarfmodel) #ssp585
plot(prediction245a)
writeRaster(prediction245a, file.path(mainDir, "Data/Results/Monthly/245aug.tif"), "GTiff", overwrite=F)



#370 winter
precip370list <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/2100_month/ssp370/precip"), pattern="tif$", full.names = TRUE)
precip370<-stack(precip370list)
fprec370<-precip370list[[2]]

temp370list <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/2100_month/ssp370/tmean"), pattern="tif$", full.names = TRUE)
temp370<-stack(temp370list)
ftemp370<-temp370list[[2]]

names(monthf)<-"monthcum"
names(predictors2)[1]<-"cly_dpt"
names(predictors2)[12]<-"slp_dgr"
names(insf)<-"ins"
names(fprec370)<-"precip"
names(ftemp370)<-"tmean"

predict370f<-stack(predictors2,monthf,insf,fprec370,ftemp370)
names(predict370f)[[19]]<-"precip"
names(predict370f)[[20]]<-"tmean"
names(predict370f)

prediction370f<-predict(predict370f, pcarfmodel) #ssp585
plot(prediction370f)
writeRaster(prediction370f, file.path(mainDir, "Data/Results/Monthly/370feb.tif"), "GTiff", overwrite=F)


#370 summer
aprec370<-precip370list[[8]]
atemp370<-temp370list[[8]]

names(montha)<-"monthcum"
names(insa)<-"ins"
names(aprec370)<-"precip"
names(atemp370)<-"tmean"

predict370a<-stack(predictors2,montha,insa,aprec370,atemp370)
names(predict370a)[[19]]<-"precip"
names(predict370a)[[20]]<-"tmean"
names(predict370a)


prediction370a<-predict(predict370a, pcarfmodel) #ssp585
plot(prediction370a)
writeRaster(prediction370a, file.path(mainDir, "Data/Results/Monthly/370aug.tif"), "GTiff", overwrite=F)



#585 winter
precip585list <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/2100_month/ssp585/precip"), pattern="tif$", full.names = TRUE)
precip585<-stack(precip585list)
fprec585<-precip585[[2]]

temp585list <-list.files(file.path(mainDir, "Data/Predictors_DK/datafuture/2100_month/ssp585/tmean"), pattern="tif$", full.names = TRUE)
temp585<-stack(temp585list)
ftemp585<-temp585[[2]]

names(monthf)<-"monthcum"
names(predictors2)[1]<-"cly_dpt"
names(predictors2)[12]<-"slp_dgr"
names(insf)<-"ins"
names(fprec585)<-"precip"
names(ftemp585)<-"tmean"

predict585f<-stack(predictors2,monthf,insf,fprec585,ftemp585)
names(predict585f)[[19]]<-"precip"
names(predict585f)[[20]]<-"tmean"
names(predict585f)

prediction585f<-predict(predict585f, pcarfmodel) #ssp585
plot(prediction585f)
writeRaster(prediction585f, file.path(mainDir, "Data/Results/Monthly/585feb.tif"), "GTiff", overwrite=F)


#585 summer
aprec585<-precip585[[8]]
atemp585<-temp585[[8]]

names(montha)<-"monthcum"
names(insa)<-"ins"
names(aprec585)<-"precip"
names(atemp585)<-"tmean"

predict585a<-stack(predictors2,montha,insa,aprec585,atemp585)
names(predict585a)[[19]]<-"precip"
names(predict585a)[[20]]<-"tmean"
names(predict585a)


prediction585a<-predict(predict585a, pcarfmodel) #ssp585
plot(prediction585a)
writeRaster(prediction585a, file.path(mainDir, "Data/Results/Monthly/585aug.tif"), "GTiff", overwrite=F)


#SUMMARY (percentage depths < than 1m)
f245<- rasterToPoints(prediction245f)
fdf245<-as.data.frame(f245)
mean(fdf245$layer)
median(fdf245$layer)
histogram(fdf245$layer)

overf245<-fdf245 %>% filter (layer<=0.5)
over1.f245<- fdf245 %>% filter (layer<=1)


a245<- rasterToPoints(prediction245a)
adf245<-as.data.frame(a245)
mean(adf245$layer)
median(adf245$layer)
histogram(adf245$layer)

overa245<-adf245 %>% filter (layer<=0.5)
over1.a245<- adf245 %>% filter (layer<=1)


f370<- rasterToPoints(prediction370f)
fdf370<-as.data.frame(f370)
mean(fdf370$layer)
median(fdf370$layer)
histogram(fdf370$layer)

overf370<-fdf370 %>% filter (layer<=0.5)
over1.f370<- fdf370 %>% filter (layer<=1)


a370<- rasterToPoints(prediction370a)
adf370<-as.data.frame(a370)
mean(adf370$layer)
median(adf370$layer)
histogram(adf370$layer)

overa370<-adf370 %>% filter (layer<=0.5)
over1.a370<- adf370 %>% filter (layer<=1)



f585<- rasterToPoints(prediction585f)
fdf585<-as.data.frame(f585)
mean(fdf585$layer)
median(fdf585$layer)
histogram(fdf585$layer)

overf585<-fdf585 %>% filter (layer<=0.5)
over1.f585<- fdf585 %>% filter (layer<=1)


a585<- rasterToPoints(prediction585a)
adf585<-as.data.frame(a585)
mean(adf585$layer)
median(adf585$layer)
histogram(adf585$layer)

overa585<-adf585 %>% filter (layer<=0.5)
over1.a585<- adf585 %>% filter (layer<=1)


####DIFFERENTIAL MAPS###########################################################
prediction245f<- raster(file.path(mainDir, "Data/Results/Monthly/245feb.tif"))
prediction245a<- raster(file.path(mainDir, "Data/Results/Monthly/245aug.tif"))

prediction370f<- raster(file.path(mainDir, "Data/Results/Monthly/370feb.tif"))
prediction370a<- raster(file.path(mainDir, "Data/Results/Monthly/370aug.tif"))

prediction585f<- raster(file.path(mainDir, "Data/Results/Monthly/585feb.tif"))
prediction585a<- raster(file.path(mainDir, "Data/Results/Monthly/585aug.tif"))

p18f<-raster(file.path(mainDir, "Data/Results/Monthly/2017-2018/14.tif"))
p18a<-raster(file.path(mainDir, "Data/Results/Monthly/2017-2018/20.tif"))

diff245f<-p18f-prediction245f
diff245a<-p18a-prediction245a

diff370f<-p18f-prediction370f
diff370a<-p18a-prediction370a

diff585f<-p18f-prediction585f
diff585a<-p18a-prediction585a

writeRaster(diff245f, file.path(mainDir, "Data/Results/Monthly/diffpresent-future/diff245feb.tif"))
writeRaster(diff245a, file.path(mainDir, "Data/Results/Monthly/diffpresent-future/diff245aug.tif"))

writeRaster(diff370f, file.path(mainDir, "Data/Results/Monthly/diffpresent-future/diff370feb.tif"))
writeRaster(diff370a, file.path(mainDir, "Data/Results/Monthly/diffpresent-future/diff370aug.tif"))

writeRaster(diff585f, file.path(mainDir, "Data/Results/Monthly/diffpresent-future/diff585feb.tif"))
writeRaster(diff585a, file.path(mainDir, "Data/Results/Monthly/diffpresent-future/diff585aug.tif"))

