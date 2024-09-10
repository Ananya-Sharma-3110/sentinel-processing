### CODE BY ANANYA SHARMA; Reference: Merlin et. al., sensors(2021)
library(raster)
library(rasterVis)
library(rgdal)
library(caTools)
library(randomForest)
library(terra)
library(sf)
library(hydroGOF)
####sentinel 1 merging
setwd("D:/academics/Research/SM_downscaling/SM_data/sentinel 1(SAR)")
m1 <- raster("2018-04-25-00_00_2018-04-25-23_59_Sentinel-1_AWS-IW-VVVH_VV_-_decibel_gamma0_-_radiometric_terrain_corrected.tiff")
m2 <- raster("2018-04-30-00_00_2018-04-30-23_59_Sentinel-1_AWS-IW-VVVH_VV_-_decibel_gamma0_-_radiometric_terrain_corrected (2).tiff")
m1
m2
res(m1) <- res(m2)
img <- list(m1,m2) # the $ exludes aux-files
ic <- sprc(lapply(img, rast))
ic
r <- mosaic(ic, fun='mean')
MER <- raster(r)
writeRaster(MER, "merged_sent.tif", format='GTiff',overwrite=TRUE)

###linear regression between SMAP and s1

setwd("D:/academics/Research/SM_downscaling/SM_data/SL2A_cropped/10")
smp <- raster("SMAP_derived_40m_2.tif")
setwd("D:/academics/Research/SM_downscaling/SM_data/sentinel 1(SAR)")
merge_sent<- raster("merged_sent.tif")
plot(merge_sent)
merg_sent <- crop(merge_sent,extent(smp))
plot(smp)
plot(merg_sent)
sen <- resample(merge_sent,smp, method='ngb')
sen
smap_value<- extract(smp,extent(smp))
sent_value <- extract(sen, extent(sen))
smap1 <- as.data.frame(smp)
sen1 <- as.data.frame(sen)

x <- cbind(smap1,sen1)
x1 <- na.omit(x)
x1
#method 1
model <- lm(x1[,1]~x1[,2])
smap_30 <- (0.1252688*merg_sent)-0.0006916 
smap_30[smap_30[]<0]<-0
#method 2
smap_30_3 <- (merg_sent-0.0006916)/0.1252688
smap_30_3[smap_30_3[]<0]<-0 
#method 3
model1 <- lm(x1[,2]~x1[,1])
smap_30_2 <- (0.4685*merg_sent)-0.1621
smap_30_2[smap_30_2[]<0]<- 0
#saving files
writeRaster(smap_30_3,"smap_26april2018_30m_3.tif", format="GTiff",overwrite=TRUE)

###validation for linear regression model
setwd("D:/academics/Research/SM_downscaling/SM_data")
valid <-read.csv("april_validation_varanasi.csv")
valid
sm_obs_ap <- cellFromXY(smap_30_2,valid[,2:3])
sm_obs_ap
sm_obs <- extract(smap_30_2,sm_obs_ap)
sm_obs_data <- data.frame(sm_obs)
sm_obs_data <- sm_obs_data*100
sm_obs_data
correlate <- cor(valid[,4],sm_obs_data, method="pearson")
correlate
w <- cbind(valid[,4],sm_obs_data)
rms_error <- rmse(sm_obs_data,valid[,4],na.rm=TRUE)
rms_error

###multiple linear regression; SMAP,Sent2,NDVI(Sent1)
#NDVI calculation
setwd("D:/academics/Research/SM_downscaling/SM_data/SL2A_cropped")
sentinel_list <- list.files(pattern='tiff')
sentinel2 <- stack(sentinel_list)
sentinel2
ndvi_sen2 <- (sentinel2[[7]]-sentinel2[[3]])/(sentinel2[[7]]+sentinel2[[3]])
ndvi_sen2

##reading sentinel2 (30m) and enhanced SMAP (40m)
setwd("D:/academics/Research/SM_downscaling/SM_data/SL2A_cropped/10")
smp <- raster("SMAP_derived_40m_2.tif")
setwd("D:/academics/Research/SM_downscaling/SM_data/sentinel 1(SAR)")
merge_sent<- raster("merged_sent.tif")
plot(merge_sent)
merg_sent <- crop(merge_sent,extent(smp))

##aggregating sentinel to 40 m (coarser res)
sen <- aggregate(merg_sent,fact=1.52729263316)
sen_agg <-resample(sen,smp, method='ngb')
plot(sen)
###masking areas in sentinel2 image where ndvi < 0.1 (urban areas)
ndvi_mask <- ndvi_sen2
ndvi_mask[ndvi_mask[]<0.1]<-NA
plot(ndvi_mask)
sen1 <- as.data.frame(sen_agg)
smap1 <- as.data.frame(smp)
ndvi_mask_df <- as.data.frame(ndvi_mask)
ndvi_mask_df
mask_bind <- cbind(sen1,smap1,ndvi_mask_df)
masked <- na.omit(mask_bind)
masked

###regression model
multi_model <- lm(masked[,1] ~ masked[,2]+masked[,3])
multi_model
simp_model <- lm(masked[,1]~masked[,2])
simp_model
smapp <- (merg_sent-0.3851)/0.5586
plot(smapp)
smapp[smapp[]>0.7]<- NA
smapp[smapp[]<0]<- NA
###sm calculation (30m)
ndvi_sen2_dis <- resample(ndvi_sen2,merg_sent,method='ngb')
sm_linear_30 <- ((merg_sent-(0.7112*ndvi_sen2_dis)-0.2236))/0.7816
sm_linear_30[sm_linear_30[]<0]<-0
plot(sm_linear_30)
sm_linear_30
writeRaster(sm_linear_30,"sm_muliple regression.tif", format='GTiff')
#comment

