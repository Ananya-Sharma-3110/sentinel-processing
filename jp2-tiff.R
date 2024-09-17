library(rgdal)
library(raster)
library(rasterVis)
library(tools)

setwd("D:/academics/Research/project pranmati/sentinel 2/RLU")
#
folder<-list.dirs("D:/academics/Research/project pranmati/sentinel 2/RLU")
folder
for (k in 2:3)
{
  setwd(folder[k])
  outdir <- file_path_sans_ext(basename(folder[k]))
  dir.create(outdir)
  
  jp2 <- list.files(pattern='.jp2',full.names=TRUE)
  for(i in jp2)
  {
    r <- raster(readGDAL(i))
    r<-r/10000
    outRaster <- file_path_sans_ext(basename(i))
    writeRaster(r, paste('./',outdir,'/',outRaster,'.tif',sep=''))
  }
}



