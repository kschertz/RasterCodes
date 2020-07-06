setwd("D:/Box Sync/Documents/MyOngoing/New_Greenspace")

#Needed libraries
library(sf)
library(rgdal)
library(raster)
library(fasterize)
library(sp)
library(mosaic)
library(dplyr)
library(RColorBrewer)

#Path to img file of tree canopy data
treecanopypath <- "D:/Box Sync/Documents/MyOngoing/New_Greenspace/NLCD_2016_TreeCanopy_raster/NLCD_2016_Tree_Canopy_L48_20190831.img"

#Raster of data
treecanopy <- raster(treecanopypath)
treecanopy

#Reading in the shapefile of Counties in the US
cty <- 'D:/Box Sync/Documents/MyOngoing/New_Greenspace/US_County_Shapefile/'
setwd(cty)
cty.shp <- readOGR(".","tl_2010_us_county10")
cty.shp <- spTransform(cty.shp,treecanopy@crs)
setwd("D:/Box Sync/Documents/MyOngoing/New_Greenspace")

head(cty.shp@data)

# Need to remove Alaska, Hawaii, Puerto Rico, etc
cty.shp <- subset(cty.shp, !grepl("02",cty.shp$STATEFP10))
cty.shp <- subset(cty.shp, !grepl("15",cty.shp$STATEFP10))
cty.shp <- subset(cty.shp, !grepl("60",cty.shp$STATEFP10))
cty.shp <- subset(cty.shp, !grepl("66",cty.shp$STATEFP10))
cty.shp <- subset(cty.shp, !grepl("72",cty.shp$STATEFP10))
cty.shp <- subset(cty.shp, !grepl("78",cty.shp$STATEFP10))

cty.shp <- cty.shp[order(cty.shp$GEOID10),]

#data storage
d2 <- data.frame(CTYFIPS = cty.shp$GEOID10, TreeCanopy = as.vector(as.integer(rep("NA",nrow(cty.shp)))))

for (j in 1:length(cty.shp$GEOID10)){
  tryCatch({
    print(paste("PROCESSING County",j,"of",length(cty.shp$GEOID10),":",as.character(cty.shp$GEOID10[j])))
  #selecting cty
  this_cty <- as.character(cty.shp$GEOID10[j])
  this.cty.shp <- cty.shp[as.character(cty.shp$GEOID10) %in% this_cty,]
  # crop
  print("cropping...")
  treecanopy.crop <- crop(treecanopy, extent(this.cty.shp))
  #transforming to sf object in order to use fasterize package
  this.cty.sf <- st_as_sf(this.cty.shp)
  # rasterize, using fasterize
  print("rasterizing...")
  treecanopy.cty.r <- fasterize::fasterize(this.cty.sf, treecanopy.crop)
  # mask
  print("masking...")
  treecanopy.cty.r.mask <- mask(treecanopy.crop, treecanopy.cty.r)
  # coding missing values, currently stored as 255 as NA
  treecanopy.cty.r.mask.no255 <- reclassify(treecanopy.cty.r.mask, cbind(100,Inf,NA), include.lowest=FALSE)
  #plotting. can be commented out to save time
  print("plotting...")
  plot(treecanopy.cty.r.mask.no255)
  # saving mean of tree canopy values in the raster masked to match the ctycode
  print("calculating mean")
  d2[j,2] <- cellStats(treecanopy.cty.r.mask.no255, mean)
  }, error=function(e){cat("ERROR: ",conditionMessage(e), "\n")})
}



cty.shp$Trees <- d2$TreeCanopy
my.palette <- brewer.pal(n=7, name = "Greens")
spplot(cty.shp, z="Trees", col.regions = my.palette, cuts = 6, col = "transparent")
spplot(cty.shp, z="Trees", col.regions = my.palette, cuts = 6)

#using a new method for counties that were too large for cellStats

need <- as.character(d2[which(is.na(d2$TreeCanopy)),1])
ctyneed <- cty.shp[which(cty.shp$GEOID10 %in% need),]

df1 <- data.frame(GEOID=character(), TreeCanopy=numeric())

for (i in 50:length(ctyneed$GEOID10)){
  tryCatch({
  print(paste("PROCESSING County",i,"of",length(ctyneed$GEOID10),":",as.character(ctyneed$GEOID10[i])))
 this_cty <- as.character(ctyneed$GEOID10[i])
 this.cty.shp <- ctyneed[as.character(ctyneed$GEOID10) %in% this_cty,]
 print("cropping...")
 treecanopy.crop <- crop(treecanopy, extent(this.cty.shp))
 #plot(treecanopy.crop)
 #plot(this.cty.shp)
 this.cty.sf <- st_as_sf(this.cty.shp)
 print("rasterizing...")
 treecanopy.cty.r <- fasterize::fasterize(this.cty.sf, treecanopy.crop)
 print("masking...")
 treecanopy.cty.r.mask <- mask(treecanopy.crop, treecanopy.cty.r)
 treecanopy.cty.r.mask.no255 <- reclassify(treecanopy.cty.r.mask, cbind(100,Inf,NA), include.lowest=FALSE)
 print(paste("Calculating mean. Start time:", as.character(Sys.time())))
 test.mean.tree <- extract(treecanopy.cty.r.mask.no255, this.cty.shp, fun=mean, na.rm=TRUE, df=TRUE)
 print(paste("End time:", as.character(Sys.time())))
 test.mean.tree[1,"ID"] <- this_cty
 df1 <- rbind(df1,test.mean.tree)
  }, error=function(e){cat("ERROR: ",conditionMessage(e), "\n")})
 }

df1 <- df1 %>% rename(CTYFIPS=ID)
df1 <- df1 %>% rename(TreeCanopy=layer)
df1$CTYFIPS <- as.character(df1$CTYFIPS)
d2$CTYFIPS <- as.character(d2$CTYFIPS)
allcountytree <- merge(d2, df1, all.x=TRUE) #didn't work! so exporting and using vlookup. maybe NA isn't coded right.

write.csv(df1, "largecsbatree.csv")
write.csv(d2, "mostcbsatree.csv")


