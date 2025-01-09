library(ggplot2)
library(raster)
library(sf)

frst <- raster("data/NG_t8_Canopy.tif")
plot(frst)
ndvi <- raster("data/NG_t8_S2_median_NDVI.tif")
plot(ndvi)
blds<-read_sf("data/NG_t8_open_buildings_v3_polygons.csv", header = TRUE)
blds$longitude<-as.numeric(blds$longitude)
blds$latitude<-as.numeric(blds$latitude)
tl.ids<-c(1365, 1369) # Tiles for oil palm data

# 1km x 1km resolution
dff<-1/111

coord.limits<-c(2.833915, 2.912248, 6.783718, 6.836628)
hst  <- expand.grid(lng = seq(coord.limits[1], coord.limits[2], by=dff),
                    lat = seq(coord.limits[3], coord.limits[4], by=dff))

# 1m x 1m resolution
df<-1/111000
bx.sz<-dff/2

#   Hyperparameters
h_min<-1.5
h_max<-6.5
ndvi_min<-0.5
ndvi_max<-0.75

hst$Done<-0
hst$op<-0 
hst$build<-0
hst$bnn<-0
for(ii in  1:nrow(hst)){
  lng<-hst$lng[ii]
  lat<-hst$lat[ii]
  if(hst$Done[ii]==0 ){ 
    hst$Done[ii]<-1
    # Make a 1m x 1m grid
    bnn.mp <- expand.grid(lng = seq(lng-bx.sz, lng+bx.sz, by=df),
                          lat = seq(lat-bx.sz, lat+bx.sz, by=df)) 
    sp <- SpatialPoints(cbind(bnn.mp$lng, bnn.mp$lat))  
    # Get ndvi values
    bnn.mp$ndvi<--1
    rasValue <- raster::extract(ndvi, sp)
    bnn.mp$ndvi[which(!is.na(rasValue))]<-rasValue[which(!is.na(rasValue))]
    #  Get canopy height
    bnn.mp$ch<-0
    rasValue <- raster::extract(frst, sp)
    bnn.mp$ch[which(!is.na(rasValue))]<-rasValue[which(!is.na(rasValue))]
    # Oil palm mask
    bnn.mp$op<-0
    for(iii in 1:length(tl.ids)){
      fln<-paste0("data/L2_2019b_", tl.ids[iii],".tif")
      ra.op = raster(fln)
      rasValue <- raster::extract(ra.op, sp, method='simple')
      wh<-which(rasValue %in% c(1:2))
      bnn.mp$op[wh]<-1
    }
    hst$op[ii]<-sum(bnn.mp$op)/nrow(bnn.mp)
    # Build-up mask
    bnn.mp$build<-0
       wh.bld<-which(blds$longitude>=lng-bx.sz & lng+bx.sz &
                   blds$latitude>= lat-bx.sz & blds$latitude<=lat+bx.sz)
      if(length(wh.bld)>0){
      for(iii in 1:length(wh.bld)){
        xy<-as.data.frame(st_coordinates(st_as_sfc(blds$geometry[wh.bld[iii]])))
        sts<-point.in.polygon(bnn.mp$lng, bnn.mp$lat, xy$X, xy$Y, mode.checked=FALSE)
        wh1<-which(sts==1)
        if(length(wh1)>0) bnn.mp$build[wh1]<-1
      }
     }
    hst$build[ii]<-sum(bnn.mp$build)/nrow(bnn.mp)
    #  Banana presence
    wh<-which(bnn.mp$ndvi>ndvi_min & bnn.mp$ndvi<ndvi_max &
                 bnn.mp$ch>h_min & bnn.mp$ch<h_max &
                 bnn.mp$op==0 & bnn.mp$build==0)
    hst$bnn[ii]<-length(wh)/nrow(bnn.mp)
  }
  cat(c(ii, ""))
}

ggplot()+
  geom_tile(data=hst, aes(x=lng, y=lat, fill=100*bnn))+
  scale_fill_continuous(name="%", type = "viridis", direction=-1) +
  xlab("")+ylab("") +
  coord_fixed() + theme_bw()+ 
  theme(legend.position="bottom")


