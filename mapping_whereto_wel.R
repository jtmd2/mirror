# general setup
library(rgdal)
library(ggmap)
library(dismo)
library(rgeos)
library(dplyr)
library(data.table)
library(sf)

setwd("C:\\Users\\Topsy\\Documents\\other\\whereto\\Wellington")

# input data formatted as spatial layers (from GIS)

admin_layer <- readOGR(dsn = "WCC_Suburb_Boundaries-shp", 
              layer = "WCC_Suburb_Boundaries")

prices_layer <- readOGR(dsn = "WCC_District_Plan-shp (4)", 
               layer = "WCC_District_Plan")

transport_layer <- readOGR(dsn = "Metlink_Service_Stops-shp", 
                               layer = "Metlink_Service_Stops")

eco_layer <- readOGR(dsn = "WCC_Ecological_Sites-shp", 
                       layer = "WCC_Ecological_Sites")

playgrounds_layer <- readOGR(dsn = "WCC_Playgrounds-shp", 
                     layer = "WCC_Playgrounds")

support_layer <- readOGR(dsn = "Support_Services_Wellington_City-shp", 
                             layer = "Support_Services_Wellington_City")

# extract data from shapefiles
admin_df <- admin_layer@data
prices_df <- prices_layer@data
transport_df <- transport_layer@data
eco_df <- eco_layer@data
playgrounds_df <- playgrounds_layer@data
support_df <- support_layer@data

# input data formatted as points
pois <- read.csv("wel_osm_poi.csv") # read in pois data
schools <- read.csv("schooldirectory-04-10-2020-083002.csv")

# input data formatted as spatial polygons/points but in csv (from CAD)
# noise_industry <- as(st_as_sf(read.csv("Amsterdam_industrynoise.csv", 
#                           stringsAsFactors = F), 
#                  wkt = "WKT_LNG_LAT"), 
#                  'Spatial')

# read in data with commas instead of points in lon and lat
# museums <- read.csv("Amsterdam_museums_galleries.csv", stringsAsFactors = F)
# museums$lat <- gsub(",", ".", museums$Latitude)
# museums$lon <- gsub(",", ".", museums$Longitude)

#library(raster)

# create subset of POIs 
poicafe <- pois[pois$type=="cafe",]
poigym <- pois[pois$type=="gym",]
poinight <- pois[pois$type=="nightclub",]
poipub <- pois[pois$type=="pub",]
poirest <- pois[pois$type=="restaurant",]

# match projections
proj4string(admin_layer)
proj4string(prices_layer)

# convert polygon projections to those used in GMaps and leaflet
common.crs <- "+init=epsg:4326"
admin_reprojected <- spTransform(admin_layer, common.crs)
prices_reprojected <- spTransform(prices_layer, common.crs)
transport_reprojected <- spTransform(transport_layer, common.crs)
eco_reprojected <- spTransform(eco_layer, common.crs)
playgrounds_reprojected <- spTransform(playgrounds_layer, common.crs)
support_reprojected <- spTransform(support_layer, common.crs)

#crs(noise_industry) <- common.crs

# check new projections
plot(admin_reprojected)
plot(prices_reprojected)

# calculate density scores for points
library(MASS)

densify <- function(lat, lon){
  dens <- MASS::kde2d(lat,lon) # estimate density based on POIs
  gr <- data.frame(with(dens, expand.grid(x,y)), as.vector(dens$z)) # put density into a data frame
  names(gr) <- c("xgr", "ygr", "zgr") # name the data frame
  mod <- loess(zgr ~ xgr*ygr, 
               data = gr,
               normalize = FALSE) # fit localized polynomial regression to estimate nightlife density from x and y
  return(mod)
}

# remove na lat and lons
schools <- schools[!is.na(schools$Latitude),]
playgrounds <- playgrounds_reprojected@data
playgrounds <- playgrounds[!is.na(playgrounds$XCoord),]
poigym <- pois[pois$type=="gym",]
poinight <- pois[pois$type=="nightclub",]
poipub <- pois[pois$type=="pub",]
poirest <- pois[pois$type=="restaurant",]

mod_schools <- densify(schools$Latitude,schools$Longitude)
mod_play <- densify(playgrounds$YCoord,playgrounds$XCoord)
mod_cafes <- densify(poicafe$lat,poicafe$lon)
mod_gym <- densify(poigym$lat,poigym$lon)
mod_pub <- densify(poipub$lat,poipub$lon)
mod_rest <- densify(poirest$lat,poirest$lon)

# map of density plot
library(OpenStreetMap)

wel <- openmap(c(-41.367778, 
                 174.601020), 
               c(-41.144309, 
                 174.933259),
               minNumTiles=9)
# other options for base map eg source=bing

plot(wel)

#-41.367778,174.601020,-41.144309,174.933259 - bbox wellington
#"(N, W, S, E)"

# print raster
png(width = 1000, height = 1000)
plot(wel,raster=TRUE)
dev.off()

# google maps version - less nice without license
wel2 <- get_map(osmdata::getbb("Wellington"), 
               maptype = "toner-background")

# density plots for predictors
schools_dens <- ggmap(wel2) +
  stat_density2d(aes(x = Longitude, y = Latitude,
                     fill = ..level.., alpha = ..level..),
                 size = 2, bins = 10, data = schools,
                 geom = "polygon") +
  scale_fill_distiller(palette = 'Reds') +
  ggtitle("Schools")

schools_dens

cafe_dens <- ggmap(wel2) +
  stat_density2d(aes(x = lon, y = lat,
                     fill = ..level.., alpha = ..level..),
                 size = 2, bins = 100, data = poicafe,
                 geom = "polygon") +
  scale_fill_distiller(palette = 'Reds') +
  ggtitle("Cafes")

cafe_dens

rest_dens <- ggmap(wel2) +
  stat_density2d(aes(x = lon, y = lat,
                     fill = ..level.., alpha = ..level..),
                 size = 2, bins = 100, data = poirest,
                 geom = "polygon") +
  scale_fill_distiller(palette = 'Reds') +
  ggtitle("Restaurants")

rest_dens

# make maps

library(leaflet)

# map with admin boundaries
library(leaflet)

centers <- data.frame(gCentroid(admin_reprojected, byid=TRUE))
#centers.region <- tgt@data$LABEL

l <- leaflet(admin_reprojected) %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(color = "black",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
#              fillOpacity = 0.5,
#              fillColor = "orange",
              popup = paste(admin_layer$suburb, "<br>"),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE))
l

library(htmlwidgets)
saveWidget(l, "admin_only.html")

##

# map with values

bins <- c(0, 
          15000,
          15250,
          15500,
          15750,
          16000,
          16250,
          16500,
          16750,
          17000,
          17250,
          17500)
pal <- colorBin("YlOrRd", domain = prices_reprojected$Upper, bins = bins)

l <- leaflet(prices_reprojected) %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(color = "white",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.7,
              fillColor = ~pal(roll),
              popup = paste("VALUE: ", prices_reprojected$roll, "<br>"),
              #                            ,
              #                            "BCNAAM: ", tgt$BCNAAM, "<br>"),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE))


l

saveWidget(l, "prices_only.html")

## 

# map with points and density

family_icons <- iconList(
  school = makeIcon("school.png", "school.png", 18, 18),
  playground = makeIcon("finish.png", "finish.png", 24, 24)
)

playgrounds_df <- playgrounds_df[!is.na(playgrounds_df$XCoord),]
playgrounds_df <- playgrounds_df[!is.na(playgrounds_df$YCoord),]

l <- leaflet(admin_reprojected) %>%
  addProviderTiles(providers$CartoDB) %>%
  addMarkers(schools$Longitude,
             schools$Latitude,
             icon = makeIcon("school.png", 
                             "school.png", 
                             18, 18)) %>%
  addMarkers(playgrounds_df$XCoord,
             playgrounds_df$YCoord,
             icon = makeIcon("finish.png", 
                             "finish.png", 
                             18, 18))


l

saveWidget(l, "family_desc.html")

yp_icons <- iconList(
  cafe = makeIcon("sandwich.png", "sandwich.png", 18, 18),
  pub = makeIcon("winebar.png", "winebar.png", 24, 24),
  rest = makeIcon("restaurant.png", "restaurant.png", 24, 24),
  gym = makeIcon("mall.png", "mall.png", 24, 24)
)

l <- leaflet(admin_reprojected) %>%
  addProviderTiles(providers$CartoDB) %>%
  addMarkers(poicafe$lon,
             poicafe$lat,
             icon = makeIcon("sandwich.png", 
                             "sandwich.png", 
                             18, 18)) %>%
  addMarkers(poipub$lon,
             poipub$lat,
             icon = makeIcon("winebar.png", 
                             "winebar.png", 
                             18, 18)) %>%
  addMarkers(poirest$lon,
             poirest$lat,
             icon = makeIcon("restaurant.png", 
                             "restaurant.png", 
                             18, 18)) %>%
  addMarkers(poigym$lon,
             poigym$lat,
             icon = makeIcon("mall.png", 
                             "mall.png", 
                             18, 18))
  

l

saveWidget(l, "yp_desc.html")


# apply the model to the point data
# 1. get centroids of admin areas (admin layer, = reference points)
# 2. calculate density at reference points (POI layer)
# 3. lookup what "suburb" reference points fall within (suburb layer)
# 4. lookup "price" for reference points (prices layer)
# 5. calculate "value" for reference points (density/value)

# 1. get centroids of admin areas (admin layer, = reference points)

centres <- data.frame(gCentroid(admin_reprojected, byid=TRUE))
centres$id <- admin_reprojected$id
centres$suburb <- admin_reprojected$suburb
centres$lat <- centres$y
centres$lon <- centres$x

#centres <- centres[centres$BC != "C1",] # remove random useless row

# 2. calculate density at reference points (POI layer)

# family score: schools, playgrounds 
centres$schooldens <- predict(mod_schools,
                              newdata = data.frame(xgr = centres$lat, 
                                                      ygr = centres$lon)) 
centres$playdens <- predict(mod_play,
                              newdata = data.frame(xgr = centres$lat, 
                                                   ygr = centres$lon)) 

# young professionals score: cafes, gyms, pubs, restaurants
centres$cafedens <- predict(mod_cafes,
                            newdata = data.frame(xgr = centres$lat, 
                                                 ygr = centres$lon)) 
centres$pubdens <- predict(mod_pub,
                            newdata = data.frame(xgr = centres$lat, 
                                                 ygr = centres$lon)) 
centres$restdens <- predict(mod_rest,
                            newdata = data.frame(xgr = centres$lat, 
                                                 ygr = centres$lon)) 
centres$gymdens <- predict(mod_gym,
                            newdata = data.frame(xgr = centres$lat, 
                                                 ygr = centres$lon)) 

# 3. lookup what "suburb" reference points fall within (suburb layer)
# not needed for this version - ref points are centroids of suburbs

# 4. lookup "price" for reference points (prices layer)

centres_sdf <- SpatialPoints(cbind(centres$lon,
                               centres$lat))

crs(centres_sdf) <- crs(prices_reprojected)

centres$price <- over(centres_sdf,
                             prices_reprojected)

library(tidyr)

centres_merged2 <- as.data.frame(unnest(centres$price,
                          cols = c("roll")))

centres_merged <- cbind(centres,centres_merged2)

# 5. calculate "value" for reference points (density/value)

centres_merged$value_family <- centres_merged$roll/(centres_merged$schooldens_10+centres_merged$playdens_10)

centres_merged$value_yp <- centres_merged$roll/(centres_merged$cafedens + centres_merged$pubdens + centres_merged$restdens + centres_merged$gymdens_10)

# get top 10 areas and lowest 10

top10_family <- centres_merged[,c("suburb",
                                  "value_family",
                                  "schooldens_10",
                                  "playdens_10")] %>%
  top_n(n = 10, 
        wt = value_family)

top10_yp <- centres_merged[,c("suburb",
                              "value_yp",
                              "cafedens_10",
                              "pubdens_10",
                              "restdens_10",
                              "gymdens_10")] %>%
  top_n(n = 10, 
        wt = value_yp)

top10_prices <- centres_merged[!is.na(centres_merged$roll),c("suburb",
                                                             "roll")] %>%
  top_n(n = 10, 
        wt = roll)

# rescale densities to (0,10)
library(scales)
# family scores
smpts$schooldens_10 <- rescale(smpts$schooldens, to=c(0,10))


# create map

# need to standardise score to get better spread?

centres_map <- centres_merged[centres_merged$value_family<30000,]

centres_map$value_family_10 <- rescale(centres_map$value_family,
                                       to=c(0,10))

bins_family <- c(0, 
          1,
          2,
          3,
          4,
          5,
          6,7,8,9,10)
pal <- colorBin("YlOrRd", 
                domain = centres_map$value_family_10, 
                bins = bins_family)

l <- leaflet(admin_reprojected) %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(color = "white",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.7,
              fillColor = ~pal(centres_map$value_family_10),
              popup = paste(admin_reprojected$suburb,
                            "Family Score: ",
                            centres_map$value_family_10,"<br>"),
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE))


l

saveWidget(l, "suburb_family.html")

# high res map

# apply the model to the point data
# 1. get 10000x grid for reference points
# 2. calculate density at reference points (POI layer)
# 4. lookup "price" for reference points (prices layer)
# 5. calculate "value" for reference points (density/value)

# 1. get 10000x grid for reference points

shp <- spTransform(admin_reprojected,
                   CRSobj = proj4string(admin_reprojected))

grdpts <- makegrid(shp, n = 10000,
                   pretty=TRUE)

# convert to points

spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(shp)))
spgrd_pts <- spgrd[shp,]

spgrd_dat <- as.data.frame(spgrd_pts)

# 2. calculate density at reference points (POI layer)

# family score: schools, playgrounds 

spgrd_dat$lat <- spgrd_dat$x2
spgrd_dat$lon <- spgrd_dat$x1

spgrd_dat$schooldens <- predict(mod_schools,
                              newdata = data.frame(xgr = spgrd_dat$lat, 
                                                   ygr = spgrd_dat$lon)) 
spgrd_dat$schooldens[is.na(spgrd_dat$schooldens)] <- 0
spgrd_dat$playdens <- predict(mod_play,
                            newdata = data.frame(xgr = spgrd_dat$lat, 
                                                 ygr = spgrd_dat$lon)) 
spgrd_dat$playdens[is.na(spgrd_dat$playdens)] <- 0

# young professionals score: cafes, gyms, pubs, restaurants
spgrd_dat$cafedens <- predict(mod_cafes,
                            newdata = data.frame(xgr = spgrd_dat$lat, 
                                                 ygr = spgrd_dat$lon)) 
spgrd_dat$cafedens[is.na(spgrd_dat$cafedens)] <- 0
spgrd_dat$pubdens <- predict(mod_pub,
                           newdata = data.frame(xgr = spgrd_dat$lat, 
                                                ygr = spgrd_dat$lon)) 
spgrd_dat$pubdens[is.na(spgrd_dat$pubdens)] <- 0
spgrd_dat$restdens <- predict(mod_rest,
                            newdata = data.frame(xgr = spgrd_dat$lat, 
                                                 ygr = spgrd_dat$lon)) 
spgrd_dat$restdens[is.na(spgrd_dat$restdens)] <- 0
spgrd_dat$gymdens <- predict(mod_gym,
                           newdata = data.frame(xgr = spgrd_dat$lat, 
                                                ygr = spgrd_dat$lon)) 
spgrd_dat$gymdens[is.na(spgrd_dat$gymdens)] <- 0

# 4. lookup "price" for reference points (prices layer)

spgrd_dat$price <- over(spgrd_pts,
                      prices_reprojected)

library(tidyr)

spgrd_dat2 <- as.data.frame(unnest(spgrd_dat$price,
                                        cols = c("roll")))

spgrd_dat <- cbind(spgrd_dat,spgrd_dat2)


# 5. calculate "value" for reference points (density/value)

# transform price data so it's better spread out

spgrd_dat$roll[spgrd_dat$roll==0] <- NA

# still weird with prices, because 3504/5003 properties have a value of NZD16690 (the median?)
# make them NAs and smooth over them, because we don't trust that these are actually "median" price for our purposes

spgrd_dat$roll[spgrd_dat$roll==16690] <- NA

# rescale density scores to c(0,10) to prevent complications making fractions from densities <0

spgrd_dat$schooldens_10 <- rescale(spgrd_dat$schooldens, to = c(0,10))
spgrd_dat$playdens_10 <- rescale(spgrd_dat$playdens, to = c(0,10))
spgrd_dat$cafedens_10 <- rescale(spgrd_dat$cafedens, to = c(0,10))
spgrd_dat$pubdens_10 <- rescale(spgrd_dat$pubdens, to = c(0,10))
spgrd_dat$restdens_10 <- rescale(spgrd_dat$restdens, to = c(0,10))
spgrd_dat$gymdens_10 <- rescale(spgrd_dat$gymdens, to = c(0,10))

# calculate scores

spgrd_dat$value_family <- spgrd_dat$roll/(spgrd_dat$schooldens_10+
                                            spgrd_dat$playdens_10)

spgrd_dat$value_yp <- spgrd_dat$roll/(spgrd_dat$cafedens_10 + 
                                        spgrd_dat$pubdens_10 + 
                                        spgrd_dat$restdens_10 + 
                                        spgrd_dat$gymdens_10)

# rescale to 0-10 (for score, else keep in NZD/unit values)

spgrd_dat$value_family_10 <- rescale(spgrd_dat$value_family,
                                        to = c(0,10))
spgrd_dat$value_yp_10 <- rescale(spgrd_dat$value_yp,
                                        to = c(0,10))

# to do: check other layers like
# roads
# terrain

# to do: add legend, make map pretty

# create map

spgrd_family <- spgrd_dat[!is.na(spgrd_dat$value_family_10),]

family_coords <- cbind(spgrd_family$lon,spgrd_family$lat)

family_map <- SpatialPointsDataFrame(spgrd_family,
                                     coords = family_coords)

# remove outliers
family_map2 <- family_map[family_map$value_family_10 < 6,]

# convert to raster

rast <- raster() # create empty raster
extent(rast) <- extent(family_map2) # give it same extent as the points
# check out ncol(rast) and nrow(rast) - assign for resolution

family_raster <- rasterize(family_map2,
                           rast,
                           family_map2$value_family_10,
                           fun = mean)

# use kriging to get values for missing pixels
x_range <- as.numeric(c(min(family_map2$x1),
                        max(family_map2$x1)))  # min/max longitude of the interpolation area
y_range <- as.numeric(c(min(family_map2$x2),
                        max(family_map2$x2)))  # min/max latitude of the interpolation area

# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(x = seq(from = x_range[1],
                           to = x_range[2], 
                           length.out = 1000),
                   y = seq(from = y_range[1],
                           to = y_range[2], 
                           length.out = 1000))  # expand points to grid
class(grd)

# Convert grd object to a matrix and then turn into a spatial points object
coordinates(grd) <- ~x + y
# turn into a spatial pixels object
gridded(grd) <- TRUE

# per https://nceas.github.io/oss-lessons/spatial-data-gis-law/4-tues-spatial-analysis-in-r.html
#In the last step we use idw() to interpolate our points to a grid. 
# idw() takes several arguments
# first the formula: elev_mm ~ 1 tells r to use the x,y coordinates 
# combined with the elev_mm value to perform the griding
# locations = represents the spatial points objects that you wish to grid
# newdata = represents the grid object that you will insert the values into
# finally we specify the power. Generall power values range between 1-3 with a smaller number creating a smoother surface (stronger influence from surrounding points) and a larger number creating a surface that is more true to the actual data and in turn a less smooth surface potentially.
# interpolate the data
library(gstat)

family_p1 <- idw(formula = value_family_10 ~ 1,
                locations = family_map2,
                newdata = grd,
                idp = 3)

# Notice that the output data is a SpatialPixelsDataFrame
class(family_p1)

# convert spatial pixels df to raster
#family_raster2 <- raster(family_p1)

rast <- raster() # create empty raster
extent(rast) <- extent(family_p1) # give it same extent as the points

# check out ncol(rast) and nrow(rast) - assign for resolution

family_raster <- rasterize(family_p1,
                           rast,
                           family_p1$var1.pred,
                           fun = mean)

# re-cut interpolated data to Council admin area (ie remove sea and non-habited places)
family_raster3 <- mask(family_raster,admin_reprojected)

# export to geotif
writeRaster(family_raster3,
            filename = "family_raster.tif", "GTiff")

# export to data frame
family_df <- as.data.frame(family_raster3,
                           xy = TRUE)

#set low density cells as NA so we can make them transparent with the colorNumeric function
#family_raster@data@values[which(family_raster@data@values < 1)] <- NA

#create pal function for coloring the raster
tgt <- family_raster3

palRaster <- colorNumeric("Blues", 
                          domain = tgt@data@values, 
                          na.color = "transparent")

l <- leaflet(tgt) %>%
  addProviderTiles(providers$CartoDB) %>%
  addRasterImage(tgt, 
                 colors = palRaster, 
                 opacity = .7) %>%
  addLegend(pal = palRaster, 
            values = tgt@data@values, 
            title = "Family Value Score")


l

saveWidget(l, "rast_family.html")

# at the moment, neighborhood scores are the score at the center point of the neighborhood
# to do: improve neighborhood scores
# either mean score across neighborhood,
# or max score in neighborhood?
# or something

# do kriging at predictor level -> create 1000x1000 raster of predictors @ multiple layers
# then convert to data.table so can be stored and looked up for RShiny

