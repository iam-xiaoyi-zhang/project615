library(ggplot2)
library(ggmap) # crime dataset is in ggmap pakcage
library(grid)

# 1
murder <- subset(crime, offense == "murder")
qmplot(lon, lat, data = murder, colour = I('red'), size = I(3), darken = .3, 
       extent="panel", # "device"(default), "normal", "panel"
       legend="right"
       #stat=hour
       )
# It downloads a bunch of blocks and puts them together to get a map.

# 2
map = get_map(location="Boston", zoom=12)
ggmap(map)
map2 = get_map(location="Brookline MA", zoom=14)
ggmap(map2)
bu = get_map(location="Boston University MA", zoom=14)
ggmap(bu)
qmap("Boston University MA", zoom=14) # qmap is a wrapper of (get_map + ggmap)
qmap("Boston University MA", zoom=14, source="google", maptype="watercolor")
# maptype can be one of these: "terrain", "terrain-background", "satellite",
#                              "roadmap", "hybrid", "watercolor", "toner", positive int
# source can be one of these: "google", "osm", "stamen"(JPEG file), "cloudmade"(API needed)

# 3
set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 5), amount=.3), # add noise to lon/lat.
  y = jitter(rep(29.76, 5), amount=.3)
), digits=2)
map <- get_googlemap('houston', markers=df, path=df, scale=2, maptype='hybrid')
ggmap(map, extent='device')

# 4
map <- get_googlemap('paris', markers=df, path=df, scale=2, maptype='hybrid')
ggmap(map, extent='device', legend="left", darken=c(.5,3))
ggmap(map, extent='normal')
ggmap(map, extent='panel')

# ggmap in action
# 5
str(crime)
qmap('huston', zoom=13)
gglocator(2)
violent_crimes <- subset(crime, offense!="auto theft" & offense!="theft" & offense!="burglary")
# get rid of useless factors (auto theft, theft, burglary)
violent_crimes$offense <- factor(violent_crimes$offense,levels=c("robbery","aggravated assault", "rape", "murder"))
violent_crimes <- subset(violent_crimes,
                         -95.39681 <= lon & lon <= -95.34188 &
                           29.73631 <= lat & lat <= 29.78400) #crimes in a certain area.

theme_set(theme_bw(16))
HoustonMap <- qmap("houston", zoom=14, color="bw", legend="topleft")
HoustonMap +
  geom_point(aes(x=lon, y=lat, colour=offense, size=offense), data=violent_crimes)

HoustonMap + 
  stat_bin2d(
    aes(x=lon, y=lat, colour=offense, fill=offense),
    size=.5, bins=30, alpha=1/2,
    data=violent_crimes
  )
?stat_bin2d
dim(violent_crimes)

# 6
houston <- get_map("houston", zoom=14)
HoustonMap <- ggmap("houston", extent="device", legend="topleft")

HoustonMap + 
  stat_density2d(
    aes(x=lon, y=lat, fill=..level.., alpha=..level..),
    size=2, bins=4, data=violent_crimes,
    geom="polygon"
  )

overlay <- stat_density2d(
  aes(x=lon, y=lat, fill=..level.., alpha=..level..),
  bins=4, geom="polygon",
  data=violent_crimes
)

HoustonMap + overlay

HoustonMap + overlay + inset(
  grob = ggplotGrob(ggplot() + overlay + theme_inset()),
  xmin=-95.35836, xmax=-95.345, ymin=29.74, ymax=29.75062
)#????

# 7
houston <- get_map(location="houston", zoom=14, color="bw",
                   source="osm")

HoustonMap <- ggmap(houston, base_layer=ggplot(aes(x=lon, y=lat),
                                               data=violent_crimes))
HoustonMap + 
  stat_density2d(aes(x=lon, y=lat, fill=..level.., alpha=..level..),
                 bins=5, geom="polygon",
                 data=violent_crimes) +
  scale_fill_gradient(low="black", high="red") +
  facet_wrap(~day)

# ggmap's utility function
# 8 geocode, revgeocode
geocode("baylor university Texas", output="more")
geocode("Boston University", output="more")

gc <- geocode("baylor university Texas")
(gc <- as.numeric(gc))
revgeocode(gc)
revgeocode(gc, output="more")

# 9 mapdist
from <- c("houston", "houston", "dallas")
to <- c("waco, texas", "san antonio", "houston")
mapdist(from, to)

# 10 API limit
distQueryCheck()
.GoogleDistQueryCount

# 11 route
legs_df <- route(
  'baylor university',
  '220 south 3rd street, waco, tx 76701',
  alternatives = TRUE
)
legs_df

qmap('424 clay avenue, waco, tx', zoom=15, maptype='hybrid',
     base_layer = ggplot(aes(x=startLon, y=startLat), data=legs_df)) +
  geom_leg(
    aes(x=startLon, y=startLat, xend=endLon, yend=endLat,
        colour=route),
    alpha=3/4, size=2, data=legs_df
    ) +
  labs(x='Longitude', y="Latitude", colour='Route') +
  facet_wrap(~route, ncol=3) + theme(legend.position='top')

# 12 shape files
# get an example shape file
download.file('http://www.census.gov/geo/cob/bdy/tr/tr00shp/tr48_d00_shp.zip',
               destfil ='census.zip') # 404 not found
# unzip, and load tools
install.packages("maptools")
install.packages("gpclib") # need compilation of C/C++/Fortran
unzip('census.zip'); library(maptools); library(gpclib); library(sp)
# read data into R
shapefile <- readShapeSpatial('tr48_d00.shp',
                              proj4string=CRS("+proj=longlat+datum=WGS84"))
# convert to a data.frame for use with ggplot2/ggmap and plot
data <- fortify(shapefile)
qmap('texas', zoom = 6, maptype = 'satellite') +
  geom_polygon(aes(x = long, y = lat, group = group), data = data,
               colour = 'white', fill = 'black', alpha = .4, size = .3)
