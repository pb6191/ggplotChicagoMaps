library("RSocrata")
library("tidyverse")
df <- read.socrata(
  "https://data.cityofchicago.org/resource/ijzp-q8t2.json",
  app_token = "xEXuVLIzaFYLeOC8cBFb1Z9H0",
  email     = "pbhargava936@gmail.com",
  password  = "swatu6191@PB"
)
head(df, n=1)
df_filtered <- filter(df, date >= "2018-08-01 00:00:00" & date <= "2021-07-31 23:59:59")
df_filtered
head(df_filtered, n=1)

censusBlocks <- read.socrata(
  "https://data.cityofchicago.org/resource/bt9m-d2mf.json",
  app_token = "xEXuVLIzaFYLeOC8cBFb1Z9H0",
  email     = "pbhargava936@gmail.com",
  password  = "swatu6191@PB"
)
head(censusBlocks, n=2)
censusTracts <- read.socrata(
  "https://data.cityofchicago.org/resource/74p9-q2aq.json",
  app_token = "xEXuVLIzaFYLeOC8cBFb1Z9H0",
  email     = "pbhargava936@gmail.com",
  password  = "swatu6191@PB"
)
censusTracts
library("tmap") 
tm_shape(censusBlocks.sf) + tm_polygons(col="grey", border.col="white")

library(sf)
plot("censusBlocks")

ggplot(data = censusBlocks)
geom_sf(data = censusBlocks, fill = NA)

library("rnaturalearth")
library("rnaturalearthdata")
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rgeos")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) + geom_sf()
head(world, n=1)
ggplot(data = censusBlocks, aes(geometry = the_geom)) + geom_sf()

ggplot() + geom_sf(data = censusBlocks, aes(geometry = the_geom))

library("plotKML")
censusBlocksKML <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois/createMaps/Boundaries - Census Blocks - 2010.kml")
plot(censusBlocksKML)
plot(censusBlocksKML[1])
ggplot(data = censusBlocksKML) + geom_sf()
ggplot(data = censusBlocksKML_trans) + geom_sf()
plotKML(censusBlocksKML)

battery_filtered <- filter(df_filtered, primary_type == "BATTERY")
aggBattery_filtered <- filter(battery_filtered, description == "AGGRAVATED - HANDGUN")
head(aggBattery_filtered, n=1)
head(censusBlocksKML$geometry, n=1)
head(censusBlocksKML, n=1)
head(censusBlocksKML_trans, n=2)
censusBlocksKML$index <- 1:nrow(censusBlocksKML)
st_intersects(aggBattery_filtered, censusBlocksKML)

aggBattery_pnts <- select(aggBattery_filtered, longitude, latitude)
head(aggBattery_pnts, n=1)
names(aggBattery_pnts)[1] <- "x"
names(aggBattery_pnts)[2] <- "y"
aggBattery_sf <- do.call("st_sfc",c(lapply(1:nrow(aggBattery_pnts), function(i) {st_point(as.numeric(aggBattery_pnts[i, ]))}), list("crs" = 4326))) 
aggBattery_trans <- st_transform(aggBattery_sf, 2163)
censusBlocksKML_trans <- st_transform(censusBlocksKML, 2163)

censusBlocksKML_sf <- do.call("st_sfc",c(lapply(1:nrow(censusBlocksKML), function(i) {st_point(as.numeric(censusBlocksKML[i, ]))}), list("crs" = 4326))) 
censusBlocksKML_sf <- read_sf("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois/createMaps/Boundaries - Census Blocks - 2010.kml")
censusBlocksKML_sf$index <- 1:nrow(censusBlocksKML_sf)

censusBlocksKML_trans <- st_transform(censusBlocksKML_sf, 2163)
head(censusBlocksKML_trans, n=1)

aggBattery_filtered$NEWblock <- apply(st_intersects(censusBlocksKML_trans, aggBattery_trans, sparse = FALSE), 2, 
                     function(col) { 
                       censusBlocksKML_trans[which(col), ]$index
                     })

filter(aggBattery_filtered, NEWblock == "NA")
aggBattery_onlyNEWblock <- aggBattery_filtered$NEWblock
aggBattery_freqDist <- data.frame(table(aggBattery_onlyNEWblock))

head(aggBattery_onlyNEWblock, n=1)

aggBattery_freqDist <- as.data.frame.matrix(table(aggBattery_onlyNEWblock))
length(aggBattery_onlyNEWblock)
write.csv(aggBattery_filtered, file = "newfile.csv")
options(max.print=5000)
capture.output(aggBattery_onlyNEWblock, file = "2gfg_list.csv")

library(purrr)
aggBattery_onlyNEWblock_filtered <- compact(aggBattery_onlyNEWblock)

aggBattery_filtered[2392,]
aggBattery_filtered[2488,]
aggBattery_filtered[2776,]
aggBattery_filtered[3124,]

aggBattery_onlyNEWblock[2392]

aggBattery_onlyNEWblock_filtered <- aggBattery_onlyNEWblock_filtered[-c(3121)]
aggBattery_onlyNEWblock_filtered[3121]
length(aggBattery_onlyNEWblock_filtered)

aggBattery_onlyNEWblock_filtered_df <- data.frame(matrix(unlist(aggBattery_onlyNEWblock_filtered), nrow=length(aggBattery_onlyNEWblock_filtered), byrow=TRUE))
aggBattery_freqDist <- as.data.frame.matrix(table(aggBattery_onlyNEWblock_filtered_df))
head(aggBattery_onlyNEWblock_filtered_df, n=1)
names(aggBattery_onlyNEWblock_filtered_df)[1] <- "NEWblock"
aggBattery_freqDist <- data.frame(table(aggBattery_onlyNEWblock_filtered_df$NEWblock))

head(aggBattery_freqDist, n=10)

censusBlocksKML_freq <- merge(x = censusBlocksKML, y = aggBattery_freqDist, by.x = "index", by.y = "Var1", all.x = TRUE)
head(censusBlocksKML_freq, n=10)
censusBlocksKML_freq$Freq[is.na(censusBlocksKML_freq$Freq)] <- 0
sum(censusBlocksKML_freq$Freq)
max(censusBlocksKML_freq$Freq)
censusBlocksKML_freq$Freq <- censusBlocksKML_freq$Freq/3
ggplot(data = censusBlocksKML_freq) + geom_sf()

ggplot(data = censusBlocksKML_freq, aes(fill = Freq)) + geom_sf()

ggplot() +
  geom_polygon(data = censusBlocksKML_freq, aes(fill = Freq)) +
  theme_void() +
  coord_map()

ggplot() +
  geom_polygon(data = censusBlocksKML_freq, aes(fill = Freq, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()

library(sp)
library(RColorBrewer)
mycolours <- brewer.pal(8, "YlOrRd")
spplot(censusBlocksKML_freq,"Freq", par.settings = list(axis.line = list(col ="transparent")), main = "Percentage vote for Bush, 2004", cuts = 5, col ="transparent", col.regions = mycolours)

library(GISTools)
myshading = auto.shading(censusBlocksKML_freq$Freq, n=10,
                         cols=brewer.pal(10, "Blues"))
choropleth(censusBlocksKML_freq, censusBlocksKML_freq$Freq,shading=myshading, main = "Percentage Vote Share for Bush - 2004")
choro.legend(px='bottomleft', sh=myshading,fmt="%4.1f",cex=0.8)

library(tmap)
tm_shape(censusBlocksKML_freq) + tm_polygons(col='Freq', title = "Blocks", palette = "Spectral") + tm_style_classic() + tm_scale_bar(position = c("right", "top")) 


ggplot(data = censusBlocksKML_freq, aes(fill = Freq), color = NA) + geom_sf()

ggplot(censusBlocksKML_freq, aes(x = "long", y = "lat", group = Freq)) + 
  geom_polygon(aes(fill = Freq, color = Freq))

library(scales)
library(ggspatial)
ggplot() + geom_sf(data = censusBlocksKML_freq, mapping = aes(fill = Freq), color=NA) + scale_fill_distiller(name="Shootings/Yr", palette = "Spectral" , breaks = pretty_breaks(n = 7)) + theme(legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 18), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings in Chicago by Census Block", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are crimes in that database classified as aggravated battery - handgun.\nData from August 1, 2018 -  July 31, 2021.")

