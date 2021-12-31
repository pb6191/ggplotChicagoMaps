head(highSchoolChicVoronoiPopln_sf, n=1)
class(highSchoolChicVoronoiPopln_sf)
library("RSocrata")
library("tidyverse")
library(geojsonR)
library(geojsonsf)
library("plotKML")
library("ggplot2")
library("sf")
library("rgeos")
library(purrr)
library("plyr")
library(scales)
library(ggspatial)
library(viridis)
library(dplyr)

options(max.print=50000)

victims_df <- read.socrata(
  "https://data.cityofchicago.org/resource/gumc-mgzr.json",
  app_token = "xEXuVLIzaFYLeOC8cBFb1Z9H0",
  email     = "pbhargava936@gmail.com",
  password  = "swatu6191@PB"
)

populationCensusBlock_df <- read.socrata(
  "https://data.cityofchicago.org/resource/5yjb-v3mj.json",
  app_token = "xEXuVLIzaFYLeOC8cBFb1Z9H0",
  email     = "pbhargava936@gmail.com",
  password  = "swatu6191@PB"
)


chicagoPopln <- read.table(file = "C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Chicago_Population_Counts.csv", 
                         sep = ";", header=TRUE)


highSchoolBoundariesMap_sf <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Chicago Public Schools - High School Attendance Boundaries SY2021.geojson")
highSchoolBoundariesMap_sf <- highSchoolBoundariesMap_sf[-c(53), ]
elementaryBoundariesMap_sf <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Chicago Public Schools - Elementary School Attendance Boundaries SY2021.geojson")

schoolPoints_ORIG_sf <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Chicago Public Schools - School Profile Information SY2021.geojson")
schoolPoints_REM_OPTIONS_sf <- schoolPoints_ORIG_sf[(schoolPoints_ORIG_sf$network!="Options"),]
schoolPoints_REM_CONTRACT_sf <- schoolPoints_REM_OPTIONS_sf[(schoolPoints_REM_OPTIONS_sf$network!="Contract"),]
schoolPoints_REM_SELECT_sf <- schoolPoints_REM_CONTRACT_sf[!grepl(paste(c("Hancock","Gwendolyn Brooks","King  Jr  College","Jones","Lane","Lindblom","Northside College","Payton","South Shore Intl","Westinghouse","Whitney M Young Magnet High School","Jefferson","York"), collapse="|"),schoolPoints_REM_CONTRACT_sf$long_name),]
schoolPoints_ATT_FALSE_sf <- schoolPoints_REM_SELECT_sf[(schoolPoints_REM_SELECT_sf$attendance_boundaries=="false"),]
schoolPoints_NET_ATT_FALSE_sf <- schoolPoints_ATT_FALSE_sf[grep("Network",schoolPoints_ATT_FALSE_sf$network),]
moreSchoolsToExcl <- c("Daniel Hale Williams Prep School of Medicine", "Walt Disney Magnet Elementary School", "Consuella B York Alternative HS", "Louisa May Alcott College Preparatory HS", "Thomas A Edison Regional Gifted Center ES", "Inter-American Elementary Magnet School", "Richard T Crane Medical Preparatory HS", "Robert A Black Magnet Elementary School", "Annie Keller Regional Gifted Center", "Edward Beasley Elementary Magnet Academic Center", "Suder Montessori Magnet ES", "George Washington Carver Military Academy HS", "Disney II Magnet School", "Nancy B Jefferson Alternative HS", "Disney II Magnet High School", "Albert R Sabin Elementary Magnet School")
schoolPoints_sf <- schoolPoints_REM_SELECT_sf[!grepl(paste(moreSchoolsToExcl, collapse="|"),schoolPoints_REM_SELECT_sf$long_name),]
highSchoolPoints_sf <- schoolPoints_sf[(schoolPoints_sf$is_high_school=="true"),]
elementarySchoolPoints_sf <- schoolPoints_sf[(schoolPoints_sf$is_elementary_school=="true"),]

library("writexl")
write_xlsx(schoolPoints_sf,"C:\\Users\\PC-P14s\\Documents\\FinalSchoolList.xlsx")

populationCensusBlock_df$total_population <- as.numeric(as.character(populationCensusBlock_df$total_population))
populationCensusBlock_df$CUSTOM_tract = substr(populationCensusBlock_df$census_block_full,1,11)
populationCensusBlock_df$CUSTOM_block_grp = substr(populationCensusBlock_df$census_block_full,1,12)

chicagoPopln<-chicagoPopln[!(chicagoPopln$Year==2019),]

overlayStreets <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Major_Streets/Major_Streets.shp")
blockGroup_sf <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Cook County Block Group files/tl_2010_17031_bg10.shp")
zipCode_sf <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Boundaries - ZIP Codes.geojson")

censusBlock_sf <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Boundaries - Census Blocks - 2010.geojson")
censusTract_sf <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Boundaries - Census Tracts - 2010.geojson")


blockGroup_sf$GEO_tract = substr(blockGroup_sf$GEOID10,1,11)
onlyTractGEOIDs <- censusTract_sf$geoid10
chicagoBlockGroup_sf <- blockGroup_sf$GEO_tract %in% onlyTractGEOIDs

blockGroup_sf$present <- chicagoBlockGroup_sf
filteredBlockGroup_sf<-blockGroup_sf[!(blockGroup_sf$present==FALSE),]


populationCensusTract_df <- as.data.frame(xtabs(total_population ~ CUSTOM_tract, populationCensusBlock_df))
populationCensusBlockGroup_df <- as.data.frame(xtabs(total_population ~ CUSTOM_block_grp, populationCensusBlock_df))

homicides_df <- filter(victims_df, primary_type != "NON-FATAL SHOOTING" & date >= "2016-08-01 00:00:00" & date <= "2021-07-31 23:59:59")
shootings_df <- filter(victims_df, primary_type != "NON-SHOOTING HOMICIDE" & date >= "2016-08-01 00:00:00" & date <= "2021-07-31 23:59:59")

homicides_pnts <- select(homicides_df, longitude, latitude)
shootings_pnts <- select(shootings_df, longitude, latitude)

names(homicides_pnts)[1] <- "x"
names(homicides_pnts)[2] <- "y"
names(shootings_pnts)[1] <- "x"
names(shootings_pnts)[2] <- "y"

homicides_pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(homicides_pnts), function(i) {st_point(as.numeric(homicides_pnts[i, ]))}), list("crs" = 4326)))
shootings_pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(shootings_pnts), function(i) {st_point(as.numeric(shootings_pnts[i, ]))}), list("crs" = 4326)))

homicides_pnts_trans <- st_transform(homicides_pnts_sf, 2163)
shootings_pnts_trans <- st_transform(shootings_pnts_sf, 2163)

censusBlock_sf_popln <- merge(x = censusBlock_sf, y = populationCensusBlock_df, by.x = "geoid10", by.y = "census_block_full", all.x = TRUE)
censusTract_sf_popln <- merge(x = censusTract_sf, y = populationCensusTract_df, by.x = "geoid10", by.y = "CUSTOM_tract", all.x = TRUE)
censusBlockGrp_sf_popln <- merge(x = filteredBlockGroup_sf, y = populationCensusBlockGroup_df, by.x = "GEOID10", by.y = "CUSTOM_block_grp", all.x = TRUE)
censusZip_sf_popln <- merge(x = zipCode_sf, y = chicagoPopln, by.x = "zip", by.y = "Geography", all.x = TRUE)

censusBlock_sf_popln$total_population[is.na(censusBlock_sf_popln$total_population)] <- 0
names(censusTract_sf_popln)[names(censusTract_sf_popln) == 'Freq'] <- 'total_population'
names(censusBlockGrp_sf_popln)[names(censusBlockGrp_sf_popln) == 'Freq'] <- 'total_population'
names(censusZip_sf_popln)[names(censusZip_sf_popln) == 'Population...Total'] <- 'total_population'


# Carson's Voronoi polygons function
voronoipolygons <- function(x) {
  require(deldir)
  require(sp)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  z <- deldir(crds[,1], crds[,2])
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
}

highSchoolGeom_sf <- data.frame(x=as.numeric(highSchoolPoints_sf$school_longitude), y=as.numeric(highSchoolPoints_sf$school_latitude), total_population = as.numeric(highSchoolPoints_sf$student_count_total))
highSchoolVoronoi_grid <- voronoipolygons(highSchoolGeom_sf)
elementarySchoolGeom_sf <- data.frame(x=as.numeric(elementarySchoolPoints_sf$school_longitude), y=as.numeric(elementarySchoolPoints_sf$school_latitude), total_population = as.numeric(elementarySchoolPoints_sf$student_count_total))
elementarySchoolVoronoi_grid <- voronoipolygons(elementarySchoolGeom_sf)


chicagoBoundary_sf <- st_read("C:/Users/PC-P14s/Desktop/WORK/RHP edcn illinois - GIT/createMaps/Boundaries - City.geojson")
chicagoBoundary_spatial <- as(chicagoBoundary_sf, 'Spatial')

highSchoolChicVoronoi_grid <- intersect(highSchoolVoronoi_grid, chicagoBoundary_spatial)
highSchoolChicVoronoi_sf <- st_as_sf(highSchoolChicVoronoi_grid)
highSchoolChicVoronoiPopln_sf <- merge(x = highSchoolChicVoronoi_sf, y = highSchoolGeom_sf, by.x = "x", by.y = "x", all.x = TRUE, all.y = TRUE)
elementarySchoolChicVoronoi_grid <- intersect(elementarySchoolVoronoi_grid, chicagoBoundary_spatial)
elementarySchoolChicVoronoi_sf <- st_as_sf(elementarySchoolChicVoronoi_grid)
elementarySchoolChicVoronoiPopln_sf <- merge(x = elementarySchoolChicVoronoi_sf, y = elementarySchoolGeom_sf, by.x = "x", by.y = "x", all.x = TRUE, all.y = TRUE)


censusTract_sf_popln$total_population[is.na(censusTract_sf_popln$total_population)] <- 0
censusBlockGrp_sf_popln$total_population[is.na(censusBlockGrp_sf_popln$total_population)] <- 0
censusZip_sf_popln$total_population[is.na(censusZip_sf_popln$total_population)] <- 0
highSchoolChicVoronoiPopln_sf$total_population[is.na(highSchoolChicVoronoiPopln_sf$total_population)] <- 0
elementarySchoolChicVoronoiPopln_sf$total_population[is.na(elementarySchoolChicVoronoiPopln_sf$total_population)] <- 0

censusZip_sf_popln$total_population <- as.numeric(gsub(",","",censusZip_sf_popln$total_population))
censusZip_sf_popln$total_population[censusZip_sf_popln$zip == '60643'] <- censusZip_sf_popln$total_population[censusZip_sf_popln$zip == '60643']/2
censusZip_sf_popln$total_population[censusZip_sf_popln$zip == '60707'] <- censusZip_sf_popln$total_population[censusZip_sf_popln$zip == '60707']/2


censusBlock_sf_popln$index <- 1:nrow(censusBlock_sf_popln)
censusTract_sf_popln$index <- 1:nrow(censusTract_sf_popln)
censusBlockGrp_sf_popln$index <- 1:nrow(censusBlockGrp_sf_popln)
censusZip_sf_popln$index <- 1:nrow(censusZip_sf_popln)
highSchoolBoundariesMap_sf$index <- 1:nrow(highSchoolBoundariesMap_sf)
elementaryBoundariesMap_sf$index <- 1:nrow(elementaryBoundariesMap_sf)
highSchoolChicVoronoiPopln_sf$index <- 1:nrow(highSchoolChicVoronoiPopln_sf)
elementarySchoolChicVoronoiPopln_sf$index <- 1:nrow(elementarySchoolChicVoronoiPopln_sf)

censusBlock_trans <- st_transform(censusBlock_sf_popln, 2163)
censusTract_trans <- st_transform(censusTract_sf_popln, 2163)
censusBlockGrp_trans <- st_transform(censusBlockGrp_sf_popln, 2163)
censusZip_trans <- st_transform(censusZip_sf_popln, 2163)
highSchoolBoundariesMap_trans <- st_transform(highSchoolBoundariesMap_sf, 2163)
elementaryBoundariesMap_trans <- st_transform(elementaryBoundariesMap_sf, 2163)
highSchoolChicVoronoiPopln_trans <- st_transform(highSchoolChicVoronoiPopln_sf, 2163)
elementarySchoolChicVoronoiPopln_trans <- st_transform(elementarySchoolChicVoronoiPopln_sf, 2163)

homicides_df$WhichHighVor <- apply(st_intersects(highSchoolChicVoronoiPopln_trans, homicides_pnts_trans, sparse = FALSE), 2, 
                                function(col) { 
                                  highSchoolChicVoronoiPopln_trans[which(col), ]$index
                                })

homicides_df$WhichElementaryVor <- apply(st_intersects(elementarySchoolChicVoronoiPopln_trans, homicides_pnts_trans, sparse = FALSE), 2, 
                                      function(col) { 
                                        elementarySchoolChicVoronoiPopln_trans[which(col), ]$index
                                      })

homicides_df$WhichHigh <- apply(st_intersects(highSchoolBoundariesMap_trans, homicides_pnts_trans, sparse = FALSE), 2, 
                                       function(col) { 
                                         highSchoolBoundariesMap_trans[which(col), ]$index
                                       })

homicides_df$WhichElementary <- apply(st_intersects(elementaryBoundariesMap_trans, homicides_pnts_trans, sparse = FALSE), 2, 
                                function(col) { 
                                  elementaryBoundariesMap_trans[which(col), ]$index
                                })

homicides_df$WhichCensusBlock <- apply(st_intersects(censusBlock_trans, homicides_pnts_trans, sparse = FALSE), 2, 
                              function(col) { 
                                censusBlock_trans[which(col), ]$index
                              })

homicides_df$WhichCensusTract <- apply(st_intersects(censusTract_trans, homicides_pnts_trans, sparse = FALSE), 2, 
                                       function(col) { 
                                         censusTract_trans[which(col), ]$index
                                       })

homicides_df$WhichCensusGroup <- apply(st_intersects(censusBlockGrp_trans, homicides_pnts_trans, sparse = FALSE), 2, 
                                       function(col) { 
                                         censusBlockGrp_trans[which(col), ]$index
                                       })

homicides_df$WhichCensusZip <- apply(st_intersects(censusZip_trans, homicides_pnts_trans, sparse = FALSE), 2, 
                                       function(col) { 
                                         censusZip_trans[which(col), ]$index
                                       })

shootings_df$WhichCensusBlock <- apply(st_intersects(censusBlock_trans, shootings_pnts_trans, sparse = FALSE), 2, 
                                       function(col) { 
                                         censusBlock_trans[which(col), ]$index
                                       })

shootings_df$WhichCensusTract <- apply(st_intersects(censusTract_trans, shootings_pnts_trans, sparse = FALSE), 2, 
                                       function(col) { 
                                         censusTract_trans[which(col), ]$index
                                       })

shootings_df$WhichCensusGroup <- apply(st_intersects(censusBlockGrp_trans, shootings_pnts_trans, sparse = FALSE), 2, 
                                       function(col) { 
                                         censusBlockGrp_trans[which(col), ]$index
                                       })

shootings_df$WhichCensusZip <- apply(st_intersects(censusZip_trans, shootings_pnts_trans, sparse = FALSE), 2, 
                                       function(col) { 
                                         censusZip_trans[which(col), ]$index
                                       })

shootings_df$WhichHigh <- apply(st_intersects(highSchoolBoundariesMap_trans, shootings_pnts_trans, sparse = FALSE), 2, 
                                function(col) { 
                                  highSchoolBoundariesMap_trans[which(col), ]$index
                                })

shootings_df$WhichElementary <- apply(st_intersects(elementaryBoundariesMap_trans, shootings_pnts_trans, sparse = FALSE), 2, 
                                      function(col) { 
                                        elementaryBoundariesMap_trans[which(col), ]$index
                                      })

shootings_df$WhichHighVor <- apply(st_intersects(highSchoolChicVoronoiPopln_trans, shootings_pnts_trans, sparse = FALSE), 2, 
                                function(col) { 
                                  highSchoolChicVoronoiPopln_trans[which(col), ]$index
                                })

shootings_df$WhichElementaryVor <- apply(st_intersects(elementarySchoolChicVoronoiPopln_trans, shootings_pnts_trans, sparse = FALSE), 2, 
                                      function(col) { 
                                        elementarySchoolChicVoronoiPopln_trans[which(col), ]$index
                                      })
homicides_onlyCensusBlock <- homicides_df$WhichCensusBlock
homicides_onlyCensusTract <- homicides_df$WhichCensusTract
homicides_onlyCensusGroup <- homicides_df$WhichCensusGroup
homicides_onlyCensusZip <- homicides_df$WhichCensusZip
homicides_onlyHigh <- homicides_df$WhichHigh
homicides_onlyElementary <- homicides_df$WhichElementary
homicides_onlyHighVor <- homicides_df$WhichHighVor
homicides_onlyElementaryVor <- homicides_df$WhichElementaryVor

shootings_onlyCensusBlock <- shootings_df$WhichCensusBlock
shootings_onlyCensusTract <- shootings_df$WhichCensusTract
shootings_onlyCensusGroup <- shootings_df$WhichCensusGroup
shootings_onlyCensusZip <- shootings_df$WhichCensusZip
shootings_onlyHigh <- shootings_df$WhichHigh
shootings_onlyElementary <- shootings_df$WhichElementary
shootings_onlyHighVor <- shootings_df$WhichHighVor
shootings_onlyElementaryVor <- shootings_df$WhichElementaryVor

homicides_onlyCensusBlock_filtered <- compact(homicides_onlyCensusBlock)
homicides_onlyCensusTract_filtered <- compact(homicides_onlyCensusTract)
homicides_onlyCensusGroup_filtered <- compact(homicides_onlyCensusGroup)
homicides_onlyCensusZip_filtered <- compact(homicides_onlyCensusZip)
homicides_onlyHigh_filtered <- compact(homicides_onlyHigh)
homicides_onlyElementary_filtered <- compact(homicides_onlyElementary)
homicides_onlyHighVor_filtered <- compact(homicides_onlyHighVor)
homicides_onlyElementaryVor_filtered <- compact(homicides_onlyElementaryVor)

shootings_onlyCensusBlock_filtered <- compact(shootings_onlyCensusBlock)
shootings_onlyCensusTract_filtered <- compact(shootings_onlyCensusTract)
shootings_onlyCensusGroup_filtered <- compact(shootings_onlyCensusGroup)
shootings_onlyCensusZip_filtered <- compact(shootings_onlyCensusZip)
shootings_onlyHigh_filtered <- compact(shootings_onlyHigh)
shootings_onlyElementary_filtered <- compact(shootings_onlyElementary)
shootings_onlyHighVor_filtered <- compact(shootings_onlyHighVor)
shootings_onlyElementaryVor_filtered <- compact(shootings_onlyElementaryVor)


#homicides_onlyCensusBlock_filtered_df <- data.frame(matrix(unlist(homicides_onlyCensusBlock_filtered), nrow=length(homicides_onlyCensusBlock_filtered), byrow=TRUE))
#shootings_onlyCensusBlock_filtered_df <- data.frame(matrix(unlist(shootings_onlyCensusBlock_filtered), nrow=length(shootings_onlyCensusBlock_filtered), byrow=TRUE))

homicides_onlyCensusBlock_filtered_df <- do.call(rbind.data.frame, homicides_onlyCensusBlock_filtered)
homicides_onlyCensusTract_filtered_df <- do.call(rbind.data.frame, homicides_onlyCensusTract_filtered)
homicides_onlyCensusGroup_filtered_df <- do.call(rbind.data.frame, homicides_onlyCensusGroup_filtered)
homicides_onlyCensusZip_filtered_df <- do.call(rbind.data.frame, homicides_onlyCensusZip_filtered)
homicides_onlyHigh_filtered_df <- do.call(rbind.data.frame, homicides_onlyHigh_filtered)
homicides_onlyElementary_filtered_df <- do.call(rbind.data.frame, homicides_onlyElementary_filtered)
homicides_onlyHighVor_filtered_df <- do.call(rbind.data.frame, homicides_onlyHighVor_filtered)
homicides_onlyElementaryVor_filtered_df <- do.call(rbind.data.frame, homicides_onlyElementaryVor_filtered)

shootings_onlyCensusBlock_filtered_df <- do.call(rbind.data.frame, shootings_onlyCensusBlock_filtered)
shootings_onlyCensusTract_filtered_df <- do.call(rbind.data.frame, shootings_onlyCensusTract_filtered)
shootings_onlyCensusGroup_filtered_df <- do.call(rbind.data.frame, shootings_onlyCensusGroup_filtered)
shootings_onlyCensusZip_filtered_df <- do.call(rbind.data.frame, shootings_onlyCensusZip_filtered)
shootings_onlyHigh_filtered_df <- do.call(rbind.data.frame, shootings_onlyHigh_filtered)
shootings_onlyElementary_filtered_df <- do.call(rbind.data.frame, shootings_onlyElementary_filtered)
shootings_onlyHighVor_filtered_df <- do.call(rbind.data.frame, shootings_onlyHighVor_filtered)
shootings_onlyElementaryVor_filtered_df <- do.call(rbind.data.frame, shootings_onlyElementaryVor_filtered)

names(homicides_onlyCensusBlock_filtered_df)[1] <- "WhichCensusBlock"
names(homicides_onlyCensusTract_filtered_df)[1] <- "WhichCensusTract"
names(homicides_onlyCensusGroup_filtered_df)[1] <- "WhichCensusGroup"
names(homicides_onlyCensusZip_filtered_df)[1] <- "WhichCensusZip"
names(homicides_onlyHigh_filtered_df)[1] <- "WhichHigh"
names(homicides_onlyElementary_filtered_df)[1] <- "WhichElementary"
names(homicides_onlyHighVor_filtered_df)[1] <- "WhichHighVor"
names(homicides_onlyElementaryVor_filtered_df)[1] <- "WhichElementaryVor"

names(shootings_onlyCensusBlock_filtered_df)[1] <- "WhichCensusBlock"
names(shootings_onlyCensusTract_filtered_df)[1] <- "WhichCensusTract"
names(shootings_onlyCensusGroup_filtered_df)[1] <- "WhichCensusGroup"
names(shootings_onlyCensusZip_filtered_df)[1] <- "WhichCensusZip"
names(shootings_onlyHigh_filtered_df)[1] <- "WhichHigh"
names(shootings_onlyElementary_filtered_df)[1] <- "WhichElementary"
names(shootings_onlyHighVor_filtered_df)[1] <- "WhichHighVor"
names(shootings_onlyElementaryVor_filtered_df)[1] <- "WhichElementaryVor"

homicides_freqDist <- data.frame(table(homicides_onlyCensusBlock_filtered_df$WhichCensusBlock))
homicides_freqDist_tract <- data.frame(table(homicides_onlyCensusTract_filtered_df$WhichCensusTract))
homicides_freqDist_group <- data.frame(table(homicides_onlyCensusGroup_filtered_df$WhichCensusGroup))
homicides_freqDist_zip <- data.frame(table(homicides_onlyCensusZip_filtered_df$WhichCensusZip))
homicides_freqDist_high <- data.frame(table(homicides_onlyHigh_filtered_df$WhichHigh))
homicides_freqDist_elementary <- data.frame(table(homicides_onlyElementary_filtered_df$WhichElementary))
homicides_freqDist_highVor <- data.frame(table(homicides_onlyHighVor_filtered_df$WhichHighVor))
homicides_freqDist_elementaryVor <- data.frame(table(homicides_onlyElementaryVor_filtered_df$WhichElementaryVor))

shootings_freqDist <- data.frame(table(shootings_onlyCensusBlock_filtered_df$WhichCensusBlock))
shootings_freqDist_tract <- data.frame(table(shootings_onlyCensusTract_filtered_df$WhichCensusTract))
shootings_freqDist_group <- data.frame(table(shootings_onlyCensusGroup_filtered_df$WhichCensusGroup))
shootings_freqDist_zip <- data.frame(table(shootings_onlyCensusZip_filtered_df$WhichCensusZip))
shootings_freqDist_high <- data.frame(table(shootings_onlyHigh_filtered_df$WhichHigh))
shootings_freqDist_elementary <- data.frame(table(shootings_onlyElementary_filtered_df$WhichElementary))
shootings_freqDist_highVor <- data.frame(table(shootings_onlyHighVor_filtered_df$WhichHighVor))
shootings_freqDist_elementaryVor <- data.frame(table(shootings_onlyElementaryVor_filtered_df$WhichElementaryVor))

censusBlock_sf_popln_homicides_freq <- merge(x = censusBlock_sf_popln, y = homicides_freqDist, by.x = "index", by.y = "Var1", all.x = TRUE)
censusTract_sf_popln_homicides_freq <- merge(x = censusTract_sf_popln, y = homicides_freqDist_tract, by.x = "index", by.y = "Var1", all.x = TRUE)
censusGroup_sf_popln_homicides_freq <- merge(x = censusBlockGrp_sf_popln, y = homicides_freqDist_group, by.x = "index", by.y = "Var1", all.x = TRUE)
censusZip_sf_popln_homicides_freq <- merge(x = censusZip_sf_popln, y = homicides_freqDist_zip, by.x = "index", by.y = "Var1", all.x = TRUE)
high_sf_popln_homicides_freq <- merge(x = highSchoolBoundariesMap_sf, y = homicides_freqDist_high, by.x = "index", by.y = "Var1", all.x = TRUE)
elementary_sf_popln_homicides_freq <- merge(x = elementaryBoundariesMap_sf, y = homicides_freqDist_elementary, by.x = "index", by.y = "Var1", all.x = TRUE)
highVor_sf_popln_homicides_freq <- merge(x = highSchoolChicVoronoiPopln_sf, y = homicides_freqDist_highVor, by.x = "index", by.y = "Var1", all.x = TRUE)
elementaryVor_sf_popln_homicides_freq <- merge(x = elementarySchoolChicVoronoiPopln_sf, y = homicides_freqDist_elementaryVor, by.x = "index", by.y = "Var1", all.x = TRUE)

censusBlock_sf_popln_shootings_freq <- merge(x = censusBlock_sf_popln, y = shootings_freqDist, by.x = "index", by.y = "Var1", all.x = TRUE)
censusTract_sf_popln_shootings_freq <- merge(x = censusTract_sf_popln, y = shootings_freqDist_tract, by.x = "index", by.y = "Var1", all.x = TRUE)
censusGroup_sf_popln_shootings_freq <- merge(x = censusBlockGrp_sf_popln, y = shootings_freqDist_group, by.x = "index", by.y = "Var1", all.x = TRUE)
censusZip_sf_popln_shootings_freq <- merge(x = censusZip_sf_popln, y = shootings_freqDist_zip, by.x = "index", by.y = "Var1", all.x = TRUE)
high_sf_popln_shootings_freq <- merge(x = highSchoolBoundariesMap_sf, y = shootings_freqDist_high, by.x = "index", by.y = "Var1", all.x = TRUE)
elementary_sf_popln_shootings_freq <- merge(x = elementaryBoundariesMap_sf, y = shootings_freqDist_elementary, by.x = "index", by.y = "Var1", all.x = TRUE)
highVor_sf_popln_shootings_freq <- merge(x = highSchoolChicVoronoiPopln_sf, y = shootings_freqDist_highVor, by.x = "index", by.y = "Var1", all.x = TRUE)
elementaryVor_sf_popln_shootings_freq <- merge(x = elementarySchoolChicVoronoiPopln_sf, y = shootings_freqDist_elementaryVor, by.x = "index", by.y = "Var1", all.x = TRUE)

censusBlock_sf_popln_homicides_freq$Freq[is.na(censusBlock_sf_popln_homicides_freq$Freq)] <- 0
censusTract_sf_popln_homicides_freq$Freq[is.na(censusTract_sf_popln_homicides_freq$Freq)] <- 0
censusGroup_sf_popln_homicides_freq$Freq[is.na(censusGroup_sf_popln_homicides_freq$Freq)] <- 0
censusZip_sf_popln_homicides_freq$Freq[is.na(censusZip_sf_popln_homicides_freq$Freq)] <- 0
high_sf_popln_homicides_freq$Freq[is.na(high_sf_popln_homicides_freq$Freq)] <- 0
elementary_sf_popln_homicides_freq$Freq[is.na(elementary_sf_popln_homicides_freq$Freq)] <- 0
highVor_sf_popln_homicides_freq$Freq[is.na(highVor_sf_popln_homicides_freq$Freq)] <- 0
elementaryVor_sf_popln_homicides_freq$Freq[is.na(elementaryVor_sf_popln_homicides_freq$Freq)] <- 0

censusBlock_sf_popln_shootings_freq$Freq[is.na(censusBlock_sf_popln_shootings_freq$Freq)] <- 0
censusTract_sf_popln_shootings_freq$Freq[is.na(censusTract_sf_popln_shootings_freq$Freq)] <- 0
censusGroup_sf_popln_shootings_freq$Freq[is.na(censusGroup_sf_popln_shootings_freq$Freq)] <- 0
censusZip_sf_popln_shootings_freq$Freq[is.na(censusZip_sf_popln_shootings_freq$Freq)] <- 0
high_sf_popln_shootings_freq$Freq[is.na(high_sf_popln_shootings_freq$Freq)] <- 0
elementary_sf_popln_shootings_freq$Freq[is.na(elementary_sf_popln_shootings_freq$Freq)] <- 0
highVor_sf_popln_shootings_freq$Freq[is.na(highVor_sf_popln_shootings_freq$Freq)] <- 0
elementaryVor_sf_popln_shootings_freq$Freq[is.na(elementaryVor_sf_popln_shootings_freq$Freq)] <- 0

censusBlock_sf_popln_homicides_freq$Freq <- censusBlock_sf_popln_homicides_freq$Freq/5
censusTract_sf_popln_homicides_freq$Freq <- censusTract_sf_popln_homicides_freq$Freq/5
censusGroup_sf_popln_homicides_freq$Freq <- censusGroup_sf_popln_homicides_freq$Freq/5
censusZip_sf_popln_homicides_freq$Freq <- censusZip_sf_popln_homicides_freq$Freq/5
high_sf_popln_homicides_freq$Freq <- high_sf_popln_homicides_freq$Freq/5
elementary_sf_popln_homicides_freq$Freq <- elementary_sf_popln_homicides_freq$Freq/5
highVor_sf_popln_homicides_freq$Freq <- highVor_sf_popln_homicides_freq$Freq/5
elementaryVor_sf_popln_homicides_freq$Freq <- elementaryVor_sf_popln_homicides_freq$Freq/5

censusBlock_sf_popln_shootings_freq$Freq <- censusBlock_sf_popln_shootings_freq$Freq/5
censusTract_sf_popln_shootings_freq$Freq <- censusTract_sf_popln_shootings_freq$Freq/5
censusGroup_sf_popln_shootings_freq$Freq <- censusGroup_sf_popln_shootings_freq$Freq/5
censusZip_sf_popln_shootings_freq$Freq <- censusZip_sf_popln_shootings_freq$Freq/5
high_sf_popln_shootings_freq$Freq <- high_sf_popln_shootings_freq$Freq/5
elementary_sf_popln_shootings_freq$Freq <- elementary_sf_popln_shootings_freq$Freq/5
highVor_sf_popln_shootings_freq$Freq <- highVor_sf_popln_shootings_freq$Freq/5
elementaryVor_sf_popln_shootings_freq$Freq <- elementaryVor_sf_popln_shootings_freq$Freq/5

censusBlock_sf_popln_homicides_freq$total_population[censusBlock_sf_popln_homicides_freq$total_population<=30] <- 0
censusTract_sf_popln_homicides_freq$total_population[censusTract_sf_popln_homicides_freq$total_population<=30] <- 0
censusGroup_sf_popln_homicides_freq$total_population[censusGroup_sf_popln_homicides_freq$total_population<=30] <- 0
censusZip_sf_popln_homicides_freq$total_population[censusZip_sf_popln_homicides_freq$total_population<=30] <- 0
highVor_sf_popln_homicides_freq$total_population[highVor_sf_popln_homicides_freq$total_population<=30] <- 0
elementaryVor_sf_popln_homicides_freq$total_population[elementaryVor_sf_popln_homicides_freq$total_population<=30] <- 0

censusBlock_sf_popln_shootings_freq$total_population[censusBlock_sf_popln_shootings_freq$total_population<=30] <- 0
censusTract_sf_popln_shootings_freq$total_population[censusTract_sf_popln_shootings_freq$total_population<=30] <- 0
censusGroup_sf_popln_shootings_freq$total_population[censusGroup_sf_popln_shootings_freq$total_population<=30] <- 0
censusZip_sf_popln_shootings_freq$total_population[censusZip_sf_popln_shootings_freq$total_population<=30] <- 0
highVor_sf_popln_shootings_freq$total_population[highVor_sf_popln_shootings_freq$total_population<=30] <- 0
elementaryVor_sf_popln_shootings_freq$total_population[elementaryVor_sf_popln_shootings_freq$total_population<=30] <- 0


censusBlock_sf_popln_homicides_freq$FreqPerCapita <- censusBlock_sf_popln_homicides_freq$Freq / censusBlock_sf_popln_homicides_freq$total_population
censusTract_sf_popln_homicides_freq$FreqPerCapita <- censusTract_sf_popln_homicides_freq$Freq / censusTract_sf_popln_homicides_freq$total_population
censusGroup_sf_popln_homicides_freq$FreqPerCapita <- censusGroup_sf_popln_homicides_freq$Freq / censusGroup_sf_popln_homicides_freq$total_population
censusZip_sf_popln_homicides_freq$FreqPerCapita <- censusZip_sf_popln_homicides_freq$Freq / censusZip_sf_popln_homicides_freq$total_population
highVor_sf_popln_homicides_freq$FreqPerCapita <- highVor_sf_popln_homicides_freq$Freq / highVor_sf_popln_homicides_freq$total_population
elementaryVor_sf_popln_homicides_freq$FreqPerCapita <- elementaryVor_sf_popln_homicides_freq$Freq / elementaryVor_sf_popln_homicides_freq$total_population

censusBlock_sf_popln_shootings_freq$FreqPerCapita <- censusBlock_sf_popln_shootings_freq$Freq / censusBlock_sf_popln_shootings_freq$total_population
censusTract_sf_popln_shootings_freq$FreqPerCapita <- censusTract_sf_popln_shootings_freq$Freq / censusTract_sf_popln_shootings_freq$total_population
censusGroup_sf_popln_shootings_freq$FreqPerCapita <- censusGroup_sf_popln_shootings_freq$Freq / censusGroup_sf_popln_shootings_freq$total_population
censusZip_sf_popln_shootings_freq$FreqPerCapita <- censusZip_sf_popln_shootings_freq$Freq / censusZip_sf_popln_shootings_freq$total_population
highVor_sf_popln_shootings_freq$FreqPerCapita <- highVor_sf_popln_shootings_freq$Freq / highVor_sf_popln_shootings_freq$total_population
elementaryVor_sf_popln_shootings_freq$FreqPerCapita <- elementaryVor_sf_popln_shootings_freq$Freq / elementaryVor_sf_popln_shootings_freq$total_population


library(cartogram)
censusZip_sf_popln_homicides_freq_projected <- st_transform(censusZip_sf_popln_homicides_freq, 26916)
censusZip_sf_popln_shootings_freq_projected <- st_transform(censusZip_sf_popln_shootings_freq, 26916)
highVor_sf_popln_homicides_freq_projected <- st_transform(highVor_sf_popln_homicides_freq, 26916)
highVor_sf_popln_shootings_freq_projected <- st_transform(highVor_sf_popln_shootings_freq, 26916)
elementaryVor_sf_popln_homicides_freq_projected <- st_transform(elementaryVor_sf_popln_homicides_freq, 26916)
elementaryVor_sf_popln_shootings_freq_projected <- st_transform(elementaryVor_sf_popln_shootings_freq, 26916)

censusZip_sf_popln_homicides_freq_projected_rmna <- subset(censusZip_sf_popln_homicides_freq_projected, (!is.na(censusZip_sf_popln_homicides_freq_projected$FreqPerCapita)) & (!is.infinite(censusZip_sf_popln_homicides_freq_projected$FreqPerCapita)))
censusZip_sf_popln_shootings_freq_projected_rmna <- subset(censusZip_sf_popln_shootings_freq_projected, (!is.na(censusZip_sf_popln_shootings_freq_projected$FreqPerCapita)) & (!is.infinite(censusZip_sf_popln_shootings_freq_projected$FreqPerCapita)))
highVor_sf_popln_homicides_freq_projected_rmna <- subset(highVor_sf_popln_homicides_freq_projected, (!is.na(highVor_sf_popln_homicides_freq_projected$FreqPerCapita)) & (!is.infinite(highVor_sf_popln_homicides_freq_projected$FreqPerCapita)))
highVor_sf_popln_shootings_freq_projected_rmna <- subset(highVor_sf_popln_shootings_freq_projected, (!is.na(highVor_sf_popln_shootings_freq_projected$FreqPerCapita)) & (!is.infinite(highVor_sf_popln_shootings_freq_projected$FreqPerCapita)))
elementaryVor_sf_popln_homicides_freq_projected_rmna <- subset(elementaryVor_sf_popln_homicides_freq_projected, (!is.na(elementaryVor_sf_popln_homicides_freq_projected$FreqPerCapita)) & (!is.infinite(elementaryVor_sf_popln_homicides_freq_projected$FreqPerCapita)))
elementaryVor_sf_popln_shootings_freq_projected_rmna <- subset(elementaryVor_sf_popln_shootings_freq_projected, (!is.na(elementaryVor_sf_popln_shootings_freq_projected$FreqPerCapita)) & (!is.infinite(elementaryVor_sf_popln_shootings_freq_projected$FreqPerCapita)))


censusZip_sf_popln_homicides_freq_projected_cart <- cartogram_cont(censusZip_sf_popln_homicides_freq_projected_rmna, "total_population", itermax=15, maxSizeError = 1.0001, prepare="adjust", threshold = 0.05)
censusZip_sf_popln_shootings_freq_projected_cart <- cartogram_cont(censusZip_sf_popln_shootings_freq_projected_rmna, "total_population", itermax=15, maxSizeError = 1.0001, prepare="adjust", threshold = 0.05)
highVor_sf_popln_homicides_freq_projected_cart <- cartogram_cont(highVor_sf_popln_homicides_freq_projected_rmna, "total_population", itermax=15, maxSizeError = 1.0001, prepare="adjust", threshold = 0.05)
highVor_sf_popln_shootings_freq_projected_cart <- cartogram_cont(highVor_sf_popln_shootings_freq_projected_rmna, "total_population", itermax=15, maxSizeError = 1.0001, prepare="adjust", threshold = 0.05)
elementaryVor_sf_popln_homicides_freq_projected_cart <- cartogram_cont(elementaryVor_sf_popln_homicides_freq_projected_rmna, "total_population", itermax=15, maxSizeError = 1.0001, prepare="adjust", threshold = 0.05)
elementaryVor_sf_popln_shootings_freq_projected_cart <- cartogram_cont(elementaryVor_sf_popln_shootings_freq_projected_rmna, "total_population", itermax=15, maxSizeError = 1.0001, prepare="adjust", threshold = 0.05)



censusZip_sf_popln_homicides_freq_projected_cart$zipCode <- censusZip_sf_popln_homicides_freq_projected_cart$zip
censusZip_sf_popln_shootings_freq_projected_cart$zipCode <- censusZip_sf_popln_shootings_freq_projected_cart$zip

library(ggh4x)
censusZip_homicides_centroids <- 
  gCentroid( 
    spgeom = methods::as( object = censusZip_sf_popln_homicides_freq_projected_cart, Class = "Spatial" )
    , byid = TRUE 
  )

censusZip_shootings_centroids <- 
  gCentroid( 
    spgeom = methods::as( object = censusZip_sf_popln_shootings_freq_projected_cart, Class = "Spatial" )
    , byid = TRUE 
  )

censusZip_sf_popln_homicides_freq_projected_cart$x <- censusZip_homicides_centroids$x
censusZip_sf_popln_homicides_freq_projected_cart$y <- censusZip_homicides_centroids$y
censusZip_sf_popln_shootings_freq_projected_cart$x <- censusZip_shootings_centroids$x
censusZip_sf_popln_shootings_freq_projected_cart$y <- censusZip_shootings_centroids$y

ggplot() + geom_sf(data = censusBlock_sf_popln_homicides_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Homicides \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides \nin Chicago by Census Block", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Homicides in Chicago by Census Block.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusTract_sf_popln_homicides_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Homicides \nper Year", na.value = "white", breaks = c(0, 0.778, 1, 2, 3), labels = c(1, 6, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides \nin Chicago by Census Tract", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Homicides in Chicago by Census Tract.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusGroup_sf_popln_homicides_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Homicides \nper Year", na.value = "white", breaks = c(0, 0.477, 1, 2, 3), labels = c(1, 3, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides \nin Chicago by Block Group", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Homicides in Chicago by Block Group.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusZip_sf_popln_homicides_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Homicides \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides \nin Chicago by Zip Code", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Homicides in Chicago by Zip Code.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = high_sf_popln_homicides_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Homicides \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides \nin Chicago by High School Boundaries", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Homicides in Chicago by High School Boundaries.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = elementary_sf_popln_homicides_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Homicides \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides \nin Chicago by Elementary School Boundaries", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Homicides in Chicago by Elementary School Boundaries.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = highVor_sf_popln_homicides_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Homicides \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides \nin Chicago by Closest High School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Homicides in Chicago by Closest High School.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = elementaryVor_sf_popln_homicides_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Homicides \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides \nin Chicago by Closest Elementary School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Homicides in Chicago by Closest Elementary School.png", width = 8.5, height = 11, units = "in")

ggplot() + geom_sf(data = censusBlock_sf_popln_shootings_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Shootings \nper Year", na.value = "white", breaks = c(0, 0.477, 1, 2, 3), labels = c(1, 3, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings \nin Chicago by Census Block", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Shootings in Chicago by Census Block.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusTract_sf_popln_shootings_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Shootings \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings \nin Chicago by Census Tract", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Shootings in Chicago by Census Tract.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusGroup_sf_popln_shootings_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Shootings \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings \nin Chicago by Block Group", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Shootings in Chicago by Block Group.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusZip_sf_popln_shootings_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Shootings \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings \nin Chicago by Zip Code", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Shootings in Chicago by Zip Code.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = high_sf_popln_shootings_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Shootings \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings \nin Chicago by High School Boundaries", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Shootings in Chicago by High School Boundaries.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = elementary_sf_popln_shootings_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Shootings \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings \nin Chicago by Elementary School Boundaries", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Shootings in Chicago by Elementary School Boundaries.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = highVor_sf_popln_shootings_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Shootings \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings \nin Chicago by Closest High School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Shootings in Chicago by Closest High School.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = elementaryVor_sf_popln_shootings_freq, mapping = aes(fill = log10(Freq), colour = log10(Freq)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(option = "mako", direction = -1, na.value = "white") + scale_fill_viridis(option="mako", direction = -1, name = "Shootings \nper Year", na.value = "white", breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings \nin Chicago by Closest Elementary School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Shootings in Chicago by Closest Elementary School.png", width = 8.5, height = 11, units = "in")


ggplot() + geom_sf(data = censusBlock_sf_popln_homicides_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Census Block", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Homicides in Chicago by Census Block.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusTract_sf_popln_homicides_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Census Tract", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Homicides in Chicago by Census Tract.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusGroup_sf_popln_homicides_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Block Group", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Homicides in Chicago by Block Group.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusZip_sf_popln_homicides_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Zip Code", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Homicides in Chicago by Zip Code.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = highVor_sf_popln_homicides_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Closest High School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Homicides in Chicago by Closest High School.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = elementaryVor_sf_popln_homicides_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Closest Elementary School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Homicides in Chicago by Closest Elementary School.png", width = 8.5, height = 11, units = "in")

ggplot() + geom_sf(data = censusBlock_sf_popln_shootings_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Census Block", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Shootings in Chicago by Census Block.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusTract_sf_popln_shootings_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Census Tract", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Shootings in Chicago by Census Tract.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusGroup_sf_popln_shootings_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Block Group", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Shootings in Chicago by Block Group.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusZip_sf_popln_shootings_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Zip Code", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Shootings in Chicago by Zip Code.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = highVor_sf_popln_shootings_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Closest High School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Shootings in Chicago by Closest High School.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = elementaryVor_sf_popln_shootings_freq, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.2, 0.2), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Closest Elementary School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.") + geom_sf(data = overlayStreets, fill = NA, size = 0.0, color = "grey")
ggsave("Yearly Per Capita Shootings in Chicago by Closest Elementary School.png", width = 8.5, height = 11, units = "in")

ggplot() + geom_sf(data = censusZip_sf_popln_homicides_freq_projected_cart, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.15, 0.15), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Zip Code", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.")
ggsave("Homicides Cartogram by Zip Code.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = censusZip_sf_popln_shootings_freq_projected_cart, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.15, 0.15), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Zip Code", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.")
ggsave("Shootings Cartogram by Zip Code.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = highVor_sf_popln_homicides_freq_projected_cart, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.15, 0.15), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Closest High School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.")
ggsave("Homicides Cartogram by Closest High School.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = highVor_sf_popln_shootings_freq_projected_cart, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.15, 0.15), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Closest High School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.")
ggsave("Shootings Cartogram by Closest High School.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = elementaryVor_sf_popln_homicides_freq_projected_cart, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Homicides \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.15, 0.15), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Homicides per 10,000 people \nin Chicago by Closest Elementary School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nHomicides are victims in that database classified as non-shooting homicide or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.")
ggsave("Homicides Cartogram by Closest Elementary School.png", width = 8.5, height = 11, units = "in")
ggplot() + geom_sf(data = elementaryVor_sf_popln_shootings_freq_projected_cart, mapping = aes(fill = log10(FreqPerCapita), colour = log10(FreqPerCapita)), size = 0.0 ) + guides(colour = "none") + scale_color_viridis(na.value="white", option = "mako", direction = -1) + scale_fill_viridis(na.value="white", option="mako", direction = -1, name = "Shootings \nper Year \nper 10,000 People", breaks = c(-4, -3, -2, -1), labels = c(1, 10, 100, 1000)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank(), plot.title = element_text(size = 20, hjust = 0.5), legend.position = c(0.15, 0.15), legend.key.size = unit(1, "cm"), legend.title = element_text(size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "transparent")) + labs(title = "Yearly Shootings per 10,000 people \nin Chicago by Closest Elementary School", caption = "Data provided by the Chicago Police Department and accessed from the Chicago Data Portal.\nShootings are victims in that database classified as non-fatal shooting or fatal shooting.\nData from August 1, 2016 -  July 31, 2021.")
ggsave("Shootings Cartogram by Closest Elementary School.png", width = 8.5, height = 11, units = "in")

