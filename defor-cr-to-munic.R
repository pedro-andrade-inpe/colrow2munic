
require(dplyr)

sf::sf_use_s2(FALSE)

biomes <- geobr::read_biomes()[-7,]
munic <- geobr::read_municipality() %>%
  dplyr::mutate(marea = sf::st_area(.))

dataDir <- "/Users/alinesoterroni/Dropbox/ALINE_RScript/colrow"
myCR   <- colrow::getCR("Brazil", dataDir) %>%
  sf::st_transform(sf::st_crs(munic)) %>%
  dplyr::mutate(crarea = sf::st_area(.))

mdata <- read.csv("data_TEDX_MAPS.CSV", header = FALSE) %>%
  dplyr::filter(V7 == "ZD") %>%
  dplyr::select(V2, V8)

names(mdata) <- c("ID", "Value")

sum(mdata$Value) # 57667.35

myCR <- myCR %>%
  dplyr::inner_join(mdata, by = "ID")

max(myCR$Value)

tm_shape(myCR) +
  tm_fill(col = "Value", palette = pal, breaks = c(0,5,10,20,30,50,100,200,300)) +
  tm_shape(biomes) +
  tm_borders()

inter <- sf::st_intersection(munic, myCR) %>%
  dplyr::mutate(iarea = sf::st_area(.)) %>%
  dplyr::mutate(defor = iarea / crarea * Value)

sum(inter$defor) # 57781.39

result <- inter %>% dplyr::group_by(code_muni) %>%
  dplyr::summarise(defor = sum(defor))

final <- dplyr::left_join(munic, as.data.frame(result), "code_muni") %>%
  dplyr::mutate(defor = units::drop_units(defor)) %>%
  dplyr::mutate(defor = coalesce(defor, 0))

sum(final$defor) # 57781.39

library(RColorBrewer)
pal <- brewer.pal(11, "YlGn") # we select 7 colors from the palette


# usando o plot do sf, nao funciona colocar os biomas
#plot(final[,"defor"], breaks = c(0, 20, 40, 60, 80, 100, 150, 200), pal = pal, border=NA)
#plot(st_geometry(biomes), add = TRUE)

# usando o tmap funciona os biomas
require(tmap)

png("map_ZD.png", width = 1240, height = 1240)
tm_shape(final) +
  tm_fill(col = "defor", palette = pal, breaks = c(0,5,10,20,30,50,100,200,500,1000,2000,4000)) +
  tm_shape(biomes) +
  tm_borders()

dev.off()


# tm_fill(col = "defor", palette = pal, breaks = c(0,0.31,1.15,2.34,3.76,5.72,15,30,60,120,150,200,250,307)) +


# usada no TEDx
# png("a_EUSB.png", width = 1240, height = 1240)
# tm_fill(col = "defor", palette = pal, breaks = c(0,0.31,1.15,2.34,3.76,5.72,15,30,60,120,150,230))
