library(terra)
library(tidyterra)
library(FedData)
nlcd_meve16 <-get_nlcd(template = FedData::meve,
                       label = "meve",
                       year = 2016,
                       extraction.dir = "Z:\\cli2\\github\\GEOG331")
nlcd_meve16
terra::plot(nlcd_meve16)

cavm <- vect("Z:\\cli2\\cp_veg_la_shp")

cavm

head(cavm)

terra::plot(cavm, y = "PHYSIOG")
