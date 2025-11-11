library(terra)
library(tidyterra)
library(FedData)

f <- list.files("Z:\\cli2\\landsat", full.names = T)

f

# read in files 3-10 as a single multi-band raster
lc <- rast(f[3:10])

#look at Band 5 to get information about the data
lc[[5]]

# create a summary of the data values
summary(lc[[5]])

# make a quick plot to see how the data looks
plot(lc[[5]])

# we can perform math on the raster layer right inside the ploit call
plot(lc[[5]]*0.0000275-0.2)

# calculate ndvi
ndvi <- (lc[[5]]-lc[[4]])/(lc[[5]]+lc[[4]])

names(ndvi) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi)


# read in the shape file of dec lands
dec <- vect("Z:\\cli2\\NYS_DEC_Lands")

# remember from last time that the attribute table just like a data frame
# we'll use this to subset to Madison County
mad_dec <- dec[dec$COUNTY=="MADISON",]

# we could also use the crop function
# what dimensions of our raster layer are used to crop the vector layer
lc_dec <- crop(dec,lc)

# lastly, create a buffer around the Madison County DEC lands
# what are the units? 
mad_buf <- buffer(mad_dec, width = 1000, singlesided = T)

# create a plot to look at our Madison County data
plot(mad_dec, col = "red")
plot(mad_buf, col = "yellow", add = T)
