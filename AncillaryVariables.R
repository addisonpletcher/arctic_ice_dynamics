# Install the necessary packages if you haven't already
install.packages("raster")
# install.packages("rgdal")  # This might also be necessary for handling GRIB files

# Load the raster package
library(raster)

# Reading file
grib_data <- raster("ERA5.grib")

# Plot
plot(grib_data)

# multiple layers and want to read all of them
grib_stack <- stack("ERA5.grib")

# plot specific layer
plot(grib_stack[[14]])
