# Create focal buffers for composite raster bands
source("./R/init.R")

# TODO update band names to match ESA (in init.R)
# band_names <- c('B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B11', 'B12')
# band_names <- c('10.1', '10.2', '10.3', '10.4', '10.5', '10.7', '10.8', '10.9', '10.10', '10.11')

buffers<-c(seq(50, 500, 50), seq(600, 1000, 100))

# Run a single moving window
buffer_band <-function(texton, buf_dist) {
  r <- focal(texton, focalWeight(texton, buf_dist, type="circle"), na.rm=T)
  r[is.na(r)] <- 0
  names(r) <- paste0(names(texton), ".", buf_dist, "m")
  writeRaster(r, filename = paste0("./data/sentinel-2/sentinel-processed/", names(r), ".tif"), overwrite = T)
  # return(r)
}


# Apply focal statistics for each buffer distance to each band
# TODO test cluster at buffer_band loop on Indra
pblapply(band_names, function(band) {
  r <- raster(paste0("./data/sentinel-2/sentinel-processed/",
                        str_replace_all(city_name, "-", "."),
                        ".", band, ".tif"))
  mclapply(buffers, function(buf) {
    buffer_band(texton = r, buf_dist = buf)
    }
    ,mc.cores = detectCores()-1
    )
  }
  #,cl=detectCores()-1
  )
