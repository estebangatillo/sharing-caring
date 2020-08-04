# Create focal buffers for composite raster bands
source("./R/init.R")

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
