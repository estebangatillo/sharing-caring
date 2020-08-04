# Created by Philip Orlando @ CAPA Strategies, LLC
# 2019-09-25
# Predict 10 m resolution Urban Heat Island raster for a given study area

source("./R/init.R")

# Predictions -------------------------------------------------------------
n_cores <- detectCores()-1

# Use our summary table to extract unique model types
cat(paste0("Reading in summary table for ", city_name, "\n"))
mod_sum <- readRDS(list.files("./output/summary-table", paste0(city_name, "-summary-table\\.RDS$"), full.names = T))
# mod_types <- unique(mod_sum$model_type)
mod_types <- "ranger"
response_vars <- unique(mod_sum$response)

# Read in our predictor stack
# TODO original layers included in model or only buffers?
pred_rasts <- stack(list.files("./data/sentinel-2/sentinel-processed", "\\.tif$", full.names = T))

# Remove city name from layer names
names(pred_rasts) <- sub(paste0(str_replace_all(city_name, "-", "."), "\\."), "", names(pred_rasts))

study_extent <- readOGR(paste0("./data/traverse/study_extent/", city_name, "-boundary.shp")) %>%
  spTransform(CRSobj = crs(pred_rasts))

# Apply models to each traverse
pblapply(travs, function(t) {
  lapply(mod_types, function(m) {
    lapply(response_vars, function(y) {
      
      mod <- readRDS(paste0("./output/models/", t, "_", y, "_", m, ".RDS"))
      # df <- as.data.frame(s, xy = T, na.rm=T)
      
      # Parallelize depending on model type
      # TODO debug clusterR()
      if(any(c("randomForest", "lm") %in% class(mod))) {
        # system.time(uhi <- raster::predict(pred_rasts, mod)) # single core is actually faster?!
        raster::beginCluster(n=n_cores,type="SOCK")
        system.time(uhi <- raster::clusterR(pred_rasts, raster::predict, args=list(model=mod)))
        raster::endCluster()
        
      }
      
      # Ranger is already parallelized and doesn't need clusterR()
      if("ranger" %in% class(mod)) {
        system.time({
        uhi <- raster::predict(object = pred_rasts, model = mod, type='response', progress='text',
                       fun = function(model, ...) predict(model, ...)$predictions)
        })
      }
      
      # Clip the predicted raster by our extent boundary (defined in init.R)
      # uhi <- crop(uhi, extent(uhi)-extent_buf)
      uhi <- mask(uhi, study_extent)
      names(uhi) <- paste0(t, "_", y, "_", m, "_uhi")
      
      # Save output
      writeRaster(uhi, filename = paste0("./output/uhi-surfaces/", t, "_", y, '_', m, ".tif"),
                  overwrite = T,
                  format = "GTiff")
    })
  })
})