# mosaic clipped ntems rasters together
# first must use the utm masks

ntems_mosaicer_gdal <- function(tibble) {
  
  # Load tibble with data type names used in terra and gdal
  source(here::here("scripts", "gdal_data_types.R"))
  
  year <- tibble %>%
    pull(year) %>%
    unique()
  
  var <- tibble %>%
    pull(var) %>%
    unique()
  
  save_path <- tibble %>%
    pull(mosaic_path) %>%
    unique()
  
  print("mosaicing ------------------")
  print(paste("zones:", toString(utmzone_all)))
  print(paste("year:", year))
  print(paste("variable:", var))
  print(paste("save path:", save_path))
  
  dir.create(dirname(save_path), showWarnings = F, recursive = T)
  
  ref <- tibble %>%
    head(1) %>%
    pull(path_out)
  
  dtype <- ref %>%
    get_data_type()
  
  print(paste("datatype:", dtype))
  #print(paste("mosaicing at: ", save_path))
  print("----------------------------")
  
  rasts <- tibble %>%
    pull(path_out) %>%
    map(rast)
  
  print("Mosaicing")
  
  # Project rasts to template
  
  ## To align projected rasters with the template grid, we need to give gdal 
  ## the target resolution and extent of the projected file
  ## Here, we project the extent of rasts to the CRS of template and align 
  ## the extent to the grid of template. The result is trg_ext and is supplied 
  ## to gdalwarp
  
  template_crs <- sf::st_crs(template)
  ext_template <- ext(template)
  ext_template_poly <- as.polygons(ext_template)
  terra::crs(ext_template_poly) <- template_crs$wkt
  
  trg_ext <- map(rasts, ext) %>%
    map(., as.polygons) %>%
    map2(.x = ., .y = rasts,
         function(x, y){
           out <- x 
           crs(out) <- sf::st_crs(y)$wkt
           return(out) 
         }) %>%
    map(., terra::project, y = template_crs$wkt) %>%
    map(., function(x) terra::align(ext(x), template, "out")) %>%
    map(., function(x) c(x$xmin, x$ymin, x$xmax, x$ymax))
  
  # gdalwarp writes output on disk. Since we need to load these files later on, 
  # we explicitly set temporary file names following terra syntax (spat_) so they can be automatically deleted later on
  
  tmp_files <- map(1:length(rasts), function(x) tempfile("spat_", tmpdir = terra_temp, fileext = ".tif"))
  
  pmap(.l = list(srcfile = tibble %>% pull(path_out), 
                 dstfile = tmp_files, 
                 te = trg_ext), 
       .f = gdalUtilities::gdalwarp, 
       t_srs = template_crs$wkt, 
       tr = c(30, 30), 
       r = "near", 
       overwrite = TRUE)
  
  # Load projected rasters as SpatRaster
  rasts_proj <- map(tmp_files, rast)
  
  # utm_masks need to align with the rasts_proj grids and have the same extent before merging
  # Need to project + crop 
  # Here, I crop first to work on the AOI extent, and then project (faster than projecting for all mask)
  
  # Projected UTM mask do not change depending on which layer is mosaiced so we write them down to disk to avoid 
  # processing them every time. Once they are created (first layer mosaiced), they are read in.
  
  if(!all(file.exists(here::here(outpath, "mosaiced", "UTM_mask", tibble$zone,"UTM_mask_LCC.dat")))) {
    
    # Store ouput of dir.creat to avoid printing in the console
    new_dir <- map(here::here(outpath, "mosaiced", "UTM_mask", tibble$zone), dir.create, showWarnings = F, recursive = T)
    
    utm_masks_crop <- map2(.x = utm_masks, .y = rasts_proj, .f = crop, snap = "out")
    
    # Grab file names with sources() might not work if files are not written to temp directory
    pmap(.l = list(srcfile = map(utm_masks_crop, sources), 
                   dstfile = here::here(outpath, "mosaiced", "UTM_mask", tibble$zone,"UTM_mask_LCC.dat"), 
                   te = trg_ext), 
         .f = gdalUtilities::gdalwarp, 
         t_srs = template_crs$wkt, 
         tr = c(30, 30), 
         r = "near", 
         ot = filter(data_types, terra == "INT1U") %>% pull(gdal),
         overwrite = TRUE)
    
  }else{
    utm_masks_proj <- map(here::here(outpath, "mosaiced", "UTM_mask", tibble$zone, "UTM_mask_LCC.dat"), 
                          rast)
  }
  
  # Mask projected rasters with projected UTM masks 
  rasts_masked <- rasts_proj %>%
    map2(.x = ., .y = utm_masks_proj, .f = mask, maskvalues = 0)
  
  # Mosaic rasters
  # Here with use gdalUtils (not on CRAN anymore) instead of gdalUtilities because 
  # the function mosaic_rasters is not available in gdalUtilites. What it does in the background (I think) is calling gdalbuildvrt + gdal_translate 
  # but it looks faster with that function instead of calling both processes separately with gdalUtilites
  
  # Grab file names with sources() might not work if files are not written to temp directory
  gdalUtils::mosaic_rasters(map(rasts_masked, sources), 
                            save_path, 
                            force_ot = filter(data_types, terra == dtype) %>% pull(gdal))
  
  # temp_vrt <- tempfile(tmpdir = "D:/Temp/", fileext = ".vrt")
  # 
  # gdalUtilities::gdalbuildvrt(gdalfile = unlist(map(rasts_masked, sources)), 
  #                             output.vrt = temp_vrt,
  #                             separate = FALSE, 
  #                             overwrite = TRUE)
  # 
  # mosaiced <- rast(temp_vrt)
  
  # gdal_format <- filter(data_types, terra == dtype) %>%
  #   pull(gdal)
  # 
  # temp_translate <- tempfile(tmpdir = "D:/Temp/", fileext = ".dat")
  # 
  # gdal_translate(src_dataset = temp_vrt, 
  #                dst_dataset = temp_translate, 
  #                of = "ENVI", 
  #                ot = gdal_format)
  
  # mosaiced <- map2(rasts, utm_masks, mask) %>%
  #   map(project, y = template, align = T, method = "near") %>%
  #   sprc() %>%
  #   merge()
  
  print("Mosaiced")

  # Have not looked into how to do this with gdal so need to read mosaic back in and overwrite it
  if (var == "VLCE" | var == "change_attribution") {
    print("adding levels and colours")
    
    # Need to read mosaic processed with gdal
    mosaiced <- rast(save_path)

    levels(mosaiced) <- levels(rast(ref))
    coltab(mosaiced) <- coltab(rast(ref))
    
    writeRaster(mosaiced, filename = save_path,
                datatype = dtype,
                filetype = "ENVI",
                overwrite = T,
                todisk = T,
                memfrac = 0.75)
  }
  
  # print("saving")
  
  # writeRaster(mosaiced, filename = save_path,
  #             datatype = dtype,
  #             filetype = "ENVI",
  #             overwrite = T,
  #             todisk = T,
  #             memfrac = 0.75)
  
  print("----------------------------")
  print(" ")
  terra::tmpFiles(remove = T)
}

