# mosaic clipped ntems rasters together
# first must use the utm masks

ntems_mosaicer <- function(tibble) {
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
  rasts_proj <- map(rasts, project, y = template, 
                  align = T, method = "near")
  
  # utm_masks need to align with the rasts_proj grids and have the same extent before merging
  # Need to project + crop 
  # Here, I crop first to work on the AOI extent, project (faster than projecting for all mask) and finally crop again to rast_proj ()
  
  if(!all(file.exists(here::here(outpath, "mosaiced", "UTM_mask", tibble$zone,"UTM_mask_LCC.dat")))) {
    utm_masks_crop <- map2(.x = utm_masks, .y = rasts_proj, .f = crop, snap = "out")
    
    utm_masks_proj <- map2(.x = utm_masks_crop, .y = rasts_proj, project,
                           align = TRUE, method = "near") %>%
      map2(.x = ., .y = rasts_proj, .f = crop)
    
    # Store ouput of dir.creat to avoid printing in the console
    new_dir <- map(here::here(outpath, "mosaiced", "UTM_mask", tibble$zone), dir.create, showWarnings = F, recursive = T)
    
    utm_masks_proj <- map2(.x = utm_masks_proj, .y = here::here(outpath, "mosaiced", "UTM_mask", tibble$zone,"UTM_mask_LCC.dat"), 
                           .f = writeRaster, 
                           datatype = "INT1U",
                           filetype = "ENVI",
                           overwrite = T,
                           todisk = T,
                           memfrac = 0.75)
    
  }else{
    utm_masks_proj <- map(here::here(outpath, "mosaiced", "UTM_mask", tibble$zone, "UTM_mask_LCC.dat"), 
                          rast)
  }
  
  mosaiced <- rasts_proj %>%
    map2(.x = ., .y = utm_masks, .f = mask, maskvalues = 0) %>%
    sprc() %>%
    merge()
  
  print("Mosaiced")
    
  if (var == "VLCE" | var == "change_attribution") {
    print("adding levels and colours")
    
    levels(mosaiced) <- levels(rast(ref))
    coltab(mosaiced) <- coltab(rast(ref))
  } 
  
  print("saving")
  
  writeRaster(mosaiced, filename = save_path,
              datatype = dtype,
              filetype = "ENVI",
              overwrite = T,
              todisk = T,
              memfrac = 0.75)
  
  print("----------------------------")
  print(" ")
  terra::tmpFiles(remove = T)
}

