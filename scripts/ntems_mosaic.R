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
  mosaiced <- map(rasts, project, y = template, 
                  align = T, method = "near")
  two <- mosaiced %>%
    map2(.x = ., .y = utm_masks, .f = mask) #%>%
    sprc() %>%
    merge()
  
  # mosaiced <- map2(rasts, utm_masks, mask) %>%
  #   map(project, y = template, align = T, method = "near") %>%
  #   sprc() %>%
  #   merge()
  
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

ntems_mosaicer(tibble)

