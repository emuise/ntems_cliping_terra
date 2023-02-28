## make clean polygon masks


mask_rcl <- matrix(c(0, 1,
                NA, 1),
              ncol = 2)


mask_save_base <- map(masks_files, tools::file_path_sans_ext) %>%
  map(basename) %>%
  unlist() 

mask_save_names <- here::here("shapefiles", paste0(mask_save_base, ".shp"))

if(!all(file.exists(mask_save_names))) {
  clean_masks <- map(masks_files, rast) %>%
    map(classify, rcl = mask_rcl) %>%
    map(as.polygons) %>%
    map2(.x = ., .y = mask_save_names, .f = writeVector)
}

utm_masks <- str_subset(mask_save_names, paste(paste0("M", utmzone_all), collapse = "|")) %>%
  map(vect)
