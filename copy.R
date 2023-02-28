loc <- "G:/Lambert_Conical_conformal/Changes_LCC_1984-2016"

tocopy <- list.files(loc) %>%
  .[startsWith(., "Mosaic")] %>%
  here::here(loc, .)


outdir <- "Z:\\ByUser\\Muise\\francois2"

map(tocopy, file.copy, to = outdir)
