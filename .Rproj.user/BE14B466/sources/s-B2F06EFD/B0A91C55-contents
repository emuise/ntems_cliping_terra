vlce_df <- to_process %>%
  filter(var == "VLCE") %>%
  mutate(path_in = here::here("M:/", "VLCE2.0_1984-2021", paste0("UTM_", zone), paste0("LC_Class_HMM_", zone, "_", year, "_v20_v20.dat")),
         path_out = here::here(outpath, zone, var, paste0("LC_Class_HMM_", zone, "_", year, "_v20_v20.dat")))

attribution_df <- to_process %>%
  filter(var == "change_attribution") %>%
  distinct(var, zone) %>%
  mutate(path_in = here::here("N:/", paste0("UTM_", zone), "Results", "Change_attribution", paste0("Attribution_UTM", zone, "_v2.dat")),
         path_out = here::here(outpath, zone, var, paste0("Attribution_UTM", zone, "_v2.dat")))

metrics_df <- to_process %>%
  filter(var == "change_metrics") %>%
  distinct(var, zone) %>%
  mutate(path_in = list(here::here("N:/", paste0("UTM_", zone), "Results", "Change_metrics") %>%
                          list.files(full.names = T, pattern = ".dat$"))) %>%
  unnest(path_in) %>%
  mutate(path_out = str_split(path_in, "/")) %>%
  unnest(path_out) %>%
  filter(str_detect(path_out, ".dat")) %>%
  mutate(path_out = here::here(outpath, zone, var, path_out)) %>%
  filter(str_detect(path_in, paste0("M", zone))) %>%
  distinct(path_in, .keep_all = T)

proxies_df <- to_process %>% 
  filter(var == "proxies") %>%
  mutate(path_in = here::here("N:/", paste0("UTM_", zone), "Results", "proxy_values_fitted", paste0("SRef_UTM", zone, "_", year, "_fitted_proxy_v2.dat")),
         path_out = here::here(outpath, zone, var, paste0("SRef_UTM", zone, "_", year, "_fitted_proxy_v2.dat")))

structure_df <- to_process %>%
  filter(startsWith(var, "structure")) %>%
  mutate(var = gsub("structure_", "", var),
         path_in = here::here("M:/", "annual_forest_structure_unfitted_1984-2021", paste0("UTM_", zone), var, paste0("UTM_", zone, "_", var, "_", year, ".dat")),
         path_out = here::here(outpath, zone, "structure", var, paste0("UTM_", zone, "_", var, "_", year, ".dat")))

topo_df <- to_process %>%
  filter(var == "topography") %>%
  distinct(var, zone) %>%
  mutate(path_in = list(here::here("G:/", "TopoData_v3") %>%
                          list.files(full.names = T, pattern = ".dat$", recursive = T))) %>%
  unnest(path_in) %>% 
  mutate(path_out = str_split(path_in, "/")) %>%
  unnest(path_out) %>%
  filter(str_detect(path_out, ".dat")) %>%
  mutate(path_out = here::here(outpath, zone, var, path_out)) %>%
  filter(str_detect(path_in, paste0("M", zone))) %>%
  distinct(path_in, .keep_all = T)

latlon_df <- to_process %>%
  filter(var == "lat" | var == "lon") %>%
  distinct(var, zone) %>%
  mutate(path_in = here::here("G:/", "ntems_lat-lon", var, paste0("UTM", zone, "_", var, ".dat")),
         path_out = here::here(outpath, zone, var, paste0("UTM", zone, "_", var, ".dat")))
         

process_df <- bind_rows(vlce_df,
                        proxies_df,
                        attribution_df,
                        metrics_df,
                        topo_df,
                        structure_df,
                        latlon_df)

rm(vlce_df, 
   proxies_df,
   attribution_df,
   metrics_df,
   topo_df,
   structure_df)
