
# Lakes included in LSTM represent the full footprint of what is included in this release
# The vector returned here is used for filtering other targets. This can be used to identify
# any vector of `site_id` values as long as that column exists in the `in_file`.
prep_site_ids <- function(in_file, is_lstm = FALSE) {
  in_data <- read_csv(in_file)
  if(is_lstm) in_data <- filter(in_data, lstm_predictions)
  site_ids <- unique(in_data$site_id)
  return(site_ids)
}

prep_lake_locations <- function(data_file, lakes_in_release, repo_path = '../../lake-temp/lake-temperature-model-prep/') {
  scipiper_freshen_files(data_files = data_file, repo_path = repo_path)
  
  # Load lake centroids and filter to those included in this release
  readRDS(data_file) %>% 
    filter(site_id %in% lakes_in_release) 
}

create_lake_centroid_map <- function(out_file, lake_centroids, state_poly_file) {
  
  # # I did the following steps locally so that I didn't need to install `spData`
  # # to the Singularity container. I saved the RDS into the `in_data/` folder
  # # and then uploaded to Caldera.
  # library(spData)
  # library(tidyverse)
  # lake_states <- c("MN", "MT", "SD", "ND", "WY", "NE", "IA", "MO", "WI", "IL", 
  #                  "KS", "MI", "IN", "OH", "KY", "TN", "MS", "AR", "OK", "TX", "LA")
  # state_info <- tibble(state_name = state.name, state_abbr = state.abb)
  # states_poly <- us_states %>%
  #   left_join(state_info, by = c('NAME' = 'state_name')) %>%
  #   filter(state_abbr %in% lake_states)
  # saveRDS(states_poly, 'in_data/states_poly.rds')
  
  CASC_proj <- '+proj=aea +lat_0=25.5 +lon_0=-97.8 +lat_1=33.6 +lat_2=41.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'
  
  lake_centroids_transf <- lake_centroids %>%
    st_transform(crs = CASC_proj)
  
  states_poly_transf <- readRDS(state_poly_file) %>%
    st_transform(sf::st_crs(CASC_proj))
  
  lakes_plot <- ggplot() +
  geom_sf(data = lake_centroids_transf, fill = 'dodgerblue2', color='dodgerblue3', alpha=0.2, size=1, shape=16) +
  geom_sf(data = states_poly_transf, fill=NA, color='black', size=1) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())  
  ggsave(out_file, lakes_plot, height = 10, width = 10, units = 'in')
}

# Load crosswalk between NLDAS driver meteo files and site ids. Filter to only those covering lakes in the data release
# Use this in lake_metadata.csv and also to prep for the zip of all the files by grouping sort of based on x/y included 
# in the NLDAS file grid. Returns a data.frame with three columns: `site_id`, `meteo_fl`, and `meteo_grp`
prep_nldas_driver_info <- function(data_file, lakes_in_release, n_zips) {
  extract_val <- function(str, x_or_y = c('x', 'y')) {
    x_regex <- 'NLDAS_time\\[0.379366]_x\\[|\\]_y\\[([0-9]{,3})\\].csv'
    y_regex <- 'NLDAS_time\\[0.379366]_x\\[([0-9]{,3})\\]_y\\[|\\].csv'
  
    regex <- switch(x_or_y,
                    x = x_regex,
                    y = y_regex)
  
    split_list <- strsplit(str, regex)
    vals <- purrr::map(split_list, pluck, 2) %>% reduce(c) %>% as.numeric()
    return(vals)
  }

  nldas_driver_info <- read_csv(data_file) %>% 
    filter(site_id %in% lakes_in_release)
  
  # Need to setup groups based on unique files and then rejoin that info
  # back with the row per site information
  meteo_fl_grps <- nldas_driver_info %>%
    select(-site_id) %>%
    distinct() %>%
    # Attempt to arrange the filepaths by x/y values
    # to somewhat organize the zip file groups
    mutate(xval = extract_val(meteo_fl, 'x'),
           yval = extract_val(meteo_fl, 'y')) %>%
    unite(xyval, xval, yval) %>%  
    arrange(xyval) %>% 
    # Actually create group numbers based on the desired number of zips
    mutate(meteo_grp = cut(seq_along(xyval), n_zips, labels = FALSE)) %>%
    select(-xyval)

  nldas_driver_info_grps <- nldas_driver_info %>%
    left_join(meteo_fl_grps, by = "meteo_fl")
  
  return(nldas_driver_info_grps)
}

# Load crosswalk between GCM driver meteo NetCDF files and site ids. Filter to only those covering lakes in the data release
# Use this in lake_metadata.csv. Each NetCDF has a columns of values per cell number, so people will need to know which cell
# a particular site is in to extract the data.
prep_gcm_driver_info <- function(data_file, lakes_in_release) {
  read_csv(data_file) %>% 
    filter(site_id %in% lakes_in_release) %>% 
    select(site_id, cell_no = data_cell_no)
}

prep_lake_metadata <- function(out_file, lake_centroids_sf, lstm_metadata_file, glm_nldas_sites, glm_gcm_sites,
                               lake_gnis_names_file, lake_depths_file, lake_clarity_file, nldas_driver_info, gcm_driver_info,
                               nldas_zipfile_pattern, repo_path = '../../lake-temp/lake-temperature-model-prep/') {
  
  scipiper_freshen_files(data_files = c(lake_gnis_names_file, lake_depths_file, lake_clarity_file), repo_path = repo_path)
  
  lake_metadata <- lake_centroids_sf %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    mutate(site_id = lake_centroids_sf$site_id) %>% 
    left_join(readRDS(lake_gnis_names_file), by = "site_id") %>% 
    left_join(ungroup(readRDS(lake_depths_file)), by = "site_id") %>% 
    # Convert 0 max depths to NAs, see https://github.com/USGS-R/lake-temp-lstm-static-data-release/issues/39
    mutate(lake_depth = ifelse(lake_depth == 0, as.numeric(NA), lake_depth)) %>% 
    left_join(ungroup(readRDS(lake_clarity_file)), by = "site_id") %>% 
    # ADD LSTM-specific metadata (it comes with `n_obs`)
    left_join(read_csv(lstm_metadata_file), by = c("site_id", "GNIS_Name")) %>% 
    mutate(n_obs = replace_na(n_obs, 0)) %>% 
    # Remove sites that don't have any LSTM predictions (those are outside of our scope)
    filter(lstm_predictions) %>% 
    # Add three additional columns indicating whether a particular model is available for that site
    mutate(model_preds_ealstm_nldas = TRUE, # All sites here have EA-LSTM available (we used that to filter)
           model_preds_glm_nldas = site_id %in% glm_nldas_sites,
           model_preds_glm_gcm = site_id %in% glm_gcm_sites) %>% 
    # Add driver data mapping info:
    left_join(nldas_driver_info, by = "site_id") %>% 
    mutate(meteo_zip = basename(sprintf(nldas_zipfile_pattern, meteo_grp))) %>%
    left_join(gcm_driver_info, by = "site_id") %>% 
    # Apply some simplifying for significant figures
    mutate(Kw = signif(Kw, 4),
           lake_depth = signif(lake_depth, 3)) %>% 
    # Rename and organize final columns
    select(site_id, 
           lake_name = GNIS_Name,
           state,
           centroid_lon = X,
           centroid_lat = Y,
           max_depth = lake_depth,
           area,
           elevation,
           clarity = Kw,
           num_observ = n_obs,
           driver_nldas_zipfile = meteo_zip,
           driver_nldas_filepath = meteo_fl,
           driver_gcm_cell_no = cell_no,
           model_preds_ealstm_nldas,
           model_preds_glm_nldas,
           model_preds_glm_gcm,
           ealstm_group = lstm_group)
  
  write_csv(lake_metadata, out_file)
}

prep_lake_id_crosswalk <- function(out_file, all_crosswalk_files, lakes_in_release, repo_path = '../../lake-temp/lake-temperature-model-prep/') {
  
  # Get all crosswalk RDS files locally (takes a long time if you don't have them):
  crosswalk_inds <- all_crosswalk_files[grepl('.ind', all_crosswalk_files)]
  scipiper_freshen_files(ind_files = crosswalk_inds, repo_path = repo_path)
  
  # Now that all inds have a corresponding data file, we can reset the list 
  # of crosswalk RDS files then filter to just the crosswalks we want to use
  crosswalk_files <- gsub('.ind', '', crosswalk_inds)
  crosswalk_files <- crosswalk_files[!grepl('gnis|Iowa|isro|lake_to_state|navico|norfork|univ_mo|wqp', crosswalk_files)]
  
  # Load in the crosswalks
  crosswalks_raw <-purrr::map(crosswalk_files, readRDS) %>% setNames(basename(crosswalk_files))
  
  # Select only the site_id and xwalk id columns + harmonize some of the xwalks naming patterns
  crosswalks <- purrr::map2(crosswalks_raw, names(crosswalks_raw), function(xwalk, xwalk_nm) {
    
    # Special fixes to harmonize crosswalk IDs to follow our patterns
    if(grepl('micorps', xwalk_nm, ignore.case=T)) {
      names(xwalk) <- c('MICORPS_ID', 'site_id')
      xwalk[['MICORPS_ID']] <- sprintf('MICORPS_%s', xwalk[['MICORPS_ID']])
    }
    
    if(grepl('winslow', xwalk_nm, ignore.case=T)) {
      xwalk[['WINSLOW_ID']] <- as.character(xwalk[['WINSLOW_ID']])
    }
    
    # Edit lagos xwalk_nm before it gets used to match the id_col (need to change lagosus to lagos)
    if(grepl('lagosus', xwalk_nm)) {
      xwalk_nm <- 'lagos_nhdhr_xwalk.rds'
    }
    
    # Select only the organization ID and site_id columns
    id_col <- names(xwalk)[grepl(gsub('_nhdhr_xwalk.rds', '', xwalk_nm), names(xwalk), ignore.case = TRUE)]
    xwalk_updated <- xwalk[,c('site_id', id_col)]
    
    # Capitalize some of the actual site id prefixes that are not currently capitalized
    need_capitalization_regex <- 'iadnr|lagos|mndow|mo_usace|ndgf'
    if(grepl(need_capitalization_regex, id_col, ignore.case=T)) {
      # All column names should be capitalized, except `site_id`
      cap_id_col <- toupper(id_col)
      names(xwalk_updated) <- c('site_id', cap_id_col)
      
      # Now replace prefixes with uppercase versions
      xwalk_updated[[cap_id_col]] <- gsub(need_capitalization_regex, gsub("_ID", "", cap_id_col), xwalk_updated[[cap_id_col]])
    }
    
    return(xwalk_updated)
  })
  
  # Join them all together into a single crosswalk
  crosswalks_all <- purrr::reduce(crosswalks, full_join, by="site_id") %>% 
    select(site_id, order(colnames(.))) %>% 
    # Filter to site_ids that appear in our models but use `right_join()` to
    # keep the modeled sites that don't appear in any of the crosswalks
    right_join(tibble(site_id = lakes_in_release), by = "site_id")
  
  write_csv(crosswalks_all, out_file)
  
}

prep_lake_hypsography <- function(out_file, data_file, lakes_in_release, repo_path = '../../lake-temp/lake-temperature-model-prep/') {
  scipiper_freshen_files(data_files = data_file, repo_path = repo_path)
  
  # Load the full list of hypsography available and then  
  # filter out lakes that are not included in this release
  H_A_list <- readRDS(data_file)
  H_A_list_in_release <- H_A_list[names(H_A_list) %in% lakes_in_release]
  
  hypso_list <- purrr::map(H_A_list_in_release, function(H_A_df) {
    H_A_df %>% 
      mutate(depths = max(H) - H, areas = A) %>% 
      arrange(depths) %>% 
      select(depths, areas)
  })
  hypso_df <- purrr::map_df(hypso_list, ~as.data.frame(.x), .id="site_id")
  
  # Prevent lakes from being included which only have a surface area (I counted 4 when I did this in Feb 2023)
  hypso_df_valid <- hypso_df %>% 
    group_by(site_id) %>% 
    mutate(site_n = n()) %>% 
    ungroup () %>% 
    filter(site_n > 1)
  
  write_csv(hypso_df_valid, out_file)
}

prep_lake_temp_obs <- function(out_file, data_file, lakes_in_release, earliest_prediction, repo_path = '../../lake-temp/lake-temperature-model-prep/') {
  scipiper_freshen_files(data_files = data_file, repo_path = repo_path)
  
  # Load temperature observations and filter to the appropriate lakes and dates
  temp_obs_all <- arrow::read_feather(data_file) %>% 
    filter(date >= as.Date(earliest_prediction)) %>% # Filter to the earliest date available
    filter(site_id %in% lakes_in_release) # Filter to only sites included in the data release
  
  # Save the CSV in a temporary location
  obs_csv <- file.path('tmp_data', 'lake_temperature_observations.csv')
  write_csv(temp_obs_all, obs_csv)
  
  # Compress the CSV into a single zip file in this directory
  zip::zip(out_file, files = obs_csv)
}

# Zip up NLDAS csvs into multiple zips using the `meteo_grp` and `meteo_fl` columns created 
# in `prep_nldas_driver_info()`. Returns a vector of zip filepaths.
prep_NLDAS_drivers <- function(ind_file, nldas_driver_info, driver_file_dir, tmp_dir, zip_fn_pattern) {
  # The NLDAS drivers are coming from a targets repo, not scipiper so no need for `scipiper_freshen_files()`
  # This will need to be run on Tallgrass in order to have the most up-to-date data, though.
  nldas_driver_info_cp <- nldas_driver_info %>%
    # Only get unique meteo files
    select(-site_id) %>%
    distinct() %>%
    # Add in the full filepath + create a filepath for the new location in this directory
    mutate(meteo_fl_full = file.path(driver_file_dir, meteo_fl),
           meteo_fl_cp = file.path(tmp_dir, meteo_fl))
  
  # Before zipping, move the files to the current directory (I got scared by a warning when I was testing
  # this on Tallgrass that said `Some paths reference parent directory, creating non-portable zip file`,
  # so I created this solution, which definitely costs more time but gets rid of that warning).
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  file.copy(from = unique(nldas_driver_info_cp$meteo_fl_full), to = unique(nldas_driver_info_cp$meteo_fl_cp))
  
  # Zip the files!
  zip_files <- function(nldas_info_grp, zip_fn_pattern) {
    zip_fn <- sprintf(zip_fn_pattern, unique(nldas_info_grp$meteo_grp))
    files_to_zip <- nldas_info_grp %>% pull(meteo_fl_cp)
    message(sprintf('Zipping %s files into %s', length(files_to_zip), zip_fn))
    zip::zip(zip_fn, files = files_to_zip)
    return(zip_fn)
  }
  
  # Change the working directory and zip_fn_pattern appropriately
  # so that we avoid folder structures within the zips.
  cwd <- getwd()
  setwd(tmp_dir)
  tmp_dir_num <- length(unlist(strsplit(tmp_dir, split='/'))) # Count number of dirs to know how many ../ to add
  zip_fn_pattern <- file.path(paste(rep('..',tmp_dir_num), collapse = '/'), zip_fn_pattern)
  
  zips_out <- nldas_driver_info_cp %>%
    mutate(meteo_fl_cp = basename(meteo_fl_cp)) %>% # Update the names to not include the dir
    split(.$meteo_grp) %>% 
    purrr::map(~zip_files(., zip_fn_pattern = zip_fn_pattern)) %>%
    reduce(c)
  
  setwd(cwd) # Reset the working directory
  
  # Combine the files that were created into a single ind file
  combine_to_ind(ind_file, zips_out)

  return(ind_file)
}

# Zip up GCM NetCDFs
prep_GCM_drivers <- function(out_file, driver_file_dir, tmp_dir, gcm_driver_regex) {
  # The GCM drivers are coming from a targets repo, not scipiper so no need for `scipiper_freshen_files()`
  # This will need to be run on Tallgrass in order to have the most up-to-date data, though.
  files_to_zip <- list.files(driver_file_dir, pattern = gcm_driver_regex, full.names = TRUE)
  
  # Before zipping, move the files to the current directory (I got scared by a warning when I was testing
  # this on Tallgrass that said `Some paths reference parent directory, creating non-portable zip file`,
  # so I created this solution, which definitely costs more time but gets rid of that warning).
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  files_moved <- file.path(tmp_dir, basename(files_to_zip))
  file.copy(from = files_to_zip, to = files_moved)
  
  # Zip the files!
  zip::zip(out_file, files = files_moved)
  
  # Delete the recently moved files since they are now in a zip file
  file.remove(files_moved) 
}
