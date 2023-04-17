
# Make sure that the data file from `lake-temperature-model-prep` is downloaded and up-to-date
scipiper_freshen_files <- function(ind_files = NULL, data_files = NULL, repo_path = '../../lake-temp/lake-temperature-model-prep/') {
  message(sprintf('Temporarily setting working directory to > %s', repo_path))
  cwd <- getwd()
  on.exit(setwd(cwd)) # Reset working directory even if this function fails!
  setwd(repo_path)
  
  # Prep files
  if(is.null(ind_files)) ind_files <- sprintf('%s.ind', data_files) 
  stopifnot(all(grepl(repo_path, ind_files))) # Stop here if we don't have the correct repo for the files
  ind_files <- gsub(repo_path, '', ind_files) # Drop repo path since we switched working dirs
  
  message('Freshening...')
  purrr::map(ind_files, function(ind_fn) {
    message(sprintf('    %s', ind_fn))
    scipiper::sc_retrieve(ind_fn)
  })
  message('All have been freshened.')
  
  message(sprintf('Resetting working directory to > %s', cwd))
  setwd(cwd) # Reset working directory
  return(ind_files)
} 

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  
  setwd(dsn)
  zip::zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

# Copy files but ensure they are the most up-to-date versions
scipiper_copy <- function(out_file, data_file, repo_path = '../../lake-temp/lake-temperature-model-prep/') {
  scipiper_freshen_files(data_file, repo_path)
  file.copy(from = data_file, to = out_file, overwrite = TRUE)
}

csv_to_feather <- function(out_feather, in_csv) {
  readr::read_csv(in_csv) %>% 
    arrow::write_feather(out_feather)
}
