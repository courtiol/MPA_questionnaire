## This script is the one retrieving the data from WDPA
## It superseeds download_old.R
## It used to be part of prepare_choices.R and was run from there but I then divided up the files

rm(list = ls())
library(wdpar) #contains codelist

update <- FALSE ## to avoid accidental new download

if (update) {

  ## A little extra coding is needed to store the file as under its original name
  ## defined by WDPA.
  
  address_global_dump <- "https://wcmc.io/wdpa_current_release"
  hdr <- httr::HEAD(address_global_dump) ## retrieve headers from website
  path_bits <- strsplit(hdr$url, "/")[[1]]
  filename <- path_bits[length(path_bits)]
  if (!dir.exists("rawdata")) dir.create("rawdata")
  opt <- options(timeout = 2000) ## delay timeout for download
  download.file(address_global_dump, destfile = paste0("rawdata/", filename))
  options(opt) ## restore R original settings
  
  ## Retrieving data from ZIP file
  filename <- dir("rawdata/", pattern = "*.zip")
  if (length(filename) > 1) stop("More than one ZIP file in folder, select the one you want by hand")
  rawdata_tbl <- wdpa_read(paste0("rawdata/", filename))
  nrow(rawdata_tbl) ## 311679
  
  rawMPA_tbl <- rawdata_tbl[rawdata_tbl$MARINE != 0, ] ## keep marine or mixed area only
  nrow(rawMPA_tbl) ## 17145
  
  cleanMPA_tbl <- wdpa_clean(rawMPA_tbl,
                             retain_status = NULL, exclude_unesco = FALSE, erase_overlaps = FALSE,
                             geometry_precision = 10000)
  nrow(cleanMPA_tbl) ## 17074
  #saveRDS(cleanMPA_tbl, file = "Robj/cleanMPA_tbl.RDS", compress = FALSE)

}