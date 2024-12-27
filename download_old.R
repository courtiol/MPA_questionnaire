library(wdpar)
library(dplyr)

download_PA <- function(countries, wait = FALSE, page_wait = 1, ...) {
  if (!dir.exists("rawdata")) dir.create("rawdata")
  while (length(countries > 0)) {
    done <- dir("rawdata")
    countries <- countries[!countries %in% done]
    for (country in countries) {
      print(paste0("Download attempt for ",
                   codelist[!is.na(codelist$iso3c) & codelist$iso3c == country, "country.name.en"][[1]],
                  " (", country, ")"))
      tryCatch(
        wdpa_fetch(x = country,
                   wait = wait,
                   download_dir = paste0("rawdata/", country),
                   page_wait = page_wait, ...),
        error = function(e) e
      )
    }
  }
}

extract_missing <- function(countries, full_name = TRUE) {
  all_files <- list.files("rawdata", recursive = TRUE)
  message(paste(length(all_files), "countries already done"))
  not_done <- setdiff(countries, substr(all_files, 1, 3))
  message(paste(length(not_done), "more countries to do"))
  if (full_name) {
    not_done <- unlist(codelist[!is.na(codelist$iso3c) & codelist$iso3c %in% not_done, "country.name.en"])
    names(not_done) <- NULL
  }
  not_done
}

download_PA_retry <- function(countries, wait = FALSE, page_wait = 1, ...) {
  not_done <- extract_missing(countries, full_name = FALSE)
  unlink(paste0("rawdata/", not_done), recursive = TRUE)
  download_PA(not_done, wait = wait, page_wait = page_wait, ...)
}

## Attempt downloading PAs from all countries
## Note: data for each country are stored in a subfolder within rawdata
countries <- na.omit(codelist$iso3c)
download_PA(countries)

## additional attempts to download PAs from all missing countries
extract_missing(countries)
download_PA_retry(countries)
download_PA_retry(countries, wait = TRUE, page_wait = 5)

## Process data
missing_countries <- extract_missing(countries, full_name = FALSE)
kept_countries <- countries[!countries %in% missing_countries]

rawdata <- lapply(kept_countries, \(country) {
  print(country)
  wdpa_fetch(x = country, download_dir = paste0("rawdata/", country))
})

rawdata_tbl <- st_as_sf(as_tibble(bind_rows(rawdata)))
if (!dir.exists("Robj")) dir.create("Robj")
#saveRDS(rawdata_tbl, file = "Robj/rawdata_tbl.RDS", compress = FALSE)
rawdata_tbl <- readRDS("Robj/rawdata_tbl.RDS")

cleandata_tbl <- wdpa_clean(rawdata_tbl, exclude_unesco = FALSE, erase_overlaps = FALSE)
saveRDS(cleandata_tbl, file = "Robj/cleandata_tbl.RDS", compress = FALSE)

rawMPA_tbl <- rawdata_tbl[rawdata_tbl$MARINE != 0, ]
cleanMPA_10000_tbl <- wdpa_clean(rawMPA_tbl, retain_status = NULL, exclude_unesco = FALSE, erase_overlaps = FALSE,
                                 geometry_precision = 10000)
saveRDS(cleanMPA_10000_tbl, file = "Robj/cleanMPA_10000_tbl.RDS", compress = FALSE)

## View excluded MPA (all have virtually null areas)
View(rawMPA_tbl[rawMPA_tbl$WDPAID %in% setdiff(rawMPA_tbl$WDPAID, cleanMPA_10000_tbl$WDPAID), ])

cleanMPA_10000_tbl[, c("WDPA_PID", "NAME", "ORIG_NAME", "ISO3", "IUCN_CAT")] |>
  filter(nchar(ISO3) == 3) |> ## remove multiple country MPA
  rename(id = "WDPA_PID", country_code = "ISO3", name_en = "NAME", name_ori = "ORIG_NAME", iucn = "IUCN_CAT") |>
  mutate(country_name = codelist[match(MPA_tbl$country_code, codelist$iso3c), "country.name.en"][[1]]) |>
  select(country_name, country_code, id, name_en, name_ori, iucn, geometry) -> MPA_tbl

head(MPA_tbl)

## english name is not always the same as the original name
MPA_tbl |>
  filter(name_en != name_ori)

## there are no PID duplicate
foo <- table(MPA_tbl$id)
foo[foo > 1]

## there are name duplicates
foo <- table(MPA_tbl$name_en)
foo[foo > 1]

## example of duplicate
MPA_tbl[paste(MPA_tbl$name_en, MPA_tbl$iucn) %in% names(foo[foo > 1]), ][2, ]
plot(MPA_tbl[MPA_tbl$name_en == "Cape Palmerston - Rocky Dam", NULL], col = 1:2)

nrow(MPA_tbl)
if (!dir.exists("cleandata")) dir.create("cleandata")
write.csv(as.data.frame(MPA_tbl)[colnames(MPA_tbl) != "geometry"], file = "cleandata/list_MPA.csv", row.names = FALSE)
saveRDS(MPA_tbl, file = "cleandata/list_MPA.RDS")
