## Note: this script is used to prepare the choices sheet used in formr
## it follows a better approach than download_old.R
## It is based on the overall WDPA dump, which prevents coping with downloading issues
## and which implies all is based on a single file, improving reproducibility.

rm(list = ls())
library(wdpar) #contains codelist
library(dplyr)
library(ggplot2)
library(rnaturalearth)
data("codelist", package = "countrycode")

#setwd("../")
cleanMPA_tbl <- readRDS("Robj/cleanMPA_tbl.RDS") ## created in download_WDPA.R


# Format data -------------------------------------------------------------

cleanMPA_tbl[, c("WDPA_PID", "NAME", "ORIG_NAME", "ISO3", "IUCN_CAT", "MARINE")] |>
  filter(nchar(ISO3) == 3) |> ## remove multiple country MPA
  rename(id = "WDPA_PID", country_code = "ISO3", name_en = "NAME", name_ori = "ORIG_NAME", iucn = "IUCN_CAT", marine = "MARINE") |>
  mutate(country_name = codelist[match(country_code, codelist$iso3c), "country.name.en"][[1]]) |>
  select(country_name, country_code, id, name_en, name_ori, iucn, geometry, marine) -> MPA_tbl


nrow(MPA_tbl) # 17052

# Inspect data ------------------------------------------------------------

## there are name duplicates
foo <- table(MPA_tbl$name_en)
foo[foo > 1]

## example of duplicate

quick_plot <- function(location, scale = "medium", regex = FALSE) {
  if (!regex) example <- MPA_tbl[MPA_tbl$name_en %in% location, ]
  if (regex) example <- MPA_tbl[grep(MPA_tbl$name_en, pattern = paste(location), perl = TRUE), ]
  example <- sf::st_transform(example, "EPSG:4326")
  
  country <- ne_countries(country = unique(example$country_name), scale = scale, returnclass = "sf")
  country <- sf::st_transform(country, "EPSG:4326")
  
  ggplot() +
    geom_sf(data = country) +
    geom_sf(aes(fill = paste(name_en, id)), alpha = 0.5, data = example) +
    coord_sf(xlim = range(sf::st_coordinates(example$geometry)[, "X"]),
             ylim = range(sf::st_coordinates(example$geometry)[, "Y"])) +
    theme_bw() +
    labs(fill = "PA")
}

quick_plot(location = "Baie Ternay|Port Launay", regex = TRUE, scale = "large")
quick_plot(location = "Hol Chan", scale = "large")

# Export for formr --------------------------------------------------------

## Countries
countries <- data.frame(list_name = "countries",
                        code = codelist$iso3c, 
                        label = paste0(codelist$country.name.en, " (", codelist$iso3c, ")"))
countries |>
  filter(!is.na(code)) |>
  arrange(label) |>
  select(-code) -> countries

rbind(countries[countries$label == "Åland Islands (ALA)", ],
      countries[countries$label != "Åland Islands (ALA)", ]) -> countries

countries |>
  mutate(label = stringr::str_replace_all(label, "&", "+")) -> countries

rownames(countries) <- NULL
countries$name <- seq_len(nrow(countries))

nrow(countries) #249

length(setdiff(stringr::str_replace_all(paste0(MPA_tbl$country_name, " (", MPA_tbl$country_code, ")"), "&", "+"), countries$label))
# result should be 0, otherwise missing countries

## MPA
MPA_tbl |>
  mutate(id = stringr::str_remove_all(id, "[:alpha:]")) |>
  mutate(id = stringr::str_remove_all(id, "_")) |>
  mutate(id = stringr::str_remove_all(id, "\r")) |>
  mutate(id = stringr::str_remove_all(id, "\n")) |>
  mutate(id = stringr::str_remove_all(id, " ")) |>
  mutate(replicate = n(), .by = "id") -> MPA_tbl2

MPA_tbl2 |>
  filter(replicate == 1) -> MPA_tbl_notreplicated

MPA_tbl2 |>
  filter(replicate > 1) -> MPA_tbl_replicated

MPA_tbl_replicated |>
  summarise(geometry = sf::st_union(geometry), .by = setdiff(colnames(MPA_tbl_replicated), c("geometry", "marine", "iucn"))) |>
  mutate(replicate = n(), .by = "id") -> MPA_tbl_replicated_merging1

MPA_tbl_replicated_merging1 |>
  filter(replicate == 1) -> MPA_tbl_merged_OK

MPA_tbl_replicated_merging1 |>
  filter(replicate > 1) -> MPA_tbl_merged_notOK

ids_trouble <- sort(unique(MPA_tbl_merged_notOK$id)) # trouble making ID (have alternative names)
ids_trouble
# [1] "555547861" "555547862" "555547863" "555547864" "555559201" "555624907" "555691674" "555691675" "555744923" "555744966"
# [11] "555781368" "555785614"

## For discrepant names among subunits, I use a root name, unlike WDPA which uses a random one it seems:

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547861", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547861", "name_en"][[1]])
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547861", "name_ori"] <- "Nukufetau"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547861", "name_en"] <- "Nukufetau"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547862", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547862", "name_en"][[1]])
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547862", "name_ori"] <- "Nukulaelae"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547862", "name_en"] <- "Nukulaelae" ## this seems terrestrial...

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547863", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547863", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547863", "name_ori"] <- "Nanumea"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547863", "name_en"] <- "Nanumea"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547864", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547864", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547864", "name_ori"] <- "Nui"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555547864", "name_en"] <- "Nui"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555559201", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555559201", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555559201", "name_ori"] <- "Isles of Scilly Sites"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555559201", "name_en"] <- "Isles of Scilly Sites"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555624907", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555624907", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555624907", "name_ori"] <- "Marae Moana"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555624907", "name_en"] <- "Marae Moana"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555691674", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555691674", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555691674", "name_ori"] <- "Cumbria Coast (Zone 1+2)"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555691674", "name_en"] <- "Cumbria Coast (Zone 1+2)"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555691675", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555691675", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555691675", "name_ori"] <- "Medway Estuary (Zone 1+2)"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555691675", "name_en"] <- "Medway Estuary (Zone 1+2)"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555744923", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555744923", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555744923", "name_ori"] <- "الهيرات الشمالية و الحزام الحاجز"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555744923", "name_en"] <- "Northern Hayrat and Buffer Zone"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555744966", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555744966", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555744966", "name_ori"] <- "Moheli"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555744966", "name_en"] <- "Moheli"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555781368", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555781368", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555781368", "name_ori"] <- "Mar Tropical de Grau"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555781368", "name_en"] <- "Mar Tropical de Grau"

MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555785614", ]
quick_plot(MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555785614", "name_en"][[1]], scale = "large")
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555785614", "name_ori"] <- "Nanaimo River Estuary"
MPA_tbl_merged_notOK[MPA_tbl_merged_notOK$id == "555785614", "name_en"] <- "Nanaimo River Estuary"

MPA_tbl_merged_notOK |>
  summarise(geometry = sf::st_union(geometry), .by = setdiff(colnames(MPA_tbl_replicated), c("geometry", "marine", "iucn"))) |>
  mutate(replicate = n(), .by = "id") -> MPA_tbl_replicated_merging2

MPA_tbl_clean <- bind_rows(MPA_tbl_notreplicated, MPA_tbl_merged_OK, MPA_tbl_replicated_merging2)
table(MPA_tbl_clean$replicate) # should all be 1

MPA_tbl_clean$replicate <- NULL
nrow(MPA_tbl_clean) # 16777 number of MPA with distinct ID (once no letter appended)

#saveRDS(MPA_tbl_clean, file = "Robj/MPA_tbl_clean.RDS")

data.frame(list_name = paste0("MPA_", MPA_tbl_clean$country_code),
           label_name_ori = MPA_tbl_clean$name_ori,
           label_name_en = MPA_tbl_clean$name_en,
           label_id = MPA_tbl_clean$id) |>
  mutate(label = if_else(label_name_ori == label_name_en,
                         paste0(label_name_en, " - ", label_id),
                         paste0(label_name_ori, " = ", label_name_en, " - ", label_id)), .by = "label_id") |> ## combine original and english names if needed (ori first, otherwise formatting problematic in arabic)
  select(-label_name_ori, -label_name_en, -label_id) -> MPAs


filler_noMPAs <- function(countrycode) {
  data.frame(list_name = paste0("MPA_", countrycode),
             label = "\U0020. MPA not listed") ## prefix with first possible unicode to be ranked first
             #label = "\UFFFF. MPA not listed") ## prefix with last possible unicode to be ranked last
}

noMPAS <- do.call("rbind", lapply(na.omit(codelist$iso3c), filler_noMPAs))

bind_rows(MPAs, noMPAS) |>
  arrange(list_name, label) |>
  mutate(name = seq_len(n()), .by = list_name) |>
  mutate(label = stringr::str_replace_all(label, "`", "'"),
         label = stringr::str_replace_all(label, "–", "-"),
         label = stringr::str_replace_all(label, "&", "+"),
         label = stringr::str_replace_all(label, ",", " "),
         label = stringr::str_replace_all(label, "  ", " "),
         label = stringr::str_remove_all(label, "\r"), 
         label = stringr::str_remove_all(label, "\n"),
         label = ifelse(label == "\U0020. MPA not listed", "MPA not listed", label)) -> MPAs_all

## check order
MPAs_all |>
  slice(1, .by = list_name) |>
  filter(label != "MPA not listed") # output should be empty df

## Combine and export
bind_rows(countries, MPAs_all) |>
  select(list_name, name, label) -> choices_tbl

nrow(choices_tbl)
head(choices_tbl)
tail(choices_tbl)

if (!dir.exists("cleandata")) dir.create("cleandata")
write.csv(choices_tbl, file = "cleandata/choices.csv", row.names = FALSE)

## export for full list (no geometry)
#d <- left_join(sf::st_drop_geometry(MPA_tbl_clean), sf::st_drop_geometry(cleanMPA_tbl), by = c("id" = "WDPA_PID"))
#d
#googlesheets4::gs4_auth(email = "alexandre.courtiol@gmail.com")
#googlesheets4::gs4_create(name = "MPA_list_used", sheets = list(MPAs = d)) ## if does not exist

