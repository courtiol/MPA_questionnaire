library(tidyverse)
library(sf)
data("codelist", package = "countrycode")

## import countries
countries <- data.frame(ISO3 = codelist$iso3c,
                        country_bare = codelist$country.name.en,
                        country = paste0(codelist$country.name.en, " (", codelist$iso3c, ")"))

## load and prepare the data of all MPAs
cleanMPA_tbl <- readRDS("Robj/cleanMPA_tbl.RDS") 

cleanMPA_tbl |>
  select(ISO3, WDPAID, AREA_KM2) |>
  mutate(WDPAID = as.character(WDPAID)) |>
  st_drop_geometry() |>
  mutate(country_name = codelist[match(ISO3, codelist$iso3c), "country.name.en"][[1]]) |>
  left_join(countries) |>
  relocate(country, 1) -> all_MPAs

all_MPAs |> ## first aggregation some WDPAID are split into multiple WDPA_ID
  summarise(AREA_KM2 = sum(AREA_KM2), .by = c("country", "country_name", "ISO3", "WDPAID")) -> all_MPAs_unique

all_MPAs_unique |> 
  summarise(N_MPA = n(), Total_area = sum(AREA_KM2), .by = c("country", "country_name", "ISO3")) -> all_aggregate_country

df_clean |> ## created in retrieve_data.R
  select(country, MPA) |>
  tidyr::separate_longer_delim(MPA, "-") |>
  filter(MPA != "") |>
  left_join(all_MPAs_unique, by = c("country" = "country", "MPA" = "WDPAID")) |>
  distinct() -> done_MPAs_unique

done_MPAs_unique |> 
  summarise(N_MPA_done = n(), Total_area_done = sum(AREA_KM2), .by = c("country", "ISO3")) -> done_aggregate_country

all_aggregate_country |>
  full_join(done_aggregate_country) |>
  replace_na(list(N_MPA_done = 0, Total_area_done = 0)) |>
  mutate(percent_MPA_done = 100 * N_MPA_done / N_MPA, percent_area_done = 100 * Total_area_done / Total_area) |>
  select(country_name, N_MPA:last_col()) |>
  arrange(desc(percent_MPA_done))

