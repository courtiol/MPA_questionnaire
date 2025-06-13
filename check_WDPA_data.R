library(tidyverse)
library(sf)
library(rnaturalearth)

data("codelist", package = "countrycode")

## import countries
countries <- data.frame(ISO3 = codelist$iso3c,
                        country_bare = codelist$country.name.en,
                        country = paste0(codelist$country.name.en, " (", codelist$iso3c, ")"))

## define small function to only display some columns
simplify_cols <- function(x) {
  x |>
    mutate(country = countries[match(ISO3, countries$ISO3), ]) |> 
    sf::st_drop_geometry() |> 
    select(WDPAID, WDPA_PID, country, NAME, MARINE, DESIG_ENG, REP_AREA, REP_M_AREA, AREA_KM2)
}

## plotting function
plot_MPA <- function(data, split_by_MPA = FALSE, zoom = FALSE, crs = "EPSG:3978", resolution_country = "medium") {
  if (split_by_MPA) {
    for (i in seq_len(nrow(data))) {
      plot_MPA(data = data[i, ], split_by_MPA = FALSE, zoom = zoom)
      }
  } else {
    selected_countries <-  countries[match(unique(data$ISO3), countries$ISO3), "country_bare"]
    data_countries <- ne_countries(country = selected_countries, scale = resolution_country, returnclass = "sf")
    
    data_countries <- sf::st_transform(data_countries, crs = crs)
    data <- sf::st_transform(data, crs = crs)
    
    ggplot() +
      geom_sf(linewidth = 0, data = data_countries) +
      geom_sf(aes(fill = MARINE), linewidth = 0, alpha = 0.5, data = data) +
      theme_bw() -> plot
    
    if (length(data$WDPA_PID) == 1) {
      plot +
        labs(title = data$WDPA_PID) -> plot
    }
    
    if (zoom) {
      plot +
      coord_sf(xlim = range(sf::st_coordinates(data)[, "X"]),
               ylim = range(sf::st_coordinates(data)[, "Y"])) -> plot
    }

    print(plot)
  }
}

## load the data of all MPAs
cleanMPA_tbl <- readRDS("Robj/cleanMPA_tbl.RDS") 

## for partial marine protected areas, the total area is most often larger than the marine area (makes sense)
## here I highlight some key exceptions
ggplot(cleanMPA_tbl |> filter(REP_AREA > 0 & REP_M_AREA > 0)) +
  aes(y = REP_AREA, x  = REP_M_AREA, col = MARINE) +
  geom_point(alpha = 0.5) +
  geom_label(aes(label = WDPAID), hjust = -0.1, data = cleanMPA_tbl |> filter(REP_AREA > 0 & REP_M_AREA > 0 & REP_M_AREA > 2*REP_AREA),
             key_glyph = draw_key_point) +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  theme_classic()

### here are the outliers
cleanMPA_tbl |>
  filter(WDPAID %in% c(27, 4227, 902308)) |> 
  simplify_cols()

## Many MPAs have a reported marine area but no reported areas, and they are all in Great Britain or Indonesia
cleanMPA_tbl |>
  filter(REP_AREA == 0 & REP_M_AREA > 0) |>
  arrange(desc(REP_M_AREA)) |> 
  simplify_cols() |> 
  count(country)

cleanMPA_tbl |>
  filter(REP_AREA == 0 & REP_M_AREA > 0) |>
  ggplot(aes(x = REP_M_AREA, y = AREA_KM2, colour = MARINE)) +
  geom_label(aes(label = WDPAID), hjust = -0.1,
             data = cleanMPA_tbl |> filter(MARINE == "marine" & REP_AREA == 0 & REP_M_AREA > 0 & AREA_KM2 > 2*REP_M_AREA),
             key_glyph = draw_key_point) +
  geom_abline(yintercept = 0, slope = 1) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  coord_cartesian(xlim = c(NA, 200000)) +
  theme_classic()

### for most we should consider REP_AREA = AREA_KM2 and keep REP_M_AREA untouched, but here are 4 outliers
### all in Indonesia, they have larger AREA_KM2 than REP_M_AREA and yet are purely marine
cleanMPA_tbl |>
  filter(WDPAID %in% c(8071, 8121, 555587239, 555511989)) |> 
  simplify_cols()

plot_MPA(cleanMPA_tbl |> filter(WDPAID %in% c(8071, 8121, 555587239, 555511989)))
plot_MPA(cleanMPA_tbl |> filter(WDPAID %in% c(8071, 8121, 555587239, 555511989)),
         split_by_MPA = TRUE, zoom = TRUE, resolution_country = "large")

## Thousands of pure marine MPAs have a reported area but no reported marine area (We should assume REP_M_AREA = REP_AREA for those)
cleanMPA_tbl |>
  filter(REP_AREA > 0 & REP_M_AREA == 0 & MARINE == "marine") |>
  arrange(desc(REP_AREA)) |> 
  simplify_cols()

## Thousands of pure partially marine MPAs have a reported area but no reported marine area (Not sure what to do there)
cleanMPA_tbl |>
  filter(REP_AREA > 0 & REP_M_AREA == 0 & MARINE == "partial") |>
  arrange(desc(REP_AREA)) |> 
  simplify_cols()



######################## Below random WIP

plot(REP_AREA ~ AREA_KM2, data = cleanMPA_tbl)

cleanMPA_tbl |> 
  filter(AREA_KM2 == 0 & REP_AREA > 0)

## Many have an area but no reported area
cleanMPA_tbl |> 
  filter(AREA_KM2 > 0 & REP_AREA == 0) |> 
  select(WDPAID, WDPA_PID, NAME, AREA_KM2, REP_AREA) |> 
  arrange(desc(AREA_KM2))


i_area_issues <- with(cleanMPA_tbl, which((REP_AREA/AREA_KM2 > 2 | AREA_KM2/REP_AREA > 2)))
length(i_area_issues)/nrow(cleanMPA_tbl) ## proportion of area issues
cleanMPA_tbl[i_area_issues, ]

