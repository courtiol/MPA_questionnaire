library(formr)
library(tidyverse)

formr_connect(
  host = "https://workforce-admin.marine-conservation.org"
)

res <- formr_results(survey_name = "MPA_workforce_survey2",
                     host = "https://workforce-admin.marine-conservation.org")

#saveRDS(res, file = "result_backup20250413.RDS")

res |> 
  select(where(fn = ~ !all(is.na(.x)))) |>
  filter(!is.na(country) & (is.na(missing_MPA) | missing_MPA != "TEST"))

res |> 
  select(where(fn = ~ !all(is.na(.x)))) |>
  filter(country == "Australia (AUS)") |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(everything()) |>
  filter(!is.na(value)) |>
  as.data.frame()
