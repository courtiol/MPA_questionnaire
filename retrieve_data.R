library(formr)
library(tidyverse)

formr_connect(
  host = "https://workforce-admin.marine-conservation.org"
)

res <- formr_results(survey_name = "MPA_workforce_survey3",
                     host = "https://workforce-admin.marine-conservation.org")

#saveRDS(res, file = "result_backup20250413.RDS")

res |> 
  select(where(fn = ~ !all(is.na(.x)))) |>
  filter(!is.na(country), !missing_MPA %in% c("test", "TEST"), !is.na(total_validate))

res |> 
  select(where(fn = ~ !all(is.na(.x)))) |>
  filter(country == "Australia (AUS)") |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(everything()) |>
  filter(!is.na(value)) |>
  as.data.frame()
