library(formr)
library(tidyverse)

formr_connect(
  host = "https://workforce-admin.marine-conservation.org"
)

res <- formr_results(survey_name = "MPA_workforce_survey2",
                     host = "https://workforce-admin.marine-conservation.org")

res |> 
  select(where(fn = ~ !all(is.na(.x))))
