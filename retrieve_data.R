library(formr)

formr_connect(
  host = "https://workforce-admin.marine-conservation.org"
)

res <- formr_results(survey_name = "MPA_workforce_survey",
                     host = "https://workforce-admin.marine-conservation.org")

res
