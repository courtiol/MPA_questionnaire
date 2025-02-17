# Upload complex survey -------------------------------------------------
library(dplyr)

## Decide to use a short version for testing things more quickly
shorten_for_testing <- TRUE

## Note: the name of the google sheet must be the same as that of the survey
survey_tbl <- read.csv("cleandata/survey.csv")
choices_tbl <- read.csv("cleandata/choices.csv")

if (shorten_for_testing) {
  choices_tbl |>
    filter((list_name == "countries" & label == "Canada (CAN)") |
           (list_name == "MPA_CAN")) -> choices_tbl_short
  
  survey_tbl |> 
    filter(grepl(x = type, pattern = "^(?!.*MPA_(?!CAN).*).*$|MPA_CAN", perl = TRUE)) -> survey_tbl_short
}

#googlesheets4::gs4_auth(email = "alexandre.courtiol@gmail.com")
#googlesheets4::gs4_create(name = "test_MPA", sheets = list(survey = survey_tbl, choices = choices_tbl)) ## if does not exist
#googlesheets4::gs4_create(name = "test_MPA_short", sheets = list(survey = survey_tbl_short, choices = choices_tbl_short)) ## if does not exist

possible_sheets <- googlesheets4::gs4_find("test_MPA")
sheet_id <- possible_sheets[possible_sheets$name == "test_MPA", "id"][[1]]
sheet_id
sheet_id_short <- possible_sheets[possible_sheets$name == "test_MPA_short", "id"][[1]]
sheet_id_short

if (shorten_for_testing) {
  googlesheets4::sheet_write(choices_tbl_short, ss = sheet_id_short, sheet = "choices")
  googlesheets4::sheet_write(survey_tbl_short, ss = sheet_id_short, sheet = "survey")
} else {
  googlesheets4::sheet_write(choices_tbl, ss = sheet_id, sheet = "choices")
  googlesheets4::sheet_write(survey_tbl, ss = sheet_id, sheet = "survey")
}
