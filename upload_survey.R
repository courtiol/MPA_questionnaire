# Upload complex survey -------------------------------------------------

## Note: the name of the google sheet must be the same as that of the survey
survey_tbl <- read.csv("cleandata/survey.csv")
choices_tbl <- read.csv("cleandata/choices.csv")

#googlesheets4::gs4_auth(email = "alexandre.courtiol@gmail.com")
#googlesheets4::gs4_create(name = "test_MPA", sheets = list(survey = survey_tbl, choices = choices_tbl)) ## if does not exist

possible_sheets <- googlesheets4::gs4_find("test_MPA")
sheet_id <- possible_sheets[possible_sheets$name == "test_MPA", "id"][[1]]
sheet_id

googlesheets4::sheet_write(choices_tbl, ss = sheet_id, sheet = "choices")

googlesheets4::sheet_write(survey_tbl, ss = sheet_id, sheet = "survey")