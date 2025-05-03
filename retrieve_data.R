## I have set this up as a CRON job to run hourly:
## run crontab -e and paste the following:
## 0 * * * * Rscript [path_to_current_file]
## Check output using 'sudo cat /var/log/cron' (on Fedora)

if (!interactive()) {
  setwd("~/Dropbox/Boulot/Mes_projets_de_recherche/R_packages/MPA_project/MPA_questionnaire_Rscripts_GitHub") ## for CRON job
}

library(formr) ## remotes::install_github("rubenarslan/formr")
suppressMessages(library(tidyverse, warn.conflicts = FALSE))

source("secret.R") ## load credentials

formr_connect(
  host = "https://workforce-admin.marine-conservation.org",
  email = secret$email,
  password = secret$password
)

res <- formr_results(survey_name = "MPA_workforce_survey3",
                     host = "https://workforce-admin.marine-conservation.org")

#saveRDS(res, file = "responses/result_backup20250413.RDS") ## backup of pre-launch data collection

hash <- digest::digest(res)
if (file.exists("responses/lastresult_hash.txt")) {
  previous_hash <- read_file("responses/lastresult_hash.txt")
} else {
  previous_hash <- "nohash"
}

## do stuff only if the data have changed
if (hash != previous_hash) {
  write_file(hash, "responses/lastresult_hash.txt")
  
  nrow(res)
  sum(!is.na(res$ended))
  sum(is.na(res$country))
  
  res |> 
    select(where(fn = ~ !all(is.na(.x)))) |>
    filter(!is.na(country), !missing_MPA %in% c("test", "TEST", "Test"),
           !anythingelse %in% c("TEST"), !is.na(total_validate)) |>
    mutate(MPA = lay::lay(pick(starts_with("MPA")), \(x) x[!is.na(x)]),
           MPA = stringr::str_remove_all(MPA, "[:alpha:]|[:punct:]|="),
           MPA = stringr::str_replace_all(MPA, "[:blank:]+", "-"),
           MPA = stringr::str_remove(MPA, ".")) |>
    select(-starts_with("MPA_")) |>
    relocate(MPA, .after = country) -> df_clean
  
  writexl::write_xlsx(df_clean, path = paste0("./responses/clean_results_", Sys.time(), ".xlsx"))
    
  df_clean |>
    select(country, total_note, total_validate, starts_with("MPA"), email) |>
    mutate(total_validate = case_match(total_validate, 1 ~ "valid",
                                                       2 ~ "estimate",
                                                       3 ~ "guesstimate"),
           email = !is.na(email)) -> df_clean_short
  
  
  ## send the data by email using s-nail in bash
  ## I create a script called prepare_zip.sh and one called send_data_email.sh as follows:
  
  ## prepare_zip.sh
  ## #!/bin/bash
  ## if test -f responses/lastdata.zip; then
  ## rm responses/lastdata.zip
  ## fi
  ## filexslx=
  ## filexslx=$(ls responses/*.xlsx -Art | tail -n 1)
  ## echo $filexslx
  ## zip -e --password=[password] responses/lastdata.zip "$filexslx"
  
  ## send_data_email.sh
  ## #!/bin/bash
  ## echo 'Here is the latest version of the data' | mailx --subject="MPA update" -a responses/lastdata.zip [email]
  
  if (file.exists("prepare_zip.sh")) system("sh prepare_zip.sh")
  if (file.exists("send_data_email.sh")) system("sh send_data_email.sh")
} else {
  cat("Nothing to be done (data have not changed)\n")
}
