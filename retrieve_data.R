## I have set this up as a CRON job to run hourly:
## run crontab -e and paste the following:
## 0 * * * * podman exec -ti emailMPA Rscript [path_to_current_file] ## one server
## 0 * * * * Rscript [path_to_current_file] ## one local computer
## Check output using 'sudo cat /var/log/cron' (on Fedora)

source("secret.R") ## load credentials

setwd("./responses") ## hard coding path is useful for running this from bash (which CRON does)

library(formr) ## remotes::install_github("rubenarslan/formr")
suppressMessages(library(tidyverse, warn.conflicts = FALSE))

formr_connect(
  host = "https://workforce-admin.marine-conservation.org",
  email = secret$formr_login,
  password = secret$formr_password
)

res <- formr_results(survey_name = "MPA_workforce_survey3",
                     host = "https://workforce-admin.marine-conservation.org")

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

nrow(df_clean)

hash <- digest::digest(df_clean)

if (file.exists("lastresult_hash.txt")) {
  previous_hash <- read_file("lastresult_hash.txt")
} else {
  previous_hash <- "nohash"
}
  
## do stuff only if the data have changed
if (hash != previous_hash) {
  write_file(hash, "lastresult_hash.txt")

  last_file <- paste0("clean_results_", Sys.time(), ".xlsx")
  writexl::write_xlsx(df_clean, path = last_file)
  
  zip(zipfile = "lastdata", 
      files = last_file, 
      flags = paste("-e --password", secret$zip_password))
  
  mailR::send.mail(from = secret$email_sender,
                   to = secret$email_recipient,
                   subject = "Latest data",
                   body = paste0("We now have ", nrow(df_clean), " rows. The last location we got data for is ", rev(as.character(df_clean$country))[1], ". Please find enclosed the latest data."),
                   attach.files = "lastdata.zip",
                   smtp = list(host.name = "smtp.gmail.com",
                               port = 587,
                               user.name = secret$email_sender,
                               passwd = secret$email_token, ## token set here: https://myaccount.google.com/apppasswords and call the app custom (otherwise may not work)
                               ssl = TRUE),
                   authenticate = TRUE,
                   send = TRUE)
  
  file.remove(last_file)
  file.remove("lastdata.zip")
  
} else {
  cat("Nothing to be done (data have not changed)\n")
}
