## Note: this script is used to prepare the survey sheet used in formr

## Buglist to maybe report on Github's formr org repo (from Leonie):
## - 1) The list_name in the choices sheet is filled in automatically when uploaded. BUT: the showif does not work if the list_name is not given explicitly for all items of this list. The example given does work strangely.
## - 2) maxType in select_or_add_one does not work with comma as documented
## - 3) select_one works with the number of the choices and stores it as "number". select_or_add_one instead works with the "choicename" itself. Not documented.

library(wdpar) #contains codelist
library(dplyr)

add_element <- function(label="unnamed", name="element", class="", type="", value="", optional="", showif = "", ...) {
  data.frame(label = label,
             name = name,
             class = class, 
             type = type,
             value = value,
             optional = optional,
             showif = showif,
             ...)
}

convert_country_label <- function(countrycode) {
  codelist <- codelist[!is.na(codelist$iso3c), ]
  countryname <- codelist$country.name.en[match(countrycode, codelist$iso3c)]
  label <- paste0(countryname, " (", countrycode, ")")
  stringr::str_replace_all(label, "&", "+")
} # convert_country_label(c("FRA", "CAN", "FRA", "ATG"))

add_MPA_country <- function(countrycode) {
  add_element(label = "#### Select the MPA(s) you are responding for
##### 💙 you can search using name, partial name, or WDPA PID.
##### 💡 the MPAs listed here are those present in WDPA for the country (or overseas land) you have selected. If you cannot find any of your MPAs, please select 'MPA not listed'.",
              name = paste0("MPA_", countrycode, "_mc"),
              class = "cant_add_choice",
              type = paste0("select_or_add_multiple MPA_", countrycode),
              showif = paste0("country == '", convert_country_label(countrycode), "'"))
}


# First page of questionnaire ---------------------------------------------

N1 <-  add_element(label = "## Step 1: select your country",
                   name = "N1",
                   type = "note")

Q1 <- add_element(label = "#### Select the country (or overseas land) with the MPA(s) you are responding for
##### 💙 you can search using name, partial name, or ISO code.
##### 💡some overseas lands or islands are listed under their own name, e.g. Guadeloupe is listed under 'Guadeloupe (GLP)' and not under 'France (FRA)'.",
                  name = "country",
                  class = "cant_add_choice", 
                  type = "select_or_add_one countries")

S1 <- add_element(label = "Let's begin",
                  name = "S1",
                  type = "submit")

C1 <- add_element(label = r"(<script>
$(document).ready(function () {
    // for Select2, one must use the 'change' event
    $('input[name^="MPA"]').on('change', function () {
        var textread = $(this).val();
        var match = [...textread.matchAll(/ - (\d+)/g)].map(match => match[1]);
        const urls = match.map(num => `🐠 <a href='https://protectedplanet.net/${num}' target='_blank'>https://protectedplanet.net/${num}</a>`);
        document.getElementById('textURLs').innerHTML = urls.join('<br>');
    });
});
</script>)",
                  name = "C1",
                  type = "note")  ## note that `r"()"` allows for triple quoting which is here needed

N2 <-  add_element(label = "## Step 2: select your Marine Protected Area(s) -- MPA(s)",
                   name = "N2",
                   type = "note")

Q2 <- do.call("rbind", lapply(sort(na.omit(codelist$iso3c)), add_MPA_country))

N3 <- add_element(label = "##### Please inspect each selected MPAs (if any) on Protected Planet by clicking on the following links:
##### <span id='textURLs'></span>
##### 💡 if some are incorrect, please revise your choices above until you are satisfied.",
                  name = "N3",
                  type = "note")

Q3 <- add_element(label = "#### Select the type of the MPA(s) you are responding for",
                  name = "typeMPA",
                  class = "mc_vertical",
                  type = "mc",
                  value = "1",
                  choice1 = "PA (i.e. classified as Protected Areas)",
                  choice2 = "OECM (i.e. classified as Other Effective area-based Conservation Measures)",
                  choice3 = "LMMA (i.e. classified as Locally-Managed Marine Area)",
                  choice4 = "other (including mixed situations in case of several MPAs; please explain below)",
                  choice5 = "I don't know")

Q4 <- add_element(label = "#### Comments (optional)
##### If the selections above did not fully work for you -- MPA(s) and/or type missing, or complex situations --, please provide details here. Otherwise, please leave this field blank and continue.",
                  name = "other_type",
                  type = "textarea",
                  optional = "*",
                  showif = "country != ''")

# Save survey -----------------------------------------------------------

survey_tbl <- bind_rows(N1, Q1, S1, C1, N2, Q2, N3, Q3, Q4)
if (!dir.exists("cleandata")) dir.create("cleandata")
write.csv(survey_tbl, file = "cleandata/survey.csv", row.names = FALSE)


# Upload complex survey -------------------------------------------------

## Note: the name of the google sheet must be the same as that of the survey
choices_tbl <- read.csv("cleandata/choices.csv")
#googlesheets4::gs4_auth(email = "alexandre.courtiol@gmail.com")
#googlesheets4::gs4_create(name = "test_MPA", sheets = list(survey = survey_tbl, choices = choices_tbl)) ## if does not exist

possible_sheets <- googlesheets4::gs4_find("test_MPA")
sheet_id <- possible_sheets[possible_sheets$name == "test_MPA", "id"][[1]]
sheet_id

googlesheets4::sheet_write(choices_tbl, ss = sheet_id, sheet = "choices")

googlesheets4::sheet_write(survey_tbl, ss = sheet_id, sheet = "survey")
