## Note: this script is used to prepare the survey sheet used in formr

## Buglist to maybe report on Github (from Leonie):
## - 1) The list_name in the choices sheet is filled in automatically when uploaded. BUT: the showif does not work if the list_name is not given explicitly for all items of this list. The example given does work strangely.
## - 2) maxType in select_or_add_one does not work with comma as documented
## - 3) select_one works with the number of the choices and stores it as"number". select_or_ad_one instead works with the "choicename" itself.  Not documented.

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
}

convert_country_label(c("FRA", "CAN", "FRA", "ATG"))


add_MPA_country <- function(countrycode) {
  add_element(label = "### Select the MPA you are responding for
#### *info*: these are the MPAs listed in WDPA for the country (or overseas land) you have selected above. If you cannot find your MPA, please select 'MPA not listed - NA'.
#### *tip*: you can search using name, partial name, or  WDPA PID.",
              name = paste0("MPA_", countrycode, "_mc"),
              class = "cant_add_choice",
              type = paste0("select_or_add_one MPA_", countrycode),
              showif = paste0("country == '", convert_country_label(countrycode), "'"))
}


survey_beginning <- bind_rows(
  add_element(label = "## Select your Marine Protected Area (MPA)",
              name = "sectionA",
              type = "note",),
  
  add_element(label = "### Select the type of the MPA you are responding for",
              name = "MPA_type",
              class = "mc_vertical",
              type = "mc",
              value = "1",
              choice1 = "PA (i.e. classified as Protected Areas)",
              choice2 = "OECM (i.e. classified as Other Effective area-based Conservation Measures)",
              choice3 = "LMMA (i.e. classified as Locally-Managed Marine Area)",
              choice4 = "other (please do explain in the text field 'Comments (optional)' that will appear below)"),
  
  add_element(label = "### Select the country (or overseas land) with the MPA you are responding for
#### *info*: some overseas lands or islands are listed under their own name, e.g. Guadeloupe is listed under 'Guadeloupe (GLP)' and not under 'France (FRA)'.",
              name = "country",
              class = "cant_add_choice", 
              type = "select_or_add_one countries")
)

survey_MPAs <- do.call("rbind", lapply(sort(na.omit(codelist$iso3c)), add_MPA_country))

survey_unlistedMPAs <- add_element(label = "### Comments (optional)
#### If the selection above did not fully work for you, please write some information to help us understand where is the MPA you are responding for and what is its type. Otherwise, leave this field blank.",
            name = "other_type",
            type = "textarea",
            optional = "*",
            showif = "country != ''"
)

# Upload survey -----------------------------------------------------------

survey_tbl <- bind_rows(survey_beginning, survey_MPAs, survey_unlistedMPAs)


## Note: the name of the google sheet must be the same as that of the survey

choices_tbl <- read.csv("cleandata/choices.csv")

#googlesheets4::gs4_auth(email = "alexandre.courtiol@gmail.com")
googlesheets4::gs4_create(name = "test_MPA", sheets = list(survey = survey_tbl, choices = choices_tbl))
