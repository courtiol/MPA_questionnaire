## Note: this script is used to prepare the survey sheet used in formr

## Buglist to maybe report on Github's formr org repo (from Leonie):
## - 1) The list_name in the choices sheet is filled in automatically when uploaded. BUT: the showif does not work if the list_name is not given explicitly for all items of this list. The example given does work strangely.
## - 2) maxType in select_or_add_one does not work with comma as documented
## - 3) select_one works with the number of the choices and stores it as "number". select_or_add_one instead works with the "choicename" itself. Not documented.

rm(list = ls())
library(wdpar)
library(dplyr)
data("codelist", package = "countrycode")


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

convert_label_country <- function(countrylabel) {
  sub(".*\\(([^)]+)\\).*", "\\1", countrylabel)
} # convert_label_country(c("France (FRA)", "Canada (CAN)", "France (FRA)", "Antigua + Barbuda (ATG)"))


# First page of questionnaire ---------------------------------------------

N0 <-  add_element(label = "# Study for MPA managers and staff:
# Who's working on your marine protected area?	

<br>

#### We invite you to participate in this Global Marine Protected Area Workforce Study!	
	
#### As the world moves toward protecting 30% of the ocean by 2030, Blue Nature Alliance, Marine Conservation Institute (MPAtlas), eOceans and Leibniz-IZW have partnered to help understand the MPA workforce — the individuals who actively contribute to helping Marine Protected Areas (MPAs) work.	
	
#### This study will help create a clear picture of the workforce behind current MPAs and give us an idea of what may be needed as the number of MPAs grows.	
	
### Here's how it works:	
#### XX questions about the workforce behind your specific MPA.
  
#### Click [here] to learn about Data Access, Risks, Support, Funders, and more.	

#### This can’t be done without you — Thank you for being part of this important study and for playing a key part in the global MPA network.	

### Sincerely,	

#### Beth Pike, Marine Conservation Institute	
#### Dr. Christine Ward-Paige, eOceans, christine@eOceans.co	
#### Dr. Alexandre Courtiol, Leibniz-IZW",
                    name = "N0",
                    type = "note")

S0 <- add_element(label = "Let's begin",
                  name = "S0",
                  type = "submit")

P1 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part A: Let's find your MPA </mark>", #FIXME? abreviation not yet defined
                   name = "P1",
                   type = "note")

N1 <-  add_element(label = "## 1: Select your country",
                   name = "N1",
                   type = "note")

Q1 <- add_element(label = "#### Select the country (or overseas land) with the MPA(s) you are responding for
💙 You can search using name, partial name, or ISO code.
💡 Some overseas lands or islands are listed under their own name, e.g. Guadeloupe is listed under 'Guadeloupe (GLP)' and not under 'France (FRA)'.",
                  name = "country",
                  class = "cant_add_choice", 
                  type = "select_or_add_one countries")

S1 <- add_element(label = "Continue",
                  name = "S1",
                  type = "submit")

P2 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part A: Let's find your MPA </mark>", #FIXME? abreviation not yet defined
                   name = "P2",
                   type = "note")

N2 <-  add_element(label = "## 2: Select your Marine Protected Area (MPA)",
                   name = "N2",
                   type = "note")

add_MPAs_country <- function(countrycode) {
  add_element(label = "#### Select the MPA you are responding for
💙 You can search using name, partial name, or WDPA PID.
💡 The sites listed here are those present in WDPA for the country (or overseas land) you have selected above. If you cannot find your MPAs, please select 'MPA not listed'.",
              name = paste0("MPA_", countrycode, "_mc_multiple"),
              class = "cant_add_choice",
              type = paste0("select_or_add_multiple MPA_", countrycode),
              showif = paste0("country == '", convert_country_label(countrycode), "'"))
}

Q2 <- do.call("rbind", lapply(sort(na.omit(codelist$iso3c)), add_MPAs_country))

C1 <- add_element(label = r"(<script>
var hasItems = false;
var hasNA = false;
$(document).ready(function () {
    // for Select2, one must use the 'change' event
    $('input[name^="MPA"]').on('change', function () {
        var textread = $(this).val();
        var match = [...textread.matchAll(/ - (\d+)/g)].map(match => match[1]);
        const urls = match.map(num => `🐠 <a href='https://protectedplanet.net/${num}' target='_blank'>https://protectedplanet.net/${num}</a>`);
        document.getElementById('textURLs').innerHTML = urls.join('<br>');
        hasItems = $(this).select2('data').filter(item => item.text !== "MPA not listed").length > 0;
        hasNA = $(this).select2('data').filter(item => item.text === "MPA not listed").length > 0;
    });
});
</script>)",
                   name = "C1",
                   type = "note")  ## note that `r"()"` allows for triple quoting which is here needed (R language)

Qmissing <- add_element(label = "#### 🛟 If you do not see your MPA, please enter the name here:",
                        name = "Qmissing",
                        type = "text 160",
                        showif = "hasNA //js_only")

N3 <- add_element(label = "#### 🔎 Inspect information on your MPA on Protected Planet by clicking on the link:
##### <span id='textURLs'style='font-size:150%'></span>",
                  name = "N3",
                  showif = "hasItems //js_only",
                  type = "note")

# Q3 <- add_element(label = "#### Select the type of the MPA(s) you are responding for",
#                   name = "typeMPA",
#                   class = "mc_vertical",
#                   type = "mc",
#                   value = "1",
#                   choice1 = "PA (i.e. classified as Protected Areas)",
#                   choice2 = "OECM (i.e. classified as Other Effective area-based Conservation Measures)",
#                   choice3 = "LMMA (i.e. classified as Locally-Managed Marine Area)",
#                   choice4 = "other (including mixed situations in case of several MPAs; please explain below)",
#                   choice5 = "I don't know")

Q_issue1 <- add_element(label = "#### 🚫 You spotted wrong information on Protected Planet",
                        type = "check",
                        showif = "hasItems //js_only",
                        name = "Q_issue1")

Q_issue1_text <- add_element(label = "#### 🛟 Tell us what is wrong:",
                  name = "Q_issue1_text",
                  type = "textarea",
                  optional = "*",
                  showif = "Q_issue1")


S3 <- add_element(label = "Continue",
                  name = "S3",
                  type = "submit")

P4 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part B: Tell us about your MPA (6 questions) </mark>", ##FIXME replace "your MPA" by its name
                   name = "P4",
                   type = "note")

N4 <-  add_element(label = "## 1: Roles of the people who work on this MPA",
                   name = "N4",
                   type = "note")

M1 <- add_element(label = "#### Including yourself, what formal roles are involved in working on this specific MPA to contribute to achieving its goals?
💡 Select all that apply.",
                 name = "M1",
                 type = "mc_multiple",
                 class = "mc_vertical",
                 choice1 = "**Site Focused** — such as rangers and ranger-like roles (with and without enforcement capabilities)",
                 choice2 = "**Stakeholder Focused** — such as education and outreach roles",
                 choice3 = "**Support Focused** — such as supporting staff",
                 choice4 = "**Scientists** — such as those collecting and analyzing data to address questions",
                 choice5 = "**Leadership Focused** — such as decision making",
                 choice6 = "**Other** — for roles unlisted above",
                 choice7 = "**None** — no one works on this MPA in a formal role",
                 choice8 = "**I don't know**")

B1 <- add_element(label = "The answer **None** cannot be combined with another category",
                  name = "B1",
                  type = "block",
                  showif = " (M1 %contains_word% '1' | M1 %contains_word% '2' | M1 %contains_word% '3' | M1 %contains_word% '4' | M1 %contains_word% '5' | M1 %contains_word% '6') && M1 %contains_word% '7'")

B2 <- add_element(label = "The answer **I don't know** cannot be combined with another category",
                  name = "B2",
                  type = "block",
                  showif = " (M1 %contains_word% '1' | M1 %contains_word% '2' | M1 %contains_word% '3' | M1 %contains_word% '4' | M1 %contains_word% '5' | M1 %contains_word% '6' | M1 %contains_word% '7') && M1 %contains_word% '8'")

S4 <- add_element(label = "Continue",
                  name = "S4",
                  type = "submit")

# MCH1 <- add_element(label = "#### For each workforce category selected above, indicate the number of formal staff who work on this specific MPA
# 💙 For each role, report the number of staff in that role or use COMMENT to record in your own way.
# 💡 FTE = Full-time equivalent; Full-time all year; Part-time all year; Seasonal (e.g., summer), and Occasional (e.g., few weeks a year).
# 🦀 Add a COMMENT to add more options or to explain anything you think we should know.",
#                     type = "mc_heading",
#                     #class = "mc_width120 rotate_label45",
#                     name = "MCH1",
#                     choice1 = "Full time all year\n (1 FTE)",
#                     choice2 = "Part time all year\n (0.5 FTE)",
#                     choice3 = "Seasonal\n (0.25 FTE)",
#                     choice4 = "Occasional\n (0.1 FTE)")

P5 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part B: Tell us about your MPA (6 questions) </mark>", ##FIXME replace "your MPA" by its name
                   name = "P5",
                   type = "note")

N5 <-  add_element(label = "## 2: Number of people in each role. 
#### For each workforce category selected previously, indicate the number of formal staff who work on this specific MPA
💙 For each role, report the number of staff in that role or use COMMENT to record in your own way.
💡 FTE = Full-time equivalent.
🦀 Add a COMMENT to add more options or to explain anything you think we should know.      
                   ",
                   name = "N5",
                   showif = "!M1 %contains_word% '7' && !M1 %contains_word% '8'",
                   type = "note")

add_personel_questions <- function(label = "### **what**",
                                   category = "what",
                                   showif = NULL,
                                   value = "0",
                                   type = "number 0,999999,1") {
  E1 <- add_element(label = label,
                    name = paste0("PERS_", category, "_note"),
                    showif = showif,
                    type = "note")
  
  E2 <- add_element(label = "Full time personel (working all year; counted as 1 FTE each)",
                    name = paste0("PERS_", category, "_FullTime"),
                    value = value, showif = showif, type = type)
  
  E3 <- add_element(label = "Part time personel (counted as 0.5 FTE each)",
                    name = paste0("PERS_", category, "_PartTime"),
                    value = value, showif = showif, type = type)
  
  E4 <- add_element(label = "Seasonal personel (working during e.g. summer; counted as 0.25 FTE each)",
                    name = paste0("PERS_", category, "_Seasonal"),
                    value = value, showif = showif, type = type)
  
  E5 <- add_element(label = "Occcasional personel (working few weeks a year; counted as 0.1 FTE each)",
                    name = paste0("PERS_", category, "_Occasional"),
                    value = value, showif = showif, type = type)
  
  C1 <- add_element(label = "Add a COMMENT",
                    name = paste0("PERS_", category, "_NeedComment"),
                    showif = showif,
                    type = "check")
  
  C2 <- add_element(label = "Add more options or explain anything you think we should know about this workforce category",
                    name = paste0("PERS_", category, "Comment"),
                    showif = paste0("PERS_", category, "_NeedComment"),
                    type = "textarea")
  
  bind_rows(E1, E2, E3, E4, E5, C1, C2)
}

## Those are personel questions, do make sure that this agrees with category numbers in M1 above!

PERS1 <- add_personel_questions(label = "### **Site Focused**", category = "site",
                                showif = "M1 %contains_word% '1'")

PERS2 <- add_personel_questions(label = "### **Stakeholder Focused**", category = "stakeholder",
                                showif = "M1 %contains_word% '2'")

PERS3 <- add_personel_questions(label = "### **Support Focused**", category = "support",
                                showif = "M1 %contains_word% '3'")

PERS4 <- add_personel_questions(label = "### **Scientists**", category = "scientists",
                                showif = "M1 %contains_word% '4'")

PERS5 <- add_personel_questions(label = "### **Leadership Focused**", category = "leadership",
                                showif = "M1 %contains_word% '5'")

PERS6 <- add_personel_questions(label = "### **Other**", category = "other",
                                showif = "M1 %contains_word% '6'")

S5 <- add_element(label = "Continue",
                  name = "S5",
                  type = "submit")

P6 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part B: Tell us about your MPA (6 questions) </mark>", ##FIXME replace "your MPA" by its name
                   name = "P6",
                   type = "note")

SUMM_note <- add_element(label = "## Confirm the number of FTE. 
#### For each workforce category please review the number of FTE corresponding to your previous choices and adjust if necessary",
                         type = "note",
                         name = "SUMM_note")

FTE_formula <- function(category) {
  form <- paste0("PERS_", category, "_FullTime + PERS_", category, "_PartTime*0.5 + PERS_", category, "_Seasonal*0.25 + PERS_", category, "_Occasional*0.1")
  paste0("ifelse(is.na(", form, "), 0,", form, ")")
}

FTE_site <- add_element(label = "### **Site Focused**",
                        name = "FTE_site",
                        type = "number 0,999999,0.1",
                        value = FTE_formula("site"))

FTE_stakeholder <- add_element(label = "### **Stakeholder Focused**",
                               name = "FTE_stakeholder",
                               type = "number 0,999999,0.1",
                               value = FTE_formula("stakeholder"))

FTE_support <- add_element(label = "### **Support Focused**",
                           name = "FTE_support",
                           type = "number 0,999999,0.1",
                           value = FTE_formula("support"))

FTE_scientists <- add_element(label = "### **Scientists**",
                              name = "FTE_scientists",
                              type = "number 0,999999,0.1",
                              value = FTE_formula("scientists"))

FTE_leadership <- add_element(label = "### **Leadership Focused**",
                              name = "FTE_leadership",
                              type = "number 0,999999,0.1",
                              value = FTE_formula("leadership"))

FTE_other <- add_element(label = "### **Other**",
                         name = "FTE_other",
                         type = "number 0,999999,0.1",
                         value = FTE_formula("other"))

S6 <- add_element(label = "Continue",
                  name = "S6",
                  type = "submit")

# Save survey -----------------------------------------------------------

survey_tbl <- bind_rows(N0, S0,
                        P1, N1, Q1, S1,
                        P2, N2, Q2, C1, Qmissing, N3, Q_issue1, Q_issue1_text, S3,
                        P4, N4, M1, B1, B2, S4,
                        P5, N5,
                        PERS1, PERS2, PERS3, PERS4, PERS5, PERS6, S5,
                        P6, SUMM_note, FTE_site, FTE_stakeholder, FTE_support, FTE_scientists, FTE_leadership, FTE_other, S6)

if (!dir.exists("cleandata")) dir.create("cleandata")
write.csv(survey_tbl, file = "cleandata/survey.csv", row.names = FALSE)
