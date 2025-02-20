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



# CSS definition ----------------------------------------------------------
# Note that the CSS definition is only active on the page it is loaded

CSS <- add_element(label = "<style>
.alert {
  padding: 20px;
  background-color: #f44336;
  color: white;
}

.closebtn {
  margin-left: 15px;
  color: white;
  font-weight: bold;
  float: right;
  font-size: 22px;
  line-height: 20px;
  cursor: pointer;
  transition: 0.3s;
}

.closebtn:hover {
  color: black;
}
</style>", 
type = "note",
name = "CSS")

# First page of questionnaire ---------------------------------------------

N0 <-  add_element(label = "# Study for Marine Protected Area managers and staff:
# Who's working on your MPA?	

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

# Second page of questionnaire ---------------------------------------------

P1 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part A: Let's find your Marine Protected Area (MPA)</mark>",
                   name = "P1",
                   type = "note")

N1 <-  add_element(label = "## 1: Select your country",
                   name = "N1",
                   type = "note")

Q1 <- add_element(label = "#### Select the country (or overseas land) with the MPA you are responding for
💙 You can search using name, partial name, or ISO code.
💡 Some overseas lands or islands are listed under their own name, e.g. Guadeloupe is listed under 'Guadeloupe (GLP)' and not under 'France (FRA)'.",
                  name = "country",
                  class = "cant_add_choice", 
                  type = "select_or_add_one countries")

S1 <- add_element(label = "Continue",
                  name = "S1",
                  type = "submit")

# Page 3 of questionnaire ---------------------------------------------

P2 <-  P1; P2$name <- "P2"

N2 <-  add_element(label = "## 2: Select your MPA",
                   name = "N2",
                   type = "note")

add_MPAs_country <- function(countrycode) {
  add_element(label = "#### Select the MPA you are responding for
💙 You can search using name, partial name, or World Database on Protected Areas (WDPA) PID.
💡 The sites listed here are those present in WDPA for the country (or overseas land) you have selected above. If you cannot find your MPAs, please select 'MPA not listed'.",
              name = paste0("MPA_", countrycode, "_mc_multiple"),
              class = "cant_add_choice",
              type = paste0("select_or_add_multiple MPA_", countrycode),
              showif = paste0("country == '", convert_country_label(countrycode), "'"))
}

Q2 <- do.call("rbind", lapply(sort(na.omit(codelist$iso3c)), add_MPAs_country))

C1 <- add_element(label = r"(<script>
var lengthItems = 0;
var hasNA = false;
$(document).ready(function () {
    // for Select2, one must use the 'change' event
    $('input[name^="MPA"]').on('change', function () {
        var textread = $(this).val();
        var match = [...textread.matchAll(/ - (\d+)/g)].map(match => match[1]);
        const urls = match.map(num => `🐠 <a href='https://protectedplanet.net/${num}' target='_blank'>https://protectedplanet.net/${num}</a>`);
        document.getElementById('textURLs').innerHTML = urls.join('<br>');
        lengthItems = $(this).select2('data').filter(item => item.text !== "MPA not listed").length;
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
                  showif = "lengthItems > 0 //js_only",
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
                        showif = "lengthItems > 0 //js_only",
                        name = "Q_issue1")

Q_issue1_text <- add_element(label = "#### 🛟 Tell us what is wrong:",
                  name = "Q_issue1_text",
                  type = "textarea",
                  optional = "*",
                  showif = "Q_issue1")

Warn_multiple <- add_element(label = r"(
<div class='alert'>
<span class="closebtn" onclick="this.parentElement.style.display='none';">&times;</span> 
<p style='font-size:200%'><strong>WARNING!</strong> you selected multiple MPAs.</p>
<p style='font-size:150%'>💡 If you continue, the MPAs will be merged and treated as a single entity.</p>
<p style='font-size:150%'>💙 To fill in separate surveys per MPA, select one above and follow instructions at the end of this survey.</p>
</div>)",
                             name = "WarnM_multiple",
                             type = "note",
                             showif = "lengthItems > 1 //js_only")

S2 <- S1; S2$name <- "S2"

# Page 4 -----------------------------------------------------------------------

P4 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part B: Tell us about your MPA (5 questions) </mark>", ##FIXME replace "your MPA" by its name
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
                 choice8 = "**I don't know**",
                 optional = "!")

B1 <- add_element(label = "The answer **None** cannot be combined with another category",
                  name = "B1",
                  type = "block",
                  showif = "(M1 %contains_word% '1' | M1 %contains_word% '2' | M1 %contains_word% '3' | M1 %contains_word% '4' | M1 %contains_word% '5' | M1 %contains_word% '6') && M1 %contains_word% '7'")

B2 <- add_element(label = "The answer **I don't know** cannot be combined with another category",
                  name = "B2",
                  type = "block",
                  showif = "(M1 %contains_word% '1' | M1 %contains_word% '2' | M1 %contains_word% '3' | M1 %contains_word% '4' | M1 %contains_word% '5' | M1 %contains_word% '6' | M1 %contains_word% '7') && M1 %contains_word% '8'")

S4 <- S1; S4$name <- "S4"

# Page 5 -----------------------------------------------------------------------

P5 <-  P4; P5$name <- "P5"; P5$showif <- "!M1 %contains_word% '7' && !M1 %contains_word% '8'"

N5 <-  add_element(label = "## 2: Number of people in each role. 
#### For each workforce category selected previously, indicate the number of formal staff who work on this specific MPA.
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

## Those are personnel questions, do make sure that this agrees with category numbers in M1 above!

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

S5 <- S1; S5$name <- "S5"; S5$showif = "!M1 %contains_word% '7' && !M1 %contains_word% '8'"

# Page 6 (optional) ------------------------------------------------------------

P6 <- P4; P6$name <- "P6"

SUMM_note <- add_element(label = "## Confirm the number of FTE. 
#### For each workforce category please review the number of FTE corresponding to your previous choices and adjust if necessary.",
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

total_note <- add_element(label = r"(### **TOTAL**
<script>
var FTE_total = 0;
$(document).ready(function () {
    // for Select2, one must use the 'change' event
    $('input[name^="FTE"]').on('change', function () {
        var textread = $(this).val();
        FTE_total = textread.reduce((sum, num) => sum + Number(num), 0).toString();;
    });
});
if (input) {
  input.readOnly = true;
}
</script>)",
                         name = "total_note",
                         type = "text") ##FIXME compute sum and prevent modifications

total_info <- add_element(label = "💡 The TOTAL is just the sum of the categories above; you cannot modify it directly.",
                        name = "total_info",
                        type = "note")

S6 <- S1; S6$name <- "S6"

# Page 7 -----------------------------------------------------------------------

P7 <- P4; P7$name <- "P7"

Tech_note <-  add_element(label = "## 3: Technology used to help?",
                          name = "Tech_note",
                          type = "note")
                          
Tech_choice <-  add_element(label = "#### If you use technology or other aids to help your MPA workforce expand its effectiveness in THIS specific MPA, please list.
💡 Select all that apply.",
                            name = "Tech_choice",
                            type = "mc_multiple",
                            class = "mc_vertical",
                            choice1 = "**Satellite technologies**",
                            choice2 = "**Radar technologies**",
                            choice3 = "**Underwater acoustic technologies**",
                            choice4 = "**Drone technologies**",
                            choice5 = "**Reporting tools**: Phone, email, or app reporting for marine species, activities, incidents",
                            choice6 = "**None of the above**",
                            choice7 = "**I don’t know**",
                            optional = "!")

Block_tech1 <- add_element(label = "The answer **None of the above** cannot be combined with another category",
                           name = "Block_tech1",
                           type = "block",
                           showif = "(Tech_choice %contains_word% '1' | Tech_choice %contains_word% '2' | Tech_choice %contains_word% '3' | Tech_choice %contains_word% '4' | Tech_choice %contains_word% '5') && Tech_choice %contains_word% '6'")

Block_tech2 <- add_element(label = "The answer **I don't know** cannot be combined with another category",
                           name = "Block_tech2",
                           type = "block",
                           showif = "(Tech_choice %contains_word% '1' | Tech_choice %contains_word% '2' | Tech_choice %contains_word% '3' | Tech_choice %contains_word% '4' | Tech_choice %contains_word% '5' | Tech_choice %contains_word% '6') && Tech_choice %contains_word% '7'")

Other_tech <- add_element(label = "#### 🦀 Add other technologies that qualify
💙 Brand names are fine too.
💡 After typing, press enter to validate what you added.",
                          type = "select_or_add_multiple",
                          name = "Other_tech",
                          choice1 = "I can't think of anything",
                          optional = "*") ##FIXME decide on whether this should be optional or not

Other_tech_comment_check <- add_element(label = "Add a COMMENT",
                                        name = "Other_tech_comment_check",
                                        type = "check")

Other_tech_comment <- add_element(label = "Tell us more, so we fully understand",
                                  name = "Other_tech_comment",
                                  showif = "Other_tech_comment_check",
                                  type = "textarea")
  
S7 <- S1; S7$name <- "S7"

# Page 8 -----------------------------------------------------------------------

P8 <- P4; P8$name <- "P8"

Others_note <-  add_element(label = "## 4: Who else is nearby?",
                            name = "Others_note",
                            type = "note")

Others_choice <-  add_element(label = "#### Who else operates seasonally or more within or nearby this specific MPA who is *not* formally responsible for working for this MPA?
💡 Select all that apply.",
                            name = "Others_choice",
                            type = "mc_multiple",
                            class = "mc_vertical",
                            choice1 = "**Fishers**",
                            choice2 = "**Divers**",
                            choice3 = "**Boat operators**",
                            choice4 = "**Cultural practitioners**",
                            choice5 = "**None** — no one uses this MPA",
                            choice6 = "**None of the above**",
                            choice7 = "**I don’t know**",
                            optional = "!")

Block_others1 <- add_element(label = "The answer **None of the above** cannot be combined with another category",
                             name = "Block_others1",
                             type = "block",
                             showif = "(Others_choice %contains_word% '1' | Others_choice %contains_word% '2' | Others_choice %contains_word% '3' | Others_choice %contains_word% '4' | Others_choice %contains_word% '5') && Others_choice %contains_word% '6'")

Block_others2 <- add_element(label = "The answer **I don't know** cannot be combined with another category",
                             name = "Block_others2",
                             type = "block",
                             showif = "(Others_choice %contains_word% '1' | Others_choice %contains_word% '2' | Others_choice %contains_word% '3' | Others_choice %contains_word% '4' | Others_choice %contains_word% '5' | Others_choice %contains_word% '6') && Others_choice %contains_word% '7'")

Block_others3 <- add_element(label = "The answer **None of the above** cannot be combined with another category",
                             name = "Block_others3",
                             type = "block",
                             showif = "(Others_choice %contains_word% '1' | Others_choice %contains_word% '2' | Others_choice %contains_word% '3' | Others_choice %contains_word% '4') && Others_choice %contains_word% '5'")

Others <- add_element(label = "#### 🦀 Add other operators that qualify
💡 After typing, press enter to validate what you added.",
type = "select_or_add_multiple",
name = "Others",
choice1 = "I can't think of any",
optional = "*") ##FIXME decide on whether this should be optional or not

Others_comment_check <- add_element(label = "Add a COMMENT",
                                    name = "Others_comment_check",
                                    type = "check")

Others_comment <- add_element(label = "💡 Tell us more, so we fully understand",
                              name = "Others_comment",
                              showif = "Others_comment_check",
                              type = "textarea")

S8 <- S1; S8$name <- "S8"

# Page 9 -----------------------------------------------------------------------

P9 <- P4; P9$name <- "P9"

Anythingelse_note <-  add_element(label = "## 5: Anything else?",
                            name = "Anythingelse_note",
                            type = "note")

Anythingelse_comment <- add_element(label = "#### Is there anything else you would like us to know about this specific MPA workforce?
💡 Tell us more, so we fully understand.",
                                    name = "Anythingelse_comment",
                                    type = "textarea",
                                    optional = "*")

S9 <- S1; S9$name <- "S9"

# Page 10 ----------------------------------------------------------------------

P10 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part C: A little about you (4 questions) </mark>",
                    name = "P10",
                    type = "note")

Role_note <-  add_element(label = "## 1: Your role?",
                          name = "Role_note",
                          type = "note")

Role_choice <- add_element(label = "#### What role do you serve in this MPA?
💡 Select all that apply.",
                           name = "Role_choice",
                           type = "mc_multiple",
                           class = "mc_vertical",
                           choice1 = "**Site Focused** — such as rangers and ranger-like roles (with and without enforcement capabilities)",
                           choice2 = "**Stakeholder Focused** — such as education and outreach roles",
                           choice3 = "**Support Focused** — such as supporting staff",
                           choice4 = "**Scientists** — such as those collecting and analyzing data to address questions",
                           choice5 = "**Leadership Focused** — such as decision making",
                           choice6 = "**Other** — for roles unlisted above",
                           choice7 = "**None** — no one works on this MPA in a formal role",
                           choice8 = "**I don't know**",
                           optional = "!")

Role_block1 <- add_element(label = "The answer **None** cannot be combined with another category",
                           name = "Role_block1",
                           type = "block",
                           showif = "(Role_choice %contains_word% '1' | Role_choice %contains_word% '2' | Role_choice %contains_word% '3' | Role_choice %contains_word% '4' | Role_choice %contains_word% '5' | Role_choice %contains_word% '6') && Role_choice %contains_word% '7'")

Role_block2 <- add_element(label = "The answer **I don't know** cannot be combined with another category",
                           name = "Role_block2",
                           type = "block",
                           showif = "(Role_choice %contains_word% '1' | Role_choice %contains_word% '2' | Role_choice %contains_word% '3' | Role_choice %contains_word% '4' | Role_choice %contains_word% '5' | Role_choice %contains_word% '6' | Role_choice %contains_word% '7') && Role_choice %contains_word% '8'")

Role_comment_check <- add_element(label = "Add a COMMENT",
                                  name = "Role_comment_check",
                                  type = "check")

Role_comment <- add_element(label = "💡 Tell us more, so we fully understand",
                            name = "Role_comment",
                            showif = "Role_comment_check",
                            type = "textarea")

E10 <- add_element(label = "## NOTE
#### 💙 Personal information will not be distributed unless requested.
#### 💡 The purpose of collecting this information includes:
<style>
li{
  color:#484c50;
  font-family:Roboto,Arial,sans-serif;
  font-weight:400;
  font-size:120%;
  margin:0 0 2px 0
}
</style>
<ol type='1'>
  <li>Requesting follow up information if necessary;</li>
  <li>It may help us understand your answers (e.g., different departments); and </li>
  <li>To share the report with you when it is complete.</li>
</ol>",
                     name = "E10",
                     type = "note")

S10 <- S1; S10$name <- "S10"

# Page 11 ----------------------------------------------------------------------

P11 <- P10; P11$name <- "P11"

Name_note <-  add_element(label = "## 2: Your name?",
                          name = "Name_note",
                          type = "note")

S11 <- S1; S11$name <- "S11"

# Page 12 ----------------------------------------------------------------------

P12 <- P10; P12$name <- "P12"

Email_note <-  add_element(label = "## 3: Contact email?",
                           name = "Email_note",
                           type = "note")

S12 <- S1; S12$name <- "S12"

# Page 13 ----------------------------------------------------------------------

P13 <- P10; P13$name <- "P13"

Acknowledgement_note <-  add_element(label = "## 4: Acknowledgement?",
                                     name = "Acknowledgement_note",
                                     type = "note")

S13 <- S1; S13$name <- "S13"

# Page 14 ----------------------------------------------------------------------

P14 <- P10; P14$name <- "P14"

Referrals_note <-  add_element(label = "## 5: Referrals?",
                               name = "Referrals_note",
                               type = "note")

S14 <- S1; S14$name <- "S14"

# Page 15 ----------------------------------------------------------------------

S15 <- S1; S15$name <- "S15"

# Save survey -----------------------------------------------------------

survey_tbl <- bind_rows(N0, S0,
                        P1, N1, Q1, S1,
                        CSS, P2, N2, Q2, C1, Qmissing, N3, Q_issue1, Q_issue1_text, Warn_multiple, S2,
                        P4, N4, M1, B1, B2, S4,
                        P5, N5,
                        PERS1, PERS2, PERS3, PERS4, PERS5, PERS6, S5,
                        P6, SUMM_note, FTE_site, FTE_stakeholder, FTE_support, FTE_scientists, FTE_leadership, FTE_other,
                        total_note, total_info, S6,
                        P7, Tech_note, Tech_choice, Block_tech1, Block_tech2, Other_tech, Other_tech_comment_check, Other_tech_comment, S7,
                        P8, Others_note, Others_choice, Block_others1, Block_others2, Block_others3, Others, Others_comment_check, Others_comment, S8,
                        P9, Anythingelse_note, Anythingelse_comment, S9,
                        P10, Role_note, Role_choice, Role_block1, Role_block2, Role_comment_check, Role_comment, E10, S10,
                        P11, Name_note, S11,
                        P12, Email_note, S12,
                        P13, Acknowledgement_note, S13,
                        P14, Referrals_note, S14,
                        S15)

names_tbl <- table(survey_tbl$name)
if (any(names_tbl > 1)) stop(paste(names(names_tbl)[names_tbl > 1], "duplicated. All name must be unique."))

if (!dir.exists("cleandata")) dir.create("cleandata")
write.csv(survey_tbl, file = "cleandata/survey.csv", row.names = FALSE)
