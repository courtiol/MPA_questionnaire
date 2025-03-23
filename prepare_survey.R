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
# It is possible to define a global CSS but only outside the survey sheet so I prefer to define things here

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

h1,
h2,
h3,
h4,
h5,
h6 {
  color:#0B1215;
}

mark {
 line-height: 2;
 background-color: #0B1215;
 color: #FFD700;
}

h4 {
  margin: 0 0 5px 0;
}

p {
  color:#484c50;
}

li{
  color:#484c50;
  font-family:Roboto,Arial,sans-serif;
  font-weight:400;
  font-size:120%;
  margin:0 0 2px 0
}

.run-container .btn-info {
  color:#0B1215;
  border:5px solid rgb(21 45 101);
}
.run-container .btn-info:active,
.run-container .btn-info:focus,
.run-container .btn-info:hover {
  background:rgb(21 45 101)!important;
  border-color:rgb(21 45 101)!important;
  color:#fff!important
}

</style>", 
type = "note",
name = "CSS")

# First page of questionnaire ---------------------------------------------

CSS0 <- CSS; CSS0$name <- "CSS0"

N0 <-  add_element(label = "
<style>
p {
  font-size:130%;
}
</style>
# Study for Marine Protected Area managers:
# What does your workforce include?

<br>

<p>Thanks for participating in this <b><i>voluntary</i></b> study— your input is valuable. <i>Please consider the information in this <a href='https://www.eoceans.org/mpa-workforce-study-2025-appendix' target='blank'>about</a> document and ask any <a href='mailto:christine@eOceans.co'>questions</a> you may have before deciding if you will participate.</i></p>

<p>As the world moves toward protecting 30% of the ocean by 2030,
<a href='https://marine-conservation.org/' target='_blank'>Marine Conservation Institute (MPAtlas)</a>,
<a href='https://www.eoceans.app/' target='_blank'>eOceans</a>,
<a href='https://www.izw-berlin.de/en/' target='_blank'>Leibniz-IZW</a>, and
<a href='https://www.bluenaturealliance.org/' target='_blank'>Blue Nature Alliance</a>
have partnered to understand the MPA workforce — this includes staff,
employees, and others who have a formal role actively contributing to achieving the goals of a specific MPA.</p>

<p>This study aims to create a clear picture of the workforce behind current MPAs and give us an idea of what may be needed as the number of MPAs grows.</p>
	
### Here's how it works:	

<span>
<ul>
  <li><strong>Part A</strong>: 2 questions: Let's identify your MPA.</li>
  <li><strong>Part B</strong>: 6 questions: Tell us about this MPA.</li>
  <li><strong>Part C</strong>: 4 questions: Tell us about you. </li>
  <li><strong>Part D</strong>: 2 (optional) questions: Tell us more? </li>
</ul>
</span>

<p>💡 Click <a href='https://www.eoceans.org/mpa-workforce-study-2025-appendix' target='blank'>here</a> to learn about <em>Data Access</em>, <em>Risks</em>, <em>Support</em>, <em>Funders</em>, and more.</p>

<p>🌟 <strong>Thank you!</strong><br>
Your input is invaluable to this study and will provide crucial insights for others working on MPAs like you.<br>
Together, we’re strengthening the global MPA network.</p>

<p><strong>Sincerely,</strong><br>	
<a href='mailto:christine@eOceans.co'>Dr. Christine Ward-Paige</a>, eOceans<br>
Beth Pike, Marine Conservation Institute<br>
Dr. Alexandre Courtiol, Leibniz-IZW</p>
",
                    name = "N0",
                    type = "note")## FIXME, what to link in [here]?

S0 <- add_element(label = "Let's begin",
                  name = "S0",
                  type = "submit")

logos <- add_element(label = "
<style>
.flex-container {
  display: flex;
  flex-flow: row wrap;
  justify-content: space-evenly;
  align-items: center;
  padding: 0;
  margin: 0;
  list-style: none;
}
</style>

<ul class='flex-container'>
  <li><img src='https://workforce-admin.marine-conservation.org/assets/tmp/admin/5vdmr4voFmgi1so46H7N/MCI_MPAtlas_combo_logo_2023_full.png?v1742736594' alt='Marine Conservation International logo' style='height:100px;'></li>
  <li><img src='https://workforce-admin.marine-conservation.org/assets/tmp/admin/qoT6H_GyxBSyTZzuiIeu/eOceans_small.jpg?v1742736908' alt='eOceans logo' style='height:100px;'></li>
  <li><img src='https://workforce-admin.marine-conservation.org/assets/tmp/admin/TRBFGZWLCDwivtX0xSzF/IZW-logo.jpg?v1740411077' alt='Leibniz-IZW logo' style='height:160px;'></li>
  <li><img src='https://workforce-admin.marine-conservation.org/assets/tmp/admin/H74ABfj3smTa-EcZL_qF/BNA-logo-full-blue-txt-transparent-300x.png?v1742736673' alt='Blue Nature Alliance logo' style='height:140px;'></li>
</ul>",
                     type = "note",
                     name = "logos")

# Second page of questionnaire ---------------------------------------------

CSS1 <- CSS; CSS1$name <- "CSS1"

P1 <-  add_element(label = "# <mark> Part A: Let's identify your Marine Protected Area (MPA)<br>(2 Questions)</mark>",
                   name = "P1",
                   type = "note")

N1 <-  add_element(label = "## 1: Select your country",
                   name = "N1",
                   type = "note")

Q1 <- add_element(label = "#### Select the country (or overseas land) with the MPA you are responding for
🌟 You can search using name, partial name, or ISO code.
💡 Some overseas lands or islands are listed under their own name, e.g. Guadeloupe is listed under 'Guadeloupe (GLP)' and not under 'France (FRA)'.",
                  name = "country",
                  class = "cant_add_choice", 
                  type = "select_or_add_one countries")

S1 <- add_element(label = "Continue",
                  name = "S1",
                  type = "submit")

# Page 3 of questionnaire ---------------------------------------------

CSS2 <- CSS; CSS2$name <- "CSS2"

P2 <-  add_element(label = "# <mark> Part A: Let's identify your MPA (2 Questions)</mark>",
                   name = "P2",
                   type = "note")

N2 <-  add_element(label = "## 2: Select your MPA",
                   name = "N2",
                   type = "note")

add_MPAs_country <- function(countrycode) {
  add_element(label = "#### Select the MPA you are responding for
🌟 You can search using name, partial name, or World Database on Protected Areas (WDPA) PID.
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

Qmissing <- add_element(label = "#### 🛟 Please provide a name and relevant links or emails so we can fully understand and include your MPA in our analysis:",
                        name = "Qmissing",
                        type = "textarea",
                        optional = "*",
                        showif = "hasNA //js_only")

N3 <- add_element(label = "#### 🔎 Inspect information on your MPA on Protected Planet by clicking on the link:
##### <span id='textURLs'style='font-size:150%'></span>",
                  name = "N3",
                  showif = "lengthItems > 0 //js_only",
                  type = "note")

Q_issue1 <- add_element(label = "#### 🚫 You spotted wrong information on Protected Planet?",
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
<div style='font-size:200%'><strong>WARNING!</strong> you selected multiple MPAs.</div>
<div style='font-size:150%'>💡 If you continue, the MPAs will be merged and treated as a single entity.</div>
<div style='font-size:150%'>🌟 To fill in separate surveys per MPA, select one above and follow instructions at the end of this survey.</div>
</div>)",
                             name = "WarnM_multiple",
                             type = "note",
                             showif = "lengthItems > 1 //js_only")

S2 <- S1; S2$name <- "S2"

# Page 4 -----------------------------------------------------------------------

CSS4 <- CSS; CSS4$name <- "CSS4"

P4 <-  add_element(label = "# <mark> Part B: Tell us about this MPA (6 questions) </mark>", ##FIXME replace "your MPA" by its name
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
                 choice1 = "**Leadership focused** — e.g., manager, superintendent, decision maker",
                 choice2 = "**In-site focused** — e.g., ranger and ranger-like roles working in the MPA, with and without enforcement capabilities",
                 choice3 = "**Support focused** — e.g., supporting staff, administrative support, finance",
                 choice4 = "**Stakeholder focused** — e.g., education and outreach",
                 choice5 = "**Science focused** — e.g., collecting and analyzing data to address questions",
                 choice6 = "**Other** — for roles not listed above",
                 choice7 = "None — no one works on this MPA in a formal role",
                 choice8 = "I don't know",
                 optional = "!")

B1 <- add_element(label = "<div style='color:#ffff'>The answer <strong>None</strong> cannot be combined with another category</div>",
                  name = "B1",
                  type = "block",
                  showif = "(M1 %contains_word% '1' | M1 %contains_word% '2' | M1 %contains_word% '3' | M1 %contains_word% '4' | M1 %contains_word% '5' | M1 %contains_word% '6') && M1 %contains_word% '7'")

B2 <- add_element(label = "<div style='color:#ffff'> The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                  name = "B2",
                  type = "block",
                  showif = "(M1 %contains_word% '1' | M1 %contains_word% '2' | M1 %contains_word% '3' | M1 %contains_word% '4' | M1 %contains_word% '5' | M1 %contains_word% '6' | M1 %contains_word% '7') && M1 %contains_word% '8'")

M1missing <- add_element(label = "#### 🛟 If you selected **Other**, please let us know which roles fall under this category:",
                         name = "M1missing",
                         type = "textarea",
                         showif = "M1 %contains_word% '6'",
                         optional = "*")

S4 <- S1; S4$name <- "S4"

# Page 5 -----------------------------------------------------------------------

CSS5 <- CSS; CSS5$name <- "CSS5"

P5 <-  P4; P5$name <- "P5"; P5$showif <- "!M1 %contains_word% '7' && !M1 %contains_word% '8'"

N5 <-  add_element(label = "## 2: Number of people in each role. 
#### For each workforce category selected previously, indicate the number of formal staff who work on this specific MPA.
🌟 For each role, report the number of staff in that role or use COMMENT to record in your own way.
🦀 FTE = Full-time equivalent.
💡 Add a COMMENT to add more options or to explain anything you think we should know.",
                   name = "N5",
                   showif = "!M1 %contains_word% '7' && !M1 %contains_word% '8'",
                   type = "note")

add_personel_questions <- function(label = "### **what**",
                                   category = "what",
                                   showif = NULL,
                                   value = "0",
                                   type = "number 0,999999,1") {

  E1 <- add_element(label = label,
                    name = paste0("PERS_note", category),
                    showif = showif,
                    type = "note")
  
  E2 <- add_element(label = "Full time personel\n(working all year; counted as 1 FTE each)",
                    name = paste0("PERS_", category, "_FullTime"),
                    value = value, showif = showif, type = type)
  
  E3 <- add_element(label = "Part time personel\n(counted as 0.5 FTE each)",
                    name = paste0("PERS_", category, "_PartTime"),
                    value = value, showif = showif, type = type)
  
  E4 <- add_element(label = "Seasonal personel\n(working during e.g. summer; counted as 0.25 FTE each)",
                    name = paste0("PERS_", category, "_Seasonal"),
                    value = value, showif = showif, type = type)
  
  E5 <- add_element(label = "Occcasional personel\n(working few weeks a year; counted as 0.1 FTE each)",
                    name = paste0("PERS_", category, "_Occasional"),
                    value = value, showif = showif, type = type)
  
  C1 <- add_element(label = "#### Add a COMMENT",
                    name = paste0("PERS_NeedComment_", category),
                    showif = showif,
                    type = "check")
  
  C2 <- add_element(label = "#### 💡 Add more options or explain anything you think we should know about this workforce category",
                    name = paste0("PERS_Comment", category),
                    showif = paste0("PERS_NeedComment_", category),
                    type = "textarea",
                    optional = "*")
  
  # NOT WORKING
  # Block <- add_element(label = "<div style='color:#ffff'><strong>NOTE</strong> If you leave all personel to 0, the category will be discarded.</div>",
  #                     name = paste0("PERS_Block", category),
  #                     type = "block",
  #                     showif = paste0(showif, " && PERS_", category, "_FullTime == 0 &&
  #                                     PERS_", category, "_PartTime == 0 &&
  #                                     PERS_", category, "_Seasonal == 0 &&
  #                                     PERS_", category, "_Occasional == 0"))
  
  bind_rows(E1, E2, E3, E4, E5, C1, C2)
}

## Those are personnel questions, do make sure that this agrees with category numbers in M1 above!

PERS1 <- add_personel_questions(label = "### **Leadership focused**", category = "leadership",
                                showif = "M1 %contains_word% '1'")

PERS2 <- add_personel_questions(label = "### **In-site focused**", category = "site",
                                showif = "M1 %contains_word% '2'")

PERS3 <- add_personel_questions(label = "### **Support focused**", category = "support",
                                showif = "M1 %contains_word% '3'")

PERS4 <- add_personel_questions(label = "### **Stakeholder focused**", category = "stakeholder",
                                showif = "M1 %contains_word% '4'")

PERS5 <- add_personel_questions(label = "### **Science focused**", category = "scientists",
                                showif = "M1 %contains_word% '5'")

PERS6 <- add_personel_questions(label = "### **Other**", category = "other",
                                showif = "M1 %contains_word% '6'")

S5 <- S1; S5$name <- "S5"; S5$showif = "!M1 %contains_word% '7' && !M1 %contains_word% '8'"

# Page 6 (optional) ------------------------------------------------------------

CSS6 <- CSS; CSS6$name <- "CSS6"

P6 <- P4; P6$name <- "P6"

SUMM_note <- add_element(label = "## 3: Confirm the number of FTE. 
#### For each workforce category please review the number of FTE corresponding to your previous choices and <b>adjust</b> the numbers if necessary.
🦀 FTE = Full-time equivalent.",
                         type = "note",
                         name = "SUMM_note")

FTE_formula <- function(category) {
  form <- paste0("PERS_", category, "_FullTime + PERS_", category, "_PartTime*0.5 + PERS_", category, "_Seasonal*0.25 + PERS_", category, "_Occasional*0.1")
  paste0("ifelse(is.na(", form, "), 0,", form, ")")
}

FTE_leadership <- add_element(label = "### **Leadership focused**",
                              name = "FTE_leadership",
                              type = "number 0,999999,0.05",
                              value = FTE_formula("leadership"))

FTE_site <- add_element(label = "### **In-site focused**",
                        name = "FTE_site",
                        type = "number 0,999999,0.05",
                        value = FTE_formula("site"))

FTE_support <- add_element(label = "### **Support focused**",
                           name = "FTE_support",
                           type = "number 0,999999,0.05",
                           value = FTE_formula("support"))

FTE_stakeholder <- add_element(label = "### **Stakeholder focused**",
                               name = "FTE_stakeholder",
                               type = "number 0,999999,0.05",
                               value = FTE_formula("stakeholder"))

FTE_scientists <- add_element(label = "### **Science focused**",
                              name = "FTE_scientists",
                              type = "number 0,999999,0.05",
                              value = FTE_formula("scientists"))

FTE_other <- add_element(label = "### **Other**",
                         name = "FTE_other",
                         type = "number 0,999999,0.05",
                         value = FTE_formula("other"))

total_note <- add_element(label = r"(### **TOTAL**
<script>
$(document).ready(function() {
  // Set the total_note field to be readonly on DOM ready
  $('[name="total_note"]').prop('readonly', true);

  // Attach a change event handler to the only form in the document
  $('form').on('change', function() {
    var total = 0;
    // Select all inputs whose name begins with "FTE_"
    $('input[name^="FTE_"]').each(function() {
      // Convert the value to a float; if NaN (missing/empty), default to 0
      var val = parseFloat($(this).val());
      if (isNaN(val)) {
        val = 0;
      }
      total += val;
      total = Math.round(total * 100) / 100;
    });
    // Update the total_note field with the computed total
    $('[name="total_note"]').val(total);
  }).trigger("change");
});
</script>)",
                         name = "total_note",
                         type = "text")

total_info <- add_element(label = "💡 The TOTAL is just the sum for the categories above; you cannot modify it directly.",
                        name = "total_info",
                        type = "note")

total_validate <- add_element(label = "#### 🔎 <b>Confirm</b>",
                              name = "total_validate",
                              type = "check",
                              optional = "!")

total_comment_check <- add_element(label = "#### Add a COMMENT",
                                        name = "total_comment_check",
                                        type = "check")

total_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                                  name = "total_comment",
                                  showif = "total_comment_check",
                                  type = "textarea",
                                  optional = "*")

S6 <- S1; S6$name <- "S6"

# Page 7 -----------------------------------------------------------------------

CSS7 <- CSS; CSS7$name <- "CSS7"

P7 <- P4; P7$name <- "P7"

Tech_note <-  add_element(label = "## 4: Technology used to help?",
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
                            choice5 = "**MPA evaluation technology** (e.g., [eOceans](https://www.eoceans.org/))",
                            choice6 = "**Reporting tools**: Phone, email, or app reporting for marine species, activities, incidents",
                            choice7 = "**None of the above**",
                            choice8 = "**I don’t know**",
                            optional = "!")

Block_tech1 <- add_element(label = "<div style='color:#ffff'>The answer <strong>None of the above</strong> cannot be combined with another category</div>",
                           name = "Block_tech1",
                           type = "block",
                           showif = "(Tech_choice %contains_word% '1' | Tech_choice %contains_word% '2' | Tech_choice %contains_word% '3' | Tech_choice %contains_word% '4' | Tech_choice %contains_word% '5' | Tech_choice %contains_word% '6') && Tech_choice %contains_word% '7'")

Block_tech2 <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                           name = "Block_tech2",
                           type = "block",
                           showif = "(Tech_choice %contains_word% '1' | Tech_choice %contains_word% '2' | Tech_choice %contains_word% '3' | Tech_choice %contains_word% '4' | Tech_choice %contains_word% '5' | Tech_choice %contains_word% '6' | Tech_choice %contains_word% '7') && Tech_choice %contains_word% '8'")

Other_tech <- add_element(label = "#### 🦀 Add other technologies that qualify
🌟 Brand names are fine too.
💡 After typing, press enter to validate what you added.",
                          type = "select_or_add_multiple",
                          name = "Other_tech",
                          choice1 = "I can't think of anything",
                          optional = "*") ##FIXME decide on whether this should be optional or not
                                          ##FIXME if I can't think of anything selected, there should be no other answer

Other_tech_comment_check <- add_element(label = "#### Add a COMMENT",
                                        name = "Other_tech_comment_check",
                                        type = "check")

Other_tech_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                                  name = "Other_tech_comment",
                                  showif = "Other_tech_comment_check",
                                  type = "textarea",
                                  optional = "*")
  
S7 <- S1; S7$name <- "S7"

# Page 8 -----------------------------------------------------------------------

CSS8 <- CSS; CSS8$name <- "CSS8"

P8 <- P4; P8$name <- "P8"

Others_note <-  add_element(label = "## 5: Who else is nearby?",
                            name = "Others_note",
                            type = "note")

Others_choice <-  add_element(label = "#### Who else operates seasonally or more within or nearby this specific MPA who is *not* formally responsible for working for this MPA?
💡 Select all that apply.",
                            name = "Others_choice",
                            type = "mc_multiple",
                            class = "mc_vertical",
                            choice1 = "**Fishers** 🎣",
                            choice2 = "**Divers** 🤿",
                            choice3 = "**Surfers** 🏄️",
                            choice4 = "**Boat operators** ⛵",
                            choice5 = "**Other** — for operators not listed above",
                            choice6 = "None — no one uses this MPA 🏝️",
                            choice7 = "I don't know",
                            optional = "!")

Block_others2 <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                             name = "Block_others2",
                             type = "block",
                             showif = "(Others_choice %contains_word% '1' | Others_choice %contains_word% '2' | Others_choice %contains_word% '3' | Others_choice %contains_word% '4' | Others_choice %contains_word% '5' | Others_choice %contains_word% '6') && Others_choice %contains_word% '7'")

Block_others3 <- add_element(label = "<div style='color:#ffff'>The answer <strong>None</strong> cannot be combined with another category</div>",
                             name = "Block_others3",
                             type = "block",
                             showif = "(Others_choice %contains_word% '1' | Others_choice %contains_word% '2' | Others_choice %contains_word% '3' | Others_choice %contains_word% '4' | Others_choice %contains_word% '5') && Others_choice %contains_word% '6'")

Fishers <- add_element(label = "#### 🦀 Specify the type of fishers 🎣
💡 After typing, press enter to validate what you added.",
type = "select_or_add_multiple",
name = "Fishers",
choice1 = "I can't think of any",
showif = "Others_choice %contains_word% '1'",
optional = "*")

Boats <- add_element(label = "#### 🦀 Specify the type of boat operators ⛵
💡 After typing, press enter to validate what you added.",
type = "select_or_add_multiple",
name = "Boats",
choice1 = "I can't think of any",
showif = "Others_choice %contains_word% '4'",
optional = "*")

Others <- add_element(label = "#### 🦀 Add other operators
💡 After typing, press enter to validate what you added.",
type = "select_or_add_multiple",
name = "Others",
choice1 = "I can't think of any",
showif = "Others_choice %contains_word% '5'",
optional = "*")

Others_comment_check <- add_element(label = "#### Add a COMMENT",
                                    name = "Others_comment_check",
                                    type = "check")

Others_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                              name = "Others_comment",
                              showif = "Others_comment_check",
                              type = "textarea")

S8 <- S1; S8$name <- "S8"

# Page 9 -----------------------------------------------------------------------

CSS9 <- CSS; CSS9$name <- "CSS9"

P9 <- P4; P9$name <- "P9"

Anythingelse_note <-  add_element(label = "## 6: Anything else?",
                            name = "Anythingelse_note",
                            type = "note")

Anythingelse_comment <- add_element(label = "#### Is there anything else you would like us to know about this specific MPA workforce?
💡 Tell us more, so we fully understand.",
                                    name = "Anythingelse_comment",
                                    type = "textarea",
                                    optional = "*")

S9 <- S1; S9$name <- "S9"

# Page 10 ----------------------------------------------------------------------

CSS10 <- CSS; CSS10$name <- "CSS10"

P10 <-  add_element(label = "# <mark> Part C: Tell us about you (4 questions) </mark>",
                    name = "P10",
                    type = "note")

Role_note <-  add_element(label = "## 1: Your role(s)?",
                          name = "Role_note",
                          type = "note")

Role_choice <- add_element(label = "#### What role(s) do you serve in this MPA?
💡 Select all that apply.",
                           name = "Role_choice",
                           type = "mc_multiple",
                           class = "mc_vertical",
                           choice1 = "**Leadership focused** — e.g., manager, superintendent, decision maker",
                           choice2 = "**In-site focused** — e.g., ranger and ranger-like roles working in the MPA, with and without enforcement capabilities",
                           choice3 = "**Support focused** — e.g., supporting staff, administrative support, finance",
                           choice4 = "**Stakeholder focused** — e.g., education and outreach",
                           choice5 = "**Science focused** — e.g., collecting and analyzing data to address questions",
                           choice6 = "**Other** — for roles not listed above",
                           choice7 = "None — no one works on this MPA in a formal role",
                           choice8 = "I don't know",
                           optional = "!")

Role_block1 <- add_element(label = "<div style='color:#ffff'>The answer <strong>None</strong> cannot be combined with another category</div>",
                           name = "Role_block1",
                           type = "block",
                           showif = "(Role_choice %contains_word% '1' | Role_choice %contains_word% '2' | Role_choice %contains_word% '3' | Role_choice %contains_word% '4' | Role_choice %contains_word% '5' | Role_choice %contains_word% '6') && Role_choice %contains_word% '7'")

Role_block2 <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                           name = "Role_block2",
                           type = "block",
                           showif = "(Role_choice %contains_word% '1' | Role_choice %contains_word% '2' | Role_choice %contains_word% '3' | Role_choice %contains_word% '4' | Role_choice %contains_word% '5' | Role_choice %contains_word% '6' | Role_choice %contains_word% '7') && Role_choice %contains_word% '8'")

Role_other_details <- add_element(label = "#### 💡 Tell us more about your **Other** role(s) so we fully understand",
                                  name = "Role_other_details",
                                  showif = "Role_choice %contains_word% '6'",
                                  type = "textarea",
                                  optional = "*")

Role_comment_check <- add_element(label = "#### Add a COMMENT",
                                  name = "Role_comment_check",
                                  type = "check")

Role_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                            name = "Role_comment",
                            showif = "Role_comment_check",
                            type = "textarea")

E10 <- add_element(label = "## NOTE
<style>
p, summary, li{
  color:#484c50;
  font-family:Roboto,Arial,sans-serif;
  font-weight:400;
  font-size:120%;
  margin:0 0 2px 0
}
</style>
#### 🌟 Personal information won't be distributed, but it is helpful to us.
<details>
  <summary>💡 Click <u>here</u> to know why we need this data.</summary>
  <p>The purpose of collecting personal information includes:</p>
  <ol type='1'>
    <li>Requesting follow up information if necessary;</li>
    <li>Helping us understand your answers (e.g., different departments); and </li>
    <li>Sharing the report with you when it is complete.</li>
  </ol>
</details>
",
                   name = "E10",
                   type = "note")

S10 <- S1; S10$name <- "S10"

# Page 11 ----------------------------------------------------------------------

CSS11 <- CSS; CSS11$name <- "CSS11"

P11 <- P10; P11$name <- "P11"

Name_note <-  add_element(label = "## 2: Your name?",
                          name = "Name_note",
                          type = "note")

Name_input <- add_element(label = "#### What is your name?",
                          name = "Name_input",
                          type = "text",
                          optional = "*",
                          showif = "!Anonymous_check")

Anonymous_check <- add_element(label = "#### 🤫 I prefer to remain anonymous",
                               name = "Anonymous_check",
                               type = "check")

Anonymous_info <- add_element(label = "#### 💡 Although remaining anonymous may affect how we can include your MPA, your input is still valuable.",
                              name = "Anonymous_info",
                              type = "note",
                              showif = "Anonymous_check")

Name_comment_check <- add_element(label = "#### Add a COMMENT",
                                  name = "Name_comment_check",
                                  type = "check")

Name_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                            name = "Name_comment",
                            showif = "Name_comment_check",
                            type = "textarea")

E11 <- E10; E11$name <- "E11"

S11 <- S1; S11$name <- "S11"

# Page 12 ----------------------------------------------------------------------

CSS12 <- CSS; CSS12$name <- "CSS12"

P12 <- P10; P12$name <- "P12"

Email_note <-  add_element(label = "## 3: Your email?",
                           name = "Email_note",
                           type = "note")

Email_input <- add_element(label = "#### What is your email address?",
                           name = "Email_input",
                           type = "email",
                           showif = "!Anonymous_email_check",
                           optional = "*")

Anonymous_email_check <- add_element(label = "#### 🤫 I prefer to remain anonymous",
                               name = "Anonymous_email_check",
                               type = "check")

Anonymous_email_info <- add_element(label = "#### 💡 Without an email, we will not be able to follow up or share updates, but your response is still appreciated.",
                              name = "Anonymous_email_info",
                              type = "note",
                              showif = "Anonymous_email_check")

Email_comment_check <- add_element(label = "#### Add a COMMENT",
                                   name = "Email_comment_check",
                                   type = "check")

Email_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                             name = "Email_comment",
                             showif = "Email_comment_check",
                             type = "textarea")

E12 <- E10; E12$name <- "E12"

S12 <- S1; S12$name <- "S12"

# Page 13 ----------------------------------------------------------------------

CSS13 <- CSS; CSS13$name <- "CSS13"

P13 <- P10; P13$name <- "P13"

Acknowledgement_note <-  add_element(label = "## 4: How can we acknowledge you?",
                                     name = "Acknowledgement_note",
                                     type = "note")

Acknowledgement_input <- add_element(label = "#### <b>We can't do this without you!</b> If you would like to be listed in the acknowledgments of the report and other publications, please share how you would like this to appear.
🌟 You may use your first and/or last name. 
💡If you add nothing, your name will not appear.",
                                     name = "Acknowledgement_input",
                                     type = "text",
                                     optional  = "*")

S13 <- S1; S13$name <- "S13"

# Page 14 ----------------------------------------------------------------------

thanks_CSS <- CSS; thanks_CSS$name <- "thanks_CSS"

thanks_head <-  add_element(label = "# <mark>Thank you</mark>",
                            name = "thanks_head",
                            type = "note")

thanks_note <-  add_element(label = "### We greatly appreciate your support for this Global Marine Protected Area Workforce Study—thank you for contributing to this important research initiative.
#### Before letting you go, we have a couple of additional questions for you...",
                            name = "thanks_note",
                            type = "note")


P14 <-  add_element(label = "# <mark>Part D: Tell us more? (2 questions) </mark>",
                    name = "P14",
                    type = "note")

Referrals_note <-  add_element(label = "## 1: Referrals?",
                               name = "Referrals_note",
                               type = "note")

Referrals_text <- add_element(label = "#### Who else should we hear from for this or other MPAs? Enter their details here or share the link with them and your networks.
🌟 If an email is provided, we will only send personal messages and will not distribute or store them. 
💡Add a COMMENT to explain any other suggestions to get wide distribution.",
                              name = "Referrals_text",
                              type = "textarea",
                              optional = "*")


Adequate_note <-  add_element(label = "## 2: Is your workforce adequate?",
                              name = "Adequate_note",
                              type = "note")

Adequate_input <- add_element(label = "#### In your opinion, is this current level of workforce adequate for ensuring this specific MPA can successfully achieve the objectives (fulfil the purpose) for which it was established?
🌟 Adequacy can refer to both the number of personnel and their training/technical skills to meet management needs. 
💡Add a COMMENT below to explain anything you think we should know.",
                              name = "Adequate_input",
                              type = "mc",
                              class = "mc_vertical",
                              choice5 = "**Not Adequate** – Does not meet basic requirements or expectations.",
                              choice4 = "**Somewhat Adequate** – Meets a few requirements but falls short in key areas.",
                              choice3 = "**Moderately Adequate** – Meets most requirements but has room for improvement.",
                              choice2 = "**Mostly Adequate** – Meets nearly all requirements with minor gaps.",
                              choice1 = "**Fully Adequate** – Meets or exceeds all requirements and expectations.",
                              optional = "*")

Adequate_comment_check <- add_element(label = "#### Add a COMMENT",
                                      name = "Adequate_comment_check",
                                      type = "check")

Adequate_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                                name = "Adequate_comment",
                                showif = "Adequate_comment_check",
                                type = "textarea")

E15 <- add_element(label = "## NOTE
#### 🌟 This information will *not* be connected to your personal information or MPA.",
                   name = "E15",
                   type = "note")

P16 <-  add_element(label = "# <mark>THANK YOU</mark>",
                    name = "P16",
                    type = "note")

S16 <- add_element(label = "End the survey",
                  name = "S16",
                  type = "submit") ## FIXME: point to new survey

# Save survey -----------------------------------------------------------

survey_tbl <- bind_rows(CSS0, N0, logos, S0,
                        CSS1, P1, N1, Q1, S1,
                        CSS2, CSS, P2, N2, Q2, C1, Qmissing, N3, Q_issue1, Q_issue1_text, Warn_multiple, S2,
                        CSS4, P4, N4, M1, B1, B2, M1missing, S4,
                        CSS5, P5, N5, PERS1, PERS2, PERS3, PERS4, PERS5, PERS6, S5,
                        CSS6, P6, SUMM_note, FTE_leadership, FTE_site, FTE_support, FTE_stakeholder, FTE_scientists, FTE_other,
                        total_note, total_info, total_validate, total_comment_check, total_comment, S6,
                        CSS7, P7, Tech_note, Tech_choice, Block_tech1, Block_tech2, Other_tech, Other_tech_comment_check, Other_tech_comment, S7,
                        CSS8, P8, Others_note, Others_choice, Block_others2, Block_others3, Fishers, Boats, Others, Others_comment_check, Others_comment, S8,
                        CSS9, P9, Anythingelse_note, Anythingelse_comment, S9,
                        CSS10, P10, Role_note, Role_choice, Role_block1, Role_block2, Role_other_details, Role_comment_check, Role_comment, E10, S10,
                        CSS11, P11, Name_note, Name_input, Anonymous_check, Anonymous_info, Name_comment_check, Name_comment, E11, S11,
                        CSS12, P12, Email_note, Email_input, Anonymous_email_check, Anonymous_email_info, Email_comment_check, Email_comment, E12, S12,
                        CSS13, P13, Acknowledgement_note, Acknowledgement_input, S13,
                        thanks_CSS, thanks_head, thanks_note,
                        Referrals_note, Referrals_text,
                        Adequate_note, Adequate_input, Adequate_comment_check, Adequate_comment,
                        S16
                        )

names_tbl <- table(survey_tbl$name)
if (any(names_tbl > 1)) stop(paste(names(names_tbl)[names_tbl > 1], "duplicated. All name must be unique."))

if (!dir.exists("cleandata")) dir.create("cleandata")
write.csv(survey_tbl, file = "cleandata/survey.csv", row.names = FALSE)

# Footer directly added in Run / Settings / General
# ©Marine Conservation Institute (2025) | [Privacy Policy](https://www.eoceans.org/mpa-workforce-study-2025-appendix) | [About](https://www.eoceans.org/mpa-workforce-study-2025-appendix) | [Report technical issues](mailto:courtiol@izw-berlin.de)
