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
# It could be added to each page, or it could be defined outside the survey directly under
# RUN/settings/CSS (I am now doing this to reduce the number of items)

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
 color: white;
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
  border:5px solid #0B1215;
}
.run-container .btn-info:active,
.run-container .btn-info:focus,
.run-container .btn-info:hover {
  background:#0B1215!important;
  border-color:#0B1215!important;
  color:#fff!important
}

</style>", 
type = "note",
name = "CSS")


# Page 1 ---------------------------------------------

CSS0 <- CSS; CSS0$name <- "CSS0"

welcome_text <-  add_element(label = "
<style>
p {
  font-size:130%;
}
</style>
# Study for Marine Protected Area managers:
# What does your workforce include?

<br>

<p>Thanks for participating in this <b><i>voluntary</i></b> study— your input is valuable. <i>Please consider the information in this <a href=' https://www.eoceans.org/mpa-workforce-study-2025-about' target='blank'>about</a> document and ask any <a href='mailto:christine@eOceans.co'>questions</a> you may have before deciding if you will participate.</i></p>

<p>As the world moves toward protecting 30% of the ocean by 2030,
<a href='https://marine-conservation.org/' target='_blank'>Marine Conservation Institute (MPAtlas)</a>,
<a href='https://www.eoceans.org/' target='_blank'>eOceans</a>,
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

<p>💡 Click <a href=' https://www.eoceans.org/mpa-workforce-study-2025-about' target='blank'>here</a> to learn about <em>Data Access</em>, <em>Risks</em>, <em>Support</em>, <em>Funders</em>, and more.</p>

<p>🪼 <strong>Thank you!</strong><br>
Your input is invaluable to this study and will provide crucial insights for others working on MPAs like you.<br>
Together, we’re strengthening the global MPA network.</p>

<p><strong>Sincerely,</strong><br>	
<a href='mailto:christine@eOceans.co'>Dr. Christine Ward-Paige</a>, eOceans<br>
Beth Pike, Marine Conservation Institute<br>
Dr. Alexandre Courtiol, Leibniz-IZW</p>
",
name = "welcome_text",
type = "note")

submit_welcome <- add_element(label = "Let's begin",
                              name = "submit_welcome",
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
  <li><img src='https://workforce-admin.marine-conservation.org/assets/tmp/admin/ZWN0zwkMcoQ3a7U0ojDN/MCI_MPAtlas_combo_logo_2023_full.png?v1742888246' alt='Marine Conservation International logo' style='height:100px;'></li>
  <li><img src='https://workforce-admin.marine-conservation.org/assets/tmp/admin/ZWN0zwkMcoQ3a7U0ojDN/eOceans_small.jpg?v1742888246' alt='eOceans logo' style='height:100px;'></li>
  <li><img src='https://workforce-admin.marine-conservation.org/assets/tmp/admin/8W76aVL77zmRysI0giU9/IZW-logo.jpg?v1742888246' alt='Leibniz-IZW logo' style='height:160px;'></li>
  <li><img src='https://workforce-admin.marine-conservation.org/assets/tmp/admin/ZWN0zwkMcoQ3a7U0ojDN/BNA-logo-full-blue-txt-transparent-300x.png?v1742888246' alt='Blue Nature Alliance logo' style='height:140px;'></li>
</ul>",
type = "note",
name = "logos")

# Page 2 ---------------------------------------------

CSS1 <- CSS; CSS1$name <- "CSS1"

partA_note1 <-  add_element(label = "# <mark> Part A: Let's identify your Marine Protected Area (MPA)<br>(2 Questions)</mark>",
                            name = "partA_note1",
                            type = "note")

note_country <-  add_element(label = "## 1: Select your country",
                             name = "note_country",
                             type = "note")

select_country <- add_element(label = "#### Select the country (or overseas land) with the MPA you are responding for
🪼 You can search using name, partial name, or ISO code.
💡 Some overseas lands or islands are listed under their own name, e.g. Guadeloupe is listed under 'Guadeloupe (GLP)' and not under 'France (FRA)'.",
                              name = "country",
                              class = "cant_add_choice", 
                              type = "select_or_add_one countries")

submit_blank <- add_element(label = "Continue",
                            name = "submit_blank",
                            type = "submit")

submit_country <- submit_blank; submit_country$name <- "submit_country"

# Page 3 ---------------------------------------------

CSS2 <- CSS; CSS2$name <- "CSS2"

partA_note2 <-  add_element(label = "# <mark> Part A: Let's identify your MPA (2 Questions)</mark>",
                            name = "partA_note2",
                            type = "note")

note_MPA <-  add_element(label = "## 2: Select your MPA",
                         name = "note_MPA",
                         type = "note")

add_MPAs_country <- function(countrycode) {
  add_element(label = "#### Select the MPA you are responding for
🪼 You can search using name, partial name, or World Database on Protected Areas (WDPA) PID.
💡 The sites listed here are those present in WDPA for the country (or overseas land) you have selected above. If you cannot find your MPAs, please select 'MPA not listed'.",
              name = paste0("MPA_", countrycode, "_mc_multiple"),
              class = "cant_add_choice",
              type = paste0("select_or_add_multiple MPA_", countrycode),
              showif = paste0("country == '", convert_country_label(countrycode), "'"))
}

select_MPA <- do.call("rbind", lapply(sort(na.omit(codelist$iso3c)), add_MPAs_country))

script_protectedplanet <- add_element(label = r"(<script>
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
name = "script_protectedplanet",
type = "note")  ## note that `r"()"` allows for triple quoting which is here needed (R language)

missing_MPA <- add_element(label = "#### 🛟 You selected *MPA not listed*, so please provide a name and relevant links or emails so we can fully understand and include your MPA in our analysis:",
                           name = "missing_MPA",
                           type = "textarea",
                           optional = "!",
                           showif = "hasNA //js_only")

inspect_protectedplanet <- add_element(label = "#### 🔎 Inspect information on your MPA on Protected Planet by clicking on the link:
##### <span id='textURLs'style='font-size:150%'></span>",
name = "inspect_protectedplanet",
showif = "lengthItems > 0 //js_only",
type = "note")

issue_protectedplanet <- add_element(label = "#### 🚫 You spotted wrong information on Protected Planet?",
                                     type = "check",
                                     showif = "lengthItems > 0 //js_only",
                                     name = "issue_protectedplanet")

fix_protectedplanet <- add_element(label = "#### 🛟 Tell us what is wrong:",
                                   name = "fix_protectedplanet",
                                   type = "textarea",
                                   optional = "*",
                                   showif = "issue_protectedplanet")

warn_multiple <- add_element(label = r"(
<div class='alert'>
<span class="closebtn" onclick="this.parentElement.style.display='none';">&times;</span> 
<div style='font-size:200%'><strong>WARNING!</strong> you selected multiple MPAs.</div>
<div style='font-size:150%'>💡 If you continue, the MPAs will be merged and treated as a single entity.</div>
<div style='font-size:150%'>🪼 To fill in separate surveys per MPA, select one above and follow instructions at the end of this survey.</div>
</div>)",
name = "warn_multiple",
type = "note",
showif = "lengthItems > 1 //js_only")

submit_MPA <- submit_blank; submit_MPA$name <- "submit_MPA"

# Page 4 -----------------------------------------------------------------------

CSS4 <- CSS; CSS4$name <- "CSS4"

partB_note1 <-  add_element(label = "# <mark> Part B: Tell us about this MPA (6 questions) </mark>",
                            name = "partB_note1",
                            type = "note")

note_roleinMPA <-  add_element(label = "## 1: Roles of the people who work on this MPA",
                               name = "note_roleinMPA",
                               type = "note")

roleinMPA <- add_element(label = "#### Including yourself, what formal roles are involved in working on this specific MPA to contribute to achieving its goals?
💡 Select all that apply.",
name = "roleinMPA",
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

block_none_roleinMPA <- add_element(label = "<div style='color:#ffff'>The answer <strong>None</strong> cannot be combined with another category</div>",
                                    name = "block_none_roleinMPA",
                                    type = "block",
                                    showif = "(roleinMPA %contains_word% '1' | roleinMPA %contains_word% '2' | roleinMPA %contains_word% '3' | roleinMPA %contains_word% '4' | roleinMPA %contains_word% '5' | roleinMPA %contains_word% '6') && roleinMPA %contains_word% '7'")

block_dontknow_roleinMPA <- add_element(label = "<div style='color:#ffff'> The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                                        name = "block_dontknow_roleinMPA",
                                        type = "block",
                                        showif = "(roleinMPA %contains_word% '1' | roleinMPA %contains_word% '2' | roleinMPA %contains_word% '3' | roleinMPA %contains_word% '4' | roleinMPA %contains_word% '5' | roleinMPA %contains_word% '6' | roleinMPA %contains_word% '7') && roleinMPA %contains_word% '8'")

detail_other_roleinMPA <- add_element(label = "#### 🛟 If you selected **Other**, please let us know which roles fall under this category:",
                                      name = "detail_other_roleinMPA",
                                      type = "textarea",
                                      showif = "roleinMPA %contains_word% '6'",
                                      optional = "*")

roleinMPA_check <- add_element(label = "#### Add a COMMENT",
                          name = "roleinMPA_check",
                          type = "check")

roleinMPA_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                            name = "roleinMPA_comment",
                            showif = "roleinMPA_check",
                            type = "textarea",
                            optional = "*")

submit_roleinMPA <- submit_MPA; submit_roleinMPA$name <- "submit_roleinMPA"

# Page 5 -----------------------------------------------------------------------

CSS5 <- CSS; CSS5$name <- "CSS5"

partB_note2 <-  partB_note1; partB_note2$name <- "partB_note2"#; partB_note2$showif <- "!roleinMPA %contains_word% '7' && !roleinMPA %contains_word% '8'"

note_personel <-  add_element(label = "## 2: Number of people in each role. 
#### For each workforce category selected previously, indicate the number of formal staff who work on this specific MPA.
🪼 For each role, report the number of staff in that role or use COMMENT to record in your own way.
🦀 FTE = Full-time equivalent.
💡 Add a COMMENT to add more options or to explain anything you think we should know.",
                              name = "note_personel",
                              showif = "!roleinMPA %contains_word% '7' && !roleinMPA %contains_word% '8'",
                              type = "note")

note_no_personel <- add_element(label = "## 2: Number of people in each role. 
#### You haven't selected any workforce category, so please continue.",
                                name = "note_no_personel",
                                showif = "roleinMPA %contains_word% '7' || roleinMPA %contains_word% '8'",
                                type = "note")

add_personel_questions <- function(label = "### **what**",
                                   category = "what",
                                   showif = NULL,
                                   value = "0",
                                   type = "number 0,999999,1") {
  
  E1 <- add_element(label = label,
                    name = paste0("pers_note", category),
                    showif = showif,
                    type = "note")
  
  E2 <- add_element(label = "Full time personel\n(working all year; counted as 1 FTE each)",
                    name = paste0("pers_", category, "_fulltime"),
                    value = value, showif = showif, type = type)
  
  E3 <- add_element(label = "Part time personel\n(counted as 0.5 FTE each)",
                    name = paste0("pers_", category, "_parttime"),
                    value = value, showif = showif, type = type)
  
  E4 <- add_element(label = "Seasonal personel\n(working during e.g. summer; counted as 0.25 FTE each)",
                    name = paste0("pers_", category, "_seasonal"),
                    value = value, showif = showif, type = type)
  
  E5 <- add_element(label = "Occcasional personel\n(working few weeks a year; counted as 0.1 FTE each)",
                    name = paste0("pers_", category, "_occasional"),
                    value = value, showif = showif, type = type)
  
  # NOT WORKING
  # Block <- add_element(label = "<div style='color:#ffff'><strong>NOTE</strong> If you leave all personel to 0, the category will be discarded.</div>",
  #                     name = paste0("pers_Block", category),
  #                     type = "block",
  #                     showif = paste0(showif, " && pers_", category, "_fulltime == 0 &&
  #                                     pers_", category, "_parttime == 0 &&
  #                                     pers_", category, "_seasonal == 0 &&
  #                                     pers_", category, "_occasional == 0"))
  
  bind_rows(E1, E2, E3, E4, E5)
}

## Those are personnel questions, do make sure that this agrees with category numbers in roleinMPA above!

pers1 <- add_personel_questions(label = "### **Leadership focused**", category = "leadership",
                                showif = "roleinMPA %contains_word% '1'")

pers2 <- add_personel_questions(label = "### **In-site focused**", category = "site",
                                showif = "roleinMPA %contains_word% '2'")

pers3 <- add_personel_questions(label = "### **Support focused**", category = "support",
                                showif = "roleinMPA %contains_word% '3'")

pers4 <- add_personel_questions(label = "### **Stakeholder focused**", category = "stakeholder",
                                showif = "roleinMPA %contains_word% '4'")

pers5 <- add_personel_questions(label = "### **Science focused**", category = "scientists",
                                showif = "roleinMPA %contains_word% '5'")

pers6 <- add_personel_questions(label = "### **Other**", category = "other",
                                showif = "roleinMPA %contains_word% '6'")

pers_check <- add_element(label = "#### Add a COMMENT",
                          name = "pers_check",
                          type = "check",
                          showif = "!roleinMPA %contains_word% '7' && !roleinMPA %contains_word% '8'")

pers_comment <- add_element(label = "#### 💡 Add more options or explain anything you think we should know about this workforce category",
                            name = "pers_comment",
                            showif = "pers_check",
                            type = "textarea",
                            optional = "*")

submit_pers <- submit_blank; submit_pers$name <- "submit_pers"; submit_pers$showif = "!roleinMPA %contains_word% '7' && !roleinMPA %contains_word% '8'"

# Page 6 ------------------------------------------------------------

CSS6 <- CSS; CSS6$name <- "CSS6"

partB_note3 <- partB_note2; partB_note3$name <- "partB_note3"

note_FTE <- add_element(label = "## 3: Confirm the number of FTE. 
#### For each workforce category please review the number of FTE corresponding to your previous choices and <b>adjust</b> the numbers if necessary.
🦀 FTE = Full-time equivalent.",
type = "note",
name = "note_FTE")

FTE_formula <- function(category) {
  form <- paste0("pers_", category, "_fulltime + pers_", category, "_parttime*0.5 + pers_", category, "_seasonal*0.25 + pers_", category, "_occasional*0.1")
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

submit_total <- submit_blank; submit_total$name <- "submit_total"

# Page 7 -----------------------------------------------------------------------

CSS7 <- CSS; CSS7$name <- "CSS7"

partB_note4 <- partB_note3; partB_note4$name <- "partB_note4"

note_technoinMPA <-  add_element(label = "## 4: Technology used to help?",
                               name = "note_technoinMPA",
                               type = "note")

technoinMPA <-  add_element(label = "#### If you use technology or other aids to help your MPA workforce expand its effectiveness in THIS specific MPA, please list.
💡 Select all that apply.",
name = "technoinMPA",
type = "mc_multiple",
class = "mc_vertical",
choice1 = "**Satellite technologies**",
choice2 = "**Radar technologies**",
choice3 = "**Underwater acoustic technologies**",
choice4 = "**Drone technologies**",
choice5 = "**MPA evaluation technology** (e.g., <a href='https://www.eoceans.org/mpas' target='_blank'>eOceans</a>)",
choice6 = "**Reporting tools**: Phone, email, or app reporting for marine species, activities, incidents",
choice7 = "None of the above",
choice8 = "I don't know",
optional = "!")

block_none_technoinMPA <- add_element(label = "<div style='color:#ffff'>The answer <strong>None of the above</strong> cannot be combined with another category</div>",
                                    name = "block_none_technoinMPA",
                                    type = "block",
                                    showif = "(technoinMPA %contains_word% '1' | technoinMPA %contains_word% '2' | technoinMPA %contains_word% '3' | technoinMPA %contains_word% '4' | technoinMPA %contains_word% '5' | technoinMPA %contains_word% '6') && technoinMPA %contains_word% '7'")

block_dontknow_technoinMPA <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                                        name = "block_dontknow_technoinMPA",
                                        type = "block",
                                        showif = "(technoinMPA %contains_word% '1' | technoinMPA %contains_word% '2' | technoinMPA %contains_word% '3' | technoinMPA %contains_word% '4' | technoinMPA %contains_word% '5' | technoinMPA %contains_word% '6' | technoinMPA %contains_word% '7') && technoinMPA %contains_word% '8'")

detail_other_technoinMPA <- add_element(label = "#### 🦀 Add other technologies that qualify
🪼 Brand names are fine too.
💡 Press **'Enter'** ↩️ after each entry to submit.",
type = "select_or_add_multiple",
name = "detail_other_technoinMPA",
choice1 = "Add your item(s) and press 'Enter'",
optional = "*")

technoinMPA_check <- add_element(label = "#### Add a COMMENT",
                               name = "technoinMPA_check",
                               type = "check")

technoinMPA_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                                 name = "technoinMPA_comment",
                                 showif = "technoinMPA_check",
                                 type = "textarea",
                                 optional = "*")

submit_tech <- submit_blank; submit_tech$name <- "submit_tech"

# Page 8 -----------------------------------------------------------------------

CSS8 <- CSS; CSS8$name <- "CSS8"

partB_note5 <- partB_note4; partB_note5$name <- "partB_note5"

note_operator <-  add_element(label = "## 5: Who else is nearby?",
                              name = "note_operator",
                              type = "note")

operator <-  add_element(label = "#### Who else operates seasonally or more within or nearby this specific MPA who is *not* formally responsible for working for this MPA?
💡 Select all that apply.",
name = "operator",
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

block_none_operator <- add_element(label = "<div style='color:#ffff'>The answer <strong>None</strong> cannot be combined with another category</div>",
                                   name = "block_none_operator",
                                   type = "block",
                                   showif = "(operator %contains_word% '1' | operator %contains_word% '2' | operator %contains_word% '3' | operator %contains_word% '4' | operator %contains_word% '5') && operator %contains_word% '6'")

block_dontknow_operator <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                                       name = "block_dontknow_operator",
                                       type = "block",
                                       showif = "(operator %contains_word% '1' | operator %contains_word% '2' | operator %contains_word% '3' | operator %contains_word% '4' | operator %contains_word% '5' | operator %contains_word% '6') && operator %contains_word% '7'")

detail_fishers_operator <- add_element(label = "#### 🦀 Specify the type of fishers 🎣
💡 Press **'Enter'** ↩️ after each entry to submit.",
type = "select_or_add_multiple",
name = "detail_fishers_operator",
choice1 = "Add your item(s) and press 'Enter'",
showif = "operator %contains_word% '1'",
optional = "*")

detail_boats_operator <- add_element(label = "#### 🦀 Specify the type of boat operators ⛵
💡 Press **'Enter'** ↩️ after each entry to submit.",
type = "select_or_add_multiple",
name = "detail_boats_operator",
choice1 = "Add your item(s) and press 'Enter'",
showif = "operator %contains_word% '4'",
optional = "*")

detail_other_operator <- add_element(label = "#### 🦀 Add other operators
💡 Press **'Enter'** ↩️ after each entry to submit.",
type = "select_or_add_multiple",
name = "detail_other_operator",
choice1 = "Add your item(s) and press 'Enter'",
showif = "operator %contains_word% '5'",
optional = "*")

operator_check <- add_element(label = "#### Add a COMMENT",
                              name = "operator_check",
                              type = "check")

operator_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                                name = "operator_comment",
                                showif = "operator_check",
                                type = "textarea")

submit_operator <- submit_blank; submit_operator$name <- "submit_operator"

# Page 9 -----------------------------------------------------------------------

CSS9 <- CSS; CSS9$name <- "CSS9"

partB_note6 <- partB_note5; partB_note6$name <- "partB_note6"

note_anythingelse <-  add_element(label = "## 6: Anything else?",
                                  name = "note_anythingelse",
                                  type = "note")

anythingelse <- add_element(label = "#### Is there anything else you would like us to know about this specific MPA workforce?
💡 Tell us more, so we fully understand.",
name = "anythingelse",
type = "textarea",
optional = "*")

submit_anythingelse <- submit_blank; submit_anythingelse$name <- "submit_anythingelse"

# Page 10 ----------------------------------------------------------------------

CSS10 <- CSS; CSS10$name <- "CSS10"

partC_note1 <-  add_element(label = "# <mark> Part C: Tell us about you (4 questions) </mark>",
                            name = "partC_note1",
                            type = "note")

note_privacy <- add_element(label = "## NOTE
<style>
p, summary, li{
  color:#484c50;
  font-family:Roboto,Arial,sans-serif;
  font-weight:400;
  font-size:120%;
  margin:0 0 2px 0
}
</style>
#### 🪼 Personal information won't be distributed, but it is helpful to us.
<details>
  <summary>💡 Click <u>here</u> to know why we need this data.</summary>
  <p>The purpose of collecting personal information includes:</p>
  <ol type='1'>
    <li>Requesting follow up information if necessary;</li>
    <li>Helping us understand your answers; </li>
    <li>Sharing the report with you when it is complete.</li>
  </ol>
</details>
",
name = "note_privacy",
type = "note")

note_roleself <-  add_element(label = "## 1: Your role(s)?",
                              name = "note_roleself",
                              type = "note")

roleself <- add_element(label = "#### What role(s) do you serve in this MPA?
💡 Select all that apply.",
name = "roleself",
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

block_none_roleself <- add_element(label = "<div style='color:#ffff'>The answer <strong>None</strong> cannot be combined with another category</div>",
                                   name = "block_none_roleself",
                                   type = "block",
                                   showif = "(roleself %contains_word% '1' | roleself %contains_word% '2' | roleself %contains_word% '3' | roleself %contains_word% '4' | roleself %contains_word% '5' | roleself %contains_word% '6') && roleself %contains_word% '7'")

block_dontknow_roleself <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                                       name = "block_dontknow_roleself",
                                       type = "block",
                                       showif = "(roleself %contains_word% '1' | roleself %contains_word% '2' | roleself %contains_word% '3' | roleself %contains_word% '4' | roleself %contains_word% '5' | roleself %contains_word% '6' | roleself %contains_word% '7') && roleself %contains_word% '8'")

detail_other_roleself <- add_element(label = "#### 💡 Tell us more about your **Other** role(s) so we fully understand",
                                     name = "detail_other_roleself",
                                     showif = "roleself %contains_word% '6'",
                                     type = "textarea",
                                     optional = "*")

roleself_check <- add_element(label = "#### Add a COMMENT",
                              name = "roleself_check",
                              type = "check")

roleself_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                                name = "roleself_comment",
                                showif = "roleself_check",
                                type = "textarea")

name_note <-  add_element(label = "## 2: Your name?",
                          name = "name_note",
                          type = "note")

name <- add_element(label = "#### What is your name?",
                    name = "name",
                    type = "text",
                    optional = "*",
                    showif = "!name_anonymous")

name_anonymous <- add_element(label = "#### 🤫 I prefer to remain anonymous",
                              name = "name_anonymous",
                              type = "check")

name_info <- add_element(label = "#### 💡 Although remaining anonymous may affect how we can include your MPA, your input is still valuable.",
                         name = "name_info",
                         type = "note",
                         showif = "name_anonymous")

name_check <- add_element(label = "#### Add a COMMENT",
                          name = "name_check",
                          type = "check")

name_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                            name = "name_comment",
                            showif = "name_check",
                            type = "textarea")

email_note <-  add_element(label = "## 3: Your email?",
                           name = "email_note",
                           type = "note")

email <- add_element(label = "#### What is your email address?",
                     name = "email",
                     type = "email",
                     showif = "!email_anonymous",
                     optional = "*")

email_anonymous <- add_element(label = "#### 🤫 I prefer to remain anonymous",
                               name = "email_anonymous",
                               type = "check")

email_info <- add_element(label = "#### 💡 Without an email, we will not be able to follow up or share updates, but your response is still appreciated.",
                          name = "email_info",
                          type = "note",
                          showif = "email_anonymous")

email_check <- add_element(label = "#### Add a COMMENT",
                           name = "email_check",
                           type = "check")

email_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                             name = "email_comment",
                             showif = "email_check",
                             type = "textarea")

note_acknowledgement <-  add_element(label = "## 4: How can we acknowledge you?",
                                     name = "note_acknowledgement",
                                     type = "note")

acknowledgement <- add_element(label = "#### <b>We can't do this without you!</b> If you would like to be listed in the acknowledgments of the report and other publications, please share how you would like this to appear.
🪼 You may use your first and/or last name. 
💡If you add nothing, your name will not appear.",
name = "acknowledgement",
type = "text",
optional  = "*")

submit_acknowledgement <- submit_blank; submit_acknowledgement$name <- "submit_acknowledgement"

# Page 11 ----------------------------------------------------------------------

CSS11 <- CSS; CSS11$name <- "CSS11"

partD_note1 <-  add_element(label = "# <mark>Thank you</mark>",
                            name = "partD_note1",
                            type = "note")

note_thanks <-  add_element(label = "### We greatly appreciate your support for this Global Marine Protected Area Workforce Study—thank you for contributing to this important research initiative.
#### Before letting you go, we have a couple of additional questions for you...",
name = "note_thanks",
type = "note")


P14 <-  add_element(label = "# <mark>Part D: Tell us more? (2 questions) </mark>",
                    name = "P14",
                    type = "note")

note_referral <-  add_element(label = "## 1: Referrals?",
                              name = "note_referral",
                              type = "note")

referral <- add_element(label = "#### Who else should we hear from for this or other MPAs? Enter their details here or share the link with them and your networks.
🪼 If an email is provided, we will only send personal messages and will not distribute or store them. 
💡Add a COMMENT to explain any other suggestions to get wide distribution.",
name = "referral",
type = "textarea",
optional = "*")


note_adequate <-  add_element(label = "## 2: Is your workforce adequate?",
                              name = "note_adequate",
                              type = "note")

adequate <- add_element(label = "#### In your opinion, is this current level of workforce adequate for ensuring this specific MPA can successfully achieve the objectives (fulfil the purpose) for which it was established?
🪼 Adequacy can refer to both the number of personnel and their training/technical skills to meet management needs. 
💡Add a COMMENT below to explain anything you think we should know.",
name = "adequate",
type = "mc",
class = "mc_vertical",
choice1 = "**Fully Adequate** – Meets or exceeds all requirements and expectations.",
choice2 = "**Mostly Adequate** – Meets nearly all requirements with minor gaps.",
choice3 = "**Moderately Adequate** – Meets most requirements but has room for improvement.",
choice4 = "**Somewhat Adequate** – Meets a few requirements but falls short in key areas.",
choice5 = "**Not Adequate** – Does not meet basic requirements or expectations.",
optional = "*")

adequate_check <- add_element(label = "#### Add a COMMENT",
                              name = "adequate_check",
                              type = "check")

adequate_comment <- add_element(label = "#### 💡 Tell us more, so we fully understand",
                                name = "adequate_comment",
                                showif = "adequate_check",
                                type = "textarea")

E15 <- add_element(label = "## NOTE
#### 🪼 This information will *not* be connected to your personal information or MPA.",
name = "E15",
type = "note")

note_loopback <- add_element(label = "## Can you fill in this survey for another MPA?",
                             name = "note_loopback",
                             type = "note")

loopback <- add_element(label = "#### Tell us if you can?",
                        name = "loopback",
                        choice1 = "Yes",
                        choice2 = "No",
                        type = "mc",
                        class = "mc_vertical",
                        optional = "!")

loopback_info <-  add_element(label = "## 💡 After clicking on *End the survey*, you will be redirected to a new survey.",
                              name = "loopback_info",
                              type = "note",
                              showif = "loopback == '1'")

submit_end <- add_element(label = "End the survey",
                          name = "submit_end",
                          type = "submit")


# Save survey -----------------------------------------------------------

survey_tbl <- bind_rows(welcome_text, logos, submit_welcome,
                        partA_note1, note_country, select_country, submit_country,
                        partA_note2, note_MPA, select_MPA, script_protectedplanet, 
                        missing_MPA, inspect_protectedplanet, issue_protectedplanet, fix_protectedplanet, warn_multiple, submit_MPA,
                        partB_note1, note_roleinMPA, roleinMPA, block_none_roleinMPA, block_dontknow_roleinMPA, detail_other_roleinMPA, roleinMPA_check, roleinMPA_comment, submit_roleinMPA,
                        partB_note2, note_personel, note_no_personel, pers1, pers2, pers3, pers4, pers5, pers6, pers_check, pers_comment, submit_pers,
                        partB_note3, note_FTE, FTE_leadership, FTE_site, FTE_support, FTE_stakeholder, FTE_scientists, FTE_other,
                        total_note, total_info, total_validate, total_comment_check, total_comment, submit_total,
                        partB_note4, note_technoinMPA, technoinMPA, block_none_technoinMPA, block_dontknow_technoinMPA, detail_other_technoinMPA, technoinMPA_check, technoinMPA_comment, submit_tech,
                        partB_note5, note_operator, operator, block_none_operator, block_dontknow_operator, detail_fishers_operator, detail_boats_operator, detail_other_operator, operator_check, operator_comment, submit_operator,
                        partB_note6, note_anythingelse, anythingelse, submit_anythingelse,
                        partC_note1, note_privacy, note_roleself, roleself, block_none_roleself, block_dontknow_roleself, detail_other_roleself, roleself_check, roleself_comment,
                        name_note, name, name_anonymous, name_info, name_check, name_comment,
                        email_note, email, email_anonymous, email_info, email_check, email_comment,
                        note_acknowledgement, acknowledgement, submit_acknowledgement,
                        partD_note1, note_thanks, note_referral, referral,note_adequate, adequate, adequate_check, adequate_comment,
                        note_loopback, loopback, loopback_info, submit_end
)

names_tbl <- table(survey_tbl$name)
if (any(names_tbl > 1)) stop(paste(names(names_tbl)[names_tbl > 1], "duplicated. All name must be unique."))

if (!dir.exists("cleandata")) dir.create("cleandata")
write.csv(survey_tbl, file = "cleandata/survey.csv", row.names = FALSE)

# Footer directly added in Run / Settings / General
# ©Marine Conservation Institute (2025) | <a href=' https://www.eoceans.org/mpa-workforce-study-2025-about' target='blank'>Privacy Policy</a> | <a href=' https://www.eoceans.org/mpa-workforce-study-2025-about' target='blank'>About</a> | [Report technical issues](mailto:courtiol@izw-berlin.de)

# Feedback text at the end
# # 🦈 Thank you 🐳
# ##💡 Feel free to share this survey with others: https://workforce-survey.marine-conservation.org/who-protects-the-ocean

