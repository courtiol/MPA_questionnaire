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

<p>We invite you to participate in this Global Marine Protected Area Workforce Study!</p>

<p>As the world moves toward protecting 30% of the ocean by 2030,
<a href='https://www.bluenaturealliance.org/' target='_blank'>Blue Nature Alliance</a>,
<a href='https://marine-conservation.org/' target='_blank'>Marine Conservation Institute (MPAtlas)</a>,
<a href='https://www.eoceans.app/' target='_blank'>eOceans</a>, and
<a href='https://www.izw-berlin.de/en/' target='_blank'>Leibniz-IZW</a>
have partnered to understand the MPA workforce — this includes staff,
employees, and others who have a formal role actively contributing to achieving the goals of a specific MPA.</p>

<p>This study aims to create a clear picture of the workforce behind current MPAs and give us an idea of what may be needed as the number of MPAs grows.</p>
	
### Here's how it works:	

<span>
<ul>
  <li><strong>Part A</strong>: 2 questions: Let's identify your MPA.</li>
  <li><strong>Part B</strong>: 6 questions: Tell us about this MPA.</li>
  <li><strong>Part C</strong>: 5 questions: Tell us about you. </li>
  <li><strong>Part D</strong>: 2 (optional) questions: Tell us more?. </li>
</ul>
</span>

<p>💡 Click <strong>here</strong> to learn about <em>Data Access</em>, <em>Risks</em>, <em>Support</em>, <em>Funders</em>, and more.</p>

<p>💙 <strong>Thank you!</strong><br>
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
  <li><img src='https://wordpress.bluenaturealliance.org/wp-content/uploads/2024/06/BNA-Logo-White.svg' alt='Blue Nature Alliance logo' style='height:100px;background-color:rgb(21 45 101);'></li>
  <li><img src='https://mpatlas.org/static/mci_logo-a0661cb77b258a220da756c5287df39c.svg' alt='Marine Conservation International logo' style='height:100px;'></li>
  <li><svg id='Layer_1' data-name='Layer 1' xmlns='http://www.w3.org/2000/svg' viewBox='0 0 69 40.7' height='100px' alt='Marine Protection Atlas log'><defs><style>.cls-1{isolation:isolate;}.cls-2{fill:#70b8cd;}.cls-3{fill:#1c597f;}</style></defs><g class='cls-1'><path class='cls-2' d='M3.46,2.76H5.39L7.33,7.68c.23.59.51,1.46.51,1.46h0s.26-.87.49-1.46L10.3,2.76h1.94l.8,10h-1.8l-.42-5.56c0-.66,0-1.54,0-1.54h0s-.29,1-.53,1.54L8.64,11H7.05L5.46,7.15c-.24-.58-.55-1.56-.55-1.56h0s0,.9,0,1.56l-.42,5.56H2.64Z'/><path class='cls-2' d='M22.55,10.16H19.06l-.84,2.55H16.36l3.5-10h1.89l3.51,10H23.39ZM20.8,4.49s-.22,1-.42,1.54L19.5,8.69h2.6L21.22,6c-.18-.56-.39-1.54-.39-1.54Z'/><path class='cls-2' d='M28.89,2.76H32.1a4.69,4.69,0,0,1,2,.28,2.82,2.82,0,0,1,1.63,2.72,2.87,2.87,0,0,1-1.68,2.72v0a2.3,2.3,0,0,1,.33.49l2,3.71h-2L32.44,9H30.7v3.71H28.89Zm3.43,4.67a1.45,1.45,0,0,0,1.57-1.57c0-1-.4-1.54-1.82-1.54H30.7V7.43Z'/><path class='cls-2' d='M40.47,2.76h1.81v10H40.47Z'/><path class='cls-2' d='M47.16,2.76H49l3.77,5.75a13.75,13.75,0,0,1,.82,1.54h0s-.1-1-.1-1.54V2.76h1.8v10h-1.8L49.73,7a14.57,14.57,0,0,1-.83-1.54h0S49,6.39,49,7v5.73H47.16Z'/><path class='cls-2' d='M60.17,2.76h6V4.32H62v2.6h3.34V8.48H62v2.68h4.37v1.55H60.17Z'/></g><g class='cls-1'><path class='cls-3' d='M2.71,16.68H5.14A2,2,0,0,1,7.31,18.8,2.05,2.05,0,0,1,5.14,21H3.4v2.76H2.71Zm2.34,3.65A1.43,1.43,0,0,0,6.6,18.8a1.41,1.41,0,0,0-1.54-1.5H3.4v3Z'/></g><g class='cls-1'><path class='cls-3' d='M9,16.68H11a3.12,3.12,0,0,1,1.32.19,1.88,1.88,0,0,1,1.06,1.81,1.88,1.88,0,0,1-1.33,1.9v0a2.74,2.74,0,0,1,.18.28l1.53,2.83H13l-1.59-3H9.65v3H9Zm2.36,3.45a1.31,1.31,0,0,0,1.39-1.43,1.3,1.3,0,0,0-.64-1.23,2,2,0,0,0-1-.17H9.65v2.83Z'/></g><g class='cls-1'><path class='cls-3' d='M18.56,16.56a3.55,3.55,0,0,1,3.58,3.59,3.58,3.58,0,1,1-7.16,0A3.55,3.55,0,0,1,18.56,16.56Zm0,6.63a2.93,2.93,0,0,0,2.86-3,2.86,2.86,0,1,0-5.72,0A2.92,2.92,0,0,0,18.56,23.19Z'/></g><g class='cls-1'><path class='cls-3' d='M25.54,17.3H23v-.62h5.74v.62H26.23v6.41h-.69Z'/></g><g class='cls-1'><path class='cls-3' d='M30.07,16.68h4v.62H30.76v2.56h2.67v.62H30.76v2.61h3.45v.62H30.07Z'/></g><g class='cls-1'><path class='cls-3' d='M39,16.56a3.57,3.57,0,0,1,2.53.91l-.37.51A3.31,3.31,0,0,0,39,17.2a2.81,2.81,0,0,0-2.83,2.94A2.9,2.9,0,0,0,39,23.19a3.29,3.29,0,0,0,2.28-.94l.4.49A3.75,3.75,0,0,1,39,23.83a3.54,3.54,0,0,1-3.56-3.68A3.49,3.49,0,0,1,39,16.56Z'/></g><g class='cls-1'><path class='cls-3' d='M45,17.3H42.49v-.62h5.74v.62H45.71v6.41H45Z'/></g><g class='cls-1'><path class='cls-3' d='M49.55,16.68h.69v7h-.69Z'/></g><g class='cls-1'><path class='cls-3' d='M55.51,16.56a3.55,3.55,0,0,1,3.58,3.59,3.58,3.58,0,1,1-7.16,0A3.55,3.55,0,0,1,55.51,16.56Zm0,6.63a2.93,2.93,0,0,0,2.86-3,2.86,2.86,0,1,0-5.72,0A2.92,2.92,0,0,0,55.51,23.19Z'/></g><g class='cls-1'><path class='cls-3' d='M60.91,16.68h.67l3.59,5.06c.24.35.58,1,.58,1h0s-.06-.58-.06-1V16.68h.69v7h-.67l-3.59-5.06c-.24-.35-.58-.95-.58-.95h0s.06.58.06.95v5.06h-.69Z'/></g><g class='cls-1'><path class='cls-2' d='M8.79,35.16H5.3l-.84,2.55H2.59L6.1,27.76H8l3.5,10H9.63ZM7,29.49s-.22,1-.42,1.54l-.88,2.66H8.34L7.45,31c-.18-.56-.39-1.54-.39-1.54Z'/><path class='cls-2' d='M20,29.32H16.72V27.76h8.33v1.56H21.79v8.39H20Z'/><path class='cls-2' d='M32.11,27.76h1.8v8.4h4.3v1.55h-6.1Z'/><path class='cls-2' d='M50.66,35.16H47.17l-.84,2.55H44.47L48,27.76h1.89l3.5,10H51.5Zm-1.75-5.67s-.23,1-.42,1.54l-.88,2.66h2.6L49.33,31c-.18-.56-.39-1.54-.39-1.54Z'/><path class='cls-2' d='M60.6,35.16a4,4,0,0,0,2.48,1.05c.75,0,1.44-.39,1.44-1.2,0-1.78-4.69-1.47-4.69-4.53a3,3,0,0,1,3.33-2.88,4.24,4.24,0,0,1,3,1.06l-.79,1.47a3.61,3.61,0,0,0-2.19-.87c-.84,0-1.5.49-1.5,1.19,0,1.77,4.68,1.34,4.68,4.51a3,3,0,0,1-3.28,2.92,4.79,4.79,0,0,1-3.43-1.36Z'/></g></svg></li>
  <li><img src='https://images.squarespace-cdn.com/content/v1/5a3bcce91f318d73497730c0/55f35164-4719-4bcf-ae2e-3f145fdfdfd4/eOceans_small.jpeg?format=1500w' alt='eOceans logo' style='height:100px;'></li>
  <li><img src='https://www.izw-berlin.de/files/images/logos/Logo_en.jpg' alt='Leibniz-IZW logo' style='height:160px;'></li>
</ul>",
                     type = "note",
                     name = "logos")

# Second page of questionnaire ---------------------------------------------

CSS1 <- CSS; CSS1$name <- "CSS1"

P1 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part A: Let's identify your Marine Protected Area (MPA)<br>(2 Questions)</mark>",
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

CSS2 <- CSS; CSS2$name <- "CSS2"

P2 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part A: Let's identify your MPA (2 Questions)</mark>",
                   name = "P2",
                   type = "note")

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
<div style='font-size:200%'><strong>WARNING!</strong> you selected multiple MPAs.</div>
<div style='font-size:150%'>💡 If you continue, the MPAs will be merged and treated as a single entity.</div>
<div style='font-size:150%'>💙 To fill in separate surveys per MPA, select one above and follow instructions at the end of this survey.</div>
</div>)",
                             name = "WarnM_multiple",
                             type = "note",
                             showif = "lengthItems > 1 //js_only")

S2 <- S1; S2$name <- "S2"

# Page 4 -----------------------------------------------------------------------

CSS4 <- CSS; CSS4$name <- "CSS4"

P4 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part B: Tell us about this MPA (6 questions) </mark>", ##FIXME replace "your MPA" by its name
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
                 optional = "!") ## FIXME do we need to make them detail Other in a field?

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
💙 For each role, report the number of staff in that role or use COMMENT to record in your own way.
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
  
  CodeBlock <- add_element(label = paste0(r"(<script>
  var total_)", category, r"( = 0;
  $(document).ready(function() {
    $('form').on('change', function() {
      total_)", category, r"( = 0;
      var val = 0;
      // Select all inputs whose name begins with "PERS_)", category, r"(
      $('input[name^="PERS_)", category, r"("]').each(function() {
        // Convert the value to a float; if NaN (missing/empty), default to 0
        val = parseFloat($(this).val());
        if (isNaN(val)) {
          val = 0;
        }
        total_)", category, r"( += val;
      });
    }).trigger("change");
  });
</script>))"),
                            name = paste0("PERS_BlockCode", category), ## Revert naming so that js does not select this and below block
                            type = "note")
  
  Block <- add_element(label = "### **Test**",
                       name = paste0("PERS_Block", category),
                       type = "block",
                       showif = paste0("total_", category, "=== 0 && M1 %contains_word%", category, "//js_only"))
  
  C1 <- add_element(label = "#### Add a COMMENT",
                    name = paste0("PERS_NeedComment_", category),
                    showif = showif,
                    type = "check")
  
  C2 <- add_element(label = "#### 💡 Add more options or explain anything you think we should know about this workforce category",
                    name = paste0("PERS_Comment", category),
                    showif = paste0("PERS_NeedComment_", category),
                    type = "textarea",
                    optional = "*")
  
  bind_rows(E1, E2, E3, E4, E5, CodeBlock, Block, C1, C2)
}

## Those are personnel questions, do make sure that this agrees with category numbers in M1 above!

PERS1 <- add_personel_questions(label = "### **Site Focused**", category = "site",
                                showif = "M1 %contains_word% '1'") ## FIXME: do we allow 0?

PERS2 <- add_personel_questions(label = "### **Stakeholder Focused**", category = "stakeholder",
                                showif = "M1 %contains_word% '2'") ## FIXME: do we allow 0?

PERS3 <- add_personel_questions(label = "### **Support Focused**", category = "support",
                                showif = "M1 %contains_word% '3'") ## FIXME: do we allow 0?

PERS4 <- add_personel_questions(label = "### **Scientists**", category = "scientists",
                                showif = "M1 %contains_word% '4'") ## FIXME: do we allow 0?

PERS5 <- add_personel_questions(label = "### **Leadership Focused**", category = "leadership",
                                showif = "M1 %contains_word% '5'") ## FIXME: do we allow 0?

PERS6 <- add_personel_questions(label = "### **Other**", category = "other",
                                showif = "M1 %contains_word% '6'") ## FIXME: do we allow 0?

S5 <- S1; S5$name <- "S5"; S5$showif = "!M1 %contains_word% '7' && !M1 %contains_word% '8'"

# Page 6 (optional) ------------------------------------------------------------

CSS6 <- CSS; CSS6$name <- "CSS6"

P6 <- P4; P6$name <- "P6"

SUMM_note <- add_element(label = "## 3: Confirm the number of FTE. 
#### For each workforce category please review the number of FTE corresponding to your previous choices and adjust if necessary.",
                         type = "note",
                         name = "SUMM_note")

FTE_formula <- function(category) {
  form <- paste0("PERS_", category, "_FullTime + PERS_", category, "_PartTime*0.5 + PERS_", category, "_Seasonal*0.25 + PERS_", category, "_Occasional*0.1")
  paste0("ifelse(is.na(", form, "), 0,", form, ")")
}

FTE_site <- add_element(label = "### **Site Focused**",
                        name = "FTE_site",
                        type = "number 0,999999,0.05",
                        value = FTE_formula("site"))

FTE_stakeholder <- add_element(label = "### **Stakeholder Focused**",
                               name = "FTE_stakeholder",
                               type = "number 0,999999,0.05",
                               value = FTE_formula("stakeholder"))

FTE_support <- add_element(label = "### **Support Focused**",
                           name = "FTE_support",
                           type = "number 0,999999,0.05",
                           value = FTE_formula("support"))

FTE_scientists <- add_element(label = "### **Scientists**",
                              name = "FTE_scientists",
                              type = "number 0,999999,0.05",
                              value = FTE_formula("scientists"))

FTE_leadership <- add_element(label = "### **Leadership Focused**",
                              name = "FTE_leadership",
                              type = "number 0,999999,0.05",
                              value = FTE_formula("leadership"))

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

total_info <- add_element(label = "💡 The TOTAL is just the sum of the categories above; you cannot modify it directly.",
                        name = "total_info",
                        type = "note")

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
                            choice5 = "**Reporting tools**: Phone, email, or app reporting for marine species, activities, incidents",
                            choice6 = "**None of the above**",
                            choice7 = "**I don’t know**",
                            optional = "!")

Block_tech1 <- add_element(label = "<div style='color:#ffff'>The answer <strong>None of the above</strong> cannot be combined with another category</div>",
                           name = "Block_tech1",
                           type = "block",
                           showif = "(Tech_choice %contains_word% '1' | Tech_choice %contains_word% '2' | Tech_choice %contains_word% '3' | Tech_choice %contains_word% '4' | Tech_choice %contains_word% '5') && Tech_choice %contains_word% '6'")

Block_tech2 <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
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
                            choice5 = "**None** — no one uses this MPA🏝️",
                            choice6 = "**None of the above**",
                            choice7 = "**I don’t know**",
                            optional = "!")

Block_others1 <- add_element(label = "<div style='color:#ffff'>The answer <strong>None of the above</strong> cannot be combined with another category</div>",
                             name = "Block_others1",
                             type = "block",
                             showif = "(Others_choice %contains_word% '1' | Others_choice %contains_word% '2' | Others_choice %contains_word% '3' | Others_choice %contains_word% '4' | Others_choice %contains_word% '5') && Others_choice %contains_word% '6'")

Block_others2 <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                             name = "Block_others2",
                             type = "block",
                             showif = "(Others_choice %contains_word% '1' | Others_choice %contains_word% '2' | Others_choice %contains_word% '3' | Others_choice %contains_word% '4' | Others_choice %contains_word% '5' | Others_choice %contains_word% '6') && Others_choice %contains_word% '7'")

Block_others3 <- add_element(label = "<div style='color:#ffff'>The answer <strong>None</strong> cannot be combined with another category</div>",
                             name = "Block_others3",
                             type = "block",
                             showif = "(Others_choice %contains_word% '1' | Others_choice %contains_word% '2' | Others_choice %contains_word% '3' | Others_choice %contains_word% '4') && Others_choice %contains_word% '5'")

Others <- add_element(label = "#### Add other operators that qualify
💡 After typing, press enter to validate what you added.",
type = "select_or_add_multiple",
name = "Others",
choice1 = "I can't think of any",
optional = "*") ##FIXME decide on whether this should be optional or not

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

P10 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'> Part C: Tell us about you (5 questions) </mark>",
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
                           choice1 = "**Site Focused** — such as rangers and ranger-like roles (with and without enforcement capabilities)",
                           choice2 = "**Stakeholder Focused** — such as education and outreach roles",
                           choice3 = "**Support Focused** — such as supporting staff",
                           choice4 = "**Scientists** — such as those collecting and analyzing data to address questions",
                           choice5 = "**Leadership Focused** — such as decision making",
                           choice6 = "**Other** — for roles unlisted above",
                           choice7 = "**None** — no one works on this MPA in a formal role",
                           choice8 = "**I don't know**",
                           optional = "!")

Role_block1 <- add_element(label = "<div style='color:#ffff'>The answer <strong>None</strong> cannot be combined with another category</div>",
                           name = "Role_block1",
                           type = "block",
                           showif = "(Role_choice %contains_word% '1' | Role_choice %contains_word% '2' | Role_choice %contains_word% '3' | Role_choice %contains_word% '4' | Role_choice %contains_word% '5' | Role_choice %contains_word% '6') && Role_choice %contains_word% '7'")

Role_block2 <- add_element(label = "<div style='color:#ffff'>The answer <strong>I don't know</strong> cannot be combined with another category</div>",
                           name = "Role_block2",
                           type = "block",
                           showif = "(Role_choice %contains_word% '1' | Role_choice %contains_word% '2' | Role_choice %contains_word% '3' | Role_choice %contains_word% '4' | Role_choice %contains_word% '5' | Role_choice %contains_word% '6' | Role_choice %contains_word% '7') && Role_choice %contains_word% '8'")

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
#### 💙 Personal information won't be distributed, but it is helpful to us.
<details>
  <summary>💡 Click here to know why we need this data.</summary>
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

Acknowledgement_input <- add_element(label = "#### We can't do this without you! If you would like to be listed in the acknowledgments of the report and other publications, please share how you would like this to appear.
💙 You may use your first and/or last name. 
💡If you add nothing, your name will not appear.",
                                     name = "Acknowledgement_input",
                                     type = "textarea",
                                     optional  = "*")

S13 <- S1; S13$name <- "S13"

# Page 14 ----------------------------------------------------------------------

CSS14 <- CSS; CSS14$name <- "CSS14"

P14 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'>Part D: Tell us more? (2 questions) </mark>",
                    name = "P14",
                    type = "note")

Referrals_note <-  add_element(label = "## 1: Referrals?",
                               name = "Referrals_note",
                               type = "note")

Referrals_text <- add_element(label = "#### Please, if there is anyone you believe we need to hear from for this or any other MPA, please enter their names and emails here, or send the link of this questionnaire to them directly. Please also share with any of your networks. 
💙 If an email is provided, we will only send personal messages and will not distribute or store them. 
💡Add a COMMENT to explain any other suggestions to get wide distribution.",
                              name = "Referrals_text",
                              type = "textarea",
                              optional = "*")

S14 <- S1; S14$name <- "S14"

# Page 15 ----------------------------------------------------------------------

CSS15 <- CSS; CSS15$name <- "CSS15"

P15 <-  P14; P15$name <- "P15"

Adequate_note <-  add_element(label = "## 2: Is your workforce adequate?",
                              name = "Adequate_note",
                              type = "note")

Adequate_input <- add_element(label = "#### In your opinion, is this current level of workforce adequate for ensuring this specific MPA can successfully achieve the objectives (fulfil the purpose) for which it was established?
💙 Adequacy can refer to both the number of personnel and their training/technical skills to meet management needs. 
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
#### 💙 This information will *not* be connected to your personal information or MPA.",
                   name = "E15",
                   type = "note")

S15 <- S1; S15$name <- "S15"

# Page 16 ----------------------------------------------------------------------

CSS16 <- CSS; CSS16$name <- "CSS16"

P16 <-  add_element(label = "# <mark style='background-color:#6495ED;color:#FFD700'>THANK YOU</mark>",
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
                        CSS6, P6, SUMM_note, FTE_site, FTE_stakeholder, FTE_support, FTE_scientists, FTE_leadership, FTE_other,
                        total_note, total_info, S6,
                        CSS7, P7, Tech_note, Tech_choice, Block_tech1, Block_tech2, Other_tech, Other_tech_comment_check, Other_tech_comment, S7,
                        CSS8, P8, Others_note, Others_choice, Block_others1, Block_others2, Block_others3, Others, Others_comment_check, Others_comment, S8,
                        CSS9, P9, Anythingelse_note, Anythingelse_comment, S9,
                        CSS10, P10, Role_note, Role_choice, Role_block1, Role_block2, Role_comment_check, Role_comment, E10, S10,
                        CSS11, P11, Name_note, Name_input, Anonymous_check, Name_comment_check, Name_comment, E11, S11,
                        CSS12, P12, Email_note, Email_input, Anonymous_email_check, Email_comment_check, Email_comment, E12, S12,
                        CSS13, P13, Acknowledgement_note, Acknowledgement_input, S13,
                        CSS14, P14, Referrals_note, Referrals_text, S14,
                        CSS15, P15, Adequate_note, Adequate_input, Adequate_comment_check, Adequate_comment, E15, S15,
                        CSS16, P16, S16
                        )

names_tbl <- table(survey_tbl$name)
if (any(names_tbl > 1)) stop(paste(names(names_tbl)[names_tbl > 1], "duplicated. All name must be unique."))

if (!dir.exists("cleandata")) dir.create("cleandata")
write.csv(survey_tbl, file = "cleandata/survey.csv", row.names = FALSE)

