
# Preparation for formr questionnaire

<!-- badges: start -->
<!-- badges: end -->

The scripts in this folder are used to create the [Google sheets](https://docs.google.com/spreadsheets/d/1B-DeYW4-xnE0gO2DTQdobg1hIGTTafkBLkgfomhUIBE/edit?usp=sharing) required by the questionnaire.

The sheet called *survey* includes the different elements of the questionnaire.

The sheet called *choices* includes the items used for large multiple choice questions (choice of country and of MPA).

## About the scripts

Note that the scripts are only drafts and thus poorly organised so far.

It is assumed that `prepare_choices.R` is run before `prepare_survey.R`.

#### Caution

Note also that running the script `prepare_choices.R` requires to create (or own) a large R object.

Doing so is computationally intensive and should not be attempted on a laptop.

So better ask me for the data in case you need them.

## Useful links

- [test run](https://test-mpa.rforms.org)
- [admin platform](https://www.rforms.org/admin/)
