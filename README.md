
# Preparation for formr questionnaire

<!-- badges: start -->
<!-- badges: end -->

The scripts in this folder are used to create the [Google sheets](https://docs.google.com/spreadsheets/d/1vHkE8yP9OJbM0NxXQzAzmpeKvePs_MtpigZJMxjLzxQ/edit?usp=sharing) required by the questionnaire.

**note**: this is WIP and a test google sheet.

The sheet called *survey* includes the different elements of the questionnaire.

The sheet called *choices* includes the items used for large multiple choice questions (choice of country and of MPA).

## About the scripts

Run the scripts in the following order:

1. `prepare_survey.R`
2. `prepare_choices.R`
3. `upload_survey.R`

#### Caution

Note also that running the script `prepare_choices.R` requires to create (or own) a large R object.

Doing so is computationally intensive and should not be attempted on a laptop.

So better ask me for the data in case you need them.

## Useful links

- [admin platform](https://workforce-survey.marine-conservation.org/info)
- [possible items in formr](https://formr.org/documentation/#available_items)
