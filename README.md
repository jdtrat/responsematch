
# responsematch

The goal of responsematch is to streamline the creation of surveys with downloadable reports for 'responsematch' clients.

## Installation

You can install this package from GitHub as follows:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("jdtrat/responsematch")
```

## Details

`responsematch` has one main function, `create_survey()`, which is used to create a survey using [shinysurveys](https://shinysurveys.jdtrat.com/) with the code necessary to save respondent data to Google Drive/Google Sheets. 

The documentation for this function can be accessed within the console by typing `?create_survey()`. As an overview, this function creates a template of a shinysurvey customized for responsematch clients. 

`create_survey()` takes in two arguments:

* `survey_name`, which is the name of the survey application to create, e.g. "GLI".
* `path`, which specifies where the survey application should be created. Default is the current working directory.

When called, this function will open an internet browser window and asks you to authenticate yout Google Drive and Google Sheets information. This is necessary to remotely store respondent data. *Be sure to allow for seeing, creating, and editing all Google Drive and Google Sheets content.*

The resulting Shiny application is housed in a folder whose name is specified by the `survey_name` parameter. It contains an 'app.R' file -- defining the user-interface and server-side logic of the survey -- a 'www' folder -- housing the survey questions, RMarkdown report, and custom JavaScript -- and an 'R' directory containing additional functions used by the application.

Before running the application, be sure to Before publishing modify the survey questions 'www/questions.csv' and parameterized RMarkdown file 'www/report.Rmd'. You should now be able to test the survey, see the report generate live, and publish it to [shinyapps.io](https://shinyapps.io).

Once published, anyone can complete the survey, view their results in-browser, and (optionally) download a PDF and HTML generated with the 'www/report.Rmd' file. Each respondent's data is stored in a Google Sheet within a Google Drive folder. 

For example, the "GLI" survey data would be stored within the Google Sheet "GLI_data" within the Google Drive folder "GLI_survey". The data can be read back in with the R packages [googledrive](https://googledrive.tidyverse.org/) and [googlesheets4](https://googlesheets4.tidyverse.org/).
