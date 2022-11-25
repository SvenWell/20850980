# Purpose

This is the README for the Financial Econometrics Practical Exam. This
folder was created using by running:

``` r
fmxdat::make_project(Mac = TRUE)
```

With the folder structure for all of the questions made with the
following code:

``` r
CHOSEN_LOCATION <- "/Users/svenwellmann/Desktop/Masters Semester 2/Financial Econometrics/Exam/"

Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}20850980/"), template_name = "Question1")

Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}20850980/"), template_name = "Question2")

Texevier::create_template_html(directory = glue::glue("{CHOSEN_LOCATION}20850980/"), template_name = "Question3")
```

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection 
```

    ##          used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    ## Ncells 479432 25.7    1036351 55.4         NA   666908 35.7
    ## Vcells 884618  6.8    8388608 64.0      16384  1824468 14.0

``` r
pacman::p_load("xts", "tidyverse", "tbl2xts", "PerformanceAnalytics", "lubridate", "glue", "rmsfuns", "fmxdat", "tidyr", "devtools", "readr", "TTR",
               "DEoptimR", "robustbase", "rportfolios", "RiskPortfolios", "fitHeavyTail", "quadprog")
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```
