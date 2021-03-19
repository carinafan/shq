Sea Hero Quest: Qualtrics Data Cleaning
================
Last updated: March 19, 2021

-   [Set up](#set-up)
    -   [Rename variables](#rename-variables)
-   [Exclusions](#exclusions)
    -   [Hardware](#hardware)
    -   [Duplicate emails](#duplicate-emails)
    -   [Catch questions](#catch-questions)
    -   [Education](#education)
    -   [Number of subjects for
        analysis](#number-of-subjects-for-analysis)
-   [Run cleaning script on excluded
    participants](#run-cleaning-script-on-excluded-participants)
-   [Export data](#export-data)
-   [Session info](#session-info)

<!-- ======================================================================= -->

# Set up

``` r
today = "2021-03-18"
```

``` r
library(BBmisc)
```

    ## 
    ## Attaching package: 'BBmisc'

    ## The following object is masked from 'package:base':
    ## 
    ##     isFALSE

``` r
library(writexl)
library(magrittr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::coalesce()  masks BBmisc::coalesce()
    ## x dplyr::collapse()  masks BBmisc::collapse()
    ## x tidyr::extract()   masks magrittr::extract()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::lag()       masks stats::lag()
    ## x purrr::set_names() masks magrittr::set_names()

``` r
df.raw = paste0("../data/qualtrics/", today, "-shq-qualtrics-raw.csv") %>% 
  read.csv(stringsAsFactors = FALSE)
```

## Rename variables

``` r
# retrieve column names from raw file exported from Qualtrics
raw_column_name = names(df.raw)

# clean up column names 
clean_column_name = 
    c("start_date", "end_date", "status", "IPaddress", "progress",
      "duration", "finished", "recorded_date", "responseID",
      "last_name", "first_name", "recipient_email", "external_reference", 
      "location_latitude", "location_longitude", "distribution_channel", 
      "user_language", "consent", "hardware", "recruitment", "recruitment_text",
      "age", "gender", "gender_text", "retired", "occupation",
      "edu_years", "edu_degree", "edu_degree_text",
      "eng_1st_lang", "age_learned_eng", "eng_fluency", "number_lang",
      "hand", "racialized", "racialized_text", "marginalized", "marginalized_text",
      "catch_text", "email", "do_not_contact", 
      "health_colourblind", "health_cholesterol", "health_hypertension",
      "health_heart", "health_cancer", "health_thyroid", "health_liver",
      "health_kidney", "health_diabetes", "health_arthritis", "health_neckback", 
      "health_sleep", "health_obese", "neuro_depression", "neuro_anxiety",
      "neuro_schiz", "neuro_stroke", "neuro_mci", "neuro_dementia", "neuro_seizures",
      "neuro_ms", "neuro_tbiMild", "neuro_tbiModSev", "neuro_braintumour", 
      "neuro_surgery", "neuro_parkinsons", "neuro_other", "neuro_other_text", 
      "sam_episodic_1", "sam_episodic_2", "sam_episodic_3", "sam_episodic_4", 
      "sam_episodic_5", "sam_episodic_6", "sam_episodic_7", "sam_episodic_8",
      "sam_semantic_1", "sam_semantic_2", "sam_semantic_3", 
      "sam_semantic_4", "sam_semantic_5", "sam_semantic_6",
      "sam_spatial_1", "sam_spatial_2", "sam_spatial_3", "catch_MC", 
      "sam_spatial_4", "sam_spatial_5", "sam_spatial_6",
      "sam_future_1", "sam_future_2", "sam_future_3", 
      "sam_future_4", "sam_future_5", "sam_future_6",
      "therapy", "therapy_text", "therapy_date", "therapy_type", "therapy_type_text",
      "phq_1", "phq_2", "phq_3", "phq_4", "phq_5", 
      "phq_6", "phq_7", "phq_8", "phq_9", "phq_function",
      "phq_covid", "covid", "covid_text", 
      "gift_card", "open_data", "shqID")

# create data frame that matches raw to clean column names  

varnames = cbind(raw_column_name, clean_column_name) %>% 
  as.data.frame(stringsAsFactors = FALSE)

# rename variables

df = df.raw

names(df) = mapValues(x = names(df),
                      from = raw_column_name,
                      to = clean_column_name)
```

``` r
# change class of all variables from factors to strings
df %<>% mutate_all(as.character)

# remove rows that don't correspond to subject data
df = df[-c(1,2), ]

# add "SHQ" to subject IDs
df$shqID %<>% paste0("SHQ", .)
```

<!-- ======================================================================= -->

# Exclusions

Before any exclusions, there are 27 survey responses.

``` r
# remove variables that don't need to be in cleaned file
df %<>%
    select(-c(start_date, end_date, status, IPaddress, progress, 
              responseID, last_name, first_name, recipient_email, 
              external_reference, location_latitude, location_longitude,
              distribution_channel, user_language, consent))

# create copy of dataframe for cleaning excluded rows later
df.excluded = df
```

## Hardware

``` r
exclude.hardware = df %>% 
  filter(hardware == "No")

df %<>% filter(!shqID %in% exclude.hardware$shqID)
```

2 participants did not have the hardware requirements to play Sea Hero
Quest and thus did not complete the study.

## Duplicate emails

``` r
exclude.email = df %>% 
  filter(email != "", duplicated(email))

df %<>% filter(!shqID %in% exclude.email$shqID)
```

0 responses were duplicates emails and thus removed. For duplicates,
only the first response was kept.

## Catch questions

There are 2 “catch” questions to ensure participants are paying
attention.

For the text catch, participants had to answer “What is 2 plus seven?”
in a text box. For the multiple choice catch, participants had to select
“Strongly Agree”.

``` r
# text catch
df$catch_text %<>% tolower()

exclude.catchT = df %>% 
  filter(!grepl(pattern = "9|nine", x = catch_text))

# MC catch
exclude.catchM = df %>% 
  filter(catch_MC != "Strongly Agree" | is.na(catch_MC))

# exclude
df %<>% filter(!shqID %in% exclude.catchT |
               !shqID %in% exclude.catchM)
```

After excluding duplicate emails, 0 participants failed the text catch
and 0 participants failed the multiple choice catch.

## Education

``` r
df$edu_years %<>% as.numeric()

exclude.edu = df %>% 
  filter(edu_years < 9 | edu_years > 26)

df %<>% filter(!shqID %in% exclude.edu$shqID)
```

After excluding duplicate emails and catch fails, 0 participants were
excluded for having fewer than 9 or greater than 26 years of education.

## Number of subjects for analysis

After all exclusions, there are 25 subjects left for analysis.

Check that the number of rows in the clean dataframe plus the number of
rows in the excluded dataframe equal the number of rows in the raw data:

``` r
df.excluded %<>% filter(!shqID %in% df$shqID)
nrow(df.raw) - 2 == nrow(df) + nrow(df.excluded)
```

    ## [1] TRUE

<!-- ======================================================================= -->

# Run cleaning script on excluded participants

``` r
# df.excluded %<>% filter(! subjectID %in% df$subjectID)
# 
# # run separate cleaning script
# source("../cleaning/demo-cleaning-excluded.R")
```

<!-- ======================================================================= -->

# Export data

``` r
varnames %>% 
  write.csv(paste0("../data/qualtrics/", today, "-shq-qualtrics-varnames.csv"),
                   row.names = FALSE)
```

``` r
# export subject IDs and email addresses
subjectID = df %>% select(shqID, email, gift_card, do_not_contact, open_data)

# insert something heree about adding a column as to whether subject made it into clean data and can be compensated

subjectID %>% 
  write_xlsx(paste0("../data/qualtrics/", today, "-shq-qualtrics-subjectID.xlsx"))
```

``` r
# df$email = NULL
```

<!-- ======================================================================= -->

# Session info

``` r
sessionInfo()
```

    ## R version 4.0.4 (2021-02-15)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.7
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] forcats_0.5.0   stringr_1.4.0   dplyr_1.0.2     purrr_0.3.4    
    ##  [5] readr_1.4.0     tidyr_1.1.2     tibble_3.0.4    ggplot2_3.3.2  
    ##  [9] tidyverse_1.3.0 magrittr_1.5    writexl_1.3.1   BBmisc_1.11    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5       cellranger_1.1.0 pillar_1.4.6     compiler_4.0.4  
    ##  [5] dbplyr_2.0.0     tools_4.0.4      digest_0.6.27    lubridate_1.7.9 
    ##  [9] jsonlite_1.7.1   evaluate_0.14    lifecycle_0.2.0  checkmate_2.0.0 
    ## [13] gtable_0.3.0     pkgconfig_2.0.3  rlang_0.4.8      reprex_0.3.0    
    ## [17] cli_2.1.0        rstudioapi_0.11  DBI_1.1.0        yaml_2.2.1      
    ## [21] haven_2.3.1      xfun_0.19        withr_2.3.0      xml2_1.3.2      
    ## [25] httr_1.4.2       knitr_1.30       fs_1.5.0         hms_0.5.3       
    ## [29] generics_0.1.0   vctrs_0.3.4      grid_4.0.4       tidyselect_1.1.0
    ## [33] glue_1.4.2       R6_2.5.0         fansi_0.4.1      readxl_1.3.1    
    ## [37] rmarkdown_2.5    modelr_0.1.8     backports_1.2.0  scales_1.1.1    
    ## [41] ellipsis_0.3.1   htmltools_0.5.0  rvest_0.3.6      assertthat_0.2.1
    ## [45] colorspace_1.4-1 stringi_1.5.3    munsell_0.5.0    broom_0.7.2     
    ## [49] crayon_1.3.4
