Sea Hero Quest: Qualtrics Data Cleaning
================
Last updated: March 29, 2021

-   [Set up](#set-up)
    -   [Rename variables](#rename-variables)
-   [Exclusions](#exclusions)
    -   [Hardware](#hardware)
    -   [Incomplete surveys](#incomplete-surveys)
    -   [Duplicate emails](#duplicate-emails)
    -   [Catch questions](#catch-questions)
    -   [Education](#education)
    -   [Number of subjects for
        analysis](#number-of-subjects-for-analysis)
-   [Demographics](#demographics)
-   [SAM](#sam)
-   [PHQ](#phq)
-   [Export data](#export-data)
-   [Session info](#session-info)

<!-- ======================================================================= -->

# Set up

``` r
today = "2021-03-29"
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
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

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
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x dplyr::coalesce()        masks BBmisc::coalesce()
    ## x dplyr::collapse()        masks BBmisc::collapse()
    ## x lubridate::date()        masks base::date()
    ## x tidyr::extract()         masks magrittr::extract()
    ## x dplyr::filter()          masks stats::filter()
    ## x lubridate::intersect()   masks base::intersect()
    ## x dplyr::lag()             masks stats::lag()
    ## x purrr::set_names()       masks magrittr::set_names()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x lubridate::union()       masks base::union()

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

# set aside subject IDs and emails and sort by date completed
subjectID = df %>% 
  select(shqID, recorded_date, email, gift_card, do_not_contact, open_data, recruitment, recruitment_text)

subjectID$recorded_date %<>% ymd_hms()

subjectID %<>% arrange(desc(recorded_date))
```

<!-- ======================================================================= -->

# Exclusions

Before any exclusions, there are 39 survey responses.

``` r
# remove variables that don't need to be in cleaned file
df %<>%
    select(-c(start_date, end_date, status, IPaddress,
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

## Incomplete surveys

88% completion takes participants all the way up to the SHQ instructions
page. So, keep only participants who have completed at least 88% of the
survey.

``` r
df$progress %<>% as.numeric()

exclude.incomplete = df %>% 
  filter(progress < 88)

df %<>% filter(!shqID %in% exclude.incomplete$shqID)
```

2 participants did not complete the Qualtrics survey.

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

After all exclusions, there are 35 subjects left for analysis.

<!-- ============================================================================= -->

# Demographics

``` r
# select demographics variables
df.dem = df %>% 
  select(shqID, recorded_date,
         age, gender, gender_text, hand,
         retired, occupation, edu_years, edu_degree, edu_degree_text,
         eng_1st_lang, age_learned_eng, eng_fluency, number_lang, 
         racialized, racialized_text, marginalized, marginalized_text,
         starts_with("health"), starts_with("neuro"), starts_with("therapy"))
```

<!-- ============================================================================= -->

# SAM

``` r
# create new dataframe that only contains SAM items
df.sam = df %>% select(shqID, starts_with("sam"))

# replace text in responses with numbers
for (row in 1:nrow(df.sam)) {
  for (col in 2:ncol(df.sam)) {
    switch(df.sam[row, col],
           "Strongly Disagree"           = {df.sam[row, col] = 1},
           "Disagree Somewhat"           = {df.sam[row, col] = 2},
           "Neither Agree nor Disagree"  = {df.sam[row, col] = 3},
           "Agree Somewhat"              = {df.sam[row, col] = 4},
           "Strongly Agree"              = {df.sam[row, col] = 5}
           )
  }
}

# change class of all variables from strings to numeric
df.sam$shqID %<>% as.factor()
df.sam %<>% mutate_if(is.character, as.numeric)

# reverse coding
sam.reverse = c("sam_episodic_1", "sam_episodic_2", 
                "sam_semantic_2", "sam_semantic_5", 
                "sam_spatial_3", "sam_spatial_4", "sam_future_6")

for (i in 1:length(sam.reverse)) {
  df.sam[ , (ncol(df.sam) + 1)] = NA # add an empty column for the reverse coded item
  names(df.sam)[ncol(df.sam)] = paste0(sam.reverse[i], "r") # rename empty column
  colToReverse = which(names(df.sam) == sam.reverse[i]) # retrieve index of item to be reversed
  df.sam[ , ncol(df.sam)] = 6 - df.sam[ , colToReverse] # reverse code the item
  df.sam[ , colToReverse] = NULL # remove the original item score
}

# reorder columns
df.sam = df.sam[ , order(names(df.sam))] %>% 
  select(shqID, everything())

# load script to calculate domain scores
source("sam-shq.R")
```

<!-- ============================================================================= -->

# PHQ

``` r
# create new dataframe that only contains PHQ items
df.phq = df %>% select(shqID, starts_with("phq"))

# replace text in responses with numbers
for (row in 1:nrow(df.phq)) {
  for (col in 2:(ncol(df.phq)-1)) {
    switch(df.phq[row, col],
           "Not at all"              = {df.phq[row, col] = 0},
           "Several days"            = {df.phq[row, col] = 1},
           "More than half the days" = {df.phq[row, col] = 2},
           "Nearly every day"        = {df.phq[row, col] = 3}
           )
  }
}

# change class of all variables from strings to numeric
df.phq %<>% 
  mutate_at(vars(phq_1, phq_2, phq_3, phq_4, phq_5, phq_6, phq_7, phq_8, phq_9), as.numeric)

# total score
df.phq$phq_total = df.phq %>%
  select(starts_with("phq")) %>%
  select(-c(phq_function, phq_covid)) %>%
  rowSums()

# reorder columns
df.phq %<>% select(shqID,
                   phq_1, phq_2, phq_3, phq_4, phq_5, phq_6, phq_7, phq_8, phq_9,
                   phq_total, phq_function, phq_covid)
```

<!-- ======================================================================= -->

# Export data

``` r
df.clean = Reduce(function(x, y) merge(x, y, by = "shqID"), 
                  list(df.dem,
                       df.sam,
                       df.phq))
```

``` r
df.clean %>% 
  write.csv(paste0("../data/qualtrics/", today, "-shq-qualtrics-clean.csv"),
            row.names = FALSE)
```

``` r
varnames %>% 
  write.csv(paste0("../data/qualtrics/", today, "-shq-qualtrics-varnames.csv"),
                   row.names = FALSE)
```

``` r
subjectID$clean = NA

for (i in 1:nrow(subjectID)) {
  if (subjectID$shqID[i] %in% df.clean$shqID) {
    subjectID$clean[i] = "yes"
  } else {subjectID$clean[i] = "no"}
}

subjectID %>% 
  write_xlsx(paste0("../data/qualtrics/", today, "-shq-qualtrics-subjectID.xlsx"))
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
    ##  [9] tidyverse_1.3.0 magrittr_1.5    writexl_1.3.1   lubridate_1.7.9
    ## [13] BBmisc_1.11    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5       cellranger_1.1.0 pillar_1.4.6     compiler_4.0.4  
    ##  [5] dbplyr_2.0.0     tools_4.0.4      digest_0.6.27    jsonlite_1.7.1  
    ##  [9] evaluate_0.14    lifecycle_0.2.0  checkmate_2.0.0  gtable_0.3.0    
    ## [13] pkgconfig_2.0.3  rlang_0.4.8      reprex_0.3.0     cli_2.1.0       
    ## [17] rstudioapi_0.11  DBI_1.1.0        yaml_2.2.1       haven_2.3.1     
    ## [21] xfun_0.19        withr_2.3.0      xml2_1.3.2       httr_1.4.2      
    ## [25] knitr_1.30       fs_1.5.0         hms_0.5.3        generics_0.1.0  
    ## [29] vctrs_0.3.4      grid_4.0.4       tidyselect_1.1.0 glue_1.4.2      
    ## [33] R6_2.5.0         fansi_0.4.1      readxl_1.3.1     rmarkdown_2.5   
    ## [37] modelr_0.1.8     backports_1.2.0  scales_1.1.1     ellipsis_0.3.1  
    ## [41] htmltools_0.5.0  rvest_0.3.6      assertthat_0.2.1 colorspace_1.4-1
    ## [45] stringi_1.5.3    munsell_0.5.0    broom_0.7.2      crayon_1.3.4
