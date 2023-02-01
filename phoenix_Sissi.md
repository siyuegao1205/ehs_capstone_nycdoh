Phoenix_Sissi
================
2023-1-31

install new package

``` r
#install.packages("tabulizer")
#if (!require("remotes")) {
#    install.packages("remotes")}
# on 64-bit Windows
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
# elsewhere
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

#install.packages("pdftools")
#options(java.parameters = "-Xmx12000m")
```

load library

``` r
library(rJava)      # Needed for tabulizer
library(tabulizer)  # Handy tool for PDF Scraping
library(tidyverse)  # Core data manipulation and visualization libraries
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(pdftools)
```

    ## Using poppler version 22.02.0

``` r
library(stringr)

knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
```

# Try scrape from the webpage and `pdftools` package –\> failed

## Download pdf and load file

``` r
url <- c("https://a002-epic.nyc.gov/api/files/6dd90109-dc57-e911-8161-005056b05749/download")
raw_text <- map(url, pdf_text)
```

## function to scrape data and clean

``` r
clean_table1 <- function(raw) {

  # split the single pages
  raw <- map(raw, ~ str_split(.x, "\\n") %>% 
    unlist())
  # Concatenate the split pages
  raw <- reduce(raw, c)
  
  #specify the start and end of the table data
  table_start <- stringr::str_which(tolower(raw), "parameter")
  table_end <- stringr::str_which(tolower(raw), "% bromofluorobenzene")
  table_end <- table_end[min(which(table_end > table_start))]
  
  #Build the table and remove special characters
  table <- raw[(table_start):(table_end)]
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
}

results <- map_df(raw_text, clean_table1)
head(results)
```

    ##                                                                              X..VOCs.by.TO.15.VOC.parameters.
    ## 1                                                                                Results of Chemical Analyses
    ## 2                  Laboratory data for soil, groundwater and soil vapor are summarized in Tables 2 through 9.
    ## 3          Laboratory data deliverables for all samples evaluated in this RIR are provided in digital form in
    ## 4                                                                                               Attachment E.
    ## 5 EBC                                                       1808 Middle Country Road       Phone 631.504.6000
    ## 6 Environmental Business Consultants                 13     Ridge, NY 11961                Fax   631.924.2870

# Use Download pdf file and `tabulizer` package

## Scrape whole dataset from 5 tables

``` r
#Extracting data from page 1185,186,188,189,191,192,194,195,197,198
soil_vapor <- extract_tables("data/2019-01-17.19TMP0033K.RIR Phase II Report.Revised 19TMP0033K.RIR Phase II Report - EBC V2.pdf.1.pdf",
             output = "data.frame",
             pages = c(185,186,188,189,191,192,194,195,197,198), 
             area = list(
                       c(352.7, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7),
                       c(352.7, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7),
                       c(352.7, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7),
                       c(352.7, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7),
                       c(352.7, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7)                       
             ), 
             columns = list(c(35, 177, 220, 260, 298, 342, 377, 407, 458, 506, 550, 581.7)),
             guess = FALSE
            )
```

## Report 1/2/3/4/5

Part a: first part of lab report

``` r
df1a = soil_vapor[[1]] %>% as_tibble()
df2a = soil_vapor[[3]] %>% as_tibble()
df3a = soil_vapor[[5]] %>% as_tibble()
df4a = soil_vapor[[7]] %>% as_tibble()
df5a = soil_vapor[[9]] %>% as_tibble()

# renaming all the column names
colnames(df1a) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")
colnames(df2a) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")
colnames(df3a) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")
colnames(df4a) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")
colnames(df5a) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")

# delete 1st row: Volatiles (TO15)
df1a = df1a[-1,]
df2a = df2a[-1,]
df3a = df3a[-1,]
df4a = df4a[-1,]
df5a = df5a[-1,]

#clean dataframe part a
df1a <-df1a %>% janitor::clean_names() %>% select(-delete) 
df2a <-df2a %>% janitor::clean_names() %>% select(-delete) 
df3a <-df3a %>% janitor::clean_names() %>% select(-delete) 
df4a <-df4a %>% janitor::clean_names() %>% select(-delete) 
df5a <-df5a %>% janitor::clean_names() %>% select(-delete) 
```

Part b: second part of lab report

``` r
df1b = soil_vapor[[2]] %>% as_tibble()
df2b = soil_vapor[[4]] %>% as_tibble()
df3b = soil_vapor[[6]] %>% as_tibble()
df4b = soil_vapor[[8]] %>% as_tibble()
df5b = soil_vapor[[10]] %>% as_tibble()
# renaming all the column names 
colnames(df1b) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")
colnames(df2b) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")
colnames(df3b) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")
colnames(df4b) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")
colnames(df5b) <- c("delete","parameter","ppbv","ppbv_RL","LOD_MDL","ug_m3","ug_m3_RL","LOD_MDL","date","by","dilution","not_certified")

# delete 41st row: QA/QC Surrogates
df1b = df1b[-41,]
df2b = df2b[-41,]
df3b = df3b[-41,]
df4b = df4b[-41,]
df5b = df5b[-41,]
#clean dataframe part b
df1b <-df1b %>% janitor::clean_names() %>% select(-delete) 
df2b <-df2b %>% janitor::clean_names() %>% select(-delete) 
df3b <-df3b %>% janitor::clean_names() %>% select(-delete) 
df4b <-df4b %>% janitor::clean_names() %>% select(-delete) 
df5b <-df5b %>% janitor::clean_names() %>% select(-delete)   
```

stack the first part and the second part together

``` r
# implementing the rbind function
sv_table_1 <- rbind(df1a, df1b)
sv_table_2 <- rbind(df2a, df2b)
sv_table_3 <- rbind(df3a, df3b)
sv_table_4 <- rbind(df4a, df4b)
sv_table_5 <- rbind(df5a, df5b)
```

## Add street address, borough and client ID

``` r
#Extracting data from page 1185,188,191,194,197
information <- extract_tables("data/2019-01-17.19TMP0033K.RIR Phase II Report.Revised 19TMP0033K.RIR Phase II Report - EBC V2.pdf.1.pdf",
             output = "data.frame",
             pages = c(185,188,191,194,197), 
             area = list(
                       c(288.1, 35, 337.6, 292.9), 
                       c(288.1, 35, 337.6, 292.9),
                       c(288.1, 35, 337.6, 292.9), 
                       c(288.1, 35, 337.6, 292.9),
                       c(288.1, 35, 337.6, 292.9)                      
             ), 
#             columns = list(c(35, 177, 220, 260, 298, 342, 377, 407, 458, 506, 550, 581.7)),
             guess = FALSE
            )
```

``` r
sv_1 <- sv_table_1 %>% 
  mutate(project_id = information[[1]][1,2],
         client_id = information[[1]][2,2]) %>% 
  separate(`project_id`, into = c("street_address", "borough"), sep = ", ") %>% 
  select(street_address, borough, client_id, everything())

sv_2 <- sv_table_2 %>% 
  mutate(project_id = information[[2]][1,2],
         client_id = information[[2]][2,2]) %>% 
  separate(`project_id`, into = c("street_address", "borough"), sep = ", ") %>% 
  select(street_address, borough, client_id, everything())

sv_3 <- sv_table_3 %>% 
  mutate(project_id = information[[3]][1,2],
         client_id = information[[3]][2,2]) %>% 
  separate(`project_id`, into = c("street_address", "borough"), sep = ", ") %>% 
  select(street_address, borough, client_id, everything())

sv_4 <- sv_table_4 %>% 
  mutate(project_id = information[[4]][1,2],
         client_id = information[[4]][2,2]) %>% 
  separate(`project_id`, into = c("street_address", "borough"), sep = ", ") %>% 
  select(street_address, borough, client_id, everything())

sv_5 <- sv_table_5 %>% 
  mutate(project_id = information[[5]][1,2],
         client_id = information[[5]][2,2]) %>% 
  separate(`project_id`, into = c("street_address", "borough"), sep = ", ") %>% 
  select(street_address, borough, client_id, everything())
```
