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

# Try to use the whole pdf –\> failed

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

# Use splited pdf

``` r
# PDF Scrape Tables
phoenix <- extract_tables(
    file   = "data/Soil_Vapor_Table.pdf", 
    method = "decide", 
    output = "data.frame")
```

``` r
# Pluck the first table in the list
endangered_species_raw_tbl <- phoenix %>% 
    pluck(1) %>% 
    as_tibble()

# Show first 6 rows
endangered_species_raw_tbl %>% head() %>% knitr::kable()
```

| X                                           | X.1            | X.2    | X.3   | X.4  | Phoenix.ID..CB90210   | X.5 |
|:--------------------------------------------|:---------------|:-------|:------|:-----|:----------------------|----:|
| Project ID: 742-848 FLUSHING AVE., BROOKLYN |                |        |       |      |                       |  NA |
| Client ID: 18SV2                            |                |        |       |      |                       |  NA |
|                                             | ppbv ppbv LOD/ | ug/m3  | ug/m3 | LOD/ |                       |  NA |
| Parameter                                   | Result RL MDL  | Result | RL    | MDL  | Date/Time By Dilution |  NA |
| Volatiles (TO15)                            |                |        |       |      |                       |  NA |
| 1,1,1,2-Tetrachloroethane                   | ND 0.146 0.146 | ND     | 1.00  | 1.00 | 11/07/18 KCA 1        |   1 |

``` r
#Extracting data from page 185 and 186
soil_vapor <- extract_tables("data/2019-01-17.19TMP0033K.RIR Phase II Report.Revised 19TMP0033K.RIR Phase II Report - EBC V2.pdf.1.pdf",
             output = "data.frame",
             pages = c(185,186,188,189,191,192,194,195,197,198), 
             area = list(
                       c(352.7, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7),
                       c(398.1, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7),
                       c(398.1, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7),
                       c(398.1, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7),
                       c(398.1, 35, 731, 581.7), 
                       c(80.4, 35, 640, 581.7)                       
             ), 
             columns = list(c(35, 177, 220, 260, 298, 342, 377, 407, 458, 506, 550, 581.7)),
             guess = FALSE
            )
#(top,left,bottom,right)
```

``` r
# area <- locate_areas("data/2019-01-17.19TMP0033K.RIR Phase II Report.Revised 19TMP0033K.RIR Phase II Report - EBC V2.pdf.1.pdf", pages = 185)
```

``` r
df1 = 
  soil_vapor[[1]] %>% 
  as_tibble()

# renaming all the column names of data frame 1
df1 <- setNames(df1, c("Delete","Parameter","ppbv","ppbv_RL","LOD/MDL","ug/m3","ug/m3_RL","LOD/MDL","Date","By","Dilution","Not_certified"))
```
