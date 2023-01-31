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
