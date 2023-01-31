Phoenix Format Lab Report
================

``` r
phoenix = extract_tables(
    file   = "data/2019-01-17.19TMP0033K.RIR Phase II Report.Revised 19TMP0033K.RIR Phase II Report - EBC V2.pdf.1.pdf", 
    method = "decide", 
    output = "data.frame")

p = phoenix[[210]]
```

Tried to read the whole pdf in - the results seemed to be too chaotic
for any further extracting and cleaning.

``` r
soil_vapor = extract_tables(
    file   = "data/Soil_Vapor_Table.pdf", 
    method = "decide", 
    output = "data.frame")

title = soil_vapor[[1]] %>% as_tibble()
continue = soil_vapor[[2]] %>% as_tibble()
```

If we manually split the pdf before reading in, the results seemed to be
more promising.

<br>

There are two types of tables (with/without text on top).

## First Type - With Text on Top

``` r
df = soil_vapor[[1]] %>%
  as_tibble()

df = df %>% 
  rbind(1:ncol(df), df) %>% 
  slice(-(1:31)) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  separate(`1`, into = c("chemical", "client_sample_id"), sep = "\\: ") %>% 
  separate(`2`, into = c("a", "b", "c"), sep = " ") %>% 
  separate(`6`, into = c("d", "e", "f"), sep = " ") %>% 
  mutate(
    client_sample_id = ifelse(is.na(client_sample_id), client_sample_id[2], client_sample_id),
    street_address = client_sample_id[1]
  ) %>% 
  slice(-c(1:2))
  
df[1,c(3:11)] <- as.list(paste(df[1,c(3:11)], df[2,c(3:11)]))
df[-2,]
```

    ## # A tibble: 28 × 13
    ##    chemical  clien…¹ a     b     c     `3`   `4`   `5`   d     e     f       `7`
    ##    <chr>     <chr>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <int>
    ##  1 ""        18SV2   "ppb… ppbv… LOD/… "ug/… "ug/… "LOD… " Da… NA By NA D…    NA
    ##  2 "Volatil… 18SV2   ""    <NA>  <NA>  ""    ""    ""    ""    <NA>  <NA>     NA
    ##  3 "1,1,1,2… 18SV2   "ND"  0.146 0.146 "ND"  "1.0… "1.0… "11/… KCA   1         1
    ##  4 "1,1,1-T… 18SV2   "ND"  0.183 0.183 "ND"  "1.0… "1.0… "11/… KCA   1        NA
    ##  5 "1,1,2,2… 18SV2   "ND"  0.146 0.146 "ND"  "1.0… "1.0… "11/… KCA   1        NA
    ##  6 "1,1,2-T… 18SV2   "ND"  0.183 0.183 "ND"  "1.0… "1.0… "11/… KCA   1        NA
    ##  7 "1,1-Dic… 18SV2   "ND"  0.247 0.247 "ND"  "1.0… "1.0… "11/… KCA   1        NA
    ##  8 "1,1-Dic… 18SV2   "ND"  0.051 0.051 "ND"  "0.2… "0.2… "11/… KCA   1        NA
    ##  9 "1,2,4-T… 18SV2   "0.2… 0.135 0.135 "2.0… "1.0… "1.0… "11/… KCA   1        NA
    ## 10 "1,2,4-T… 18SV2   "0.9… 0.204 0.204 "4.7… "1.0… "1.0… "11/… KCA   1        NA
    ## # … with 18 more rows, 1 more variable: street_address <chr>, and abbreviated
    ## #   variable name ¹​client_sample_id

``` r
df_sub = df %>% 
  select(3:11) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  janitor::clean_names()

df = df %>% 
  select(-c(3:11)) %>% 
  slice(-1)

df = cbind(df, df_sub)

df = df %>% 
  select(-`7`) %>% 
  slice(-c(1:2)) %>% 
  mutate(
    client_sample_id = str_sub(client_sample_id, start = 3)
  ) %>% 
  select(street_address, client_sample_id, everything()) %>% 
  separate(street_address, into = c("street_address", "borough"), sep = ", ") %>% 
  rename(
    by = na_by,
    dilution = na_dilution
  )
```

**Problems:**

1.  Do not know how to get the project ID, and collection date info.  
2.  Some col names need to be fixed.
3.  If we want to pivot_wider…

## Second Type - Without Text on Top

``` r
df_cont = soil_vapor[[2]] %>%
  as_tibble() %>% 
  select(-X.4)

df_cont = setNames(rbind(names(df_cont), df_cont), names(df_cont))

df_cont[1,c(2:10)] <- as.list(paste(df_cont[1,c(2:10)], df_cont[2,c(2:10)]))
df_cont[-2,]
```

    ## # A tibble: 43 × 10
    ##    X                   ppbv  ppbv.1 LOD.  ug.m3 ug.m3.1 LOD..1 X.1   X.2   X.3  
    ##    <chr>               <chr> <chr>  <chr> <chr> <chr>   <chr>  <chr> <chr> <chr>
    ##  1 X                   ppbv… ppbv.… LOD.… ug.m… ug.m3.… LOD..… X.1 … X.2 … X.3 …
    ##  2 Bromodichlorometha… ND    0.149  0.149 ND    1.00    1.00   11/0… KCA   1    
    ##  3 Bromoform           ND    0.097  0.097 ND    1.00    1.00   11/0… KCA   1    
    ##  4 Bromomethane        ND    0.258  0.258 ND    1.00    1.00   11/0… KCA   1    
    ##  5 Carbon Disulfide    0.492 0.321  0.321 1.53  1.00    1.00   11/0… KCA   1    
    ##  6 Carbon Tetrachlori… 0.038 0.032  0.032 0.24  0.20    0.20   11/0… KCA   1    
    ##  7 Chlorobenzene       ND    0.217  0.217 ND    1.00    1.00   11/0… KCA   1    
    ##  8 Chloroethane        ND    0.379  0.379 ND    1.00    1.00   11/0… KCA   1    
    ##  9 Chloroform          9.99  0.205  0.205 48.7  1.00    1.00   11/0… KCA   1    
    ## 10 Chloromethane       ND    0.485  0.485 ND    1.00    1.00   11/0… KCA   1    
    ## # … with 33 more rows

``` r
df_cont = df_cont %>% 
  mutate(
    X = str_replace(X, "X", "chemical"),
    ppbv.1 = str_replace(ppbv.1, ".1", ""),
    ug.m3.1 = str_replace(ug.m3.1, ".1", ""),
    LOD..1 = str_replace(LOD..1, ".1", ""),
    X.1 = str_replace(X.1, "X.1 ", ""),
    X.2 = str_replace(X.2, "X.2 ", ""),
    X.3 = str_replace(X.3, "X.3 ", ""),
  ) %>% 
  slice(-2) %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  mutate(
    street_address = NA_character_,
    borough = NA_character_,
    client_sample_id = NA_character_
  ) %>% 
  select(street_address, borough, client_sample_id, everything())
```

## Binding the separated parts of reports together

``` r
df_full = rbind(df, df_cont)

df_full = df_full %>% 
  mutate(
    street_address = ifelse(is.na(street_address), street_address[1], street_address),
    borough = ifelse(is.na(borough), borough[1], borough),
    client_sample_id = ifelse(is.na(client_sample_id), client_sample_id[1], client_sample_id)
  )

df_full %>% 
  head() %>% 
  knitr::kable()
```

| street_address        | borough  | client_sample_id | chemical                  | ppbv_result | ppbv_rl | lod_mdl | ug_m3_result | ug_m3_rl | lod_mdl_2 | date_time | by  | dilution |
|:----------------------|:---------|:-----------------|:--------------------------|:------------|:--------|:--------|:-------------|:---------|:----------|:----------|:----|:---------|
| 742-848 FLUSHING AVE. | BROOKLYN | SV2              | 1,1,1,2-Tetrachloroethane | ND          | 0.146   | 0.146   | ND           | 1.00     | 1.00      | 11/07/18  | KCA | 1        |
| 742-848 FLUSHING AVE. | BROOKLYN | SV2              | 1,1,1-Trichloroethane     | ND          | 0.183   | 0.183   | ND           | 1.00     | 1.00      | 11/07/18  | KCA | 1        |
| 742-848 FLUSHING AVE. | BROOKLYN | SV2              | 1,1,2,2-Tetrachloroethane | ND          | 0.146   | 0.146   | ND           | 1.00     | 1.00      | 11/07/18  | KCA | 1        |
| 742-848 FLUSHING AVE. | BROOKLYN | SV2              | 1,1,2-Trichloroethane     | ND          | 0.183   | 0.183   | ND           | 1.00     | 1.00      | 11/07/18  | KCA | 1        |
| 742-848 FLUSHING AVE. | BROOKLYN | SV2              | 1,1-Dichloroethane        | ND          | 0.247   | 0.247   | ND           | 1.00     | 1.00      | 11/07/18  | KCA | 1        |
| 742-848 FLUSHING AVE. | BROOKLYN | SV2              | 1,1-Dichloroethene        | ND          | 0.051   | 0.051   | ND           | 0.20     | 0.20      | 11/07/18  | KCA | 1        |

Some mistakes induced during extracting process were inspected, probably
due to the italic in the original source - need to check!

**Things need to figure out:**  
1. How to iterate on downloading and slicing the pdfs … and also for
different samples  
2. Other information related to the site, such as project ID, collection
date, is still hard to extract

## Function (Not Finished)

``` r
phoenix = function(x){
  
  for (i in 1:10) {
    
    if (ncol(soil_vapor[[i]]) == 7) {
      
      df = soil_vapor[[i]] %>%
        as_tibble()
      
      df = df %>% 
        rbind(1:ncol(df), df) %>% 
        slice(-(1:31)) %>% 
        janitor::row_to_names(row_number = 1) %>% 
        separate(`1`, into = c("chemical", "client_sample_id"), sep = "\\: ") %>% 
        separate(`2`, into = c("a", "b", "c"), sep = " ") %>% 
        separate(`6`, into = c("d", "e", "f"), sep = " ") %>% 
        mutate(
          client_sample_id = ifelse(is.na(client_sample_id), client_sample_id[2], client_sample_id),
          street_address = client_sample_id[1]
        ) %>% 
        slice(-c(1:2))
        
      df[1,c(3:11)] <- as.list(paste(df[1,c(3:11)], df[2,c(3:11)]))
      df[-2,]
      
      df_sub = df %>% 
        select(3:11) %>% 
        janitor::row_to_names(row_number = 1) %>% 
        janitor::clean_names()
      
      df = df %>% 
        select(-c(3:11)) %>% 
        slice(-1)
      
      df = cbind(df, df_sub)
      
      df = df %>% 
        select(-`7`) %>% 
        slice(-c(1:2)) %>% 
        mutate(
          client_sample_id = str_sub(client_sample_id, start = 3)
          ) %>% 
        select(street_address, client_sample_id, everything()) %>% 
        
        
    }
    
  }
    
}
```
