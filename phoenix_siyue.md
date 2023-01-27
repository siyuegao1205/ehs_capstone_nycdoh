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

## First type - With Text on Top

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
  select(street_address, everything())
```

**Problems:**

1.  Do not know how to get the project ID, and collection date info.  
2.  Some col names need to be fixed.
3.  If we want to pivot_wider…

## Second type - Without Text on Top

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
        select(street_address, everything())
        
    }
    
  }
    
}
```
