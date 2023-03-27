data_clean_Sissi
================
2023-1-31

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(dplyr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(openxlsx)
```

# lab list

``` r
tibble(
    files = list.files("./data/data_from_M/phoenix/"),
    path = str_c("./data/data_from_M/phoenix/", files)
  )
```

    ## # A tibble: 74 × 2
    ##    files                              path                                      
    ##    <chr>                              <chr>                                     
    ##  1 ~$SV 1044 Bedford Ave.xlsx         ./data/data_from_M/phoenix/~$SV 1044 Bedf…
    ##  2 ~$SV 1570 60th Street.xlsx         ./data/data_from_M/phoenix/~$SV 1570 60th…
    ##  3 ~$SV 1875 Atlantic Ave.xlsx        ./data/data_from_M/phoenix/~$SV 1875 Atla…
    ##  4 ~$SV 902-908 Flushing Ave.xlsx     ./data/data_from_M/phoenix/~$SV 902-908 F…
    ##  5 ~$SV 948 Myrtle Ave.xlsx           ./data/data_from_M/phoenix/~$SV 948 Myrtl…
    ##  6 SV 1-37 Forrest Street.xlsx        ./data/data_from_M/phoenix/SV 1-37 Forres…
    ##  7 SV 1026-1030 Manhattan Ave.xlsx    ./data/data_from_M/phoenix/SV 1026-1030 M…
    ##  8 SV 1044 Bedford Ave.xlsx           ./data/data_from_M/phoenix/SV 1044 Bedfor…
    ##  9 SV 1048 Manhattan Ave.xlsx         ./data/data_from_M/phoenix/SV 1048 Manhat…
    ## 10 SV 1050-1066 Manhattan Avenue.xlsx ./data/data_from_M/phoenix/SV 1050-1066 M…
    ## # … with 64 more rows

``` r
project_id_list = read_csv("data/brooklyn_cvcp_list.csv")
```

    ## Rows: 431 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (6): project_id, name, borough, address, block_lot, project_class
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Notes

**Errors from Excel**

didn’t do: SV 308 North 7th Street

# Code For Each Lab

## SV 67 Duffield Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 67 Duffield Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 2",
                        ifelse(row.names(df1) %in% 68:134, "SG 5",
                               ifelse(row.names(df1) %in% 135:201, "SG 4",
                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df2 <- df %>% 
  slice(c(4:5)) %>% #may change
  select(1,2) 


# Syntax using colnames()
colnames(df2)[1] = "id"
colnames(df2)[2] = "client_id"

#df3 <- df2 %>% 
#  separate(id, into = c("id", "address"), sep = "Project ID:       ") #may change

#address <- strsplit(as.character(df2$id),'Project ID:       ') 
#do.call(rbind, address)
#df3 <- data.frame(df2$id, df2$client_id, do.call(rbind, address))

#df3 <- df3 %>% 
 # select(4)

# colnames(df3)[1] = "address"


df4 <-df1 %>% 
    mutate(lab = "phoenix",
           street_address = "SV 67 Duffield Street") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>% 
  select(street_address, sample_id, everything())



#df4 <-df1 %>% 
#    mutate(project_id = df3[2,1],
#         client_id = df3[1,2],
 #        project_id = project_id$address,
 #        client_id = client_id$client_id) %>% 
 # select(project_id, client_id, everything())

phoenix_1 <- df4 
write_csv(phoenix_1, "./data/cleandata/phoenix/SV 67 Duffield Street.csv")
```

## SV 81 McGuinness Blvd

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 81 McGuinness Blvd.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV1",
                        ifelse(row.names(df1) %in% 68:134, "SV3","SV2"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 81 McGuinness Blvd") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_2 <- df4 
write_csv(phoenix_2, "./data/cleandata/phoenix/SV 81 McGuinness Blvd.csv")
```

## SV 98 3rd Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 98 3rd Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV-2",
                        ifelse(row.names(df1) %in% 68:134, "SV-1",
                               ifelse(row.names(df1) %in% 135:201, "SV-3","SV-4")))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 98 3rd Avenue") %>% 
  select(-lod_mdl, -lod_mdl_2, -reference) %>%    
  select(street_address, sample_id, everything())


phoenix_3 <- df4 
write_csv(phoenix_3, "./data/cleandata/phoenix/SV 98 3rd Avenue.csv")
```

## SV 113 Hamilton Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 113 Hamilton Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV-1",
                        ifelse(row.names(df1) %in% 68:135, "SV-2",
                               ifelse(row.names(df1) %in% 136:203, "SV-4",
                                  ifelse(row.names(df1) %in% 204:271, "SV-5","SV-3"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 113 Hamilton Ave") %>% 
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_4 <- df4 
write_csv(phoenix_4, "./data/cleandata/phoenix/SV 113 Hamilton Ave.csv")
```

## SV 137 Frost Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 137 Frost Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG1",
                        ifelse(row.names(df1) %in% 68:134, "SG2",
                               ifelse(row.names(df1) %in% 135:201, "SG3",
                                   ifelse(row.names(df1) %in% 202:268, "SG4","SG5"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene



df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 137 Frost Street") %>% 
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_5 <- df4 
write_csv(phoenix_5, "./data/cleandata/phoenix/SV 137 Frost Street.csv")
```

## SV 183 McGuinness Blvd

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 183 McGuinness Blvd.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV3",
                        ifelse(row.names(df1) %in% 68:134, "SV2","SV1"))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 183 McGuinness Blvd") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_6 <- df4 
write_csv(phoenix_6, "./data/cleandata/phoenix/SV 183 McGuinness Blvd.csv")
```

## SV 288-292 Union Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 288-292 Union Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG2",
                        ifelse(row.names(df1) %in% 68:135, "SG3","SG1"))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 288-292 Union Ave") %>% 
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_7 <- df4 
write_csv(phoenix_7, "./data/cleandata/phoenix/SV 288-292 Union Ave.csv")
```

## SV 633 Marcy Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 633 Marcy Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV2",
                        ifelse(row.names(df1) %in% 68:134, "SV1",
                               ifelse(row.names(df1) %in% 135:201, "SV3",
                                      ifelse(row.names(df1) %in% 202:268, "SS1",
                                             ifelse(row.names(df1) %in% 269:335, "SV5",
                                                    ifelse(row.names(df1) %in% 336:402, "SV4","SS2"))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 633 Marcy Avenue") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_8 <- df4 
write_csv(phoenix_8, "./data/cleandata/phoenix/SV 633 Marcy Avenue.csv")
```

## SV 842-846 Flushing Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 842-846 Flushing Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...1`
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "18SV2",
                        ifelse(row.names(df1) %in% 68:134, "18SV5",
                               ifelse(row.names(df1) %in% 135:201, "18SV3",
                                   ifelse(row.names(df1) %in% 202:268, "18SV1","18SV4"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 842-846 Flushing Ave") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_9 <- df4 
write_csv(phoenix_9, "./data/cleandata/phoenix/SV 842-846 Flushing Ave.csv")
```

## SV 922 Myrtle Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 922 Myrtle Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`

``` r
df1a <- df %>% 
  slice(c(1:156)) %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter)) %>% 
    mutate(dilution = NA) %>% 
  select(parameter, ppbv_result, ppbv_rl, lod_mdl, ug_m3_result, ug_m3_rl, lod_mdl_2, date_time, by, reference, dilution)
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(157:731)) %>% 
  clean_names() %>% 
  row_to_names(row_number = 12) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter)) %>% 
  mutate(lod_mdl = NA, lod_mdl_2 = NA, dilution = NA) %>% 
  select(parameter, ppbv_result, ppbv_rl, lod_mdl, ug_m3_result, ug_m3_rl, lod_mdl_2, date_time, by, reference, dilution)
```

    ## Warning: Row 12 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(c(732:815)) %>% 
  clean_names() %>% 
  row_to_names(row_number = 11) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter)) %>% 
    mutate(reference = NA) %>% 
  select(parameter, ppbv_result, ppbv_rl, lod_mdl, ug_m3_result, ug_m3_rl, lod_mdl_2, date_time, by, reference, dilution)
```

    ## Warning: Row 11 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
# Stack three datasets df1a, df1b, and df1c into one dataset df1
df1 <- rbind(df1a, df1b, df1c)

df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV1",
                        ifelse(row.names(df1) %in% 68:135, "SV7",
                               ifelse(row.names(df1) %in% 136:205, "SV8",
                                    ifelse(row.names(df1) %in% 206:275, "SV6",
                                        ifelse(row.names(df1) %in% 276:345, "SV9",
                                               ifelse(row.names(df1) %in% 346:415, "SV3",
                                                      ifelse(row.names(df1) %in% 416:485, "SV4",
                                                              ifelse(row.names(df1) %in% 486:555, "SV5",
                                                             ifelse(row.names(df1) %in% 556:625, "SV2","SV10")))))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 922 Myrtle Avenue") %>% 
  select(-lod_mdl, -lod_mdl_2, -reference) %>%  
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_10 <- df4 
write_csv(phoenix_10, "./data/cleandata/phoenix/SV 922 Myrtle Avenue.csv")
```

## SV 975 Manhattan Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 975 Manhattan Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...1`
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`
    ## • `` -> `...33`
    ## • `` -> `...34`
    ## • `` -> `...35`
    ## • `` -> `...36`
    ## • `` -> `...37`
    ## • `` -> `...38`
    ## • `` -> `...39`
    ## • `` -> `...40`
    ## • `` -> `...41`

``` r
df1a <- df %>% 
  slice(c(1:156)) %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(157:239)) %>% 
  row_to_names(row_number = 11) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(lod_mdl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 11 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1 <- rbind(df1a, df1b)
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS3",
                        ifelse(row.names(df1) %in% 68:134, "SS2","SS1"))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 975 Manhattan Ave") %>% 
  select(-lod_mdl, -lod_mdl_2, -reference) %>%    
  select(street_address, sample_id, everything())

phoenix_11 <- df4 
write_csv(phoenix_11, "./data/cleandata/phoenix/SV 975 Manhattan Ave.csv")
```

## SV 1026-1030 Manhattan Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1026-1030 Manhattan Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV1",
                        ifelse(row.names(df1) %in% 68:134, "SV4",
                               ifelse(row.names(df1) %in% 135:201, "SV2","SV3")))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1026-1030 Manhattan Ave") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_12 <- df4 
write_csv(phoenix_12, "./data/cleandata/phoenix/SV 1026-1030 Manhattan Ave.csv")
```

## SV 1048 Manhattan Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1048 Manhattan Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS1",
                        ifelse(row.names(df1) %in% 68:134, "SS2","SV1"))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1048 Manhattan Ave") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_13 <- df4 
write_csv(phoenix_13, "./data/cleandata/phoenix/SV 1048 Manhattan Ave.csv")
```

## SV 1096 Broadway

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1096 Broadway.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV3",
                        ifelse(row.names(df1) %in% 68:134, "SV2",
                               ifelse(row.names(df1) %in% 135:201, "SV1","SV4")))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1096 Broadway") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_14 <- df4 
write_csv(phoenix_14, "./data/cleandata/phoenix/SV 1096 Broadway.csv")
```

## SV 1457-1471 Flatbush Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1457-1471 Flatbush Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS1",
                        ifelse(row.names(df1) %in% 68:134, "SV1",
                               ifelse(row.names(df1) %in% 135:201, "SV2",
                                      ifelse(row.names(df1) %in% 202:268, "SS2",
                                             ifelse(row.names(df1) %in% 269:335, "SS3",
                                   ifelse(row.names(df1) %in% 336:402, "SV4","SV3"))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1457-1471 Flatbush Avenue") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    select(street_address, sample_id, everything())


phoenix_15 <- df4 
write_csv(phoenix_15, "./data/cleandata/phoenix/SV 1457-1471 Flatbush Avenue.csv")
```

## SV 1458-1460 Flatbush Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1458-1460 Flatbush Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV 3",
                        ifelse(row.names(df1) %in% 68:134, "SV 1",
                               ifelse(row.names(df1) %in% 135:201, "SV 2",
                                   ifelse(row.names(df1) %in% 202:268, "SV 4","SV 5"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1458-1460 Flatbush Avenue") %>% 
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_16 <- df4 
write_csv(phoenix_16, "./data/cleandata/phoenix/SV 1458-1460 Flatbush Avenue.csv")
```

## SV 1516 Fulton Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1516 Fulton Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG4",
                        ifelse(row.names(df1) %in% 68:134, "SG2",
                               ifelse(row.names(df1) %in% 135:201, "IA1",
                                      ifelse(row.names(df1) %in% 202:268, "SG1",
                                  ifelse(row.names(df1) %in% 269:335, "OA1","SG3")))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1516 Fulton Street") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    select(street_address, sample_id, everything())


phoenix_17 <- df4 
write_csv(phoenix_17, "./data/cleandata/phoenix/SV 1516 Fulton Street.csv")
```

## SV 2437 Pitkin Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 2437 Pitkin Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV1",
                        ifelse(row.names(df1) %in% 68:134, "SV2",
                               ifelse(row.names(df1) %in% 135:201, "SV3","SV4")))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 2437 Pitkin Ave") %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_18 <- df4 
write_csv(phoenix_18, "./data/cleandata/phoenix/SV 2437 Pitkin Ave.csv")
```

## SV 47 Walton Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 47 Walton Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS-1",
                        ifelse(row.names(df1) %in% 68:134, "SS-3",
                               ifelse(row.names(df1) %in% 135:201, "SS-2","SS-4")))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 47 Walton Street") %>% 
      select(-reference) %>%  
  select(street_address, sample_id, everything())


phoenix_19 <- df4 
write_csv(phoenix_19, "./data/cleandata/phoenix/SV 47 Walton Street.csv")
```

## SV 71 & 76 North 7th Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 71 & 76 North 7th Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV-2",
                        ifelse(row.names(df1) %in% 68:134, "IA-1",
                               ifelse(row.names(df1) %in% 135:201, "SV-1","SV-3")))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 71 & 76 North 7th Street") %>% 
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_20 <- df4 
write_csv(phoenix_20, "./data/cleandata/phoenix/SV 71 & 76 North 7th Street.csv")
```

## SV 141 Metropolitan Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 141 Metropolitan Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`

``` r
df1 <- df %>% 
  row_to_names(row_number = 7) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(lod_mdl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 7 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS3",
                        ifelse(row.names(df1) %in% 68:135, "SS2", "SS1"))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 141 Metropolitan Avenue",
           ppbv_rl = NA) %>% 
  select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_21 <- df4 
write_csv(phoenix_21, "./data/cleandata/phoenix/SV 141 Metropolitan Avenue.csv")
```

## SV 241-245 4th Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 241-245 4th Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV-3",
                        ifelse(row.names(df1) %in% 68:135, "SV-5",
                               ifelse(row.names(df1) %in% 136:203, "SV-1",
                                   ifelse(row.names(df1) %in% 204:271, "SV-2","SV-4"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 241-245 4th Avenue") %>% 
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_22 <- df4 
write_csv(phoenix_22, "./data/cleandata/phoenix/SV 241-245 4th Avenue.csv")
```

\##SV 415-419 Marcy Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 415-419 Marcy Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 2",
                        ifelse(row.names(df1) %in% 68:134, "SG 1",
                               ifelse(row.names(df1) %in% 135:201, "SG 3",
                                 ifelse(row.names(df1) %in% 202:268, "SG 4","SG 5"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 415-419 Marcy Avenue") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_23 <- df4 
write_csv(phoenix_23, "./data/cleandata/phoenix/SV 415-419 Marcy Avenue.csv")
```

## SV 578 5th Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 578 5th Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV-1",
                        ifelse(row.names(df1) %in% 68:134, "SV-2",
                               ifelse(row.names(df1) %in% 135:201, "SV-3","AMB")))
 #                                  ifelse(row.names(df1) %in% 202:268, "SG3","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 578 5th Avenue") %>%
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_24 <- df4 
write_csv(phoenix_24, "./data/cleandata/phoenix/SV 578 5th Avenue.csv")
```

## SV 609-619 4th Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 609-619 4th Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV-1",
                        ifelse(row.names(df1) %in% 68:134, "SV-2",
                               ifelse(row.names(df1) %in% 135:201, "SV-3",
                                      ifelse(row.names(df1) %in% 202:268, "SV-4",
                                            ifelse(row.names(df1) %in% 269:335, "SV-5",
                                                  ifelse(row.names(df1) %in% 336:402, "Indoor Air","Ourdoor Air"))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 609-619 4th Avenue") %>% 
      select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_25 <- df4 
write_csv(phoenix_25, "./data/cleandata/phoenix/SV 609-619 4th Avenue.csv")
```

## SV 814 Bedford Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 814 Bedford Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG5",
                        ifelse(row.names(df1) %in% 68:134, "SG4",
                               ifelse(row.names(df1) %in% 135:201, "SG2",
                                  ifelse(row.names(df1) %in% 202:268, "SG6","SG1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 814 Bedford Avenue") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_26 <- df4 
write_csv(phoenix_26, "./data/cleandata/phoenix/SV 814 Bedford Avenue.csv")
```

## SV 1050-1066 Manhattan Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1050-1066 Manhattan Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 6",
                        ifelse(row.names(df1) %in% 68:134, "SG 7",
                               ifelse(row.names(df1) %in% 135:201, "SG 5",
                                  ifelse(row.names(df1) %in% 202:268, "SG 1","SG 4"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1050-1066 Manhattan Avenue") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_27 <- df4 
write_csv(phoenix_27, "./data/cleandata/phoenix/SV 1050-1066 Manhattan Avenue.csv")
```

## SV 1353 Flatbush Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1353 Flatbush Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1a <- df %>% 
  slice(c(1:34)) %>% 
  row_to_names(row_number =7) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 7 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(35:78)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(c(79:115)) %>% 
  row_to_names(row_number =10) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 10 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1d <- df %>% 
  slice(c(116:160)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1e <- df %>% 
  slice(c(161:196)) %>% 
  row_to_names(row_number =9) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 9 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1f <- df %>% 
  slice(c(197:241)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1g <- df %>% 
  slice(c(242:277)) %>% 
  row_to_names(row_number =9) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 9 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1h <- df %>% 
  slice(c(278:322)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1i <- df %>% 
  slice(c(323:358)) %>% 
  row_to_names(row_number =9) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 9 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1j <- df %>% 
  slice(c(359:404)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1 <- rbind(df1a, df1b, df1c, df1d, df1e, df1f, df1g, df1h, df1i, df1j)
df1$date_time <- convertToDateTime(df1$date_time)


row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "Outdoor Air",
                        ifelse(row.names(df1) %in% 68:134, "SV-4",
                               ifelse(row.names(df1) %in% 135:201, "SV-3",
                                  ifelse(row.names(df1) %in% 202:268, "SV-2","SV-1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1353 Flatbush Avenue") %>%     select(-dilution) %>%  
  select(street_address, sample_id, everything())


phoenix_28 <- df4 
write_csv(phoenix_28, "./data/cleandata/phoenix/SV 1353 Flatbush Avenue.csv")
```

## SV 1-37 Forrest Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1-37 Forrest Street.xlsx") %>% 
  janitor::clean_names()
```

    ## Warning: Expecting logical in U1603 / R1603C21: got '12:44
    ## 16:54'

    ## Warning: Expecting logical in H1604 / R1604C8: got 'Laboratory Data'

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")

df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "BL 3139 SG-3",
                        ifelse(row.names(df1) %in% 68:134, "BL 3139 SG-4",
                               ifelse(row.names(df1) %in% 135:201, "BL 3141 SG-6",
                                      ifelse(row.names(df1) %in% 202:268, "BL 3139 SG-2",
                                            ifelse(row.names(df1) %in% 269:335, "BL 3141 SG-4",
                                                  ifelse(row.names(df1) %in% 336:402, "BL 3139 SG-1",
                                                         ifelse(row.names(df1) %in% 403:469, "BL 3141 SG-2",
                                                            ifelse(row.names(df1) %in% 470:536, "BL 3141 SG-7", 
                                                                       ifelse(row.names(df1) %in% 537:603, "BL 3141 SG-1",
                                                                              ifelse(row.names(df1) %in% 604:670, "BL 3141 SG-5",
                                                                                     ifelse(row.names(df1) %in% 671:737, "BL 3139 SG-3",
                                                                                            ifelse(row.names(df1) %in% 738:804, "BL 3139 SG-4",
                                                                                                   ifelse(row.names(df1) %in% 805:871, "BL 3141 SG-6",
                                                                                                          ifelse(row.names(df1) %in% 872:938, "BL 3139 SG-2",
                                                                                                                 ifelse(row.names(df1) %in% 939:1005, "BL 3141 SG-4",
                                                                                                                        ifelse(row.names(df1) %in% 1006:1072, "BL 3139 SG-1",
                                                                                                                               ifelse(row.names(df1) %in% 1073:1139, "BL 3141 SG-2",
                                                                                                                                      ifelse(row.names(df1) %in% 1140:1206, "BL 3141 SG-7",
                                                                                                                                             ifelse(row.names(df1) %in% 1207:1273, "BL 3141 SG-1",
                                                                                                                                                    ifelse(row.names(df1) %in% 1274:1340, "BL 3141 SG-5", "SG 1-20-15"))))))))))))))))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene

df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1-37 Forrest Street") %>% 
#  select(-lod_mdl, -lod_mdl_2, -dilution) %>%
      select(-reference) %>%  
  select(street_address, sample_id, everything())


phoenix_29 <- df4 
write_csv(phoenix_29, "./data/cleandata/phoenix/SV 1-37 Forrest Street.csv")
```

## SV 20 Marcy Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 20 Marcy Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 5",
                        ifelse(row.names(df1) %in% 68:134, "HOPE SG 3",
                               ifelse(row.names(df1) %in% 135:201, "SG 1",
                                      ifelse(row.names(df1) %in% 202:268, "SG 4",
                                            ifelse(row.names(df1) %in% 269:335, "SG 2",
                                                  ifelse(row.names(df1) %in% 336:402, "SG 3",
                                                         ifelse(row.names(df1) %in% 403:469, "SG 6",
                                                            ifelse(row.names(df1) %in% 470:536, "HOPE SG 1","HOPE SG 2"))))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 20 Marcy Ave") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>% 
  select(street_address, sample_id, everything())


phoenix_30 <- df4 
write_csv(phoenix_30, "./data/cleandata/phoenix/SV 20 Marcy Ave.csv")
```

## SV 79 Clay Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 79 Clay Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
    filter(if_any(by, ~ !(.x %in% c("by", NA)))) %>% 
  filter(if_any(by, ~ !(.x %in% c("By", NA)))) %>% 
  mutate(date_time = 41541) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG-1",
                        ifelse(row.names(df1) %in% 68:134, "SG-2","SG-3"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 79 Clay Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_31 <- df4 
write_csv(phoenix_31, "./data/cleandata/phoenix/SV 79 Clay Street.csv")
```

## SV 79-97 4th Avenue and 82 St. Marks Place

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 79-97 4th Avenue and 82 St. Marks Place.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) 
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1<-data.frame(df1)
# df1$parameter[df1$parameter == "QA/QC Surrogates/Internals\r\n% Bromofluorobenzene"] <- "% Bromofluorobenzene"

# df1 %>% mutate(parameter = replace(parameter, 67, "% Bromofluorobenzene"))
df1$parameter[67] <- "% Bromofluorobenzene"
df1$parameter[141] <- "% Bromofluorobenzene"
df1$parameter[211] <- "% Bromofluorobenzene"
df1$parameter[281] <- "% Bromofluorobenzene"
df1$parameter[355] <- "% Bromofluorobenzene"

df1$date_time <- convertToDateTime(df1$date_time)

 row_numbers <- which(df1$parameter == "1,1,1,2-Tetrachloroethane")

df1$sample_id <- ifelse(row.names(df1) %in% 1:74, "SG8",
                        ifelse(row.names(df1) %in% 75:144, "SS2",
                               ifelse(row.names(df1) %in% 145:214, "SS3",
                                  ifelse(row.names(df1) %in% 215:288, "SG9","SS1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 79-97 4th Avenue and 82 St. Marks Place") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_32 <- df4 
write_csv(phoenix_32, "./data/cleandata/phoenix/SV 79-97 4th Avenue and 82 St. Marks Place.csv")
```

## SV 80-82 Ainslie

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 80-82 Ainslie.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "indoor",
                        ifelse(row.names(df1) %in% 68:134, "SG4",
                               ifelse(row.names(df1) %in% 135:201, "SG1",
                                  ifelse(row.names(df1) %in% 202:268, "SG2",
                                          ifelse(row.names(df1) %in% 269:335, "SG3",
                                                  ifelse(row.names(df1) %in% 336:402, "SG5","outdoor"))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 80-82 Ainslie") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_33 <- df4 
write_csv(phoenix_33, "./data/cleandata/phoenix/SV 80-82 Ainslie.csv")
```

## SV 125 Borinquen Place

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 125 Borinquen Place.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
    rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
  filter(if_any(by, ~ !(.x %in% c("by", NA)))) %>% 
  filter(if_any(by, ~ !(.x %in% c("By", NA)))) %>% 
  mutate(date_time = 41939) %>% 
  mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG1",
                        ifelse(row.names(df1) %in% 68:134, "SG2",
                               ifelse(row.names(df1) %in% 135:201, "SG3",
                                  ifelse(row.names(df1) %in% 202:268, "SG4","SG5"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 125 Borinquen Place") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_34 <- df4 
write_csv(phoenix_34, "./data/cleandata/phoenix/SV 125 Borinquen Place.csv")
```

## SV 153 Lorimer Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 153 Lorimer Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
      rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
    filter(if_any(by, ~ !(.x %in% c("by", NA)))) %>% 
  filter(if_any(by, ~ !(.x %in% c("By", NA)))) %>% 
  mutate(date_time = 41521) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG1",
                        ifelse(row.names(df1) %in% 68:134, "SG2",
                               ifelse(row.names(df1) %in% 135:201, "SG3","SG4")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 153 Lorimer Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_35 <- df4 
write_csv(phoenix_35, "./data/cleandata/phoenix/SV 153 Lorimer Street.csv")
```

## SV 291 Metropolitan Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 291 Metropolitan Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1a <- df %>% 
  slice(c(1:28)) %>% 
  row_to_names(row_number =6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(35:75)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(c(76:115)) %>% 
  row_to_names(row_number =12) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 12 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1d <- df %>% 
  slice(c(116:156)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1e <- df %>% 
  slice(c(157:196)) %>% 
  row_to_names(row_number =12) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 12 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1f <- df %>% 
  slice(c(197:237)) %>%
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1g <- df %>% 
  slice(c(238:277)) %>% 
  row_to_names(row_number =12) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 12 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1h <- df %>% 
  slice(c(278:318)) %>%
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1i <- df %>% 
  slice(c(319:358)) %>% 
  row_to_names(row_number =12) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 12 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1j <- df %>% 
  slice(c(359:399)) %>%
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1k <- df %>% 
  slice(c(400:439)) %>%
  row_to_names(row_number = 12) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 12 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1l <- df %>% 
  slice(c(440:480)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1m <- df %>% 
  slice(c(481:520)) %>% 
  row_to_names(row_number =12) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 12 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1n <- df %>% 
  slice(c(521:566)) %>%
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1 <- rbind(df1a, df1b, df1c, df1d, df1e, df1f, df1g, df1h,df1i, df1j, df1k, df1l, df1m, df1n)


df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:61, "SG 1",
                        ifelse(row.names(df1) %in% 62:128, "SG 2",
                               ifelse(row.names(df1) %in% 129:195, "SG 3",
                                  ifelse(row.names(df1) %in% 196:262, "SG 4",
                                         ifelse(row.names(df1) %in% 263:329, "SS 1",
                                                ifelse(row.names(df1) %in% 330:396, "SS3","SS4"))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 291 Metropolitan Avenue") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_36 <- df4 
write_csv(phoenix_36, "./data/cleandata/phoenix/SV 291 Metropolitan Avenue.csv")
```

## SV 376-382 Wallabout Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 376-382 Wallabout Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 4",
                        ifelse(row.names(df1) %in% 68:134, "SG 3",
                               ifelse(row.names(df1) %in% 135:201, "SG 2","SG 1")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 376-382 Wallabout Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_37 <- df4 
write_csv(phoenix_37, "./data/cleandata/phoenix/SV 376-382 Wallabout Street.csv")
```

## SV 400 Union Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 400 Union Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`
    ## • `` -> `...33`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter),
          by = "DD")
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1 <- df1 %>% 
  slice(c(1:268)) 
  
row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 2",
                        ifelse(row.names(df1) %in% 68:134, "SG 4",
                               ifelse(row.names(df1) %in% 135:201, "SG 3","SG 1")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 400 Union Ave") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_38 <- df4 
write_csv(phoenix_38, "./data/cleandata/phoenix/SV 400 Union Ave.csv")
```

## SV 511 Meeker Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 511 Meeker Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
  filter(if_any(ppbv_rl, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
  rename(ug_m3_rl = ug_m3_lod_rl_mdl)
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$parameter[67] <- "% Bromofluorobenzene"
df1$parameter[137] <- "% Bromofluorobenzene"
df1$parameter[207] <- "% Bromofluorobenzene"
df1$parameter[277] <- "% Bromofluorobenzene"


df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV3",
                        ifelse(row.names(df1) %in% 68:137, "SV2",
                               ifelse(row.names(df1) %in% 138:207, "SS1","SV1")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 511 Meeker Avenue") %>% 
    select(-lod_mdl,  -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_39 <- df4 
write_csv(phoenix_39, "./data/cleandata/phoenix/SV 511 Meeker Avenue.csv")
```

## SV 533-537 Flushing Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 533-537 Flushing Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 1",
                        ifelse(row.names(df1) %in% 68:134, "SG 2", "SG 3"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 533-537 Flushing Avenue") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_40 <- df4 
write_csv(phoenix_40, "./data/cleandata/phoenix/SV 533-537 Flushing Avenue.csv")
```

## SV 977 Manhattan Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 977 Manhattan Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`
    ## • `` -> `...33`
    ## • `` -> `...34`
    ## • `` -> `...35`
    ## • `` -> `...36`
    ## • `` -> `...37`

``` r
df1a <- df %>% 
  slice(c(1:34)) %>% 
  row_to_names(row_number =6) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(35:78)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(c(79:117)) %>% 
  row_to_names(row_number =11) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 11 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1d <- df %>% 
  slice(c(118:160)) %>% 
  row_to_names(row_number =1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1e <- df %>% 
  slice(c(161:199)) %>% 
  row_to_names(row_number =12) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 12 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1f <- df %>% 
  slice(c(200:245)) %>% 
  row_to_names(row_number =2) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ug_m3_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 2 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1 <- rbind(df1a, df1b, df1c, df1d, df1e, df1f)
df1$date_time <- convertToDateTime(df1$date_time)



row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS3",
                        ifelse(row.names(df1) %in% 68:134, "SS2","SS1"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 977 Manhattan Ave") %>% 
    select(-lod_mdl, -lod_mdl_2, -reference) %>%    
  select(street_address, sample_id, everything())


phoenix_41 <- df4 
write_csv(phoenix_41, "./data/cleandata/phoenix/SV 977 Manhattan Ave.csv")
```

## SV 1068-1072 Fulton Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1068-1072 Fulton Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`

``` r
df1 <- df %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1 <- df1 %>% 
  slice(c(1:268)) 
row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 6",
                        ifelse(row.names(df1) %in% 68:134, "SG 4",
                               ifelse(row.names(df1) %in% 135:201, "SG 1","SG 5")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1068-1072 Fulton Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_42 <- df4 
write_csv(phoenix_26, "./data/cleandata/phoenix/SV 1068-1072 Fulton Street.csv")
```

## SV 1146 Fulton Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1146 Fulton Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG1",
                        ifelse(row.names(df1) %in% 68:134, "SG7",
                               ifelse(row.names(df1) %in% 135:201, "SG2",
                                  ifelse(row.names(df1) %in% 202:268, "SG6",
                                         ifelse(row.names(df1) %in% 269:335, "SG4",
                                                ifelse(row.names(df1) %in% 336:402, "SG3",
                                                       ifelse(row.names(df1) %in% 403:469, "OUTSIDE",
                                                              ifelse(row.names(df1) %in% 470:536, "INSIDE",
                                                                     ifelse(row.names(df1) %in% 537:577, "AMBIENT AIR","SUB SLAB"
                                                                            )))))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1146 Fulton Street") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_43 <- df4 
write_csv(phoenix_43, "./data/cleandata/phoenix/SV 1146 Fulton Street.csv")
```

## SV 5111 4th Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 5111 4th Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG-1",
                        ifelse(row.names(df1) %in% 68:134, "SG-2", "SG-3"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 5111 4th Avenue") %>% 
    select(-lod_mdl, -lod_mdl_2, -reference) %>%    
  select(street_address, sample_id, everything())


phoenix_44 <- df4 
write_csv(phoenix_44, "./data/cleandata/phoenix/SV 5111 4th Avenue.csv")
```

## SV DOMINO SUGAR SITE A

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV DOMINO SUGAR SITE A.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "A-SG1",
                        ifelse(row.names(df1) %in% 68:134, "A-SG2","A-SG3"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV DOMINO SUGAR SITE A") %>% 
    select( -reference) %>%    
  select(street_address, sample_id, everything())


phoenix_45 <- df4 
write_csv(phoenix_45, "./data/cleandata/phoenix/SV DOMINO SUGAR SITE A.csv")
```

## SV DOMINO SUGAR SITE B 270-290 Kent Avenue

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV DOMINO SUGAR SITE B 270-290 Kent Avenue.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "B-SG1",
                        ifelse(row.names(df1) %in% 68:134, "B-SG2",
                               ifelse(row.names(df1) %in% 135:201, "B-SG3","B-SG4")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV DOMINO SUGAR SITE B 270-290 Kent Avenue") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_46 <- df4 
write_csv(phoenix_46, "./data/cleandata/phoenix/SV DOMINO SUGAR SITE B 270-290 Kent Avenue.csv")
```

## SV 36-70 Noll

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 36-70 Noll.xlsx") %>% 
  janitor::clean_names()
```

    ## Warning: Expecting logical in S1012 / R1012C19: got 'ND'

    ## Warning: Expecting logical in S1015 / R1015C19: got 'ND'

    ## Warning: Expecting logical in S1018 / R1018C19: got 'ND'

    ## Warning: Expecting logical in S1021 / R1021C19: got 'ND'

    ## Warning: Expecting logical in S1026 / R1026C19: got 'ND'

    ## Warning: Expecting logical in S1027 / R1027C19: got 'ND'

    ## Warning: Expecting logical in S1029 / R1029C19: got 'ND'

    ## Warning: Expecting logical in S1031 / R1031C19: got 'ND'

    ## Warning: Expecting logical in S1032 / R1032C19: got 'ND'

    ## Warning: Expecting logical in S1035 / R1035C19: got 'ND'

    ## Warning: Expecting logical in S1036 / R1036C19: got 'ND'

    ## Warning: Expecting logical in B1039 / R1039C2: got 'MS Dup - Matrix Spike Duplicate
    ## NC - No Criteria Intf - Interference'

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`

``` r
df1 <- df %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1 <- df1 %>% 
  slice(c(1:871)) 

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 12",
                        ifelse(row.names(df1) %in% 68:134, "SG 15",
                               ifelse(row.names(df1) %in% 135:201, "SG 14",
                                      ifelse(row.names(df1) %in% 202:268, "SG 7",
                                            ifelse(row.names(df1) %in% 269:335, "SG 16",
                                                  ifelse(row.names(df1) %in% 336:402, "SG 8",
                                                         ifelse(row.names(df1) %in% 403:469, "SG 4",
                                                            ifelse(row.names(df1) %in% 470:536, "SG 13", 
                                                                       ifelse(row.names(df1) %in% 537:603, "SG 5",
                                                                              ifelse(row.names(df1) %in% 604:670, "SG 3",
                                                                                     ifelse(row.names(df1) %in% 671:737, "SG 9", "SG 10")))))))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 36-70 Noll") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_47 <- df4 
write_csv(phoenix_47, "./data/cleandata/phoenix/SV 36-70 Noll.csv")
```

## SV 45 Walton Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 45 Walton Street .xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`

``` r
df1 <- df %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1 <- df1 %>% 
  slice(c(1:268)) 
row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS-1",
                        ifelse(row.names(df1) %in% 68:134, "SS-3",
                               ifelse(row.names(df1) %in% 135:201, "SS-2","SS-4")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 45 Walton Street ") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_48 <- df4 
write_csv(phoenix_48, "./data/cleandata/phoenix/SV 45 Walton Street .csv")
```

## SV 53 Grand Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 53 Grand Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`

``` r
df1 <- df %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1 <- df1 %>% 
  slice(c(1:134)) 

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG-3", "SG-2")


# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 53 Grand Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_49 <- df4 
write_csv(phoenix_49, "./data/cleandata/phoenix/SV 53 Grand Street.csv")
```

## SV 59-63 North 6th Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 59-63 North 6th Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1 <- df1 %>% 
  slice(c(1:271)) 

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV-2",
                        ifelse(row.names(df1) %in% 68:135, "SV-4",
                               ifelse(row.names(df1) %in% 136:203, "SV-1" ,"SV-3")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 59-63 North 6th Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_50 <- df4 
write_csv(phoenix_50, "./data/cleandata/phoenix/SV 59-63 North 6th Street.csv")
```

## SV 89-141 Melrose Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 89-141 Melrose Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`

``` r
df1 <- df %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

df1 <- df1 %>% 
  slice(c(1:871)) 

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 12",
                        ifelse(row.names(df1) %in% 68:134, "SG 15",
                               ifelse(row.names(df1) %in% 135:201, "SG 14",
                                      ifelse(row.names(df1) %in% 202:268, "SG 7",
                                            ifelse(row.names(df1) %in% 269:335, "BSG 16",
                                                  ifelse(row.names(df1) %in% 336:402, "SG 8",
                                                         ifelse(row.names(df1) %in% 403:469, "SG 4",
                                                            ifelse(row.names(df1) %in% 470:536, "SG 13", 
                                                                       ifelse(row.names(df1) %in% 537:603, "SG 5",
                                                                              ifelse(row.names(df1) %in% 604:670, "SG 3",
                                                                                     ifelse(row.names(df1) %in% 671:737, "SG 9",
                                                                                            ifelse(row.names(df1) %in% 738:804, "SG 11", "SG 10"))))))))))))
                                                                                                   

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 89-141 Melrose Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_51 <- df4 
write_csv(phoenix_51, "./data/cleandata/phoenix/SV 89-141 Melrose Street.csv")
```

## SV 108 Frost Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 108 Frost Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`
    ## • `` -> `...33`
    ## • `` -> `...34`
    ## • `` -> `...35`
    ## • `` -> `...36`
    ## • `` -> `...37`
    ## • `` -> `...38`

``` r
df1a <- df %>% 
  slice(c(1:150)) %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(232:260)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(c(261:302)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1 <- rbind(df1a, df1b, df1c)
df1$date_time <- convertToDateTime(df1$date_time)


row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG-1",
                        ifelse(row.names(df1) %in% 68:134, "SG-3","SG-2"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 108 Frost Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_52 <- df4 
write_csv(phoenix_52, "./data/cleandata/phoenix/SV 108 Frost Street.csv")
```

## SV 171-173 Bayrad Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 171-173 Bayrad Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter),
          date_time=41753)
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")

df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG-2",
                        ifelse(row.names(df1) %in% 68:134, "SG-1",
                               ifelse(row.names(df1) %in% 135:201, "SG-5", 
                                      ifelse(row.names(df1) %in% 202:268, "SG-3", "SG-4"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 171-173 Bayrad Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())

phoenix_53 <- df4 
write_csv(phoenix_53, "./data/cleandata/phoenix/SV 171-173 Bayrad Street.csv")
```

## SV 196 MIddleton Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 196 MIddleton Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`

``` r
df1a <- df %>% 
  slice(c(1:154)) %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter),
          date_time = 41918) 
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(155:182)) %>% 
  clean_names() %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(c(183:224)) %>% 
  clean_names() %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
# Stack three datasets df1a, df1b, and df1c into one dataset df1
df1 <- rbind(df1a, df1b, df1c)

df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG-3",
                        ifelse(row.names(df1) %in% 68:134, "SG-2", "SG-1"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 196 MIddleton Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_54 <- df4 
write_csv(phoenix_54, "./data/cleandata/phoenix/SV 196 MIddleton Street.csv")
```

## SV 299-301 Wallabout Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 299-301 Wallabout Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
  filter(if_any(parameter, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG-2",
                        ifelse(row.names(df1) %in% 68:134, "SG-1",
                               ifelse(row.names(df1) %in% 135:201, "SG-3","SG-4")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 299-301 Wallabout Street") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_55 <- df4 
write_csv(phoenix_55, "./data/cleandata/phoenix/SV 299-301 Wallabout Street.csv")
```

## SV 313 Wallabout Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 313 Wallabout Street .xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1 <- df1 %>% 
  slice(c(1:268)) 

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS-1",
                        ifelse(row.names(df1) %in% 68:134, "SG-3",
                               ifelse(row.names(df1) %in% 135:201, "SG-1","SG-2")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 313 Wallabout Street ") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_56 <- df4 
write_csv(phoenix_56, "./data/cleandata/phoenix/SV 313 Wallabout Street .csv")
```

## SV 417 Manhattan Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 417 Manhattan Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`

``` r
df1a <- df %>% 
  slice(c(1:32)) %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(33:79)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(lod_mdl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(c(80:107)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(lod_mdl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1d <- df %>% 
  slice(c(108:154)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(lod_mdl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1e <- df %>% 
  slice(c(155:182)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(lod_mdl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1f <- df %>% 
  slice(c(183:224)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(lod_mdl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1 <- rbind(df1a, df1b,df1c, df1d,df1e, df1f)
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 1",
                        ifelse(row.names(df1) %in% 68:134, "SG 2", "SG 3"))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 417 Manhattan Ave") %>% 
    select(-lod_mdl, -lod_mdl_2, -reference) %>%    
  select(street_address, sample_id, everything())


phoenix_57 <- df4 
write_csv(phoenix_57, "./data/cleandata/phoenix/SV 417 Manhattan Ave.csv")
```

## SV 575-581 4th Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 575-581 4th Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG4",
                        ifelse(row.names(df1) %in% 68:134, "SG6",
                               ifelse(row.names(df1) %in% 135:201, "SG2",
                                  ifelse(row.names(df1) %in% 202:268, "SG5","SG3"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 575-581 4th Ave") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_58 <- df4 
write_csv(phoenix_58, "./data/cleandata/phoenix/SV 575-581 4th Ave.csv")
```

## SV 635 4th Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 635 4th Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
df1 <- df %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SS-1",
                        ifelse(row.names(df1) %in% 68:134, "SS-2",
                               ifelse(row.names(df1) %in% 135:201, "SG-1",
                                  ifelse(row.names(df1) %in% 202:268, "SG-2","SG-3"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 635 4th Ave") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_59 <- df4 
write_csv(phoenix_59, "./data/cleandata/phoenix/SV 635 4th Ave.csv")
```

## SV 687 Flushing Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 687 Flushing Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1 <-df1 %>% 
  slice(1:268)
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG-2",
                        ifelse(row.names(df1) %in% 68:134, "SG-1",
                               ifelse(row.names(df1) %in% 135:201, "SG-4", "SG-3")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 687 Flushing Ave",
           by="KCA") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_60 <- df4 
write_csv(phoenix_60, "./data/cleandata/phoenix/SV 687 Flushing Ave.csv")
```

## SV 771-781 Metro Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 771-781 Metro Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`

``` r
df1a <- df %>% 
  slice(c(1:32)) %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(c(33:79)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(c(80:107)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1d <- df %>% 
  slice(c(108:149)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1e <- df %>% 
  slice(c(232:259)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) 
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1f <- df %>% 
  slice(c(260:307)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1g <- df %>% 
  slice(c(308:335)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1h <- df %>% 
  slice(c(336:383)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1i <- df %>% 
  slice(c(384:411)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) 
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1j <- df %>% 
  slice(c(412:455)) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1 <- rbind(df1a, df1b,df1c, df1d,df1e, df1f,df1g, df1h,df1i, df1j)

df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 1",
                        ifelse(row.names(df1) %in% 68:134, "SG 2",
                               ifelse(row.names(df1) %in% 135:201, "SG 4",
                                  ifelse(row.names(df1) %in% 202:268, "SG 5","SG 6"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 771-781 Metro Ave") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_61 <- df4 
write_csv(phoenix_61, "./data/cleandata/phoenix/SV 771-781 Metro Ave.csv")
```

## SV 802-806 Myrtle Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 802-806 Myrtle Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`

``` r
df1 <- df %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1<-df1 %>% 
  slice(1:268)
row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG1",
                        ifelse(row.names(df1) %in% 68:134, "SG2",
                               ifelse(row.names(df1) %in% 135:201, "SG3", "SG4")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 802-806 Myrtle Ave", by="KCA") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_62 <- df4 
write_csv(phoenix_62, "./data/cleandata/phoenix/SV 802-806 Myrtle Ave.csv")
```

## SV 901 Myrtle Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 901 Myrtle Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG1",
                        ifelse(row.names(df1) %in% 68:134, "SG5",
                               ifelse(row.names(df1) %in% 135:201, "SG2","SG3")))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 901 Myrtle Ave") %>% 
    select(-lod_mdl, -lod_mdl_2, -dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_63 <- df4 
write_csv(phoenix_63, "./data/cleandata/phoenix/SV 901 Myrtle Ave.csv")
```

## SV 902-908 Flushing Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 902-908 Flushing Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`
    ## • `` -> `...33`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1<-df1 %>% 
  slice(1:671)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "BL 3139 SG-3",
                        ifelse(row.names(df1) %in% 68:134, "BL 3139 SG-4",
                               ifelse(row.names(df1) %in% 135:201, "BL 3141 SG-6",
                                      ifelse(row.names(df1) %in% 202:268, "BL 3139 SG-2", 
                                             ifelse(row.names(df1) %in% 269:335, "BL 3141 SG-4",
                                                  ifelse(row.names(df1) %in% 336:402, "BL 3139 SG-1",
                                                         ifelse(row.names(df1) %in% 403:469, "BL 3139 SG-2",
                                                            ifelse(row.names(df1) %in% 470:536, "BL 3141 SG-7", 
                                                                       ifelse(row.names(df1) %in% 537:603, "BL 3141 SG-1","BL 3141 SG-5")))))))))
# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 902-908 Flushing Ave") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_64 <- df4 
write_csv(phoenix_64, "./data/cleandata/phoenix/SV 902-908 Flushing Ave.csv")
```

## SV 948 Myrtle Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 948 Myrtle Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
  filter(!is.na(date_time)) %>%
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1 <- df1 %>%
filter(!is.na(date_time)) 

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SV8",
                        ifelse(row.names(df1) %in% 68:134, "SV6",
                               ifelse(row.names(df1) %in% 135:201, "SV9",
                                  ifelse(row.names(df1) %in% 202:268, "SV3",
                                         ifelse(row.names(df1) %in% 269:335, "SV4",
                                               ifelse(row.names(df1) %in% 336:402, "SV5", "SV2"))))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 948 Myrtle Ave") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_65 <- df4 
write_csv(phoenix_65, "./data/cleandata/phoenix/SV 948 Myrtle Ave.csv")
```

## SV 1044 Bedford Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1044 Bedford Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`

``` r
df1 <- df %>% 
  row_to_names(row_number = 5) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
    rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter),
          by = "KCA")
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1<-df1 %>% 
  slice(1:137)

df1 <- df1 %>%
filter(!is.na(date_time)) 

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SG 1", "SG 2")

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1044 Bedford Ave") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_66 <- df4 
write_csv(phoenix_66, "./data/cleandata/phoenix/SV 1044 Bedford Ave.csv")
```

## SV 1570 60th Street

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1570 60th Street.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`
    ## • `` -> `...33`
    ## • `` -> `...34`

``` r
df1 <- df %>% 
  row_to_names(row_number = 4) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 4 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)
```

    ## Warning in convertToDateTime(df1$date_time): NAs introduced by coercion

``` r
df1<-df1 %>% 
  slice(1:402)
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "SSV-1",
                        ifelse(row.names(df1) %in% 68:134, "SSV-2",
                               ifelse(row.names(df1) %in% 135:201, "SSV-3",
                                      ifelse(row.names(df1) %in% 202:268, "SSV-4",
                                            ifelse(row.names(df1) %in% 269:335, "SSV-5","SSV-6")))))
# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1570 60th Street") %>% 
    select(-dilution) %>%    
  select(street_address, sample_id, everything())


phoenix_67 <- df4 
write_csv(phoenix_67, "./data/cleandata/phoenix/SV 1570 60th Street.csv")
```

## SV 1875 Atlantic Ave

``` r
df = 
  read_excel("./data/data_from_M/phoenix/SV 1875 Atlantic Ave.xlsx") %>% 
  janitor::clean_names()
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`
    ## • `` -> `...33`
    ## • `` -> `...34`

``` r
df1a <- df %>% 
  slice(1:37) %>% 
  row_to_names(row_number = 6) %>%  #may change
  clean_names() %>% 
    rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 6 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1b <- df %>% 
  slice(38:84) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1c <- df %>% 
  slice(85:116) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
    rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1d <- df %>% 
  slice(117:163) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1e <- df %>% 
  slice(164:195) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
    rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1f <- df %>% 
  slice(196:242) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1g <- df %>% 
  slice(243:274) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
    rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1h <- df %>% 
  slice(275:321) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1i <- df %>% 
  slice(322:353) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
    rename(ppbv_result = result,
         ppbv_rl = rl,
         ug_m3_result = result_2,
         ug_m3_rl = rl_2) %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1j <- df %>% 
  slice(354:396) %>% 
  row_to_names(row_number = 1) %>%  #may change
  clean_names() %>% 
  select(-starts_with("na")) %>% 
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 1 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1 <- rbind(df1a, df1b, df1c, df1d, df1e, df1f, df1g, df1h, df1j, df1i)

df1$date_time <- convertToDateTime(df1$date_time)

row_numbers <- which(df1$parameter == "% Bromofluorobenzene")
df1$sample_id <- ifelse(row.names(df1) %in% 1:67, "S14-SOIL VAPOR 5",
                        ifelse(row.names(df1) %in% 68:134, "S14-SOIL VAPOR 4",
                               ifelse(row.names(df1) %in% 135:201, "S14-SOIL VAPOR 3",
                                      ifelse(row.names(df1) %in% 202:268,"S14-SOIL VAPOR 2","S14-SOIL VAPOR 1"))))

# check the data: using keyword: 1. Tetrachloroethane 2.Bromofluorobenzene


df4 <-df1 %>% 
    mutate(lab = "phoenix", street_address = "SV 1875 Atlantic Ave") %>% 
    select(-reference) %>%    
  select(street_address, sample_id, everything())


phoenix_68 <- df4 
write_csv(phoenix_68, "./data/cleandata/phoenix/SV 1875 Atlantic Ave.csv")
```
