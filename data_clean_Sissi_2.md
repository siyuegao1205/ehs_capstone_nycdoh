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

    ## # A tibble: 47 × 2
    ##    files                              path                                      
    ##    <chr>                              <chr>                                     
    ##  1 SV 1-37 Forrest Street.xlsx        ./data/data_from_M/phoenix/SV 1-37 Forres…
    ##  2 SV 1026-1030 Manhattan Ave.xlsx    ./data/data_from_M/phoenix/SV 1026-1030 M…
    ##  3 SV 1048 Manhattan Ave.xlsx         ./data/data_from_M/phoenix/SV 1048 Manhat…
    ##  4 SV 1050-1066 Manhattan Avenue.xlsx ./data/data_from_M/phoenix/SV 1050-1066 M…
    ##  5 SV 1068-1072 Fulton Street.xlsx    ./data/data_from_M/phoenix/SV 1068-1072 F…
    ##  6 SV 1096 Broadway.xlsx              ./data/data_from_M/phoenix/SV 1096 Broadw…
    ##  7 SV 113 Hamilton Ave.xlsx           ./data/data_from_M/phoenix/SV 113 Hamilto…
    ##  8 SV 1146 Fulton Street.xlsx         ./data/data_from_M/phoenix/SV 1146 Fulton…
    ##  9 SV 125 Borinquen Place.xlsx        ./data/data_from_M/phoenix/SV 125 Borinqu…
    ## 10 SV 1353 Flatbush Avenue.xlsx       ./data/data_from_M/phoenix/SV 1353 Flatbu…
    ## # … with 37 more rows

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

SV 1353 Flatbush Avenue: address is incomplete

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
    mutate(street_address = "SV 67 Duffield Street") %>% 
  select(street_address, sample_id, everything())



#df4 <-df1 %>% 
#    mutate(project_id = df3[2,1],
#         client_id = df3[1,2],
 #        project_id = project_id$address,
 #        client_id = client_id$client_id) %>% 
 # select(project_id, client_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 67 Duffield Street.csv")
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
    mutate(street_address = "SV 81 McGuinness Blvd") %>% 
  select(street_address, sample_id, everything())

write_csv(df4, "./data/cleandata/phoenix/SV 81 McGuinness Blvd.csv")
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
    mutate(street_address = "SV 98 3rd Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 98 3rd Avenue.csv")
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
    mutate(street_address = "SV 113 Hamilton Ave") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 113 Hamilton Ave.csv")
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
    mutate(street_address = "SV 137 Frost Street") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 137 Frost Street.csv")
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
    mutate(street_address = "SV 183 McGuinness Blvd") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 183 McGuinness Blvd.csv")
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
    mutate(street_address = "SV 288-292 Union Ave") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 288-292 Union Ave.csv")
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
    mutate(street_address = "SV 633 Marcy Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 633 Marcy Avenue.csv")
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
    mutate(street_address = "SV 842-846 Flushing Ave") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 842-846 Flushing Ave.csv")
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
    mutate(street_address = "SV 922 Myrtle Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 922 Myrtle Avenue.csv")
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
    mutate(street_address = "SV 975 Manhattan Ave") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 975 Manhattan Ave.csv")
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
    mutate(street_address = "SV 1026-1030 Manhattan Ave") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 1026-1030 Manhattan Ave.csv")
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
    mutate(street_address = "SV 1048 Manhattan Ave") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 1048 Manhattan Ave.csv")
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
    mutate(street_address = "SV 1096 Broadway") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 1096 Broadway.csv")
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
    mutate(street_address = "SV 1457-1471 Flatbush Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 1457-1471 Flatbush Avenue.csv")
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
    mutate(street_address = "SV 1458-1460 Flatbush Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 1458-1460 Flatbush Avenue.csv")
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
    mutate(street_address = "SV 1516 Fulton Street") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 1516 Fulton Street.csv")
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
    mutate(street_address = "SV 2437 Pitkin Ave") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 2437 Pitkin Ave.csv")
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
    mutate(street_address = "SV 47 Walton Street") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 47 Walton Street.csv")
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
    mutate(street_address = "SV 71 & 76 North 7th Street") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 71 & 76 North 7th Street.csv")
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
    mutate(street_address = "SV 141 Metropolitan Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 141 Metropolitan Avenue.csv")
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
    mutate(street_address = "SV 241-245 4th Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 241-245 4th Avenue.csv")
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
    mutate(street_address = "SV 415-419 Marcy Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 415-419 Marcy Avenue.csv")
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
    mutate(street_address = "SV 578 5th Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 578 5th Avenue.csv")
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
    mutate(street_address = "SV 609-619 4th Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 609-619 4th Avenue.csv")
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
    mutate(street_address = "SV 814 Bedford Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 814 Bedford Avenue.csv")
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
    mutate(street_address = "SV 1050-1066 Manhattan Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 1050-1066 Manhattan Avenue.csv")
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
    mutate(street_address = "SV 1353 Flatbush Avenue") %>% 
  select(street_address, sample_id, everything())


write_csv(df4, "./data/cleandata/phoenix/SV 1353 Flatbush Avenue.csv")
```
