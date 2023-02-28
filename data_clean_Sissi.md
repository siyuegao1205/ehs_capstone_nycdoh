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
  row_to_names(row_number = 5) %>% 
  clean_names() %>% 
  select(-(2:6), -8, -12, -14, -16, -19, -21) %>% #may change
  filter(if_any(ppbv_result, ~ !(.x %in% c("ppbv Result", NA)))) %>% 
   mutate(parameter = ifelse(ppbv_rl=="%", "% Bromofluorobenzene", parameter))
```

    ## Warning: Row 5 does not provide unique names. Consider running clean_names()
    ## after row_to_names().

``` r
df1$date_time <- convertToDateTime(df1$date_time)

df2 <- df %>% 
  slice(c(3:4)) %>% #may change
  select(1,2) 


# Syntax using colnames()
colnames(df2)[1] = "id"
colnames(df2)[2] = "client_id"

#df3 <- df2 %>% 
#  separate(id, into = c("id", "address"), sep = "Project ID:       ") #may change

address <- strsplit(as.character(df2$id),'Project ID:       ') 
do.call(rbind, address)
```

    ##      [,1]                                           [,2]           
    ## [1,] "P.O.#:\r\nCanister Id:             23326\r\n" "81 MCGUINNESS"
    ## [2,] "Client ID:"                                   "Client ID:"

``` r
df3 <- data.frame(df2$id, df2$client_id, do.call(rbind, address))

df3 <- df3 %>% 
  select(2,4) 

colnames(df3)[1] = "client_id"
colnames(df3)[2] = "address"


df4 <-df1 %>% 
    mutate(project_id = df3[2,1],
         client_id = df3[1,2]) %>% 
  select(project_id, client_id, everything())



#df4 <-df1 %>% 
#    mutate(project_id = df3[2,1],
#         client_id = df3[1,2],
 #        project_id = project_id$address,
 #        client_id = client_id$client_id) %>% 
 # select(project_id, client_id, everything())
```

``` r
write_csv(df4, "./data/cleandata/phoenix/SV 81 McGuinness Blvd.csv")
```
