# EHS Capstone Spring 2023

## Health-based Concentrations for Air

Note that RfCs are for non-cancer effects. The health-based guidance for TCE (trichloroethene) and PCE (tetrachloroethene) are based on **cancer effects**. There is not really a health-based value for BTEX, because some of the components (e.g., benzene) are much more toxic than others. BTEX concentrations mainly indicate the amount of petroleum-related contamination.The vapor barrier will attenuate the concentration by roughly 10%, so the aim is to have an indoor air concentration less than the guidance value or RfC.

| Compound     | Concentration (μg/m3) |                                             |
|--------------|-----------------------|---------------------------------------------|
| Benzene      | 30                    | Reference Concentration (RfC) (from US EPA) |
| Toluene      | 5000                  | RfC (from US EPA)                           |
| Ethylbenzene | 1000                  | RfC (from US EPA)                           |
| Xylenes      | 100                   | RfC (from US EPA)                           |
| TCE          | 2                     | NYS DOH and NYC guidance value              |
| PCE          | 30                    | NYS DOH and NYC guidance value              |


## Final Results

442 Brooklyn brownfield sites data retrieved and stored in more workable CSV format, with two separate database established.  

*   Soil Vapor Chemical Information  
*   Site Characteristics Information  


### Data Dictionary

#### Soil Vapor Chemical Information

| _Variable_ | _Definition_ | _Data type_ | _Source_ |
|--------------|-----------------------|---------------------------------------------|
| Project ID | Unique indicator containing strings of “CVCP” (or “CBCP) | Text | EPIC |
| Street Address | Physical address | Text | EPIC |
| Client Sample ID | Unique indicator for repeated samples at each site | Text | Lab report |
| Collection Date |  | Date mm/dd/yyyy | Lab report |
| Analysis Date |  | Date mm/dd/yyyy | Lab report |
| Chemical Name (ppbv) | In part per billion/volume | Number | Lab report |





