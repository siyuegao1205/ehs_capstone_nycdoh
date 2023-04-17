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
|--------------|-----------------------|---------------------------------------------|---------------------------------------------|
| Project ID | Unique indicator containing strings of “CVCP” (or “CBCP) | Text | EPIC |
| Street Address | Physical address | Text | EPIC |
| Client Sample ID | Unique indicator for repeated samples at each site | Text | Lab report |
| Collection Date |  | Date mm/dd/yyyy | Lab report |
| Analysis Date |  | Date mm/dd/yyyy | Lab report |
| Chemical Name (ppbv) | In part per billion/volume | Number | Lab report |
| ppbv RL | Reporting limit ppbv | Number | Lab report |
| Chemical Name (µg/m3) | In micrograms per cubic meter | Number | Lab report |
| µg/m3 RL | Reporting limit µg/m3 | Number | Lab report |
| Dilution | Dilution factor if available | Number | Lab report |


#### Site Characteristics Information

| _Variable_         | _Definition_ | _Data type_ | _Source_ |
|--------------------|-----------------------|---------------------------------------------|---------------------------------------------|
| Project ID         | Unique indicator containing strings of “CVCP” (or “CBCP) | Text | EPIC |
| Street Address     | Physical address | Text | EPIC |
| Borough            | K = Brooklyn | Text | EPIC |
| Zip Code           | 5-digit zip code | Text | Geocoding |
| X Coordinate       |  | Number | Geocoding |
| Y Coordinate       |  | Number | Geocoding |
| Latitude           |  | Number | Geocoding |
| Longitude          |  | Number | Geocoding |
| Elevation          | Range of height above mean sea level in feet | Text | RIR/RAWP text |
| GW Flow            | Direction of groundwater flow | Text | RIR/RAWP text |
| GW Depth           | Range of depth to GW from surface or cellar in feet | Text | RIR/RAWP text |
| Bedrock            | Depth to bedrock in feet if encountered | Number | RIR/RAWP text |
| Lot Area           | Area of site in square feet | Number | RIR/RAWP text |
| Proposed Use       | Description of proposed use of site | Text | RIR/RAWP text |
| Use Code           | C = commercial / community <br> I = industrial / manufacturing <br> M = mixed use <br> R = residential <br> S = school <br> S/C = school / commercial <br> S/M = school     / mixed | Text | RIR/RAWP text |
| Cellar             | 1 = Yes <br> 0 = No | Number | RIR/RAWP text |
| Elevator           | 1 = Yes <br> 0 = No | Number | RIR/RAWP text |
| Current Conditions | Vacant, occupied, with supplemental descriptions | Text | RIR/RAWP text |
| History            | Past uses where available | Text | RIR/RAWP text |
| VB                 | Vapor barrier: brand and thickness in mil | Text | RIR/RAWP text |
| SSDS               | Sub-slab depressurization system: none (denoted by 0), passive, active, ventilated parking, or soil vapor extraction | Text | RIR/RAWP text |


### Poster

[Download the Poster.](./results/poster_2.0.pdf)


### Link to Final Report

