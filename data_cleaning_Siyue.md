Data Cleaning for Alpha Labs
================

## Notes

- Method Blank Analysis for Batch Quality Control is **NOT** extracted

- Air Canister Certification Results are **NOT** extracted

- Some manual edits are made before reading in

- Repetitive codes are used for some iterations

- Have to manually re-position the respective range for each sub-form
  before binding them together

## Current Progress: 6 out of 16 Finished

(codes are hidden)

Some `mutate` on `full_alpha` and merging `project_id`.

| street_address      | laboratory       | lab_data_availability | sample_id | collection_date | analysis_date | parameter                              | ppbv_results | ppbv_rl | ppbv_mdl | ug_m3_results | ug_m3_rl | ug_m3_mdl | dilution |
|:--------------------|:-----------------|:----------------------|:----------|:----------------|:--------------|:---------------------------------------|:-------------|--------:|:---------|:--------------|---------:|:----------|---------:|
| 63-71 Bridge Street | Alpha Analytical | Yes                   | SV-01     | 2019-05-31      | 2019-06-05    | Dichlorodifluoromethane                | 0.286        |    0.20 | –        | 1.41          |    0.989 | –         |        1 |
| 63-71 Bridge Street | Alpha Analytical | Yes                   | SV-01     | 2019-05-31      | 2019-06-05    | Chloromethane                          | 0.893        |    0.20 | –        | 1.84          |    0.413 | –         |        1 |
| 63-71 Bridge Street | Alpha Analytical | Yes                   | SV-01     | 2019-05-31      | 2019-06-05    | 1,2-Dichloro-1,1,2,2-tetrafluoroethane | ND           |    0.05 | –        | ND            |    0.349 | –         |        1 |
| 63-71 Bridge Street | Alpha Analytical | Yes                   | SV-01     | 2019-05-31      | 2019-06-05    | Vinyl chloride                         | 0.035        |    0.02 | –        | 0.09          |    0.051 | –         |        1 |
| 63-71 Bridge Street | Alpha Analytical | Yes                   | SV-01     | 2019-05-31      | 2019-06-05    | 1,3-Butadiene                          | ND           |    0.02 | –        | ND            |    0.044 | –         |        1 |
| 63-71 Bridge Street | Alpha Analytical | Yes                   | SV-01     | 2019-05-31      | 2019-06-05    | Bromomethane                           | ND           |    0.02 | –        | ND            |    0.078 | –         |        1 |
