Data Cleaning for Alpha Labs
================

## Notes

- Method Blank Analysis for Batch Quality Control is **NOT** extracted

  - 63-71 Bridge Street

- Air Canister Certification Results are **NOT** extracted

  - 174 Kings Highway

- Blank is **NOT** extracted

  - 820 Glenmore Avenue

- Sample IDs are **NOT** consecutive numbers

  - **315 Linwood Street**: B1 - B7 with B5 missing
  - **540 Fulton Street**: SV01 - SV06 with SV02 and SV03 missing

- Some manual edits are made before reading in

- Repetitive codes are used for some iterations

- Have to manually re-position the respective range for each sub-form
  before binding them together

## Current Progress: 15 out of 18 Finished

(codes are hidden)

Some `mutate` on `full_alpha` and merging `project_id`, with some sample
data shown below.
