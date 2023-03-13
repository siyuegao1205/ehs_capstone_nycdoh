Data Cleaning
================

## Notes

- Method Blank Analysis for Batch Quality Control is **NOT** extracted

  - 63-71 Bridge Street

- Air Canister Certification Results are **NOT** extracted

  - 174 Kings Highway

- Blank is **NOT** extracted

  - 820 Glenmore Avenue

- Sample IDs are **NOT** consecutive numbers - Treat as sample names

  - **315 Linwood Street**: B1 - B7 with B5 missing
  - **540 Fulton Street**: SV01 - SV06 with SV02 and SV03 missing

- Some manual edits are made before reading in

- Repetitive codes are used for some iterations

- Have to manually re-position the respective range for each sub-form
  before binding them together

## Alpha Reports: 20 out of 20 Finished

(codes are hidden)

1.  63-71 Bridge Street

2.  75 Dupont Street

3.  81-111 Junius Street

4.  142-150 South Portland Avenue

5.  157 New Jersey Avenue

6.  161 Harrison Avenue

7.  174 Kings Highway

8.  205 Park Avenue

9.  264 North 10th Street

10. 315 Linwood Street

11. 335 Ralph Avenue

12. 526 Union Avenue

13. 540 Fulton Street

14. 777 Glenmore Avenue

15. 820 Glenmore Avenue

16. 2618 Fulton Street

17. Greenpoint Landing D

18. Greenpoint Landing F1

19. Greenpoint Landing H3

20. Ralph Avenue and Preston Court

Some `mutate` on `full_alpha` and merging `project_id`, with some sample
data shown below.

## Mixed Reports: 1 out of 3 Finished

1.  86 Fleet Place

2.  209-227 McGuinness Boulevard

3.  Greenpoint Landing G1
