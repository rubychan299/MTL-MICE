This directory contains the public data inputs used in the MTL-MICE case study
and the scripts used to prepare those data for analysis.

## Contents

- `uselection/`: county-level U.S. election and covariate files used in the
  real-data application.
- `uselection/bystate_2020/uselect_list2020.RData`: processed list of
  state-level tasks used by the manuscript's 2020 election analysis.
- `data_cleaning_uselection.R`: builds the state-level task objects and missing
  data replicates from the raw election files.
- `vars_selection_uselection.R`: helper functions for screening and comparing
  candidate covariates in the election data.

## Notes

- The 2020 case-study files required by the manuscript are included in this
  repository.
- `data_cleaning_uselection.R` also contains an archival 2024 preprocessing
  block. That part only runs if the additional 2024 raw CSV files are placed in
  `data/uselection/`.
