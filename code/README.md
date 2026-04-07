This directory contains the method implementation and the scripts used to
produce the analyses results.

.

## Contents

- `R/`: core MTL-MICE functions, simulation helpers, metrics, and output
  extraction utilities.
- `MTL_MICE_toy_example.qmd`: small toy vignette showing how to simulate
  multi-task data, induce missingness, run `mtlmice()`, and inspect completed
  datasets.
- `rda/`: real-data analysis scripts for the U.S. election application,
  including SLURM submission wrappers and figure-generation scripts.
- `sim/`: simulation-study scripts and summary plotting code.

## Notes

- The `.sh` files in `rda/` and `sim/` are the original SLURM launch scripts
  used for the long batch runs.
- The core functions in `R/` are shared by both the simulations and the
  real-data application.
