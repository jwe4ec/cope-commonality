# cope-commonality
Analysis code for manuscript "Unique and shared effects of single-session intervention 
proximal outcomes on 3-month depression symptoms in adolescents: A commonality analysis"

# Overview

This repo supersedes an old repo for this manuscript (called the *source repo* in pull
requests that import code snapshots at key milestones from the old repo)

# File Organization

Scripts are numbered in the order to be run

Data and results are stored on this repo's corresponding [OSF project](https://osf.io/ftxr2)

```plaintext
.
├── .gitignore   # Used to prevent committing data and results to GitHub
├── README.md
|
├── 0_define_functions.R   # Main code
├── 1_install_groundhog.R
├── 2_clean_data.Rmd
├── 3_run_PCA.Rmd
├── 4_run_analysis.Rmd
├── 5_describe_demographics.Rmd
├── 6_run_mcdonalds_omega.R
├── 7_extract_omega_results.R
|
├── session_info/   # Session info (from "clean_data.Rmd" and "run_anlaysis.Rmd")
├── archive/   # Old cleaning and analysis code
|
├── data/   # Data (stored on OSF)
|   ├── source/cleaned_cope_data_randomized_rev1.rds
|   └── processed/cope_ca_data_nonimputed_rev1.csv
|
├── results/   # Main results (stored on OSF)
|   ├── omega/
|   └── total_effect_plots/
|
└── evaluate_shs_scoring/   # Used to evaluate SHS scoring in main COPE paper
    ├── evaluate_shs_scoring_in_schleider_et_al_2022.R
    └── results/   # SHS scoring-specific results (stored on OSF)
```

# Source Data

Source data are from the [OSF project](https://osf.io/8mk6x) for the COPE study's 
main outcomes paper ([Schleider et al., 2022](https://doi.org/gnq5rq))