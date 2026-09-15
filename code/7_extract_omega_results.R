# ---------------------------------------------------------------------------- #
# Setup ----
# ---------------------------------------------------------------------------- #

# Use R 4.4.0

# Load required packages

library(groundhog)
groundhog_date <- "2025-04-10"
meta.groundhog(groundhog_date)

groundhog.library("here", groundhog_date)

# Read in omega results list

omega_ls <- readRDS(here("results", "omega", "omega_ls.RDS"))

# ---------------------------------------------------------------------------- #
# Extract results ----
# ---------------------------------------------------------------------------- #

omega_ls$b_cdi_PP_dat
omega_ls$b_cdi_ABC_dat
omega_ls$b_bhs_PP_dat
omega_ls$b_bhs_ABC_dat
omega_ls$b_shs_PP_dat
omega_ls$b_shs_ABC_dat
omega_ls$b_pfs_PP_dat
omega_ls$b_pfs_ABC_dat