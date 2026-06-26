# ---------------------------------------------------------------------------- #
# Setup ----
# ---------------------------------------------------------------------------- #

# Use R 4.4.0

# No packages loaded

# Read in omega results list

omega_ls <- readRDS(file.path("results", "omega", "omega_ls.RDS"))

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



