# ---------------------------------------------------------------------------- #
# Setup ----
# ---------------------------------------------------------------------------- #

# Use R 4.4.0

# Load required packages

library(groundhog)
groundhog_date <- "2025-04-10"
meta.groundhog(groundhog_date)

groundhog.library(c("future", "future.apply", "tidyverse", "tictoc"), groundhog_date)
set.seed(1234)

# Read in data

cope_ca_data_nonimputed <- read.csv(file.path("data", "processed", "cope_ca_data_nonimputed.csv"))

# ---------------------------------------------------------------------------- #
# Subset data such that each measure at baseline has its own dataset for each condition ----
# ---------------------------------------------------------------------------- #

b_cdi_PP_dat <- cope_ca_data_nonimputed %>% 
  filter(condition == 1) %>% 
  select(b_cdi_1:b_cdi_12) 

b_cdi_ABC_dat <- cope_ca_data_nonimputed %>% 
  filter(condition == 2) %>% 
  select(b_cdi_1:b_cdi_12)

b_bhs_PP_dat <- cope_ca_data_nonimputed %>% 
  filter(condition == 1) %>% 
  select(b_bhs_1:b_bhs_4)

b_bhs_ABC_dat <- cope_ca_data_nonimputed %>% 
  filter(condition == 2) %>% 
  select(b_bhs_1:b_bhs_4)

b_shs_PP_dat <- cope_ca_data_nonimputed %>% 
  filter(condition == 1) %>% 
  select(b_shs_1, b_shs_3, b_shs_5)

b_shs_ABC_dat <- cope_ca_data_nonimputed %>% 
  filter(condition == 2) %>% 
  select(b_shs_1, b_shs_3, b_shs_5)

b_pfs_PP_dat <- cope_ca_data_nonimputed %>% 
  filter(condition == 1) %>% 
  select(pi_pfs_1:pi_pfs_7)

b_pfs_ABC_dat <- cope_ca_data_nonimputed %>% 
  filter(condition == 2) %>% 
  select(pi_pfs_1:pi_pfs_7)

# ---------------------------------------------------------------------------- #
# Compute omegas in parallel ----
# ---------------------------------------------------------------------------- #

# Put datasets in named list

dat_ls <- list(b_cdi_PP_dat, b_cdi_ABC_dat, b_bhs_PP_dat, b_bhs_ABC_dat, 
               b_shs_PP_dat, b_shs_ABC_dat, b_pfs_PP_dat, b_pfs_ABC_dat)
names(dat_ls) <- c("b_cdi_PP_dat", "b_cdi_ABC_dat", "b_bhs_PP_dat", "b_bhs_ABC_dat", 
                   "b_shs_PP_dat", "b_shs_ABC_dat", "b_pfs_PP_dat", "b_pfs_ABC_dat")

# Start timer

tic("Omega parallel computation")

# Set up parallel backend that works on Mac and Windows

plan(multisession)

# Create folders for omega results and logs

omega_path <- file.path("results", "omega")
omega_log_path <- file.path(omega_path, "logs")

dir.create(omega_log_path, recursive = TRUE)

# Compute categorical omega for each dataset

omega_ls <- future_lapply(names(dat_ls), function(dat_name) {
  # Get dataset from list
  
  dat <- dat_ls[[dat_name]]
  
  # Load packages and set seed for worker
  
  groundhog.library(c("MBESS", "lavaan"), groundhog_date)
  set.seed(1234)

  # Compute omega and export output to TXT

  logfile <- file.path(omega_log_path, paste0("omega_log_", dat_name, ".txt"))

  capture.output({
    omega <- ci.reliability(data = dat, type = "categorical",
                            conf.level = 0.95, interval.type = "bca", B = 1000)
  }, file = logfile)

  return(omega)
}, future.seed = TRUE)

names(omega_ls) <- names(dat_ls)

# Stop parallel backend

plan(sequential)

# Stop timer

toc()

# ---------------------------------------------------------------------------- #
# Document runtime and warning ----
# ---------------------------------------------------------------------------- #

# On laptop below, omega parallel computation ran on 12/4/2025 in 5598.32 sec (1.56 hr)
# - Windows 11 Enterprise (x64, build 22631) with Intel Core Ultra 7 165U, 1700 Mhz, 
# 12 cores, 14 logical processors, and 32 GB of RAM

# The following warning printed to the console twice
# - lavaan->lav_object_post_check(): some estimated ov variances are negative 

# ---------------------------------------------------------------------------- #
# Save results ----
# ---------------------------------------------------------------------------- #

saveRDS(omega_ls, file.path(omega_path, "omega_ls.RDS"))