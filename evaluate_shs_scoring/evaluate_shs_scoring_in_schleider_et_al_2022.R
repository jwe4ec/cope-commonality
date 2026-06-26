# ---------------------------------------------------------------------------- #
# Evaluate SHS Scoring in Schleider et al. (2022; https://doi.org/gnq5rq )
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Setup ----
# ---------------------------------------------------------------------------- #

# Use R 4.4.0

# Load packages

library(groundhog)
groundhog_date <- "2025-04-10"
meta.groundhog(groundhog_date)

groundhog.library(c("dplyr", "ggplot2", "ggrepel", "cowplot"), groundhog_date)

# Load data from Schleider et al. (2022) OSF project (https://osf.io/8mk6x )

dat <- readRDS("cleaned_cope_data_randomized.rds")

# ---------------------------------------------------------------------------- #
# Confirm scoring ----
# ---------------------------------------------------------------------------- #

# Document column names for SHS items at baseline (all 6 SHS items), postintervention
# (presumably the 3 SHS-Pathways subscale items), and follow-up (presumably the 3 SHS-
# Pathways items)
# - SHS-Pathways items are odd-numbered items in Snyder et al. (1996; https://doi.org/fwc2xc )

b_shs_item_cols  <- paste0("b_shs_", 1:6)
pi_shs_item_cols <- paste0("pi_shs_", c(1, 3, 5))
f1_shs_item_cols <- paste0("f1_shs_", c(1, 3, 5))

shs_item_cols <- c(b_shs_item_cols, pi_shs_item_cols, f1_shs_item_cols)

stopifnot(
  setequal(grep("_shs_\\d$", names(dat), value = TRUE), shs_item_cols)
)

# Confirm how SHS mean columns were computed at each time point

stopifnot(
  # Baseline mean computed from all 6 SHS items
  identical(dat$b_shs_mean,  rowMeans(dat[b_shs_item_cols], na.rm = TRUE)),
  
  # Postintervention mean computed from only 3 SHS-Pathways items
  identical(dat$pi_shs_mean, rowMeans(dat[pi_shs_item_cols], na.rm = TRUE)),
  
  # Follow-up mean computed from only 3 SHS-Pathways items
  identical(dat$f1_shs_mean, rowMeans(dat[f1_shs_item_cols], na.rm = TRUE))
)

# ---------------------------------------------------------------------------- #
# Compare SHS overall score with SHS-Pathways score at baseline ----
# ---------------------------------------------------------------------------- #

# Compute SHS-Pathways score at baseline

b_shs_pathways_item_cols <- paste0("b_shs_", c(1, 3, 5))

dat$b_shs_pathways_mean <- rowMeans(dat[b_shs_pathways_item_cols], na.rm = TRUE)

# Create condition label
# - Per Lines 354-361 of "cope_mcm_main_analyses_rmarkdown.Rmd" from Schleider et al. (2022) OSF project

dat <- dat %>% mutate(condition_label = case_when(
  condition == "0" ~ "Placebo Control",
  condition == "1" ~ "Project Personality",
  condition == "2" ~ "Project ABC",
  TRUE ~ NA_character_
))

# Compute correlation between SHS overall score and SHS-Pathways at baseline

round(cor(dat$b_shs_mean, dat$b_shs_pathways_mean), 2) == .87

# Compute means and SDs for given score by condition

## Define function

compute_desc <- function(dat, score, timepoint, score_label) {
  desc <- dat %>%
    group_by(condition_label) %>%
    summarise(
      mean = mean({{ score }} , na.rm = TRUE),
      sd   = sd({{ score }}, na.rm = TRUE),
    ) %>%
    mutate(time = timepoint,
           score = score_label)
  
  return(desc)
}

## Run function for each score

b_shs_mean_desc          <- compute_desc(dat, b_shs_mean, "b", "Baseline SHS Overall")
b_shs_pathways_mean_desc <- compute_desc(dat, b_shs_pathways_mean, "b", "Baseline SHS-Pathways")

## Document means and SDs for each score at baseline

stopifnot(
  # SHS values below match those in Table 3 for Placebo, BA-SSI, and GM-SSI
  
  round(b_shs_mean_desc$mean, 2)          == c(3.88, 3.93, 3.96),
  round(b_shs_mean_desc$sd, 2)            == c(1.39, 1.40, 1.44),
  
  # SHS-Pathways values below are higher in each group
  
  round(b_shs_pathways_mean_desc$mean, 2) == c(4.60, 4.58, 4.63),
  round(b_shs_pathways_mean_desc$sd, 2)   == c(1.49, 1.44, 1.48)
)

## Also run function for SHS-Pathways scores at postintervention and follow-up

pi_shs_mean_desc <- compute_desc(dat, pi_shs_mean, "pi", "Post SHS-Pathways (From Raw Data)")
f1_shs_mean_desc <- compute_desc(dat, f1_shs_mean, "f1", "Follow-Up SHS-Pathways (From Raw Data)")

## Document means and SDs for each score at postintervention and follow-up

stopifnot(
  # SHS-Pathways values below don't match those in Table 3 (see reason below)

  round(pi_shs_mean_desc$mean, 2) == c(5.29, 5.67, 5.52),
  round(pi_shs_mean_desc$sd, 2)   == c(1.54, 1.43, 1.46),
  
  round(f1_shs_mean_desc$mean, 2) == c(4.99, 5.11, 5.19),
  round(f1_shs_mean_desc$sd, 2)   == c(1.61, 1.60, 1.64)
)

### Values don't match those in Table 3 likely because Table 3 used imputed data 
### for postintervention and follow-up values
### - See Line 1539 onward of "cope_mcm_main_analyses_rmarkdown.Rmd"

stopifnot(
  sum(is.na(dat$b_shs_mean))  == 0,    # Complete scores at baseline
  sum(is.na(dat$pi_shs_mean)) == 452,  # Missing scores at postintervention
  sum(is.na(dat$f1_shs_mean)) == 994   # Missing scores at follow-up
)

# ---------------------------------------------------------------------------- #
# Plot scores ----
# ---------------------------------------------------------------------------- #

# Combine rows into table for plotting

shs_tbl <- rbind(b_shs_mean_desc, b_shs_pathways_mean_desc,
                 pi_shs_mean_desc, f1_shs_mean_desc)

# Add rows for SHS-Pathways at postintervention and follow-up from Table 3

condition_labels <- c("Placebo Control", "Project ABC", "Project Personality")

shs_tbl3_pi_f1 <- tibble(
  condition_label = rep(condition_labels, 2),
  mean = c(5.26, 5.68, 5.50, 4.89, 5.06, 5.17),
  sd   = c(1.52, 1.43, 1.48, 1.64, 1.63, 1.65),
  time = c(rep("pi", 3), rep("f1", 3)),
  score = c(rep("Post SHS-Pathways (From Table 3)", 3),
            rep("Follow-Up SHS-Pathways (From Table 3)", 3))
)

shs_tbl <- bind_rows(shs_tbl, shs_tbl3_pi_f1)

# Create tables for plotting

shs_tbl_a <- shs_tbl[shs_tbl$score %in% c("Baseline SHS Overall",
                                          "Post SHS-Pathways (From Table 3)",
                                          "Follow-Up SHS-Pathways (From Table 3)"), ]
shs_tbl_b <- shs_tbl[shs_tbl$score %in% c("Baseline SHS-Pathways",
                                          "Post SHS-Pathways (From Table 3)",
                                          "Follow-Up SHS-Pathways (From Table 3)"), ]

shs_tbl_c <- shs_tbl[shs_tbl$score %in% c("Baseline SHS Overall",
                                          "Post SHS-Pathways (From Raw Data)",
                                          "Follow-Up SHS-Pathways (From Raw Data)"), ]
shs_tbl_d <- shs_tbl[shs_tbl$score %in% c("Baseline SHS-Pathways",
                                          "Post SHS-Pathways (From Raw Data)",
                                          "Follow-Up SHS-Pathways (From Raw Data)"), ]

# Create plots

## Define function

create_plot <- function(tbl, title) {
  tbl %>% 
    mutate(time = factor(time,
                         levels = c("b", "pi", "f1"),
                         labels = c("Baseline", "Post", "Follow-up"))) %>%
    ggplot(aes(x = time,
               y = mean,
               color = condition_label,
               group = condition_label)) +
    scale_y_continuous(breaks = 1:8,
                       limits = c(1, 8)) +
    geom_line() +
    geom_point() +
    geom_text_repel(aes(label = round(mean, 2)),
                    show.legend = FALSE,
                    seed = 1234) +
    labs(title = title,
         x = "Time",
         y = "Score",
         color = "Condition") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11),
          panel.grid.minor = element_blank())
}

## Run function

p_a <- create_plot(shs_tbl_a, "BL SHS Overall (and Post/FU SHS-Pathways from Table 3)")
p_b <- create_plot(shs_tbl_b, "BL SHS-Pathways (and Post/FU SHS-Pathways from Table 3)")
p_c <- create_plot(shs_tbl_c, "BL SHS Overall (and Post/FU SHS-Pathways from Raw Data)")
p_d <- create_plot(shs_tbl_d, "BL SHS-Pathways (and Post/FU SHS-Pathways from Raw Data)")

p_all <- plot_grid(p_a, p_b, p_c, p_d, labels = LETTERS[1:4])

## Export plot grid to PDF

evaluate_shs_scoring_results_dir <- file.path("evaluate_shs_scoring", "results")
dir.create(evaluate_shs_scoring_results_dir)

ggsave2(file.path(evaluate_shs_scoring_results_dir, "evaluate_shs_scoring_in_schleider_et_al_2022_plots.pdf"),
        plot = p_all,
        width = 10, height = 10)

# ---------------------------------------------------------------------------- #
# Compute approximate within-group effect sizes ----
# ---------------------------------------------------------------------------- #

# Define function to compute within-group Cohen's d
# - Defined as (mean at Time 2 - mean at Time 1) / SD at Time 1
# - Note: Schleider et al. (2022) paper used a different method
#   - See "cope_mcm_lm_tables_rmarkdown.Rmd" from Schleider et al. (2022) OSF project

wtn_d <- function(shs_tbl, cond_lbl, t1_score_lbl, t2_score_lbl) {
  # Extract relevant means and baseline SD
  
  m_t1 <- shs_tbl$mean[shs_tbl$condition_label == cond_lbl & shs_tbl$score == t1_score_lbl]
  m_t2 <- shs_tbl$mean[shs_tbl$condition_label == cond_lbl & shs_tbl$score == t2_score_lbl]
  
  sd_t1 <- shs_tbl$sd[shs_tbl$condition_label == cond_lbl & shs_tbl$score == t1_score_lbl]

  # Compute d
  
  wtn_d <- (m_t2 - m_t1) / sd_t1
  
  return(wtn_d)
}

# Run function to compute within-group effect sizes at postintervention

abc_lbl  <- "Project ABC"
pers_lbl <- "Project Personality"
cont_lbl <- "Placebo Control"

## Using postintervention mean from Table 3

post_tbl3_lbl <- "Post SHS-Pathways (From Table 3)"

stopifnot(
  # Using Baseline SHS Overall score (within-group effect sizes not reported in Schleider et al., 2022)
  
  round(wtn_d(shs_tbl, abc_lbl,  "Baseline SHS Overall",  post_tbl3_lbl), 2) == 1.25,
  round(wtn_d(shs_tbl, pers_lbl, "Baseline SHS Overall",  post_tbl3_lbl), 2) == 1.07,
  round(wtn_d(shs_tbl, cont_lbl, "Baseline SHS Overall",  post_tbl3_lbl), 2) == 0.99,
  
  # Using Baseline SHS-Pathways score (substantially smaller than above)
  
  round(wtn_d(shs_tbl, abc_lbl,  "Baseline SHS-Pathways", post_tbl3_lbl), 2) == 0.76,
  round(wtn_d(shs_tbl, pers_lbl, "Baseline SHS-Pathways", post_tbl3_lbl), 2) == 0.59,
  round(wtn_d(shs_tbl, cont_lbl, "Baseline SHS-Pathways", post_tbl3_lbl), 2) == 0.45
)

## Using postintervention mean from raw data (similar results as above)

post_raw_lbl  <- "Post SHS-Pathways (From Raw Data)"

stopifnot(
  # Using Baseline SHS Overall score
  
  round(wtn_d(shs_tbl, abc_lbl,  "Baseline SHS Overall",  post_raw_lbl), 2) == 1.25,
  round(wtn_d(shs_tbl, pers_lbl, "Baseline SHS Overall",  post_raw_lbl), 2) == 1.08,
  round(wtn_d(shs_tbl, cont_lbl, "Baseline SHS Overall",  post_raw_lbl), 2) == 1.02,
  
  # Using Baseline SHS-Pathways score
  
  round(wtn_d(shs_tbl, abc_lbl,  "Baseline SHS-Pathways", post_raw_lbl), 2) == 0.76,
  round(wtn_d(shs_tbl, pers_lbl, "Baseline SHS-Pathways", post_raw_lbl), 2) == 0.60,
  round(wtn_d(shs_tbl, cont_lbl, "Baseline SHS-Pathways", post_raw_lbl), 2) == 0.47
)

# ---------------------------------------------------------------------------- #
# Compute approximate between-group effect sizes ----
# ---------------------------------------------------------------------------- #

# Define function to compute between-group Cohen's d
# - Defined as (Condition 2 mean at Time 2 - Condition 1 mean at Time 2) / pooled SD at Time 1
# - Note: Schleider et al. (2022) paper used a different method
#   - See "cope_mcm_lm_tables_rmarkdown.Rmd" from Schleider et al. (2022) OSF project

btw_d <- function(dat, shs_tbl, cond1_lbl, cond2_lbl, t1_score_lbl, t2_score_lbl) {
  # Extract relevant means, baseline SDs, and sample sizes
  
  m_1 <- shs_tbl$mean[shs_tbl$condition_label == cond1_lbl & shs_tbl$score == t2_score_lbl]
  m_2 <- shs_tbl$mean[shs_tbl$condition_label == cond2_lbl & shs_tbl$score == t2_score_lbl]
  
  bl_sd_1 <- shs_tbl$sd[shs_tbl$condition_label == cond1_lbl & shs_tbl$score == t1_score_lbl]
  bl_sd_2 <- shs_tbl$sd[shs_tbl$condition_label == cond2_lbl & shs_tbl$score == t1_score_lbl]
  
  n_1 <- sum(dat$condition_label == cond1_lbl)
  n_2 <- sum(dat$condition_label == cond2_lbl)
  
  # Compute pooled baseline SD
  
  bl_sd_pooled <- sqrt(((n_1 - 1)*bl_sd_1^2 + (n_2 - 1)*bl_sd_2^2) / (n_1 + n_2 - 2))
  
  # Compute d
  
  btw_d <- (m_2 - m_1) / bl_sd_pooled
  
  return(btw_d)
}

# Run function to compute between-group effect sizes at postintervention

## Using postintervention mean from Table 3

stopifnot(
  # Using baseline SD for SHS Overall score (similar to -0.31 and -0.15 on p. 261 of Schleider et al., 2022)
  
  round(btw_d(dat, shs_tbl, abc_lbl,  cont_lbl, "Baseline SHS Overall",  post_tbl3_lbl), 2) == -0.30,
  round(btw_d(dat, shs_tbl, pers_lbl, cont_lbl, "Baseline SHS Overall",  post_tbl3_lbl), 2) == -0.17,
  
  # Using baseline SD for SHS-Pathways score (similar results as above)
  
  round(btw_d(dat, shs_tbl, abc_lbl,  cont_lbl, "Baseline SHS-Pathways", post_tbl3_lbl), 2) == -0.29,
  round(btw_d(dat, shs_tbl, pers_lbl, cont_lbl, "Baseline SHS-Pathways", post_tbl3_lbl), 2) == -0.16
)

## Using postintervention mean from raw data (similar results as above)

stopifnot(
  # Using baseline SD for SHS Overall score
  
  round(btw_d(dat, shs_tbl, abc_lbl,  cont_lbl, "Baseline SHS Overall",  post_raw_lbl), 2) == -0.27,
  round(btw_d(dat, shs_tbl, pers_lbl, cont_lbl, "Baseline SHS Overall",  post_raw_lbl), 2) == -0.16,
  
  # Using baseline SD for SHS-Pathways score
  
  round(btw_d(dat, shs_tbl, abc_lbl,  cont_lbl, "Baseline SHS-Pathways", post_raw_lbl), 2) == -0.26,
  round(btw_d(dat, shs_tbl, pers_lbl, cont_lbl, "Baseline SHS-Pathways", post_raw_lbl), 2) == -0.15
)