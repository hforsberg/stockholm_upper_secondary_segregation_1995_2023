###############################################################
# 05_ols_entropy_share_gap_figure_4.R
#
# Purpose:
#   - Construct delta outcomes (programme – neighbourhood) for:
#       * Entropy measures
#       * Share measures
#   - Run year-by-year OLS regressions for a set of outcomes.
#   - Extract various focal coefficient across years and plot:
#       * raw coefficients (with 95% CI)
#       * standardised coefficients (beta; with 95% CI)
#   - Includes a template for generating control plots and a panel figure.
#   - Includes code to export appendix tables (4-year snapshots) to HTML + DOCX.
#
# Inputs:
#   - data/final/sthlm_95_23_final_new.csv
#     One row per student-year with programme and neighbourhood composition measures.
#
# Outputs (optional, depending on save flags / paths):
#   - plots_final/  (figures)
#   - out_tables/ols_table  (appendix tables)
#
###############################################################

## ------------------------------------------------------------
## 0. Load packages ####
## ------------------------------------------------------------

{
  library(tidyverse)
  library(data.table)
  library(broom)
  library(ggplot2)
  library(RColorBrewer)
  library(stringr)
  library(modelsummary)
  library(fixest)
  library(officer)
}

## ------------------------------------------------------------
## 1. Load data + apply analysis filters ####
## ------------------------------------------------------------

sthlm_95_23 <- fread("data/final/sthlm_95_23_final_new.csv", encoding = "UTF-8") %>%
  as_tibble() %>%
  filter(n_deso >= 5, keep_prog_10 == 1)

## -----------------------------------------------------------------
## 2. Harmonise analysis frame (keep only variables used below) ####
## -----------------------------------------------------------------

df <- sthlm_95_23 %>%
  mutate(across(where(~ inherits(.x, "integer64")), as.numeric)) %>%
  transmute(
    id_s,
    year = as.integer(year),
    
    # IDs
    deso_home,
    gym_id_prog,
    school_id = new_id_2,
    
    # Individual covariates
    student_foreign   = foreign_back,
    student_sex       = sex,
    student_prior_gpa = gpa_y9,
    student_lowinc    = if_else(inc_3groups == "Low_income",  1L, 0L),
    student_highinc   = if_else(inc_3groups == "High_income", 1L, 0L),
    parent_lowedu     = if_else(edu_3groups == "Low_education",  1L, 0L),
    parent_highedu    = if_else(edu_3groups == "High_education", 1L, 0L),
    
    # Neighbourhood shares
    neigh_n_unit,
    neigh_share_foreign,
    neigh_share_lowinc,
    neigh_share_highinc,
    neigh_share_lowedu,
    neigh_share_highedu,
    
    # Program shares
    prog_share_foreign,
    prog_share_lowinc,
    prog_share_highinc,
    prog_share_lowedu,
    prog_share_highedu,
    
    # Entropy variables
    prog_ent_foreign,
    prog_ent_lowinc,
    prog_ent_highinc,
    prog_ent_lowedu,
    prog_ent_highedu,
    
    deso_ent_foreign,
    deso_ent_lowinc,
    deso_ent_highinc,
    deso_ent_lowedu,
    deso_ent_highedu
  )

## ------------------------------------------------------------
## 3. Build delta outcomes ####
## ------------------------------------------------------------

# Entropy gaps: programme minus neighbourhood
df <- df %>%
  mutate(
    delta_fmix  = prog_ent_foreign - deso_ent_foreign,
    delta_limix = prog_ent_lowinc  - deso_ent_lowinc,
    delta_himix = prog_ent_highinc - deso_ent_highinc,
    delta_lemix = prog_ent_lowedu  - deso_ent_lowedu,
    delta_hemix = prog_ent_highedu - deso_ent_highedu
  )

# Share gaps: programme minus neighbourhood (default)
SHARE_CONTEXT <- "prog"   # change to "school" ONLY if you have school_share_* in df

df <- df %>%
  mutate(
    delta_fshare  = if (SHARE_CONTEXT == "prog") prog_share_foreign - neigh_share_foreign else NA_real_,
    delta_lishare = if (SHARE_CONTEXT == "prog") prog_share_lowinc  - neigh_share_lowinc  else NA_real_,
    delta_hishare = if (SHARE_CONTEXT == "prog") prog_share_highinc - neigh_share_highinc else NA_real_,
    delta_leshare = if (SHARE_CONTEXT == "prog") prog_share_lowedu  - neigh_share_lowedu  else NA_real_,
    delta_heshare = if (SHARE_CONTEXT == "prog") prog_share_highedu - neigh_share_highedu else NA_real_
  )

if (SHARE_CONTEXT != "prog") {
  message("NOTE: SHARE_CONTEXT != 'prog'. You must add school_share_* variables and define the SCHOOL-NEIGH deltas.")
}

## ------------------------------------------------------------
## 4. Sanity checks ####
## ------------------------------------------------------------

df %>%
  summarise(
    miss_neigh_foreign = mean(is.na(neigh_share_foreign)),
    miss_prog_foreign  = mean(is.na(prog_share_foreign)),
    rng_neigh_foreign  = paste(range(neigh_share_foreign, na.rm = TRUE), collapse = "–"),
    rng_prog_foreign   = paste(range(prog_share_foreign,  na.rm = TRUE), collapse = "–"),
    max_prog_ent       = max(prog_ent_foreign, na.rm = TRUE),
    max_deso_ent       = max(deso_ent_foreign, na.rm = TRUE)
  ) %>%
  print()

## ------------------------------------------------------------
## 5. Year-by-year OLS models ####
## ------------------------------------------------------------

run_lm_by_year <- function(data, fml) {
  data %>%
    group_split(year) %>%
    map(\(d) {
      y <- unique(d$year)[1]
      list(year = y, model = lm(fml, data = d))
    })
}

rhs_common <- quote(
  student_foreign + student_sex +
    parent_lowedu + parent_highedu + student_lowinc + student_highinc + # Plotta KONTROLL VARIABLER
    student_prior_gpa
)

rhs_neigh_control <- list(
  foreign = quote(neigh_share_foreign),
  lowinc  = quote(neigh_share_lowinc),
  highinc = quote(neigh_share_highinc),
  lowedu  = quote(neigh_share_lowedu),
  highedu = quote(neigh_share_highedu)
)

specs <- tibble::tribble(
  ~outcome,        ~dim,      ~label,
  "delta_fmix",    "foreign", "Entropy – Foreign",
  "delta_fshare",  "foreign", "Share – Foreign",
  
  "delta_limix",   "lowinc",  "Entropy – Low income",
  "delta_lishare", "lowinc",  "Share – Low income",
  
  "delta_himix",   "highinc", "Entropy – High income",
  "delta_hishare", "highinc", "Share – High income",
  
  "delta_lemix",   "lowedu",  "Entropy – Low education",
  "delta_leshare", "lowedu",  "Share – Low education",
  
  "delta_hemix",   "highedu", "Entropy – High education",
  "delta_heshare", "highedu", "Share – High education"
)

models <- specs %>%
  mutate(
    formula = pmap(
      list(outcome, dim),
      function(outcome, dim) {
        as.formula(paste0(
          outcome, " ~ ",
          deparse(rhs_neigh_control[[dim]]), " + ",
          paste(deparse(rhs_common), collapse = "")
        ))
      }
    ),
    fitted = map(formula, ~ run_lm_by_year(df, .x))
  )

## ------------------------------------------------------------
## 5b. Inspect individual model summaries (optional) ####
## ------------------------------------------------------------

models %>%
  mutate(spec_id = row_number()) %>%
  select(spec_id, label)

spec_i <- 3 # choose which model to look at
years_keep <- c(1995, 2005, 2015, 2023) # choose years

models$fitted[[spec_i]] %>%
  purrr::keep(~ .x$year %in% years_keep) %>%
  purrr::walk(~ {
    cat("\n====================\nYear:", .x$year, "\n")
    print(summary(.x$model))
  })

# Summary across years: R2 values
spec_label <- "Share – Low income" # choose
spec_i <- which(specs$label == spec_label)[1]

models$fitted[[spec_i]] %>%
  purrr::map_dfr(~ broom::glance(.x$model) %>%
                   mutate(year = .x$year)) %>%
  arrange(year) %>%
  select(year, nobs, r.squared, adj.r.squared)

## ------------------------------------------------------------
## 6. Extract coefficient-by-year (raw scale) ####
## ------------------------------------------------------------

extract_coeff_by_year <- function(model_list, term_keep, model_name) {
  purrr::map_dfr(model_list, function(obj) {
    broom::tidy(obj$model) %>%
      dplyr::filter(term == term_keep) %>%
      dplyr::mutate(
        year = as.integer(obj$year),
        conf.low  = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error,
        model = model_name
      )
  })
}

# Choose focal coefficient to visualise
focal_term <- "student_prior_gpa"

coef_df <- pmap_dfr(
  list(models$fitted, models$label),
  \(fitted_list, label) extract_coeff_by_year(fitted_list, term_keep = focal_term, model_name = label)
) %>%
  mutate(
    outcome_type = if_else(str_detect(model, "^Entropy"), "Entropy", "Share"),
    year = as.integer(year)
  )

stopifnot("year" %in% names(coef_df))
dplyr::count(coef_df, model) %>% print()

## ------------------------------------------------------------
## 6b. Standardised coefficients (beta) by year ####
## ------------------------------------------------------------
# beta_std = b * sd(x) / sd(y), computed within each year and outcome

get_sd_xy_by_year <- function(data, outcome, x_var) {
  data %>%
    group_by(year) %>%
    summarise(
      sd_x = sd(.data[[x_var]], na.rm = TRUE),
      sd_y = sd(.data[[outcome]], na.rm = TRUE),
      .groups = "drop"
    )
}

sd_df <- specs %>%
  distinct(outcome, label) %>%
  mutate(sd_tbl = map(outcome, ~ get_sd_xy_by_year(df, outcome = .x, x_var = focal_term))) %>%
  unnest(sd_tbl)

coef_df_std <- coef_df %>%
  left_join(specs %>% select(outcome, label), by = c("model" = "label")) %>%
  left_join(sd_df %>% select(outcome, year, sd_x, sd_y), by = c("outcome", "year")) %>%
  mutate(
    estimate_std  = estimate  * (sd_x / sd_y),
    conf.low_std  = conf.low  * (sd_x / sd_y),
    conf.high_std = conf.high * (sd_x / sd_y)
  )

## ------------------------------------------------------------
## 7. Plot styling ####
## ------------------------------------------------------------

year_min <- 1995
year_max <- 2023
x_breaks <- sort(unique(c(year_min, seq(year_min, year_max, by = 2))))

palette_name <- "Set1"
pal5 <- RColorBrewer::brewer.pal(5, palette_name)

col_dim <- c(
  "Foreign"        = pal5[1],
  "Low income"     = pal5[2],
  "High income"    = pal5[3],
  "Low education"  = pal5[4],
  "High education" = pal5[5]
)

theme_panel <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.25),
      plot.title = element_text(face = "bold", size = 15),
      plot.title.position = "plot",
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 13)
    )
}

plot_coef_panels <- function(dat,
                             type = c("Entropy","Share"),
                             focal_term,
                             year_min = 1995,
                             year_max = 2023,
                             x_breaks = NULL,
                             line_w = 1.0,
                             point_s = 1.8,
                             col_dim,
                             title = NULL,
                             subtitle = NULL,
                             ncol = 5,
                             use_std = FALSE,
                             y_limits = NULL) {
  
  type <- match.arg(type)
  
  y_est  <- if (use_std) "estimate_std"  else "estimate"
  y_low  <- if (use_std) "conf.low_std"  else "conf.low"
  y_high <- if (use_std) "conf.high_std" else "conf.high"
  
  dd <- dat %>%
    filter(outcome_type == type) %>%
    filter(year >= year_min, year <= year_max) %>%
    mutate(
      year = as.integer(year),
      dim  = str_remove(model, "^(Entropy|Share)\\s*–\\s*")
    )
  
  p <- ggplot(dd, aes(x = year, colour = dim, fill = dim)) +
    geom_ribbon(aes(ymin = .data[[y_low]], ymax = .data[[y_high]]),
                alpha = 0.18, colour = NA) +
    geom_line(aes(y = .data[[y_est]]), linewidth = line_w) +
    geom_point(aes(y = .data[[y_est]]), size = point_s) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
    facet_wrap(~ model, ncol = ncol) +
    scale_x_continuous(
      limits = c(year_min, year_max),
      breaks = x_breaks,
      minor_breaks = NULL
    ) +
    scale_colour_manual(values = col_dim, drop = FALSE) +
    scale_fill_manual(values = col_dim, drop = FALSE) +
    labs(
      x = "Year",
      y = if (use_std) {
        paste0("Standardised coefficient (β) for ", focal_term)
      } else {
        paste0("OLS coefficient for ", focal_term)
      },
      title = title,
      subtitle = subtitle
    ) +
    theme_panel() +
    guides(colour = "none", fill = "none")
  
  return(p)
}

# Define y-scale (optional template)
y_lim_common <- c(-0.05, 0.08)

## ------------------------------------------------------------
## 8. Main plots: GPA (raw + standardised) ####
## ------------------------------------------------------------

p_entropy <- plot_coef_panels(
  coef_df,
  type = "Entropy",
  focal_term = focal_term,
  year_min = year_min,
  year_max = year_max,
  x_breaks = x_breaks,
  col_dim = col_dim,
  title = "Association between GPA and entropy-based outcomes over time",
  subtitle = "Year-by-year models with OLS coefficients (95% CI)",
  ncol = 5,
  use_std = FALSE
)

p_share <- plot_coef_panels(
  coef_df,
  type = "Share",
  focal_term = focal_term,
  year_min = year_min,
  year_max = year_max,
  x_breaks = x_breaks,
  col_dim = col_dim,
  title = "Association between GPA and share-based outcomes over time",
  subtitle = "Year-by-year models with OLS coefficients (95% CI)",
  ncol = 5,
  use_std = FALSE
)

p_entropy_std <- plot_coef_panels(
  coef_df_std,
  type = "Entropy",
  focal_term = focal_term,
  year_min = year_min,
  year_max = year_max,
  x_breaks = x_breaks,
  col_dim = col_dim,
  title = "Association between GPA and entropy-based outcomes over time (standardised betas)",
  subtitle = "Year-by-year models with standardised coefficients (95% CI)",
  ncol = 5,
  use_std = TRUE
)

p_share_std <- plot_coef_panels(
  coef_df_std,
  type = "Share",
  focal_term = focal_term,
  year_min = year_min,
  year_max = year_max,
  x_breaks = x_breaks,
  col_dim = col_dim,
  title = "Association between GPA and share-based outcomes over time (standardised betas)",
  subtitle = "Year-by-year models with standardised coefficients (95% CI)",
  ncol = 5,
  use_std = TRUE
)

print(p_entropy)
print(p_share)
print(p_entropy_std)
print(p_share_std)

## ------------------------------------------------------------
## 9. Save figures (optional) ####
## ------------------------------------------------------------

save_dir <- "plots_final/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

{
  ggplot2::ggsave(file.path(save_dir, "f4_p_entropy_gpa.png"), p_entropy, dpi = 400, width = 16, height = 8, units = "in")
  ggplot2::ggsave(file.path(save_dir, "f4_p_entropy_gpa.pdf"), p_entropy, width = 16, height = 8, units = "in", device = grDevices::cairo_pdf)
  
  ggplot2::ggsave(file.path(save_dir, "f4_p_share_gpa.png"), p_share, dpi = 400, width = 16, height = 8, units = "in")
  ggplot2::ggsave(file.path(save_dir, "f4_p_share_gpa.pdf"), p_share, width = 16, height = 8, units = "in", device = grDevices::cairo_pdf)
  
  ggplot2::ggsave(file.path(save_dir, "f4_p_entropy_std_gpa.png"), p_entropy_std, dpi = 400, width = 16, height = 8, units = "in")
  ggplot2::ggsave(file.path(save_dir, "f4_p_entropy_std_gpa.pdf"), p_entropy_std, width = 16, height = 8, units = "in", device = grDevices::cairo_pdf)
  
  ggplot2::ggsave(file.path(save_dir, "f4_p_share_std_gpa.png"), p_share_std, dpi = 400, width = 16, height = 8, units = "in")
  ggplot2::ggsave(file.path(save_dir, "f4_p_share_std_gpa.pdf"), p_share_std, width = 16, height = 8, units = "in", device = grDevices::cairo_pdf)
}

## -------------------------------------------------------
## CONTROL PLOTS (TEMPLATE) — copy/paste per control ####
## -------------------------------------------------------

#student_sex2
#student_prior_gpa
#student_foreign
#parent_lowedu
#parent_highedu
#student_lowinc
#student_highinc

#Nhood
# neigh_share_foreign
# Neighbourhood share foreign background

#Labels
#"Male (ref. female)"
#prior achievement (Grade 9 GPA)
#foreign background (ref. Swedish)
#low parental education
#high parental education
#low household income
#high household income
# ---------------------------
# A) Choose control to plot
# ---------------------------
FOCAL_TERM  <- "neigh_share_highedu"               # <-- CHANGE HERE (e.g., parent_lowedu)
FOCAL_LABEL <- "Neighbourhood share high income"   # <-- CHANGE HERE

save_dir <- "plots_final/Figure_4/foreign_nhood/"  # <-- CHANGE HERE
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

# ---------------------------
# B) Extract coefficient-by-year (raw)
# ---------------------------
coef_df_ctrl <- pmap_dfr(
  list(models$fitted, models$label),
  \(fitted_list, label)
  extract_coeff_by_year(
    fitted_list,
    term_keep  = FOCAL_TERM,
    model_name = label
  )
) %>%
  mutate(
    outcome_type = if_else(str_detect(model, "^Entropy"), "Entropy", "Share"),
    year = as.integer(year)
  )

if (nrow(coef_df_ctrl) == 0) {
  message("No rows extracted for ", FOCAL_TERM, ". Term not estimated in any model/year.")
} else {
  
  # ---------------------------
  # C) Standardised coefficients (beta) for this control
  # ---------------------------
  get_sd_xy_by_year <- function(data, outcome, x_var) {
    data %>%
      group_by(year) %>%
      summarise(
        sd_x = sd(.data[[x_var]], na.rm = TRUE),
        sd_y = sd(.data[[outcome]], na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  sd_df_ctrl <- specs %>%
    distinct(outcome, label) %>%
    mutate(sd_tbl = map(outcome, ~ get_sd_xy_by_year(df, outcome = .x, x_var = FOCAL_TERM))) %>%
    unnest(sd_tbl)
  
  coef_df_ctrl_std <- coef_df_ctrl %>%
    left_join(specs %>% select(outcome, label), by = c("model" = "label")) %>%
    left_join(sd_df_ctrl %>% select(outcome, year, sd_x, sd_y), by = c("outcome", "year")) %>%
    mutate(
      estimate_std  = estimate  * (sd_x / sd_y),
      conf.low_std  = conf.low  * (sd_x / sd_y),
      conf.high_std = conf.high * (sd_x / sd_y)
    )
  
  # ---------------------------
  # D) Plots (raw)
  # ---------------------------
  p_entropy_ctrl <- plot_coef_panels(
    coef_df_ctrl,
    type = "Entropy",
    focal_term = FOCAL_TERM,
    year_min = year_min,
    year_max = year_max,
    x_breaks = x_breaks,
    col_dim = col_dim,
    title = paste0("Association between ", FOCAL_LABEL, " and entropy-based outcomes over time"),
    subtitle = "Year-by-year models with OLS coefficients (95% CI)",
    ncol = 5,
    use_std = FALSE
  )
  
  p_share_ctrl <- plot_coef_panels(
    coef_df_ctrl,
    type = "Share",
    focal_term = FOCAL_TERM,
    year_min = year_min,
    year_max = year_max,
    x_breaks = x_breaks,
    col_dim = col_dim,
    title = paste0("Association between ", FOCAL_LABEL, " and share-based outcomes over time"),
    subtitle = "Year-by-year models with OLS coefficients (95% CI)",
    ncol = 5,
    use_std = FALSE
  )
  
  # ---------------------------
  # E) Plots (standardised)
  # ---------------------------
  p_entropy_ctrl_std <- plot_coef_panels(
    coef_df_ctrl_std,
    type = "Entropy",
    focal_term = FOCAL_TERM,
    year_min = year_min,
    year_max = year_max,
    x_breaks = x_breaks,
    col_dim = col_dim,
    title = paste0("Association between ", FOCAL_LABEL, " and entropy-based outcomes over time (standardised betas)"),
    subtitle = "Year-by-year models with standardised coefficients (95% CI)",
    ncol = 5,
    use_std = TRUE
  )
  
  p_share_ctrl_std <- plot_coef_panels(
    coef_df_ctrl_std,
    type = "Share",
    focal_term = FOCAL_TERM,
    year_min = year_min,
    year_max = year_max,
    x_breaks = x_breaks,
    col_dim = col_dim,
    title = paste0("Association between ", FOCAL_LABEL, " and share-based outcomes over time (standardised betas)"),
    subtitle = "Year-by-year models with standardised coefficients (95% CI)",
    ncol = 5,
    use_std = TRUE
  )
  
  # ---------------------------
  # F) Save (control name in filename)
  # ---------------------------
  stub <- paste0("f4_", FOCAL_TERM)
  
  ggsave(file.path(save_dir, paste0(stub, "_entropy.png")), p_entropy_ctrl,
         dpi = 400, width = 16, height = 8, units = "in")
  ggsave(file.path(save_dir, paste0(stub, "_entropy.pdf")), p_entropy_ctrl,
         width = 16, height = 8, units = "in", device = grDevices::cairo_pdf)
  
  ggsave(file.path(save_dir, paste0(stub, "_share.png")), p_share_ctrl,
         dpi = 400, width = 16, height = 8, units = "in")
  ggsave(file.path(save_dir, paste0(stub, "_share.pdf")), p_share_ctrl,
         width = 16, height = 8, units = "in", device = grDevices::cairo_pdf)
  
  #ggsave(file.path(save_dir, paste0(stub, "_entropy_std.png")), p_entropy_ctrl_std,
  #       dpi = 400, width = 16, height = 8, units = "in")
  #ggsave(file.path(save_dir, paste0(stub, "_entropy_std.pdf")), p_entropy_ctrl_std,
  #       width = 16, height = 8, units = "in", device = grDevices::cairo_pdf)
  #
  #ggsave(file.path(save_dir, paste0(stub, "_share_std.png")), p_share_ctrl_std,
  #       dpi = 400, width = 16, height = 8, units = "in")
  #ggsave(file.path(save_dir, paste0(stub, "_share_std.pdf")), p_share_ctrl_std,
  #       width = 16, height = 8, units = "in", device = grDevices::cairo_pdf)
}

print(p_entropy_ctrl)
print(p_share_ctrl)
print(p_entropy_ctrl_std)
print(p_share_ctrl_std)

## ==================================================
## Panel figure from saved PNGs  ####
## ==================================================

{
  library(tidyverse)
  library(patchwork)
  library(magick)
  library(ggplot2)
}

# --------------------------------------------------
# Paths (edit to match local setup)
# --------------------------------------------------
base_dir <- "D:/r_workspace/acta_swedish_exp_2025_enrolled/plots_final/Figure_4"

folders <- c("gpa", "foreign_back", "low_inc", "high_inc", "low_edu", "high_edu")

titles <- c(
  "Grade 9 GPA",
  "Foreign background",
  "Low income",
  "High income",
  "Low education",
  "High education"
)

pick_share_png <- function(folder_path) {
  files <- list.files(folder_path, pattern = "share.*\\.png$", full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[which.max(file.info(files)$mtime)]
}

share_paths <- file.path(base_dir, folders) %>%
  map_chr(pick_share_png)

print(tibble(folder = folders, file = share_paths))

if (any(is.na(share_paths))) {
  stop("Missing share png in: ",
       paste(folders[is.na(share_paths)], collapse = ", "))
}

img_to_gg <- function(path, title = NULL) {
  img <- magick::image_read(path)
  g <- ggplot() +
    annotation_custom(grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))) +
    coord_cartesian(clip = "off") +
    theme_void()
  
  if (!is.null(title)) {
    g <- g + ggtitle(title) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 6))
      )
  }
  g
}

plots <- map2(share_paths, titles, img_to_gg)

panel <- (plots[[1]] | plots[[2]]) /
  (plots[[3]] | plots[[4]]) /
  (plots[[5]] | plots[[6]]) +
  plot_annotation(
    title = "OLS coefficients over time (Share gap outcomes)",
    subtitle = "Year-by-year models; 95% confidence intervals",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    )
  )

panel

out_dir <- file.path(base_dir, "panel")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(out_dir, "Figure4_share_panel_2x3.png"),
       panel, width = 16, height = 16, units = "in", dpi = 400)

ggsave(file.path(out_dir, "Figure4_share_panel_2x3.pdf"),
       panel, width = 16, height = 16, units = "in", device = grDevices::cairo_pdf)

## ------------------------------------------------------------
## Appendix tables: 4-year snapshots (share-gap outcomes)  ####
## ------------------------------------------------------------

tab_dir <- file.path("out_tables/ols_table")
APPENDIX_YEARS <- c(1995, 2005, 2015, 2023)

if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)

rhs_common <- c(
  "student_foreign",
  "student_sex",
  "parent_lowedu",
  "parent_highedu",
  "student_lowinc",
  "student_highinc",
  "student_prior_gpa"
)

dims <- tibble::tribble(
  ~dim,         ~outcome,        ~neigh_term,            ~table_stub,           ~table_title,
  "Foreign",     "delta_fshare",  "neigh_share_foreign",  "A_foreign_share",     "Appendix Table A. Foreign background (share gap)",
  "Low income",  "delta_lishare", "neigh_share_lowinc",   "A_lowinc_share",      "Appendix Table A. Low income (share gap)",
  "High income", "delta_hishare", "neigh_share_highinc",  "A_highinc_share",     "Appendix Table A. High income (share gap)",
  "Low edu",     "delta_leshare", "neigh_share_lowedu",   "A_lowedu_share",      "Appendix Table A. Low parental education (share gap)",
  "High edu",    "delta_heshare", "neigh_share_highedu",  "A_highedu_share",     "Appendix Table A. High parental education (share gap)"
)

coef_map <- c(
  "neigh_share_foreign" = "Neighbourhood foreign share",
  "neigh_share_lowinc"  = "Neighbourhood low-income share",
  "neigh_share_highinc" = "Neighbourhood high-income share",
  "neigh_share_lowedu"  = "Neighbourhood low-education share",
  "neigh_share_highedu" = "Neighbourhood high-education share",
  
  "student_foreign"     = "Foreign background (ref. Swedish)",
  "student_sex"         = "Male (ref. female)",
  "student_prior_gpa"   = "Grade 9 GPA",
  "student_lowinc"      = "Low income (ref. mid)",
  "student_highinc"     = "High income (ref. mid)",
  "parent_lowedu"       = "Low parental education (ref. mid)",
  "parent_highedu"      = "High parental education (ref. mid)"
)

gof_map <- tibble::tribble(
  ~raw,            ~clean,        ~fmt,
  "nobs",          "N",           0,
  "r.squared",     "R\u00B2",      3,
  "adj.r.squared", "Adj. R\u00B2", 3
)

fit_models_4years <- function(outcome, neigh_term, years, data) {
  
  fml_txt <- paste0(
    outcome, " ~ ",
    neigh_term, " + ",
    paste(rhs_common, collapse = " + ")
  )
  fml <- as.formula(fml_txt)
  
  mods <- setNames(
    lapply(years, function(y) {
      d_sub <- data %>% dplyr::filter(year == y)
      lm(fml, data = d_sub)
    }),
    paste0(years)
  )
  
  mods
}

for (i in seq_len(nrow(dims))) {
  
  outcome    <- dims$outcome[i]
  neigh_term <- dims$neigh_term[i]
  
  out_html <- file.path(tab_dir, paste0(dims$table_stub[i], ".html"))
  out_docx <- file.path(tab_dir, paste0(dims$table_stub[i], ".docx"))
  
  message("\n--- Building table: ", dims$table_stub[i], " ---")
  
  tryCatch({
    
    mod_list <- fit_models_4years(
      outcome    = outcome,
      neigh_term = neigh_term,
      years      = APPENDIX_YEARS,
      data       = df
    )
    
    modelsummary(
      mod_list,
      title     = dims$table_title[i],
      coef_map  = coef_map,
      statistic = "conf.int",
      stars     = TRUE,
      gof_map   = gof_map,
      notes     = "Notes: Each column is an OLS regression estimated within year. 95% confidence intervals shown.",
      output    = out_html
    )
    
    ft <- modelsummary(
      mod_list,
      title     = dims$table_title[i],
      coef_map  = coef_map,
      gof_map   = gof_map,
      fmt       = 3,
      estimate  = "{estimate}{stars}\n({conf.low}, {conf.high})",
      statistic = NULL,
      notes     = "Notes: Each column is an OLS regression estimated within year. 95% confidence intervals shown.",
      output    = "flextable"
    )
    
    ft <- ft %>%
      flextable::autofit() %>%
      flextable::fontsize(size = 10, part = "all")
    
    flextable::save_as_docx(ft, path = out_docx)
    
    message("Saved:\n- ", normalizePath(out_html), "\n- ", normalizePath(out_docx))
    
  }, error = function(e) {
    
    message("FAILED for table_stub = ", dims$table_stub[i])
    message("Error message: ", conditionMessage(e))
    message("Tip: if DOCX is open, close it and re-run. Otherwise report the exact error text.")
    
  })
}

message("\nDone. Tables (attempted) in: ", normalizePath(tab_dir))
