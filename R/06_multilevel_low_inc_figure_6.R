################################################################################
# 06_multilevel_low_inc_figure_6.R
#
# Purpose:
#   Multilevel models for low-income mismatch:
#     delta_lishare = programme share low income − neighbourhood share low income
#
# Inputs:
#   - data/final/sthlm_95_23_final_new.csv
#
# Outputs (optional):
#   - out_tables/low_inc_tables/    (ICC + model tables)
#   - out_plots_multilevel/         (prediction plots)
#   - save_filer/low_inc/           (RDS checkpoints)
#
# Notes:
#   - Project-relative paths (assumes working directory is root).
#   - Filter early and keep variables tight (large-N friendly).
################################################################################

## ------------------------------------------------------------
## 0) Packages + settings ####
## ------------------------------------------------------------

{
  library(data.table)   # fast I/O
  library(tidyverse)    # dplyr/ggplot/stringr
  library(lme4)         # lmer
  library(lmerTest)     # p-values / df (optional)
  library(broom.mixed)  # tidy() for lmer
  library(modelsummary) # model tables
  library(splines)      # ns()
  library(scales)       # percent labels
  library(performance)  # ICC / R2 helpers (optional)
  library(flextable)
  library(officer)
}

data_path  <- "data/final/sthlm_95_23_final_new.csv"
out_dir_tbl <- "out_tables/low_inc_tables/"   # tables
out_dir_plt <- "out_plots_multilevel/"        # plots

dir.create(out_dir_tbl, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_plt, showWarnings = FALSE, recursive = TRUE)

# Filters (aligned with OLS scripts)
min_n_deso    <- 5
keep_prog_var <- "keep_prog_10"  # change to keep_prog_15 etc. if needed

## ------------------------------------------------------------
## 1) Load + keep only required variables (compact frame) ####
## ------------------------------------------------------------

giso <- fread(data_path, encoding = "UTF-8")
setnames(giso, tolower(names(giso)))

vars_keep <- c(
  # IDs / grouping
  "id_s","year","deso_home","new_id_2","prog",
  
  # Individual covariates
  "sex","foreign_back","g_lowinc","g_highinc","g_lowedu","g_highedu",
  "gpa_y9","prog_yp_hp","hman_2gr",
  
  # Neighbourhood shares
  "neigh_share_lowinc","neigh_share_foreign","neigh_share_lowedu",
  
  # Programme shares (for outcome)
  "prog_share_lowinc",
  
  # System / metro-level predictors
  "share_private_year","share_vocational_year",
  "ratio_cohort_to_options_year","cohort_size_year",
  "thiel_h_deso_inc","thiel_h_deso_fb",
  
  # Cell-size controls for filtering
  "n_deso", keep_prog_var
)

vars_keep <- intersect(vars_keep, names(giso))

df <- giso[, ..vars_keep] %>%
  as_tibble()

rm(giso); gc()

## ------------------------------------------------------------
## 2) Basic cleaning + filter early ####
## ------------------------------------------------------------

df <- df %>%
  mutate(across(where(~ inherits(.x, "integer64")), as.numeric))

df <- df %>%
  filter(.data$n_deso >= min_n_deso) %>%
  filter(.data[[keep_prog_var]] == 1 | .data[[keep_prog_var]] == TRUE)

df <- df %>%
  mutate(
    year = factor(year),
    
    # Outcome: mismatch (programme − neighbourhood)
    delta_lishare = prog_share_lowinc - neigh_share_lowinc
  )

## ------------------------------------------------------------
## 3) Factor coding (grouping + categorical covariates) ####
## ------------------------------------------------------------

df <- df %>%
  mutate(
    # Grouping variables must be factors in lmer()
    deso_home = factor(deso_home),
    new_id_2  = factor(new_id_2),
    prog      = factor(prog),
    
    # Individual covariates
    sex          = factor(sex),
    foreign_back = factor(foreign_back),
    prog_yp_hp   = factor(prog_yp_hp),
    hman_2gr     = factor(hman_2gr),
    
    # Indicators (as in colleague script)
    g_lowinc  = factor(g_lowinc),
    g_highinc = factor(g_highinc),
    g_lowedu  = factor(g_lowedu),
    g_highedu = factor(g_highedu)
  )

## ------------------------------------------------------------
## 4) Neighbourhood context (20/80 cutpoints -> 3 groups) ####
## ------------------------------------------------------------

q <- quantile(df$neigh_share_lowinc, probs = c(0, .20, .80, 1), na.rm = TRUE)

df <- df %>%
  mutate(
    neigh_lowinc_tertile = cut(
      neigh_share_lowinc,
      breaks = q,
      include.lowest = TRUE,
      labels = c("Low", "Medium", "High")
    ),
    neigh_lowinc_tertile = factor(neigh_lowinc_tertile, levels = c("Low","Medium","High"))
  )

## ------------------------------------------------------------
## 5) Optional: standardise continuous predictors ####
## ------------------------------------------------------------

vars_scale <- c(
  "gpa_y9",
  "thiel_h_deso_inc","thiel_h_deso_fb",
  "share_vocational_year",
  "ratio_cohort_to_options_year","cohort_size_year"
)

vars_scale <- intersect(vars_scale, names(df))

df <- df %>%
  mutate(across(all_of(vars_scale), ~ as.numeric(scale(.))))

# share_private_year often kept in [0,1] for interpretability
# df <- df %>% mutate(share_private_year = as.numeric(scale(share_private_year)))

## ------------------------------------------------------------
## 6) Multilevel models (stepwise) ####
## ------------------------------------------------------------

rand_part <- "(1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog)"

m0 <- lmer(
  delta_lishare ~ 1 +
    (1 | deso_home) +
    (1 | new_id_2) +
    (1 | new_id_2:prog),
  data = df,
  REML = TRUE
)

m1 <- lmer(
  delta_lishare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    (1 | deso_home) +
    (1 | new_id_2) +
    (1 | new_id_2:prog),
  data = df,
  REML = TRUE
)

m2 <- lmer(
  delta_lishare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    neigh_share_foreign + neigh_share_lowedu + neigh_lowinc_tertile +
    (1 | deso_home) +
    (1 | new_id_2) +
    (1 | new_id_2:prog),
  data = df,
  REML = TRUE
)

m3 <- lmer(
  delta_lishare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    neigh_share_foreign + neigh_share_lowedu + neigh_lowinc_tertile +
    thiel_h_deso_inc + thiel_h_deso_fb +
    share_private_year + share_vocational_year +
    ratio_cohort_to_options_year + cohort_size_year +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE,
  control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

m4 <- lmer(
  delta_lishare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    neigh_share_foreign + neigh_share_lowedu + neigh_lowinc_tertile +
    thiel_h_deso_inc + thiel_h_deso_fb +
    share_private_year + share_vocational_year +
    ratio_cohort_to_options_year + cohort_size_year +
    share_private_year * g_lowinc +
    share_private_year * neigh_lowinc_tertile +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE,
  control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

print(summary(m0))
print(summary(m1))
print(summary(m2))
print(summary(m3))
print(summary(m4))

## ------------------------------------------------------------
## 7) ICC (total) + export ####
## ------------------------------------------------------------

icc_tbl <- tibble(
  model = c("M0","M1","M2","M3","M4"),
  icc   = c(
    performance::icc(m0)$ICC_adjusted,
    performance::icc(m1)$ICC_adjusted,
    performance::icc(m2)$ICC_adjusted,
    performance::icc(m3)$ICC_adjusted,
    performance::icc(m4)$ICC_adjusted
  )
)

write.csv(icc_tbl, file.path(out_dir_tbl, "table_lishare_ICC.csv"), row.names = FALSE)

## ------------------------------------------------------------
## 7b) ICC by level (variance shares) + export ####
## ------------------------------------------------------------

ICC_STUB <- "lishare"

icc_by_level <- function(model) {
  vc <- lme4::VarCorr(model)
  
  re_vars <- sapply(vc, function(x) {
    as.numeric(diag(as.matrix(x)))[1]
  })
  
  resid_var <- attr(vc, "sc")^2
  total_var <- sum(re_vars) + resid_var
  
  c(re_vars / total_var, residual = resid_var / total_var)
}

models_list <- list(M0 = m0, M1 = m1, M2 = m2, M3 = m3, M4 = m4)

icc_total <- tibble(
  model = names(models_list),
  icc_total = sapply(models_list, function(m) performance::icc(m)$ICC_adjusted)
)

write.csv(
  icc_total,
  file.path(out_dir_tbl, paste0("table_", ICC_STUB, "_ICC_total.csv")),
  row.names = FALSE
)

icc_level_wide <- bind_rows(lapply(names(models_list), function(nm) {
  shares <- icc_by_level(models_list[[nm]])
  tibble(model = nm) %>% bind_cols(as_tibble_row(shares))
}))

expected <- c("deso_home", "new_id_2", "new_id_2:prog", "residual")
missing_cols <- setdiff(expected, names(icc_level_wide))
if (length(missing_cols) > 0) {
  warning(
    "Missing columns in ICC-by-level table: ",
    paste(missing_cols, collapse = ", "),
    "\nCheck your random-effects structure / naming in VarCorr()."
  )
}

write.csv(
  icc_level_wide,
  file.path(out_dir_tbl, paste0("table_", ICC_STUB, "_ICC_by_level.csv")),
  row.names = FALSE
)

icc_level_long <- icc_level_wide %>%
  tidyr::pivot_longer(
    cols = -model,
    names_to = "level",
    values_to = "share"
  )

write.csv(
  icc_level_long,
  file.path(out_dir_tbl, paste0("table_", ICC_STUB, "_ICC_by_level_long.csv")),
  row.names = FALSE
)

print(icc_total)
print(icc_level_wide)

## ------------------------------------------------------------
## 8) Model table (stepwise; APA-style) ####
## ------------------------------------------------------------

models_step <- list(
  "M0: Null model"       = m0,
  "M1: Individual"       = m1,
  "M2: + Neighbourhood"  = m2,
  "M3: + System & Metro" = m3,
  "M4: + Interactions"   = m4
)

ft <- modelsummary(
  models_step,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  gof_map = c(
    "nobs"   = "Observations",
    "AIC"    = "AIC",
    "BIC"    = "BIC",
    "logLik" = "Log Likelihood"
  ),
  coef_omit = "Intercept",
  fmt = 3,
  output = "flextable",
  title = "Table X. Multilevel models of school–neighbourhood mismatch (Δ share low income)"
)

ft <- autofit(ft)
ft <- fontsize(ft, size = 10, part = "all")
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- bold(ft, part = "header")

ft <- align(ft, j = 1, align = "left", part = "all")
ft <- align(
  ft,
  j = 2:ncol(ft$body$dataset),
  align = "center",
  part = "all"
)

save_as_docx(
  ft,
  path = file.path(out_dir_tbl, "table_lishare_stepwise_APA.docx")
)

tab_step <- modelsummary(
  models_step,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  gof_map = c("nobs" = "Observations","AIC"="AIC","BIC"="BIC","logLik"="Log Likelihood"),
  output = "data.frame"
) %>%
  filter(term != "(Intercept)")

write.csv(tab_step, file.path(out_dir_tbl, "table_lishare_stepwise_keep_yearfactor.csv"), row.names = FALSE)

## ------------------------------------------------------------
## 8c) Save checkpoints (data + fitted models) ####
## ------------------------------------------------------------

out_dir_save <- "save_filer/low_inc/"
dir.create(out_dir_save, showWarnings = FALSE, recursive = TRUE)

saveRDS(df, file.path(out_dir_save, "df_prepped.rds"), compress = "xz")

saveRDS(m0, file.path(out_dir_save, "m0_null.rds"), compress = "gzip")
saveRDS(m1, file.path(out_dir_save, "m1_individual.rds"), compress = "gzip")
saveRDS(m2, file.path(out_dir_save, "m2_neighbourhood.rds"), compress = "gzip")
saveRDS(m3, file.path(out_dir_save, "m3_system.rds"), compress = "gzip")
saveRDS(m4, file.path(out_dir_save, "m4_interaction.rds"), compress = "gzip")

saveRDS(icc_tbl, file.path(out_dir_save, "icc_lishare.rds"))
saveRDS(tab_step, file.path(out_dir_save, "table_lishare.rds"))

# Reload (template)
df  <- readRDS("save_filer/low_inc/df_prepped.rds")
m0  <- readRDS("save_filer/low_inc/m0_null.rds")
m1  <- readRDS("save_filer/low_inc/m1_individual.rds")
m2  <- readRDS("save_filer/low_inc/m2_neighbourhood.rds")
m3  <- readRDS("save_filer/low_inc/m3_system.rds")
m4  <- readRDS("save_filer/low_inc/m4_interaction.rds")

## ------------------------------------------------------------
## 9) Spline model (M4-style with spline interaction) ####
## ------------------------------------------------------------

m4_spline <- lmer(
  delta_lishare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    neigh_share_foreign + neigh_share_lowedu + neigh_lowinc_tertile +
    thiel_h_deso_inc + thiel_h_deso_fb +
    share_vocational_year + ratio_cohort_to_options_year + cohort_size_year +
    splines::ns(share_private_year, df = 3) +
    splines::ns(share_private_year, df = 3):g_lowinc +
    splines::ns(share_private_year, df = 3):neigh_lowinc_tertile +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE,
  control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

print(summary(m4_spline))

tab_spline <- modelsummary(
  list("M: spline interaction" = m4_spline),
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  gof_map = c("nobs" = "Observations","AIC"="AIC","BIC"="BIC","logLik"="Log Likelihood"),
  output = "data.frame"
) %>%
  filter(term != "(Intercept)")

write.csv(tab_spline, file.path(out_dir_tbl, "table_lishare_spline_keep_yearfactor.csv"), row.names = FALSE)

## ------------------------------------------------------------
## 10) Predictions for visualisation (spline) ####
## ------------------------------------------------------------

priv_seq <- seq(
  min(df$share_private_year, na.rm = TRUE),
  max(df$share_private_year, na.rm = TRUE),
  length.out = 100
)

nd <- tidyr::expand_grid(
  share_private_year     = priv_seq,
  g_lowinc               = factor(c("FALSE","TRUE"), levels = levels(df$g_lowinc)),
  neigh_lowinc_tertile   = factor(c("Low","Medium","High"), levels = levels(df$neigh_lowinc_tertile))
) %>%
  mutate(
    g_highinc  = factor("FALSE", levels = levels(df$g_highinc)),
    g_highedu  = factor("FALSE", levels = levels(df$g_highedu)),
    g_lowedu   = factor("FALSE", levels = levels(df$g_lowedu)),
    sex        = factor(levels(df$sex)[min(2, length(levels(df$sex)))], levels = levels(df$sex)),
    foreign_back = factor(levels(df$foreign_back)[1], levels = levels(df$foreign_back)),
    prog_yp_hp   = factor("YP", levels = levels(df$prog_yp_hp)),
    hman_2gr     = factor(levels(df$hman_2gr)[1], levels = levels(df$hman_2gr)),
    
    gpa_y9 = mean(df$gpa_y9, na.rm = TRUE),
    share_vocational_year        = mean(df$share_vocational_year, na.rm = TRUE),
    ratio_cohort_to_options_year = mean(df$ratio_cohort_to_options_year, na.rm = TRUE),
    cohort_size_year             = mean(df$cohort_size_year, na.rm = TRUE),
    neigh_share_foreign          = mean(df$neigh_share_foreign, na.rm = TRUE),
    neigh_share_lowedu           = mean(df$neigh_share_lowedu, na.rm = TRUE),
    neigh_share_lowinc           = mean(df$neigh_share_lowinc, na.rm = TRUE),
    thiel_h_deso_inc             = mean(df$thiel_h_deso_inc, na.rm = TRUE),
    thiel_h_deso_fb              = mean(df$thiel_h_deso_fb, na.rm = TRUE)
  ) %>%
  mutate(
    pred = predict(m4_spline, newdata = ., re.form = NA),
    g_lowinc = forcats::fct_recode(
      g_lowinc,
      "Not low income" = "FALSE",
      "Low income"     = "TRUE"
    )
  )

p1 <- ggplot(nd, aes(x = share_private_year, y = pred, colour = g_lowinc)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ neigh_lowinc_tertile) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Privatisation and differences in program–neighbourhood composition (low income share)",
    subtitle = "Spline predictions by low income background and neighbourhood low income context",
    x = "Share of students in non-public schools",
    y = "Predicted difference between\nprogram and neighbourhood share",
    colour = "Student background"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(p1)

## ------------------------------------------------------------
## 10A) Main figure: low income only (context held at Medium) ####
## ------------------------------------------------------------

ref_neigh <- "Medium"
if (!ref_neigh %in% levels(df$neigh_lowinc_tertile)) {
  ref_neigh <- levels(df$neigh_lowinc_tertile)[1]
}

nd_main <- tidyr::expand_grid(
  share_private_year = priv_seq,
  g_lowinc           = factor(c("FALSE","TRUE"), levels = levels(df$g_lowinc))
) %>%
  mutate(
    neigh_lowinc_tertile = factor(ref_neigh, levels = levels(df$neigh_lowinc_tertile)),
    
    g_highinc  = factor("FALSE", levels = levels(df$g_highinc)),
    g_highedu  = factor("FALSE", levels = levels(df$g_highedu)),
    g_lowedu   = factor("FALSE", levels = levels(df$g_lowedu)),
    sex        = factor(levels(df$sex)[min(2, length(levels(df$sex)))], levels = levels(df$sex)),
    foreign_back = factor(levels(df$foreign_back)[1], levels = levels(df$foreign_back)),
    prog_yp_hp   = factor("YP", levels = levels(df$prog_yp_hp)),
    hman_2gr     = factor(levels(df$hman_2gr)[1], levels = levels(df$hman_2gr)),
    
    gpa_y9 = mean(df$gpa_y9, na.rm = TRUE),
    share_vocational_year        = mean(df$share_vocational_year, na.rm = TRUE),
    ratio_cohort_to_options_year = mean(df$ratio_cohort_to_options_year, na.rm = TRUE),
    cohort_size_year             = mean(df$cohort_size_year, na.rm = TRUE),
    neigh_share_foreign          = mean(df$neigh_share_foreign, na.rm = TRUE),
    neigh_share_lowedu           = mean(df$neigh_share_lowedu, na.rm = TRUE),
    neigh_share_lowinc           = mean(df$neigh_share_lowinc, na.rm = TRUE),
    thiel_h_deso_inc             = mean(df$thiel_h_deso_inc, na.rm = TRUE),
    thiel_h_deso_fb              = mean(df$thiel_h_deso_fb, na.rm = TRUE)
  ) %>%
  mutate(
    pred = predict(m4_spline, newdata = ., re.form = NA),
    g_lowinc = forcats::fct_recode(
      g_lowinc,
      "Not low income" = "FALSE",
      "Low income"     = "TRUE"
    )
  )

p_main <- ggplot(nd_main, aes(x = share_private_year, y = pred, colour = g_lowinc)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Privatisation and differences in program–neighbourhood composition\n(low income share)",
    subtitle = paste0(
      "Spline predictions by low income background\n",
      "(neighbourhood low income-share held at: ", ref_neigh, ")"
    ),
    x = "Share of students in non-public schools",
    y = "Predicted difference between\nprogram and neighbourhood share",
    colour = "Student background"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(p_main)

## ------------------------------------------------------------
## 10B) Low vs High neighbourhood context (2 panels) ####
## ------------------------------------------------------------

priv_seq2 <- seq(
  min(df$share_private_year, na.rm = TRUE),
  max(df$share_private_year, na.rm = TRUE),
  length.out = 80
)

make_nd_context <- function(neigh_level, context_label) {
  tidyr::expand_grid(
    share_private_year = priv_seq2,
    g_lowinc           = factor(c("FALSE","TRUE"), levels = levels(df$g_lowinc))
  ) %>%
    mutate(
      neigh_lowinc_tertile = factor(neigh_level, levels = levels(df$neigh_lowinc_tertile)),
      context = context_label,
      
      g_highinc  = factor("FALSE", levels = levels(df$g_highinc)),
      g_highedu  = factor("FALSE", levels = levels(df$g_highedu)),
      g_lowedu   = factor("FALSE", levels = levels(df$g_lowedu)),
      sex        = factor(levels(df$sex)[min(2, length(levels(df$sex)))], levels = levels(df$sex)),
      foreign_back = factor(levels(df$foreign_back)[1], levels = levels(df$foreign_back)),
      prog_yp_hp   = factor("YP", levels = levels(df$prog_yp_hp)),
      hman_2gr     = factor(levels(df$hman_2gr)[1], levels = levels(df$hman_2gr)),
      
      gpa_y9 = mean(df$gpa_y9, na.rm = TRUE),
      share_vocational_year        = mean(df$share_vocational_year, na.rm = TRUE),
      ratio_cohort_to_options_year = mean(df$ratio_cohort_to_options_year, na.rm = TRUE),
      cohort_size_year             = mean(df$cohort_size_year, na.rm = TRUE),
      neigh_share_foreign          = mean(df$neigh_share_foreign, na.rm = TRUE),
      neigh_share_lowedu           = mean(df$neigh_share_lowedu, na.rm = TRUE),
      neigh_share_lowinc           = mean(df$neigh_share_lowinc, na.rm = TRUE),
      thiel_h_deso_inc             = mean(df$thiel_h_deso_inc, na.rm = TRUE),
      thiel_h_deso_fb              = mean(df$thiel_h_deso_fb, na.rm = TRUE)
    ) %>%
    mutate(pred = predict(m4_spline, newdata = ., re.form = NA))
}

need_levels <- c("Low", "High")
if (!all(need_levels %in% levels(df$neigh_lowinc_tertile))) {
  stop(
    "neigh_lowinc_tertile does not contain: ",
    paste(setdiff(need_levels, levels(df$neigh_lowinc_tertile)), collapse = ", ")
  )
}

pred_low  <- make_nd_context("Low",  "Low share low income in neighbourhood")
pred_high <- make_nd_context("High", "High share low income in neighbourhood")

pred_2panels <- bind_rows(pred_low, pred_high) %>%
  mutate(
    g_lowinc = forcats::fct_recode(
      g_lowinc,
      "Not low income" = "FALSE",
      "Low income"     = "TRUE"
    )
  )

p_2panels <- ggplot(pred_2panels, aes(x = share_private_year, y = pred, colour = g_lowinc)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ context, ncol = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Privatisation and differences in program–neighbourhood composition\n(low income share)",
    subtitle = "Spline predictions by student low income in low and\nhigh low-income neighbourhood contexts",
    x = "Share of students in non-public schools",
    y = "Predicted difference between\nprogram and neighbourhood share",
    colour = "Student background"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(p_2panels)

## ------------------------------------------------------------
## 11) Save plots (PDF + PNG helper) ####
## ------------------------------------------------------------

save_plot_pdf_png <- function(plot, filename_stub, out_dir, width, height, dpi = 400) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  ggsave(
    filename = file.path(out_dir, paste0(filename_stub, ".pdf")),
    plot = plot, width = width, height = height, device = grDevices::cairo_pdf
  )
  
  ggsave(
    filename = file.path(out_dir, paste0(filename_stub, ".png")),
    plot = plot, width = width, height = height, dpi = dpi
  )
}

save_plot_pdf_png(
  plot = p1,
  filename_stub = "figure_lishare_spline_lowinc_context_3panels",
  out_dir = out_dir_plt,
  width = 8.5, height = 4.8
)

save_plot_pdf_png(
  plot = p_main,
  filename_stub = "figure_lishare_spline_main_student_lowinc",
  out_dir = out_dir_plt,
  width = 8, height = 4.5
)

save_plot_pdf_png(
  plot = p_2panels,
  filename_stub = "figure_lishare_spline_low_vs_high_neigh_context",
  out_dir = out_dir_plt,
  width = 8, height = 4.5
)

gc()
