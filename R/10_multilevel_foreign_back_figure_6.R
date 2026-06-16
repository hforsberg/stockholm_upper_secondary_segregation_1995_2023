################################################################################
# 10_multilevel_foreign_back_figure_6.R
#
# Purpose:
#   Multilevel models for foreign-background mismatch:
#     delta_fbshare = programme share foreign background − neighbourhood share foreign background
#
# Neighbourhood controls (consistent with earlier scripts + Andreas logic):
#   - Baseline controls: neigh_share_lowinc + neigh_share_lowedu
#   - Context variable in this script: neigh_foreign_tertile (20/80 cut on neigh_share_foreign)
#
# Inputs:
#   - data/final/sthlm_95_23_final_new.csv
#
# Outputs:
#   - out_tables/foreign_tables/     (ICC + model tables)
#   - out_plots_multilevel/          (prediction plots)
#   - save_filer/foreign/            (RDS checkpoints)
#
# Notes:
#   - Project-relative paths (assumes working directory is repo root).
#   - Filter early and keep variables tight (large-N friendly).
################################################################################

## ------------------------------------------------------------
## 0) Packages + settings ####
## ------------------------------------------------------------

{
  library(data.table)
  library(tidyverse)
  library(lme4)
  library(lmerTest)
  library(modelsummary)
  library(splines)
  library(scales)
  library(performance)
  library(flextable)
  library(officer)
}

data_path <- "data/final/sthlm_95_23_final_new.csv"

out_dir_tbl <- "out_tables/foreign_tables/"
out_dir_plt <- "out_plots_multilevel/"
out_dir_rds <- "save_filer/foreign/"

dir.create(out_dir_tbl, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_plt, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_rds, showWarnings = FALSE, recursive = TRUE)

min_n_deso    <- 5
keep_prog_var <- "keep_prog_10"

## ------------------------------------------------------------
## 1) Load + keep only required variables (compact frame) ####
## ------------------------------------------------------------

giso <- fread(data_path, encoding = "UTF-8")
setnames(giso, tolower(names(giso)))

vars_keep <- c(
  # IDs / grouping
  "id_s","year","deso_home","new_id_2","prog",
  
  # Individual covariates (kept constant across scripts)
  "sex","foreign_back","g_lowinc","g_highinc","g_lowedu","g_highedu",
  "gpa_y9","prog_yp_hp","hman_2gr",
  
  # Neighbourhood shares (baseline controls + outcome component)
  "neigh_share_foreign","neigh_share_lowinc","neigh_share_lowedu",
  
  # Programme shares (for outcome)
  "prog_share_foreign",
  
  # System / metro-level predictors
  "share_private_year","share_vocational_year",
  "ratio_cohort_to_options_year","cohort_size_year",
  "thiel_h_deso_inc","thiel_h_deso_fb",
  
  # Cell-size controls for filtering
  "n_deso", keep_prog_var
)

vars_keep <- intersect(vars_keep, names(giso))

df <- giso[, ..vars_keep] %>% as_tibble()
rm(giso); gc()

## ------------------------------------------------------------
## 2) Basic cleaning + filter early ####
## ------------------------------------------------------------

df <- df %>%
  mutate(across(where(~ inherits(.x, "integer64")), as.numeric)) %>%
  filter(.data$n_deso >= min_n_deso) %>%
  filter(.data[[keep_prog_var]] == 1 | .data[[keep_prog_var]] == TRUE) %>%
  mutate(
    year = factor(year),
    delta_fbshare = prog_share_foreign - neigh_share_foreign
  )

## ------------------------------------------------------------
## 3) Factor coding (grouping + categorical covariates) ####
## ------------------------------------------------------------

df <- df %>%
  mutate(
    deso_home = factor(deso_home),
    new_id_2  = factor(new_id_2),
    prog      = factor(prog),
    
    sex          = factor(sex),
    foreign_back = factor(foreign_back),
    prog_yp_hp   = factor(prog_yp_hp),
    hman_2gr     = factor(hman_2gr),
    
    g_lowinc  = factor(g_lowinc),
    g_highinc = factor(g_highinc),
    g_lowedu  = factor(g_lowedu),
    g_highedu = factor(g_highedu)
  )

## ------------------------------------------------------------
## 4) Neighbourhood context (20/80 cutpoints -> 3 groups) ####
## ------------------------------------------------------------

q <- quantile(df$neigh_share_foreign, probs = c(0, .20, .80, 1), na.rm = TRUE)

df <- df %>%
  mutate(
    neigh_foreign_tertile = cut(
      neigh_share_foreign,
      breaks = q,
      include.lowest = TRUE,
      labels = c("Low","Medium","High")
    ),
    neigh_foreign_tertile = factor(neigh_foreign_tertile, levels = c("Low","Medium","High"))
  )

## -----------------------------------------------------------------------
## 5) Standardise continuous predictors (optional but consistent) ####
## ----------------------------------------------------------------------- 

vars_scale <- c(
  "gpa_y9",
  "thiel_h_deso_inc","thiel_h_deso_fb",
  "share_vocational_year",
  "ratio_cohort_to_options_year","cohort_size_year"
)

vars_scale <- intersect(vars_scale, names(df))

df <- df %>%
  mutate(across(all_of(vars_scale), ~ as.numeric(scale(.))))

## ----------------------------------------------------------------
## 6) Multilevel models (stepwise)  [Strategy B: no year FE] ####
## ---------------------------------------------------------------

ctrl_big <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

m0 <- lmer(
  delta_fbshare ~ 1 +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE, control = ctrl_big
)

m1 <- lmer(
  delta_fbshare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE, control = ctrl_big
)

# Neighbourhood controls (baseline): lowinc + lowedu + (context tertile for foreign)
m2 <- lmer(
  delta_fbshare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    neigh_share_lowinc + neigh_share_lowedu + neigh_foreign_tertile +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE, control = ctrl_big
)

m3 <- lmer(
  delta_fbshare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    neigh_share_lowinc + neigh_share_lowedu + neigh_foreign_tertile +
    thiel_h_deso_inc + thiel_h_deso_fb +
    share_private_year + share_vocational_year +
    ratio_cohort_to_options_year + cohort_size_year +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE, control = ctrl_big
)

m4 <- lmer(
  delta_fbshare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    neigh_share_lowinc + neigh_share_lowedu + neigh_foreign_tertile +
    thiel_h_deso_inc + thiel_h_deso_fb +
    share_private_year + share_vocational_year +
    ratio_cohort_to_options_year + cohort_size_year +
    share_private_year * foreign_back +
    share_private_year * neigh_foreign_tertile +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE, control = ctrl_big
)

print(summary(m0))
print(summary(m1))
print(summary(m2))
print(summary(m3))
print(summary(m4))

## ------------------------------------------------------------
## 7) ICC tables (total + by level) ####
## ------------------------------------------------------------

ICC_STUB <- "fbshare"

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

# 1) Total ICC (performance::icc)
icc_total <- tibble(
  model = names(models_list),
  icc_total = sapply(models_list, function(m) performance::icc(m)$ICC_adjusted)
)

write.csv(
  icc_total,
  file.path(out_dir_tbl, paste0("table_", ICC_STUB, "_ICC_total.csv")),
  row.names = FALSE
)

# 2) ICC shares by level (variance allocation)
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

# 3) Long version (handy for plotting)
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
## 8) Model table (APA Word + CSV) ####
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
  gof_map = c("nobs"="Observations","AIC"="AIC","BIC"="BIC","logLik"="Log Likelihood"),
  coef_omit = "Intercept",
  fmt = 3,
  output = "flextable",
  title = "Table X. Multilevel models of school–neighbourhood mismatch (Δ share foreign background)"
)

ft <- autofit(ft)
ft <- fontsize(ft, size = 10, part = "all")
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- bold(ft, part = "header")
ft <- align(ft, j = 1, align = "left", part = "all")

n_body_cols <- ncol(ft$body$dataset)
if (!is.na(n_body_cols) && n_body_cols >= 2) {
  ft <- align(ft, j = 2:n_body_cols, align = "center", part = "all")
}

save_as_docx(ft, path = file.path(out_dir_tbl, "table_fbshare_stepwise_APA.docx"))

tab_step <- modelsummary(
  models_step,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  gof_map = c("nobs"="Observations","AIC"="AIC","BIC"="BIC","logLik"="Log Likelihood"),
  output = "data.frame"
) %>%
  filter(term != "(Intercept)")

write.csv(tab_step, file.path(out_dir_tbl, "table_fbshare_stepwise.csv"), row.names = FALSE)

## ------------------------------------------------------------
## 8C) Save checkpoints ####
## ------------------------------------------------------------

# NOTE: icc_tbl is not created above (only icc_total / icc_level_* are).
# Keep the following line commented unless you create icc_tbl explicitly.
# saveRDS(icc_tbl, file.path(out_dir_rds, "icc_fbshare.rds"))

saveRDS(m4,       file.path(out_dir_rds, "m4_interaction.rds"), compress = "gzip")
saveRDS(tab_step, file.path(out_dir_rds, "table_fbshare.rds"))

## ------------------------------------------------------------
## 9) Spline model (no year FE) ####
## ------------------------------------------------------------

m4_spline <- lmer(
  delta_fbshare ~
    sex + foreign_back + g_lowinc + g_highinc + g_highedu + g_lowedu +
    gpa_y9 + prog_yp_hp + hman_2gr +
    neigh_share_lowinc + neigh_share_lowedu + neigh_foreign_tertile +
    thiel_h_deso_inc + thiel_h_deso_fb +
    share_vocational_year + ratio_cohort_to_options_year + cohort_size_year +
    splines::ns(share_private_year, df = 3) +
    splines::ns(share_private_year, df = 3):foreign_back +
    splines::ns(share_private_year, df = 3):neigh_foreign_tertile +
    (1 | deso_home) + (1 | new_id_2) + (1 | new_id_2:prog),
  data = df, REML = TRUE, control = ctrl_big
)

print(summary(m4_spline))

tab_spline <- modelsummary(
  list("M: spline interaction" = m4_spline),
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  gof_map = c("nobs"="Observations","AIC"="AIC","BIC"="BIC","logLik"="Log Likelihood"),
  output = "data.frame"
) %>%
  filter(term != "(Intercept)")

write.csv(tab_spline, file.path(out_dir_tbl, "table_fbshare_spline.csv"), row.names = FALSE)

## ------------------------------------------------------------
## 10) Predictions for visualisation (spline) ####
## ------------------------------------------------------------

priv_seq <- seq(
  min(df$share_private_year, na.rm = TRUE),
  max(df$share_private_year, na.rm = TRUE),
  length.out = 100
)

# Use two levels of foreign_back for plotting (robust if coding differs)
fb_levels <- levels(df$foreign_back)
if (length(fb_levels) < 2) stop("foreign_back has <2 levels; cannot plot group contrasts.")
fb_two <- fb_levels[1:2]

# ----------------------------
# 10A) 3-panel: neighbourhood context (Low/Medium/High)
# ----------------------------
nd <- tidyr::expand_grid(
  share_private_year     = priv_seq,
  foreign_back           = factor(fb_two, levels = fb_levels),
  neigh_foreign_tertile  = factor(c("Low","Medium","High"), levels = levels(df$neigh_foreign_tertile))
) %>%
  mutate(
    # hold other categorical vars fixed
    g_lowinc  = factor("FALSE", levels = levels(df$g_lowinc)),
    g_highinc = factor("FALSE", levels = levels(df$g_highinc)),
    g_highedu = factor("FALSE", levels = levels(df$g_highedu)),
    g_lowedu  = factor("FALSE", levels = levels(df$g_lowedu)),
    sex       = factor(levels(df$sex)[min(2, length(levels(df$sex)))], levels = levels(df$sex)),
    prog_yp_hp = factor("YP", levels = levels(df$prog_yp_hp)),
    hman_2gr   = factor(levels(df$hman_2gr)[1], levels = levels(df$hman_2gr)),
    
    # continuous at means (some are z-scored)
    gpa_y9 = mean(df$gpa_y9, na.rm = TRUE),
    share_vocational_year        = mean(df$share_vocational_year, na.rm = TRUE),
    ratio_cohort_to_options_year = mean(df$ratio_cohort_to_options_year, na.rm = TRUE),
    cohort_size_year             = mean(df$cohort_size_year, na.rm = TRUE),
    neigh_share_lowinc           = mean(df$neigh_share_lowinc, na.rm = TRUE),
    neigh_share_lowedu           = mean(df$neigh_share_lowedu, na.rm = TRUE),
    thiel_h_deso_inc             = mean(df$thiel_h_deso_inc, na.rm = TRUE),
    thiel_h_deso_fb              = mean(df$thiel_h_deso_fb, na.rm = TRUE)
  ) %>%
  mutate(pred = predict(m4_spline, newdata = ., re.form = NA))

p1 <- ggplot(nd, aes(x = share_private_year, y = pred, colour = foreign_back)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ neigh_foreign_tertile) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Privatisation and differences in program–neighbourhood composition (foreign background share)",
    subtitle = "Spline predictions by foreign background and neighbourhood foreign-share context",
    x = "Share of students in non-public schools",
    y = "Predicted difference between\nprogram and neighbourhood share",
    colour = "Student background"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p1)

# ----------------------------
# 10B) Main figure: context held at Medium
# ----------------------------
ref_neigh <- "Medium"
if (!ref_neigh %in% levels(df$neigh_foreign_tertile)) {
  ref_neigh <- levels(df$neigh_foreign_tertile)[1]
}

nd_main <- tidyr::expand_grid(
  share_private_year = priv_seq,
  foreign_back       = factor(fb_two, levels = fb_levels)
) %>%
  mutate(
    neigh_foreign_tertile = factor(ref_neigh, levels = levels(df$neigh_foreign_tertile)),
    
    g_lowinc  = factor("FALSE", levels = levels(df$g_lowinc)),
    g_highinc = factor("FALSE", levels = levels(df$g_highinc)),
    g_highedu = factor("FALSE", levels = levels(df$g_highedu)),
    g_lowedu  = factor("FALSE", levels = levels(df$g_lowedu)),
    sex       = factor(levels(df$sex)[min(2, length(levels(df$sex)))], levels = levels(df$sex)),
    prog_yp_hp = factor("YP", levels = levels(df$prog_yp_hp)),
    hman_2gr   = factor(levels(df$hman_2gr)[1], levels = levels(df$hman_2gr)),
    
    gpa_y9 = mean(df$gpa_y9, na.rm = TRUE),
    share_vocational_year        = mean(df$share_vocational_year, na.rm = TRUE),
    ratio_cohort_to_options_year = mean(df$ratio_cohort_to_options_year, na.rm = TRUE),
    cohort_size_year             = mean(df$cohort_size_year, na.rm = TRUE),
    neigh_share_lowinc           = mean(df$neigh_share_lowinc, na.rm = TRUE),
    neigh_share_lowedu           = mean(df$neigh_share_lowedu, na.rm = TRUE),
    thiel_h_deso_inc             = mean(df$thiel_h_deso_inc, na.rm = TRUE),
    thiel_h_deso_fb              = mean(df$thiel_h_deso_fb, na.rm = TRUE)
  ) %>%
  mutate(pred = predict(m4_spline, newdata = ., re.form = NA))

p_main <- ggplot(nd_main, aes(x = share_private_year, y = pred, colour = foreign_back)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Privatisation and differences in program–neighbourhood composition\n(foreign background share)",
    subtitle = paste0(
      "Spline predictions by foreign background ",
      "(neighbourhood foreign-share held at: ", ref_neigh, ")"
    ),
    x = "Share of students in non-public schools",
    y = "Predicted difference between\nprogram and neighbourhood share",
    colour = "Student background"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p_main)

# ----------------------------
# 10C) Low vs High neighbourhood context (2 panels)
# ----------------------------
priv_seq2 <- seq(
  min(df$share_private_year, na.rm = TRUE),
  max(df$share_private_year, na.rm = TRUE),
  length.out = 80
)

make_nd_context <- function(neigh_level, context_label) {
  tidyr::expand_grid(
    share_private_year = priv_seq2,
    foreign_back       = factor(fb_two, levels = fb_levels)
  ) %>%
    mutate(
      neigh_foreign_tertile = factor(neigh_level, levels = levels(df$neigh_foreign_tertile)),
      context = context_label,
      
      g_lowinc  = factor("FALSE", levels = levels(df$g_lowinc)),
      g_highinc = factor("FALSE", levels = levels(df$g_highinc)),
      g_highedu = factor("FALSE", levels = levels(df$g_highedu)),
      g_lowedu  = factor("FALSE", levels = levels(df$g_lowedu)),
      sex       = factor(levels(df$sex)[min(2, length(levels(df$sex)))], levels = levels(df$sex)),
      prog_yp_hp = factor("YP", levels = levels(df$prog_yp_hp)),
      hman_2gr   = factor(levels(df$hman_2gr)[1], levels = levels(df$hman_2gr)),
      
      gpa_y9 = mean(df$gpa_y9, na.rm = TRUE),
      share_vocational_year        = mean(df$share_vocational_year, na.rm = TRUE),
      ratio_cohort_to_options_year = mean(df$ratio_cohort_to_options_year, na.rm = TRUE),
      cohort_size_year             = mean(df$cohort_size_year, na.rm = TRUE),
      neigh_share_lowinc           = mean(df$neigh_share_lowinc, na.rm = TRUE),
      neigh_share_lowedu           = mean(df$neigh_share_lowedu, na.rm = TRUE),
      thiel_h_deso_inc             = mean(df$thiel_h_deso_inc, na.rm = TRUE),
      thiel_h_deso_fb              = mean(df$thiel_h_deso_fb, na.rm = TRUE)
    ) %>%
    mutate(pred = predict(m4_spline, newdata = ., re.form = NA))
}

need_levels <- c("Low","High")
if (!all(need_levels %in% levels(df$neigh_foreign_tertile))) {
  stop(
    "neigh_foreign_tertile does not contain: ",
    paste(setdiff(need_levels, levels(df$neigh_foreign_tertile)), collapse = ", ")
  )
}

pred_low  <- make_nd_context("Low",  "Low share foreign background in neighbourhood")
pred_high <- make_nd_context("High", "High share foreign background in neighbourhood")

pred_2panels <- bind_rows(pred_low, pred_high)

p_2panels <- ggplot(pred_2panels, aes(x = share_private_year, y = pred, colour = foreign_back)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ context, ncol = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Privatisation and differences in program–neighbourhood composition (foreign background share)",
    subtitle = "Spline predictions by foreign background in low and high\nforeign-share neighbourhood contexts",
    x = "Share of students in non-public schools",
    y = "Predicted difference between\nprogram and neighbourhood share",
    colour = "Student background"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

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
  filename_stub = "figure_fbshare_spline_foreign_context_3panels",
  out_dir = out_dir_plt,
  width = 8.5, height = 4.8
)

save_plot_pdf_png(
  plot = p_main,
  filename_stub = "figure_fbshare_spline_main_foreign",
  out_dir = out_dir_plt,
  width = 8, height = 4.5
)

save_plot_pdf_png(
  plot = p_2panels,
  filename_stub = "figure_fbshare_spline_low_vs_high_neigh_context",
  out_dir = out_dir_plt,
  width = 8, height = 4.5
)

gc()
