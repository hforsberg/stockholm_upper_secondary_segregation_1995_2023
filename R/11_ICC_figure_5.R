################################################################################
# 11_ICC_figure_5  (M0–M3 only)
################################################################################
# Purpose:
#   Stacked ICC figure: variance shares by level (sums to 100%)
#   - Reads *_ICC_by_level_long.csv files (produced by the model scripts)
#   - Keeps only M0–M3 (drops M4)
#   - Renormalises shares to sum to 1 within each outcome × model
#
# Inputs:
#   out_tables/*/table_*_ICC_by_level_long.csv
#
# Output:
#   plots_final/f4_ICC_variance_allocation_M0_M3.(png|pdf)
################################################################################

library(tidyverse)
library(RColorBrewer)

## ------------------------------------------------------------
## 0) Helper: read + harmonise one long ICC file (M0–M4 in file)
##    -> drops M4 so plotting data contains M0–M3 only
## ------------------------------------------------------------

prepare_icc_long_m0m3 <- function(path, outcome_name) {
  
  if (!file.exists(path)) stop("File not found: ", path)
  
  readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(
      outcome = outcome_name,
      
      # Harmonise model naming (supports both "M0" and "icc0" styles)
      model = as.character(model),
      model = case_when(
        model %in% c("M0","M1","M2","M3","M4") ~ model,
        model %in% c("icc0","icc1","icc2","icc3","icc4") ~
          recode(model, icc0="M0", icc1="M1", icc2="M2", icc3="M3", icc4="M4"),
        TRUE ~ model
      )
    ) %>%
    # Keep only M0–M3 (drop interactions model)
    filter(model %in% c("M0","M1","M2","M3")) %>%
    mutate(
      model = factor(
        model,
        levels = c("M0","M1","M2","M3"),
        labels = c("Empty (M0)", "Individual (M1)", "Neighbourhood (M2)", "System & Metro (M3)")
      ),
      
      # Harmonise level names across scripts / random-effect labels
      level = as.character(level),
      level = case_when(
        level %in% c("new_id_2:prog", "school_prog", "school_id:prog", "new_id_prog") ~ "School × programme",
        level %in% c("new_id_2", "school_id", "school")                               ~ "School",
        level %in% c("deso_home", "deso", "neighbourhood")                            ~ "Neighbourhood",
        level %in% c("residual", "Residual")                                          ~ "Residual",
        TRUE ~ level
      ),
      level = factor(level, levels = c("Neighbourhood", "School", "School × programme", "Residual")),
      
      share = as.numeric(share)
    ) %>%
    # Drop missing/invalid shares
    filter(!is.na(share), share >= 0) %>%
    
    # Renormalise so each stacked bar sums to 1 within outcome × model
    group_by(outcome, model) %>%
    mutate(share = share / sum(share, na.rm = TRUE)) %>%
    ungroup()
}

## ------------------------------------------------------------
## 1) Read all outcomes
## ------------------------------------------------------------

icc_long <- bind_rows(
  prepare_icc_long_m0m3(file.path("out_tables/low_edu_tables",  "table_leshare_ICC_by_level_long.csv"), "Low education"),
  prepare_icc_long_m0m3(file.path("out_tables/high_edu_tables", "table_heshare_ICC_by_level_long.csv"), "High education"),
  prepare_icc_long_m0m3(file.path("out_tables/low_inc_tables",  "table_lishare_ICC_by_level_long.csv"), "Low income"),
  prepare_icc_long_m0m3(file.path("out_tables/high_inc_tables", "table_hishare_ICC_by_level_long.csv"), "High income"),
  prepare_icc_long_m0m3(file.path("out_tables/foreign_tables",  "table_fbshare_ICC_by_level_long.csv"), "Foreign background")
)

# Sanity check: each stacked bar should sum to 1
check_sums <- icc_long %>%
  group_by(outcome, model) %>%
  summarise(sum_share = sum(share), .groups = "drop")

print(check_sums)

## ------------------------------------------------------------
## 2) Palette (Set1)
## ------------------------------------------------------------

pal <- RColorBrewer::brewer.pal(4, "Set1")

fill_map <- c(
  "Neighbourhood"      = pal[2],
  "School"             = pal[1],
  "School × programme" = pal[3],
  "Residual"           = pal[4]
)

## ------------------------------------------------------------
## 3) Plot: stacked bars by model, facet by outcome (M0–M3)
## ------------------------------------------------------------

p_icc <- ggplot(icc_long, aes(x = model, y = share, fill = level)) +
  geom_col(width = 0.75) +
  facet_wrap(~ outcome, ncol = 1) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = fill_map, drop = FALSE) +
  labs(
    x = "Model specification",
    y = "Share of total variance (sums to 100%)",
    fill = "Variance component"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.9),
    panel.grid.minor = element_blank()
  )

print(p_icc)

## ------------------------------------------------------------
## 4) Save
## ------------------------------------------------------------

out_dir <- "plots_final/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(
  filename = file.path(out_dir, "f4_ICC_variance_allocation_M0_M3.png"),
  plot = p_icc,
  width = 8.5, height = 11,
  dpi = 400
)

ggsave(
  filename = file.path(out_dir, "f4_ICC_variance_allocation_M0_M3.pdf"),
  plot = p_icc,
  width = 8.5, height = 11,
  device = grDevices::cairo_pdf
)

message("Done. Saved ICC figure (M0–M3) to: ", normalizePath(out_dir))
