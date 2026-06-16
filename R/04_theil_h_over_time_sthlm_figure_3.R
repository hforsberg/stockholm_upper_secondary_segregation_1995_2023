###############################################################
# 04_theil_h_over_time_sthlm_figure_3.R
#
# Purpose:
#   Descriptive overview of segregation trends (Theil’s H) in Stockholm 1995–2023.
#   Uses precomputed year-level H-series (from segregation::mutual_total) for:
#     - Unique school-programme (prog)
#     - Neighbourhood (DeSO)
#   across three dimensions:
#     - Foreign background
#     - Income
#     - Education
#
# Output:
#   Optional saved figures in plots_final/ (PNG + PDF).
#
# Notes:
#   - Paths are project-relative (assumes working directory is the root).
###############################################################

## ------------------------------------------------------------
## 0. Load packages ####
## ------------------------------------------------------------

{
  library(tidyverse)
  library(data.table)
  library(ggplot2)
  library(RColorBrewer)
  library(stringr)
}

## ------------------------------------------------------------
## 1. Load data ####
## ------------------------------------------------------------

sthlm_95_23 <- fread("data/final/sthlm_95_23_final_new.csv", encoding = "UTF-8") %>%
  as_tibble() %>%
  mutate(across(where(~ inherits(.x, "integer64")), as.numeric)) %>%
  mutate(year = as.integer(year))

## ------------------------------------------------------------
## 2. Build year-level series (overall region trends) ####
## ------------------------------------------------------------
# NOTE:
# - The variables are computed with segregation::mutual_total().
# - They should be constant within year; we keep one row per year.

thiel_overall <- sthlm_95_23 %>%
  transmute(
    year,
    
    prog_fb  = thiel_h_prog_fb,
    prog_inc = thiel_h_prog_inc,
    prog_edu = thiel_h_prog_edu,
    
    deso_fb  = thiel_h_deso_fb,
    deso_inc = thiel_h_deso_inc,
    deso_edu = thiel_h_deso_edu
  ) %>%
  distinct(year, .keep_all = TRUE) %>%
  pivot_longer(
    cols = -year,
    names_to = "series",
    values_to = "thiel_h"
  ) %>%
  mutate(
    level = case_when(
      str_detect(series, "^prog_") ~ "Unique school-programme",
      str_detect(series, "^deso_") ~ "Neighbourhood",
      TRUE ~ NA_character_
    ),
    dim = case_when(
      str_detect(series, "_fb$")  ~ "Foreign background",
      str_detect(series, "_inc$") ~ "Income",
      str_detect(series, "_edu$") ~ "Education",
      TRUE ~ NA_character_
    ),
    level = factor(level, levels = c("Unique school-programme", "Neighbourhood")),
    dim   = factor(dim,   levels = c("Foreign background", "Income", "Education"))
  ) %>%
  filter(!is.na(level), !is.na(dim)) %>%
  arrange(dim, level, year)

# Optional diagnostics
#diag_tbl <- thiel_overall %>%
#  group_by(dim, level) %>%
#  summarise(n_years = n_distinct(year), n_na = sum(is.na(thiel_h)), .groups = "drop")
#print(diag_tbl)

## ------------------------------------------------------------
## 3. Plot styling ####
## ------------------------------------------------------------

year_min <- 1995
year_max <- 2023
x_breaks <- sort(unique(c(year_min, seq(year_min, year_max, by = 2))))

palette_name <- "Set1"
pal3 <- RColorBrewer::brewer.pal(3, palette_name)

# One colour per dimension (3 series total)
col_dim <- c(
  "Foreign background" = pal3[1],
  "Income"             = pal3[2],
  "Education"          = pal3[3]
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
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12)
    )
}

y_lim <- range(thiel_overall$thiel_h, na.rm = TRUE)

## ------------------------------------------------------------
## 4. Main figure: 3 rows × 2 columns ####
## ------------------------------------------------------------

p_thiel_overall <- thiel_overall %>%
  filter(year >= year_min, year <= year_max) %>%
  mutate(
    # row labels with line breaks
    dim_lbl = recode(
      as.character(dim),
      "Foreign background" = "Foreign\nbackground",
      "Income"             = "Income",
      "Education"          = "Education"
    ),
    dim_lbl = factor(dim_lbl, levels = c("Foreign\nbackground", "Income", "Education"))
  ) %>%
  ggplot(aes(x = year, y = thiel_h, colour = dim, group = dim)) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(size = 1.9, na.rm = TRUE) +
  facet_grid(
    dim_lbl ~ level,
    switch = "y"
  ) +
  scale_x_continuous(
    limits = c(year_min, year_max),
    breaks = x_breaks,
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    limits = y_lim,
    position = "right"
  ) +
  scale_colour_manual(values = col_dim, drop = FALSE) +
  labs(
    x = " \nYear \n",
    y = " \nTheil's H \n",
    title = "Theil's H for neighbourhoods and school-programs in Stockholm school market 1995–2023"
  ) +
  theme_panel() +
  theme(
    strip.placement = "outside",
    strip.text.y = element_text(size = 11),
    strip.text.x = element_text(size = 13),
    axis.title.y = element_text(margin = margin(l = 12)),
    axis.text.y  = element_text(margin = margin(r = 0))
  )

print(p_thiel_overall)

## -----------------------------------------------------------------
## 5. Alternative figure: 3 rows, two lines per panel (levels) ####
## -----------------------------------------------------------------

# Colours for the two levels (Set1)
pal2 <- RColorBrewer::brewer.pal(3, "Set1")[1:2]
col_level <- c(
  "Unique school-programme" = pal2[1],
  "Neighbourhood"           = pal2[2]
)

p_thiel_3panels_levels <- thiel_overall %>%
  filter(year >= year_min, year <= year_max) %>%
  mutate(
    dim_lbl = recode(
      as.character(dim),
      "Foreign background" = "Foreign\nbackground",
      "Income"             = "Income",
      "Education"          = "Education"
    ),
    dim_lbl = factor(dim_lbl, levels = c("Foreign\nbackground", "Income", "Education"))
  ) %>%
  ggplot(aes(x = year, y = thiel_h, colour = level, group = level)) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(size = 1.9, na.rm = TRUE) +
  facet_grid(dim_lbl ~ ., switch = "y") +
  scale_x_continuous(
    limits = c(year_min, year_max),
    breaks = x_breaks,
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    limits = y_lim,
    position = "right"
  ) +
  scale_colour_manual(
    values = col_level,
    breaks = c("Neighbourhood", "Unique school-programme"),
    name = NULL,
    drop = FALSE
  ) +
  labs(
    x = " \nYear \n",
    y = " \nTheil's H \n",
    title = "Theil's H for neighbourhoods and school-programs in Stockholm school market 1995–2023 \n"
    #subtitle = "Three dimensions (rows); lines compare Unique school-programme vs neighbourhood"
  ) +
  theme_panel() +
  theme(
    strip.placement = "outside",
    strip.text.y = element_text(size = 12),
    legend.position = "bottom",
    axis.title.y = element_text(margin = margin(l = 12)),
    axis.text.y  = element_text(margin = margin(r = 0))
  )

print(p_thiel_3panels_levels)

## -----------------------------------------------------------------
## 6. Variant: three separate plots (3a–3c) arranged as a panel ####
## -----------------------------------------------------------------

# Ensure legend order: Neighbourhood first
thiel_overall2 <- thiel_overall %>%
  mutate(
    level = factor(level, levels = c("Neighbourhood", "Unique school-programme")),
    dim   = factor(dim, levels = c("Foreign background", "Income", "Education"))
  )

# Colours for the two levels (Set1)
pal2 <- RColorBrewer::brewer.pal(3, "Set1")[1:2]
col_level <- c(
  "Neighbourhood"           = pal2[2],
  "Unique school-programme" = pal2[1]
)

plot_one_dim <- function(dat, dim_keep, tag) {
  dat %>%
    filter(dim == dim_keep) %>%
    ggplot(aes(x = year, y = thiel_h, colour = level, group = level)) +
    geom_line(linewidth = 1.05, na.rm = TRUE) +
    geom_point(size = 1.9, na.rm = TRUE) +
    scale_x_continuous(
      limits = c(year_min, year_max),
      breaks = x_breaks,
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      limits = y_lim,
      position = "right"
    ) +
    scale_colour_manual(values = col_level, drop = FALSE) +
    labs(
      x = " \nYear\n",
      y = " \nTheil's H\n",
      title = paste0("(", tag, ") ", dim_keep)
    ) +
    theme_panel() +
    theme(
      legend.position = "bottom",
      axis.title.y = element_text(margin = margin(t = 12)),
      plot.title = element_text(face = "plain", size = 13)
    ) +
    guides(colour = guide_legend(title = NULL))
}

p_3a <- plot_one_dim(thiel_overall2, "Foreign background", "3a")
p_3b <- plot_one_dim(thiel_overall2, "Income",             "3b")
p_3c <- plot_one_dim(thiel_overall2, "Education",          "3c")

# Panel layout (robust resizing): 4 columns
# - Top: (3a) spans cols 1-2, (3b) spans cols 3-4
# - Bottom: (3c) spans cols 2-3 (centered)
layout_3abc <- c(
  patchwork::area(1, 1, 1, 2),  # p_3a
  patchwork::area(1, 3, 1, 4),  # p_3b
  patchwork::area(2, 2, 2, 3)   # p_3c (centered)
)

p_panel_3abc <- p_3a + p_3b + p_3c +
  patchwork::plot_layout(design = layout_3abc, guides = "collect") &
  theme(legend.position = "bottom")

p_panel_3abc <- p_panel_3abc +
  patchwork::plot_annotation(
    title = "Theil's H for neighbourhoods and school-programs in Stockholm school market 1995–2023 \n",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16)
    )
  )

print(p_panel_3abc)

## ------------------------------------------------------------
## 7. Save outputs (optional) ####
## ------------------------------------------------------------

save_dir <- "plots_final/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

#ggsave(file.path(save_dir, "f_thiel_overall_3x2.png"), p_thiel_overall,
#       dpi = 400, width = 12, height = 8, units = "in")
#ggsave(file.path(save_dir, "f_thiel_overall_3x2.pdf"), p_thiel_overall,
#       width = 12, height = 8, units = "in", device = grDevices::cairo_pdf)

ggsave(file.path(save_dir, "f_thiel_3panels_levels.png"), p_thiel_3panels_levels,
       dpi = 400, width = 12, height = 10, units = "in")
ggsave(file.path(save_dir, "f_thiel_3panels_levels.pdf"), p_thiel_3panels_levels,
       width = 12, height = 10, units = "in", device = grDevices::cairo_pdf)

ggsave(file.path(save_dir, "f_thiel_3abc_panel.png"), p_panel_3abc,
       dpi = 400, width = 14, height = 9, units = "in")
ggsave(file.path(save_dir, "f_thiel_3abc_panel.pdf"), p_panel_3abc,
       width = 14, height = 9, units = "in", device = grDevices::cairo_pdf)
