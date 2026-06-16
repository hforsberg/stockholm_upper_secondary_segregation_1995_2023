#### DESCRIPTIVE FIGURES — STOCKHOLM 1995–2023 (FIGURE 2a–2d) ###
#-----------------------------------------------------------------------
# Purpose:
#   Produce four descriptive figures for Stockholm County (Figure 2a–2d):
#   (2a) Number of public and private upper secondary schools over time
#   (2b) Share of students in public vs private schools over time
#   (2c) Number of unique school–programme offerings (stvkod) by provider + total
#   (2d) Share of students in academic vs vocational vs introductory programmes
#
# Data input:
#   - sthlm_95_23_final_new.csv
#     One row per student-year with harmonised school IDs, programme classification,
#     provider info, and register track variable (utbildningstyp).
#
# Output:
#   - Optional: individual files for 2a–2d + combined 2×2 panel
#-----------------------------------------------------------------------

{
  library(tidyverse)
  library(data.table)
  library(ggplot2)
  library(RColorBrewer)
  library(stringr)
  library(scales)
  library(cowplot)
}

## ------------------------------------------------------------
## 1) USER SETTINGS (edit here) ####
## ------------------------------------------------------------

# Data
data_path <- "data/final/sthlm_95_23_final_new.csv"

# Time window
year_min <- 1995
year_max <- 2023
x_breaks <- seq(year_min, year_max, by = 2)

# Palette (match earlier scripts)
palette_name <- "Set1"

# Line/point sizes
line_w  <- 1.0
point_s <- 1.8

# Save outputs?
save_plots <- TRUE
out_dir <- "plots_final/"

## ------------------------------------------------------------
## 2) Load + slim analysis frame ####
## ------------------------------------------------------------

gym <- fread(data_path, encoding = "UTF-8") %>%
  as_tibble() %>%
  mutate(across(where(~ inherits(.x, "integer64")), as.numeric)) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(year), year >= year_min, year <= year_max) %>%
  # Keep only variables used below (reduces weird class surprises)
  transmute(
    year,
    new_id_2,
    prog,
    utbildningstyp,
    stvkod,
    provider,
    prog_yp_hp,
    programme_type,
    gym_id_prog_h,
    gym_id_stvkod
  ) %>%
  # Drop blank programme labels if these represent missing values in your data
  filter(!is.na(prog), prog != "")


## ------------------------------------------------------------
## 3) Colours + shared theme ####
## ------------------------------------------------------------

pal4 <- RColorBrewer::brewer.pal(4, palette_name)
pal3 <- RColorBrewer::brewer.pal(3, palette_name)
pal2 <- RColorBrewer::brewer.pal(3, palette_name)[1:2]

# Public vs Private
col_provider <- c(Public = pal2[1], Private = pal2[2])

# Offer series (Public/Private/Total)
col_offer <- c(Public = pal3[1], Private = pal3[2], Total = pal3[3])

# Programme type (Academic/Vocational/Introductory)
col_track <- c(
  Academic     = pal4[1],
  Vocational   = pal4[2],
  Introductory = pal4[4]
)

theme_panel <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      plot.title.position = "plot",
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text  = element_text(size = 11)
    )
}

## ------------------------------------------------------------
## 4) FIGURE 2a: Number of schools by provider ####
## ------------------------------------------------------------

schools_count <- gym %>%
  filter(!is.na(provider)) %>%
  distinct(year, new_id_2, provider) %>%
  count(year, provider, name = "n")

f2a <- ggplot(schools_count, aes(x = year, y = n, colour = provider, group = provider)) +
  geom_line(linewidth = line_w) +
  geom_point(size = point_s) +
  scale_x_continuous(breaks = x_breaks) +
  scale_colour_manual(values = col_provider, drop = FALSE) +
  labs(
    title = "(2a) Number of public and private upper secondary schools",
    x = "Year",
    y = "Number of schools",
    colour = "Provider"
  ) +
  theme_panel() +
  theme(plot.title = element_text(face = "plain", size = 13))

## ------------------------------------------------------------
## 5) FIGURE 2b: Student share by provider ####
## ------------------------------------------------------------

students_share_provider <- gym %>%
  filter(!is.na(provider)) %>%
  count(year, provider, name = "n") %>%
  group_by(year) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

f2b <- ggplot(students_share_provider, aes(x = year, y = pct, colour = provider, group = provider)) +
  geom_line(linewidth = line_w) +
  geom_point(size = point_s) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_colour_manual(values = col_provider, drop = FALSE) +
  labs(
    title = "(2b) Share of students in public and private upper secondary schools",
    x = "Year",
    y = "Percent of students",
    colour = "Provider"
  ) +
  theme_panel() +
  theme(plot.title = element_text(face = "plain", size = 13))

## ---------------------------------------------------------------------------------
## 6) FIGURE 2c: Unique school–programme offerings (stvkod) by provider + total ####
## ---------------------------------------------------------------------------------

programme_offer <- gym %>%
  filter(!is.na(provider), !is.na(gym_id_stvkod)) %>%
  distinct(year, gym_id_stvkod, provider) %>%
  count(year, provider, name = "n") %>%
  pivot_wider(names_from = provider, values_from = n, values_fill = 0) %>%
  mutate(Total = Public + Private) %>%
  pivot_longer(cols = c("Public", "Private", "Total"),
               names_to = "series",
               values_to = "value") %>%
  mutate(series = factor(series, levels = c("Public", "Private", "Total")))

f2c <- ggplot(programme_offer, aes(x = year, y = value, colour = series, group = series)) +
  geom_line(linewidth = line_w) +
  geom_point(size = point_s) +
  scale_x_continuous(breaks = x_breaks) +
  scale_colour_manual(values = col_offer, drop = FALSE) +
  labs(
    title = "(2c) Unique school–program offerings by provider",
    x = "Year",
    y = "Number of unique offerings",
    colour = "Series"
  ) +
  theme_panel() +
  theme(plot.title = element_text(face = "plain", size = 13))

## ---------------------------------------------------------------------------
## 7) FIGURE 2d: Programme type shares (Academic/Vocational/Introductory) ####
## ---------------------------------------------------------------------------

track_share <- gym %>%
  filter(!is.na(programme_type)) %>%
  count(year, programme_type, name = "n") %>%
  group_by(year) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(programme_type = factor(programme_type, levels = c("Academic", "Vocational", "Introductory")))

f2d <- ggplot(track_share, aes(x = year, y = pct, colour = programme_type, group = programme_type)) +
  geom_line(linewidth = line_w) +
  geom_point(size = point_s) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_colour_manual(values = col_track, drop = FALSE) +
  labs(
    title = "(2d) Share of students in academic and vocational programs",
    x = "Year",
    y = "Percent of students",
    colour = "Program type"
  ) +
  theme_panel() +
  theme(plot.title = element_text(face = "plain", size = 13))

## ------------------------------------------------------------
## 8) Print figures ####
## ------------------------------------------------------------

print(f2a)
print(f2b)
print(f2c)
print(f2d)

## ------------------------------------------------------------
## 9) Combine into a 2×2 panel (Figure 2a–2d) ####
## ------------------------------------------------------------

panel_2 <- cowplot::plot_grid(f2a, f2b, f2c, f2d, ncol = 2, align = "hv")
print(panel_2)

## ------------------------------------------------------------
## 10) Save outputs (optional) ####
## ------------------------------------------------------------

if (save_plots) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  ggsave(file.path(out_dir, "figure_2a.png"), f2a, width = 10, height = 8, dpi = 400)
  ggsave(file.path(out_dir, "figure_2b.png"), f2b, width = 10, height = 8, dpi = 400)
  ggsave(file.path(out_dir, "figure_2c.png"), f2c, width = 10, height = 8, dpi = 400)
  ggsave(file.path(out_dir, "figure_2d.png"), f2d, width = 10, height = 8, dpi = 400)
  
  ggsave(file.path(out_dir, "f1_market_abcd_panel.png"), panel_2, width = 14, height = 8, dpi = 500)
  ggsave(file.path(out_dir, "f1_market_abcd_panel.pdf"), panel_2, width = 14, height = 8, device = grDevices::cairo_pdf)
}
