################################################################################
# Table 1 — Key variables (1995, 2005, 2015, 2023)
################################################################################
# Purpose:
#   Descriptive table for selected years from the full base dataset.
# Output:
#   - CSV (wide format)
#   - DOCX (flextable)
################################################################################

## ------------------------------------------------------------
## 0) SETTINGS ####
## ------------------------------------------------------------

{
  library(data.table)
  library(tidyverse)
  library(flextable)
  library(officer)
  library(readr)
}

data_path <- "data/final/sthlm_95_23_final_new.csv"
years_keep <- c(1995, 2005, 2015, 2023)

out_dir_tbl <- "out_tables/descriptives/"
dir.create(out_dir_tbl, showWarnings = FALSE, recursive = TRUE)

out_csv  <- file.path(out_dir_tbl, "Table1_Key_variables_1995_2005_2015_2023.csv")
out_docx <- file.path(out_dir_tbl, "Table1_Key_variables_1995_2005_2015_2023.docx")

## ------------------------------------------------------------
## 1) LOAD + FILTER  ####
## ------------------------------------------------------------

sthlm_gym_95_23 <- fread(data_path, encoding = "UTF-8")
setnames(sthlm_gym_95_23, tolower(names(sthlm_gym_95_23)))

d <- sthlm_gym_95_23 %>%
  as_tibble() %>%
  filter(year %in% years_keep)

## ------------------------------------------------------------
## 2) FORMAT HELPERS  ####
## ------------------------------------------------------------

# Proportion (0–1) -> percentage string
fmt_pct <- function(x, d = 0) paste0(round(100 * x, d), "%")

# Integer with thousands separator (space)
fmt_int <- function(x) format(as.integer(x), big.mark = " ", scientific = FALSE)

# Numeric with fixed decimals, preserving trailing zeros
fmt_dec <- function(x, d = 3) {
  format(round(x, d), nsmall = d, big.mark = " ", scientific = FALSE)
}

# Mean (SD) for numeric variables
fmt_ms_num <- function(x, d = 2) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  paste0(
    format(round(m, d), nsmall = d, big.mark = " ", scientific = FALSE),
    " (",
    format(round(s, d), nsmall = d, big.mark = " ", scientific = FALSE),
    ")"
  )
}

## ------------------------------------------------------------
## 3) BUILD TABLE (LONG -> WIDE)  ####
## ------------------------------------------------------------

tab_long <- bind_rows(
  # N
  d %>% group_by(year) %>%
    summarise(value = fmt_int(n()), .groups = "drop") %>%
    mutate(label = "Total N"),
  
  # Shares (individual)
  d %>% group_by(year) %>%
    summarise(value = fmt_pct(mean(sex == "2", na.rm = TRUE)), .groups = "drop") %>%
    mutate(label = "Share female"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_pct(mean(foreign_back == 1, na.rm = TRUE)), .groups = "drop") %>%
    mutate(label = "Foreign background"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_pct(mean(g_lowedu, na.rm = TRUE)), .groups = "drop") %>%
    mutate(label = "Low parental education*"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_pct(mean(g_highedu, na.rm = TRUE)), .groups = "drop") %>%
    mutate(label = "High parental education**"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_pct(mean(g_lowinc, na.rm = TRUE)), .groups = "drop") %>%
    mutate(label = "Low household income"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_pct(mean(g_highinc, na.rm = TRUE)), .groups = "drop") %>%
    mutate(label = "High household income"),
  
  # Continuous (mean, SD)
  d %>% group_by(year) %>%
    summarise(value = fmt_ms_num(gpa_y9, d = 2), .groups = "drop") %>%
    mutate(label = "Grade 9 GPA (mean, SD)***"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_ms_num(hh_inc_3y, d = 2), .groups = "drop") %>%
    mutate(label = "Household income (equivalised)****"),
  
  # System / structure
  d %>% group_by(year) %>%
    summarise(value = fmt_pct(mean(share_private_year, na.rm = TRUE)), .groups = "drop") %>%
    mutate(label = "Share in non-public schools"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_pct(mean(share_vocational_year, na.rm = TRUE)), .groups = "drop") %>%
    mutate(label = "Share in vocational programmes"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_int(n_distinct(new_id_2)), .groups = "drop") %>%
    mutate(label = "Number of upper secondary schools"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_int(n_distinct(gym_id_stvkod)), .groups = "drop") %>%
    mutate(label = "Number of school–programmes"),
  
  # Segregation (Theil's H)
  d %>% group_by(year) %>%
    summarise(value = fmt_dec(mean(thiel_h_deso_inc, na.rm = TRUE), d = 3), .groups = "drop") %>%
    mutate(label = "Theil's H (income, neighbourhoods)"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_dec(mean(thiel_h_prog_inc, na.rm = TRUE), d = 3), .groups = "drop") %>%
    mutate(label = "Theil's H (income, school–programmes)"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_dec(mean(thiel_h_deso_fb, na.rm = TRUE), d = 3), .groups = "drop") %>%
    mutate(label = "Theil's H (foreign background, neighbourhoods)"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_dec(mean(thiel_h_prog_fb, na.rm = TRUE), d = 3), .groups = "drop") %>%
    mutate(label = "Theil's H (foreign background, school–programmes)"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_dec(mean(thiel_h_deso_edu, na.rm = TRUE), d = 3), .groups = "drop") %>%
    mutate(label = "Theil's H (education, neighbourhoods)"),
  
  d %>% group_by(year) %>%
    summarise(value = fmt_dec(mean(thiel_h_prog_edu, na.rm = TRUE), d = 3), .groups = "drop") %>%
    mutate(label = "Theil's H (education, school–programmes)")
)

tab_wide <- tab_long %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(across(-label, as.character))

# Save CSV
readr::write_csv(tab_wide, out_csv)

## ------------------------------------------------------------
## 4) FLExtable OUTPUT (DOCX)  ####
## ------------------------------------------------------------

ft <- flextable(tab_wide) %>%
  set_header_labels(
    label = "Variable",
    `1995` = "1995",
    `2005` = "2005",
    `2015` = "2015",
    `2023` = "2023"
  ) %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all")

ft <- add_footer_lines(
  ft,
  values = c(
    "* Two years or less of upper secondary education. ** Four years or more of higher education.",
    "*** Grade 9 GPA is standardised within year using GPA deciles.",
    "**** Income is a three-year rolling average of equivalised disposable income (hundreds of SEK)."
  )
)

ft <- fontsize(ft, size = 8, part = "footer") %>%
  align(align = "left", part = "footer")

print(ft)

save_as_docx(ft, path = out_docx)

## ------------------------------------------------------------
## 5) OPTIONAL: CHECK INCOME DISPERSION (diagnostic)  ####
## ------------------------------------------------------------
# Household income is typically skewed in register data

d %>%
  filter(year == 2015) %>%
  summarise(
    mean = mean(hh_inc_3y, na.rm = TRUE),
    sd   = sd(hh_inc_3y, na.rm = TRUE),
    p50  = median(hh_inc_3y, na.rm = TRUE),
    p90  = quantile(hh_inc_3y, 0.90, na.rm = TRUE),
    p99  = quantile(hh_inc_3y, 0.99, na.rm = TRUE),
    max  = max(hh_inc_3y, na.rm = TRUE)
  )

d %>%
  filter(year == 2015) %>%
  ggplot(aes(hh_inc_3y)) +
  geom_histogram(bins = 100) +
  scale_x_log10() +
  labs(x = "Equivalised household income (log scale)")
