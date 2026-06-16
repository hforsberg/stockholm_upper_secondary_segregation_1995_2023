## ----------------------------------------------------------------------------------------------------------------
## Calculating thiel's meassures for income,education and foreign background using the the segregation package ####
## ----------------------------------------------------------------------------------------------------------------

#install.packages("segregation")
{
  library(tidyverse)   
  library(data.table)
  library(janitor)
  library(segregation)
}


#------------------------------------------------------------------------------------------------
# Step 15 - Creating Thiel's H and M   #####
#------------------------------------------------------------------------------------------------

# ------------------------------------------------------------
# Goal:
# Create year-level Stockholm segregation series (Theil M and H)
# for:
#   - School composition (unit = new_id_2)
#   - School + program composition (unit = gym_id_prog)
#   - Neighbourhood composition (unit = deso_home)
# Then merge back to the student-year panel (id_s × year).
# ------------------------------------------------------------

#load data
sthlm_95_23 <- fread("data/btw/gym_95_23_step13_sthlm.csv", encoding = "UTF-8") %>%
  as_tibble()

# OPTIONAL: enforce Stockholm only
years <- sort(unique(sthlm_95_23$year))

run_mutual_total_year <- function(df_counts_year) {
  if (nrow(df_counts_year) == 0 || dplyr::n_distinct(df_counts_year$group_id) < 2) {
    return(tibble(M = NA_real_, H = NA_real_))
  }
  out <- tryCatch(
    mutual_total(df_counts_year, unit = "unit_id", group = "group_id", weight = "n"),
    error = function(e) NULL
  )
  if (is.null(out)) return(tibble(M = NA_real_, H = NA_real_))
  tibble(
    M = out$est[out$stat == "M"][1],
    H = out$est[out$stat == "H"][1]
  )
}

thiel_by_year <- function(data, unit_var, group_var, prefix) {
  map_dfr(years, function(y) {
    
    df_y <- data %>%
      filter(year == y) %>%
      transmute(
        unit_id  = as.character(.data[[unit_var]]),
        group_id = as.character(.data[[group_var]])
      ) %>%
      filter(!is.na(unit_id), unit_id != "", !is.na(group_id)) %>%
      count(unit_id, group_id, name = "n") %>%
      mutate(n = as.numeric(n)) %>%
      as.data.frame()
    
    res <- run_mutual_total_year(df_y)
    
    tibble(
      year = y,
      !!paste0("thiel_m_", prefix) := res$M,
      !!paste0("thiel_h_", prefix) := res$H
    )
  })
}

# ---- Income
thiel_school_inc <- thiel_by_year(sthlm_95_23, "new_id_2",    "inc_3groups",      "school_inc")
thiel_prog_inc   <- thiel_by_year(sthlm_95_23, "gym_id_prog", "inc_3groups",      "prog_inc")
thiel_deso_inc   <- thiel_by_year(sthlm_95_23, "deso_home",   "inc_3groups",      "deso_inc")

# ---- Education
thiel_school_edu <- thiel_by_year(sthlm_95_23, "new_id_2",    "edu_3groups",      "school_edu")
thiel_prog_edu   <- thiel_by_year(sthlm_95_23, "gym_id_prog", "edu_3groups",      "prog_edu")
thiel_deso_edu   <- thiel_by_year(sthlm_95_23, "deso_home",   "edu_3groups",      "deso_edu")

# ---- Foreign background
thiel_school_fb  <- thiel_by_year(sthlm_95_23, "new_id_2",    "foreign_back_lab", "school_fb")
thiel_prog_fb    <- thiel_by_year(sthlm_95_23, "gym_id_prog", "foreign_back_lab", "prog_fb")
thiel_deso_fb    <- thiel_by_year(sthlm_95_23, "deso_home",   "foreign_back_lab", "deso_fb")

# Combine year series
thiel_year <- list(
  thiel_school_inc, thiel_prog_inc, thiel_deso_inc,
  thiel_school_edu, thiel_prog_edu, thiel_deso_edu,
  thiel_school_fb,  thiel_prog_fb,  thiel_deso_fb
) %>%
  reduce(left_join, by = "year")

# Merge back to panel
sthlm_95_23 <- sthlm_95_23 %>%
  left_join(thiel_year, by = "year")


# Saving step 14
#fwrite(sthlm_95_23,"data/btw/gym_95_23_step14_sthlm.csv",encoding = "UTF-8")
sthlm_95_23 <- fread("data/btw/gym_95_23_step14_sthlm.csv",encoding = "UTF-8")


#------------------------------------------------------------------------------------------------
# Step 16 - Creating entropi indices  #####
#------------------------------------------------------------------------------------------------


# ============================================================
# Denton & Massey Entropy (Eq.1 and Eq.2) — binary "mix" measures
# Units: school (new_id_2) and neighbourhood (deso_home)
# Group splits:
#   - Foreign vs Swedish
#   - Low income vs rest  (Low vs Mid+High)
#   - High income vs rest (High vs Low+Mid)
#   - Low education vs rest
#   - High education vs rest
#
# Output:
#   - Year-level metro entropy E (Eq.1):    ent_*_metro
#   - Unit-year entropy Ei (Eq.2):          sch_ent_* and deso_ent_*
#
# Note:
#   Theil H (Eq.3) uses weighted average of (E - Ei) / E.
#   You said H is already computed; this script reconstructs the E and Ei parts.
# ============================================================


# ----------------------------
# 0) Optional: keep Stockholm only (matches your earlier workflow)
# ----------------------------

# ----------------------------
# 1) Build binary indicator variables for your "low vs rest" and "high vs rest" logic
#    These are the group proportions Π_r in Eq.1 and Eq.2, but simplified to 2 groups.
# ----------------------------
df <- sthlm_95_23 %>%
  mutate(
    g_foreign = foreign_back_lab == "Foreign background",
    g_lowinc  = inc_3groups      == "Low_income",
    g_highinc = inc_3groups      == "High_income",
    g_lowedu  = edu_3groups      == "Low_education",
    g_highedu = edu_3groups      == "High_education"
  )

# ----------------------------
# 2) Entropy function (Denton & Massey Eq.1 / Eq.2)
#
# Eq.1/Eq.2 general form:
#   E = Σ_r Π_r * ln(1/Π_r)
#
# For a binary split, r ∈ {group, rest}.
# Let p = Π_group, (1-p) = Π_rest
# Then:
#   E(p) = p*ln(1/p) + (1-p)*ln(1/(1-p))
#
# Edge handling:
#   If p is 0 or 1 -> entropy is 0 (no mix).
# ----------------------------
entropy_binary <- function(p) {
  p <- as.numeric(p)
  # Liten clipping ifall p p.g.a. avrundning hamnar utanför [0,1]
  p <- pmin(pmax(p, 0), 1)
  ifelse(
    is.na(p), NA_real_,
    ifelse(p <= 0 | p >= 1, 0,
           p * log(1 / p) + (1 - p) * log(1 / (1 - p)))
  )
}

# ----------------------------
# 3) Eq.1 — Metro/region entropy E by year
#    Here, Π_r is computed on the full Stockholm population in year t.
# ----------------------------
ent_metro <- df %>%
  group_by(year) %>%
  summarise(
    # Andelar i hela Stockholm det året
    p_foreign_metro = mean(g_foreign, na.rm = TRUE),
    p_lowinc_metro  = mean(g_lowinc,  na.rm = TRUE),
    p_highinc_metro = mean(g_highinc, na.rm = TRUE),
    p_lowedu_metro  = mean(g_lowedu,  na.rm = TRUE),
    p_highedu_metro = mean(g_highedu, na.rm = TRUE),
    
    # Entropy baserat på de NYSS skapade kolumnerna
    ent_foreign_metro = entropy_binary(p_foreign_metro),
    ent_lowinc_metro  = entropy_binary(p_lowinc_metro),
    ent_highinc_metro = entropy_binary(p_highinc_metro),
    ent_lowedu_metro  = entropy_binary(p_lowedu_metro),
    ent_highedu_metro = entropy_binary(p_highedu_metro),
    
    n_metro = n(),
    .groups = "drop"
  )

# ----------------------------
# 4) Eq.2 — Unit-year entropy Ei for SCHOOLS+PROGRAM
#    Π_r is now computed within each school u in year t.
# ----------------------------
ent_program <- df %>%
  filter(!is.na(gym_id_prog), gym_id_prog != "") %>%
  group_by(year, gym_id_prog) %>%
  summarise(
    n_gym_id_prog = n(),
    
    p_foreign_prog = mean(g_foreign, na.rm = TRUE),
    p_lowinc_prog  = mean(g_lowinc,  na.rm = TRUE),
    p_highinc_prog = mean(g_highinc, na.rm = TRUE),
    p_lowedu_prog  = mean(g_lowedu,  na.rm = TRUE),
    p_highedu_prog = mean(g_highedu, na.rm = TRUE),
    
    prog_ent_foreign = entropy_binary(p_foreign_prog),
    prog_ent_lowinc  = entropy_binary(p_lowinc_prog),
    prog_ent_highinc = entropy_binary(p_highinc_prog),
    prog_ent_lowedu  = entropy_binary(p_lowedu_prog),
    prog_ent_highedu = entropy_binary(p_highedu_prog),
    
    .groups = "drop"
  )

# ----------------------------
# 5) Eq.2 — Unit-year entropy Ei for NEIGHBOURHOODS (DeSO)
# ----------------------------
ent_deso <- df %>%
  filter(!is.na(deso_home), deso_home != "") %>%
  group_by(year, deso_home) %>%
  summarise(
    n_deso = n(),
    
    p_foreign_deso = mean(g_foreign, na.rm = TRUE),
    p_lowinc_deso  = mean(g_lowinc,  na.rm = TRUE),
    p_highinc_deso = mean(g_highinc, na.rm = TRUE),
    p_lowedu_deso  = mean(g_lowedu,  na.rm = TRUE),
    p_highedu_deso = mean(g_highedu, na.rm = TRUE),
    
    deso_ent_foreign = entropy_binary(p_foreign_deso),
    deso_ent_lowinc  = entropy_binary(p_lowinc_deso),
    deso_ent_highinc = entropy_binary(p_highinc_deso),
    deso_ent_lowedu  = entropy_binary(p_lowedu_deso),
    deso_ent_highedu = entropy_binary(p_highedu_deso),
    
    .groups = "drop"
  )

# ----------------------------
# 6) Merge back to student-year panel
#    Each student gets the Ei of their school and their neighbourhood in that year.
#    (And optionally the metro entropy E for that year.)
# ----------------------------
sthlm_95_23 <- df %>%
  left_join(ent_program, by = c("year", "gym_id_prog")) %>%
  left_join(ent_deso,   by = c("year", "deso_home")) %>%
  left_join(ent_metro,  by = "year")

# ----------------------------
# 7) Quick sanity checks (recommended)
#    - Entropy must be within [0, log(2)] for binary splits
#    - log(2) ~ 0.693
# ----------------------------
sthlm_95_23 %>%
  summarise(
    max_sch_ent_foreign = max(prog_ent_foreign, na.rm = TRUE),
    max_deso_ent_foreign = max(deso_ent_foreign, na.rm = TRUE),
    max_metro_foreign = max(ent_foreign_metro, na.rm = TRUE)
  )

# Saving step 15
#fwrite(sthlm_95_23,"data/btw/gym_95_23_step15_sthlm.csv",encoding = "UTF-8")
sthlm_95_23 <- fread("data/btw/gym_95_23_step15_sthlm.csv",encoding = "UTF-8")

## ------------------------------------------------------------
## Creating share variables per year for school_program and nhood  #####
## ------------------------------------------------------------

# ------------------------------------------------------------
# Helper: compute shares by (year, unit)
# ------------------------------------------------------------
share_by_year_unit <- function(data, unit_var, prefix) {
  data %>%
    filter(!is.na(.data[[unit_var]]), .data[[unit_var]] != "") %>%
    group_by(year, .data[[unit_var]]) %>%
    summarise(
      n_unit = n(),
      
      share_foreign = mean(foreign_back_lab == "Foreign background", na.rm = TRUE),
      
      share_lowinc  = mean(inc_3groups == "Low_income",  na.rm = TRUE),
      share_highinc = mean(inc_3groups == "High_income", na.rm = TRUE),
      
      share_lowedu  = mean(edu_3groups == "Low_education",  na.rm = TRUE),
      share_highedu = mean(edu_3groups == "High_education", na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    rename(unit_id = !!unit_var) %>%
    rename_with(~ paste0(prefix, .x), -c(year, unit_id))
}

# ------------------------------------------------------------
# 1) DeSO-year composition table (neighbourhood shares)
# ------------------------------------------------------------
deso_comp <- share_by_year_unit(sthlm_95_23, unit_var = "deso_home", prefix = "neigh_") %>%
  rename(deso_home = unit_id)

# ------------------------------------------------------------
# 2) School+program-year composition table (gym_id_prog shares)
# ------------------------------------------------------------
prog_comp <- share_by_year_unit(sthlm_95_23, unit_var = "gym_id_prog", prefix = "prog_") %>%
  rename(gym_id_prog = unit_id)

# (Optional) 3) School-year shares (new_id_2)
# school_comp <- share_by_year_unit(sthlm_95_23, unit_var = "new_id_2", prefix = "school_") %>%
#   rename(new_id_2 = unit_id)

# ------------------------------------------------------------
# 4) Merge back to the student-year panel
# ------------------------------------------------------------
sthlm_95_23 <- sthlm_95_23 %>%
  left_join(deso_comp, by = c("year", "deso_home")) %>%
  left_join(prog_comp, by = c("year", "gym_id_prog"))
# %>% left_join(school_comp, by = c("year", "new_id_2"))

# Saving step 16
#fwrite(sthlm_95_23,"data/btw/gym_95_23_step16_sthlm.csv",encoding = "UTF-8")
sthlm_95_23 <- fread("data/btw/gym_95_23_step16_sthlm.csv",encoding = "UTF-8")

#------------------------------------------------------------------------------------------------
# Step 17 - Creating a filter variable for later use in the analysis #####
#------------------------------------------------------------------------------------------------

df <- sthlm_95_23 %>%
  mutate(
    # --- Make NA explicit as 0 ---
    # All observations with NA in program or DeSO are automatically dropped.
    prog_n_unit = replace_na(prog_n_unit, 0L),
    n_deso      = replace_na(n_deso, 0L),
    
    # --- Program-size filter ---
    prog_size_cat = case_when(
      prog_n_unit < 10  ~ "<10",
      prog_n_unit < 15  ~ "10–14",
      prog_n_unit < 20  ~ "15–19",
      prog_n_unit < 25  ~ "20–24",
      prog_n_unit >= 25 ~ "25+"
    ),
    
    # --- DeSO-size filter ---
    deso_size_cat = case_when(
      n_deso < 10  ~ "<10",
      n_deso < 15  ~ "10–14",
      n_deso < 20  ~ "15–19",
      n_deso < 25  ~ "20–24",
      n_deso >= 25 ~ "25+"
    ),
    
    # --- Binary keep flags (most practical in models) ---
    keep_prog_10 = prog_n_unit >= 10,
    keep_prog_15 = prog_n_unit >= 15,
    keep_prog_20 = prog_n_unit >= 20,
    keep_prog_25 = prog_n_unit >= 25,
    
    keep_deso_10 = n_deso >= 10,
    keep_deso_15 = n_deso >= 15,
    keep_deso_20 = n_deso >= 20,
    keep_deso_25 = n_deso >= 25
  )

# Checking filter impact on nb of cells for analysis

df %>%
  filter(n_deso >= 5) %>%
  filter(keep_prog_10 == 1) %>%
  summarise(
    n_obs = n(),
    share_of_total = n_obs / 642453,
    n_cells = n_distinct(year, deso_home, gym_id_prog)
  )

cells_total <- df %>%
  summarise(
    n_cells_total = n_distinct(year, deso_home, gym_id_prog)
  )

cells_after <- 
df %>%
  filter(n_deso >= 5, keep_prog_10 == 1) %>% # cell drop 10 in deso and 10 in prog drops 15,6%, 5 vs 10 drops 6,5%
  summarise(
    n_cells_after = n_distinct(year, deso_home, gym_id_prog)
  )

cells_total %>%
  bind_cols(cells_after) %>%
  mutate(
    n_cells_dropped = n_cells_total - n_cells_after,
    share_cells_dropped = n_cells_dropped / n_cells_total
  )
  


sthlm_95_23 <- df %>% 
  select(
    -starts_with("keep_deso_"),
    -keep_prog_25
      )

# ============================================================
# Saving final dataset for analyses  #####
# ============================================================

fwrite(sthlm_95_23,"data/final/sthlm_95_23_final_new.csv",encoding = "UTF-8")




