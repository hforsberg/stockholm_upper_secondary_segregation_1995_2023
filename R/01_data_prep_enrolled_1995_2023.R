###############################################################
# 01_data_prep_enrolled_1995_2023.R
#
# Purpose:
#   Construct the main dataset for Swedish upper secondary education
#   (1995–2023), including:
#     - Enrolled students in grade 2
#     - Links to parents
#     - Home geodata (DeSO, coordinates)
#     - Primary school GPA (year 9) + yearly deciles
#     - Upper secondary school geodata + harmonised school IDs (new_id_2)
#     - Household income and parental education (LISA)
#     - Foreign background for student and parents
#     - Harmonised upper secondary programme groups (≈28)
#     - Programme-within-school ID (gym_id_prog)
#
# Output:
#   data/gym_enrolled_95_23.csv   (final output produced later in the script)
#
# Notes on reproducibility
#   - This script uses project-root relative paths and a small set of
#     directory variables. Update the directory variables in Section 0.2
#     to point to your local copies of the raw register extracts.
#
###############################################################

## ------------------------------------------------------------
## 0. Load packages
## ------------------------------------------------------------

{
  library(tidyverse)
  library(data.table)
  library(janitor)
}

## ------------------------------------------------------------
## 0.2 Project paths (edit locally)
## ------------------------------------------------------------

# Recommended: run from repo root and keep all project outputs relative.
# Raw data directories should be set to your local storage locations.
dir_raw_giso       <- "<PATH_TO_RAW_DATA>/VR-giso/"
dir_raw_overgangar <- "<PATH_TO_RAW_DATA>/vr_overgangar/"

dir_btw  <- "data/btw"
dir_out  <- "data"

## ------------------------------------------------------------
## 1. Load application data & link to parents ####
## ------------------------------------------------------------

# 1.1 Load application data and define target population
#   - One record per student: latest observed final admission
#   - Excludes 2018–2019
#   - Filters on the second year of upper secondary education (arskurs == 2)

gym_95_17 <- fread(file.path(dir_raw_giso, "data/elever_sverige/e_individer/csv_files/HF_Lev_E_EleverGymn.csv")) %>%
  filter(ar != 2018 & arskurs == 2) %>%
  rename(
    id_s = lopnr,
    year = ar,
  )

# updated data for 2018-2022
o_gym_18_22 <- fread(file.path(dir_raw_overgangar, "data/gym_reg/HF_Lev_Gymn_Elever_2014_2022.csv")) %>%
  rename_all(tolower) %>%
  filter(ar >= 2018 & arskurs == 2)

o_gym_23 <- fread(file.path(dir_raw_overgangar, "data/gym_reg/HF_Lev_Gymn_Elever_2023_2024.txt")) %>%
  rename_all(tolower) %>%
  filter(ar == 2023 & arskurs == 2)

o_gym_18_23 <- bind_rows(
  o_gym_18_22,
  o_gym_23
) %>%
  rename(
    id_s = lopnr,
    year = ar,
  )

rm(o_gym_18_22, o_gym_23)

# Check
o_gym_18_23 %>% group_by(year) %>% summarise(sum(!is.na(arskurs))) %>% view()

# 1.2 Load parents data and prepare IDs and birth country

# Parents data for 1995-2017
parents_95_21 <- fread(
  file.path(dir_raw_giso, "path[...]/HF_Lev_E_AdBioForaldrar_95_21.txt")
) %>%
  rename_all(tolower) %>%
  rename(
    id_s     = lopnr,
    id_mo    = lopnr_mor,
    id_fa    = lopnr_far,
    id_mo_ad = lopnr_admor,
    id_fa_ad = lopnr_adfar
  ) %>%
  mutate(
    # Prefer adoptive IDs when available
    id_mo = case_when(
      is.na(id_mo) ~ id_mo_ad,
      id_mo != id_mo_ad & !is.na(id_mo_ad) ~ id_mo_ad,
      TRUE ~ id_mo
    ),
    id_fa = case_when(
      is.na(id_fa) ~ id_fa_ad,
      id_fa != id_fa_ad & !is.na(id_fa_ad) ~ id_fa_ad,
      TRUE ~ id_fa
    ),
    # Prefer adoptive parents' country of birth when adoptive parent exists
    birth_mo = case_when(
      is.na(id_mo_ad) ~ fodlandgrp_mor,
      !is.na(id_mo_ad) ~ fodlandgrp_mor
    ),
    birth_fa = case_when(
      is.na(id_fa_ad) ~ fodlandgrp_far,
      !is.na(id_fa_ad) ~ fodlandgrp_far
    )
  )

# Parents data for 2018-2023
parents_18_23 <- fread(
  file.path(dir_raw_overgangar, "path[...]/HF_Lev_Skola_Foraldrar_20221231.csv"),
  showProgress = TRUE
) %>%
  rename_with(tolower) %>%
  rename(
    id_s     = lopnr,
    id_fa    = lopnr_far,
    id_mo    = lopnr_mor,
    id_fa_ad = lopnr_adfar,
    id_mo_ad = lopnr_admor,
    
    # Countries of birth (EU28 grouping in this file)
    birth_fa_bio = fodelselandfar_eu28,
    birth_mo_bio = fodelselandmor_eu28,
    birth_fa_ad  = fodelselandadfar_eu28,
    birth_mo_ad  = fodelselandadmor_eu28,
    
    # Education (SUN2020 level in this file)
    edu_fa_bio = sun2020nivafar,
    edu_mo_bio = sun2020nivamor,
    edu_mo_ad  = sun2020nivaadmor,
    edu_fa_ad  = sun2020nivaadfar
  ) %>%
  mutate(
    # -----------------------------
    # 1) Choose "active" parent id:
    # prefer adoptive id if present
    # -----------------------------
    id_mo = coalesce(id_mo_ad, id_mo),
    id_fa = coalesce(id_fa_ad, id_fa),
    
    # -----------------------------
    # 2) Choose parent birth-country:
    # if adoptive parent exists, prefer adoptive birth info
    # -----------------------------
    birth_mo = if_else(!is.na(id_mo_ad), birth_mo_ad, birth_mo_bio),
    birth_fa = if_else(!is.na(id_fa_ad), birth_fa_ad, birth_fa_bio),
    
    # -----------------------------
    # 3) Choose parent education:
    # if adoptive parent exists, prefer adoptive education
    # -----------------------------
    edu_mo = if_else(!is.na(id_mo_ad), edu_mo_ad, edu_mo_bio),
    edu_fa = if_else(!is.na(id_fa_ad), edu_fa_ad, edu_fa_bio)
  ) %>%
  # Optional: keep only harmonised columns + raw (audit trail)
  select(
    id_s, id_mo, id_fa, id_mo_ad, id_fa_ad,
    birth_mo, birth_fa, edu_mo, edu_fa,
    birth_mo_bio, birth_fa_bio, birth_mo_ad, birth_fa_ad,
    edu_mo_bio, edu_fa_bio, edu_mo_ad, edu_fa_ad
  )

# 1.3 Remove duplicated student IDs (manual cleaning)

dup_children <- parents_95_21 %>%
  count(id_s) %>%
  filter(n > 1)

parents_95_21 <- parents_95_21 %>%
  filter(
    !id_s %in% c(1498097, 3942345, 5282091, 1327258)
  )

dup_children_2 <- parents_18_23 %>%
  count(id_s) %>%
  filter(n > 1)

# Removing duplicates, keeping the one with most information
parents_18_23 <- parents_18_23 %>%
  mutate(
    info_score =
      (!is.na(id_mo)) +
      (!is.na(id_fa)) +
      (!is.na(birth_mo)) +
      (!is.na(birth_fa)) +
      (!is.na(edu_mo)) +
      (!is.na(edu_fa))
  ) %>%
  arrange(id_s, desc(info_score)) %>%
  distinct(id_s, .keep_all = TRUE) %>%
  select(-info_score)

# sanity check
nrow(parents_95_21)
n_distinct(parents_95_21$id_s)
nrow(parents_18_23)
n_distinct(parents_18_23$id_s)

rm(dup_children, dup_children_2)

# 1.4 Link parents to application data
gym_95_17 <- gym_95_17 %>%
  left_join(
    parents_95_21 %>%
      select(
        id_s,
        id_mo,
        id_fa,
        birth_mo,
        birth_fa
      ),
    by = "id_s"
  )

o_gym_18_23 <- o_gym_18_23 %>%
  left_join(
    parents_18_23 %>%
      select(
        id_s,
        id_mo,
        id_fa,
        birth_mo,
        birth_fa
      ),
    by = "id_s"
  )

# geo_data for all housholds with students 1995-2017
geo_data <- fread(file.path(dir_raw_giso, "path[...]/geodata_long_1992_2021.csv"))

gym_95_17 <- gym_95_17 %>%
  left_join(
    geo_data %>%
      select(
        id_s = lopnr,
        year,
        deso_home   = deso,
        ruta_home   = ruta,
        xkoord_home = xkoord,
        ykoord_home = ykoord
      ),
    by = c("id_s", "year")
  )

rm(geo_data)

# geo_data for all housholds with students 2018-2022
geo_data <- fread(file.path(dir_raw_overgangar, "path[...]/combined_boende_geo.csv"))

# geo_data for 2023
geo_data_23 <- fread(file.path(dir_raw_overgangar, "path[...]/HF_Lev_Ind_ruta_2023.txt")) %>%
  rename_all(tolower) %>%
  mutate(year = 2023)

o_gym_18_23 <- o_gym_18_23 %>%
  left_join(
    geo_data %>%
      transmute(
        id_s        = lopnr,
        year,
        deso_home   = deso,
        xkoord_home = x_east,
        ykoord_home = y_north
      ),
    by = c("id_s", "year")
  ) %>%
  left_join(
    geo_data_23 %>%
      transmute(
        id_s         = lopnr,
        year,
        deso_home_23   = deso_2018,
        xkoord_home_23 = x_100_sw,
        ykoord_home_23 = y_100_sw
      ),
    by = c("id_s", "year")
  ) %>%
  mutate(
    deso_home   = coalesce(deso_home, deso_home_23),
    xkoord_home = coalesce(xkoord_home, xkoord_home_23),
    ykoord_home = coalesce(ykoord_home, ykoord_home_23)
  ) %>%
  select(-deso_home_23, -xkoord_home_23, -ykoord_home_23)

rm(geo_data, geo_data_23)

gc()

## ------------------------------------------------------------
## 3. Add upper secondary school geodata and construct new_id ####
## ------------------------------------------------------------
# 4.1 Load harmonised school geodata (already cleaned: xkoord/ykoord/deso/year etc.)

geo_raw <- fread(file.path(dir_raw_giso, "path[...]/geo_gym_long_1995_2021.csv")) %>%
  as_tibble()

gym_95_17 <- gym_95_17 %>%
  mutate(id_s = as.character(id_s)
  ) %>%
  left_join(geo_raw %>% select(
    id_s = lopnr,
    year,
    deso_gym = deso,
    xkoord_gym = xkoord,
    ykoord_gym = ykoord,
    skolnamn_agg
  )) %>%
  relocate(skolnamn_agg, deso_gym, xkoord_gym, ykoord_gym, .after = lopnr_skolenhetskod)

gym_95_17 <- gym_95_17 %>%
  mutate(
    hman_2gr = case_when(
      hman == 2 ~ 1,
      TRUE ~ hman
    ),
    deso_4   = if_else(!is.na(deso_gym), str_sub(deso_gym, 6, 9), NA_character_),
    ykoord_4 = if_else(!is.na(ykoord_gym), str_sub(as.character(ykoord_gym), 1, 4), NA_character_),
    new_id   = paste(kommun, skolnamn_agg, hman_2gr, sep = "_"),
    new_id_2 = paste(kommun, skolnamn_agg, hman_2gr, ykoord_4, deso_4, sep = "_"),
  ) %>%
  relocate(hman_2gr, .after = hman) %>%
  relocate(new_id, new_id_2, .after = lopnr_skolenhetskod) %>%
  select(-deso_4, -ykoord_4)

# harmonizing the new_id_2 for the years 2018-2023 by taking skolenhetskod as default (it's the only school id for this period)

o_gym_18_23 <- o_gym_18_23 %>%
  mutate(
    new_id_2 = skolenhetskod,
    hman_2gr = case_when(
      hman == 2 ~ 1,
      TRUE ~ hman
    )
  ) %>%
  relocate(hman_2gr, .after = hman) %>%
  relocate(new_id_2, .after = skolenhetskod)

# Sanity check
unika_skolor_95_17 <- gym_95_17 %>%
  group_by(year) %>%
  summarize(
    antal_unika_skolor = n_distinct(new_id_2, na.rm = TRUE)
  ) %>%
  view()

unika_skolor_18_23 <- o_gym_18_23 %>%
  group_by(year) %>%
  summarize(
    antal_unika_skolor = n_distinct(new_id_2, na.rm = TRUE)
  ) %>%
  view()

rm(unika_skolor_95_17, unika_skolor_18_23, geo_raw)

#saving step 3
fwrite(gym_95_17, file.path(dir_btw, "gym_17_step3.csv"), encoding = "UTF-8")
fwrite(o_gym_18_23, file.path(dir_btw, "gym_23_step3.csv"), encoding = "UTF-8")

## ------------------------------------------------------------
## 4. Add primary school GPA ####
## ------------------------------------------------------------

# Securing unique observations per year and removing duplicates
gym_95_17 <- gym_95_17 %>% distinct(id_s, year, .keep_all = T)

# Add gpa from primary school year 9 1995-2017 and create yearly gpa deciles
gpa_95_21 <- fread(file.path(dir_raw_giso, "path[...]/ak9_betyg_1988_2022.csv")) %>%
  rename_all(tolower) %>%
  rename(
    id_s = lopnr,
    year = ar
  ) %>%
  distinct(id_s, year, .keep_all = T)

gpa_deciles <- gpa_95_21 %>%
  group_by(year) %>%
  mutate(
    dec_m2 = if_else(
      !is.na(meritvarde_m2),
      ntile(as.numeric(meritvarde_m2), 10),
      NA_integer_
    ),
    dec_mv = if_else(
      is.na(meritvarde_m2) & !is.na(meritvarde),
      ntile(as.numeric(meritvarde), 10),
      NA_integer_
    ),
    dec_mb = if_else(
      is.na(meritvarde_m2) & is.na(meritvarde) & !is.na(medelbetyg),
      ntile(as.numeric(medelbetyg), 10),
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  mutate(
    grade_decile = coalesce(dec_m2, dec_mv, dec_mb),
    grade_source = case_when(
      !is.na(dec_m2) ~ "meritvarde_m2",
      !is.na(dec_mv) ~ "meritvarde",
      !is.na(dec_mb) ~ "medelbetyg",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(id_s, grade_decile, .keep_all = T) %>%
  mutate(id_s = as.character(id_s))

gym_95_17 <- gym_95_17 %>%
  left_join(
    gpa_deciles %>%
      select(
        id_s,
        year_y9 = year,
        medelbetyg,
        meritvarde,
        meritvarde_m2,
        gpa_y9 = grade_decile,
        gpa_source = grade_source
      ) %>%
      distinct(id_s, .keep_all = TRUE),
    by = "id_s"
  )

rm(gpa_95_21, gpa_deciles)

# Add gpa from primary school year 9 2018-2023 and create yearly gpa deciles

# Securing unique observations per year and removing duplicates
o_gym_18_23 <- o_gym_18_23 %>% distinct(id_s, year, .keep_all = T) %>%
  mutate(id_s = as.character(id_s))

gpa_18_23 <- fread(file.path(dir_raw_overgangar, "path[...]/betyg_panel_2014_2024.csv")) %>%
  rename_all(tolower) %>%
  rename(
    id_s = lopnr,
  ) %>%
  distinct(id_s, year, .keep_all = T)

gpa_deciles <- gpa_18_23 %>%
  group_by(year) %>%
  mutate(
    dec_m2 = if_else(
      !is.na(meritvarde_m2),
      ntile(as.numeric(meritvarde_m2), 10),
      NA_integer_
    ),
    dec_mv = if_else(
      is.na(meritvarde_m2) & !is.na(meritvarde),
      ntile(as.numeric(meritvarde), 10),
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  mutate(
    grade_decile = coalesce(dec_m2, dec_mv),
    grade_source = case_when(
      !is.na(dec_m2) ~ "meritvarde_m2",
      !is.na(dec_mv) ~ "meritvarde",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(id_s, grade_decile, .keep_all = T) %>%
  mutate(id_s = as.character(id_s))

o_gym_18_23 <- o_gym_18_23 %>%
  left_join(
    gpa_deciles %>%
      select(
        id_s,
        year_y9 = year,
        meritvarde,
        meritvarde_m2,
        gpa_y9 = grade_decile,
        gpa_source = grade_source
      ) %>%
      distinct(id_s, .keep_all = TRUE),
    by = "id_s"
  )

rm(gpa_18_23, gpa_deciles)

#saving step 4
#fwrite(gym_95_17, file.path(dir_btw, "gym_17_step4.csv"), encoding = "UTF-8")
#fwrite(o_gym_18_23, file.path(dir_btw, "gym_23_step4.csv"), encoding = "UTF-8")
{
  gym_95_17 <- fread(file.path(dir_btw, "gym_17_step4.csv"), encoding = "UTF-8")
  o_gym_18_23 <- fread(file.path(dir_btw, "gym_23_step4.csv"), encoding = "UTF-8")
}
gc()

## ------------------------------------------------------------
## 5. Add household income and parental education (LISA) ####
## ------------------------------------------------------------

# Social background data from Lisa register 1995-2020
lisa_tot <- fread(file.path(dir_raw_giso, "path[...]/lisa_90_20_sthlm_gym_pop.csv")) %>%
  filter(year >= 1995 & year <= 2020) %>%
  mutate(
    id_mo = lopnr,
    id_fa = lopnr,
  )

gym_lisa <- gym_95_17 %>%
  mutate(
    id_mo = as.character(id_mo),
    id_fa = as.character(id_fa)
  ) %>%
  # ---- mother --------------------------------------------------------------
left_join(
  lisa_tot %>%
    transmute(
      id_mo = as.character(lopnr),
      year,
      sun2000niva_m  = sun2000niva,
      sun2000inr_m   = sun2000inr,
      dispinkpersf_m = dispinkpersf,
      dispinkke_m    = dispinkke,
      dispinkke04_m  = dispinkke04
    ),
  by = c("id_mo", "year")
) %>%
  # ---- father --------------------------------------------------------------
left_join(
  lisa_tot %>%
    transmute(
      id_fa = as.character(lopnr),
      year,
      sun2000niva_f  = sun2000niva,
      sun2000inr_f   = sun2000inr,
      dispinkpersf_f = dispinkpersf,
      dispinkke_f    = dispinkke,
      dispinkke04_f  = dispinkke04
    ),
  by = c("id_fa", "year")
)

gym_95_17 <- gym_lisa

# Harmonising yearly income after join
gym_95_17 <- gym_95_17 %>%
  mutate(
    # Mother yearly income
    inc_m = case_when(
      year <= 1997 ~ dispinkpersf_m,
      year <= 2004 ~ dispinkke_m,
      TRUE         ~ dispinkke04_m
    ),
    # Father yearly income
    inc_f = case_when(
      year <= 1997 ~ dispinkpersf_f,
      year <= 2004 ~ dispinkke_f,
      TRUE         ~ dispinkke04_f
    )
  )

# --- Helper: build key -------------------------------------------------------
make_key <- function(id, year) paste0(id, "|", year)

# --- 1) Mother income lookup -------------------------------------------------
mo_lookup <- gym_95_17 %>%
  transmute(
    id_mo = as.character(id_mo),
    year,
    dispinkpersf_m,
    dispinkke_m,
    dispinkke04_m
  ) %>%
  distinct(id_mo, year, .keep_all = TRUE) %>%
  mutate(
    inc_m = case_when(
      year <= 1997 ~ as.numeric(dispinkpersf_m),
      year <= 2004 ~ as.numeric(dispinkke_m),
      TRUE         ~ as.numeric(dispinkke04_m)
    ),
    inc_m = if_else(!is.na(inc_m) & inc_m < 0, NA_real_, inc_m)
  ) %>%
  arrange(id_mo, desc(year)) %>%
  group_by(id_mo) %>%
  fill(inc_m, .direction = "down") %>%
  ungroup() %>%
  mutate(key_mo = make_key(id_mo, year)) %>%
  select(key_mo, inc_m)

mo_vec <- mo_lookup$inc_m
names(mo_vec) <- mo_lookup$key_mo

# --- 2) Father income lookup --------------------------------------------------
fa_lookup <- gym_95_17 %>%
  transmute(
    id_fa = as.character(id_fa),
    year,
    dispinkpersf_f,
    dispinkke_f,
    dispinkke04_f
  ) %>%
  distinct(id_fa, year, .keep_all = TRUE) %>%
  mutate(
    inc_f = case_when(
      year <= 1997 ~ as.numeric(dispinkpersf_f),
      year <= 2004 ~ as.numeric(dispinkke_f),
      TRUE         ~ as.numeric(dispinkke04_f)
    ),
    inc_f = if_else(!is.na(inc_f) & inc_f < 0, NA_real_, inc_f)
  ) %>%
  arrange(id_fa, desc(year)) %>%
  group_by(id_fa) %>%
  fill(inc_f, .direction = "down") %>%
  ungroup() %>%
  mutate(key_fa = make_key(id_fa, year)) %>%
  select(key_fa, inc_f)

fa_vec <- fa_lookup$inc_f
names(fa_vec) <- fa_lookup$key_fa

# --- 3) Map back to gym_95_17 using match() ----------------------------------
gym_95_17 <- gym_95_17 %>%
  mutate(
    id_mo = as.character(id_mo),
    id_fa = as.character(id_fa),
    key_mo = make_key(id_mo, year),
    key_fa = make_key(id_fa, year),
    
    inc_m = mo_vec[match(key_mo, names(mo_vec))],
    inc_f = fa_vec[match(key_fa, names(fa_vec))]
  ) %>%
  mutate(
    hh_inc = case_when(
      !is.na(inc_m) & !is.na(inc_f) ~ inc_m + inc_f,
      !is.na(inc_m) ~ inc_m,
      !is.na(inc_f) ~ inc_f,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-key_mo, -key_fa)

# 4) Creating equivalised houshold income variable
gym_95_17 <- gym_95_17 %>%
  mutate(
    hh_inc_ekv = coalesce(inc_m, inc_f)) %>%
  mutate(
    hh_inc_individual = case_when(
      year <= 1997 ~ inc_m + inc_f,
      TRUE         ~ coalesce(inc_m, inc_f)
    )
  )

# Creating a houshold variable for educational level
gym_95_17 <- gym_95_17 %>%
  mutate(
    hh_edu = as.integer(case_when(
      !is.na(sun2000niva_m) & !is.na(sun2000niva_f) ~ pmax(sun2000niva_m, sun2000niva_f),
      !is.na(sun2000niva_m) ~ sun2000niva_m,
      !is.na(sun2000niva_f) ~ sun2000niva_f,
      TRUE ~ NA_real_
    ))
  ) %>%
  mutate(
    edu_niva_6gr = case_when(
      hh_edu <= 206 ~ 1,
      hh_edu <= 327 ~ 2,
      hh_edu <= 337 ~ 3,
      hh_edu == 410 ~ 4,
      hh_edu == 412 ~ 5,
      hh_edu <= 413 ~ 4,
      hh_edu == 415 ~ 4,
      hh_edu == 522 ~ 5,
      hh_edu <= 525 ~ 4,
      hh_edu <= 532 ~ 5,
      hh_edu == 535 ~ 4,
      hh_edu <= 537 ~ 5,
      hh_edu <= 557 ~ 6,
      hh_edu <= 640 ~ 7,
      TRUE ~ NA_real_
    ),
    edu_niva_6gr_lab = case_when(
      edu_niva_6gr == 1 ~ "Compulsory school",
      edu_niva_6gr == 2 ~ "Upper secondary (< = 2 years)",
      edu_niva_6gr == 3 ~ "Upper secondary (3 years)",
      edu_niva_6gr == 4 ~ "Post-secondary (non-HE)",
      edu_niva_6gr == 5 ~ "Higher education (> 3 years)",
      edu_niva_6gr == 6 ~ "Higher education (< = 4 years)",
      edu_niva_6gr == 7 ~ "PhD level",
      TRUE ~ NA_character_
    )
  )

#check
gym_95_17 %>% group_by(year) %>% summarise(sum(!is.na(hh_inc_individual))) %>% view()
gym_95_17 %>% group_by(year) %>% summarise(sum(!is.na(edu_niva_6gr_lab))) %>% view()

# saving step 5 for gym_95_17
fwrite(gym_95_17, file.path(dir_btw, "gym_17_step5.csv"), encoding = "UTF-8")

# Adding income and education variables for the period 2018-2023
lisa_18_23 <- fread(file.path(dir_raw_overgangar, "path[...]/combined_lisa_18_23.csv")) %>%
  rename_all(tolower) %>%
  select(
    lopnr,
    kommun,
    famstf,
    famtypf,
    dispinkke04,
    dispinkfam04,
    dispink04,
    forvink,
    ssyk3_2012_j16,
    sun2000niva,
    sun2000inr,
    sun2020niva,
    sun2020inr,
    year
  ) %>%
  mutate(
    sun_niva_agg = coalesce(sun2000niva, sun2020niva),
    id_mo = lopnr,
    id_fa = lopnr,
  )

#check
lisa_18_23 %>% group_by(year) %>% summarise(sum(!is.na(sun_niva_agg))) %>% view()

lisa_18_23 <- lisa_18_23 %>%
  distinct(lopnr, year, .keep_all = T)

o_gym_18_23 <- o_gym_18_23 %>%
  mutate(
    id_mo = as.character(id_mo),
    id_fa = as.character(id_fa)
  ) %>%
  # ---- mother --------------------------------------------------------------
left_join(
  lisa_18_23 %>%
    transmute(
      id_mo = as.character(lopnr),
      year,
      sun2000niva_m  = sun2000niva,
      sun2000inr_m   = sun2000inr,
      sun2020niva_m  = sun2020niva,
      sun2020inr_m   = sun2020inr,
      dispinkke04_m  = dispinkke04,
      sun_niva_agg_m = sun_niva_agg
    ),
  by = c("id_mo", "year")
) %>%
  # ---- father --------------------------------------------------------------
left_join(
  lisa_18_23 %>%
    transmute(
      id_fa = as.character(lopnr),
      year,
      sun2000niva_f  = sun2000niva,
      sun2000inr_f   = sun2000inr,
      sun2020niva_f  = sun2020niva,
      sun2020inr_f   = sun2020inr,
      dispinkke04_f  = dispinkke04,
      sun_niva_agg_f = sun_niva_agg
    ),
  by = c("id_fa", "year")
)

o_gym_18_23 <- o_gym_18_23 %>%
  mutate(
    edu_m = coalesce(sun_niva_agg_m, sun2020niva_m, sun2000niva_m),
    edu_f = coalesce(sun_niva_agg_f, sun2020niva_f, sun2000niva_f),
    hh_edu = case_when(
      !is.na(edu_m) & !is.na(edu_f) ~ pmax(edu_m, edu_f),
      !is.na(edu_m) ~ edu_m,
      !is.na(edu_f) ~ edu_f,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    edu_niva_6gr = case_when(
      hh_edu <= 206 ~ 1,
      hh_edu <= 327 ~ 2,
      hh_edu <= 337 ~ 3,
      hh_edu == 410 ~ 4,
      hh_edu == 412 ~ 5,
      hh_edu <= 413 ~ 4,
      hh_edu == 415 ~ 4,
      hh_edu == 522 ~ 5,
      hh_edu <= 525 ~ 4,
      hh_edu <= 532 ~ 5,
      hh_edu == 535 ~ 4,
      hh_edu <= 537 ~ 5,
      hh_edu <= 557 ~ 6,
      hh_edu <= 640 ~ 7,
      TRUE ~ NA_real_
    ),
    edu_niva_6gr_lab = case_when(
      edu_niva_6gr == 1 ~ "Compulsory school",
      edu_niva_6gr == 2 ~ "Upper secondary (< = 2 years)",
      edu_niva_6gr == 3 ~ "Upper secondary (3 years)",
      edu_niva_6gr == 4 ~ "Post-secondary (non-HE)",
      edu_niva_6gr == 5 ~ "Higher education (> 3 years)",
      edu_niva_6gr == 6 ~ "Higher education (< = 4 years)",
      edu_niva_6gr == 7 ~ "PhD level",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    hh_inc_ekv = coalesce(dispinkke04_m, dispinkke04_f))

#check
o_gym_18_23 %>% group_by(year) %>% summarise(sum(!is.na(edu_niva_6gr))) %>% view()

# saving step 5 for gym_18_23
fwrite(o_gym_18_23, file.path(dir_btw, "gym_23_step5.csv"), encoding = "UTF-8")

## ------------------------------------------------------------
## 6. Add foreign background (student and parents) #####
## ------------------------------------------------------------

# adding foreign background information for 1995-2017
birth <- fread(
  file.path(dir_raw_giso, "path[...]/csv_files/birth.csv")
)

gym_95_17 <- gym_95_17 %>%
  mutate(
    id_fa = as.integer(id_fa),
    id_mo = as.integer(id_mo)
  ) %>%
  # Student
  left_join(
    birth %>%
      select(
        id_s           = lopnr,
        sex            = kon,
        utlsvbakg_elev = utlsvbakg,
        fodlandgr_elev = fodlandgr
      ),
    by = "id_s"
  ) %>%
  # Father
  left_join(
    birth %>%
      select(
        id_fa        = lopnr,
        utlsvbakg_fa = utlsvbakg,
        fodlandgr_fa = fodlandgr
      ),
    by = "id_fa"
  ) %>%
  # Mother
  left_join(
    birth %>%
      select(
        id_mo        = lopnr,
        utlsvbakg_mo = utlsvbakg,
        fodlandgr_mo = fodlandgr
      ),
    by = "id_mo"
  ) %>%
  mutate(
    foreign_back = if_else(
      utlsvbakg_elev %in% c(11, 12),
      1L,
      0L
    ),
    foreign_back_lab = if_else(
      foreign_back == 1L,
      "Foreign background",
      "Swedish background"
    )
  )

# adding foreign background information for 2018-2023
birth_2 <- fread(file.path(dir_raw_overgangar, "path[...]/HF_Lev_Grunduppgifter_20241231_inkl_familj.txt")) %>%
  rename_all(tolower) %>%
  rename(
    fodlandgr = fodelseland_eu28
  )

o_gym_18_23 <- o_gym_18_23 %>%
  mutate(
    id_fa = as.integer(id_fa),
    id_mo = as.integer(id_mo)
  ) %>%
  # Student
  left_join(
    birth_2 %>%
      select(
        id_s           = lopnr,
        sex            = kon,
        utlsvbakg_elev = utlsvbakg,
        fodlandgr_elev = fodlandgr
      ),
    by = "id_s"
  ) %>%
  # Father
  left_join(
    birth_2 %>%
      select(
        id_fa        = lopnr,
        utlsvbakg_fa = utlsvbakg,
        fodlandgr_fa = fodlandgr
      ),
    by = "id_fa"
  ) %>%
  # Mother
  left_join(
    birth_2 %>%
      select(
        id_mo        = lopnr,
        utlsvbakg_mo = utlsvbakg,
        fodlandgr_mo = fodlandgr
      ),
    by = "id_mo"
  ) %>%
  mutate(
    foreign_back = if_else(
      utlsvbakg_elev %in% c(11, 12),
      1L,
      0L
    ),
    foreign_back_lab = if_else(
      foreign_back == 1L,
      "Foreign background",
      "Swedish background"
    )
  )

rm(birth, birth_2)

#saving step 6
{
  fwrite(gym_95_17, file.path(dir_btw, "gym_17_step6.csv"), encoding = "UTF-8")
  fwrite(o_gym_18_23, file.path(dir_btw, "gym_23_step6.csv"), encoding = "UTF-8")
}

## ------------------------------------------------------------
## 7. Add upper secondary programme classification groups ####
## ------------------------------------------------------------

#load step 6
gym_95_17 <- fread(file.path(dir_btw, "gym_17_step6.csv"), encoding = "UTF-8")
o_gym_18_23 <- fread(file.path(dir_btw, "gym_23_step6.csv"), encoding = "UTF-8")

# (Programme harmonisation function + application)
# -------------------------------------------------------------------------
# Programme classification and harmonisation (1995–2023)
#
# Students’ upper-secondary programmes are harmonised using a rule-based
# classifier that maps detailed study-track codes (stvkod) and auxiliary
# programme variables (program, utbildning, utbildningstyp) into a stable
# set of about 25–30 programme groups that are comparable across curricula.
#
# The main principle is to rely on the first two characters of stvkod, which
# identify the core programme family in all Swedish upper-secondary curricula
# since the mid-1990s (e.g. NV = Natural Science, SP = Social Science, TP =
# Technology, and the major vocational programmes such as EC, BP, HR, HV, VVS).
# These prefixes are mapped to canonical programme groups. If stvkod is
# missing, the broader programme type (utbildningstyp, e.g. HP or IM) is used
# as a fallback to distinguish between study-preparatory, vocational and
# introductory tracks.
#
# Specially designed programmes (SM*) are treated explicitly. Codes starting
# with SM are classified using their suffix: SMNV and SMSP/SMIP are mapped to
# a study-preparatory special-design track (SMNV), SMES is mapped to the arts
# track (ES), and all remaining SM* are mapped to a residual special-design
# category (SM). In the national registers these SM programmes disappear
# from 2018 onwards, which is consistent with the formal abolition of
# specially designed programmes under the post-2011 curriculum.
#
# All remaining rare, legacy or idiosyncratic study codes are collapsed into
# a small number of residual categories (e.g. YP for other vocational tracks),
# ensuring that the final classification yields a stable and interpretable
# programme typology that can be combined with school identifiers to define
# students’ school contexts over time.
# -------------------------------------------------------------------------

# Creating the classification function

make_prog <- function(df,
                      year_var = "year",
                      stvkod_var = "stvkod",
                      program_var = "program",
                      utbildning_var = "utbild",
                      utbildningstyp_var = "utbildningstyp") {
  
  df %>%
    mutate(
      # Raw code: prefer stvkod, else program, else utbild
      prog_raw = coalesce(
        as.character(.data[[stvkod_var]]),
        as.character(.data[[program_var]]),
        as.character(.data[[utbildning_var]])
      ) %>%
        str_trim() %>%
        na_if("") %>%
        str_to_upper(),
      
      # Programme prefix: first two characters (the key principle you want)
      prog2 = if_else(!is.na(prog_raw), str_sub(prog_raw, 1, 2), NA_character_),
      
      # SM suffix used for special handling
      sm_suffix2 = if_else(!is.na(prog_raw) & str_starts(prog_raw, "SM"),
                           str_sub(prog_raw, 3, 4),
                           NA_character_),
      
      utbildningstyp_clean = as.character(.data[[utbildningstyp_var]]) %>%
        str_trim() %>%
        na_if("") %>%
        str_to_upper()
    ) %>%
    mutate(
      prog = case_when(
        # Waldorf
        prog_raw == "W" | str_starts(prog_raw, "W") ~ "W",
        
        # Specially designed (SM*)
        str_starts(prog_raw, "SM") & sm_suffix2 == "NV" ~ "SMNV",
        str_starts(prog_raw, "SM") & sm_suffix2 %in% c("SP", "IP") ~ "SMNV",
        str_starts(prog_raw, "SM") & sm_suffix2 == "ES" ~ "ES",
        str_starts(prog_raw, "SM") ~ "SM",
        
        # Core mappings based on prefixes / known variants
        prog2 == "BA" ~ "BP",
        prog2 == "BP" ~ "BP",
        
        prog2 %in% c("BF") ~ "BF",
        
        prog2 %in% c("EC", "EE", "EN") ~ "EC",
        
        prog2 == "EK" ~ "SPEK",
        
        prog2 %in% c("EP", "ES", "ET", "MU") ~ "ES",
        
        prog2 %in% c("FP", "FT") ~ "FP",
        
        # FR: do NOT pass through as detailed codes -> collapse
        prog2 == "FR" & utbildningstyp_clean == "HP" ~ "SF",
        prog2 == "FR" & utbildningstyp_clean == "IM" ~ "IV",
        prog2 == "FR" ~ "YP",
        
        prog2 %in% c("FX", "MX") ~ "TP",
        prog2 == "TE" ~ "TP",
        prog2 == "TP" ~ "TP",
        prog2 == "T"  ~ "TP",
        
        prog2 %in% c("HA", "HP") ~ "HP",
        
        prog2 %in% c("HR", "HT") ~ "HR",
        
        prog2 == "HU" ~ "SPHU",
        
        prog2 == "HV" ~ "HV",
        
        prog2 == "IB" ~ "IB",
        
        prog2 %in% c("IM", "IV") ~ "IV",
        
        prog2 %in% c("IN", "IP", "VI") ~ "IP",
        
        prog2 %in% c("LA", "LI", "LP", "RL") ~ "LP",
        
        prog2 == "MP" ~ "MP",
        
        prog2 %in% c("NB", "NP") ~ "NP",
        
        # Natural science
        str_starts(prog_raw, "NANAT") ~ "NVNV",
        prog2 == "NA" ~ "NVIN",
        prog2 == "NV" ~ "NVNV",
        prog2 %in% c("NE") ~ "NVIN",  # optional legacy
        str_starts(prog_raw, "NVE") ~ "NVIN",
        
        # Health & care
        prog2 %in% c("OP", "VO") ~ "VOP",
        
        # Social science (prefix-based)
        prog2 %in% c("SA", "SB") ~ "SPIN",
        prog2 == "SP" ~ "SPSP",
        
        # VVS
        prog2 %in% c("VD", "VE", "VF") ~ "VVS",
        
        # If stvkod missing, use broad type
        is.na(.data[[stvkod_var]]) & utbildningstyp_clean == "HP" ~ "SF",
        is.na(.data[[stvkod_var]]) & utbildningstyp_clean == "IM" ~ "IV",
        
        # Everything else: collapse unknowns to YP (or set to "OTHER")
        TRUE ~ "YP"
      ),
      
      # Patch remaining known odd codes
      prog = case_when(
        prog %in% c("KO64", "KO913", "KO914", "KOKG", "KONÄ") ~ "HV",
        prog == "SMR" ~ "SM",
        str_to_lower(prog_raw) %in% c("vffas", "vfvvs") ~ "VVS",
        str_to_lower(prog_raw) == "improee" ~ NA_character_,
        TRUE ~ prog
      ),
      
      prog_lab = case_when(
        prog == "W"    ~ "Waldorfskola",
        prog == "SMNV" ~ "Specialutformat program (NV/SP)",
        prog == "SM"   ~ "Specialutformat program (övriga)",
        prog == "BF"   ~ "Barn- och fritidsprogrammet",
        prog == "BP"   ~ "Byggprogrammet",
        prog == "EC"   ~ "Elprogrammet",
        prog == "ES"   ~ "Estetiska programmet",
        prog == "FP"   ~ "Fordonsprogrammet",
        prog == "HP"   ~ "Handelsprogrammet",
        prog == "HR"   ~ "Hotell- och restaurangprogrammet",
        prog == "HV"   ~ "Hantverksprogrammet",
        prog == "IB"   ~ "International Baccalaureate",
        prog == "IP"   ~ "Industriprogrammet",
        prog == "IV"   ~ "Individuellt/introduktionsprogram",
        prog == "LP"   ~ "Livsmedelsprogrammet",
        prog == "MP"   ~ "Medieprogrammet",
        prog == "NP"   ~ "Naturbruksprogrammet",
        prog == "NVIN" ~ "Naturvetenskapliga programmet med inriktning",
        prog == "NVNV" ~ "Naturvetenskapliga programmet",
        prog == "SF"   ~ "Studieförberedande program (ej känd)",
        prog == "SPHU" ~ "Samhällsvetenskapliga programmet språk",
        prog == "SPIN" ~ "Samhällsvetenskapliga programmet olika inriktningar",
        prog == "SPSP" ~ "Samhällsvetenskapliga programmet",
        prog == "SPEK" ~ "Ekonomiprogrammet",
        prog == "TP"   ~ "Teknikprogrammet",
        prog == "VVS"  ~ "VVS- och fastighetsprogrammet",
        prog == "VOP"  ~ "Vård- och omsorgsprogrammet",
        prog == "YP"   ~ "Yrkesprogram övriga",
        TRUE ~ prog
      )
    ) %>%
    select(-prog2, -sm_suffix2, -utbildningstyp_clean)
}


# Running the funcktion on the different periods

gym_95_17 <- gym_95_17 %>%
  make_prog(
    year_var = "year",
    stvkod_var = "stvkod",
    program_var = "program",
    utbildning_var = "utbild",
    utbildningstyp_var = "utbildningstyp"
  ) 


o_gym_18_23 <- o_gym_18_23 %>%
  make_prog(
    year_var = "year",
    stvkod_var = "stvkod",
    program_var = "program",
    utbildning_var = "utbild",
    utbildningstyp_var = "utbildningstyp"
  )

## ------------------------------------------------------------
## 8. Create programme-within-school ID (gym_id_prog) ####
## ------------------------------------------------------------
## Creates a stable unit identifier for "programme within school" by
## combining harmonised school ID (new_id_2) with harmonised programme code (prog).

gym_95_17 <- gym_95_17 %>%
  mutate(
    gym_id_prog = str_c(new_id_2, prog, sep = "_")
  )

o_gym_18_23 <- o_gym_18_23 %>%
  mutate(
    gym_id_prog = str_c(new_id_2, prog, sep = "_")
  )

## --- Sanity checks: number of programme-within-school units per year --------
gym_95_17 %>%
  filter(!is.na(gym_id_prog)) %>%
  count(year, gym_id_prog) %>%
  count(year, name = "n_gym_id_prog") %>%
  arrange(year)

o_gym_18_23 %>%
  filter(!is.na(gym_id_prog)) %>%
  count(year, gym_id_prog) %>%
  count(year, name = "n_gym_id_prog") %>%
  arrange(year)

## --- Optional check: keep only units with at least X students (example: 15) --
gym_95_17 %>%
  filter(!is.na(gym_id_prog)) %>%
  count(year, gym_id_prog, name = "n_students") %>%
  filter(n_students >= 15) %>%
  count(year, name = "n_gym_id_prog_15plus") %>%
  arrange(year)

o_gym_18_23 %>%
  filter(!is.na(gym_id_prog)) %>%
  count(year, gym_id_prog, name = "n_students") %>%
  filter(n_students >= 15) %>%
  count(year, name = "n_gym_id_prog_15plus") %>%
  arrange(year)

## --- Optional save (generic paths) ------------------------------------------
# path_step8_95_17 <- file.path("data", "btw", "gym_17_step8.csv")
# path_step8_18_23 <- file.path("data", "btw", "gym_23_step8.csv")
# fwrite(gym_95_17, path_step8_95_17, encoding = "UTF-8")
# fwrite(o_gym_18_23, path_step8_18_23, encoding = "UTF-8")


## ------------------------------------------------------------
## 9. Combine datasets (1995–2017 + 2018–2023) ####
## ------------------------------------------------------------
## Stacks the harmonised student–school data into one dataset for 1995–2023.

o_gym_18_23 <- o_gym_18_23 %>%
  mutate(
    new_id_2 = as.character(new_id_2),
    sex      = as.character(sex)
  )

gym_95_23 <- bind_rows(
  gym_95_17,
  o_gym_18_23
)

## --- Basic completeness checks (optional) -----------------------------------
# gym_95_23 %>% group_by(year) %>% summarise(p_miss_gym_id_prog = mean(is.na(gym_id_prog)))
# gym_95_23 %>% group_by(year) %>% summarise(p_miss_foreign = mean(is.na(foreign_back)))
# gym_95_23 %>% group_by(year) %>% summarise(p_miss_edu6   = mean(is.na(edu_niva_6gr)))
# gym_95_23 %>% group_by(year) %>% summarise(p_miss_inc    = mean(is.na(hh_inc_ekv)))
# gym_95_23 %>% group_by(year) %>% summarise(p_miss_deso   = mean(is.na(deso_home)))

rm(gym_95_17, o_gym_18_23)

## --- Optional save/load (generic paths) -------------------------------------
# path_step9 <- file.path("data", "btw", "gym_95_23_step9.csv")
# fwrite(gym_95_23, path_step9, encoding = "UTF-8")
# gym_95_23 <- fread(path_step9, encoding = "UTF-8")


## ------------------------------------------------------------------
## 10. Harmonise income: 3-year rolling mean + relative position ####
## ------------------------------------------------------------------
# NOTE:
#Household income is operationalised as a three-year rolling average of equivalised disposable income. 
#Negative and zero values are treated as missing. The rolling window smooths transitory income shocks 
#and register artefacts related to capital income and tax timing, and yields a more stable proxy for 
#families’ long-term economic position. To capture families’ relative position in local school markets,
#income is further transformed into within-municipality, year-specific percentiles and quantile groups.



## 1) Clean annual income (non-positive -> NA)
## 2) Compute rolling mean over (t, t-1, t-2) using available observations
## 3) Create within-municipality, year-specific ranks and quantile groups

inc_lu <- gym_95_23 %>%
  transmute(
    id_s,
    year,
    inc0 = if_else(hh_inc_ekv > 0, as.numeric(hh_inc_ekv), NA_real_)
  ) %>%
  distinct(id_s, year, .keep_all = TRUE)

gym_95_23 <- gym_95_23 %>%
  left_join(inc_lu, by = c("id_s", "year")) %>%
  left_join(
    inc_lu %>% transmute(id_s, year = year + 1, inc1 = inc0),
    by = c("id_s", "year")
  ) %>%
  left_join(
    inc_lu %>% transmute(id_s, year = year + 2, inc2 = inc0),
    by = c("id_s", "year")
  ) %>%
  mutate(
    n_obs = (!is.na(inc0)) + (!is.na(inc1)) + (!is.na(inc2)),
    hh_inc_3y = (coalesce(inc0, 0) + coalesce(inc1, 0) + coalesce(inc2, 0)) / n_obs,
    hh_inc_3y = if_else(n_obs == 0, NA_real_, hh_inc_3y)
  ) %>%
  select(-inc0, -inc1, -inc2, -n_obs)

# Sanity checks

gym_95_23 %>%
  group_by(year) %>%
  summarise(
    mean_hh_inc_ekv = mean(hh_inc_ekv, na.rm = TRUE),
    mean_hh_inc_3y  = mean(hh_inc_3y,  na.rm = TRUE)
  ) %>%
  mutate(
    pct_diff = 100 * (mean_hh_inc_3y - mean_hh_inc_ekv) / mean_hh_inc_ekv
  ) %>%
  arrange(year) %>% view()

gym_95_23 %>%
  summarise(
    sd_1y = sd(hh_inc_ekv, na.rm = TRUE),
    sd_3y = sd(hh_inc_3y,  na.rm = TRUE),
    p99_1y = quantile(hh_inc_ekv, 0.99, na.rm = TRUE),
    p99_3y = quantile(hh_inc_3y,  0.99, na.rm = TRUE)
  )

gym_95_23 %>%
  summarise(
    share_na_1y = mean(is.na(if_else(hh_inc_ekv > 0, hh_inc_ekv, NA_real_))),
    share_na_3y = mean(is.na(hh_inc_3y)),
    share_nonpos_1y = mean(hh_inc_ekv <= 0, na.rm = TRUE)
  )

gym_95_23 %>%
  filter(!is.na(hh_inc_ekv), !is.na(hh_inc_3y)) %>%
  summarise(
    corr = cor(hh_inc_ekv, hh_inc_3y, use = "complete.obs")
  )
# -------------------------------------------------------------------------
# Diagnostics for 3-year household income (hh_inc_3y)
#
# These checks show that the 3-year rolling income behaves as intended.
# Annual means of hh_inc_3y are almost identical to the 1-year income
# (hh_inc_ekv), indicating that the long-term income level is preserved and
# that the transformation mainly smooths short-term fluctuations.
#
# The standard deviation is slightly lower for hh_inc_3y, while the 99th
# percentile is virtually unchanged. This means that transitory volatility
# in the middle of the distribution is reduced, but top incomes are not
# artificially truncated.
#
# The share of missing values is almost identical for 1-year and 3-year
# income, and the proportion of zero or negative incomes in the raw data
# is very small. Thus, the rolling average does not introduce additional
# missingness or bias.
#
# Finally, the correlation between hh_inc_ekv and hh_inc_3y is extremely high
# (~0.99), showing that households’ relative income ranking is preserved.
# Overall, hh_inc_3y provides a more robust proxy for long-term household
# economic position without distorting the underlying income hierarchy.
# -------------------------------------------------------------------------

## Within municipality × year: percent rank + quantiles
gym_95_23 <- gym_95_23 %>%
  group_by(kommun, year) %>%
  mutate(
    hh_inc_pctl  = percent_rank(hh_inc_3y),
    hh_inc_dec   = ntile(hh_inc_3y, 10),
    hh_inc_quint = ntile(hh_inc_3y, 5),
    hh_inc_terc  = ntile(hh_inc_3y, 3)
  ) %>%
  ungroup()

## --- Optional save/load (generic paths) -------------------------------------
# path_step10 <- file.path("data", "btw", "gym_95_23_step10.csv")
# fwrite(gym_95_23, path_step10, encoding = "UTF-8")
# gym_95_23 <- fread(path_step10, encoding = "UTF-8")


## ------------------------------------------------------------
## 11. Subset Stockholm County (lan == 1) ####
## ------------------------------------------------------------
sthlm_gym_95_23 <- gym_95_23 %>%
  filter(lan == 1)

## --- Optional save/load (generic paths) -------------------------------------
# path_step11 <- file.path("data", "btw", "gym_95_23_step11_sthlm.csv")
# fwrite(sthlm_gym_95_23, path_step11, encoding = "UTF-8")
# sthlm_95_23 <- fread(path_step11, encoding = "UTF-8")

## If you already saved/loaded, keep using sthlm_95_23:
sthlm_95_23 <- sthlm_gym_95_23


## ------------------------------------------------------------
## 12. Create 3-group income and education variables
## ------------------------------------------------------------
## Creates coarse groups to avoid small counts while keeping focus on tails.

sthlm_95_23 <- sthlm_95_23 %>%
  mutate(
    inc_3groups = case_when(
      hh_inc_dec <= 2 ~ "Low_income",   # bottom 20%
      hh_inc_dec <= 7 ~ "Mid_income",   # middle 50%
      hh_inc_dec >= 8 ~ "High_income",  # top 30%
      TRUE            ~ NA_character_
    ),
    edu_3groups = case_when(
      edu_niva_6gr <= 2 ~ "Low_education",
      edu_niva_6gr <= 5 ~ "Mid_education",
      edu_niva_6gr <= 7 ~ "High_education",
      TRUE              ~ NA_character_
    )
  ) %>%
  relocate(inc_3groups, .after = hh_inc_3y) %>%
  relocate(edu_3groups, .after = edu_niva_6gr_lab)

## --- Optional save/load (generic paths) -------------------------------------
# path_step12 <- file.path("data", "btw", "gym_95_23_step12_sthlm.csv")
# fwrite(sthlm_95_23, path_step12, encoding = "UTF-8")
# sthlm_95_23 <- fread(path_step12, encoding = "UTF-8")


## ------------------------------------------------------------
## 13. Variables for Figure 2 and descriptive tables ####
## ------------------------------------------------------------
## Derives programme type, provider type, and year-level system variables.

sthlm_95_23 <- sthlm_95_23 %>%
  mutate(
    prog_yp_hp = na_if(as.character(utbildningstyp), ""),
    prog_yp_hp = case_when(
      is.na(prog_yp_hp) & prog %in% c("NVNV","NVIN","SPSP","SPIN","SPHU","SPEK","TP","ES","IB","SF") ~ "HP",
      is.na(prog_yp_hp) & prog %in% c("BF","BP","EC","FP","HP","HR","HV","IP","LP","MP","NP","VOP","VVS","YP") ~ "YP",
      is.na(prog_yp_hp) & prog %in% c("IV") ~ "IV",
      prog_yp_hp %in% c("IM","IV") ~ "IV",
      prog_yp_hp %in% c("HP")      ~ "HP",
      prog_yp_hp %in% c("YP")      ~ "YP",
      prog_yp_hp == "SM"           ~ "HP",
      TRUE                         ~ prog_yp_hp
    ),
    programme_type = case_when(
      prog_yp_hp == "HP" ~ "Academic",
      prog_yp_hp == "YP" ~ "Vocational",
      prog_yp_hp == "IV" ~ "Introductory",
      TRUE               ~ NA_character_
    ),
    provider = case_when(
      hman_2gr == 1 ~ "Public",
      hman_2gr == 5 ~ "Private",
      TRUE          ~ NA_character_
    ),
    gym_id_prog_h = paste(new_id_2, prog, sep = "_"),
    gym_id_stvkod = paste(new_id_2, stvkod, sep = "_")
  )

## Year-level system variables
year_stats <- sthlm_95_23 %>%
  group_by(year) %>%
  summarise(
    cohort_size_year = n(),
    share_private_year = mean(provider == "Private", na.rm = TRUE),
    n_unique_deso_prog_year = n_distinct(paste0(deso_home, "___", gym_id_prog)),
    share_vocational_year = mean(programme_type == "Vocational", na.rm = TRUE),
    n_prog_options_year = n_distinct(gym_id_prog),
    .groups = "drop"
  ) %>%
  mutate(
    ratio_cohort_to_options_year = cohort_size_year / n_prog_options_year
  )

sthlm_95_23 <- sthlm_95_23 %>%
  left_join(year_stats, by = "year")

## --- Optional save/load (generic paths) -------------------------------------
# path_step13 <- file.path("data", "btw", "gym_95_23_step13_sthlm.csv")
# fwrite(sthlm_95_23, path_step13, encoding = "UTF-8")
# sthlm_95_23 <- fread(path_step13, encoding = "UTF-8")

## End of script
## Note: segregation indices are added in the next script (02_segregation_indices)

