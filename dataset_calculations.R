### Cross-crisis 2026 - Data Extraction #######################################

## Setup ----------------------------------------------------------------------

setwd("C:/Users/MarioFIDALGO/ACTED/IMPACT HQ-Progs Res Comms - Documents/RS Unit/4- Programs/1- MSNA/3- R&D/Cross-crisis/08. X crisis 26/data")

# Load packages

library(dplyr)
library(openxlsx)

## Function to iterate over list of countries

global_indicators <- function(
    df,
    weight_var,
    pop_group_var,
    gender_var
) {

# Standardize weight column

df <- df %>%
  mutate(weight = .data[[weight_var]])

# Leading sector in need

sector_vars <- c(
  "comp_foodsec_in_need",
  "comp_snfi_in_need",
  "comp_prot_in_need",
  "comp_health_in_need",
  "comp_wash_in_need",
  "comp_edu_in_need"
)
  
sector_vars <- sector_vars[sector_vars %in% names(df)]

sector_props <- sapply(sector_vars, function(v) {

  100 * weighted.mean(
    df[[v]],
    df$weight,
    na.rm = TRUE
  )

})

if (length(sector_props) == 0 || all(is.na(sector_props))) {

  leading_sector_in_need <- NA
  leading_sector_pct <- NA

} else {

  leading_sector_in_need <- names(which.max(sector_props))

  leading_sector_pct <- round(
    max(sector_props, na.rm = TRUE),
    1
  )

}  
  
  # Leading sector in severe need
  
  severe_sector_vars <- c(
  "comp_foodsec_in_severe_need",
  "comp_snfi_in_severe_need",
  "comp_prot_in_severe_need",
  "comp_health_in_severe_need",
  "comp_wash_in_severe_need",
  "comp_edu_in_severe_need"
  )

  severe_sector_vars <- severe_sector_vars[severe_sector_vars %in% names(df)]

severe_sector_props <- sapply(severe_sector_vars, function(v) {

  100 * weighted.mean(
    df[[v]],
    df$weight,
    na.rm = TRUE
  )

})

if (length(severe_sector_props) == 0 || all(is.na(severe_sector_props))) {

  leading_sector_in_severe_need <- NA
  leading_sector_severe_pct <- NA

} else {

  leading_sector_in_severe_need <- names(which.max(severe_sector_props))

  leading_sector_severe_pct <- round(
    max(severe_sector_props, na.rm = TRUE),
    1
  )

}  
  
  # Summary indicators

  summary_stats <- data.frame(

    leading_sector_in_need = leading_sector_in_need,
    leading_sector_pct = leading_sector_pct,
    leading_sector_in_severe_need = leading_sector_in_severe_need,
    leading_sector_severe_pct = leading_sector_severe_pct,

    prop_severe_needs_2_or_more = round(
      100 * (
        sum(df$weight[df$sector_in_severe_need_n >= 2], na.rm = TRUE) /
          sum(df$weight[!is.na(df$sector_in_severe_need_n)], na.rm = TRUE)
      ),
      1
    ),

    avg_msni = round(
      weighted.mean(
        df$msni_score,
        df$weight,
        na.rm = TRUE
      ),
      2
    ),

    avg_sectors_in_need = round(
      weighted.mean(
        df$sector_in_need_n,
        df$weight,
        na.rm = TRUE
      ),
      2
    ),

    avg_sectors_in_severe_need = round(
      weighted.mean(
        df$sector_in_severe_need_n,
        df$weight,
        na.rm = TRUE
      ),
      2
    ),

    pct_msni_severe = round(
      100 * weighted.mean(
        df$msni_in_severe_need,
        df$weight,
        na.rm = TRUE
      ),
      1
    )
  )

  # Needs profile

  needs_profile <- df %>%
    filter(!is.na(sector_needs_profile)) %>%
    group_by(sector_needs_profile) %>%
    summarise(
      weighted_n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      weighted_pct = round(
        100 * weighted_n / sum(weighted_n),
        1
      )
    ) %>%
    arrange(desc(weighted_pct))

  # Severe needs profile
  
  severe_profile <- df %>%
    filter(!is.na(sector_severe_needs_profile)) %>%
    group_by(sector_severe_needs_profile) %>%
    summarise(
      weighted_n = sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      weighted_pct = round(
        100 * weighted_n / sum(weighted_n),
        1
      )
    ) %>%
    arrange(desc(weighted_pct))

  # Population group

  if (!is.null(pop_group_var) &&
    pop_group_var %in% names(df)) {

  pop_group <- df %>%
    filter(
      !is.na(.data[[pop_group_var]]),
      !is.na(msni_in_severe_need)
    ) %>%
    group_by(group = .data[[pop_group_var]]) %>%
    summarise(
      pct_severe = round(
        100 * weighted.mean(
          msni_in_severe_need,
          weight,
          na.rm = TRUE
        ),
        1
      ),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_severe))

} else {

  pop_group <- NULL

}

  # Gender

if (!is.null(gender_var) &&
    gender_var %in% names(df)) {

  gender <- df %>%
    filter(
      !is.na(.data[[gender_var]]),
      !is.na(msni_in_severe_need)
    ) %>%
    group_by(group = .data[[gender_var]]) %>%
    summarise(
      pct_severe = round(
        100 * weighted.mean(
          msni_in_severe_need,
          weight,
          na.rm = TRUE
        ),
        1
      ),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_severe))

} else {

  gender <- NULL

}

list(
  summary = summary_stats,
  needs_profile = needs_profile,
  severe_profile = severe_profile,
  pop_group = pop_group,
  gender = gender
)

}
  
## Apply the function

# Store data as list

datasets <- list(
  car = read.xlsx("CAR2602.xlsx"),
  cmr = read.xlsx("CMR2601.xlsx", sheet = "main_dataset"),
  ner = read.xlsx("NER2601.xlsx", sheet = "main_dataset"),
  mmr = read.xlsx("MMR2602.xlsx", sheet = "msni_main"),
  hti = read.xlsx("HTI2601.xlsx", sheet = "hh_clean_data"),
  mli = read.xlsx("MLI2601.xlsx", sheet = "main_dataset")
)

# Country-specific var names

configs <- list(

  CAR = list(
    data = datasets$car,
    weight_var = "weights",
    pop_group_var = "pop_group",
    gender_var = "hoh_gender_final"
  ),

  CMR = list(
    data = datasets$cmr,
    weight_var = "weight_admin2",
    pop_group_var = "PoI",
    gender_var = "hoh_gender_final"
  ),

  NER = list(
    data = datasets$ner,
    weight_var = "weights",
    pop_group_var = "PoI",
    gender_var = "hoh_gender_final"
  ),

  MMR = list(
    data = datasets$mmr,
    weight_var = "weight_final",
    pop_group_var = "pop_group",
    gender_var = "hoh_gender_final"
  ),

  HTI = list(
  data = datasets$hti,
  weight_var = "weight",
  pop_group_var = "pop_group",
  gender_var = "hoh_gender"
),

MLI = list(
  data = datasets$mli,
  weight_var = "weights",
  pop_group_var = "PoI",
  gender_var = "hoh_gender"
)
)

# Run

results <- setNames(
  lapply(configs, function(x) {

    global_indicators(
      df = x$data,
      weight_var = x$weight_var,
      pop_group_var = x$pop_group_var,
      gender_var = x$gender_var
    )

  }),
  names(configs)
)

# Generate a summary table

summary_table <- bind_rows(
  lapply(names(results), function(country) {

    results[[country]]$summary %>%
      mutate(country = country)

  })
) %>%
  select(country, everything())

  summary_table |> 
    write.csv(summary_table, "C:/Users/MarioFIDALGO/ACTED/IMPACT HQ-Progs Res Comms - Documents/RS Unit/4- Programs/1- MSNA/3- R&D/Cross-crisis/08. X crisis 26/msni_global_indicators.csv")