### Cross-crisis 2026 - Data Extraction #######################################

## Setup ----------------------------------------------------------------------

setwd("C:/Users/MarioFIDALGO/ACTED/IMPACT HQ-Progs Res Comms - Documents/RS Unit/4- Programs/1- MSNA/3- R&D/Cross-crisis/08. X crisis 26/data")

# Load packages

library(dplyr)
library(openxlsx)

# Store data as list

datasets <- list(
  car = read.xlsx("CAR2602.xlsx"),
  cmr = read.xlsx("CMR2601.xlsx", sheet = "main_dataset"),
  ner = read.xlsx("NER2601.xlsx", sheet = "main_dataset"),
  mmr = read.xlsx("MMR2602.xlsx", sheet = "msni_main"),
  hti = read.xlsx("HTI2601.xlsx", sheet = "hh_clean_data"),
  mli = read.xlsx("MLI2601.xlsx", sheet = "main_dataset"),
  ven = read.xlsx("VEN2601_unvalidated.xlsx", sheet = "hh_indicadores"),
  ukr = read.xlsx("UKR2609.xlsx", sheet = "main")
)

## Function to iterate over list of countries

global_indicators <- function(
    df,
    weight_var,
    pop_group_var,
    gender_var,
    admin1_pcode_var = NULL
) {

# Standardize weight column

df <- df %>%
  mutate(weight = .data[[weight_var]])

# Recode fsl phases to numeric
  
fsl_phase_vars <- c(
  "fsl_fc_phase",
  "fclcm_phase"
)

for (v in fsl_phase_vars) {

  if (v %in% names(df)) {

    df[[v]] <- as.numeric(
      gsub("[^0-9]", "", df[[v]])
    )

  }

}

# Recode hwise4 (0 to 12) to severity score (1 to 5)
  
  if ("wash_hwise4_score" %in% names(df)) {

  df <- df %>%
    mutate(
      wash_hwise4_score = case_when(
        wash_hwise4_score >= 0  & wash_hwise4_score <= 3  ~ 1,
        wash_hwise4_score >= 4  & wash_hwise4_score <= 6  ~ 2,
        wash_hwise4_score >= 7  & wash_hwise4_score <= 8  ~ 3,
        wash_hwise4_score >= 9  & wash_hwise4_score <= 10 ~ 4,
        wash_hwise4_score >= 11 & wash_hwise4_score <= 12 ~ 5,
        TRUE ~ NA_real_
      )
    )
}
  
# Recode admin1 var

if (!is.null(admin1_pcode_var) &&
    admin1_pcode_var %in% names(df)) {

  df <- df %>%
    mutate(admin1_pcode = .data[[admin1_pcode_var]])

} else {

  df <- df %>%
    mutate(admin1_pcode = NA_character_)

}

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

  # Score distributions

  # Score distributions

score_vars <- names(df)[grepl("score", names(df), ignore.case = TRUE)]

score_distributions <- NULL

if (length(score_vars) > 0) {

  score_distributions <- lapply(score_vars, function(v) {

    temp <- df %>%
      filter(!is.na(.data[[v]])) %>%
      mutate(
        score = as.numeric(as.character(.data[[v]]))
      ) %>%
      group_by(score) %>%
      summarise(
        weighted_n = sum(weight, na.rm = TRUE),
        .groups = "drop"
      )

    temp <- merge(
      data.frame(score = 1:5),
      temp,
      by = "score",
      all.x = TRUE
    )

    temp$weighted_n[is.na(temp$weighted_n)] <- 0

    total_weight <- sum(temp$weighted_n)

    temp <- temp %>%
  mutate(
    score = as.character(score),
    weighted_pct = round(
      100 * weighted_n / total_weight,
      1
    ),
    variable = v
  )

    # Need and severe need summary rows

    summary_rows <- data.frame(
      variable = v,
      score = c("need", "severe_need"),
      weighted_n = c(
        sum(temp$weighted_n[temp$score %in% c(3, 4, 5)]),
        sum(temp$weighted_n[temp$score %in% c(4, 5)])
      ),
      weighted_pct = c(
        round(
          100 * sum(temp$weighted_n[temp$score %in% c(3, 4, 5)]) /
            total_weight,
          1
        ),
        round(
          100 * sum(temp$weighted_n[temp$score %in% c(4, 5)]) /
            total_weight,
          1
        )
      )
    )

    bind_rows(
      temp %>%
        select(
          variable,
          score,
          weighted_n,
          weighted_pct
        ),
      summary_rows
    )

  }) %>%
    bind_rows()

}

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
  
  # Admin-1

if (!is.null(admin1_pcode_var) &&
    admin1_pcode_var %in% names(df)) {

  admin1 <- df %>%
    filter(
      !is.na(admin1_pcode),
      !is.na(msni_in_severe_need)
    ) %>%
    group_by(admin1_pcode) %>%
    summarise(
      pct_msni_severe = round(
        100 * weighted.mean(
          msni_in_severe_need,
          weight,
          na.rm = TRUE
        ),
        1
      ),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_msni_severe))

} else {

  admin1 <- NULL

}

  # Put analyses in the results object list
  
list(
  summary = summary_stats,
  needs_profile = needs_profile,
  severe_profile = severe_profile,
  score_distributions = score_distributions,
  pop_group = pop_group,
  gender = gender,
  admin1 = admin1
)

}
  
## Apply the function

# Country-specific var names

configs <- list(

  CAR = list(
    data = datasets$car,
    weight_var = "weights",
    pop_group_var = "pop_group",
    gender_var = "hoh_gender_final",
    admin1_pcode_var = "admin1"
  ),

  CMR = list(
    data = datasets$cmr,
    weight_var = "weight_admin2",
    pop_group_var = "PoI",
    gender_var = "hoh_gender_final",
    admin1_pcode_var = "Admin_1"
  ),

  NER = list(
    data = datasets$ner,
    weight_var = "weights",
    pop_group_var = "PoI",
    gender_var = "hoh_gender_final",
    admin1_pcode_var = "Admin_1"
  ),

  MMR = list(
    data = datasets$mmr,
    weight_var = "weight_final",
    pop_group_var = "pop_group",
    gender_var = "hoh_gender_final",
    admin1_pcode_var = NULL # Not in the data
  ),

  HTI = list(
    data = datasets$hti,
    weight_var = "weight",
    pop_group_var = "pop_group",
    gender_var = "hoh_gender",
    admin1_pcode_var = "admin1"
),

  MLI = list(
    data = datasets$mli,
    weight_var = "weights",
    pop_group_var = "PoI",
    gender_var = "hoh_gender",
    admin1_pcode_var = "Admin_1"
),

  VEN = list(
    data = datasets$ven,
    weight_var = "weight",
    pop_group_var = "pop_group",
    gender_var = "hoh_gender",
    admin1_pcode_var = "admin1"
),

  UKR = list(
    data = datasets$ukr,
    weight_var = "weight",
    pop_group_var = NULL,
    gender_var = "hoh_gender",
    admin1_pcode_var = "admin1_pcode"
)

)

# Run

results <- setNames(
  lapply(configs, function(x) {

    global_indicators(
      df = x$data,
      weight_var = x$weight_var,
      pop_group_var = x$pop_group_var,
      gender_var = x$gender_var,
      admin1_pcode_var = x$admin1_pcode_var
    )

  }),
  names(configs)
)

# Extract top 3 needs and severe needs scores

excluded_scores <- c(
  "comp_prot_score_prot_needs_1_services",
  "comp_prot_score_prot_needs_1_justice",
  "comp_prot_score_prot_needs_2_activities",
  "comp_prot_score_prot_needs_2_social",
  "comp_prot_score_prot_needs_3",
  "fsl_fcs_score",
  "fsl_rcsi_score",
  "fsl_hhs_score",
  "comp_foodsec_score",
  "comp_wash_score",
  "comp_edu_score",
  "comp_prot_score",
  "comp_health_score",
  "comp_snfi_score",
  "msni_score"
)

score_rankings <- bind_rows(

  lapply(names(results), function(country) {

    scores <- results[[country]]$score_distributions

    if (is.null(scores)) return(NULL)

    # Top 3 in need
    need_top3 <- scores %>%
      filter(
        score == "need",
        !variable %in% excluded_scores
      ) %>%
      arrange(desc(weighted_pct)) %>%
      slice_head(n = 3)

    # Top 3 severe need
    severe_top3 <- scores %>%
      filter(
        score == "severe_need",
        !variable %in% excluded_scores
      ) %>%
      arrange(desc(weighted_pct)) %>%
      slice_head(n = 3)

    data.frame(data.frame(

      country = country,

      top1_need = ifelse(
        nrow(need_top3) >= 1,
        need_top3$variable[1],
        NA
      ),

      top1_need_pct = ifelse(
        nrow(need_top3) >= 1,
        need_top3$weighted_pct[1],
        NA
      ),

      top2_need = ifelse(
        nrow(need_top3) >= 2,
        need_top3$variable[2],
        NA
      ),

      top2_need_pct = ifelse(
        nrow(need_top3) >= 2,
        need_top3$weighted_pct[2],
        NA
      ),

      top3_need = ifelse(
        nrow(need_top3) >= 3,
        need_top3$variable[3],
        NA
      ),
      
      top3_need_pct = ifelse(
        nrow(need_top3) >= 3,
        need_top3$weighted_pct[3],
        NA
      ),

      top1_severe_need = ifelse(
        nrow(severe_top3) >= 1,
        severe_top3$variable[1],
        NA
      ),

      top1_severe_need_pct = ifelse(
        nrow(severe_top3) >= 1,
        severe_top3$weighted_pct[1],
        NA
      ),
 
      top2_severe_need = ifelse(
        nrow(severe_top3) >= 2,
        severe_top3$variable[2],
        NA
      ),

      top2_severe_need_pct = ifelse(
        nrow(severe_top3) >= 2,
        severe_top3$weighted_pct[2],
        NA
      ),
      
      top3_severe_need = ifelse(
        nrow(severe_top3) >= 3,
        severe_top3$variable[3],
        NA
      ),

      top3_severe_need_pct = ifelse(
        nrow(severe_top3) >= 3,
        severe_top3$weighted_pct[3],
        NA
      )
      
    ))

  })

)

# Generate a summary table

summary_table <- bind_rows(
  lapply(names(results), function(country) {

    results[[country]]$summary %>%
      mutate(country = country)

  })
) %>%
  select(country, everything()) %>%
  left_join(
    score_rankings,
    by = "country"
  )

  summary_table 

  # write.csv(summary_table, "C:/Users/MarioFIDALGO/ACTED/IMPACT HQ-Progs Res Comms - Documents/RS Unit/4- Programs/1- MSNA/3- R&D/Cross-crisis/08. X crisis 26/msni_global_indicators.csv")