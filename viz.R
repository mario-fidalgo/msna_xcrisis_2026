### Cross-crisis 2026 - Data Visualization ####################################

## Setup ----------------------------------------------------------------------

# Set working directory

setwd("C:/Users/MarioFIDALGO/ACTED/IMPACT HQ-Progs Res Comms - Documents/RS Unit/4- Programs/1- MSNA/3- R&D/Cross-crisis/08. X crisis 26")

# Load packages

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)
library(openxlsx)

# Load global table

df <- read.csv("convergence.csv")

## Stacked bars for in need and in severe need

plot_data <- df %>%
  arrange(prop_need) %>%
  mutate(
    country = factor(country, levels = country),
    prop_need = prop_need * 100,
    prop_need_severe = prop_need_severe * 100
  )

plot_data <- df %>%
  filter(country %in%  c("OPT - Gaza", "DRC","KEN","CAR","CMR","NER","HTI","MLI","OPT - West Bank","MMR")) |> 
  mutate(
    prop_need = prop_need * 100,
    prop_need_severe = prop_need_severe * 100
  ) %>%
  arrange(desc(prop_need_severe)) %>%
  mutate(
    country = factor(country, levels = rev(country))
  )

depth <- ggplot(plot_data, aes(y = country)) +

  # Need (outer bar)
  geom_col(
    aes(x = prop_need, fill = "Need"),
    width = 0.75
  ) +

  # Severe need (inner bar)
  geom_col(
    aes(x = prop_need_severe, fill = "Severe need"),
    width = 0.45
  ) +

  scale_fill_manual(
    values = c(
      "Need" = "#EE5859",
      "Severe need" = "#58585A"
    )
  ) +

  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +

  labs(
    x = "% of households",
    y = NULL,
    fill = NULL
  ) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    legend.direction = "horizontal",
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      colour = "#58585A",
      face = "bold"
    ),
    axis.text.x = element_text(
      colour = "#58585A"
    )
  )

ggsave(
  "depth.png",
  depth,
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

## Ranking for severe need in 2 or more sectors

plot_data <- df %>%
  filter(country %in% c(
    "OPT - Gaza",
    "DRC",
    "CAR",
    "CMR",
    "HTI",
    "MLI",
    "MMR"
  )) %>%
  mutate(
    prop_need_severe_2_or_more = prop_need_severe_2_or_more * 100
  ) %>%
  arrange(desc(prop_need_severe_2_or_more)) %>%
  mutate(
    country = factor(country, levels = rev(country))
  )

depth_severe_2 <- ggplot(
  plot_data,
  aes(
    x = prop_need_severe_2_or_more,
    y = country
  )
) +

  geom_col(
    fill = "#f83e44",
    width = 0.7
  ) +

  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +

  labs(
    x = "% of households",
    y = NULL
  ) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",

    axis.title.y = element_blank(),

    axis.text.y = element_text(
      colour = "#58585A",
      face = "bold"
    ),

    axis.text.x = element_text(
      colour = "#58585A"
    )
  )

ggsave(
  "depth_severe_2_or_more.png",
  depth_severe_2,
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

## Magnitude for 2 or more sectors

library(scales)

plot_data <- df %>%
  filter(country %in% c(
    "OPT - Gaza",
    "DRC",
    "CAR",
    "CMR",
    "HTI",
    "MLI",
    "MMR"
  )) %>%
  mutate(
    magnitude_severe_2_or_more =
      as.numeric(magnitude_severe_2_or_more)
  ) %>%
  arrange(desc(magnitude_severe_2_or_more)) %>%
  mutate(
    country = factor(country, levels = rev(country))
  )

magnitude_2_or_more <- ggplot(
  plot_data,
  aes(
    x = magnitude_severe_2_or_more,
    y = country
  )
) +

  geom_col(
    fill = "#f83e44",
    width = 0.7
  ) +

  scale_x_continuous(
    labels = label_number(
      scale = 1e-6,
      suffix = "M",
      accuracy = 0.1
    ),
    expand = c(0, 0)
  ) +

  labs(
    x = "People (millions)",
    y = NULL
  ) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",

    axis.title.y = element_blank(),

    axis.text.y = element_text(
      colour = "#58585A",
      face = "bold"
    ),

    axis.text.x = element_text(
      colour = "#58585A"
    )
  )

magnitude_2_or_more <- magnitude_2_or_more +
  geom_text(
    aes(
      label = paste0(
        round(magnitude_severe_2_or_more / 1e6, 1),
        "M"
      )
    ),
    hjust = -0.2,
    colour = "#58585A",
    size = 3.5
  ) 
   
ggsave(
  "magnitude_2_or_more.png",
  magnitude_2_or_more,
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300,
  bg = "white"
)






## Convergence of metrics

library(dplyr)
library(ggplot2)
library(openxlsx)

plot_data <- df %>%
  mutate(
    prop_need = prop_need * 100,
    prop_need_severe = prop_need_severe * 100
  ) %>%
  arrange(prop_need) %>%
  mutate(
    country = factor(country, levels = country)
  )

ggplot(plot_data, aes(y = country)) +

  # Need (outer bar)
  geom_col(
    aes(x = prop_need, fill = "Need"),
    width = 0.75
  ) +

  # Severe need (inner bar)
  geom_col(
    aes(x = prop_need_severe, fill = "Severe need"),
    width = 0.45
  ) +

  scale_fill_manual(
    values = c(
      "Need" = "#EE5859",
      "Severe need" = "#58585A"
    )
  ) +

  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +

  labs(
    x = "% of households",
    y = NULL,
    fill = NULL
  ) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      colour = "#58585A",
      face = "bold"
    ),
    axis.text.x = element_text(
      colour = "#58585A"
    )
  )

# Create quintile breaks

df$magnitude_severe <- trimws(df$magnitude_severe)
df$magnitude_severe[df$magnitude_severe %in% c("", "-")] <- NA
df$magnitude_severe <- gsub(",", "", df$magnitude_severe)
df$magnitude_severe <- as.numeric(df$magnitude_severe)

q_breaks <- quantile(
  df$magnitude_severe,
  probs = seq(0,1,0.2),
  na.rm = TRUE
)

df <- df %>%
  mutate(
    pop_class = ntile(magnitude_severe, 5)
  )

# REACH colours

reach_red   <- "#EE5859"
reach_grey  <- "#58585A"
reach_beige <- "#D2CBB8"
reach_lgrey <- "#C7C8CA"

labels <- c(
  paste0(round(q_breaks[1]/1e6,1), "-", round(q_breaks[2]/1e6,1), "M"),
  paste0(round(q_breaks[2]/1e6,1), "-", round(q_breaks[3]/1e6,1), "M"),
  paste0(round(q_breaks[3]/1e6,1), "-", round(q_breaks[4]/1e6,1), "M"),
  paste0(round(q_breaks[4]/1e6,1), "-", round(q_breaks[5]/1e6,1), "M"),
  paste0(round(q_breaks[5]/1e6,1), "+M")
)

df$pop_class <- factor(
  df$pop_class,
  levels = 1:5,
  labels = labels
)

# Plot
p <- ggplot(
  df,
  aes(
    x = average_msni,
    y = avg_sectors_severe_need,
    size = pop_class
  )
) +
  geom_point(
    fill = reach_red,
    colour = reach_grey,
    alpha = 0.75,
    shape = 21,
    stroke = 0.5
  ) +
  geom_text_repel(
    aes(label = country),
    family = "Segoe UI",
    colour = reach_grey,
    size = 3.5
  ) +
  scale_size_manual(
    values = c(5, 8, 11, 14, 18),
    name = "Estimated population"
  ) +
  labs(
    title = "Cross-Crisis Comparison",
    subtitle = "Bubble size represents estimated population affected",
    x = "Number of sectors in severe need",
    y = "Average MSNI score"
  ) +
  scale_x_continuous(
  limits = c(2.5, 4.5),
  breaks = seq(2.5, 4.5, by = 0.5)
) +
scale_y_continuous(
  limits = c(1, 2.5),
  breaks = seq(1, 2.5, by = 0.5)
) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      family = "Roboto Condensed",
      face = "bold",
      size = 18,
      colour = reach_grey,
      hjust = 0
    ),
    plot.subtitle = element_text(
      colour = reach_grey
    ),
    axis.title = element_text(
      family = "Roboto Condensed",
      colour = reach_grey
    ),
    axis.text = element_text(
      colour = reach_grey
    ),
    legend.title = element_text(
      family = "Roboto Condensed",
      size = 9
    ),
    panel.grid.major = element_line(
      colour = "#E8E9E9"
    ),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm")
)

ggsave(
  "cross_crisis_bubble.png",
  p,
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
