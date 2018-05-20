## ----setup, include = FALSE, echo = FALSE, warning=FALSE-----------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>")
knitr::opts_knit$set(root.dir = normalizePath(file.path("..", "inst", "extdata")))
library(dplyr)
library(ggplot2)
library(eqvispkg)

## ----load_data-----------------------------------------------------------
raw_data <- eq_load_data("signif.txt")

## ----clean_data----------------------------------------------------------
data <- raw_data %>% eq_clean_data() %>% eq_location_clean() 
str(data)

## ----sample-timeline-1, fig.width = 8, fig.height = 6--------------------
h <- data %>% 
 filter(YEAR >= 2000 & YEAR < 2015 & is.na(FLAG_TSUNAMI)) %>%
 filter(COUNTRY == "USA")


ggplot( h, aes (x = DATE )) +
  geom_timeline(
    aes(
      size = EQ_PRIMARY,
      alpha = EQ_PRIMARY,
      colour = TOTAL_DEATHS
    )
    ) +
  scale_y_discrete() +
  theme_timeline() +
  labs(
    size = "Richter scale value",
    alpha = "Richter scale value",
    colour = "# deaths"
  ) +
  geom_timeline_label(
    aes(
      n_max = 5,
      magnitude = EQ_PRIMARY,
      label = LOCATION_NAME
    )
  )

## ----sample-timeline-2, fig.width = 8, fig.height = 6--------------------
h <- data %>%
 filter(YEAR >= 2000 & YEAR < 2015 & is.na(FLAG_TSUNAMI)) %>%
 filter(COUNTRY == "USA" | COUNTRY == "CHINA")

ggplot( h, aes (x = DATE )) +
  geom_timeline(
    aes(
      y = COUNTRY,
      size = EQ_PRIMARY,
      alpha = EQ_PRIMARY,
      colour = TOTAL_DEATHS
    )
    ) +
  scale_y_discrete() +
  theme_timeline() +
  labs(
    size = "Richter scale value",
    alpha = "Richter scale value",
    colour = "# deaths"
  ) +
  geom_timeline_label(
    aes(
      y = COUNTRY,
      n_max = 5,
      magnitude = EQ_PRIMARY,
      label = LOCATION_NAME
    )
  )


## ----eq_map, fig.width = 8, fig.height = 6-------------------------------
h <- data %>% 
 filter(YEAR >= 2000 & YEAR < 2015 & is.na(FLAG_TSUNAMI)) %>%
 filter(COUNTRY == "USA")
h %>% 
  mutate(
    popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popup_text")

