library(tidyverse)
library(lubridate)

emperors <- read_csv("emperors-clean.csv")

parse_bc_dates <- function(ymd_string) {
  ymd_split_strings <- str_split(ymd_string, "-")
  years <- purrr::map(ymd_split_strings, function(x) as.numeric(nth(x, -3))) %>% invoke(c, .)
  print(years)
  months <- purrr::map(ymd_split_strings, function(x) as.numeric(nth(x, -2))) %>% invoke(c, .)
  days <- purrr::map(ymd_split_strings, function(x) as.numeric(last(x))) %>% invoke(c, .)
  zero_year_dates <- lubridate::ymd(glue::glue("0000-{months}-{days}", .na = "01"))
  purrr::map2(zero_year_dates, years, function(zyd, y) {
    if (y < 0) {
      zyd - lubridate::years(abs(y))
    }
    else {
      zyd + lubridate::years(y)
    }
  }) %>% purrr::invoke(c, .)
}
emperors <- emperors %>%
  mutate(reign.start = parse_bc_dates(reign.start))
  mutate(reign.length = reign.end - reign.start)
