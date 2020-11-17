# Load necessary package
library(tidyverse)
library(lubridate)

system_datetime <- as.POSIXct(Sys.time())
adjusted_datetime <- as.Date(format(system_datetime, tz = "EST",usetz = TRUE))

# Load data from John Hopkins GitHub & transform
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  filter(Province_State == "North Carolina") %>%
  filter(Admin2 != "Unassigned" & Admin2 != "Out of NC") %>%
  pivot_longer(cols = 12:(as.integer(adjusted_datetime - as.Date("2020-01-22")) + 11), names_to = "date", values_to = "cases") %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  group_by(Admin2) %>%
  mutate(daily_cases = lag(cases, n = 1)) %>%
  ungroup() %>%
  mutate(daily_cases = ifelse(is.na(daily_cases), 0, daily_cases)) %>%
  mutate(daily_cases = cases - daily_cases)

deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
  filter(Province_State == "North Carolina") %>%
  filter(Admin2 != "Unassigned" & Admin2 != "Out of NC") %>%
  pivot_longer(cols = 13:(as.integer(adjusted_datetime - as.Date("2020-01-22")) + 12), names_to = "date", values_to = "deaths") %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  group_by(Admin2) %>%
  mutate(daily_deaths = lag(deaths, n = 1)) %>%
  ungroup() %>%
  mutate(daily_deaths = ifelse(is.na(daily_deaths), 0, daily_deaths)) %>%
  mutate(daily_deaths = deaths - daily_deaths)

by_week <- confirmed %>%
  mutate(date = as.integer(week(date))) %>%
  group_by(Admin2, date) %>%
  summarise(week_total = as.integer(sum(daily_cases))) %>%
  ungroup()
  
by_dow <- confirmed %>%
  mutate(date = wday(date, label = TRUE, abbr = FALSE)) %>%
  group_by(Admin2, date) %>%
  summarise(day_total = as.integer(sum(daily_cases))) %>%
  ungroup() %>%
  group_by(Admin2) %>%
  arrange(date, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(date = as.character(date))

confirmed_map <- confirmed %>%
  group_by(Admin2) %>%
  top_n(1, date) %>%
  ungroup() %>%
  filter(Lat > 1) %>%
  left_join(select(deaths, Admin2, Population)) %>%
  distinct(Admin2, .keep_all = TRUE) %>%
  mutate(cases_per = as.integer(cases / (Population / 10000)))

combined <- confirmed %>%
  left_join(deaths) %>%
  select(Province_State, Admin2, Country_Region,Population, date, cases, deaths) %>%
  group_by(Province_State, Admin2, Country_Region) %>%
  nest() %>%
  ungroup() %>%
  mutate(data = map(.x = data, .f = as.data.frame)) %>%
  mutate(data = map(.x = data, .f = ~arrange(.x, date))) %>%
  left_join(select(confirmed_map, Admin2, cases, daily_cases, cases_per)) %>%
  select(Admin2, daily_cases, cases, cases_per, data)


