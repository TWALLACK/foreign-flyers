# This R script looks at changes in passengers of citizens vs non-citizens using data 
# from the Customs and Border Patrol.

# The raw data is available through CBP at https://awt.cbp.gov/
# Looks like it is available for the past three years online

library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(zoo)
library(ggthemes)


# load latest data 
data_2023_2025 <- read_excel("data/Awt.cbp.gov_A041_2023-01-01-2025-31-03.xlsx") %>% 
  clean_names %>% 
  select(flight_date,pax=total_passenger_count,us_pax=usa_passenger_count,non_us_pax = non_usa_passenger_count)

# load data from 2022 separately
data_2022 <- read_excel("data/Awt.cbp.gov_A041_2022-01-05-2022-31-12.xlsx") %>% 
  clean_names %>% 
  select(flight_date,pax=total_passenger_count,us_pax=usa_passenger_count,non_us_pax = non_usa_passenger_count)

# combine files
data_22_25 <-  bind_rows(data_2023_2025,data_2022)

data_22_25 <- data_22_25 %>%
  mutate(
    flight_date = ymd(flight_date),
    year  = year(flight_date),
    month = month(flight_date)
  )



# add up total passengers by month
monthly_changes <- data_22_25 %>%
  group_by(year, month) %>%
  summarize(
    us_pax_total = sum(us_pax, na.rm = TRUE),
    non_us_pax_total = sum(non_us_pax, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    yearmonth = as.yearmon(paste(year, month), "%Y %m")
  ) %>%
  arrange(yearmonth)

# create data from a year ago
table_yoy <- monthly_changes %>%
  select(month, year, us_pax_total, non_us_pax_total) %>%
  mutate(year = year + 1) %>%
  rename(
    us_pax_total_prev_year = us_pax_total,
    non_us_pax_total_prev_year = non_us_pax_total
  )

# Join the current year with the previous year’s data (matched by month and year)
table_combined <- monthly_changes %>%
  left_join(table_yoy, by = c("month", "year")) %>%
  mutate(
    us_pax_yoy_change = us_pax_total - us_pax_total_prev_year,
    non_us_pax_yoy_change = non_us_pax_total - non_us_pax_total_prev_year,
    us_pax_yoy_pct = round((us_pax_yoy_change / us_pax_total_prev_year) * 100,1),
    non_us_pax_yoy_pct = round((non_us_pax_yoy_change / non_us_pax_total_prev_year) * 100,1)
  )

# create shortened year & month column 
table_combined$yearmonth2 <-  gsub(" 20", "", table_combined$yearmonth)

write_csv(table_combined,"output/monthly_changes.csv")


plot_data <- table_combined %>%
  filter(year %in% c(2024, 2025)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  select(date, us_pax_yoy_change, non_us_pax_yoy_change) %>%
  pivot_longer(cols = c(us_pax_yoy_change, non_us_pax_yoy_change),
               names_to = "group",
               values_to = "yoy_change") %>%
  mutate(group = recode(group,
                        us_pax_yoy_change = "US Passengers",
                        non_us_pax_yoy_change = "Foreign Passengers"))




# Plot
ggplot(plot_data, aes(x = date, y = yoy_change, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_x_date(
    date_labels = "%b%y",   
    breaks = "1 month"
  ) +
  
  labs(
    title = "Pax Increase from previous year (Jan 2024 – Mar 2025)",
    y = "Passenger Change",
    color = "Passenger Type",
    caption = "Source: Customs and Border Protection"
  ) +
  theme_minimal(base_family = "sans") +
  
  theme(
    panel.grid = element_blank(),        
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA), 
    axis.text.x = element_text(size = 9) 
  ) +
  
  scale_color_fivethirtyeight()

sessionInfo()