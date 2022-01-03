

library(tidyverse)
library(here)
library(readxl)
library(janitor)

data_2017 <- read_excel(here(
  "raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx"))

clean_2017 <- data_2017 %>% 
  clean_names() %>% 
  mutate(id = row_number(),
         year = "2017") %>% 
  unite("year_id", year, id) %>% 
  pivot_longer(c(7:12, 13:42, 44:78, 80, 82:103, 105, 106, 109),
               names_to = "sweets",
               values_to = "rating") %>% 
  rename("age" = "q3_age",
         "gender" = "q2_gender",
         "country"= "q4_country",
         "trick_or_treating" = "q1_going_out") %>% 
  select(year_id, age, gender, trick_or_treating, sweets, 
         rating, country) %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(age > 0 & age < 120)


country_clean_2017 <- clean_2017 %>% 
  mutate(country = str_to_lower(country),
         sweets = str_replace_all(sweets, "q6_", "")) %>% 
  mutate(
    country = if_else(str_detect(country, "united states"), "US", country),
    country = if_else(str_detect(country, "us"), "US", country),
    country = if_else(str_detect(country, "usa"), "US", country),
    country = if_else(str_detect(country, "u.s.a."), "US", country),
    country = if_else(str_detect(country, "u s a"), "US", country),
    country = if_else(str_detect(country, "murica"), "US", country),
    country = if_else(str_detect(country, "murrika"), "US", country),
    country = if_else(str_detect(country, "california"), "US", country),
    country = if_else(str_detect(country, "alaska"), "US", country),
    country = if_else(str_detect(country, "'merica"), "US", country),
    country = if_else(str_detect(country, "new jersey"), "US", country),
    country = if_else(str_detect(country, "new york"), "US", country),
    country = if_else(str_detect(country, "trumpistan"), "US", country),
    country = if_else(str_detect(country, "ud"), "US", country),
    country = if_else(str_detect(country, "unite states"), "US", country),
    country = if_else(str_detect(country, "unied states"), "US", country),
    country = if_else(str_detect(country, "united state"), "US", country),
    country = if_else(str_detect(country, "united statea"), "US", country),
    country = if_else(str_detect(country, "'north carolina"), "US", country),
    country = if_else(str_detect(country, "united stated"), "US", country),
    country = if_else(str_detect(country, "us of a"), "US", country),
    country = if_else(str_detect(country, "usaa"), "US", country),
    country = if_else(str_detect(country, "ussa"), "US", country),
    country = if_else(str_detect(country, "usausausa"), "US", country),
    country = if_else(str_detect(country, "pittsburgh"), "US", country),
    country = if_else(str_detect(country, "u s"), "US", country),
    country = if_else(str_detect(country, "u.s."), "US", country),
    country = if_else(str_detect(country, "unhinged states"), "US", country),
    country = if_else(str_detect(country, "united sates"), "US", country),
    country = if_else(str_detect(country, "united staes"), "US", country),
    country = if_else(str_detect(country, "united statss"), "US", country),
    country = if_else(str_detect(country, "united ststes"), "US", country),
    country = if_else(str_detect(country, "unites states"), "US", country),
    country = if_else(str_detect(country, "america"), "US", country),
    country = if_else(str_detect(country, "ahem....amerca"), "US", country)
  ) %>% 
  
  mutate(country = str_trim(country, side = "both"),    # Change to Uk
         country = str_squish(country),
         country = str_replace_all(country, "england", "UK"),
         country = str_replace_all(country, "endland", "UK"),
         country = str_replace_all(country, "scotland", "UK"),
         country = str_replace_all(country, "[u][k]", "UK"),
         country = str_replace_all(country, "u.k.", "UK"),
         country = str_replace_all(country, "[uU]nited [kK]ingdom", "UK")
  ) %>%
  mutate(country = str_replace_all(country, "canae", "Unknow Country")
  ) %>% 
  mutate(country = str_trim(country, side = "both"),    # Change to Canada
         country = str_squish(country),
         country = str_replace_all(country, "[cC]anada", "Canada"),
         country = str_replace_all(country, "can", "Canada")
  ) %>%
  mutate(country = if_else(str_detect(country, "Canada`"), 
                           "Canada", country)) %>% 
  
  
  mutate(country = if_else(str_detect(country,   # change to Unknown Country
                                      "atlantis"), "Unknown Country", country),
         country = if_else(str_detect(country, 
                                    "fear and loathing"), "Unknown Country", 
                                    country),
         country = if_else(str_detect(country, 
                                    "i don't know anymore"), "Unknown Country", 
                                    country),
         country = if_else(str_detect(country, 
                                      "insanity lately"), "Unknown Country", 
                                      country),
         country = if_else(str_detect(country, 
                                      "narnia"), "Unknown Country", country),
         country = if_else(str_detect(country, 
                                    "soviet CanadaUKstan"), "Unknown Country", 
                                    country),
         country = if_else(str_detect(country, 
                                      "[0-9]|[0-9][0-9]"), "Unknown Country", 
                                      country)
         
         
  ) %>% 
  
  mutate(country = if_else(str_detect(country, "US") |# change to rest of world
                             str_detect(country, "UK") |
                             str_detect(country, "Canada") |
                             str_detect(country, "Unknown Country") |
                             is.na(country),
                           country, "all other countries")
  )

write_csv(country_clean_2017, "clean_data/2017_clean_data.csv")