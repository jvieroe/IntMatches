# ---------------------------------------------------------
# Preamble
# ---------------------------------------------------------
rm(list=ls())

library(rio)
library(tidyverse)
library(knitr)
library(haven)
library(lubridate)
library(survival)
library(janitor)
library(tidylog)
library(fixest)

setwd("/Users/jeppeviero/Dropbox/03 Football/IntMatches")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
games_full <- rio::import("data/IntMatches1872_2021.Rdata") %>% 
  rename(cntry_games = cntry,
         year_games = year) #%>% 
  #tibble()

ess_full <- rio::import("/Users/jeppeviero/Library/Mobile Documents/com~apple~CloudDocs/Data/ESS/ess_surveys.Rdata") %>% 
  tibble() %>% 
  mutate(ess_id = row_number()) %>% 
  select(-c(bdate_long, edate_short, edate_long))

ess_full <- ess_full %>% 
  rename(cntry_ess = cntry) %>% 
  mutate(cntry = case_when(cntry_ess == "AL" ~ "Albania",
                           cntry_ess == "AT" ~ "Austria",
                           cntry_ess == "BE" ~ "Belgium",
                           cntry_ess == "BG" ~ "Bulgaria",
                           cntry_ess == "AT" ~ "Austria",
                           cntry_ess == "CH" ~ "Switzerland",
                           cntry_ess == "CY" ~ "Cyprus",
                           cntry_ess == "CZ" ~ "Czech Republic",
                           cntry_ess == "DE" ~ "Germany",
                           cntry_ess == "DK" ~ "Denmark",
                           cntry_ess == "EE" ~ "Estonia",
                           cntry_ess == "ES" ~ "Spain",
                           cntry_ess == "FI" ~ "Finland",
                           cntry_ess == "FR" ~ "France",
                           cntry_ess == "GB" ~ "United Kingdom", # no national teams
                           cntry_ess == "GR" ~ "Greece",
                           cntry_ess == "HR" ~ "Croatia",
                           cntry_ess == "HU" ~ "Hungary",
                           cntry_ess == "IE" ~ "Ireland",
                           cntry_ess == "IL" ~ "Israel",
                           cntry_ess == "IS" ~ "Iceland",
                           cntry_ess == "IT" ~ "Italy",
                           cntry_ess == "LT" ~ "Lithuania",
                           cntry_ess == "LU" ~ "Luxembourg",
                           cntry_ess == "LV" ~ "Latvia",
                           cntry_ess == "ME" ~ "Montenegro",
                           cntry_ess == "NL" ~ "Netherlands",
                           cntry_ess == "NO" ~ "Norway",
                           cntry_ess == "PL" ~ "Poland",
                           cntry_ess == "PT" ~ "Portugal",
                           cntry_ess == "RO" ~ "Romania",
                           cntry_ess == "RS" ~ "Serbia",
                           cntry_ess == "SE" ~ "Sweden",
                           cntry_ess == "SI" ~ "Slovenia",
                           cntry_ess == "SK" ~ "Slovakia",
                           cntry_ess == "TR" ~ "Turkey",
                           cntry_ess == "UA" ~ "Ukraine",
                           cntry_ess == "RO" ~ "Romania",
                           cntry_ess == "RU" ~ "Russia",
                           cntry_ess == "XK" ~ "Kosovo")) 

ess_full <- ess_full %>% 
  filter(cntry %in% games_full$cntry_games)


# ---------------------------------------------------------
# Prep data
# ---------------------------------------------------------
ess <- ess_full %>% 
  select(bdate_short,
         cntry,
         ess_id) %>% 
  arrange(bdate_short)

games <- games_full %>% 
  arrange(date)

# ---------------------------------------------------------
# Match respondents to nearest game
# https://stackoverflow.com/questions/23342647/how-to-match-by-nearest-date-from-two-data-frames
# ---------------------------------------------------------
cntry_seq <- unique(ess$cntry)
cntry_seq

cntry_list <- list()

# ----- Match to nearest game
for (i in seq_along(cntry_seq)) {
  
  # Subset data
  print(cntry_seq[i])

  df1 <- ess %>% 
    filter(cntry == cntry_seq[i])
  
  df2 <- games %>% 
    filter(cntry_games == cntry_seq[i])
  
  # Calculate time to nearest game
  temp <- outer(df1$bdate_short, df2$date,  "-")
  ind_temp <- apply(temp, 1, function(i) which.min(abs(i)))

  # Merge with ESS data
  df3 <- df1 %>%
    cbind(., df2[ind_temp,]) %>%
    tibble()
  
  cntry_list[[i]] <- df3
  
}


# ----- Unpack data
ess_games <- bind_rows(cntry_list)

rm(df1,
   df2,
   df3,
   cntry_list)

ess_games <- ess_games %>% 
  mutate(time_diff = bdate_short - date) %>% 
  mutate(time_diff = as.numeric(time_diff)) %>% 
  mutate(abs_time_diff = abs(time_diff))


# ---------------------------------------------------------
# Match respondents to nearest previous game
# https://stackoverflow.com/questions/23342647/how-to-match-by-nearest-date-from-two-data-frames
# ---------------------------------------------------------
# ----- Create duplicated match data
col_names <- colnames(games)

col_names_before <- paste0(col_names, "_before")

games_before <- games %>%
  rename_with(~ col_names_before[which(col_names == .x)], .cols = col_names)

colnames(games_before)


# ----- Match to nearest previous game
cntry_list <- list()

for (i in seq_along(cntry_seq)) {
  
  # ----- Subset data
  print(cntry_seq[i])
  
  df1 <- ess %>% 
    filter(cntry == cntry_seq[i])
  
  df2 <- games_before %>% 
    filter(cntry_games_before == cntry_seq[i])
  
  # ----- Calculate time to nearest previous game
  temp <- outer(df1$bdate_short, df2$date,  "-")
  temp[temp < 0] <- NA
  ind_temp <- apply(temp, 1, function(i) which.min(abs(i)))

  # ----- Merge with ESS data
  df3 <- df1 %>%
    cbind(., df2[ind_temp,]) %>%
    tibble()
  
  cntry_list[[i]] <- df3
  
}

# ----- Unpack data
ess_games_before <- bind_rows(cntry_list)

rm(df1,
   df2,
   df3,
   cntry_list)

ess_games_before <- ess_games_before %>% 
  mutate(time_diff_before = bdate_short - date_before) %>% 
  mutate(time_diff_before = as.numeric(time_diff_before)) %>% 
  mutate(abs_time_diff_before = abs(time_diff_before))

rm(ess)

ess <- ess_full %>% 
  tidylog::left_join(.,
                     ess_games,
                     by = c("ess_id", "bdate_short", "cntry")) %>% 
  tidylog::left_join(.,
                     ess_games_before,
                     by = c("ess_id", "bdate_short", "cntry"))

ess <- ess %>% 
  mutate(cntry_match = paste(cntry,
                             match_id,
                             sep = "_"))

ess <- ess %>% 
  mutate(cntry_match = factor(cntry_match))

temp <- ess %>% 
  select(bdate_short,
         date,
         date_before,
         time_diff,
         time_diff_before,
         abs_time_diff,
         abs_time_diff_before)
rm(temp)

ess <- ess %>% 
  mutate(after = ifelse(time_diff > 0, 1, 0)) %>% 
  mutate(after = ifelse(time_diff == 0, NA, after)) %>% 
  mutate(after = factor(after))

tabyl(ess$result)
class(ess$result)

ess <- ess %>% 
  mutate(result = factor(result)) %>% 
  mutate(goal_diff = goals_for - goals_against)


# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
getwd()
save(ess,
     file = "data/DataMerged.Rdata")

rm(list=ls())


