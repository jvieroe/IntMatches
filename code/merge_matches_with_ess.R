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

setwd("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/15 football")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
games_full <- rio::import("data/matches2000_2021.Rdata") %>% 
  rename(cntry_games = cntry,
         year_games = year)
  tibble()

ess_full <- rio::import("data/ess_surveys.Rdata") %>% 
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

tt <- ess %>% 
  filter(abs_time_diff < 5) %>% 
  filter(time_diff_before > 10 | after == 1)

tabyl(tt$after)


feols(atchctr ~ after:result + result | cntry_match,
      subset(ess,
             abs_time_diff < 2 & (time_diff_before > 5 | after == "1")),
      cluster = "cntry_match")


feols(atchctr ~ after*goal_diff | cntry_match,
      subset(ess,
             abs_time_diff < 2 & (time_diff_before > 5 | after == "1")),
      cluster = "cntry_match")




ess <- ess %>% 
  mutate(postvictory = ifelse(after == "1" & victory == 1, 1, 0))
hist(ess$postvictory)

feols(atchctr ~ postvictory, ess)
feols(atchctr ~ postvictory | cntry_match, ess)
feols(atchctr ~ postvictory | cntry_match, subset(ess,
                                            abs_time_diff < 10),
      cluster = "cntry_match")

hist(tt$time_diff)
hist(tt$time_diff_before)
tabyl(tt$after)
feols(atchctr ~ after | cntry_ess + year + match_id, tt)

feols(atchctr ~ after | cntry_ess + year + match_id, tt)
feols(atchctr ~ after:victory + victory | cntry_ess + year + match_id, tt)
feols(atchctr ~ after*victory | cntry_ess + year + match_id, tt)
feols(atchctr ~ after*victory | cntry_ess^match_id, subset(ess_games, time_diff_before < 10))

feols(trstprl ~ after | cntry_ess + year, tt)
feols(trstlgl ~ after | cntry_ess + year, tt)
feols(trstplc ~ after | cntry_ess + year, tt)
feols(trstplt ~ after | cntry_ess + year, tt)
feols(trstep ~ after | cntry_ess + year, tt)
feols(trstun ~ after | cntry_ess + year, tt)
feols(trstprt ~ after | cntry_ess + year, tt)

# # ----- Games before
# before <- outer(ess$bdate_short, games_cut$date,  "-")
# 
# #before[before < 0] <- NA
# 
# ind_before <- apply(before, 1, function(i) which.min(abs(i)))
# ind_before
# 
# # ----- Games after
# after <- outer(ess$bdate_short, games_after$date,  "-")
# 
# after[after > 0] <- NA
# 
# ind_after <- apply(after, 1, function(i) which.max(i))
# 
# 
# 
# # ----- Merge with ESS data
# ess_games <- ess %>% 
#   #select(c(ess_id, bdate_short)) %>% 
#   cbind(., games_cut[ind_before,]) %>% 
#   #cbind(., games_after[ind_after,]) %>% 
#   tibble()
# 
# ess_games <- ess_games %>% 
#   mutate(time_diff = bdate_short - date)
# 
# # ----- Calculate differences
# ess_games <- ess_games %>% 
#   mutate(since_last = bdate_short - date_before,
#          until_next = date_after - bdate_short)
# 
# 
# tt <- ess_games %>% 
#   filter(since_last < 20) %>% 
#   filter(until_next < 20)
# 
# 
# 
# df2 <- df2 %>% 
#   mutate(diff = bdate_short - date) %>% 
#   mutate(diff2 = as.numeric(diff))
# 
# hist(df2$diff2)
# 
# index <- neardate(ess$id,
#                   games$id,
#                   ess$date,
#                   games$date,
#                   best = "prior")
# index
# rm(index)
# 
# # get time differences
# dates_list <- list()
# 
# for (i in 1:nrow(ess)) {
#   
#   print(i)
#   
#   df <- ess[1,]
#   
#   index <- outer(df$date,
#                  games$date,
#                  "-")
#   
#   dates_list[[i]] <- index
#   
# }
# 
# dates_list[[1]]
# ess$date[1]
# games$date[1]
# games$date[11]
# ess$date[1]-games$date[11]
# 
# temp <- outer(ess$date, games$date,  "-")
# temp[1]
# 
# ess$date[1]
# games$date[1]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# index1 <- survival::neardate(ess$ess_id, games$match_id,
#                              ess$bdate_short, games$date)
# 
# index1
# 
# indx2 <- neardate(data1$id, data2$id, data1$entry.dt, data2$lab.dt, 
#                   best="prior")
# ifelse(is.na(indx1), indx2, # none after, take before
#        ifelse(is.na(indx2), indx1, #none before
#               ifelse(abs(data2$lab.dt[indx2]- data1$entry.dt) <
#                        abs(data2$lab.dt[indx1]- data1$entry.dt), indx2, indx1)))
# 
# 
# 
# 
# # NOT RUN {
# data1 <- data.frame(id = 1:10,
#                     entry.dt = as.Date(paste("2011", 1:10, "5", sep='-')))
# temp1 <- c(1,4,5,1,3,6,9, 2,7,8,12,4,6,7,10,12,3)
# data2 <- data.frame(id = c(1,1,1,2,2,4,4,5,5,5,6,8,8,9,10,10,12),
#                     lab.dt = as.Date(paste("2011", temp1, "1", sep='-')),
#                     chol = round(runif(17, 130, 280)))
# 
# #first cholesterol on or after enrollment
# class(data1$entry.dt)
# class(data2$lab.dt)
# 
# indx1 <- neardate(data1$id, data2$id, data1$entry.dt, data2$lab.dt)
# data2[indx1, "chol"]
# 
# # Closest one, either before or after. 
# # 
# indx2 <- neardate(data1$id, data2$id, data1$entry.dt, data2$lab.dt, 
#                   best="prior")
# ifelse(is.na(indx1), indx2, # none after, take before
#        ifelse(is.na(indx2), indx1, #none before
#               ifelse(abs(data2$lab.dt[indx2]- data1$entry.dt) <
#                        abs(data2$lab.dt[indx1]- data1$entry.dt), indx2, indx1)))
# 
# # closest date before or after, but no more than 21 days prior to index
# indx2 <- ifelse((data1$entry.dt - data2$lab.dt[indx2]) >21, NA, indx2)
# ifelse(is.na(indx1), indx2, # none after, take before
#        ifelse(is.na(indx2), indx1, #none before
#               ifelse(abs(data2$lab.dt[indx2]- data1$entry.dt) <
#                        abs(data2$lab.dt[indx1]- data1$entry.dt), indx2, indx1)))
# # }
# 
# 
# set.seed(123)
# df1<-data.frame(bmi=rnorm(20, 25, 5),
#                 date1=sample(seq.Date(as.Date("2014-01-01"),
#                                       as.Date("2014-02-28"),by="day"), 20))
# 
# df2<-data.frame(epi=1:5,
#                 date2=as.Date(c("2014-1-8", "2014-1-15", "2014-1-28",
#                                 "2014-2-05", "2014-2-24")))
# 
# Ind_closest_or_after <- function(d1, d2){
#   which.min(ifelse(d1 - d2 < 0, Inf, d1 - d2))
# }
# 
# # Calculate the indices
# closest_or_after_ind <- map_int(.x = df2$date2, .f = Ind_closest_or_after, d2 = df1$date1)
# 
# # Add index columns to the data frames and join
# df1 <- df1 %>%
#   mutate(ind = 1:nrow(df1))
# 
# df2 <- df2 %>%
#   mutate(ind = closest_or_after_ind)
# 
# df3 <- left_join(df2, df1, by = 'ind')

