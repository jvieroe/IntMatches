# ---------------------------------------------------------
# Preamble
# ---------------------------------------------------------
rm(list=ls())

library(rio)
library(tidyverse)
library(knitr)
library(haven)
library(lubridate)
library(janitor)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

setwd("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/15 football")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
setwd("/Users/jeppeviero/Library/Mobile Documents/com~apple~CloudDocs/Data/ESS")
list.files()

folders_vec <- list.files()

folders_vec
length(folders_vec)

ess_list <- list()

for (i in seq_along(folders_vec)) {
  
  folder <- folders_vec[i]

  print(folder)
  
  temp <- haven::read_dta(
    paste0(folder,
           "/Fixed",
           folder,
           ".dta"),
    encoding = NULL
  )
  
  temp <- temp %>% 
    select_if(names(.) %in% c('proddate',
                              'cntry',
                              'ctzcntr',
                              #atchctr,
                              'beg_month',
                              'beg_year',
                              'beg_day',
                              'beg_hour',
                              'beg_minute',
                              'end_month',
                              'end_year',
                              'end_day',
                              'end_hour',
                              'end_minute',
                              #inwdds,
                              #inwmms,
                              #inwyys,
                              #inwshh,
                              #inwsmm
                              'trstprl',
                              'trstlgl',
                              'trstplc',
                              'trstplt',
                              'trstprt',
                              'trstep',
                              'trstun',
                              'atchctr')
    )

  
  ess_list[[i]] <- temp
  
}

ess <- bind_rows(ess_list)

# ----- Create survey date (begin)
ess <- ess %>% 
  filter(!is.na(beg_day) & !is.na(beg_month) & !is.na(beg_year)) %>% 
  mutate(bdate_short_raw = paste(beg_year, beg_month, beg_day,
                             sep = "-"),
         bdate_long_raw = paste(beg_year, beg_month, beg_day, beg_hour, beg_minute,
                            sep = "-"))

ess <- ess %>% 
  mutate(bdate_short = ymd(bdate_short_raw),
         bdate_long = ymd_hm(bdate_long_raw))

ess <- ess %>% 
  filter(!is.na(bdate_short))

# ----- Create survey date (end)
ess <- ess %>% 
  mutate(edate_short_raw = paste(end_year, end_month, end_day,
                                 sep = "-"),
         edate_long_raw = paste(end_year, end_month, end_day, end_hour, end_minute,
                                sep = "-"))

ess <- ess %>% 
  mutate(edate_short = ymd(edate_short_raw),
         edate_long = ymd_hm(edate_long_raw))

# ----- Clean up data
ess <- ess %>% 
  dplyr::select(-c(beg_year, beg_month, beg_day, beg_hour, beg_minute,
                   end_year, end_month, end_day, end_hour, end_minute))

ess <- ess %>% 
  dplyr::select(-c(bdate_short_raw,
                   bdate_long_raw,
                   edate_short_raw,
                   edate_long_raw))


# ----- Mutate responses to numeric
str(ess)

ess <- ess %>% 
  mutate_at(vars(one_of(c('trstprl',
                          'trstlgl',
                          'trstplc',
                          'trstplt',
                          'trstprt',
                          'trstep',
                          'trstun',
                          'atchctr'))),
            funs(as.numeric(.)))
    
str(ess)

# ----- Export data
setwd("/Users/jeppeviero/Dropbox/02 PhD/0 Papers/15 football")
save(ess,
     file = "data/ess_surveys.Rdata")

rm(ess_list, temp, ess)
