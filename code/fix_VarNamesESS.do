* ============================================
* Fix (align) variable names in ESS data
* ============================================

cd "/Users/jeppeviero/Library/Mobile Documents/com~apple~CloudDocs/Data/ESS"


// ESS 1
clear all
use "ESS1e06_6/ESS1e06_6.dta"

rename inwdd beg_day
rename inwmm beg_month
rename inwyr beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwehh end_hour
rename inwemm end_minute

gen end_day = beg_day
gen end_month = beg_month
gen end_year = beg_year

save "ESS1e06_6/FixedESS1e06_6.dta", replace


// ESS 2
clear all
use "ESS2e03_6/ESS2e03_6.dta"

rename inwdd beg_day
rename inwmm beg_month
rename inwyr beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwehh end_hour
rename inwemm end_minute

gen end_day = beg_day
gen end_month = beg_month
gen end_year = beg_year


save "ESS2e03_6/FixedESS2e03_6.dta", replace


// ESS 3
clear all
use "ESS3e03_7/ESS3e03_7.dta"

rename inwdds beg_day
rename inwmms beg_month
rename inwyys beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwdde end_day
rename inwmme end_month
rename inwyye end_year

rename inwehh end_hour
rename inwemm end_minute

save "ESS3e03_7/FixedESS3e03_7.dta", replace



// ESS 4
clear all
use "ESS4e04_5/ESS4e04_5.dta"

rename inwdds beg_day
rename inwmms beg_month
rename inwyys beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwdde end_day
rename inwmme end_month
rename inwyye end_year

rename inwehh end_hour
rename inwemm end_minute

save "ESS4e04_5/FixedESS4e04_5.dta", replace


// ESS 5
clear all
use "ESS5e03_4/ESS5e03_4.dta"

rename inwdds beg_day
rename inwmms beg_month
rename inwyys beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwdde end_day
rename inwmme end_month
rename inwyye end_year

rename inwehh end_hour
rename inwemm end_minute

save "ESS5e03_4/FixedESS5e03_4.dta", replace


// ESS 6
clear all
use "ESS6e02_4/ESS6e02_4.dta"

rename inwdds beg_day
rename inwmms beg_month
rename inwyys beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwdde end_day
rename inwmme end_month
rename inwyye end_year

rename inwehh end_hour
rename inwemm end_minute

save "ESS6e02_4/FixedESS6e02_4.dta", replace


// ESS 7
clear all
use "ESS7e02_2/ESS7e02_2.dta"

rename inwdds beg_day
rename inwmms beg_month
rename inwyys beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwdde end_day
rename inwmme end_month
rename inwyye end_year

rename inwehh end_hour
rename inwemm end_minute

save "ESS7e02_2/FixedESS7e02_2.dta", replace


// ESS 8
clear all
use "ESS8e02_2/ESS8e02_2.dta"

rename inwdds beg_day
rename inwmms beg_month
rename inwyys beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwdde end_day
rename inwmme end_month
rename inwyye end_year

rename inwehh end_hour
rename inwemm end_minute

save "ESS8e02_2/FixedESS8e02_2.dta", replace


// ESS 9
clear all
use "ESS9e03_1/ESS9e03_1.dta"

rename inwdds beg_day
rename inwmms beg_month
rename inwyys beg_year

rename inwshh beg_hour
rename inwsmm beg_minute

rename inwdde end_day
rename inwmme end_month
rename inwyye end_year

rename inwehh end_hour
rename inwemm end_minute

save "ESS9e03_1/FixedESS9e03_1.dta", replace

clear all
