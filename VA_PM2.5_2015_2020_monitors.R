# Gabi Armada 
# Pull VA data from relevant monitors for PM 2.5 2015-2020


## =============================
# installation 
## =============================
devtools::install_github("jpkeller/aqsr")
library(aqsr)
library(dplyr)
library(lubridate)
library(here)

# load Dr. Krall AQS functions 
source(here("get-daily-organized-2018-2019.R"))

## =============================
# creating a user key 
## =============================
# aqs_signup(email = 'your email')

# myuser <- create_user(email= 'your email',
#                        key= 'your key')


## =============================
# DATA REQUESTS
## =============================
data <- aqs_mult_years(param = "88101", bdate = "20150101", edate = "20201231", state = "51")
data <- data %>% 
      filter(county_code == "013" & site_number == "0020"|
             county_code == "059" & site_number == "0030"|
             county_code == "059" & site_number == "0031") %>% 
      mutate(day_of_week = case_when(wday(date_local) >= 2 & wday(date_local) <= 6 ~ 'weekday',
                                     wday(date_local) == 7 | wday(date_local) == 1 ~ 'weekend'), 
             AQS_site_ID = paste(state_code, county_code, site_number, poc, sep = "-")) %>% 
      relocate(AQS_site_ID, 1)
      

# ARLINGTON, VA 
arlington <- data %>% 
            filter(county_code == "013" & site_number == "0020")

# GROVETON, VA 
groveton <- data %>% 
            filter(county_code == "059" & site_number == "0030")
            
# SPRINGFIELD, VA 
# No 2015 data for 51-059-0031 monitor 
springfield <- data %>% 
              filter(county_code == "059" & site_number == "0031") 


save(data, file = "data.Rdata")
save(arlington, file = "arlington.Rdata")
save(groveton, file = "groveton.Rdata")           
save(springfield, file = "springfield.Rdata")

## ====================================================
# obtain PM2.5 data for all monitors in Virginia
## ====================================================
va_monitor_data <- aqs_mult_years(param = "88101", 
                                  bdate = "20150101", 
                                  edate = "20201231", 
                                  state = "51")     

va_monitor_data <- va_monitor_data %>% 
                      mutate(AQS_site_ID = paste(state_code, county_code, site_number, poc, sep = "-")) %>% 
                      relocate(AQS_site_ID, 1)

save(va_monitor_data, file = "va_monitor_data.Rdata")


              