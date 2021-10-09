# keep only columns of interest
clean_aqs <- function(param, bdate, edate, state = NULL, select1 = T) {
  
  # get key info
  # key1 <- "your key"
  # usr <- create_user("your email", key1)
  
  dat <- aqs_sampleData_byState(usr, param, bdate, edate,
                              state)

  # if(nrow(dat) > 0 & select1) {
  #   dat <- (select(dat, parameter, date_local, arithmetic_mean))
  # }
  
  dat
}

# get first word
ss <- function(string) {
  strsplit(string, " ") %>% sapply(., function(x) x[[1]])
} 

# one site, multiple years
aqs_mult_years <- function(param, bdate, edate, state) {
  
  # get data from aqsr package
  year0 <- as.numeric(substr(bdate, 1, 4))
  year1 <- as.numeric(substr(edate, 1, 4))
  bdate1 <- bdate
  iter <- 1
  
  # get function
  clean_aqs1 <- function(bdate, edate) {
    clean_aqs(param, bdate, edate, state)
  }
  
  # if only 1 year, then okay
  if(year0 == year1) {
    datall <- clean_aqs1(bdate, edate)
  # multiple years
  } else {
    while(year0 < year1 & iter < 15) {
      # end of first year
      edate1 <- paste0(year0, "1231")
      dat <- clean_aqs1(bdate1, edate1)
      
      # merge together years
      if(iter == 1) {
        datall <- dat
      } else if(nrow(dat) > 0) {
        datall <- full_join(datall, dat)
      }
      
      year0 <- year0 + 1
      bdate1 <- paste0(year0, "0101")
      iter <- iter + 1
      
    }
    
    # final year
    dat <- clean_aqs1(bdate1, edate)
    # join all
    if(nrow(dat) > 0) {
      datall <- full_join(datall, dat)
    }
    
  }
  
  datall
  
}

# get multiple parameters
aqs_mult_param <- function(params, bdate, edate, state) {
  # one site, multiple years
  iter <- 1
  for(i in 1 : length(params)) {
    #print(params[i])
    #if(i == 4) {browser()}
    dat <- aqs_mult_years(params[i], bdate, edate, state) 
    
    #browser()
    
    
    if(iter == 1 & nrow(dat) > 0) {
      dat <- mutate(dat, qualifier = as.character(qualifier), cbsa_code = as.character(cbsa_code))
      
      datall <- dat
      iter <- iter + 1
    } else if(nrow(dat) > 0) {
      #browser()
      dat <- mutate(dat, qualifier = as.character(qualifier), cbsa_code = as.character(cbsa_code))
      
      datall <- (full_join(datall, dat))
    }
  }
  datall <- mutate(datall, date_local = as.Date(date_local, format = "%Y-%m-%d")) 
    
  
  datall
}





# get multiple parameters
aqs_mult_state <- function(params, bdate, edate, states, pm = F) {
  # one site, multiple years
  iter <- 1
  for(i in 1 : length(states)) {
    print(c(i, states[i]))
    #if(i == 4) {browser()}
    dat <- aqs_mult_param(params, bdate, edate, states[i]) 
    
    if(pm) {
      dat <- filter(dat, method_code == "145", sample_duration == "24 HOUR")
    }

    if(iter == 1 & nrow(dat) > 0) {
      
      # fix qualifier issue
      dat <- mutate(dat, qualifier = as.character(qualifier), cbsa_code = as.character(cbsa_code))
      
      datall <- dat
      iter <- iter + 1
    } else if(nrow(dat) > 0) {
      
      # fix qualifier issue
      dat <- mutate(dat, qualifier = as.character(qualifier), cbsa_code = as.character(cbsa_code))
      
      datall1 <- (full_join(datall, dat))
      if(class(datall1) == "try-error") {
        browser()
      } else {
        datall <- datall1
      }
    }
  }
  
  datall
}