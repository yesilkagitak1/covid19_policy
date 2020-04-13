### Corona policy responses project data preparation and analysis script
# Packages ----------------------------------------------------------------
library(utils)
library(httr)
library(tidyverse)
library(pmdplyr)
library(wbstats)
library(haven)
library(countrycode)
library(car)
library(survival)

# Get data and subset ----------------------------------------------------------------
# download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
# read the Dataset sheet into “R”. The dataset will be called "data".
dt <- read.csv(tf)

# subset European countries
countries = c("AUT","BEL","BGR","CHE","CYP","CZE","DEU","DNK","ESP","EST",
              "FIN","FRA","GBR","GRC","HRV","HUN","IRL","ISL","ITA","LUX",
              "LTU","LVA","MLT","NLD","NOR","POL","PRT","ROU","SVK","SVN","SWE")

europe = subset(dt, (dt$countryterritoryCode %in% countries) | dt$countriesAndTerritories=='Czechia') 


# Additional variables ------------------------------------------------------------------
# rename, make date, select only variables of interest and order
europe <- europe %>%
  mutate (country = countriesAndTerritories,
          iso3c = countryterritoryCode) %>%
  mutate (date = as.Date(dateRep, "%d/%m/%Y"),
          date2 = as.numeric(date),
          pop = popData2018) %>%
  select (-dateRep, -day, -month, -year, -countryterritoryCode, -countriesAndTerritories, -popData2018) %>%
  filter (date > '2020-01-15') %>%
  arrange (country, date)

europe$iso3c = car::recode (europe$iso3c, "''='CZE'")
europe$country = car::recode (europe$country, "'Czechia'='Czech_Republic'")
europe$pop <- ifelse (europe$geoId == 'CZ' , 10650000, europe$pop)

# fill-in the panel with NAs the missing rows          
europe <-  panel_fill(europe,
                      .set_NA = c('cases','deaths'),
                      .min =  18276,
                      .max = NA,
                      .backwards = FALSE,
                      .flag = 'miss',
                      .i = 'country',
                      .t = 'date2',
)

# replace the NAs with 0s and add the total European population
europe <- europe %>% 
  mutate(cases = replace_na(cases, 0),
         deaths = replace_na(deaths, 0),
         total.pop = 534379900,
         date = as.Date(date2, origin = '1970-01-01'))

# check things
table(droplevels(europe$geoId))
table(europe$date)

# add cumulative number of cases and deaths
europe <- europe %>% 
  group_by(geoId) %>%
  mutate(cum.cases = cumsum(cases),
         cum.deaths = cumsum(deaths))

# add Europe-level totals of cases and deaths
europe <- europe %>%
  group_by(date) %>%
  mutate (total.cases = sum (cases, na.rm=T),
          total.deaths = sum (deaths, na.rm=T),
          total.cum.cases = sum (cum.cases, na.rm=T),
          total.cum.deaths = sum (cum.deaths, na.rm=T))

# add per capita varibles
europe <- europe %>%
  mutate (cases.pc = round (cases / pop * 1e6, 0),
          deaths.pc = round (deaths / pop * 1e6, 0),
          cum.cases.pc = round (cum.cases / pop * 1e6, 0),
          cum.deaths.pc = round (cum.deaths / pop * 1e6, 0))

# Add the policy data -----------------------------------------------------

# read the file
policy <- read.csv('./data/policy_09042020.csv', stringsAsFactors = FALSE)

# rename some countries to match the other file
policy$CountryName[policy$CountryName=='Czechia'] = 'Czech_Republic'         
policy$CountryName[policy$CountryName=='United Kingdom'] = 'United_Kingdom'         
policy$schools <- as.Date(policy$schools, format='%m/%d/%Y')
policy$events <- as.Date(policy$events, format='%m/%d/%Y')
policy$lockdown <- as.Date(policy$lockdown, format='%m/%d/%Y')
policy$emerg <- as.Date(policy$emergency_powers, format='%m/%d/%Y')
policy$emerg.state <- as.Date(policy$emergency_state, format='%m/%d/%Y')


# merge
europep<-merge (europe, policy, by.x='country', by.y = 'CountryName')
# order again
europep <- europep [order(europep$country,europep$date ),]

# add indicators for whether a restriction was in place or not
for ( i in 1:nrow(europep)){
  ifelse ((europep$schools[i] > europep$date[i] | is.na(europep$schools[i]) == TRUE), europep$school.d[i] <- 0, europep$school.d[i] <- 1)
  ifelse ((europep$events[i] > europep$date[i] | is.na(europep$events[i]) == TRUE), europep$events.d[i] <- 0, europep$events.d[i] <- 1)
  ifelse ((europep$lockdown[i] > europep$date[i] | is.na(europep$lockdown[i]) == TRUE), europep$lockdown.d[i] <- 0, europep$lockdown.d[i] <- 1)
  ifelse ((europep$emerg[i] > europep$date[i] | is.na(europep$emerg[i]) == TRUE), europep$emerg.d[i] <- 0, europep$emerg.d[i] <- 1)
  ifelse ((europep$emerg.state[i] > europep$date[i] | is.na(europep$emerg.state[i]) == TRUE), europep$emerg.state.d[i] <- 0, europep$emerg.state.d[i] <- 1)
  
}

# create indicators for observations after the policy restriction events
europep$remove.school<-FALSE
europep$remove.events<-FALSE
europep$remove.lockdown<-FALSE
europep$remove.emerg<-FALSE
europep$remove.emerg.state<-FALSE

for (i in 2:nrow(europep)){
  if ((europep$country[i] == europep$country[i-1]) & europep$school.d[i-1] == 1)
    europep$remove.school[i] = TRUE
  else
    next
}

for (i in 2:nrow(europep)){
  if ((europep$country[i] == europep$country[i-1]) & europep$events.d[i-1] == 1)
    europep$remove.events[i] = TRUE
  else
    next
}

for (i in 2:nrow(europep)){
  if ((europep$country[i] == europep$country[i-1]) & europep$lockdown.d[i-1] == 1)
    europep$remove.lockdown[i] = TRUE
  else
    next
}

for (i in 2:nrow(europep)){
  if ((europep$country[i] == europep$country[i-1]) & europep$emerg.d[i-1] == 1)
    europep$remove.emerg[i] = TRUE
  else
    next
}

for (i in 2:nrow(europep)){
  if ((europep$country[i] == europep$country[i-1]) & europep$emerg.state.d[i-1] == 1)
    europep$remove.emerg.state[i] = TRUE
  else
    next
}

# Add World Bank data -----------------------------------------------------
# download the Government Effectiveness indicators
ge <- wb(indicator = c("GE.EST", 'CC.EST', 'RL.EST', 'RQ.EST', 'PV.EST'), 
         startdate = 2018, enddate = 2018, country = countries, return_wide=TRUE)
# remove unneeded variables
ge <- ge %>%
  select (-date, -iso2c, -country)
# merge with the other file
europep <- left_join (europep, ge,  by = 'iso3c')



# Get Freedom House data --------------------------------------------------
free<-read.table('./data/freedom_house_data2020.txt', header=T, sep='\t', dec='.') #text version of their files from the website

free <- free %>%
  filter (year == 2020) %>%
  mutate (iso3c = countrycode(country, 'country.name','iso3c')) %>%
  filter (row_number()!=136) %>% # remove second Cyprus!
  select (iso3c, free)

# merge with the rest
europep <- left_join (europep, free,  by = 'iso3c')


# Get Regional Autonomy data ----------------------------------------------

#rai <- read_dta ('./data/RAI country data online version.11.01.15.dta') # old years
rai <- read.table ('./data/RAI_2018_country_scores 2016.txt', sep='\t', header = T) # more recent years but unverified

rai <- rai %>%
  mutate (iso3c = countrycode(country_name, 'country.name','iso3c'),
          rai = n_RAI) %>%
  filter (year ==2016) %>%
  select (iso3c, rai)
# merge with the rest
europep <- left_join (europep, rai,  by = 'iso3c')


# Get Government composition and other data ----------------------------------------------

govs<-read_csv('./data/Corona policy sudy - Sheet1.csv')

europep<-left_join (europep, govs, by='country')

# Save full data files ---------------------------------------------------------
save(europep, file = './output data/europep09042020.RData')

write.csv(europep, file = './output data/europep09042020.csv')

# Subset for TVC analysis and save--------------------------------
# subset the data so that countries exit the data once the restrictions are enacted
tvc.s <- europep[europep$remove.school==FALSE, ] #schools

tvc.e <- europep[europep$remove.events==FALSE, ] #events

tvc.l <- europep[europep$remove.lockdown==FALSE, ] #lockdown

tvc.em <- europep[europep$remove.emerg==FALSE, ] #lockdown

tvc.ems <- europep[europep$remove.emerg.state==FALSE, ] #lockdown

write.csv(tvc.s, file = './output data/tvc_s_09042020.csv')
write.csv(tvc.e, file = './output data/tvc_e_09042020.csv')
write.csv(tvc.l, file = './output data/tvc_l_09042020.csv')
write.csv(tvc.em, file = './output data/tvc_em_09042020.csv')
write.csv(tvc.ems, file = './output data/tvc_ems_09042020.csv')

save(tvc.s, file = './output data/tvc_s_09042020.RData')
save(tvc.e, file = './output data/tvc_e_09042020.RData')
save(tvc.l, file = './output data/tvc_l_09042020.RData')
save(tvc.em, file = './output data/tvc_em_09042020.RData')
save(tvc.ems, file = './output data/tvc_ems_09042020.RData')


# Subset for OLS analysis and save--------------------------------
# subset the data to the dates on which restrictions occured and the last they if they did not


ols.s <- europep [(europep$school.d == 1 & europep$remove.school == FALSE) |
                    (is.na(europep$schools) & europep$date == max (europep$date)),]
ols.s2 <- europep [(europep$school.d == 1 & europep$remove.school == FALSE) |
                    (is.na(europep$schools) & europep$date == '2020-04-01'),]
ols.e <- europep [(europep$events.d == 1 & europep$remove.events == FALSE) |
                    (is.na(europep$events) & europep$date == max (europep$date)),]
ols.e2 <- europep [(europep$events.d == 1 & europep$remove.events == FALSE) |
                    (is.na(europep$events) & europep$date == '2020-04-01'),]
ols.l <- europep [(europep$lockdown.d == 1 & europep$remove.lockdown == FALSE) |
                    (is.na(europep$lockdown) & europep$date == max (europep$date)),]
ols.l2 <- europep [(europep$lockdown.d == 1 & europep$remove.lockdown == FALSE) |
                    (is.na(europep$lockdown) & europep$date == '2020-04-01'),]
ols.em <- europep [(europep$emerg.d == 1 & europep$remove.emerg == FALSE) |
                    (is.na(europep$emerg) & europep$date == max (europep$date)),]
ols.em2 <- europep [(europep$emerg.d == 1 & europep$remove.emerg == FALSE) |
                     (is.na(europep$emerg) & europep$date == '2020-04-01'),]
ols.ems <- europep [(europep$emerg.state.d == 1 & europep$remove.emerg.state == FALSE) |
                     (is.na(europep$emerg.state) & europep$date == max (europep$date)),]
ols.ems2 <- europep [(europep$emerg.state.d == 1 & europep$remove.emerg.state == FALSE) |
                      (is.na(europep$emerg.state) & europep$date == '2020-04-01'),]

write.csv(ols.s, file = './output data/ols_s_09042020.csv')
write.csv(ols.e, file = './output data/ols_e_09042020.csv')
write.csv(ols.l, file = './output data/ols_l_09042020.csv')
write.csv(ols.em, file = './output data/ols_em_09042020.csv')
write.csv(ols.ems, file = './output data/ols_ems_09042020.csv')

write.csv(ols.s2, file = './output data/ols_s2_09042020.csv')
write.csv(ols.e2, file = './output data/ols_e2_09042020.csv')
write.csv(ols.l2, file = './output data/ols_l2_09042020.csv')
write.csv(ols.em2, file = './output data/ols_em2_09042020.csv')
write.csv(ols.ems2, file = './output data/ols_ems2_09042020.csv')

save(ols.s2, file = './output data/ols_s2_09042020.RData')
save(ols.e2, file = './output data/ols_e2_09042020.RData')
save(ols.l2, file = './output data/ols_l2_09042020.RData')
save(ols.em2, file = './output data/ols_em2_09042020.RData')
save(ols.ems2, file = './output data/ols_ems2_09042020.RData')
# Subset for survival analysis and save--------------------------------
# subset the data to the dates on which restrictions occured and the last they if they did not

first.cases <- europep %>% 
  group_by(country) %>% 
  filter(cum.cases >= 1) %>%
  filter (row_number() == 1) %>%
  mutate (first.case.date = date) %>%
  select (country, first.case.date)

europep<-left_join (europep, first.cases, by = 'country')

europep <- europep [europep$date < '2020-04-02',]

surv.s <- europep %>% 
  group_by(country) %>% 
  filter(date == schools | date == tail(date,1)) %>% 
  group_by(country) %>%
  filter (row_number() == 1) %>%
  mutate (duration = as.numeric(date - first.case.date),
          growth.cases.daily = cum.cases / as.numeric(duration),
          censored = ifelse (is.na(schools),1,0))


surv.e <- europep %>% 
  group_by(country) %>% 
  filter(date == events | date == tail(date,1)) %>% 
  group_by(country) %>%
  filter (row_number() == 1) %>%
  mutate (duration = as.numeric(date - first.case.date),
          growth.cases.daily = cum.cases / as.numeric(duration),
          censored = ifelse (is.na(events),1,0))


surv.l <- europep %>% 
  group_by(country) %>% 
  filter(date == lockdown | date == tail(date,1)) %>% 
  group_by(country) %>%
  filter (row_number() == 1) %>%
  mutate (duration = as.numeric(date - first.case.date),
          growth.cases.daily = cum.cases / as.numeric(duration),
          censored = ifelse (is.na(lockdown),1,0))


surv.em <- europep %>% 
  group_by(country) %>% 
  filter(date == emerg | date == tail(date,1)) %>% 
  group_by(country) %>%
  filter (row_number() == 1) %>%
  mutate (duration = as.numeric(date - first.case.date),
          growth.cases.daily = cum.cases / as.numeric(duration),
          censored = ifelse (is.na(lockdown),1,0))

surv.ems <- europep %>% 
  group_by(country) %>% 
  filter(date == emerg.state | date == tail(date,1)) %>% 
  group_by(country) %>%
  filter (row_number() == 1) %>%
  mutate (duration = as.numeric(date - first.case.date),
          growth.cases.daily = cum.cases / as.numeric(duration),
          censored = ifelse (is.na(lockdown),1,0))

surv.s$surv<-Surv(surv.s$duration, surv.s$censored==0)  
surv.e$surv<-Surv(surv.e$duration, surv.e$censored==0)  
surv.l$surv<-Surv(surv.l$duration, surv.l$censored==0)  
surv.em$surv<-Surv(surv.em$duration, surv.l$censored==0)  
surv.ems$surv<-Surv(surv.ems$duration, surv.l$censored==0)  

summary(coxph(surv ~ growth.cases.daily, data = surv.l))

# to check
#data.frame(surv.l[, c('country','date', 'cum.cases', 'first.case.date', 'duration', 'growth.cases.daily', 'censored')]) 

write.csv(surv.s, file = './output data/surv_s_09042020.csv')
write.csv(surv.e, file = './output data/surv_e_09042020.csv')
write.csv(surv.l, file = './output data/surv_l_09042020.csv')
write.csv(surv.em, file = './output data/surv_l_09042020.csv')
write.csv(surv.ems, file = './output data/surv_l_09042020.csv')

save(surv.s, file = './output data/surv_s_09042020.RData')
save(surv.e, file = './output data/surv_e_09042020.RData')
save(surv.l, file = './output data/surv_l_09042020.RData')
save(surv.em, file = './output data/surv_em_09042020.RData')
save(surv.ems, file = './output data/surv_ems_09042020.RData')

