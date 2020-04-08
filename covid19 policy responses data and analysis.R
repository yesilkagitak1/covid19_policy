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
          date2 = as.numeric(date)) %>%
  select (-dateRep, -day, -month, -year, -countryterritoryCode, -countriesAndTerritories) %>%
  filter (date > '2020-01-15') %>%
  arrange (country, date)

europe$iso3c = car::recode (europe$iso3c, "''='CZE'")
europe$country = car::recode (europe$country, "'Czechia'='Czech_Republic'")

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
  mutate (cases.pc = round (cases / popData2018 * 1e6, 0),
          deaths.pc = round (deaths / popData2018 * 1e6, 0),
          cum.cases.pc = round (cum.cases / popData2018 * 1e6, 0),
          cum.deaths.pc = round (cum.deaths / popData2018 * 1e6, 0))

# Add the policy data -----------------------------------------------------

# read the file
policy <- read.csv('./data/policy.csv', stringsAsFactors = FALSE)
# rename some countries to match the other file
policy$CountryName[policy$CountryName=='Czechia'] = 'Czech_Republic'         
policy$CountryName[policy$CountryName=='United Kingdom'] = 'United_Kingdom'         

# merge
europep<-merge (europe, policy, by.x='country', by.y = 'CountryName')
# order again
europep <- europep [order(europep$country,europep$date ),]

# add indicators for whether a restriction was in place or not
for ( i in 1:nrow(europep)){
  ifelse ((europep$schools[i] >= europep$date[i] | is.na(europep$schools[i]) == TRUE), europep$school.d[i] <- 0, europep$school.d[i] <- 1)
  ifelse ((europep$events[i] >= europep$date[i] | is.na(europep$events[i]) == TRUE), europep$events.d[i] <- 0, europep$events.d[i] <- 1)
  ifelse ((europep$lockdown[i] >= europep$date[i] | is.na(europep$lockdown[i]) == TRUE), europep$lockdown.d[i] <- 0, europep$lockdown.d[i] <- 1)
  }

# create indicators for observations after the policy restriction events
europep$remove.school<-FALSE
europep$remove.events<-FALSE
europep$remove.lockdown<-FALSE

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

# Save data files ---------------------------------------------------------
save(europep, file = './output data/europep.RData')

write.csv(europep, file = './output data/europep.csv')

# Statistical modesl (logistic regression) --------------------------------

# subset the data so that countries exit the data once the restrictions are enacted
ds <- europep[europep$remove.school==FALSE, ] # for schools
de <- europep[europep$remove.events==FALSE, ] # for events
dl <- europep[europep$remove.lockdown==FALSE, ] # for lockdowns

summary(glm(school.d ~ cases , family = 'binomial', data = ds))
summary(glm(events.d ~ cases + date, family = 'binomial', data = de))
summary(glm(lockdown.d ~ cases + date + sgi_experts, family = 'binomial', data = dl))

# Statistical modesl (linear regression) --------------------------------

# subset the data for the dates of restrictions or last observed day for those with none
ds2 <- europep [(europep$school.d == 1 & europep$remove.school == FALSE) |
                 (is.na(europep$schools) & europep$date == max (europep$date)),]

de2 <- europep [(europep$events.d == 1 & europep$remove.events == FALSE) |
                  (is.na(europep$events) & europep$date == max (europep$date)),]

dl2 <- europep [(europep$lockdown.d == 1 & europep$remove.lockdown == FALSE) |
                  (is.na(europep$lockdown) & europep$date == max (europep$date)),]


summary(lm(log(cum.cases) ~ federalism + pm_party_family + popData2018, data=dl2))



# Plot of restriction date per number of cases ----------------------------
plot (NULL, xlim = c(as.Date("2020-03-01"), as.Date("2020-03-27")), ylim=c(0,log(15000) ))

points (x = ds2$date, y = log(ds2$cum.cases), cex=0.1)
text (ds2$iso3c, x = ds2$date, y = log(ds2$cum.cases+1), cex=0.75)


plot (NULL, xlim = c(as.Date("2020-03-10"), as.Date("2020-03-30")), ylim=c(0,log(25000) ))

points (x = dl2$date, y = log(dl2$cum.cases), cex=0.1)
text (dl2$iso3c, x = dl2$date, y = log(dl2$cum.cases), cex=0.75)

# Plot of policy by number of cases and deaths --------------------------
par (mfrow=c(1,2),
     bty='n')
temp <- ds[ds$school.d==1,]
plot (x = log(temp$cases+1), y = log(temp$deaths+1), type = 'p', cex = 0.1, ylim= c(0,5), 
      main = 'Cases and deaths when schools were closed in each country')
text (temp$iso3c, x = log(temp$cases+1), y = jitter(log(temp$deaths+1), amount = 0.1), cex = 0.75, col = 'red')

temp <- dl[dl$lockdown.d==1,]
plot (x = log(temp$cases+1), y = log(temp$deaths+1), type = 'p', cex = 0.1,
      main = 'Cases and deaths when lockdown was imposed in each country')
text (temp$iso3c, x = log(temp$cases+1), y = jitter(log(temp$deaths+1), amount = 0.1), cex = 0.75, col = 'red')
