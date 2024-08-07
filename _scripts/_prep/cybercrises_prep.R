# data prep script for project on interaction between cyber incidents and international crises
# M1: cyber incidents to icb
# M2: icb -> cyber incidents -> icb hostility

# potential issues: icb includes non-rivalrous dyads, whereas dcid only covers rivals

# TO-DO: define rivalry population to whittle down ICB dyads; pull in outcome var(s) from ICB

library(tidyverse)
library(foreign)
library(ggplot2)
library(countrycode)
library(readxl)
library(data.table)
library(sandwich)
library(lmtest)
library(peacesciencer)
library(vdemdata)
library(magrittr)
library(caret)

# Pulling data ------------------------------------------------------------

dcid <- read_excel("_data/_raw/DCID_2.0_Release_update_February_2023.xlsx")
# need to add in cow codes

# making DCID directed dyad where statea is initiator
dcid <- dcid %>% mutate(ccode1_temp = countrycode(StateA, "country.name", "cown")) %>%
  mutate(ccode2_temp = countrycode(StateB, "country.name", "cown")) %>%
  mutate(ccode1 = ifelse(ccode1_temp==initiator, ccode1_temp, ccode2_temp), 
         ccode2 = ifelse(ccode1_temp==initiator, ccode2_temp, ccode1_temp),
         StateA = countrycode(ccode1, "cown", "country.name"),
         StateB = countrycode(ccode2, "cown", "country.name")) %>%
  select(-c(ccode1_temp, ccode2_temp)) %>%
  relocate(., ccode1, ccode2, .after = StateB)
  
# creating covar for previous severe incident
dcid <- dcid %>%
  mutate(previous_sev = mapply(function(a, b, sd) {
    any((a == dcid$ccode1 | b == dcid$ccode1) &
          as.Date(sd) > as.Date(dcid$interactionstartdate) &
          severity>=5)
  }, ccode1, ccode2, interactionstartdate))
table(dcid$previous_sev)
# that is very common, because US, Russia, Iran, and Israel have all done high-severity operations before
#table(dcid$severity)
#sev_df <- filter(dcid, severity>=5)
#filter(dcid, (ccode1==2 | ccode2==2 | ccode1==365 | ccode2==365 | ccode1==630 | ccode2==630 | ccode1==666 | ccode2==666) & severity>=5)


icb_dyad_year <- read_csv("_data/_raw/icbdy_v15.csv")

# creating date vars and collapsing the yearly icb_dyad data to icb-dyad-level and cutting some unnecessary 
# note: using dyadic-calculated start and end dates here. see icb documentation for details
icb_dyad <- icb_dyad_year %>% mutate(startdate = make_date(year = trgyrdy, month = trgmody, day = trgdady)) %>%
  mutate(enddate = make_date(year = trmyrdy, month = trmmody, day = trmdady)) %>%
  select(-c("year", "ongoing")) %>% # don't need these
  distinct()

# need main icb for outcome variables, including "outesr" (easing or increasing tension post-crisis)
icb <- read_csv("_data/_raw/icb2v15.csv")

# wrangling near crises data from Iakhnis and James 2021
nc <- read_excel("_data/_raw/nc2v1_actor_level_finalized.xlsx")

nc <- nc %>%
  filter(YERTRIG>=2000) %>%
  rename(ccode1 = CRACID)
head(nc)

df <- nc %>%
  group_by(CRISNO) %>%
  slice(1) %>%
  ungroup()
head(df)

# joining the first observation per crisis with the rest to create dyadic dataframe
# direction (triggering entity) doesn't matter for this purpose
nc_dyad <- df %>%
  left_join(nc %>%
              group_by(CRISNO) %>%
              slice(-1) %>%
              ungroup() %>%
              rename(ccode2 = ccode1) %>%
              select(CRISNO, ccode2),
            by = c("CRISNO")) %>%
  relocate(ccode2, .after = 4) %>%
  mutate(startdate = make_date(YERTRIG, MONTRIG, DAYTRIG)) %>%
  relocate(startdate, .after = 5)
head(nc_dyad)

# need rivalry data to subset icb for df2 and df3
kdg <- read_csv("_data/_raw/kdg.csv")
head(kdg)

# need to fix date vars. lubridate won't parse some, so doing it this way instead:
# pulling year
kdg$startyear_kdg <- substr(kdg$beginr, 1, 4)
filter(kdg, is.na(startyear_kdg))

kdg$endyear_kdg <- substr(kdg$endr, 1, 4)
filter(kdg, is.na(endyear_kdg))

kdg <- kdg[kdg$endyear_kdg>=2000, ]
# issue here is that lot of rivalries ended in 2001, but can't tell what's right-censored and which ones actually ended

# creating directed dyadic data
kdg1 <- kdg %>% rename(ccode1 = rivala, ccode2 = rivalb)

kdg2 <- kdg %>% rename(ccode2 = rivala, ccode1 = rivalb)

df.kdg <- rbind(kdg1, kdg2)

# adding in new peace (rivarly) data version of kdg
peace <- read_csv("_data/_raw/peacedatav30dyadyrmean.csv", col_names = FALSE) %>%
  rename(dyadid = X1, year = X2, peace = X3) %>%
  filter(year>=2000)
# this still leaves the issue of parsing the dyad ids..
peace$ccode1 <- substr(peace$dyadid, 1, ifelse(nchar(peace$dyadid) == 6, 3, ifelse(nchar(peace$dyadid) == 5, 2, 1)))
peace$ccode2 <- ifelse(nchar(peace$dyadid) == 4, substr(peace$dyadid, 2, 4),
                    ifelse(nchar(peace$dyadid) == 5, substr(peace$dyadid, 3, 5),
                           ifelse(nchar(peace$dyadid) == 6, substr(peace$dyadid, 4, 6), NA)))
# removing the leading zero from ccode2
peace$ccode2 <- as.numeric(peace$ccode2)
peace$ccode1 <- as.numeric(peace$ccode1) # recasting

# Creating dyad-year df (with covars) -------------------------------------

# generating basic dyad-year dataset with peace sciencer, which will double as source of many covars and the basic structure for df2
# this adds in covars for MID hostility, military expenditure
dyadyears <- create_dyadyears(system = "cow", directed = TRUE, subset_years = c(1999:2016)) %>%
  add_cow_mids() %>%
  add_nmc() %>%
  select(ccode1, ccode2, year, cowmidongoing, cowmidonset, hostlev, milex1, milex2) # dropping most vars

# pulling in polyarchy covariates for ccode1
vdem.vars1 <- vdem %>% 
  dplyr::rename(., ccode1 = COWcode, v2x_polyarchy1 = v2x_polyarchy) %>%
  filter(., year>=1999) %>%
  select(., ccode1, year, v2x_polyarchy1) %>%
  arrange(., ccode1, year)
# and ccode2
vdem.vars2 <- vdem %>% 
  dplyr::rename(., ccode2 = COWcode, v2x_polyarchy2 = v2x_polyarchy) %>%
  filter(., year>=1999) %>%
  select(., ccode2, year, v2x_polyarchy2) %>%
  arrange(., ccode2, year)

# pulling in sipri milex data

milex_df <- read_xlsx("_data/_raw/SIPRI-Milex-data-1949-2022.xlsx")

milex_df1 <- milex_df %>%
  pivot_longer(
    cols = '1999':'2022',
    names_to = "year",
    values_to = "sipri_milex1") %>%
  mutate(ccode1 = countrycode(Country, "country.name", "cown")) %>%
  mutate(sipri_milex1 = as.numeric(sipri_milex1)) %>%
  mutate(year = as.numeric(year)) %>%
  select(-c('Country')) %>%
  group_by(ccode1, year) %>%
  summarise(sipri_milex1 = max(sipri_milex1)) %>% # to deal with multiple entries per year
  ungroup()

# rescaling to 0 to 1
milex_df1$sipri_milex1 <- (milex_df1$sipri_milex1 - min(milex_df1$sipri_milex1, na.rm = TRUE)) /
  (max(milex_df1$sipri_milex1, na.rm = TRUE) - min(milex_df1$sipri_milex1, na.rm = TRUE))

milex_df2 <- milex_df1 %>%
  rename(ccode2 = ccode1, sipri_milex2 = sipri_milex1)

# Prepping territorial dispute (COW) data) --------------------------------

# grabbing territorial dispute data from COW and fixing it
# unclear from documentation when sample ends (goes to 2018 according to website)
terr <- read_csv("_data/_raw/tc2018.csv")

# cleaning up
terr <- terr %>%
  rename(., ccode1 = gainer, ccode2 = loser) %>%
  mutate(date = make_date(year = year, month = month, day = "01")) %>%
  filter(., year>=1999) %>% # keeping 1999 in case we want to lag later
  select(date, year, ccode1, ccode2)

terr2 <- terr %>%
  rename(., ccode1 = ccode2, ccode2= ccode1)
terr <- rbind(terr, terr2) # now created directed dyad data, because we don't care about who gained and who lost

terr$terr_dispute <- 1

# M1: Merging dcid and icb ------------------------------------------------

# mapply function to check for icb within 180, etc. days after start (?) of DCID
# multiple versions for whether icb turned violent or not

df1 <- dcid %>%
  mutate(crisis = mapply(function(a, b, sd) {
    any(((a == icb_dyad$statea & b == icb_dyad$stateb) | (b == icb_dyad$statea & a == icb_dyad$stateb)) &
          as.Date(sd) <= as.Date(icb_dyad$startdate) &
          as.Date(sd + days(180)) >= as.Date(icb_dyad$startdate))
  }, ccode1, ccode2, interactionstartdate))
table(df1$crisis)
# not that many crises
filter(df1, crisis==TRUE)
# need to sort out direction on icb, and who initiated icb

# # let me check if this is working properly
# df1_test <- dcid %>%
#   rowwise() %>%
#   mutate(crisis = any(((ccode1 == icb_dyad_temp$statea & ccode2 == icb_dyad_temp$stateb) | (ccode2 == icb_dyad_temp$statea & ccode1 == icb_dyad_temp$stateb)) &
#                         interactionstartdate <= icb_dyad_temp$startdate &
#                         interactionstartdate + days(180) >= icb_dyad_temp$startdate)) %>%
#   ungroup()
# table(df1_test$crisis)
# # exact same result
# filter(df1_test, crisis==TRUE)

# looking to see if everything worked properly
filter(icb_dyad, statea==2 & stateb==365 & startdate>"2000-01-01")
ymd("2017-04-07") - ymd("2016-11-08")
as.Date("2016-11-08") + days(180)
# looks good

# pulling outcome vars from icb
icb_out1 <- icb %>%
  select(crisno, sevvio) %>%
  group_by(crisno) %>%
  summarise(max_sevvio = max(sevvio)) %>%
  ungroup()

icb_dyad_temp <- icb_dyad %>%
  left_join(., icb_out1, by = c('crisno'))

# violent crisis version
df1 <- df1 %>%
  mutate(crisis_sev = mapply(function(a, b, sd) {
    any(((a == icb_dyad_temp$statea & b == icb_dyad_temp$stateb) | (b == icb_dyad_temp$statea & a == icb_dyad_temp$stateb)) &
          as.Date(sd) <= as.Date(icb_dyad_temp$startdate) &
          as.Date(sd + days(180)) >= as.Date(icb_dyad_temp$startdate) &
          icb_dyad_temp$max_sevvio>=3)
  }, ccode1, ccode2, interactionstartdate))
table(df1$crisis_sev, df1$crisis)
filter(df1, crisis_sev==TRUE)
# only Russia, and I'm guessing they're the initiator too for the crises

# adding in a var for whether crisis happened within 180 days but in the following year
df1 <- df1 %>%
  mutate(nextyear = mapply(function(a, b, sd) {
    any(((a == icb_dyad$statea & b == icb_dyad$stateb) | (b == icb_dyad$statea & a == icb_dyad$stateb)) &
          as.Date(sd) <= as.Date(icb_dyad$startdate) &
          as.Date(sd + days(180)) >= as.Date(icb_dyad$startdate) &
          year(sd)< year(icb_dyad$startdate))
  }, ccode1, ccode2, interactionstartdate))
table(df1$crisis, df1$nextyear)
# four positive obs

# the same for more severe crises
df1 <- df1 %>%
  mutate(nextyear_sev = mapply(function(a, b, sd) {
    any(((a == icb_dyad_temp$statea & b == icb_dyad_temp$stateb) | (b == icb_dyad_temp$statea & a == icb_dyad_temp$stateb)) &
          as.Date(sd) <= as.Date(icb_dyad_temp$startdate) &
          as.Date(sd + days(180)) >= as.Date(icb_dyad_temp$startdate) &
          icb_dyad_temp$max_sevvio>=3 &
          year(sd)< year(icb_dyad_temp$startdate))
  }, ccode1, ccode2, interactionstartdate))
table(df1$crisis_sev, df1$nextyear_sev)
# no positive obs

# creating near crisis var
df <- df1 %>%
  mutate(nearcrisis = mapply(function(a, b, sd) {
    any(((a == nc_dyad$ccode1 & b == nc_dyad$ccode2) | (b == nc_dyad$ccode1 & a == nc_dyad$ccode2)) &
          as.Date(sd) <= as.Date(nc_dyad$startdate) &
          as.Date(sd + days(180)) >= as.Date(nc_dyad$startdate))
  }, ccode1, ccode2, interactionstartdate))
table(df$nearcrisis)
# none!! that needs to be validated

# changing from boolean to numerical
df1 %<>% mutate_if(is.logical, as.numeric)

# M1: Descriptive stats and crosstabs -------------------------------------

table(df1$severity, df1$crisis)

table(df1$severity, df1$crisis_sev)

# M1: Final join ----------------------------------------------------------

df1 <- df1 %>%
  mutate(year = year(interactionstartdate)) %>%
  left_join(., dyadyears, by = c('ccode1', 'ccode2', 'year')) %>%
  left_join(., vdem.vars1, by =c('ccode1', 'year')) %>%
  left_join(., vdem.vars2, by =c('ccode2', 'year')) %>%
  left_join(., terr, by =c('ccode1', 'ccode2', 'year')) %>%
  left_join(., milex_df1, by =c('ccode1', 'year')) %>%
  left_join(., milex_df2, by =c('ccode2', 'year')) %>%
  select(-c('date'))

# dealing with missing
df1$hostlev[is.na(df1$hostlev)] <- 0
df1$terr_dispute[is.na(df1$terr_dispute)] <- 0

# M2: Prepping DCID -------------------------------------------------------

# collapsing DCID to dyad-year
# note: make sure to handle time ranges that cross years

dcid_dyadyear <- df1 %>%
  mutate(year = year(interactionstartdate)) %>%
  group_by(ccode1, ccode2, year) %>%
  summarise(incidents = n_distinct(Cyberincidentnum), 
            cyber_objective = max(cyber_objective), 
            crisis = max(crisis), crisis_sev = max(crisis_sev),
            nextyear = max(nextyear)) %>%
  ungroup()
table(dcid_dyadyear$crisis)
table(dcid_dyadyear$incidents)

# M2: Prepping ICB --------------------------------------------------------

# collapsing ICB to dyad-year
# awful naming, but need to distinguish it
dyad_year.icb <- icb_dyad_year %>%
  mutate(crisis_onset = ifelse(trgyrdy==year, 1, 0)) %>%
  mutate(startdate = make_date(year = trgyrdy, month = trgmody, day = trgdady)) %>%
  mutate(enddate = make_date(year = trmyrdy, month = trmmody, day = trmdady)) %>%
  group_by(statea, stateb, year) %>%
  summarise(crisis_onset = max(crisis_onset), ongoing = max(ongoing), startdate_icb_min = min(startdate)) %>% # gonna leave out severity for now
  ungroup() %>%
  filter(., year>=1999) %>% # don't need observations before 1999 (keeping those for lagged DV)
  rename(ccode1 = statea, ccode2  = stateb)
# need second df for directed dyad
dyad_year.icb2 <- icb_dyad_year %>%
  mutate(crisis_onset = ifelse(trgyrdy==year, 1, 0)) %>%
  mutate(startdate = make_date(year = trgyrdy, month = trgmody, day = trgdady)) %>%
  mutate(enddate = make_date(year = trmyrdy, month = trmmody, day = trmdady)) %>%
  group_by(statea, stateb, year) %>%
  summarise(crisis_onset = max(crisis_onset), ongoing = max(ongoing), startdate_icb_min = min(startdate)) %>% # gonna leave out severity for now
  ungroup() %>%
  filter(., year>=1999) %>% # don't need observations before 1999 (keeping those for lagged DV)
  rename(ccode2 = statea, ccode1  = stateb)

dyad_year.icb <- rbind(dyad_year.icb, dyad_year.icb2)
table(dyad_year.icb$crisis_onset, dyad_year.icb$ongoing)


# M2: Prepping NC ---------------------------------------------------------

# let's make sure we don't need to aggregate up to dyad-year
nc_dyad %>% 
  count(ccode1, ccode2, year(startdate)) %>%
  summarise(dupes = any(n>1)) %>%
  pull(dupes)
# no dupes on dyad-year level

# now removing any extraneous vars
nc_dirdyad <- nc_dyad %>%
  select(ccode1, ccode2, startdate) %>%
  mutate(nearcrisis = 1, # will need to change from NA to 0 in main df after join
         year = year(startdate)) %>% 
  rename(startdate_nc = startdate)

# now creating directed dyad by creating second df before rbind
nc_dirdyad2 <- nc_dirdyad %>%
  mutate(
    temp = ccode1,
    ccode1 = ccode2,
    ccode2 = temp) %>%
  select(-temp)

nc_dirdyad <- rbind(nc_dirdyad, nc_dirdyad2)


# M2: Final join ----------------------------------

# Join DCID, and general dyad-year df

df2 <- dyadyears %>%
  filter(., year>=1999) %>%
  left_join(., dyad_year.icb, by = c('ccode1', 'ccode2', 'year')) %>%
  left_join(., nc_dirdyad, by = c('ccode1', 'ccode2', 'year')) %>%
  left_join(., dcid_dyadyear, by = c('ccode1', 'ccode2', 'year')) %>%
  left_join(., peace, by = c('ccode1', 'ccode2', 'year')) %>%
  left_join(., vdem.vars1, by =c('ccode1', 'year')) %>%
  left_join(., vdem.vars2, by =c('ccode2', 'year')) %>%
  left_join(., terr, by =c('ccode1', 'ccode2', 'year')) %>%
  left_join(., milex_df1, by =c('ccode1', 'year')) %>%
  left_join(., milex_df2, by =c('ccode2', 'year')) %>% # lot of missing values here now, lot for Syria
  select(-c('date')) %>%
  filter(peace<0.5|incidents>0) # wide net to include rivalry dyads
  
# adding in previous severe cyber separately
# creating covar for previous severe incident
df2 <- df2 %>%
  mutate(previous_sev = mapply(function(a, b, sd) {
    any((a == dcid$ccode1 | b == dcid$ccode1) &
          sd > lubridate::year(dcid$interactionstartdate) &
          dcid$severity>=5)
  }, ccode1, ccode2, year))
table(df2$previous_sev)
df2 %<>% mutate_if(is.logical, as.numeric)

# creating dummy to see if any icb started before cyber incidents
df2 <- df2 %>%
  mutate(reverse = mapply(function(a, b, sd, sy) {
    any(a == dcid$ccode1 & b == dcid$ccode2 &
          incidents>0 &
          sy == lubridate::year(dcid$interactionstartdate) &
          sd < as.Date(dcid$interactionstartdate))
  }, ccode1, ccode2, startdate_icb_min, year))
table(df2$reverse)

# creating dummy to see if any nc started before cyber incidents
df2 <- df2 %>%
  mutate(reverse_nc = mapply(function(a, b, sd, sy) {
    any(a == dcid$ccode1 & b == dcid$ccode2 &
          incidents>0 &
          sy == lubridate::year(dcid$interactionstartdate) &
          sd < as.Date(dcid$interactionstartdate))
  }, ccode1, ccode2, startdate_nc, year))
table(df2$reverse_nc)

df2 %<>% mutate_if(is.logical, as.numeric)


# dealing with missing
df2$hostlev[is.na(df2$hostlev)] <- 0
df2$terr_dispute[is.na(df2$terr_dispute)] <- 0
df2$incidents[is.na(df2$incidents)] <- 0
df2$crisis_onset[is.na(df2$crisis_onset)] <- 0
df2$nearcrisis[is.na(df2$nearcrisis)] <- 0
df2$ongoing[is.na(df2$ongoing)] <- 0
df2$nextyear[is.na(df2$nextyear)] <- 0
df2$reverse[is.na(df2$reverse)] <- 0
df2$reverse_nc[is.na(df2$reverse_nc)] <- 0


# creating alt dv because icb-based dv means some dyad-years might have cyber and and icb, but not fit the 180-day range
# this is not right and needs to be amended
df2 <- df2 %>%
  mutate(crisis_alt = ifelse(crisis_onset==1 & incidents==0, 0, crisis_onset)) %>%
  mutate(cyber = ifelse(incidents>0, 1, 0))

# create dyad ID for cluster-robust SEs
df2 <- df2 %>%
  mutate(dyadid = as.numeric(factor(paste(pmin(ccode1, ccode2),
                                          pmax(ccode1, ccode2), sep="-"))))

# M2: Descriptive statistics and crosstabs --------------------------------

table(df2$crisis_onset)
table(df2$cyber)
table(df2$crisis_alt, df2$crisis_onset)
table(df2$cyber, df2$crisis_onset)
# this does not seem to suggest that cyber is positively associated with crises

# let's look at near crisis
table(df2$cyber, df2$nearcrisis)
filter(df2, cyber==1 & nearcrisis==1 & reverse_nc==1)
df2 %>% filter(reverse_nc==0) %>% with(table(cyber, nearcrisis))
# so only positive observations are near crises happening in the same year as cyber incident, but before

# M3: Merging dcid and icb ----------------------------------------------------

### DO WE NEED TO EXCLUDE CRISES THAT STARTED BEFORE 2000?

# filter and add rivalry data
df3 <- filter(icb_dyad, !enddate<'2000-1-1') %>%
  filter(., !startdate>'2016-12-31') %>%
  left_join(., df.kdg, by = c("statea" = "ccode1", "stateb" = "ccode2"))

# joining in the newer peace data. since the latter is dyad-year, we'll join on TRGYRDY of icb
df3 <- df3 %>%
  left_join(., peace, by = c('statea' = 'ccode1', 'stateb' = 'ccode2', 'trgyrdy' = 'year')) %>%
  select(-c('dyadid'))

# M3: Creating cyber variables ------------------------------------------------

# starting with whether there is at least one cyber incident between the two countries in period

# this seems to work: for each row in df3, see if there is a matching obs in dcid
df3 <- df3 %>%
  mutate(cyber = mapply(function(a, b, sd, ed) {
    any(((a == dcid$ccode1 & b == dcid$ccode2) | (a == dcid$ccode2 & b == dcid$ccode1)) & 
          as.Date(sd) <= as.Date(dcid$interactionenddate) & 
          as.Date(ed) >= as.Date(dcid$interactionstartdate))
  }, statea, stateb, startdate, enddate))
# cyber: dichotomous variable whether there is at least one cyber incident from dcid in icb

# creating string variable with all ids for cyber incidents
# collapses unique_ids to string with semicolon separator and replaces missing
df3 <- df3 %>%
  mutate(matches = mapply(function(a, b, sd, ed) {
    sum(a == dcid$ccode1 & b == dcid$ccode2 & # sum just summarizes the number of rows fitting these conditions
          as.Date(sd) <= as.Date(dcid$interactionenddate) & 
          as.Date(ed) >= as.Date(dcid$interactionstartdate))
  }, statea, stateb, startdate, enddate),
  cyber_id = mapply(function(a, b, sd, ed) { # mapply is meant to create a vector with equal length to df
    unique_vals <- unique(dcid$Cyberincidentnum[a == dcid$ccode1 & b == dcid$ccode2 & # cool how it subsets on conditions within unique function
                                                  as.Date(sd) <= as.Date(dcid$interactionenddate) & 
                                                  as.Date(ed) >= as.Date(dcid$interactionstartdate)])
    if(length(unique_vals) > 0){
      paste(unique_vals, collapse = ";") # paste collapses multiple elements ("unique_vals") into a single element, here being a string
    } else {
      NA_character_
    }
  }, statea, stateb, startdate, enddate))
# cyber_id: string list of cyber incident IDs from DCID

# next step is to write another function that pulls out relevant vars from dcid based on values in 'cyber_id'
# Split the "cyber_id" string variable in df1 into list of separate IDs
df1_ids <- strsplit(df3$cyber_id, ";")

# Create an empty vector to store the results
sev5 <- logical(nrow(df3))

# Loop through each row in df3
for (i in seq_along(df1_ids)) {
  # Find the corresponding rows in dcid for the current IDs in df3
  current_ids <- df1_ids[[i]]
  matching_rows <- dcid$Cyberincidentnum %in% current_ids
  
  # Check if any of the matching rows have severity >= 5
  if (any(dcid$severity[matching_rows] >= 5)) {
    sev5[i] <- TRUE
  }
}
# sev5: binary var for whether there are any cyber incidents of severity>=5 in icb

# Add the new variable to df1
df3$sev5 <- sev5

# seems to be working right
filter(df3, matches>0 & sev5== "FALSE")

df3$max_severity <- sapply(df1_ids, function(ids) {
  max_severity_val <- max(dcid$severity[dcid$Cyberincidentnum %in% ids])
  if(is.infinite(max_severity_val)) {
    max_severity_val <- NA_real_
  }
  return(max_severity_val)
})
# max_severity_val: highest severity value of cyber incidents in each icb


# M3: Subsetting ICB dyadic ---------------------------------------------------

# from peace codebook: 
# The peace scale has values 0.0 -- serious rivalry, .25 -- lesser rivalry, .50 -- negative peace, .75 -- warm peace, and 1.0 -- security community

# crises with no cyber and no rivalry coded in kdg
candidates <- filter(df3, cyber=="FALSE" & is.na(rivalry)) %>%
  select(crisno, crisname, statea, namea, stateb, nameb, startdate, enddate, endyear_kdg, rivnumb, peace, cyber)
# these are the ones that need to be examined.
# should probably keep those at peace=<0.25, drop is.na(peace) | peace>0.25

candidates2 <- filter(df3, endyear_kdg==2000 & trgyrdy>2000 & cyber=="FALSE") %>%
  select(crisno, crisname, statea, namea, stateb, nameb, startdate, enddate, endyear_kdg, rivnumb, peace, cyber)

df.cand <- rbind(candidates, candidates2)

# now need to cut dyads that were checked for DCID
cand_cut <- data.frame(ccode1 = c("630", "630", "2", "2", "640", "652"),
                       ccode2 = c("666", "640", "630", "731", "652", "666"))

df.cand <- df.cand %>%
  mutate(cut = mapply(function(a, b) {
    any(a == cand_cut$ccode1 & b == cand_cut$ccode2)
  }, statea, stateb)) %>%
  filter(., cut=="FALSE") %>%
  select(-c("cut"))

# exporting this to check with co-authors
write.csv(df.cand, "_data/_processed/icb_candidates_dropped.csv", row.names=FALSE)

# crises with no cyber but included in kdg as of 1/1/2000
candidates3 <- filter(df3, cyber=="FALSE" & !is.na(rivalry))
# rivalry might be outdated, but should probably be included
# only one observation of peace>0.25 that maybe should be dropped

# M3: Creating DV from icb ----------------------------------------------------

# need to bring in outcome var from main icb
# creating a separate df for the icb outcomes
icb_out2 <- icb %>%
  select(crisno, cracno, cracid, actor, crisname, yrtrig, outesr, yrterm) %>%
  filter(., !yrterm<2000) %>% 
  group_by(crisno) %>%
  mutate(n_dist_out = n_distinct(outesr)) %>%
  ungroup()
# for some inexplicable reason, congo war has different outcomes per actor..
# need to gen a var that indicates whether a crisis has different outcomes depending on case/actor

# time to join

df3 <- left_join(df3, 
                  icb_out2 %>% select(crisno, outesr, cracid), by = c("crisno" = "crisno", "statea" = "cracid")) %>%
  rename(disp_out1 = outesr) %>%
  left_join(., 
            icb_out2 %>% select(crisno, outesr, cracid), 
            by = c("crisno" = "crisno", "statea" = "cracid")) %>%
  rename(disp_out2 = outesr) # doing two outcomes because there is at least one crisis with different outcomes per actor

# M3: Final join ----------------------------------------------------------

df3 <- df3 %>%
  rename(year = trgyrdy, ccode1 = statea, ccode2 = stateb) %>%
  left_join(., vdem.vars1, by =c('ccode1', 'year')) %>%
  left_join(., vdem.vars2, by =c('ccode2', 'year')) %>%
  left_join(., terr, by =c('ccode1', 'ccode2', 'year')) %>%
  left_join(., milex_df1, by =c('ccode1', 'year')) %>%
  left_join(., milex_df2, by =c('ccode2', 'year')) %>% # lot of missing values here now, lot for Syria
  left_join(., dyadyears %>% 
              dplyr::select(ccode1, ccode2, year, hostlev), 
            by =c('ccode1', 'ccode2', 'year')) %>%
  select(-c('date')) %>%
  filter(peace<0.5|cyber==TRUE) # wide net to include rivalry dyads
# some missing values on outcome

# dealing with missing, double check these and df!
df3$hostlev[is.na(df3$hostlev)] <- 0
df3$terr_dispute[is.na(df3$terr_dispute)] <- 0

# adding in previous severe cyber separately
# creating covar for previous severe incident
df3 <- df3 %>%
  mutate(previous_sev = mapply(function(a, b, sd) {
    any((a == dcid$ccode1 | b == dcid$ccode1) &
          as.Date(sd) > as.Date(dcid$interactionstartdate) &
          dcid$severity>=5)
  }, ccode1, ccode2, startdate))
table(df3$previous_sev)
df3 %<>% mutate_if(is.logical, as.numeric)

# dropping recent cases to make it binary outcome
df3 <- filter(df3, disp_out1<3)
#flipping order of outcome, so 1 is escalation, and 0 is reduction
df3$disp_out1[df3$disp_out1==2] <- 0

df3$disp_out1 <- as.integer(df3$disp_out1)

table(df3$disp_out1)
