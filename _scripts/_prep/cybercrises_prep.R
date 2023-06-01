# data prep script for project on interaction between cyber incidents and international crises
# df: icb -> cyber incidents -> icb hostility

# potential issues: icb includes non-rivalrous dyads, whereas dcid only covers rivals

# TO-DO: define rivalry population to whittle down ICB dyads; pull in outcome var(s) from ICB

library(tidyverse)
library(foreign)
library(ggplot2)
library(countrycode)
library(readxl)
library(data.table)

# Pulling data ------------------------------------------------------------

dcid <- read_excel("_data/_raw/DCID_2.0_Release_update_February_2023.xlsx")
# need to add in cow codes

dcid <- dcid %>% mutate(ccode1 = countrycode(StateA, "country.name", "cown")) %>%
  mutate(ccode2 = countrycode(StateB, "country.name", "cown"))

icb_dyad <- read_csv("_data/_raw/icbdy_v15.csv")
# should probably trim this to years >=2000 since it's dyad-years

# need main icb for outcome variables, including "outesr" (easing or increasing tension post-crisis)
icb <- read_csv("_data/_raw/icb2v15.csv")

# need rivalry data to subset icb

kdg <- read_csv("_data/_raw/kdg.csv")
head(kdg)

# need to fix date vars. lubridate won't parse some, so doing it this way instead:
# pulling year
kdg$startyear_kdg <- substr(kdg$beginr, 1, 4)
filter(kdg, is.na(startyear_kdg))

kdg$endyear_kdg <- substr(kdg$endr, 1, 4)
filter(kdg, is.na(endyear_kdg))

kdg <- kdg[kdg$endyear_kdg>=2000, ]
# issue here is that lot of rivalries ended in 2000 or 2001, but can't tell what's right-censored and which ones actually ended

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

# Merging dcid and icb ----------------------------------------------------

# need to define and align date formats across both dataframes
head(icb_dyad)

# note: using dyadic-calculated start and end dates here. see icb documentation for details
icb_dyad <- icb_dyad %>% mutate(startdate = make_date(year = trgyrdy, month = trgmody, day = trgdady)) %>%
  mutate(enddate = make_date(year = trmyrdy, month = trmmody, day = trmdady)) %>%
  select(-c("year", "ongoing")) %>%
  distinct()
# don't need yearly data on the dyads, do collapsing to dyad-level

### DO WE NEED TO EXCLUDE CRISES THAT STARTED BEFORE 2000?

# filter and add rivalry data
icb_dyad <- filter(icb_dyad, !enddate<'2000-1-1') %>%
  left_join(., df.kdg, by = c("statea" = "ccode1", "stateb" = "ccode2"))

# joining in the newer peace data. since the latter is dyad-year, we'll join on TRGYRDY of icb
icb_dyad <- icb_dyad %>%
  left_join(., peace, by = c('statea' = 'ccode1', 'stateb' = 'ccode2', 'trgyrdy' = 'year')) %>%
  select(-c('dyadid'))

# Creating cyber variables ------------------------------------------------

# starting with whether there is at least one cyber incident between the two countries in period

# this seems to work: for each row in icb_dyad, see if there is a matching obs in dcid
icb_dyad <- icb_dyad %>%
  mutate(cyber = mapply(function(a, b, sd, ed) {
    any(a == dcid$ccode1 & b == dcid$ccode2 & 
          as.Date(sd) <= as.Date(dcid$interactionenddate) & 
          as.Date(ed) >= as.Date(dcid$interactionstartdate))
  }, statea, stateb, startdate, enddate))
# cyber: dichotomous variable whether there is at least one cyber incident from dcid in icb

# creating string variable with all ids for cyber incidents
# collapses unique_ids to string with semicolon separator and replaces missing
icb_dyad <- icb_dyad %>%
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
df1_ids <- strsplit(icb_dyad$cyber_id, ";")

# Create an empty vector to store the results
sev5 <- logical(nrow(icb_dyad))

# Loop through each row in icb_dyad
for (i in seq_along(df1_ids)) {
  # Find the corresponding rows in dcid for the current IDs in icb_dyad
  current_ids <- df1_ids[[i]]
  matching_rows <- dcid$Cyberincidentnum %in% current_ids
  
  # Check if any of the matching rows have severity >= 5
  if (any(dcid$severity[matching_rows] >= 5)) {
    sev5[i] <- TRUE
  }
}
# sev5: binary var for whether there are any cyber incidents of severity>=5 in icb

# Add the new variable to df1
icb_dyad$sev5 <- sev5

# seems to be working right
filter(icb_dyad, matches>0 & sev5== "FALSE")

icb_dyad$max_severity <- sapply(df1_ids, function(ids) {
  max_severity_val <- max(dcid$severity[dcid$Cyberincidentnum %in% ids])
  if(is.infinite(max_severity_val)) {
    max_severity_val <- NA_real_
  }
  return(max_severity_val)
})
# max_severity_val: highest severity value of cyber incidents in each icb


# Subsetting ICB dyadic ---------------------------------------------------

# from peace codebook: 
# The peace scale has values 0.0 -- serious rivalry, .25 -- lesser rivalry, .50 -- negative peace, .75 -- warm peace, and 1.0 -- security community

# crises with no cyber and no rivalry coded in kdg
candidates <- filter(icb_dyad, cyber=="FALSE" & is.na(rivalry)) %>%
  select(crisno, crisname, statea, namea, stateb, nameb, startdate, enddate, endyear_kdg, rivnumb, peace, cyber)
# these are the ones that need to be examined.
# should probably keep those at peace=<0.25, drop is.na(peace) | peace>0.25

candidates2 <- filter(icb_dyad, endyear_kdg==2000 & trgyrdy>2000 & cyber=="FALSE") %>%
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
candidates3 <- filter(icb_dyad, cyber=="FALSE" & !is.na(rivalry))
# rivalry might be outdated, but should probably be included
# only one observation of peace>0.25 that maybe should be dropped

# Creating DV from icb ----------------------------------------------------

# need to bring in outcome var from main icb
#table(icb_dyad_test$outesr, icb_dyad_test$cyber)
# this is not straightforward
head(icb)
icb <- icb %>%
  select(crisno, cracno, cracid, actor, crisname, yrtrig, outesr, yrterm) %>%
  filter(., !yrterm<2000) %>% 
  group_by(crisno) %>%
  mutate(n_dist_out = n_distinct(outesr)) %>%
  ungroup()
# for some inexplicable reason, congo war has different outcomes per actor..
# need to gen a var that indicates whether a crisis has different outcomes depending on case/actor

# time to join

icb_dyad <- left_join(icb_dyad, 
                  icb %>% select(crisno, outesr, cracid), by = c("crisno" = "crisno", "statea" = "cracid")) %>%
  rename(disp_out1 = outesr) %>%
  left_join(., 
            icb %>% select(crisno, outesr, cracid), 
            by = c("crisno" = "crisno", "statea" = "cracid")) %>%
  rename(disp_out2 = outesr)

# let's check on some cross tabs
table(icb_dyad$cyber, icb_dyad$disp_out1)
table(icb_dyad$cyber, icb_dyad$disp_out2)