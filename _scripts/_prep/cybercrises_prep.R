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
kdg$startyear <- substr(kdg$beginr, 1, 4)
filter(kdg, is.na(startyear))

kdg$endyear <- substr(kdg$endr, 1, 4)
filter(kdg, is.na(endyear))

kdg <- kdg[kdg$endyear>=2000, ]
# issue here is that lot of rivalries ended in 2000 or 2001, but can't tell what's right-censored and which ones actually ended

# creating directed dyadic data
kdg1 <- kdg %>% rename(ccode1 = rivala, ccode2 = rivalb)

kdg2 <- kdg %>% rename(ccode2 = rivala, ccode1 = rivalb)

df.kdg <- rbind(kdg1, kdg2)

# Merging dcid and icb ----------------------------------------------------


# need to define and align date formats across both dataframes
head(icb_dyad)

# note: using dyadic-calculated start and end dates here. see icb documentation for details
icb_dyad <- icb_dyad %>% mutate(startdate = make_date(year = trgyrdy, month = trgmody, day = trgdady)) %>%
  mutate(enddate = make_date(year = trmyrdy, month = trmmody, day = trmdady))

### DO WE NEED TO EXCLUDE CRISES THAT STARTED BEFORE 2000?

# filter and add rivalry data
icb_dyad <- filter(icb_dyad, !enddate<'2001-1-1') %>%
  left_join(., df.kdg, by = c("statea" = "ccode1", "stateb" = "ccode2"))

# Creating cyber variables ------------------------------------------------

# starting with whether there is at least one cyber incident between the two countries in period

# this seems to work: for each row in icb_dyad, see if there is a matching obs in dcid
icb_dyad <- icb_dyad %>%
  mutate(cyber = mapply(function(a, b, sd, ed) {
    any(a == dcid$ccode1 & b == dcid$ccode2 & 
          as.Date(sd) <= as.Date(dcid$interactionenddate) & 
          as.Date(ed) >= as.Date(dcid$interactionstartdate))
  }, statea, stateb, startdate, enddate))

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

# next step is to write another function that pulls out relevant vars from dcid based on values in 'cyber_id'
# Split the "cyber_id" string variable in df1 into separate IDs
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


# Subsetting ICB dyadic ---------------------------------------------------

# now need to remove ICB dyads that are outside of the scope
filter(icb_dyad, cyber=="TRUE" & is.na(rivalry))
# plenty of crises with cyber incidents not captured in KDG
# NEED TO BE INCLUDED

candidates <- filter(icb_dyad, cyber=="FALSE" & is.na(rivalry))
# these are the ones that need to be examined.

candidates2 <- filter(icb_dyad, cyber=="FALSE" & !is.na(rivalry))
# lot of crises with  no cyber and in rivalry
# rivalry might be outdated


# Creating DV from icb ----------------------------------------------------

# need to bring in outcome var from main icb
#table(icb_dyad_test$outesr, icb_dyad_test$cyber)
# this is not straightforward
head(icb)
test_outcome <- icb %>%
  select(crisno, cracno, cracid, actor, crisname, yrtrig, outesr, yrterm) %>%
  filter(., !yrterm<=2001) %>% 
  group_by(crisno) %>%
  mutate(n_dist_out = n_distinct(outesr)) %>%
  ungroup()
# for some inexplicable reason, congo war has different outcomes per actor..
# need to gen a var that indicates whether a crisis has different outcomes depending on case/actor

