get_new <- TRUE
library(devtools)
library(tidyverse)
if(!require('cism')){
  install_github('joebrew/cism')
}
library(cism)


# If we've already downloaded data today, don't redownload
# Check by saying if there is a dated backup for today
if(!get_new){
  today <- max(as.Date(gsub('data_|.RData', '', dir('backups/'))))
  message('Using data from ', today)
} else {
  today <- Sys.Date()
}

file_name <- paste0('data_', today, '.RData')

if(file_name %in% dir('backups')){
  load(paste0('backups/', file_name))
} else {
  # Get mopeia data
  # Make sure you have a folder at the parent level named "credentials"
  # Within that folder, have a file named "credentials.yaml" with the below 
  # parameters filled out
  # dbname: mopeia
  # host: sap.manhica.net
  # port: 4706 # or 3306 if at cism
  # user: xxxx
  # pass: xxxx
  co <- credentials_connect(credentials_extract())
  
  # Loop through our necessary tables and download them
  tbls <- c('acd', 
            'acd_childs', 
            'acd_nets', 
            'censo_2017_childs', 
            'censo_2017_core', 
            'cross', 
            'cross_nets', 
            'follow_up',
            'follow_up_childs', 
            'follow_up_core_V062017', 
            'follow_up_nets_V062017',
            'pcd', 
            'pcd_v62017')
  for (i in 1:length(tbls)){
    message(tbls[i])
    # Get the table into memory
    this_table <- get_data(tab = tbls[i],
                           connection_object = co)
    names(this_table) <- tolower(names(this_table))
    # Assign it to global environment
    assign(tbls[i],
           this_table,
           envir = .GlobalEnv)
  }
  save.image(file = paste0('backups/', file_name))
}

# Read the table with the visit date, etc.
# (to be sent by email from Eldo monthly)
visit_dates <- read_csv('supplementary_data/Controle_de_visitas.csv')
# Clean up column names
names(visit_dates) <-
  gsub('part_info/|getpart_|part_|get_', '', names(visit_dates))

# Organize the visit dates meaningfully
visit_dates <-
  visit_dates %>%
  # rename(date = vdate) %>%
  dplyr::select(
    family_id,
    permid,
    getdob,
    cluster,
    dplyr::contains('round')) %>%
  tidyr::gather(key, value, round1:round7_rdt) 


# Get the visit number
matches <- regmatches(visit_dates$key, gregexpr("[[:digit:]]+", visit_dates$key))
matches <- as.numeric(unlist(matches))
visit_dates$visit_number <- matches

# Remove the number from the key
visit_dates$key <- gsub('[[:digit:]]+', '', visit_dates$key)

# Clean up the key
visit_dates <-
  visit_dates %>%
  mutate(key = ifelse(key == 'round_rdt', 'rdt',
                      ifelse(key == 'round_date', 'date',
                             ifelse(key == 'round', 'x', NA))))

# Spread out
visit_dates <- visit_dates %>%
  spread(key = key,
         value = value)

# Clean up the dataframe a little bit
visit_dates <-
  visit_dates %>%
  mutate(dob = as.Date(getdob, format = '%m/%d/%Y')) %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  dplyr::select(-getdob)

# For now, I'm going to make some assumptions about what the columns mean
visit_dates$rdt[visit_dates$rdt == 'n/a'] <- NA
visit_dates$x[visit_dates$x == 'n/a'] <- NA
visit_dates$x <- as.numeric(visit_dates$x)
visit_dates$rdt <- as.numeric(visit_dates$rdt)

visit_dates$rdt <- ifelse(visit_dates$rdt == 1, 'Positive',
                          ifelse(visit_dates$rdt == 2, 'Negative', NA))

visit_dates$malaria <- ifelse(visit_dates$rdt == 'Positive', TRUE,
                              ifelse(visit_dates$rdt == 'Negative', FALSE, NA))
visit_dates$round <- visit_dates$visit_number

# Read in geogrpahic and demographic data
# (need to get this in relative path)
master_table <- readr::read_csv("~/Documents/zambezia/master_table_for_carlos.csv")

# Get spray status
ss <- master_table %>% 
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(village_number) %>%
  dplyr::summarise(spray_status = dplyr::first(status)) 
ss <- ss %>%
  dplyr::mutate(spray_status = ifelse(spray_status, 'Spray', 'No spray'))

# Process data #########################################

# Create a follow up table
follow_up <- follow_up %>% filter(!duplicated(`_uri`))
follow_up <- follow_up_childs %>%
  left_join(follow_up,
            by = c('_parent_auri'='_uri'))

# Create a date
follow_up$date <- as.Date(substr(follow_up$start, 1, 10))

# Create an acd table
acd <- left_join(acd_childs,
                 acd,
                 by = c('_parent_auri'='_uri'))

# Create a pcd table

# Get a binary malaria variable
acd$malaria <- ifelse(acd$diagnostic == '1', TRUE,
                      ifelse(acd$diagnostic == '2', FALSE, 
                             NA))

# Get a binary had malaria variable
acd$had_malaria <- ifelse(acd$had_malaria == '1', TRUE,
                                 ifelse(acd$had_malaria == '2', FALSE,
                                        NA))
follow_up$had_malaria <- ifelse(follow_up$had_malaria == '1', TRUE,
                                       ifelse(follow_up$had_malaria == '2', FALSE,
                                              NA))

# Create a date variable in acd
acd$date <- as.Date(substr(acd$start, 1, 10))

# Bring spray status into visit_dates
visit_dates <-
  visit_dates %>%
  mutate(village_number = as.numeric(unlist(lapply(strsplit(permid, '-'), function(x){x[2]})))) %>%
  left_join(ss,
            by = 'village_number')

# Fix likely broken dates
visit_dates <- 
  visit_dates %>%
  filter(!is.na(date)) %>%
  mutate(date = ifelse(date < '2016-06-01', date + lubridate::years(1), date)) %>% 
  mutate(date = as.Date(date, origin = '1970-01-01'))

# Expand each child to total time observed
expand_child <- function(visit_dates,
                         perm_id = '004-7060-004-0001'){
  # Isolate just this child
  this_child <- visit_dates %>%
    filter(permid == perm_id)
  # Create an expansion of dates
  expanded_dates <- data_frame(date = seq(min(this_child$date),
                                          max(this_child$date),
                                          by = 1))
  # Join with observed data
  joined <- left_join(x = expanded_dates,
                      y = this_child %>% mutate(visit = TRUE),
                      by = 'date') %>%
    dplyr::select(date, permid, cluster, round, rdt, malaria, visit) %>%
    mutate(visit = ifelse(is.na(visit), FALSE, visit))
  # Forward fill everything
  joined <- joined %>%
    fill(permid:malaria, .direction = 'down')
  return(joined)
}
expanded_name <- paste0('expanded_', Sys.Date(), '.RData')
if(expanded_name %in% dir('backups')){
  load(paste0('backups/', expanded_name))
} else {
  results <- list()
  for(i in 1:nrow(visit_dates)){
    message(i)
    results[[i]] <- 
      expand_child(visit_dates = visit_dates,
                   perm_id = visit_dates$permid[i])
  }
  etar <- bind_rows(results)
  etar <- etar %>%
    mutate(village_number = as.numeric(unlist(lapply(strsplit(permid, '-'), function(x){x[2]})))) %>%
    left_join(ss,
              by = 'village_number')
  # Define a new case
  etar$new_case <- etar$malaria & etar$visit
  
  save(etar,
       file = paste0('backups/', expanded_name))
}

visit_dates$visit_done <- ifelse(visit_dates$x == 1,
                                 TRUE,
                                 ifelse(visit_dates$x == 2,
                                        FALSE, NA))
visit_dates$x <- NULL

# ACD ########################
# Round Incidence (seven of them)
# (# of RDT+ that round* / # of children in cohort that round)
#   * Round: equals visits of ACD. We are currently at the end of the 8th monthly follow up

# x <- visit_dates %>%
#   group_by(round) %>%
#   summarise(children = n(),
#             )



#   Cumulative incidence
#   (Total # of RDT+ in the cohort since start / # of child-years at risk**)
#     ** number of children in the cohort x time they have been followed. If a child was LFU then he
#     contributed until that moment to child-time-at-risk


#     Cumulative incidence by spray status
#     As above but disaggregated by those in “spray” and “no-spray” areas.
#     Please add RR and 95%CI for the RR
#     Corrected incidence 1
#     Cumulative incidence BUT every time a child is positive we should remove 30 days of time-at-risk from
#     the denominator
#     Corrected incidence 2
#     Cumulative incidence BUT every time a child is RDT+ we should remove 30 days of time-at-risk from the
#     denominator and disregard the results of the immediate next test (as if it had not been done)
#     Cumulative incidence by spray status AND Net usage
#     As above but also disaggregated by sleep last night (question 9 of follow questionnaire)
#     
#     PCD
#     Monthly Incidence in children under 5 (nine of them)
#     (# of <5s RDT+ that month / # of children under 5 that live in areas served by that HF (according to
#       census) * I acknowledge this is not perfect as they might go to other HF as well
#     Monthly Incidence in children under 5 BY spray status
#     As above but disaggregated by those residing in “spray” and “no-spray” areas
#     Please add RR and 95%CI for the RR
#     RDT positivity rate by HF by month in children <5
#     (Pooled # of RDT+ in children <5 that month/ all RDTs performed in children <5 that month)
#       Mopeia map of RDT positivity rate by age <5 and >5

