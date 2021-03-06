get_new <- FALSE
library(devtools)
library(tidyverse)
if(!require('cism')){
  install_github('joebrew/cism')
}
library(cism)

#Clean perm id
clean_perm_id <- function(x){
  xl <- strsplit(x, '-')
  xo <- lapply(xl, function(x){
    as.numeric(x)
  })
  xz <- lapply(xo, function(x){
    paste0(unlist(x), collapse = '-')
  })
  return(unlist(xz))
}


# If we've already downloaded data today, don't redownload
# Check by saying if there is a dated backup for today
if(!get_new){
  today <- max(as.Date(gsub('data_|.RData', '', dir('backups/'))), na.rm = TRUE)
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
visit_dates <- read_csv('supplementary_data/controle_de_visitas_round11.csv')
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
  tidyr::gather(key, value, round1:round11_rdt)  # this always needs to be updated


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
  group_by(family_id, permid, getdob, cluster, key, visit_number) %>%
  summarise(value = dplyr::first(value)) %>%
  ungroup %>% 
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


# According to Eldo
# Census uses "old_permid"
# "Controle_de_visitas" uses "new_permid"
# CROSS Data Set uses - "new_permid"
# ACD Data Set uses - "new_permid" (We can still have some "old_permids" from the previous months, but we are assessing it through the data cleaning) 
# Attached i'm sending a csv containing all the clusters and villages from the ACD. anything that differs from this, can be an input error that will be assessed in the cleaning process.
# Read that final csv
cvss <- read_csv('supplementary_data/COST_Clusters.Villages.Spray_Status.csv')
# Replace the perm_id in "visit_dates" with the correct one
# Per Eldo, the "old_permid" should match with the permid in the census for each person.
perm_id_corrections <- read_csv('supplementary_data/COST_Permids.New&Old_EE.csv') %>%
  filter(!duplicated(new_permid))

# Get the correct permid in in visit_dates
visit_dates <-
  left_join(visit_dates %>%
              mutate(id = clean_perm_id(permid)) %>%
                       dplyr::select(-family_id,
                                     -cluster),
            y = perm_id_corrections %>%
              mutate(id = clean_perm_id(new_permid))) %>%
  mutate(id = clean_perm_id(old_permid))

master_table <- readr::read_csv("~/Documents/zambezia/master_table_for_carlos.csv") %>%
  mutate(id = clean_perm_id(permid))

# Now using the "id" column to link visit_dates to master_table

# Get spray status
ss <- master_table %>% 
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(village_number) %>%
  dplyr::summarise(spray_status = dplyr::first(!is.na(status))) 
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
# visit_dates <-
#   visit_dates %>%
#   mutate(village_number = as.numeric(unlist(lapply(strsplit(permid, '-'), function(x){x[2]})))) %>%
#   left_join(ss,
#             by = 'village_number')

cvss$Clusters <- as.numeric(cvss$Clusters)

visit_dates <- visit_dates %>%
  left_join(cvss %>%
              dplyr::rename(cluster = Clusters) %>%
              dplyr::filter(!duplicated(cluster))) %>%
  mutate(spray_status = Spray_status)

# Fix likely broken dates
visit_dates <- 
  visit_dates %>%
  filter(!is.na(date)) %>%
  mutate(date = ifelse(date < '2016-06-01', date + lubridate::years(1), date)) %>% 
  mutate(date = as.Date(date, origin = '1970-01-01'))

# Remove all visit dates prior to 2017, since they appear incorrect
visit_dates <- visit_dates %>%
  filter(date >= '2017-01-01')


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
  all_pids <- sort(unique(visit_dates$permid))
  for(i in 1:length(all_pids)){
    this_perm_id <- all_pids[i]
    message(i)
    expanded <- expand_child(visit_dates = visit_dates,
                             perm_id = this_perm_id)
    message(': ', length(which(expanded$visit)))
    results[[i]] <- expanded
      
    
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

prettify <-
  function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE, 
            cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y", 
            round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE, 
            data_table = TRUE, nrows = 8, download_options = TRUE) 
  {
    column_names <- names(the_table)
    the_table <- data.frame(the_table)
    names(the_table) <- column_names
    classes <- lapply(the_table, function(x) {
      unlist(class(x))[1]
    })
    if (cap_columns) {
      names(the_table) <- Hmisc::capitalize(names(the_table))
    }
    if (remove_underscores_columns) {
      names(the_table) <- gsub("_", " ", names(the_table))
    }
    for (j in 1:ncol(the_table)) {
      the_column <- the_table[, j]
      the_class <- classes[j][1]
      if (the_class %in% c("character", "factor")) {
        if (cap_characters) {
          the_column <- as.character(the_column)
          the_column <- Hmisc::capitalize(the_column)
        }
        if (remove_line_breaks) {
          the_column <- gsub("\\n", " ", the_column)
        }
      }
      else if (the_class %in% c("POSIXct", "Date")) {
        the_column <- format(the_column, format = date_format)
      }
      else if (the_class %in% c("numeric", "integer")) {
        the_column <- round(the_column, digits = round_digits)
        if (comma_numbers) {
          the_column <- scales::comma(the_column)
        }
      }
      the_table[, j] <- the_column
    }
    if (remove_row_names) {
      row.names(the_table) <- NULL
    }
    if (data_table) {
      if (download_options) {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows, 
                                                             dom = "Bfrtip", buttons = list("copy", "print", 
                                                                                            list(extend = "collection", buttons = "csv", 
                                                                                                 text = "Download"))), rownames = FALSE, extensions = "Buttons")
      }
      else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows, 
                                                             dom = 't',
                                                             columnDefs = list(list(className = "dt-right", 
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      }
    }
    return(the_table)
  }

date_truncate <- function (date_object, level = c("month", "quarter", "year")) {
    if (is.null(level)) {
      stop('Must provide month quarter year')
    }
    date_object <- as.Date(date_object)
    if (sum(!is.na(date_object)) == 0) {
      return(date_object)
    }
    if (level == "month") {
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y-%m"), "-01"))
      return(return_object)
    }
    if (level == "quarter") {
      q_month <- (((((as.numeric(format(date_object, "%m"))) - 
                       1)%/%3) + 1) * 3) - 2
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y"), ifelse(nchar(q_month[!is.na(return_object)]) == 
                                                                                    2, "-", "-0"), q_month, "-01"))
      return(return_object)
    }
    if (level == "year") {
      return_object <- date_object
      return_object[!is.na(return_object)] <- as.Date(paste0(format(return_object[!is.na(return_object)], 
                                                                    "%Y"), "-01-01"))
      return(return_object)
    }
  }

# Combine the two pcds
pcd <- 
  bind_rows(pcd %>% mutate(village = as.character(village)),
            pcd_v62017)

# Clean up date in pcd
pcd$date <- as.Date(pcd$visit_date)

# Get malaria status in pcd
pcd <- pcd %>%
  mutate(rdt = ifelse(tdr_res == '1', 'Positive',
                      ifelse(tdr_res == '2', 'Negative', NA))) %>%
  mutate(malaria = ifelse(tdr_res == '1', TRUE, FALSE))

# Get spray status
# pcd <-
#   left_join(pcd %>% mutate(village = as.numeric(as.character(village))),
#             ss,
#             by = c('village' = 'village_number'))
pcd <-
  left_join(pcd %>% mutate(village = as.numeric(as.character(village))),
            cvss,
            by = c('village' = 'Village_Code')) %>%
  mutate(spray_status = Spray_status)

# Keep only 0-4 year olds
pcd <- pcd %>%
  filter(age == '1')

# Keep only realistic dates
pcd <-
  pcd %>%
  filter(date >= '2016-12-01',
         date <= '2017-08-31')

# Get health facility
pcd$health_facility <- pcd$hf_id

# Clean up health facility id
pcd <-
  pcd %>%
  mutate(health_facility = as.numeric(health_facility)) %>%
  mutate(health_facility = ifelse(health_facility %in% c(1, 17), 'Mopeia Sede',
                                  ifelse(health_facility %in% c(2, 27), '8 de Marco',
                                         ifelse(health_facility %in% c(3, 37), 'Sangalaza',
                                                ifelse(health_facility %in% c(4, 47), 'Gulamo',
                                                       ifelse(health_facility %in% c(5, 57), 'Luala',
                                                              ifelse(health_facility %in% c(6, 67),  'Chimuara',
                                                                     ifelse(health_facility %in% c(7, 77), 'Noere',
                                                                            ifelse(health_facility %in% c(8, 87), 'Nhacatundo',
                                                                                   ifelse(health_facility %in% c(9, 97), 'Nzanza', ifelse(health_facility %in% c(10, 18), 'Posto Campo', 
                                                                                                                                          ifelse(health_facility %in% c(11, 19), 'Mungane',
                                                                                                                                                 ifelse(health_facility %in% c(12, 28), 'Catale', NA)))))))))))))

pcd <- pcd %>% filter(!is.na(health_facility))
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


# Define function for making heat map
heat_map <- function(x){
  library(akima)
  fld <- with(x, interp(x = lng, 
                        y = lat, 
                        z = outcome,
                        xo = seq(min(mop2_fortified$long), 
                                 max(mop2_fortified$long), 
                                 length=100),
                        yo = seq(min(mop2_fortified$lat), 
                                 max(mop2_fortified$lat), 
                                 length=100),
                        linear = FALSE,
                        extrap = TRUE,
                        duplicate="mean",
                        nx = 100,
                        ny = 100))
  y <- interp2xyz(fld, data.frame=TRUE)
  # Keep only those in mop2
  ysp <- y
  coordinates(ysp)<- ~x+y
  proj4string(ysp) <- proj4string(mop2)
  overs <- over(ysp, polygons(mop2))
  y <- y[!is.na(overs),]
  y$z <- ifelse(y$z < 0, 0, 
                ifelse(y$z > 100, 100, y$z))
  # y$z[is.na(y$z)] <- mean(y$z, na.rm = TRUE)
  ggplot(data = y,
         aes(x = x, y = y, z = z)) + 
    geom_tile(aes(fill=z)) + 
    # coord_equal() +
    theme_map() +
    stat_contour(aes(fill=..level..), 
                 binwidth = 20,
                 # binwidth=0.005, 
                 geom="polygon") + 
    # geom_contour(color="white", alpha=0.5) +
    scale_fill_distiller(name = 'Positivity', palette="Spectral", na.value="white") + 
    theme_bw() +
    labs(x = 'Longitude',
         y = 'Latitude') +
    geom_polygon(data = mop2_fortified,
                 aes(x = long,
                     y = lat,
                     group = group, z= NA),
                 fill = NA,
                 color = 'black')
  }

