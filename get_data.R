library(devtools)
if(!require('cism')){
  install_github('joebrew/cism')
}
library(cism)

# If we've already downloaded data today, don't redownload
# Check by saying if there is a dated backup for today
today <- Sys.Date()
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
    # Get the table into memory
    this_table <- get_data(tab = tbls[i],
                           connection_object = co)
    # Assign it to global environment
    assign(tbls[i],
           this_table,
           envir = .GlobalEnv)
  }
  save.image(file = paste0('backups/', file_name))
}

