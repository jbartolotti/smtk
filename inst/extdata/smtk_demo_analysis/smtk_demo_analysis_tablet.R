library(smtk)

# Demo Data Analysis
data_dir <- file.path('smtk_demo_data','tablet','9999')

# Get picture resources and configuration values.
config <- LOAD.config('default')
config$process_trials <- 4 #Instead of 'all'

resources <- LOAD.resources(config)

# Loads the accelerometer, touch, and event data for participant 9999 and consolidates into a list
tablet_dat <- importRaw.Tablet(data_dir)


#NB: you can pass the variable 'data_dir' to this function directly, instead of 'tablet_dat' and it will call importRaw.Tablet on that filepath.
mydata <- procOneP.Tablet('9999', tablet_dat,
                          figdir = file.path('smtk_demo_results','tablet','figures','9999'),
                          resultsdir = file.path('smtk_demo_results','tablet','results','9999'),
                          config = config)

# 'mydata' contains figure handles and processed touch data.
# Figures and processed data are also saved in figdir and resultsdir respectively.


