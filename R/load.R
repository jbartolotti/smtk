
LOAD.import_tablet_data <- function(folder, preheader = 2, filelist = c('accel','event','touch')) {
  files = dir(folder)
  dfs = list()
  all_boottimes = list()

  for(deriv in filelist)
  {
    filename = files[grep(deriv,files)]

    boottimes = read.csv(file = file.path(folder,filename),nrows = preheader, sep = ' ', header = FALSE, stringsAsFactors = FALSE)
    colnames(boottimes) = c('label','ms')
    boottimes$derivative = deriv
    all_boottimes[[deriv]] = boottimes

    dfs[[deriv]] = read.csv(file = file.path(folder,filename), sep = ',', skip = preheader,stringsAsFactors = FALSE)
    dfs[[deriv]]$delta_time = c(0,diff(dfs[[deriv]]$time))
    totrim = sapply(dfs[[deriv]], typeof) == 'character'
    for(i in 1:dim(dfs[[deriv]])[2])
    {
      if(totrim[i])
      {
        dfs[[deriv]][,i] = trimws(dfs[[deriv]][,i])
      }
    }

  }
  dfs$boottimes = do.call('rbind',all_boottimes)
  return(dfs)
}

# Get the names of the start and end pictures if available, or use defaults.
LOAD.getPics <- function(eventdat, resources){
  if('start_identity' %in% names(eventdat)){
    start_identity <- eventdat$start_identity
  } else {
    start_identity <- 'bunny'
  }
  start_picture <- resources[[start_identity]]

  if('end_identity' %in% names(eventdat)){
    end_identity <- eventdat$end_identity
  } else {
    end_identity <- 'carrot'
  }
  end_picture <- resources[[end_identity]]
  return(list(start = start_picture, end = end_picture))
}


#' Load picture resources to use for figure annotations
#' @export
LOAD.resources <- function(config){
  resources <- list()
  suppressWarnings(resources$bunny <- png::readPNG(system.file('extdata','resources','bunny_transp.png', package = 'smtk')))
  resources$carrot <- png::readPNG(system.file('extdata','resources','carrot_transp.png', package = 'smtk'))

  if(config$filter_type == 'butter'){
    if (config$filter_Fn == 'Fe/2'){
      config$filter_Fn <- config$filter_Fe/2
    }
    resources$filter <- signal::butter(config$filter_N, config$filter_Fc/config$filter_Fn, type = config$filter_pass)

  } else{stop('filters other than butter not currently supported. Update config$filter_type.')}
  if(config$deriv_filter_type == 'butter'){
    if (config$deriv_filter_Fn == 'Fe/2'){
      config$deriv_filter_Fn <- config$deriv_filter_Fe/2
    }
    resources$deriv_filter <- signal::butter(config$deriv_filter_N, config$deriv_filter_Fc/config$deriv_filter_Fn, type = config$deriv_filter_pass)

  } else{stop('filters other than butter not currently supported. Update config$deriv_filter_type.')}
  if(config$accel_filter_type == 'butter'){
    if (config$accel_filter_Fn == 'Fe/2'){
      config$accel_filter_Fn <- config$accel_filter_Fe/2
    }
    resources$accel_filter <- signal::butter(config$accel_filter_N, config$accel_filter_Fc/config$accel_filter_Fn, type = config$accel_filter_pass)

  } else{stop('filters other than butter not currently supported. Update config$accel_filter_type.')}


  return(resources)
  }


#' load configurations.
#' If a list, first load defaults, then overwrite with anything in the supplied config.
#' If filename, first load defaults then overwrite with anything in the txt file.
#' If 'default' then just load defaults.
#' @export
LOAD.config <- function(input_config){
  if(class(input_config) == 'list' ){
    default_config <- LOAD.defaultConfig()
    config <- modifyList(default_config, input_config)
  } else if(class(input_config) == 'character'){

    if(input_config == 'default'){
      config <- LOAD.defaultConfig()
    } else{
      #default_config <- LOAD.defaultConfig()
      # file_config <- LOAD.fileConfig(input_config) LOAD VALUES FROM TEXT FILE AND PUT IN A LIST
      # config <- modifyList(default_config, file_config)
      stop('loading config fields from a text file not currently supported. Please pass a list containing just the fields you want to overwrite to config instead of a filename.')
    }
  }
  return(config)
}

# Default configuration
LOAD.defaultConfig <- function(){
  config <- list()
  config$tablet_pixels_width <- 1920
  config$tablet_pixels_height <- 1200
  config$tablet_pixels_per_inch <- 225
  config$tablet_pixels_axes_breaks <- 200
  config$tablet_touch_sampling_rate <- 120
  config$tablet_preheader <- 2
  config$tablet_filelist <- c('accel','event','touch')
  config$process_trials <- 'all'

  config$filter_type <- 'butter'
  config$filter_Fe <- 120 # sampling frequency
  config$filter_Fn <- 'Fe/2' # Nyquist frequency
  config$filter_Fc <- 15 # Cut-off frequency
  config$filter_N <- 4 # filter order
  config$filter_pass <- 'low'

  config$deriv_filter_type <- 'butter'
  config$deriv_filter_Fe <- 120 # sampling frequency
  config$deriv_filter_Fn <- 'Fe/2' # Nyquist frequency
  config$deriv_filter_Fc <- 5 # Cut-off frequency
  config$deriv_filter_N <- 4 # filter order
  config$deriv_filter_pass <- 'low'

  config$accel_filter_type <- 'butter'
  config$accel_filter_Fe <- 100 # sampling frequency
  config$accel_filter_Fn <- 'Fe/2' # Nyquist frequency
  config$accel_filter_Fc <- 5 # Cut-off frequency
  config$accel_filter_N <- 4 # filter order
  config$accel_filter_pass <- 'low'




  return(config)
}
