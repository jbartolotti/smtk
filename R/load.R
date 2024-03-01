
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
    if(is.na(boottimes$ms[1])){
      boottimes$ms[1]  <- strsplit(boottimes$label[1],'BOOT_TIME_EPOCH')[[1]][2]
      boottimes$label[1] <- 'BOOT_TIME_EPOCH'
    }
    if(is.na(boottimes$ms[2])){
      boottimes$ms[2] <- strsplit(boottimes$label[2],'BOOT_UPTIME_EPOCH')[[1]][2]
      boottimes$label[1] <- 'BOOT_UPTIME_EPOCH'
    }

    all_boottimes[[deriv]] = boottimes

    dfs[[deriv]] = read.csv(file = file.path(folder,filename), sep = ',', skip = preheader,stringsAsFactors = FALSE)
    # Can't do delta time here, because multiple readings for simultaneous touches get subsequent lines, and thus throw off dt. Have to do it after splitting touches.
    #    dfs[[deriv]]$delta_time = c(0,diff(dfs[[deriv]]$time))

    # If path type and direction columns included, and no condition column, create condition by combining type+dir
    if('path_type' %in% names(dfs[[deriv]]) && 'path_direction' %in% names(dfs[[deriv]]) && !('condition' %in% names(dfs[[deriv]])) ){
      dfs[[deriv]]$condition <- paste(trimws(dfs[[deriv]]$path_type), trimws(dfs[[deriv]]$path_direction), sep = '-')
    }

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
  if('start_item_name' %in% names(eventdat)){
    start_identity <- eventdat$start_item_name
  } else {
    start_identity <- 'bunny'
  }
  start_picture <- resources[[start_identity]]

  if('end_item_name' %in% names(eventdat)){
    end_identity <- eventdat$end_item_name
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
  resources$BUNNY <- resources$bunny
  resources$CARROT <- resources$carrot
  suppressWarnings(resources$straight <- png::readPNG(system.file('extdata','resources','path_straight_1600.png', package = 'smtk')))
  suppressWarnings(resources$curved <- png::readPNG(system.file('extdata','resources','path_curved_1600.png', package = 'smtk')))


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
  config$process_blocks <- 'all'
  config$path_types <- c('straight', 'curved')
  config$directions <- c('leftright', 'rightleft')
  config$mergesample_threshold_ms <- 7

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

  #centery is relative to the center of the start item
  config$curved_pathdef <- list(
    centerx1 = 481, centerx2 = 1143,
    centery1 = 191, centery2 = -191,
    radius1 = 382, radius2 = 382,
    thetastart1 = 7/6*pi, thetastart2 = 5/6*pi,
    thetaend1 = 11/6*pi, thetaend2 = 1/6*pi,
    numpoint1 = 100, numpoint2 = 100
    )

  xloc <- function(t,centerx,radius){
    return(centerx+radius*cos(t))
  }
  yloc <- function(t,centery,radius){
    return(centery+radius*sin(t))
  }
  ts1 <- seq(from=config$curved_pathdef$thetastart1,
             to = config$curved_pathdef$thetaend1,
             length.out = config$curved_pathdef$numpoint1)
  ts2 <- seq(from=config$curved_pathdef$thetastart2,
             to = config$curved_pathdef$thetaend2,
             length.out = config$curved_pathdef$numpoint2)
  config$curved_pathpoints <- data.frame(
    t = 1:(length(ts1)+length(ts2)),
    x = c( xloc(ts1, config$curved_pathdef$centerx1, config$curved_pathdef$radius1),
           xloc(ts2, config$curved_pathdef$centerx2, config$curved_pathdef$radius2)),
    y = c( yloc(ts1, config$curved_pathdef$centery1, config$curved_pathdef$radius1),
           yloc(ts2, config$curved_pathdef$centery2, config$curved_pathdef$radius2))
    )

  config$straight_pathdef <- list(
    startx = 150, endx = 1750,
    starty = 0, endy = 0,
    numpoint = 200)
  config$straight_pathpoints <- data.frame(
    t = 1:config$straight_pathdef$numpoint,
    x = seq(from = config$straight_pathdef$startx,
            to = config$straight_pathdef$endx,
            length.out = config$straight_pathdef$numpoint),
    y = seq(from = config$straight_pathdef$starty,
            to = config$straight_pathdef$endy,
            length.out = config$straight_pathdef$numpoint)
  )

  return(config)
}
