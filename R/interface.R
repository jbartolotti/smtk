

#' smtdemo
#' Copy demo data to a target location, and
#' a demo script for loading and analyzing the data.
#' @param writedir Location to copy the demo data and script. Default = getwd()
#' @export
smtdemo <- function(writedir = getwd() ){
  #Get the path to the included Demo data
  tabdir <- file.path('smtk_demo_data', 'tablet')
  tablet_raw_data_folder <- system.file('extdata',tabdir,'9998', package = 'smtk')

  #Create the destination folder structure to contain the demo data, then copy it to the destination
  dir.create(file.path(writedir,tabdir), recursive = TRUE, showWarnings = FALSE)
  file.copy(tablet_raw_data_folder, file.path(writedir,tabdir), recursive = TRUE)
  message(sprintf('demo participant 9998 raw tablet data stored in %s', file.path(writedir, tabdir,'9998')))

  #Get the path to the included tablet analysis demo script
  tablet_analysis_file <- system.file('extdata','smtk_demo_analysis','smtk_demo_analysis_tablet.R', package = 'smtk')

  #Create the destination folder structure to contain analysis scripts, then copy the tablet script to the destination
  dir.create(file.path(writedir,'smtk_demo_analysis'), recursive = TRUE, showWarnings = FALSE)
  file.copy(tablet_analysis_file, file.path(writedir, 'smtk_demo_analysis','smtk_demo_analysis_tablet.R'))


}


#' importRaw.Tablet
#' Read raw text files output by the SMTK Tablet Application
#' for further processing.
#'
#' @param folder the location of the folder containing the tablet accelerometer, touch, and event text files
#' @param preheader The number of header files in the data files to ignore. Default = 2
#' @param filelist The data type suffixes of the filenames to load. Default = c('accel','event','touch')
#' @return A list with subfields for each of the three data files
#' @export
importRaw.Tablet <- function(folder,
                             preheader = 2,
                             filelist = c('accel','event','touch')){
  dat <- LOAD.import_tablet_data(folder, preheader, filelist)
  return(dat)
}


#' procOne.Tablet
#' load one participant's tablet data, process, and generate figures
#'
#' @param pid Participant ID
#' @param tablet_dat can either be a list obtained with importRaw.Tablet, or a folder name to load
#' @param figdir where to save figures. Defaults to current directory. 'none' to skip saving figures
#' @param config can be 'default' to load default values, or a list with fields that overwrite the defaults, or (FEATURE NOT SUPPORTED YET) a text file containing field-value pairs to overwrite the defaults.
procOneP.Tablet <- function(pid, tablet_dat, figdir = getwd(), resultsdir = getwd(),
                       config = 'default', runcond = c('overview','deriv')){
  # this will be returned at the end of the function and contain all figure handles and processed data.
  mydat <- list()

  #If writing figures, create the figure directory
  dofig <- figdir != 'none'
  if(dofig){
    dir.create(file.path(figdir), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(figdir, 'tiled'), recursive = TRUE, showWarnings = FALSE)

  }
  writeresults <- resultsdir != 'none'
  if(writeresults){
    dir.create(file.path(resultsdir), recursive = TRUE, showWarnings = FALSE)
  }



  # load configurations.
  # If a list, first load defaults, then overwrite with anything in the supplied config.
  # If filename, first load defaults then overwrite with anything in the txt file.
  # If 'default' then just load defaults.
  config <- LOAD.config(config)

  #load pictures for figure annotations.
  # generate filter for filtering touch data
  resources <- LOAD.resources(config)

  if(class(tablet_dat) == 'character' ){
    # Read Tablet raw data files from the selected folder (one participant)
    tablet_dat <- importRaw.Tablet(raw_tablet_folder,
                     preheader = config$tablet_preheader,
                     filelist = config$tablet_filelist)
  }

  pfigs <- list()
  if('overview' %in% runcond){
  #Create figures for each trial with an overview of all touch paths on the screen.
  #Also create a tiled figure with one row per condition, and trials as columns.
  pfigs$touch_overview <- FIGURES.OnePTouch.Tablet(tablet_dat, config, resources, pid, dofig, figdir)
  }
  # Returns a list for each Trial. Each trial contains a list of all Touches in that trial.
  # Calculated fields for: distance from the target location, distance from the end of the touch,
  # interpolated distance at each timepoint along a smooth continuous trajectory start to target or endpoint,
  # velocity, acceleration,
  # Residual distance from the endpoint, raw and filtered (at each timepoint, the euclidean distance between the actual position and
  # the corresponding position in the smooth continuous trajectory).
  if('deriv' %in% runcond){
  pTouchDat <- PROCESS.OnePTouch.Tablet(tablet_dat, config, resources, pid)
  eachTrialDat <- list()

#  aa <- list()
#  for(trial in names(pTouchDat)[!names(pTouchDat) %in% 'summary'] ){
#    aa[[trial]] <- do.call('rbind', pTouchDat[[trial]])
#  }
#  a <- do.call('rbind',aa)

  pfigs$touch_reldirtime <- list()
  pfigs$touch_reldirloc <- list()
  pfigs$touch_derivatives <- list()
  pfigs$touch_slope <- list()
  pfigs$touch_cumdist <- list()
  pfigs$touch_sqerror <- list()
  for(trial in names(pTouchDat)[!names(pTouchDat) %in% 'summary'] ){
    eachTrialDat[[trial]] <- do.call('rbind',pTouchDat[[trial]])

    pfigs$touch_reldirtime[[trial]] <- list()
    pfigs$touch_reldirloc[[trial]] <- list()
    pfigs$touch_derivatives[[trial]] <- list()
    pfigs$touch_slope[[trial]] <- list()
    pfigs$touch_cumdist[[trial]] <- list()
    pfigs$touch_sqerror[[trial]] <- list()
    trial_start <- pTouchDat$summary[[trial]]$trial_start
    trial_end <- pTouchDat$summary[[trial]]$trial_end
    timing <- list(trial_start = trial_start, trial_end = trial_end)

    for(touch in names(pTouchDat[[trial]])){
      if(!is.na(touch)){
#  dat_trialtouch <- (pTouchDat[['COND-taskA_TRIAL-4']][['TOUCH-0']])
    dat_trialtouch <- (pTouchDat[[trial]][[touch]])
    #condname <- strsplit(strsplit(trial,'_')[[1]][1],'-')[[1]][2]
    pathtype_name <- unique(dat_trialtouch$path_type)
    pathdir_name <- unique(dat_trialtouch$path_direction)
    condname <- paste(pathtype_name,pathdir_name,sep = '-')
    trialnum <- unique(dat_trialtouch$trial_number)
    touchnum <- unique(dat_trialtouch$touch_ID)
    figlabel <- sprintf('P-%s_C-%s_T-%s_M-%s', pid, condname, trialnum, touchnum)
    trial_label <- list(pid = pid, cond = condname, trial = trialnum, touch = touchnum, figlabel = figlabel)

    pfigs$touch_reldirtime[[trial]][[touch]] <-   FIGURES.TouchRelativeDirectTime.Tablet(dat_trialtouch, dofig, figdir, trial_label, timing)
    pfigs$touch_reldirloc[[trial]][[touch]] <- FIGURES.TouchRelativeDirectLocation.Tablet(dat_trialtouch, config, dofig, figdir, trial_label)
    pfigs$touch_slope[[trial]][[touch]] <- FIGURES.TouchSlope.Tablet(dat_trialtouch, config, dofig, figdir, trial_label)
    pfigs$touch_cumdist[[trial]][[touch]] <- FIGURES.TouchCumDist.Tablet(dat_trialtouch, config, dofig, figdir, trial_label)
    pfigs$touch_sqerror[[trial]][[touch]] <- FIGURES.TouchSquareError(dat_trialtouch, config, dofig, figdir, trial_label)

    #TODO!!!!!!! fix PROCESS.touch_derivatives to use delta_time for velocity etc instead of diff. Or just use the values already calculated in OnePTouch.Tablet
    pTouchDerivatives <- PROCESS.touch_derivatives(dat_trialtouch)
    pfigs$touch_derivatives[[trial]][[touch]] <- FIGURES.TouchDerivatives.Tablet(pTouchDerivatives, dofig, figdir, trial_label, timing)
    }
  }
  allTrialDat <- do.call('rbind',eachTrialDat)
  write.table(allTrialDat, file = file.path(resultsdir,sprintf('TouchData_P-%s.txt',pid)), row.names = FALSE, sep = '\t')
  }

  }
  if('tiled' %in% runcond){
    FIGURES.OnePTouch.Tiled.Tablet(tablet_dat, pid, dofig, figdir)
    gc()
    FIGURES.Touch.Tiled.Tablet(tablet_dat, pid, dofig, file.path(figdir,'cumulative_distance'),c('CumDist','RelCumDist'))
    gc()
    FIGURES.Touch.Tiled.Tablet(tablet_dat, pid, dofig, file.path(figdir,'derivatives'),c('TouchDerivatives','TouchDerivativesGrid'))
    gc()
    FIGURES.Touch.Tiled.Tablet(tablet_dat, pid, dofig, file.path(figdir,'slope'),c('Slope'))
    gc()
    FIGURES.Touch.Tiled.Tablet(tablet_dat, pid, dofig, file.path(figdir,'touch_relative_direct'),c('locationRelativeDirect','positionRelativeDirect','velocityRelativeDirect'))
    gc()
    FIGURES.Touch.Tiled.Tablet(tablet_dat, pid, dofig, file.path(figdir,'square_error'),c('SquareErrorOverview','SquareErrorValue'))
    gc()


  }


  # Put the figure handles and the processed touch data into the participant's data list
  mydat$touch_figures <- pfigs
  mydat$touch_data <- pTouchDat



  return(mydat)

}




