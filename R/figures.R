
#Run FIGURES.overviewTouch() for each trial. Save figures, and create a tiled figure of all trials * condition.
FIGURES.OnePTouch.Tablet <- function(tablet_dat, config, resources, pid, dofig, figdir){
  #TODO NEED TO ADD THE TARGET LINE TO THE FIGURES
  all_figures <- list()
  for(block in tablet_dat$event$block_number){
    for(trial in tablet_dat$event$trial_number[tablet_dat$event$block_number == block]){
      thisrow <- tablet_dat$event$trial_number == trial & tablet_dat$event$block_number == block
      if('condition' %in% names(tablet_dat$event)){
        cond <- tablet_dat$event$condition[thisrow]
      } else {
        cond <- 'NONE'
      }
      tname <- sprintf('COND-%s_TRIAL-%s',cond, trial)
      all_figures[[tname]] <- list()
      #create the overview figures and append them to this trials' figures
      ovr_fig <- FIGURES.overviewTouch(tablet_dat, config, resources, pid, block, trial)
      all_figures[[tname]] <- append(all_figures[[tname]],ovr_fig)

      if(dofig){
        dir.create(file.path(figdir, 'touchpath'), recursive = TRUE, showWarnings = FALSE)

        ggplot2::ggsave(file.path(figdir, 'touchpath', sprintf('touchpath_P-%s_C-%s_T-%s.png',
                                                  pid, cond, trial)),
                        plot = ovr_fig$overview_titled, width = 8, height = 5)

      }


    }
  }
  return(all_figures)
}

FIGURES.OnePTouch.Tiled.Tablet <- function(tablet_dat, pid, dofig, figdir){
  if(dofig){
    allfigs <- list()
    appendedfigs <- list()
    for(cc in unique(tablet_dat$event$block)){
      allfigs[[cc]] <- list()
    }
    fignames <- dir(file.path(figdir,'touchpath'))
    for(fg in fignames){
      thiscond <- strsplit(fg,'_C-')[[1]][2]
      thiscond <- strsplit(thiscond,'_T')[[1]][1]
      thistrial <- strsplit(fg,'_T-')[[1]][2]
      thistrial <- strsplit(thistrial, '[.png]')[[1]][1]

      thisrow <- tablet_dat$event[tablet_dat$event$condition == thiscond & tablet_dat$event$trial_number == thistrial,]
      block <- thisrow$block_number
      trial <- thisrow$trial_number

      allfigs[[block]][[trial]] <- magick::image_read(file.path(figdir, 'touchpath',fg))
    }
    for(cc in 1:length(allfigs)){
        appendedfigs[[cc]] <- magick::image_append(do.call('c',allfigs[[cc]]))
    }
    allplots <- magick::image_append(do.call('c',appendedfigs), stack = TRUE)
    magick::image_write(allplots, path = file.path(figdir, 'tiled',sprintf('touchpath_P-%s.png', pid)))
    }
}

FIGURES.TouchSquareError.Tiled <- function(tablet_dat, pid, dofig, figdir){
  if(dofig){
    allfigs <- list()
    appendedfigs <- list()

    for(bb in unique(tablet_dat$event$block_number)){
      blockname <- unique(tablet_dat$event$condition[tablet_dat$event$block_number == bb])
      allfigs[[bb]] <- list()
      for(tt in unique(tablet_dat$event$trial_number[tablet_dat$event$block_number == bb])){
        touches <- unique(tablet_dat$touch$touch_ID[tablet_dat$touch$condition == blockname & tablet_dat$touch$trial_number == tt])
        biggest_touch <- 0
        biggest_size <- 0
        biggest_filename <- as.character()
        for(mm in touches){
          filename <- sprintf('SquareErrorOverview_P-%s_C-%s_T-%s_M-%s.png',pid,blockname,tt,mm)
          thissize <- file.info(file.path(figdir,'square_error',filename))$size
          if(!is.na(thissize)){
            if(thissize > biggest_size){
              biggest_touch <- mm
              biggest_size <- thissize
              biggest_filename <- filename
              }
          }
        }

        allfigs[[bb]][[tt]] <- magick::image_read(file.path(figdir,'square_error',biggest_filename))


      }
      for(cc in 1:length(allfigs)){
        appendedfigs[[cc]] <- magick::image_append(do.call('c',allfigs[[cc]]))
      }
      allplots <- magick::image_append(do.call('c',appendedfigs), stack = TRUE)
      magick::image_write(allplots, path = file.path(figdir, 'tiled',sprintf('SquareError_P-%s.png', pid)))
    }

    }


}

#figdir, file.path(figdir,'square_error')
FIGURES.Touch.Tiled.Tablet <- function(tablet_dat, pid, dofig, full_figdir, prefix ){
  if(dofig){
    for(thisprefix in prefix){
      allfigs <- list()
      appendedfigs <- list()

      for(bb in unique(tablet_dat$event$block_number)){
        blockname <- unique(tablet_dat$event$condition[tablet_dat$event$block_number == bb])
        allfigs[[bb]] <- list()
        for(tt in unique(tablet_dat$event$trial_number[tablet_dat$event$block_number == bb])){
          touches <- unique(tablet_dat$touch$touch_ID[tablet_dat$touch$condition == blockname & tablet_dat$touch$trial_number == tt])
          biggest_touch <- 0
          biggest_size <- 0
          biggest_filename <- as.character()
          for(mm in touches){
            filename <- sprintf('%s_P-%s_C-%s_T-%s_M-%s.png',thisprefix, pid,blockname,tt,mm)
            doesexist <- file.exists(file.path(full_figdir,filename))
            if(doesexist){
              thissize <- file.info(file.path(full_figdir,filename))$size
              if(!is.na(thissize)){
                if(thissize > biggest_size){
                  biggest_touch <- mm
                  biggest_size <- thissize
                  biggest_filename <- filename
                }
              }
            }
          }

          if(length(biggest_filename) > 0){
            allfigs[[bb]][[tt]] <- magick::image_read(file.path(full_figdir,biggest_filename))
          }

        }
        for(cc in 1:length(allfigs)){
          appendedfigs[[cc]] <- magick::image_append(do.call('c',allfigs[[cc]]))
        }
        allplots <- magick::image_append(do.call('c',appendedfigs), stack = TRUE)
        magick::image_write(allplots, path = file.path(figdir, 'tiled',sprintf('%s_P-%s.png', thisprefix, pid)))
      }
    }

  }


}


FIGURES.OnePTouch.Tiled.Tablet_old <- function(all_figures, pid, dofig, figdir){
  #tile the trial overview figures
  if(dofig){
    #if more than one condition, row for each condition.
    #TODO: if not, make a box from the trials, wider than tall

    allconds <- list()
    cond_appended <- list()
    for(ct in names(all_figures)){
      ct_split <- strsplit(ct,'_')[[1]]
      cond <- strsplit(ct_split,'COND-')[[1]][2]
      trialnum <- strsplit(ct_split,'TRIAL-')[[2]][2]


      thisfig <- magick::image_read(
        file.path(figdir, 'touchpath', sprintf('touchpath_P-%s_C-%s_T-%s.png', pid, cond, trialnum)) )
      if( !(cond %in% names(allconds)) ){
        allconds[[cond]] <- list()
      }
      index <- length(allconds[[cond]])+1
      allconds[[cond]][[index]] <- thisfig
    }
    for(cond in names(allconds)){
      cond_appended[[cond]] <- magick::image_append(do.call('c',allconds[[cond]]))
    }
    allplots <- magick::image_append(do.call('c',cond_appended), stack = TRUE)
    magick::image_write(allplots, path = file.path(figdir, 'tiled', sprintf('touchpath_P-%s.png',pid)))
  }
}

# Create a figure object the size of the tablet in pixels.
# Add start and end pictures and touch data.
# Returns the overview figure, and an additional version with a participant/trial title.
# Does not save figures to disk
FIGURES.overviewTouch <- function(dat, config, resources, pid, blocknum, trialnum){
  figlist <- list()

  # Get trial start/end times. The start of one trial is the end of the previous,
  # We need to append a very large number to the end to serve as the final trial's end.
  #trial_times <- c(dat$event$time[dat$event$event_marker == 'START'],1000000000)
  #OUTDATED. Trials now store trial and block data
  #Get this trial's data
  trialdat <- subset(dat$touch, block_number == blocknum & trial_number == trialnum)
  eventdat <- dat$event[dat$event$trial_number == trialnum & dat$event$block_number == blocknum,]
  if(trialdat$touch_action[1] == 'TOUCH_END'){
    trialdat = trialdat[2:dim(trialdat)[1],]
  }


  if('condition' %in% names(eventdat)){
    cond <- eventdat$condition
  } else {
    cond <- 'NONE'
  }

  # Get the names of the start and end pictures if available, or use defaults.
  # Then assign pictures to pics$start and pics$end
  pics <- LOAD.getPics(eventdat, resources)

  # Initialize Figure
  gtouch <- ggplot2::ggplot(trialdat) +
    ggplot2::aes(x = touch_x, y = touch_y, color = time-time[1]+1, shape = factor(touch_ID), alpha = touch_area)
  # Add Start and Ending figures
  gtouch <- FIGURES.add_pictures_plot(gtouch, eventdat, pics$start, pics$end)
  # Add touch trajectory data
  gtouch <- gtouch + ggplot2::geom_point(size = 4)
  gtouch <- FIGURES.add_onepicture_plot(gtouch, resources[[eventdat$path_type]],
                                       0, eventdat$start_item_center_y-(500/2),
                                       1920,eventdat$start_item_center_y+(500/2),
                                       1920,500, resize = FALSE)

  # Sets axes, tick marks, labels, and legend
  figlist$overview <- FIGURES.format_plot(gtouch, config, 'touch')
  figlist$overview_titled <- FIGURES.format_plot(gtouch, config, 'touch', plot_title = sprintf('Participant: %s, Condition: %s, Trial: %s',pid, cond, trialnum))

  return(figlist)
}

FIGURES.TouchRelativeDirectTime.Tablet <- function(dat, dofig, figdir, trial_label, timing){

#  first_timepoint <- dat$time[1]
  f1 <- ggplot2::ggplot(dat[20:dim(dat)[1],], ggplot2::aes(x = (time-timing$trial_start)/1000, y = velocity_resid_dist_end_filter*-1)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line(size = 1) +
    ggplot2::coord_cartesian(xlim = c(0, (timing$trial_end-timing$trial_start)/1000)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x = 'time (S)', y = 'velocity relative to direct motion (pixel/S)', title = sprintf('Participant %s, Condition %s, Trial %s, Touch %s',trial_label$pid,trial_label$cond,trial_label$trial,trial_label$touch) )
  if(dofig){
    dir.create(file.path(figdir, 'touch_relative_direct'), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(file.path(figdir, 'touch_relative_direct', sprintf('velocityRelativeDirect_%s.png',trial_label$figlabel)),
                    plot = f1, width = 8, height = 5)
  }
  f2 <- ggplot2::ggplot(dat[1:dim(dat)[1],], ggplot2::aes(x = (time-timing$trial_start)/1000, y = resid_dist_end_filter)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line(size = 1) +
    ggplot2::coord_cartesian(xlim = c(0, (timing$trial_end-timing$trial_start)/1000)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x = 'time (S)', y = 'position relative to direct motion (pixels)', title = sprintf('Participant %s, Condition %s, Trial %s, Touch %s',trial_label$pid,trial_label$cond,trial_label$trial,trial_label$touch) )
  if(dofig){
    ggplot2::ggsave(file.path(figdir, 'touch_relative_direct', sprintf('positionRelativeDirect_%s.png',trial_label$figlabel)),
                    plot = f2, width = 8, height = 5)
  }
  reldirtime <- list(velocity = f1, position = f2)
  return(reldirtime)
}

FIGURES.TouchSquareError <- function(dat, config, dofig, figdir, trial_label){
  if(dat$path_type[1] == 'curved'){
    targetline <- config$curved_pathpoints
  } else {targetline <- config$straight_pathpoints}

  targetline$y <- targetline$y + dat$target_starty[1]

  f1 <- ggplot2::ggplot(dat, ggplot2::aes(x = touch_x, y = touch_y, color = sqer)) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = targetline, ggplot2::aes(x = x, y = y), color = 'black', size = .5, shape = 1)
  f1 <- FIGURES.format_plot(f1, config, 'touch')
  f1 <- f1 + ggplot2::scale_color_gradientn(limits = c(0, max(dat$sqer)), colors = c('yellow','red'), name = 'Square Error')

  f2 <- ggplot2::ggplot(dat, ggplot2::aes(x = (time - time[1])/1000, y = sqer))+#, color = (time-time[1])/1000)) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    #viridis::scale_color_viridis(name = 'Time') +
    ggplot2::labs(x = 'Time', y = 'Square Error')
  if(dofig){
    dir.create(file.path(figdir, 'square_error'), recursive = TRUE, showWarnings = FALSE)

    ggplot2::ggsave(file.path(figdir, 'square_error',sprintf('SquareErrorOverview_%s.png',trial_label$figlabel)),
                    plot = f1, width = 8, height = 5)
    ggplot2::ggsave(file.path(figdir, 'square_error',sprintf('SquareErrorValue_%s.png',trial_label$figlabel)),
                    plot = f2, width = 6, height = 5)

  }
  }

FIGURES.TouchSlope.Tablet <- function(dat, config, dofig, figdir, trial_label, timing){

  # X, Y, Time: start, end, range
  xstart <- dat$touch_x[1]
  xend <- dat$touch_x[length(dat$touch_x)]
  xrange <- xend - xstart

  ystart <- dat$touch_y[1]
  yend <- dat$touch_y[length(dat$touch_y)]
  yrange <- yend-ystart

  timestart <- min(dat$time)
  timeend <- max(dat$time)
  timerange <- timeend-timestart
  numt <- dim(dat)[1]

  #the maximum in the positive or negative direction for the slope; used for calibrating color scale
  r_extent <- max(abs(min(dat$slope, na.rm = TRUE)), abs(max(dat$slope, na.rm = TRUE)), na.rm = TRUE)

  #corresponding x and y values for each timepoint on a direct motion trajectory
  xs <- xstart + xrange*(dat$time - timestart)/timerange
  ys <- ystart + yrange*(dat$time - timestart)/timerange

  #identities of the locations where slope is at extremes

  highest_up_slope <- subset(dat, slope == max(dat$slope, na.rm = TRUE))
  highest_down_slope <- subset(dat, slope == min(dat$slope, na.rm = TRUE))
  flattest_slope <-  subset(dat, abs(dat$slope) == min(abs(dat$slope), na.rm = TRUE))

  f1 <- ggplot2::ggplot(dat, ggplot2::aes(x = touch_x, y = touch_y, color = slope)) +
    ggplot2::theme_bw() +
    ggplot2::geom_point()

  #apply standard formatting to reflect tablet screen
  f1 <- FIGURES.format_plot(f1, config, 'touch', plot_title = sprintf('Slope\nParticipant %s, Condition %s, Trial %s, Touch %s',trial_label$pid,trial_label$cond,trial_label$trial,trial_label$touch))
  #overwrite viridis color with a gradient scaled farther or closer relative to smooth movement
  f1 <- f1 + ggplot2::scale_color_gradientn(limits = c(-1*r_extent, r_extent), colors = c('blue','cyan','#AAAAAA','yellow','red'), values = c(0,.49,.5,.51,1), name = 'Slope')

  if(dofig){
    dir.create(file.path(figdir, 'slope'), recursive = TRUE, showWarnings = FALSE)

    ggplot2::ggsave(file.path(figdir, 'slope',sprintf('Slope_%s.png',trial_label$figlabel)),
                    plot = f1, width = 8, height = 5)
  }
}


FIGURES.TouchCumDist.Tablet <- function(dat, config, dofig, figdir, trial_label, timing){

  # X, Y, Time: start, end, range
  xstart <- dat$touch_x[1]
  xend <- dat$touch_x[length(dat$touch_x)]
  xrange <- xend - xstart

  ystart <- dat$touch_y[1]
  yend <- dat$touch_y[length(dat$touch_y)]
  yrange <- yend-ystart

  timestart <- min(dat$time)
  timeend <- max(dat$time)
  timerange <- timeend-timestart
  numt <- dim(dat)[1]

  dat$cumdist_delta_pct <- dat$cumdist_pct - dat$cumdist_smooth_pct
  #the maximum in the positive or negative direction for the slope; used for calibrating color scale
  r_extent <- max(abs(dat$cumdist_delta_pct))

  #corresponding x and y values for each timepoint on a direct motion trajectory
  xs <- xstart + xrange*(dat$time - timestart)/timerange
  ys <- ystart + yrange*(dat$time - timestart)/timerange

  #identities of the locations where slope is at extremes

  biggest <- subset(dat, cumdist_delta_pct == max(cumdist_delta_pct, na.rm = TRUE))
  smallest <- subset(dat, slope == min(cumdist_delta_pct, na.rm = TRUE))
  flattest <-  subset(dat, abs(cumdist_delta_pct) == min(abs(cumdist_delta_pct), na.rm = TRUE))

  f1 <- ggplot2::ggplot(dat, ggplot2::aes(x = time, y = cumdist_pct)) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(ggplot2::aes(y = (time-min(time))/(max(time)-min(time))), color = 'grey') +
    ggplot2::geom_point()
  if(dofig){
    dir.create(file.path(figdir, 'cumulative_distance'), recursive = TRUE, showWarnings = FALSE)

    ggplot2::ggsave(file.path(figdir, 'cumulative_distance', sprintf('CumDist_%s.png',trial_label$figlabel)),
                    plot = f1, width = 5, height = 5)
  }

  f2 <- ggplot2::ggplot(dat, ggplot2::aes(x = touch_x, y = touch_y, color = cumdist_delta_pct)) +
    ggplot2::theme_bw() +
    ggplot2::geom_point()

    f2 <- FIGURES.format_plot(f2, config, 'touch', plot_title = sprintf('Relative Cumulative Distance\nParticipant %s, Condition %s, Trial %s, Touch %s',trial_label$pid,trial_label$cond,trial_label$trial,trial_label$touch))
  #overwrite viridis color with a gradient scaled farther or closer relative to smooth movement
  f2 <- f2 + ggplot2::scale_color_gradientn(limits = c(-1*r_extent, r_extent), colors = c('blue','cyan','#AAAAAA','yellow','red'), values = c(0,.49,.5,.51,1), name = 'Relative Cumulative Distance')
  if(dofig){

    ggplot2::ggsave(file.path(figdir, 'cumulative_distance',sprintf('RelCumDist_%s.png',trial_label$figlabel)),
                    plot = f2, width = 8, height = 5)
  }



}


FIGURES.TouchRelativeDirectLocation.Tablet <- function(dat, config, dofig, figdir, trial_label) {

  # X, Y, Time: start, end, range
  xstart <- dat$touch_x[1]
  xend <- dat$touch_x[length(dat$touch_x)]
  xrange <- xend - xstart

  ystart <- dat$touch_y[1]
  yend <- dat$touch_y[length(dat$touch_y)]
  yrange <- yend-ystart

  timestart <- min(dat$time)
  timeend <- max(dat$time)
  timerange <- timeend-timestart
  numt <- dim(dat)[1]

  #the maximum in the positive or negative direction for the residual distance; used for calibrating color scale
  r_extent <- max(abs(min(dat$resid_dist_end_filter)), abs(max(dat$resid_dist_end_filter)))

  #corresponding x and y values for each timepoint on a direct motion trajectory
  xs <- xstart + xrange*(dat$time - timestart)/timerange
  ys <- ystart + yrange*(dat$time - timestart)/timerange

  #identities of the locations where the actual movement is:
  # closest to end relative to smooth (fastest_resid)
  # farthest from end relative to smooth (slowest_resid)
  # most similar to smooth (mid_resid)

  dat$resid_dist_end_filter[is.na(dat$resid_dist_end_filter)] <- 0
  fastest_resid <- subset(dat, resid_dist_end_filter == min(resid_dist_end_filter))
  slowest_resid <- subset(dat, resid_dist_end_filter == max(resid_dist_end_filter))
  mid_resid <-  subset(dat, abs(resid_dist_end_filter) == min(abs(resid_dist_end_filter)))


  f1 <- ggplot2::ggplot(dat, ggplot2::aes(x = touch_x, y = touch_y, color = resid_dist_end_filter)) +
    ggplot2::theme_bw() +
    # Actual movement line
    ggplot2::geom_line(color = 'black') +
    #markers for close/far/similar
    ggplot2::geom_point(ggplot2::aes(x = fastest_resid$touch_x, y = fastest_resid$touch_y), color = 'blue', shape = 8, size = 5) +
    ggplot2::geom_point(ggplot2::aes(x = slowest_resid$touch_x, y = slowest_resid$touch_y), color = 'red', shape = 8, size = 5) +
    ggplot2::geom_point(ggplot2::aes(x = mid_resid$touch_x, y = mid_resid$touch_y), color = '#AAAAAA', shape = 8, size = 5) +

    #plot the smooth line and its markers
    ggplot2::geom_line(ggplot2::aes(x = xs, y = ys), color = 'black') +
    ggplot2::geom_point(ggplot2::aes(x = xstart+xrange*((fastest_resid$time-timestart)/timerange), y = ystart+yrange*((fastest_resid$time-timestart)/timerange)), color = 'blue', shape = 8, size = 5) +
    ggplot2::geom_point(ggplot2::aes(x = xstart+xrange*((slowest_resid$time-timestart)/timerange), y = ystart+yrange*((slowest_resid$time-timestart)/timerange)), color = 'red', shape = 8, size = 5) +
    ggplot2::geom_point(ggplot2::aes(x = xstart+xrange*((mid_resid$time-timestart)/timerange), y = ystart+yrange*((mid_resid$time-timestart)/timerange)), color = '#AAAAAA', shape = 8, size = 5) +

    # plot the dots for the actual movement and smooth movement
    ggplot2::geom_point(data = dat[seq(1,dim(dat)[1], by = 10),]) +
    ggplot2::geom_point(data = dat[seq(1,dim(dat)[1], by = 10),], ggplot2::aes(x = xs[seq(1,dim(dat)[1], by = 10)], y = ys[seq(1,dim(dat)[1], by = 10)]))

  #apply standard formatting to reflect tablet screen
  f1 <- FIGURES.format_plot(f1, config, 'touch', plot_title = sprintf('Actual vs Smooth Movement\nParticipant %s, Condition %s, Trial %s, Touch %s',trial_label$pid,trial_label$cond,trial_label$trial,trial_label$touch))
  #overwrite viridis color with a gradient scaled farther or closer relative to smooth movement
  f1 <- f1 + ggplot2::scale_color_gradientn(limits = c(-1*r_extent, r_extent), colors = c('blue','cyan','#AAAAAA','yellow','red'), values = c(0,.49,.5,.51,1), name = 'Relative\nDistance')
  if(dofig){
    dir.create(file.path(figdir, 'touch_relative_direct'), recursive = TRUE, showWarnings = FALSE)

    ggplot2::ggsave(file.path(figdir, 'touch_relative_direct', sprintf('locationRelativeDirect_%s.png',trial_label$figlabel)),
                    plot = f1, width = 8, height = 5)
  }
}

FIGURES.TouchDerivatives.Tablet <- function(pTouchDerivatives, dofig, figdir, trial_label, timing){
  dat <- subset(pTouchDerivatives$long, type == 'scale_plusminus1' & metric == 'x')
  dat$degree <- factor(dat$degree, levels = c('position','velocity','acceleration','jerk'))
#  first_timepoint <- dat$time[1]
  f1 <- ggplot2::ggplot(subset(dat, degree != 'jerk'), ggplot2::aes(x = (time-timing$trial_start)/1000, y = value, color = degree)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line() +
    ggplot2::coord_cartesian(xlim = c(0, (timing$trial_end-timing$trial_start)/1000)) +
    ggplot2::geom_line(data = subset(dat, degree == 'position'), size = 1.5) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_color_manual(values = c('black','red','#009900','blue')) +
    ggplot2::labs(x = 'Time (s)', y = 'Value (scaled -1 to 1)', title = sprintf('Participant %s, Condition %s, Trial %s, Touch %s',trial_label$pid,trial_label$cond,trial_label$trial,trial_label$touch))
  if(dofig){
    dir.create(file.path(figdir, 'derivatives'), recursive = TRUE, showWarnings = FALSE)

    ggplot2::ggsave(file.path(figdir, 'derivatives',sprintf('TouchDerivatives_%s.png',trial_label$figlabel)),
                    plot = f1, width = 8, height = 5)
  }


  dat <- subset(pTouchDerivatives$long, type == 'filtered')
  dat$metric <- factor(dat$metric, levels = c('x','y','dist'), labels = c('X Position','Y Position','Travel Distance'))
  dat$degree <- factor(dat$degree, levels = c('position','velocity','acceleration','jerk'))
#  first_timepoint <- dat$time[1]
  f2 <- ggplot2::ggplot(dat, ggplot2::aes(x = (time-timing$trial_start)/1000, y = value, color = degree)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line() +
    ggplot2::coord_cartesian(xlim = c(0, (timing$trial_end-timing$trial_start)/1000)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_color_manual(values = c('black','red','#009900','blue'), guide = 'none') +
    ggplot2::facet_grid(degree~metric, scales = 'free') +
    ggplot2::labs(x = 'Time (s)', y = 'pixels', title = sprintf('Participant %s, Condition %s\nTrial %s, Touch %s',trial_label$pid,trial_label$cond,trial_label$trial,trial_label$touch))
  if(dofig){
    ggplot2::ggsave(file.path(figdir, 'derivatives', sprintf('TouchDerivativesGrid_%s.png',trial_label$figlabel)),
                    plot = f2, width = 6, height = 6)
  }


  dat <- subset(pTouchDerivatives$long, type == 'raw')
  dat$metric <- factor(dat$metric, levels = c('x','y','dist'), labels = c('X Position','Y Position','Travel Distance'))
  dat$degree <- factor(dat$degree, levels = c('position','velocity','acceleration','jerk'))
  #  first_timepoint <- dat$time[1]
  f3 <- ggplot2::ggplot(dat, ggplot2::aes(x = (time-timing$trial_start)/1000, y = value, color = degree)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line() +
    ggplot2::coord_cartesian(xlim = c(0, (timing$trial_end-timing$trial_start)/1000)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_color_manual(values = c('black','red','#009900','blue'), guide = 'none') +
    ggplot2::facet_grid(degree~metric, scales = 'free') +
    ggplot2::labs(x = 'Time (s)', y = 'pixels', title = sprintf('Participant %s, Condition %s\nTrial %s, Touch %s',trial_label$pid,trial_label$cond,trial_label$trial,trial_label$touch))
  if(dofig){
    ggplot2::ggsave(file.path(figdir, 'derivatives', sprintf('TouchDerivativesGrid_Unfilt_%s.png',trial_label$figlabel)),
                    plot = f3, width = 6, height = 6)
  }

  touchderiv <- list(pva = f1, pvaj_grid = f2, pvaj_unfilt_grid = f3)
  return(touchderiv)
}

FIGURES.add_pictures_plot <- function(gplot, eventdat, stpic, endpic)
{
  # Get starting item dimensions and add to plot
  stpicbox_start_x <- eventdat$start_item_center_x - eventdat$start_item_width/2
  stpicbox_start_y <- eventdat$start_item_center_y - eventdat$start_item_height/2
  stpicbox_end_x <- eventdat$start_item_center_x + eventdat$start_item_width/2
  stpicbox_end_y <- eventdat$start_item_center_y + eventdat$start_item_height/2
  stpicbox_width <- eventdat$start_item_width
  stpicbox_height <- eventdat$start_item_height

  gplot <- FIGURES.add_onepicture_plot(gplot, stpic,
                      stpicbox_start_x, stpicbox_start_y,
                      stpicbox_end_x, stpicbox_end_y,
                      stpicbox_width, stpicbox_height)

  # Get ending item dimensions and add to plot
  endpicbox_start_x <- eventdat$end_item_center_x - eventdat$end_item_width/2
  endpicbox_start_y <- eventdat$end_item_center_y - eventdat$end_item_height/2
  endpicbox_end_x <- eventdat$end_item_center_x + eventdat$end_item_width/2
  endpicbox_end_y <- eventdat$end_item_center_y + eventdat$end_item_height/2
  endpicbox_width <- eventdat$end_item_width
  endpicbox_height <- eventdat$end_item_height

  gplot <- FIGURES.add_onepicture_plot(gplot, endpic,
                       endpicbox_start_x, endpicbox_start_y,
                       endpicbox_end_x, endpicbox_end_y,
                       endpicbox_width, endpicbox_height)


}


# Add a single image to the plot, centering in a box if necessary
FIGURES.add_onepicture_plot <- function(gplot, pic,
                                start_x, start_y,
                                end_x, end_y,
                                box_width, box_height, resize = TRUE){

  # Get dimensions of picture, and set the start and end points to center the item in a box, if it is shorter in one dimension
  if(resize){
    height = dim(pic)[1]
    width = dim(pic)[2]
    if(height > width) #tree. Move on x to be centered in the square
    {
      start_x = start_x + floor( (height - width)/2 /height * box_width )
      end_x = end_x - floor( (height - width)/2 /height * box_width )
    } else if(height < width) #snake. Move on y to be centered in the square
    {
      start_y = start_y + floor( (width - height)/2 /width * box_height )
      end_y =end_y - floor( (width - height)/2 /width * box_height )
    }
  }
  # Add picture to plot
  gplot <- gplot +
    ggplot2::annotation_raster(pic,
                               xmin = start_x, xmax = end_x,
                               ymin =-start_y, ymax = -end_y)

  return(gplot)
}

FIGURES.format_plot <- function(gplot, config, type, plot_title = '')
{
  HEIGHT <- config$tablet_pixels_height
  WIDTH <- config$tablet_pixels_width
  SPACING <- config$tablet_pixels_axes_breaks

    gplot <- gplot +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = c(0,WIDTH), ylim = c(HEIGHT,00)) +
    viridis::scale_color_viridis() +
      #x scale goes from 0 to WIDTH. tick marks are spaced every SPACING pixels,
      #and an additional label at the endpoint
    ggplot2::scale_x_continuous(expand = c(0,0), position = 'top', breaks = unique(c(seq(0,floor(WIDTH/SPACING)*SPACING, SPACING), WIDTH)) ) +
      #y scale goes from 0 to HEIGHT. tick marks are spaced every SPACING pixels,
      #and additional label at the endpoint. Y scale is reversed to put 0,0 in top left.
    ggplot2::scale_y_continuous(expand = c(0,0), trans = 'reverse',breaks = unique(c(HEIGHT, seq(HEIGHT,0,-1*SPACING))) ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::labs(x = '', y = '', color = 'time', shape = 'touchID', alpha = 'pressure', title = plot_title)
  if (type == 'touch')
  {
    # No additional formatting
  }

  return(gplot)
}
