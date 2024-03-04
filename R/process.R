

PROCESS.OnePTouch.Tablet <- function(tablet_dat, config, resources, pid){

  #Determine which blocks to process. Default is all of them,
  # by cross-combining all path_types and all directions., and then including those present in event data
  # e.g., "straight_leftright" "curved_leftright"   "straight_rightleft" "curved_rightleft"
  use_blocks <- config$process_blocks
  if (class(use_blocks) == 'character'){
    if(use_blocks == 'all'){
      use_blocks <- apply(expand.grid(config$path_types,config$directions),1,paste, collapse = '-')
      available_blocks <- unique(tablet_dat$event$condition)
      use_blocks <- use_blocks[use_blocks %in% available_blocks]
    }
  }


  #Determine which trials in each block to process. Default is all of them.
  #If specifying a subset, config$process_trials should be a list with trial entries for each block
  use_trials <- config$process_trials
  if (class(use_trials) == 'character'){
    if(use_trials == 'all'){
      use_trials <- list()
      for(b in use_blocks){
        use_trials[[b]] <- tablet_dat$event$trial_number[tablet_dat$event$condition == b]
      }
    }
  }

#  trial_times <- c(tablet_dat$event$time[tablet_dat$event$event_marker == 'START'],1000000000)
#  xstarts <- tablet_dat$event$start_item_center_x[tablet_dat$event$event_marker == 'START']
#  ystarts <- tablet_dat$event$start_item_center_y[tablet_dat$event$event_marker == 'START']
#  xends <- tablet_dat$event$end_item_center_x[tablet_dat$event$event_marker == 'START']
#  yends <- tablet_dat$event$end_item_center_y[tablet_dat$event$event_marker == 'START']


  trial_touches <- list()
  trial_touches$summary <- list()
  for(block in use_blocks){
  for( trial in use_trials[[block]]){
    if('condition' %in% names(tablet_dat$event)){
      eventrow <- tablet_dat$event$trial_number == trial & tablet_dat$event$condition == block
      cond <- tablet_dat$event$condition[eventrow]
    } else {
      cond <- 'NONE'
    }

    this_xstart <- tablet_dat$event$start_item_center_x[eventrow]
    this_ystart <- tablet_dat$event$start_item_center_y[eventrow]
    this_xend <- tablet_dat$event$end_item_center_x[eventrow]
    this_yend <- tablet_dat$event$end_item_center_y[eventrow]
    this_distance <- sqrt((this_xstart-this_xend)^2 + (this_ystart-this_yend)^2)

    if( 'trial_number' %in% names(tablet_dat$touch) && 'condition' %in% names(tablet_dat$touch)){
      alltouches <- subset(tablet_dat$touch, condition == cond & trial_number == trial)
      alltouches$pid <- pid
      alltouches$trial_id <- trial
    } else{
      alltouches <- subset(tablet_dat$touch, time >= trial_times[trial] & time < trial_times[trial+1])
      alltouches$pid <- pid
      alltouches$trial_id <- trial
      alltouches$condition <- cond
    }
    alltouches$target_startx <- this_xstart
    alltouches$target_starty <- this_ystart
    alltouches$target_endx <- this_xend
    alltouches$target_endy <- this_yend


    each_touch <- list()
    for(t_id in unique(alltouches$touch_ID)){
      dd <- subset(alltouches, touch_ID == t_id)
      if(dim(dd)[1] > 2){
        if(dd$touch_action[1] == 'TOUCH_END'){
          dd = dd[2:dim(dd)[1],]
        }
        dd$delta_time <- c(-1, dd$time[2:dim(dd)[1]] - dd$time[1:(dim(dd)[1]-1)] ) #first timepoint gets dt -1
        dd <- dd[dd$delta_time < 0 | dd$delta_time > 1 ,] #remove DTs of 0 or 1; they're not real samples and have fractional positions.

        #If there are two samples in a row that are each less than 7 ms, delete the first one. This will essentially combine the two into a single sample closer to the standard 8ms sampling rate.
        delrow <- logical(length = dim(dd)[1])
        for(i in 1:(dim(dd)[1]-1)){
          do_delete <- dd$delta_time[i] < config$mergesample_threshold_ms & dd$delta_time[i+1] < config$mergesample_threshold_ms
          if(do_delete){
            # Mark the row for deletion, and add its time to the subsequent sample (so that a chain of 5,5,5 becomes 10,5 instead of 15)
            delrow[i] <- TRUE
            dd$delta_time[i+1] <- dd$delta_time[i] + dd$delta_time[i+1]
          }
        }
        dd <- dd[!delrow,]
        if(dim(dd)[1] > 2){
        #Redo delta time after removing rows just in case. It should already be adjusted though.
        dd$delta_time <- c(0, dd$time[2:dim(dd)[1]] - dd$time[1:(dim(dd)[1]-1)] )


        numt <- dim(dd)[1]
        dd$x_velocity_raw <- c(diff(dd$touch_x)/dd$delta_time[2:numt],0)
        dd$x_acceleration_raw <- c(diff(dd$x_velocity_raw)/dd$delta_time[2:numt],0)
        dd$x_jerk_raw <- c(diff(dd$x_acceleration_raw)/dd$delta_time[2:numt],0)

        dd$x_velocity <- c(signal::filtfilt(resources$deriv_filter, diff(dd$touch_x)/dd$delta_time[2:numt]),0)
        dd$x_acceleration <- c(signal::filtfilt(resources$deriv_filter, diff(dd$x_velocity)/dd$delta_time[2:numt]),0)
        dd$x_jerk <- c(signal::filtfilt(resources$deriv_filter, diff(dd$x_acceleration)/dd$delta_time[2:numt]),0)
  #      dd$x_velocity[(numt-1):numt] <- NA
  #      dd$x_acceleration[(numt-2):numt] <- NA

        dd$y_velocity <- c(signal::filtfilt(resources$deriv_filter, diff(dd$touch_y)/dd$delta_time[2:numt]),0)
        dd$y_acceleration <- c(signal::filtfilt(resources$deriv_filter, diff(dd$y_velocity)/dd$delta_time[2:numt]),0)
        dd$y_jerk <- c(signal::filtfilt(resources$deriv_filter, diff(dd$y_acceleration)/dd$delta_time[2:numt]),0)
  #      dd$y_velocity[(numt-1):numt] <- NA
  #      dd$y_velocity[(numt-2):numt] <- NA

        dd$y_velocity_raw <- c(diff(dd$touch_y)/dd$delta_time[2:numt],0)
        dd$y_acceleration_raw <- c(diff(dd$y_velocity_raw)/dd$delta_time[2:numt],0)
        dd$y_jerk_raw <- c(diff(dd$y_acceleration_raw)/dd$delta_time[2:numt],0)


        dd$eucdist <- 0
        for(i in 2:(dim(dd)[1])){
          dd$eucdist[i] <- sqrt( (dd$touch_x[i] - dd$touch_x[i-1])^2 + (dd$touch_y[i] - dd$touch_y[i-1])^2 )
        }
        dd$cumdist <- cumsum(dd$eucdist)
        dd$cumdist_pct <- dd$cumdist / max(dd$cumdist)
        dd$cumdist_smooth_pct <- (dd$time - min(dd$time)) / (max(dd$time) - min(dd$time))


        dd$slope = 0
        dd$slope2 = 0
        for(i in 1:(dim(dd)[1]-1)){
          rise = dd$touch_y[i+1] - dd$touch_y[i]
          run = dd$touch_x[i+1] - dd$touch_x[i]
          if(run == 0){dd$slope[i] = NA} else{
            dd$slope[i] = rise/run
            }
          }


        #testing figures
        if(FALSE){
          r_extent <- max(abs(dd$cumdist_smooth_pct - dd$cumdist_pct))
          ggplot(dd, aes(x = time, y = cumdist/max(cumdist))) + geom_point(aes(y = (time-min(time))/(max(time)-min(time))), color = 'grey') + geom_point()
          ggplot(dd, aes(x = touch_x, y = touch_y, color = cumdist_pct - cumdist_smooth_pct)) + theme_bw() + geom_point() + scale_color_gradientn(limits = c(-1*r_extent, r_extent), colors = c('blue','cyan','#AAAAAA','yellow','red'), values = c(0,.49,.5,.51,1), name = 'Relative\nDistance') + coord_cartesian(xlim = c(100,1500), ylim = c(100,1500))
          ggplot(dd, aes(x = touch_x, y = touch_y, color = eucdist)) + theme_bw() + geom_point() + viridis::scale_color_viridis()+ coord_cartesian(xlim = c(100,1500), ylim = c(100,1500))

        ggplot(dd, aes(x = touch_x, y = touch_y, color = slope)) + theme_bw() + geom_point() + viridis::scale_color_viridis()+ coord_cartesian(xlim = c(100,1500), ylim = c(100,1500))

      r_extent_s <- max(abs(dd$slope), na.rm = TRUE)
      ggplot(dd, aes(x = touch_x, y = touch_y, color = slope)) + theme_bw() + geom_point() + scale_color_gradientn(limits = c(-1*r_extent_s, r_extent_s), colors = c('blue','cyan','#AAAAAA','yellow','red'), values = c(0,.49,.5,.51,1), name = 'Slope') + coord_cartesian(xlim = c(100,1500), ylim = c(100,1500))
      }

        ###at each timepoint, want shortest distance to the target line.
        if(dd$path_type[1] == 'curved'){
          targetline <- config$curved_pathpoints
        } else {targetline <- config$straight_pathpoints}

        targetline$y <- targetline$y + this_ystart

        dd$sqer <- unlist(lapply(1:dim(dd)[1], function(n){
          min(UTILS.PointToCurve(dd$touch_x[n], dd$touch_y[n],
                                 targetline$x, targetline$y)
          )
        }))


        ###TODO: Curve methods: start and end times of curve. Get position on the curve smooth movement at each time.
        ### first half: angleA = 2pi/3*time/(maxt/2). second half: angleB = 2pi/3*(time/maxt - .5)
        ### LtoR: y'= sin(angleA+pi/6)*382; x' = cos(angleA+pi/6)*382
        ### LtoR: y0 = sin(pi/6)*382; x0 = cos(pi/6)*382
        ### LtoR: y = STARTY + y'-y0; x = STARTX + x0-x' * (1 if t < maxt/4; else -1)

        #for start and end of touch, find the closest points. use those as start end of ideal curve.

        #cumdist is progression along

        dd$distance = NA
        dd$dist_from_target <- NA

        num_touchsamples <- dim(dd)[1]
        #get percent of max dist from target, then multiply by the max distance.
        #This is distance from target at each time, based constant velocity smooth movement.
        dd$smooth_dist_from_target <- ((num_touchsamples:1)/num_touchsamples)*this_distance
        #distance between the first sample of this touch and the last sample of this touch
        dist_start_end <- sqrt((dd$touch_x[1]-dd$touch_x[num_touchsamples])^2-(dd$touch_y[1]-dd$touch_y[num_touchsamples])^2)

        #How much distance there should be between the current sample and the end of the touch movement, based on a smooth linear movement from the touch's start to its end
        dd$smooth_dist_from_end <- ((num_touchsamples:1)/num_touchsamples)*dist_start_end

        #get distance traveled and distance to the target and dist to end of movement.
        for(i in 1:dim(dd)[1])
        {
          if(i == 1){dd$distance[i] = 0} else
          {
            #distance traveled since the last touch
            dd$distance[i] = sqrt((dd$touch_x[i] - dd$touch_x[i-1])^2 + (dd$touch_y[i] - dd$touch_y[i-1])^2)
          }
          # distance to the target and to the endpoint of this movement
          dd$dist_from_target[i] <- sqrt((dd$touch_x[i] - this_xend)^2 + (dd$touch_y[i] - this_yend)^2)
          dd$dist_from_end[i] <- sqrt((dd$touch_x[i] - dd$touch_x[num_touchsamples])^2 + (dd$touch_y[i] - dd$touch_y[num_touchsamples])^2)
        }

        #Velocity and Acceleration
  #      dd$velocity = dd$distance/dd$delta_time
  #      dd$velocity[is.na(dd$velocity)] <- 0
  #      dd$acceleration = dd$velocity/dd$delta_time
  #      dd$acceleration[is.na(dd$acceleration)] <- 0
        dd$travel <- cumsum(dd$distance)
        dd$travel_velocity <- c(signal::filtfilt(resources$deriv_filter, diff(dd$travel)/dd$delta_time[2:numt]),0)
        dd$travel_acceleration <- c(signal::filtfilt(resources$deriv_filter, diff(dd$travel_velocity)/dd$delta_time[2:numt]),0)
        dd$travel_jerk <- c(signal::filtfilt(resources$deriv_filter, diff(dd$travel_acceleration)/dd$delta_time[2:numt]),0)
  #      dd$distance_velocity[(numt-1):numt] <- NA
  #      dd$distance_velocity[(numt-2):numt] <- NA

        dd$travel_velocity_raw <- c(diff(dd$travel)/dd$delta_time[2:numt],0)
        dd$travel_acceleration_raw <- c(diff(dd$travel_velocity_raw)/dd$delta_time[2:numt],0)
        dd$travel_jerk_raw <- c(diff(dd$travel_acceleration_raw)/dd$delta_time[2:numt],0)



        #how far off the current time point is from where it would be on a
        #smooth continuous movement to the target.
        dd$residual_smooth_dist_from_end <- dd$dist_from_end - dd$smooth_dist_from_end

        #Filtered residual distance from end. i.e., how far they are from a smooth motion to endpoint
        dd$resid_dist_end_filter <- signal::filtfilt(resources$filter,dd$residual_smooth_dist_from_end)

        dd$resid_dist_end_filter_inches <- dd$resid_dist_end_filter/config$tablet_pixels_per_inch
        sample_milliseconds <- (1/config$tablet_touch_sampling_rate) * 1000
        #velocity in pixels per millisecond
        vel <- diff(dd$resid_dist_end_filter) / sample_milliseconds
        dd$velocity_resid_dist_end_filter <- c(vel,vel[length(vel)])


        each_touch[[sprintf('TOUCH-%s',t_id)]] <- dd
        }
      }
    }
    trial_touches[[sprintf('COND-%s_TRIAL-%s',cond,trial)]] <- each_touch
    trial_touches$summary[[sprintf('COND-%s_TRIAL-%s',cond,trial)]] <- list(trial_start = min(alltouches$time), trial_end = max(alltouches$time))
  }
  }

  return(trial_touches)
}

# Given a single Touch data frame, calculates derivatives for X, Y, and Distance (i.e., distance travelled between successive timepoints)
# OUTDATED -- Velocity, Acceleration, and Jerk are Loess fit of e.g. diff(position), diff(vel_smooth)
# Velocity, Acceleration, and Jerk are filtered of diff(position) etc.
# Outputs pos/vel/acc/jerk, Z-scores of each (no centering, to keep zero-crossings), and scaled to the range 0 to 1.

PROCESS.touch_derivatives <- function(dd)
{
  out <- data.frame(time = dd$time)
  dist = rep(0,(dim(dd)[1]))
  for(i in 1:dim(dd)[1])
  {
    dist[i] = sqrt((dd$touch_x[i] - dd$touch_x[max(1,i-1)])^2 + (dd$touch_y[i] - dd$touch_y[max(1,i-1)])^2)
  }
  dd$touch_dist = cumsum(dist)

  time = out$time
  numt = length(time)

  long <- list()

  inputs = c('x','y','dist')
  for( i in inputs){
    ddlong <- list()
    ddlong$filt <- data.frame(time = rep(dd$time,4))
    ddlong$scalepm1 <- data.frame(time = rep(dd$time,4))
    ddlong$z <- data.frame(time = rep(dd$time,4))
    ddlong$raw <- data.frame(time = rep(dd$time,4))

    out[,sprintf('%s_position',i)] = dd[,sprintf('touch_%s',i)]
    #vel_smooth = predict(loess(vel ~ time, data.frame(time = time[1:(numt-1)], vel = diff(out[,sprintf('%s_position',i)]))))
    #accel_smooth = predict(loess(accel ~ time, data.frame(time = time[1:(numt-2)], accel = diff(vel_smooth))))
    #jerk_smooth = predict(loess(jerk ~ time, data.frame(time = time[1:(numt-3)], jerk = diff(accel_smooth))))
    vel_filt <- signal::filtfilt(resources$deriv_filter, diff(out[,sprintf('%s_position',i)])/dd$delta_time[2:numt]  )
    accel_filt <- signal::filtfilt(resources$deriv_filter, diff(vel_filt)/dd$delta_time[2:(numt-1)])
    jerk_filt <- signal::filtfilt(resources$deriv_filter, diff(accel_filt)/dd$delta_time[2:(numt-1)])




    out[,sprintf('%s_velocity',i)] = c(vel_filt,NA)
    out[,sprintf('%s_acceleration',i)] = c(accel_filt,NA,NA)
    out[,sprintf('%s_jerk',i)] = c(jerk_filt,NA,NA,NA)

    ddlong$filt$value <- c(out[,sprintf('%s_position',i)],
                              out[,sprintf('%s_velocity',i)],
                              out[,sprintf('%s_acceleration',i)],
                              out[,sprintf('%s_jerk',i)])
    ddlong$filt$degree <- rep(c('position','velocity','acceleration','jerk'), each = numt)
    ddlong$filt$type <- 'filtered'

    position_raw <- dd[,sprintf('touch_%s',i)]
    vel_raw <- dd[,sprintf('%s_velocity_raw',i)]
    accel_raw <- dd[,sprintf('%s_acceleration_raw',i)]
    jerk_raw <- dd[,sprintf('%s_jerk_raw',i)]

    ddlong$raw$value <- c(c(position_raw,vel_raw,accel_raw,jerk_raw))
    ddlong$raw$degree <- rep(c('position','velocity','acceleration','jerk'), each = numt)
    ddlong$raw$type <- 'raw'

    out[,sprintf('%s_position_scale_plusminus1',i)] = UTILS.range_plusminus1(out[,sprintf('%s_position',i)], na.rm = TRUE)
    out[,sprintf('%s_velocity_scale_plusminus1',i)] = UTILS.range_plusminus1(out[,sprintf('%s_velocity',i)], na.rm = TRUE)
    out[,sprintf('%s_acceleration_scale_plusminus1',i)] = UTILS.range_plusminus1(out[,sprintf('%s_acceleration',i)], na.rm = TRUE)
    out[,sprintf('%s_jerk_scale_plusminus1',i)] = UTILS.range_plusminus1(out[,sprintf('%s_jerk',i)], na.rm = TRUE)

    ddlong$scalepm1$value <- c(out[,sprintf('%s_position_scale_plusminus1',i)],
                              out[,sprintf('%s_velocity_scale_plusminus1',i)],
                              out[,sprintf('%s_acceleration_scale_plusminus1',i)],
                              out[,sprintf('%s_jerk_scale_plusminus1',i)])
    ddlong$scalepm1$degree <- rep(c('position','velocity','acceleration','jerk'), each = numt)
    ddlong$scalepm1$type <- 'scale_plusminus1'


    out[,sprintf('%s_position_z',i)] = scale(out[,sprintf('%s_position',i)], center = FALSE)
    out[,sprintf('%s_velocity_z',i)] = scale(out[,sprintf('%s_velocity',i)], center = FALSE)
    out[,sprintf('%s_acceleration_z',i)] = scale(out[,sprintf('%s_acceleration',i)], center = FALSE)
    out[,sprintf('%s_jerk_z',i)] = scale(out[,sprintf('%s_jerk',i)], center = FALSE)

    ddlong$z$value <- c(out[,sprintf('%s_position_z',i)],
                              out[,sprintf('%s_velocity_z',i)],
                              out[,sprintf('%s_acceleration_z',i)],
                              out[,sprintf('%s_jerk_z',i)])
    ddlong$z$degree <- rep(c('position','velocity','acceleration','jerk'), each = numt)
    ddlong$z$type <- 'z'

    long[[i]] <- do.call('rbind',ddlong)
    long[[i]]$metric <- i
  }
    alllong <- do.call('rbind',long)


  return(list(wide = out, long = alllong))


}

PROCESS.OnePAccel.Tablet <- function(tablet_dat, config, resources, pid){

  #Determine which trials to process. Default is all of them
  use_trials <- config$process_trials
  if (class(use_trials) == 'character'){
    if(use_trials == 'all'){
      use_trials <- 1:dim(tablet_dat$event)[1]
    }
  }

  trial_times <- c(tablet_dat$event$time[tablet_dat$event$event_marker == 'START'],1000000000)
  xstarts <- tablet_dat$event$start_item_center_x[tablet_dat$event$event_marker == 'START']
  ystarts <- tablet_dat$event$start_item_center_y[tablet_dat$event$event_marker == 'START']
  xends <- tablet_dat$event$end_item_center_x[tablet_dat$event$event_marker == 'START']
  yends <- tablet_dat$event$end_item_center_y[tablet_dat$event$event_marker == 'START']

  trial_accel <- list()
  trial_accel$summary <- list()
  for( trial in use_trials){

    if('condition' %in% names(tablet_dat$event)){
      cond <- tablet_dat$event$condition[trial]
    } else {
      cond <- 'NONE'
    }

    this_xstart <- xstarts[trial]
    this_ystart <- ystarts[trial]
    this_xend <- xends[trial]
    this_yend <- yends[trial]

    this_trial <- subset(tablet_dat$accel, time >= trial_times[trial] & time < trial_times[trial+1] & delta_time >0)
    this_trial$pid <- pid
    this_trial$trial_id <- trial
    this_trial$condition <- cond


  }

}

PROCESS.Accel <- function(){


  library(pracma)

  peaks <- findpeaks(this_trial$accel_z, minpeakdistance = 1, threshold = .06)

  ggplot2::ggplot(this_trial, ggplot2::aes(x = 1:759)) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = peaks[,2]) +
    ggplot2::geom_line(ggplot2::aes(y = accel_z ), color = 'red')


  library(stats)
  ggplot(data = this_trial, aes(x = time)) +
    theme_bw() +
    geom_line(aes(y = accel_x - mean(accel_x)), color = 'red') +
    geom_line(aes(y = accel_y - mean(accel_y)), color = 'green') +
    geom_line(aes(y = accel_z - mean(accel_z)), color = 'blue')


  fs <- 100 #sampling frequency
  n <- floor(nrow(this_trial)/2)*2
  this_trial <- this_trial[1:n,]
  f <- fs / 2 * seq(0, 1, length.out = n / 2 + 1) # frequency vector

  fft_data_x <- fft(this_trial$accel_x) # fft output
  psd_data_x <- (1 / n) * fft_data_x * Conj(fft_data_x) # psd output
  psd_data_x <- psd_data_x[1:(n / 2 + 1)] # keep only positive frequencies

  fft_data_y <- fft(this_trial$accel_y) # fft output
  psd_data_y <- (1 / n) * fft_data_y * Conj(fft_data_y) # psd output
  psd_data_y <- psd_data_y[1:(n / 2 + 1)] # keep only positive frequencies

  fft_data_z <- fft(this_trial$accel_z) # fft output
  psd_data_z <- (1 / n) * fft_data_z * Conj(fft_data_z) # psd output
  psd_data_z <- psd_data_z[1:(n / 2 + 1)] # keep only positive frequencies
  dfft <- data.frame(freq = f, x = Mod(psd_data_x), y = Mod(psd_data_y), z = Mod(psd_data_z))
  dfft <- dfft[2:nrow(dfft),]

  ggplot(data = dfft, aes(x = freq)) + theme_bw() +
    geom_line(aes(y = x), color = 'red') +
    geom_line(aes(y = y), color = 'green') +
    geom_line(aes(y = z), color = 'blue')

  bin_width <- .5
  dfft$fbin <- cut(dfft$freq, breaks = seq(0, max(dfft$freq), by = bin_width), right = FALSE)

  dfft_bin <- aggregate(cbind(x, y, z) ~ fbin, data = dfft, FUN = mean)

  ggplot(data = dfft_bin, aes(x = as.numeric(fbin))) + theme_bw() +
    geom_line(aes(y = x), color = 'red') +
    geom_line(aes(y = y), color = 'green') +
    geom_line(aes(y = z), color = 'blue') +
    geom_point(aes(y = x), color = 'red', size = 2) +
    geom_point(aes(y = y), color = 'green', size = 2) +
    geom_point(aes(y = z), color = 'blue', size = 2)







  }
