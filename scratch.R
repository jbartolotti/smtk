
notafun <- function(){
#R/Bartolotti/Tablet_7sub/XXXX
#get all _touch files
#calc a delta time field
#find trial onsets
#extract the first X samples
#plot all of the delta times.
#get cumsum and plot all of the time post onset

# Create an empty list to store the data frames
df_list <- list()

# Get the names of all the folders in the current directory
folders <- dir()
folders <- folders[!grepl('Maggie',folders)]
folders <- c('9999_1xnull','9999_2xnull','9999_3xnull','9999_4xnull')

# Loop through each folder
for (folder in folders) {
  # Find the file with the suffix _touch.csv in the folder
  file <- list.files(path = folder, pattern = "null_touch\\.csv$", full.names = TRUE)
  # If the file exists, import it into a data frame
  if (length(file) > 0) {
    df <- read.csv(file, skip = 2)
    # Append the data frame to the list
    df_list[[folder]] <- df
  }
}

#for(ff in file){
#df <- read.csv(ff, skip = 2)
## Append the data frame to the list
#df_list[[ff]] <- df
#}

dfshort <- list()
for(pid in names(df_list)){
  if(!('down_time' %in% colnames(df_list[[pid]]))){
    df_list[[pid]]$down_time <- NA
    }
  rownum <- dim(df_list[[pid]])[1]
  df_list[[pid]]$delta_time <- NA
  df_list[[pid]]$delta_time[2:rownum] <- df_list[[pid]]$time[2:rownum] - df_list[[pid]]$time[1:(rownum-1)]

  df_list[[pid]]$pid <- pid
  startrows <- which(trimws(df_list[[pid]]$touch_action) == 'TOUCH_START')

  df_list[[pid]]$index <- NA
  getrows <- as.numeric()
  for(x in startrows){
    ii <- which(startrows == x)
    if(ii == length(startrows)){
      endrow <- rownum
    } else {
      endrow <- (startrows[ii+1]-1)
    }
    df_list[[pid]]$index[x:endrow] <- 0:(length(x:endrow)-1)
    getrows <- c(getrows, (x+1):(x+20))
    }
  dfshort[[pid]] <- subset(df_list[[pid]], index > 0 & index <= 20)
}
dfall <- do.call('rbind',dfshort)

dfall$blocktrial <- paste(dfall$block_number,dfall$trial_number, sep = '_')

ggplot(data = dfall, aes(x = index, y = delta_time, color = factor(block_number), shape = factor(trial_number))) + geom_point()

ggplot(data = dfall, aes(x = index, y = delta_time, color = factor(block_number), shape = factor(trial_number))) + geom_hline(yintercept = (1:5)*8.667) + geom_quasirandom() + facet_wrap(.~pid) + coord_cartesian(ylim = c(0,150))
ggsave('tablet_deltatime_mh.png', width = 12, height = 12)


ggplot(data = dfall, aes(x = index, y = delta_time, color = factor(block_number), shape = factor(trial_number))) + geom_hline(yintercept = (1:5)*8.667) + geom_quasirandom() + scale_color_discrete(name = 'Block') + scale_shape_discrete(name = 'Trial')
ggsave('tablet_deltatime_mh_all.png', width = 8, height = 6)

ggsave('~/R-Drive/Bartolotti_J/tablet_deltatime_all.png',width = 8, height = 6)



}

