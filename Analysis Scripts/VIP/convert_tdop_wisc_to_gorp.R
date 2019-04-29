library(tidyr)
library(ggplot2)
library(plyr)
library(grid)
library(data.table)

instructor_directory =  '~/Google Drive/ASSETT/VIP Service/BERI & TDOP Data_Visuals_Reports/Spring 2019/Shane Schwikert' ### MODIFY

# standard params
show_subtitles = T
verbose = F
figures_to_pdf = F

project_dir = here::here() # save the project's root directory, in order to load helper files (copus_codes.csv and beri_codes.csv)

# convert TDOP data from Wisconsin's interface to GORP-style row-wise data 


#extract date, course name, instructor, and observer from filename
course = NULL
instructor = NULL
extract_observation_metadata = function(df){
  subs = strsplit(df$filename, '_')[[1]]
  df$filename_long = df$filename
  df$obs_date = subs[1]
  df$course = subs[2]
  df$instructor = subs[3]
  df$observer = subs[4]
  #replace filename with date for display in figure subtitles
  df$filename = df$obs_date
  return(df)
}

run_tdop = TRUE
if(run_tdop){
  tdop_dir = file.path(instructor_directory, "TDOP_WISC")
  tdop_filenames <- list.files(tdop_dir, pattern="*.csv", full.names=TRUE)
  D_tdop = lapply(tdop_filenames, read.csv) # keep each classroom observation in its own df
  D_tdop = lapply(D_tdop, data.table)
  D_tdop = lapply(D_tdop, function(D){D[, !"X"]})  # drop weird "X" column of NAs
  #D_tdop = lapply(D_tdop, extract_observation_metadata)
}


# if(figures_to_pdf){
#   dir.create(file.path(instructor_directory, 'figures'), showWarnings = FALSE) # create figures subfolder if it doesn't already exist
#   plots_filename = paste0('plots_', instructor, '_', course, '.pdf')
#   print('opening pdf file')
#   plots_filepath = file.path(instructor_directory, 'figures', plots_filename)
#   pdf(plots_filepath, width=12, height=11) #output all figures to pdf instead of displaying them in R
# }

# load tdop_codes
tdop_codes = read.csv(file.path(project_dir, 'tdop_codes.csv'), header=T)

# split multi-observation dataframe into different dataframes based on Observation.ID, 
# since Wisconsin's system puts all observations into one file
D_tdop = D_tdop[[1]]
D_tdop = split(D_tdop, f = D_tdop$Observation.ID)


convert_wisconsic_data_to_gorp_data = function(D, start_time, observation_date, course_name){
  
  # insert Time.End column
  intervals = seq(
    from=as.POSIXct(start_time, tz=Sys.timezone(), format="%I:%M:%S %p"),
    #to=as.POSIXct(tail(df$Time.End,1), format="%I:%M:%S %p"),
    by="2 min",
    length.out=nrow(D)
  )
  intervals = strftime(intervals, format="%I:%M:%S %p")
  #D$Time.End = intervals
  D = cbind(Time.End = intervals, D)
  
  D$Time.End = sub("^[0]+", "", as.character(D$Time.End)) 
  
  #observation_date = rep(observation_date, times=nrow(D))
  #D = cbind(Date = observation_date, D)
  
  # Convert wide data format to long data format using gather()
  # The arguments to gather():
  # - data: Data object
  # - key: Name of new key column (made from names of data columns)
  # - value: Name of new value column
  # - ...: Names of source columns that contain values
  # - factor_key: Treat the new key column as a factor (instead of character vector)
  code_column_start = 8 # TDOP codes start in column 7 of the wide (column-wise) data
  D_long = gather(D, key=Code, value=counts, seq(code_column_start,ncol(D)), factor_key=FALSE)
  D_long = D_long[D_long$counts > 0, ] # drop 0 counts
  D_long = D_long[!is.na(D_long$counts), ] # drop NAs
  
  # merge with tdop_codes (do this in vip_analysis script)
  #D_long = merge(D_long, tdop_codes, by='Code')
  D_long$Minutes = (D_long$Interval.ID - 1) * 2
  
  D_long$Date = observation_date
  D_long$Course_Name = course_name
  D_long$Event = D_long$Code
  
  return(D_long)
}

#start_times = list(as.ITime("3:32:00 PM")) # define list of start times for each observation
start_times = list("3:32:00 PM")
observation_dates = list("2019-04-09")
course_names = list("Intro to Cognitive Psychology")
tdop = mapply(convert_wisconsic_data_to_gorp_data, D_tdop, start_times, observation_dates, course_names, SIMPLIFY = FALSE)



write_tdop_to_file = function(D){
  date = D$Date[[1]]
  course_name = D$Course_Name[[1]]
  instructor = D$Insteructor[[1]]
  observer = D$Observation.ID[[1]]
  filename = paste0(date,'_',course_name,'_', instructor,'_', observer, ".csv")
  filepath = file.path(instructor_directory, "TDOP", filename)
  D = subset(D, select = -c(Comments)) # drop comments from file output because they can contain commas messing up .csv format
  write.csv(D[order(D$Time.End),], row.names = F, file= filepath, quote=F)
}

dir.create(file.path(instructor_directory, 'TDOP'), showWarnings = FALSE) # create figures subfolder if it doesn't already exist
lapply(tdop, write_tdop_to_file)







