### HOW TO USE THIS SCRIPT ###
#1. Save the VIP project folder (which contains this script) to your computer
#2. Open the 'VIP.Rproj' file to open this project in R Studio. Do not open the 'vip_analysis.R' script directly, because the script
#   will not be able to find the required supporting .csv files (beri_codes.csv, copus_codes.csv, copul_codes.csv, and tdop_codes.csv).
#3. Change the instructor_directory below to the folder containing the instructor's BERI and/or COPUS observations you want to visualize.
#4. Run the whole script. If there are warnings or errors, make sure you understand what they mean and resolve them. Often resolving the
#   errors requires fixing the Time.End timestamps in the observation data files so that codes are recorded on correct 2-minute intervals.
#5. After resolving any warnings or errors, re-run the whole script.

# Set the instructor_directory variable below to the location of the instructor's data folder on your computer.
# Within the instructor's folder, put BERI and COPUS observation .csv's in sub folders called BERI and COPUS.
#instructor_directory =  '~/Google Drive/ASSETT/VIP Service/BERI & COPUS Data_Visuals_Reports/Spring 2019/Janet Casagrand' ### MODIFY
instructor_directory =  '~/Google Drive/ASSETT/VIP Service/BERI & TDOP Data_Visuals_Reports/Spring 2019/Shane Schwikert' ### MODIFY
#instructor_directory =  '~/Google Drive/ASSETT/VIP Service/BERI & COPUS Data_Visuals_Reports/Spring 2019/Leilani Arthurs' ### MODIFY


# Set parameters (default is figures_to_pdf = TRUE, verbose = FALSE, enforce_completeness = TRUE, x_axis_text_size = 12, y_axis_text_size = 12)
figures_to_pdf = TRUE  # TRUE means send all the figures to a pdf within the instructor_directory specified above. 
                        # FALSE means display the figures as they are created (in the Plots window within R Studio)
verbose = FALSE  # TRUE means print more diagnostic information in the output of the script. 
                 # FALSE means only print the most important information in the output of the script.
enforce_completeness = TRUE # TRUE means all observation files must not be missing data from a time period
                            # FALSE means allow observations to have all data missing from one or more time intervals
x_axis_text_size = 12  # You can change the size of x-axis text in timecourse figures to accomodate longer classes. Default 12 works for most class durations.
y_axis_text_size = 12 # You can change the size of y-axis text in timecourse figures to accomodate longer classes. Default 12 works well.


# COPUS vs. COPUL mapped codes. COPUL re-uses all the existing COPUS code infrastructure, with the only difference 
# being in the copus_codes_file which maps GORP's COPUS codes to COPUL codes as defined in copul_codes.csv.
copus_codes_file = "copus_codes.csv"  # copus_codes.csv means use COPUS codes
                                      # copul_codes.csv means use COPUL codes

# Heatmap and timeline colors and settings
copus_beri_heatmap_low_color = 'beige'; copus_beri_heatmap_high_color = 'red'
beri_heatmap_low_color = '#00BFC4'; beri_heatmap_high_color = '#F8766D'  # low color is for disengaged panel; high color is for engaged panel. 
minimum_heatmap_lowerbound = 10  # can be used to expand or contract heatmap color range. Default of 10 auto-sets range based on the min count of engagment in the data 

# TDOP panel/facet grouping level
facet_factor = 'Category' # Subcategory or Category. Default is Subcategory


#{
#--------------------------------- Project Setup ---------------------------------
# Install and load required packages if not already installed
required_packages = c('data.table', 'ggplot2', 'grid', 'magrittr', 'dplyr', 'plyr', 'here', 'viridis')
if (!require("pacman")) install.packages("pacman")
pacman::p_load(required_packages, character.only=T)

project_dir = here::here() # save the project's root directory, in order to load helper files (copus_codes.csv and beri_codes.csv)

# clear environment
#rm(list=ls()) # unload/detach all variables


#generate_plots = function(instructor_directory, show_subtitles=T, verbose=F, figures_to_pdf=T) { #put the whole script into this function, so it can be called in one line at the end
  #--------------------------------- Import classroom observation data ---------------------------------
  
  show_subtitles = TRUE # parameter that determines whether figure subtitles are shown, default is show_subtitles = TRUE

  #setwd(working_dir_filepath) #set working directory to folder containing the two observers' .csv files
  theme_set(theme_grey(base_size = 18))
  options(warn=1) #print warnings as they arise
  
  #check if BERI and/or COPUS data folders exist and which visuals should be created
  if(dir.exists(file.path(instructor_directory,"BERI")) & dir.exists(file.path(instructor_directory,"COPUS"))){
    protocol = 'BERI_COPUS'
    run_beri = T
    run_copus = T
    run_tdop = F
    message('Creating BERI and COPUS visuals.')
  }else if(dir.exists(file.path(instructor_directory,"BERI")) & dir.exists(file.path(instructor_directory,"TDOP"))){
    protocol = 'BERI_TDOP'
    run_beri = T
    run_copus = F
    run_tdop = T
    message('Creating BERI and TDOP visuals.')
  }else if(dir.exists(file.path(instructor_directory,"BERI"))){
    protocol = 'BERI'
    run_beri = T
    run_copus = F
    run_tdop = F
    message("Creating BERI visuals only. No COPUS folder exists in working directory.")
  }else if(dir.exists(file.path(instructor_directory,"COPUS"))){
    protocol = "COPUS"
    run_beri = F
    run_copus = T
    run_tdop = F
    message("Creating COPUS visuals only. No BERI folder exists in working directory.")
  }else if(dir.exists(file.path(instructor_directory,"TDOP"))){
    protocol = "TDOP"
    run_beri = F
    run_copus = F
    run_tdop = T
    message("Creating TDOP visuals only. No BERI folder exists in working directory.")
  }else{
    protocol = NA
    run_beri = F
    run_copus = F
    run_tdop = F
    stop("No BERI or COPUS data folders exist in working directory. No visuals created.")
  }
  
  #define helper function read_plus to load files with extra filename column
  read_plus = function(flnm, header=T) {
    flnm_short = head(strsplit(tail(strsplit(flnm, "/", fixed=T)[[1]], 1), ".", fixed=T)[[1]], 1)
    read.csv(flnm, header) %>%   # %>% acts like a pipe to send the output of this line into the next line
      mutate(filename = flnm_short)
  }
  
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
  
  
  # COPUS: Load all files from a directory of COPUS observations
  if(run_copus){
    copus_dir = file.path(instructor_directory, "COPUS")
    copus_filenames <- list.files(copus_dir, pattern="*.csv", full.names=TRUE)
    D_copus = lapply(copus_filenames, read_plus) # keep each classroom observation in its own df
    D_copus = lapply(D_copus, extract_observation_metadata)
    instructor = D_copus[[1]]$instructor[1] #pull out instructor name
    course = D_copus[[1]]$course[1] #pull out instructor name
  }
  
  # BERI: Load all files from a directory of BERI observations
  if(run_beri){
    beri_dir = file.path(instructor_directory, "BERI")
    beri_filenames <- list.files(beri_dir, pattern="*.csv", full.names=TRUE)
    D_beri = lapply(beri_filenames, read_plus) # keep each classroom observation in its own df
    D_beri = lapply(D_beri, extract_observation_metadata)
    instructor = D_beri[[1]]$instructor[1] #pull out instructor name
    course = D_beri[[1]]$course[1] #pull out instructor name
  }
  
  # TDOP: Load all files from a directory of TDOP observations
  if(run_tdop){
    tdop_dir = file.path(instructor_directory, "TDOP")
    tdop_filenames <- list.files(tdop_dir, pattern="*.csv", full.names=TRUE)
    D_tdop = lapply(tdop_filenames, read_plus) # keep each classroom observation in its own df
    D_tdop = lapply(D_tdop, extract_observation_metadata)
    instructor = D_tdop[[1]]$instructor[1] #pull out instructor name
    course = D_tdop[[1]]$course[1] #pull out instructor name
  }
  
  
  # Check that dates align between BERI and COPUS observation files
  if(run_beri & run_copus){
    if(length(D_copus) != length(D_beri)) {
      stop('unequal numbers of BERI and COPUS observations')
    }else{
      for(i in 1:length(D_copus)) {
        if(D_copus[[i]]$obs_date[1] != D_beri[[i]]$obs_date[1]){
          stop('BERI and COPUS observation dates do not align')
        }
      }
    }
  }
  
  # Check that dates align between BERI and TDOP observation files
  if(run_beri & run_tdop){
    if(length(D_tdop) != length(D_beri)) {
      stop('unequal numbers of BERI and TDOP observations')
    }else{
      for(i in 1:length(D_tdop)) {
        if(D_tdop[[i]]$obs_date[1] != D_beri[[i]]$obs_date[1]){
          stop('BERI and TDOP observation dates do not align')
        }
      }
    }
  }
  
   # Prepare to output figures to pdf file in figures folder within instructor_directory
   if(figures_to_pdf){
    dir.create(file.path(instructor_directory, 'figures'), showWarnings = FALSE) # create figures subfolder if it doesn't already exist
    plots_filename = paste0('plots_', instructor, '_', course, '.pdf')
    print('opening pdf file')
    plots_filepath = file.path(instructor_directory, 'figures', plots_filename)
    pdf(plots_filepath, width=12, height=11) #output all figures to pdf instead of displaying them in R
   }
  
  # Extract notes and save to file 
  extract_notes = function(df, protocol_name, notes_filepath) {
    notes_mask = startsWith(as.character(df$Event), 'Notes-')
    notes = df[notes_mask, ]
    if(nrow(notes) > 0){
      notes$Event = gsub("Notes-", "", notes$Event) #strip "Notes-" text
      notes$pad = "   "
      #header = paste(protocol_name, 'observer notes recorded on', notes$obs_date[1], '\n')
      header = paste(notes$obs_date[1], protocol_name, 'observer notes\n')
      cat(header, file=notes_filepath, append=TRUE)
      #writeLines(notes[,c("Time.End", "Event")], con=notes_filepath)
      write.table(notes[c("pad", "Time.End", "Event")], row.names = F, col.names = F,file= notes_filepath, quote=F, append=T)
      #close(notes_filename)
      cat('\n', file=notes_filepath, append=TRUE)
    }
    return(df[!notes_mask, ])
  }
  
  # open/create notes file
  dir.create(file.path(instructor_directory, 'figures'), showWarnings = FALSE) # create figures subfolder if it doesn't already exist
  notes_filename = paste0('notes_', instructor, '_', course, '.txt')
  notes_filepath = file.path(instructor_directory, 'figures', notes_filename)
  notes_file = file.create(notes_filepath)
  if(run_copus) D_copus = lapply(D_copus, extract_notes, 'COPUS', notes_filepath)
  if(run_beri) D_beri = lapply(D_beri, extract_notes, 'BERI', notes_filepath)
  if(run_tdop) D_tdop = lapply(D_tdop, extract_notes, 'TDOP', notes_filepath) # extract_notes only works with GORP output
  
  # Filter out events that were de-selected, based on their Time.End seconds not being equal to :00 or Time.End minutes being odd (instead of even)
  # note: last interval of observation has variable Time.End, and is currently excluded from analysis
  # note: Notes also have variable Time.End, and are currently excluded from analysis
  # note: Also filter out any rows that are missing Time.End
  filter_deselected_events = function(df, required_number_of_codes_per_interval, verbose) {
    if(verbose) print(paste("filtering deselected events for file:", df$filename_long[1]))
    if(second(strptime(df$Time.End,format="%I:%M:%S %p"))[1] != 0) {
      stop(paste0("First row is not an even minute multiple in file: ", df$filename_long[1]))
    }
    
    pre = nrow(df)
    df = df[!is.na(df$Time.End),] # remove NA Time.End
    df = df[df$Time.End!="",] # remove blank "" times
    
    # fix time periods where "save observations" was clicked, which gave those rows an abnormal Time.End
    #start_minute = minute(strptime(df$Time.End, format="%I:%M:%S %p"))[1]
    #start_time = min(as.character(df$Time.End))
    #end_time =  max(as.character(df$Time.End))
    #for(i in 0:nrow(df)-1) {
    #  current_minute = (start_minute+i*2)%%60
    #  if(sum(minute(strptime(df$Time.End, format="%I:%M:%S %p"))==current_minute)==0)
    #}
    
    intervals = seq(
      from=as.POSIXct(head(df$Time.End,1), format="%I:%M:%S %p"),
      to=as.POSIXct(tail(df$Time.End,1), format="%I:%M:%S %p"),
      by="2 min"
    )
    for(i in 1:length(intervals)) {
      interval = intervals[i]
      posix_time.ends = as.POSIXct(df$Time.End, format="%I:%M:%S %p")
      if(sum(posix_time.ends==interval)==0) {
        if(enforce_completeness){
          stop(paste(df$filename_long[1],"is missing all datapoints for interval Time.End =", format(interval, format="%H:%M:%S")))
        }
      }else if(sum(posix_time.ends==interval)<required_number_of_codes_per_interval){
        warning(paste(df$filename_long[1],"is missing", as.character(required_number_of_codes_per_interval-sum(posix_time.ends==interval)), "datapoints for interval Time.End =", format(interval, format="%H:%M:%S")))
      }
    }
    #1 for odd minutes
    df = df[second(strptime(df$Time.End, format="%I:%M:%S %p")) == 0 & minute(strptime(df$Time.End, format="%I:%M:%S %p"))%%2 == 0,]
    
    post = nrow(df)
    if(verbose) print(paste("    Removed", pre-post, "rows from", df$filename_long[1]))
    return(df)
  }
  
  if(run_copus) D_copus = lapply(D_copus, filter_deselected_events, required_number_of_codes_per_interval=2, verbose)
  if(run_beri) D_beri = lapply(D_beri, filter_deselected_events, required_number_of_codes_per_interval=10, verbose)
  if(run_tdop) D_tdop = lapply(D_tdop, filter_deselected_events, required_number_of_codes_per_interval=1, verbose)
  
  # Find common start and end times
  filter_nonintersecting_times = function(df_beri, df_copus_or_tdop) {
    df_copus_or_tdop$time = strptime(df_copus_or_tdop$Time.End, format="%I:%M:%S %p")
    df_beri$time = strptime(df_beri$Time.End, format="%I:%M:%S %p")
    #df_copus_or_tdop$time = as.character(df_copus_or_tdop$Time.End) # Format time as string
    #df_beri$time = as.character(df_beri$Time.End) # Format time as string
    start_time = min(intersect(df_copus_or_tdop$time, df_beri$time))
    end_time = max(intersect(df_copus_or_tdop$time, df_beri$time))
    # keep only observations which intersect in time
    df_copus_or_tdop = df_copus_or_tdop[df_copus_or_tdop$time>=start_time & df_copus_or_tdop$time<=end_time,]
    df_beri = df_beri[df_beri$time>=start_time & df_beri$time<=end_time,]
    return(list(df_beri, df_copus_or_tdop))
  }
  
  #Check that BERI and COPUS data have equal observation pairings
  if(run_beri & run_copus){
    if(length(D_copus)==length(D_beri)) {
      D_combined = mapply(filter_nonintersecting_times, D_beri, D_copus)
      D_beri = D_combined[1,]
      D_copus = D_combined[2,]
    }else{
      stop("Error: unequal pairings of copus and beri observations")
    }
  }
  
  #Check that BERI and TDOP data have equal observation pairings
  if(run_beri & run_tdop){
    if(length(D_tdop)==length(D_beri)) {
      D_combined = mapply(filter_nonintersecting_times, D_beri, D_tdop)
      D_beri = D_combined[1,]
      D_tdop = D_combined[2,]
    }else{
      stop("Error: unequal pairings of copus and beri observations")
    }
  }
  
  
  #--------------------------------- Compile Individual Coder Data (BERI) ---------------------------------
  if(run_beri){
    n_seats = 10
    n_codes = 12
    #code_set = c("D:_Comp","D:_Stud_Int","Distract","E:_Comp","E:_Instr_Int","E:_Stud_Int","Listen","Off","Read","Settle,_Pack","Unresp","Write")
    #engaged_codes = c("E:_Comp","E:_Instr_Int","E:_Stud_Int","Listen","Read","Write")
    #disengaged_codes = c("D:_Comp","D:_Stud_Int","Distract","Off","Settle,_Pack","Unresp")
    #beri_codes = read.csv("../../beri_codes.csv", header=T)
    beri_codes = read.csv(file.path(project_dir, 'beri_codes.csv'), header=T)
    code_set = beri_codes$Code
    engaged_codes = beri_codes[beri_codes$Engaged==1, "Code"]
    disengaged_codes = beri_codes[beri_codes$Engaged==0, "Code"]
    
    aggregate_data_beri = function(D)
    {
      print(paste("Compiling BERI data from file:", D$filename_long[1]))
      fn = D$filename[1]
      D$time = as.character(D$Time.End)
      dt = data.table(D)
      event_split = unlist(strsplit(as.character(dt$Event), "-", fixed=T))
      dt$Seat = event_split[seq(1, length(event_split), 2)]
      dt$Code = event_split[seq(2, length(event_split), 2)]
      
      dt[,list(codes_per_interval=length(Event)), by=time]
      dt_counts = dt[,list(code_type_count = .N), by=list(time, Code)]
      dt_counts$filename = D$filename[1]
      
      #need to add in codes that weren't assigned in each time period
      used_codes_by_interval = dt[,list(codes=list(Code)), by=time]
      
      for(i in 1:nrow(used_codes_by_interval))
      {
        row = used_codes_by_interval[i,]
        unused_codes = setdiff(code_set, row$codes[[1]])
        for(j in 1:length(unused_codes))
        {
          dt_counts = rbind(dt_counts, data.frame(time=row$time, Code=unused_codes[j], code_type_count=0, filename=fn))
        }
      }
      dt_counts = dt_counts[order(time,Code),]
      
      #dt_counts[1:100,]
      #nrow(dt_counts)
      
      #mark whether each code indiciates engaged (1) disengaged (0)
      dt_counts$engaged = 0
      dt_counts[dt_counts$Code %in% engaged_codes,]$engaged = 1
      dt_counts[dt_counts$Code %in% disengaged_codes,]$engaged = 0
      
      return(dt_counts)
    }
    beri = lapply(D_beri, aggregate_data_beri)
  }
  
  #--------------------------------- Compile Individual Coder Data (COPUS) ---------------------------------
  if(run_copus){
    copus_codes = read.csv(file.path(project_dir, copus_codes_file), header=T)
    aggregate_data_copus = function(D)
    {
      print(paste("Compiling COPUS data from file:", D$filename_long[1]))
      D$time = as.character(D$Time.End)
      #split up events into Instructor vs. Student and Code, and merge with Code_Names from file
      dt = data.table(D)
      event_split = unlist(strsplit(as.character(dt$Event), "-", fixed=T))
      dt$Instructor_Student = event_split[seq(1, length(event_split), 2)]
      dt$Code = event_split[seq(2, length(event_split), 2)]
      #dt[,list(codes_per_interval=length(Event)), by=time]
      dt = merge(dt, copus_codes[,c("Event", "Code_Name")], by=c("Event"), all.x=TRUE)
      dt = dt[order(as.POSIXct(dt$time, format="%I:%M:%S %p")),]
      if(nrow(dt[is.na(dt$Code_Name)]) > 0){
        stop(paste("Unknown Codes in file", D$filename[1], ":", dt[is.na(dt$Code_Name),]$Code, "\n"))
        print(dt[is.na(dt$Code_Name)])
      }
      
      #convert copus time sequence to minutes numbering (relies on consecutive time sequences)
      dt$Minutes = 0
      for(i in 2:nrow(dt)) {
        if(dt[i,"Time.End"] == dt[i-1,"Time.End"]) {
          dt[i,"Minutes"] = dt[i-1,"Minutes"]
        }else{
          dt[i,"Minutes"] = dt[i-1,"Minutes"]+2
        }
      }
      
      return(dt)
    }
    copus = lapply(D_copus, aggregate_data_copus)
  }
  
  #--------------------------------- Compile Individual Coder Data (TDOP) ---------------------------------
  if(run_tdop){
    tdop_codes = read.csv(file.path(project_dir, "tdop_codes.csv"), header=T)
    aggregate_tdop_data = function(D){
      dt = D
      #rows_before_merge = nrow(dt)
      dt = merge(dt, tdop_codes, by='Code', all.x=TRUE)
      #rows_after_merge = nrow(dt)
      
      #if(rows_after_merge != rows_before_merge){
      #  stop(paste("Unknown Codes in file", dt$filename_long[1], "\n"))
      #}
      
      
      dt$time = as.character(dt$Time.End)
      #convert tdop time sequence to minutes numbering (relies on consecutive time sequences)
      dt = dt[order(as.POSIXct(dt$time, format="%I:%M:%S %p")),]
      dt$Minutes = 0
      for(i in 2:nrow(dt)) {
        if(dt[i,"Time.End"] == dt[i-1,"Time.End"]) {
          dt[i,"Minutes"] = dt[i-1,"Minutes"]
        }else{
          dt[i,"Minutes"] = dt[i-1,"Minutes"]+2
        }
      }
      
      if(nrow(dt[is.na(dt$Code_Name),]) > 0){
        stop(paste("Unknown Codes in file", dt$filename_long[1], ":", dt[is.na(dt$Code_Name),]$Code, "\n"))
        print(dt[is.na(dt$Code_Name)])
      }
      
      return(dt)
    } 
    tdop = lapply(D_tdop, aggregate_tdop_data)
  }
    
  
  #--------------------------------- Create beri line graph ---------------------------------
  if(run_beri){
    plot_beri_linegraph = function(beri) {
      engaged_counts = beri[engaged==1, list(engaged_count=sum(code_type_count)), by=time]
      engaged_counts$Minutes = seq(0,2*(nrow(engaged_counts)-1),2)
      
      subtitle = NULL
      if(show_subtitles){
        subtitle = beri$filename[1]
      }
      
      #theme_set(theme_bw()) # Change the theme to my preference
      gg_beri = ggplot(aes(x = Minutes, y = engaged_count), data = engaged_counts) + geom_line() +
        #ggtitle("Count of Engaged Students (BERI)") +
        #labs(x="Minutes",y="Number of Engaged Students")
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
        ggtitle("Student Engagement by Time", subtitle=subtitle) +
        labs(x="Minutes",y="Engagement") +
        scale_x_continuous(limits=c(min(engaged_counts$Minutes), max(engaged_counts$Minutes)), expand=c(0,0))
      return(gg_beri)
    }
    gg_beri = lapply(beri, plot_beri_linegraph)
    gg_beri #plot each classroom observation
    #i = 1; gg_beri[[i]] #plot the i'th particular classroom observation
    
    
    # Create combined BERI line graphs for each observation overlayed in the same figure
    plot_beri_combined_linegraphs = function(beri) {
      # compile across time
      engaged_counts = vector("list", length = length(beri))
      subtitle = ''
      max_minutes = 0
      Observation_Num = 1:length(beri)
      for(i in 1:length(beri)){
        engaged_counts[[i]] = beri[[i]][engaged==1, list(engaged_count=sum(code_type_count)), by=time]
        engaged_counts[[i]]$Minutes = seq(0,2*(nrow(engaged_counts[[i]])-1),2)
        engaged_counts[[i]]$filename = beri[[i]]$filename[1]
        engaged_counts[[i]]$Observation = i
        subtitle = paste0(subtitle, '\n', beri[[i]]$filename[1])
        max_minutes = max(max_minutes, max(engaged_counts[[i]]$Minutes))
      }
      
      title = "Student Engagement by Time"
      cex.main = 1
      if(show_subtitles){
        title = paste0(title, subtitle)
        cex.main = .75
      }
      
      ymin = 0
      ymax = 10
      xmin = 0
      xmax = max_minutes
      xlab="Minutes"
      ylab = "Engagement"
      if(length(beri) <= 8){
        cols = palette()[c(1,2,4,6,7,8,5,3)] #c("black", "red", "blue")p
      }else{
        cols = rainbow(length(beri))
      }
      
      plot(engaged_counts[[1]]$Minutes, engaged_counts[[1]]$engaged_count, xlim=c(xmin, xmax), ylim=c(ymin,ymax), type='l', col=cols[1], xlab=xlab, ylab=ylab, main=title, cex.main=cex.main)#, sub=subtitle)
      if(length(beri) > 1){
        for(i in 2:length(beri)){
          points(engaged_counts[[i]]$Minutes, engaged_counts[[i]]$engaged_count, type='l', col=cols[i])
        }
      }
      legend(x=.75, y=2,legend=Observation_Num, title='Observation #',lty=1,col=cols[1:length(beri)], cex=1)
      #return(gg_beri)
    }
    plot_beri_combined_linegraphs(beri)
    
    
    format_data_for_excel_plotting = function(beri){
      #### output data for shane pathway
      engaged_counts = vector("list", length = length(beri))
      subtitle = ''
      max_minutes = 0
      Observation_Num = 1:length(beri)
      for(i in 1:length(beri)){
        engaged_counts[[i]] = beri[[i]][engaged==1, list(engaged_count=sum(code_type_count)), by=time]
        engaged_counts[[i]]$Minutes = seq(0,2*(nrow(engaged_counts[[i]])-1),2)
        engaged_counts[[i]]$filename = beri[[i]]$filename[1]
        engaged_counts[[i]]$Observation = i
        subtitle = paste0(subtitle, '\n', beri[[i]]$filename[1])
        max_minutes = max(max_minutes, max(engaged_counts[[i]]$Minutes))
        
        engaged_count = dcast(engaged_counts[[i]], filename+Observation ~ Minutes, value.var = "engaged_count")
        write.csv(engaged_count, row.names = F, file=paste0('./data_wide/',i,'_engaged_counts', beri_i_wide_minutes$filename[1], ".csv"))
      }
      
      
      for(i in 1:length(beri)){
        engaged_count = engaged_counts[[i]]
        # format and save data for shane
        #reshape(engaged_count, idvar="filename", timevar="Minutes", direction="wide")
        dcast(engaged_count, filename+Observation ~ Minutes, value.var = "engaged_count")
        
        beri_i = beri[[i]]
        minutes_df = data.frame(time = levels(beri_i$time), Minutes=seq(0, length.out=length(levels(beri_i$time)), by=2))
        beri_i = merge(beri_i, minutes_df, by='time')
        beri_i_wide_minutes = dcast(beri_i, filename + Code ~ Minutes, value.var = "code_type_count")
        beri_i_wide_time = dcast(beri_i, filename + Code ~ time, value.var = "code_type_count")
        
        write.csv(beri_i_wide_minutes, row.names = F, file=paste0('./data_wide/',i,'_minutes_', beri_i_wide_minutes$filename[1], ".csv"))
        write.csv(beri_i_wide_time, row.names = F, file=paste0('./data_wide/',i,'_time_', beri_i_wide_time$filename[1], ".csv"))
      }
    }
    #format_data_for_excel_plotting(beri)
  }
  
  #--------------------------------- Create copus activity by time plot (combined instructor and student) ---------------------------------
  if(run_copus){
    plot_copus_timecourse = function(copus) {
      #copus$Code_Factor = factor(copus$Code, levels=c("O", "W", "T/Q", "Prd", "OG", "WG", "CG", "Ind", "SP", "WC", "SQ", "AnQ", "L",
      #                                                 "Adm", "1o1", "MG", "CQ", "PQ", "FUp", "D/V", "RtW", "Lec"))#copus$Code_Names = factor(copus$Code_Name, levels=copus_codes$Code_Name, ordered=TRUE)
      #copus$Code_Name_Factor = factor(copus$Code_Name, levels=copus_codes$Code_Name)
      copus$Instructor_Student_Factor = factor(copus$Instructor_Student, levels=c("Student", "Instructor"))
      copus$code_type_count = 1
      copus[Instructor_Student=="Instructor", "code_type_count"]=2
      
      subtitle = NULL
      if(show_subtitles){
        subtitle = copus$filename[1]
      }
      
      gg_copus = (ggplot(data = copus, aes(x = as.factor(Minutes), y = Code_Name, fill=code_type_count)) +
                    geom_tile(colour="black") +
                    scale_fill_gradient(low="red", high="steelblue") +
                    #coord_equal() +
                    facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
                    theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
                    ggtitle("Occurrence of Activity by Time", subtitle=subtitle) +
                    labs(x = "Minutes",y = "") +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
                    theme(axis.ticks=element_blank()) +
                    theme(axis.text.x=element_text(size=x_axis_text_size, color="black")) +
                    theme(axis.text.y=element_text(size=y_axis_text_size, color="black")) +
                    theme(legend.position = "none"))
      return(gg_copus)
    }
    gg_copus = lapply(copus, plot_copus_timecourse)
    gg_copus #plot each classroom observation
    #i = 1; gg_copus[[i]] #plot the i'th particular classroom observation
  }
  
  
  #Plot BERI line above COPUS heatmap
  if(run_beri & run_copus){
    plot_beri_and_copus = function(gg_beri, gg_copus) {
      grid.newpage()
      grid.draw(rbind(ggplotGrob(gg_beri), ggplotGrob(gg_copus), size = "last"))
    }
    mapply(plot_beri_and_copus, gg_beri, gg_copus)
  }
  
  
  #--------------------------------- Create tdop activity by time plot ---------------------------------
  if(run_tdop){
    plot_tdop_timecourse = function(tdop, facet_factor="Subcategory", fill_factor="Category") {
                                          #facet_factor can be "Category" or "Subcategory"

      tdop$Category = factor(tdop$Category)
      tdop$Subcategory = factor(tdop$Subcategory)
      
      subtitle = NULL
      if(show_subtitles){
        subtitle = tdop$filename[1]
        #subtitle = "Subcategory labels on top"
      }
      
      tdop$Minutes = as.factor(tdop$Minutes)

      
      gg_tdop = (ggplot(data = tdop, aes_string(x = "Minutes", y = "Short_Code", fill=fill_factor)) +
                   geom_tile(colour="black") +
                   scale_fill_discrete(guide=FALSE) +
                   #coord_equal() +
                   #facet_wrap(~Category, nrow=length(unique(tdop$Category)), scales="free_y") +
                   theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
                   ggtitle("Occurrence of Activity by Time", subtitle=subtitle) +
                   labs(x = "Minutes",y = "") +
                   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
                   theme(axis.ticks=element_blank()) +
                   theme(axis.text=element_text(size=12, color="black")) + #TODO size=12
                   theme(legend.position = "none") +
                   #facet_grid(Category ~ ., scales = "free_y", space = "free_y")) #+
                   facet_wrap(as.formula(paste("~", facet_factor)), ncol=1, scales = "free_y"))
      #theme(strip.placement = "top")
      
      g1 = ggplotGrob(gg_tdop) 
      
      # From 'df', get the number of unique codes  for each Category (facet)'.
      # That is, the number y-breaks in each panel.
      if(facet_factor=='Subcategory'){
        N = dlply(tdop, .(Subcategory), function(x) length(unique(x$Code)))
      }else{
        N = dlply(tdop, .(Category), function(x) length(unique(x$Code)))
      }
      
      # Get the items in the g1 layout corresponding to the panels.
      panels1 <- g1$layout$t[grepl("panel", g1$layout$name)]
      
      # Replace the default panel heights with relative heights
      g1$heights[panels1] <- unit(N, "null")
      
      ## Draw g1
      grid.newpage()
      grid.draw(g1)
      
      #return(g1)
      #return(gg_tdop)
    }
    #lapply(tdop, plot_tdop_timecourse)
    #tdop = D_long
    gg_tdop_timecourse = lapply(tdop, plot_tdop_timecourse, facet_factor=facet_factor)
    #gg_tdop_timecourse
 
  }
  
  #--------------------------------- DEPRECATED: Create BERI code by time plot (combined engaged and disengaged) ---------------------------------
  if(run_beri){
    plot_beri_timecourse = function(beri, use_color=T) {
      beri$Engaged_Factor = "Disengaged"
      beri[beri$engaged==1,]$Engaged_Factor = "Engaged"
      beri$Engaged_Factor = factor(beri$Engaged_Factor, levels=c("Engaged", "Disengaged"))
      
      beri = merge(beri, beri_codes, by='Code')
      beri$Code_Labels = beri$Short_Code_Name
      
      #beri[, list(Minutes = seq(0, length(unique(beri$time)))), by=c('time')]
      minutes_df = data.frame(time = levels(beri$time), Minutes=seq(0, length.out=length(levels(beri$time)), by=2))
      beri = merge(beri, minutes_df, by='time')
      
      subtitle = NULL
      if(show_subtitles){
        subtitle = beri$filename[1]
      }
      
      if(use_color) {
        low_color = beri_heatmap_low_color
        high_color = beri_heatmap_high_color
      }else{
        low_color = "white"
        high_color = "black"
      }
      
      beri = beri[beri$code_type_count>0,] # restrict to non-zero code counts
      gg_beri = (ggplot(data = beri, aes(x = as.factor(Minutes), y = Code_Labels, fill=code_type_count)) +
                   geom_tile(colour="black") +
                   scale_fill_gradient(low=low_color, high=high_color, name="Number of Students") +
                   #coord_equal() +
                   facet_wrap(~Engaged_Factor, nrow=2, scales="free_y") +
                   theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
                   ggtitle("Engaged & Disengaged Behaviors by Time", subtitle=subtitle) +
                   labs(x = "Minutes",y = "") +
                   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
                   theme(axis.ticks=element_blank()) +
                   theme(axis.text.x=element_text(size=x_axis_text_size, color="black")) +
                   theme(axis.text.y=element_text(size=y_axis_text_size, color="black")) +
                   theme(legend.position = "bottom", legend.key.width=unit(3,"cm")))
      gg_beri
      return(gg_beri)
    }
    gg_beri_timecourse = lapply(beri, plot_beri_timecourse)
    #gg_beri_timecourse #plot each classroom observation
    #i = 1; gg_beri_timecourse[[i]] #plot the i'th particular classroom observation
    
    #Plot BERI line above BERI timecourse heatmap
    plot_beri_line_and_timecourse_heatmap = function(gg_beri, gg_beri_timecourse) {
      grid.newpage()
      grid.draw(rbind(ggplotGrob(gg_beri), ggplotGrob(gg_beri_timecourse), size = "last"))
    }
    #mapply(plot_beri_line_and_timecourse_heatmap, gg_beri, gg_beri_timecourse)
  }
  
  #--------------------------------- Create BERI code by time plot (separate plots for engaged vs. disengaged) ---------------------------------
  if(run_beri){
    plot_beri_timecourse = function(beri, use_color=T) {
      beri$Engaged_Factor = "Disengaged"
      beri[beri$engaged==1,]$Engaged_Factor = "Engaged"
      beri$Engaged_Factor = factor(beri$Engaged_Factor, levels=c("Engaged", "Disengaged"))
      
      beri = merge(beri, beri_codes, by='Code')
      beri$Code_Labels = beri$Short_Code_Name
      
      #beri[, list(Minutes = seq(0, length(unique(beri$time)))), by=c('time')]
      minutes_df = data.frame(time = levels(beri$time), Minutes=seq(0, length.out=length(levels(beri$time)), by=2))
      beri = merge(beri, minutes_df, by='time')
      
      subtitle = NULL
      #if(show_subtitles){
      #  subtitle = beri$filename[1]
      #}
      
      #if(use_color) {
      #  low_color = beri_heatmap_low_color
      #  high_color = beri_heatmap_high_color
      #}else{
      #  low_color = "white"
      #  high_color = "black"
      #}
      
      beri = beri[beri$code_type_count>0,] # restrict to non-zero code counts
      beri$Minutes = as.factor(beri$Minutes)
      
      beri_engaged = beri[beri$Engaged==1,]
      gg_beri_engaged = (ggplot(data = beri_engaged, aes(x = Minutes, y = Code_Labels, fill=code_type_count)) +
                   geom_tile(colour="black") +
                   scale_fill_gradient(limits=c(min(beri$code_type_count), max(beri$code_type_count)), low="beige", high=beri_heatmap_high_color, name="Number of Students") +
                   #coord_equal() +
                   facet_wrap(~Engaged_Factor, nrow=2, scales="free_y") +
                   theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
                   ggtitle("Engaged & Disengaged Behaviors by Time", subtitle=subtitle) +
                   labs(x = "",y = "") +
                   #xlim(0,max(beri$Minutes)) +
                   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
                   theme(axis.ticks=element_blank()) +
                   theme(axis.text.x=element_blank()) +
                   theme(axis.text.y=element_text(size=y_axis_text_size, color="black")) +
                   theme(plot.margin=unit(c(1,1,-.4,1), units='cm')) +  #t,r,b,l dimension order
                   #theme(panel.spacing=unit(.1,'lines')) +
                   theme(legend.position = "top", legend.key.width=unit(3,"cm")))
      #gg_beri_engaged
      
      beri_disengaged = beri[beri$Engaged==0,]
      gg_beri_disengaged = (ggplot(data = beri_disengaged, aes(x = Minutes, y = Code_Labels, fill=code_type_count)) +
                           geom_tile(colour="black") +
                           scale_fill_gradient(limits=c(min(beri$code_type_count), max(beri$code_type_count)), low="beige", high=beri_heatmap_low_color, name="Number of Students") +
                           #coord_equal() +
                           facet_wrap(~Engaged_Factor, nrow=2, scales="free_y") +
                           theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
                           #ggtitle("Engaged & Disengaged Behaviors by Time", subtitle=subtitle) +
                           labs(x = "Minutes",y = "") +
                           scale_x_discrete(drop=FALSE) +
                           #xlim(0,max(beri$Minutes)) +
                           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
                           theme(axis.ticks=element_blank()) +
                           theme(axis.text.x=element_text(size=x_axis_text_size, color="black")) +
                           theme(axis.text.y=element_text(size=y_axis_text_size, color="black")) +
                           theme(plot.margin=unit(c(-.4,1,1,1), units='cm')) + 
                           #theme(panel.spacing=unit(.1,'lines')) +
                           theme(legend.position = "bottom", legend.key.width=unit(3,"cm")))
      #gg_beri_disengaged
      
      gg_beri_timecourse = list(gg_beri_engaged, gg_beri_disengaged)
      
      #grid.newpage()
      #grid.draw(rbind(ggplotGrob(gg_beri_engaged), ggplotGrob(gg_beri_disengaged), size = "last"))
      #grid.draw(rbind(gg_beri, ggplotGrob(gg_beri_engaged), ggplotGrob(gg_beri_disengaged), size = "last"))
      #grid.draw(rbind(ggplotGrob(gg_beri[[1]]), ggplotGrob(gg_beri_engaged), ggplotGrob(gg_beri_disengaged), size="last"))
      #grid.draw(rbind(ggplotGrob(gg_beri_engaged), ggplotGrob(gg_beri_disengaged)))
      
      #return(list(gg_beri_engaged, gg_beri_disengaged))
      return(gg_beri_timecourse)
    }
    gg_beri_timecourse = lapply(beri, plot_beri_timecourse)
    #gg_beri_timecourse #plot each classroom observation
    #i = 1; gg_beri_timecourse[[i]] #plot the i'th particular classroom observation
    
    #Plot BERI line above BERI timecourse heatmap
    plot_beri_line_and_timecourse_heatmap = function(gg_beri, gg_beri_timecourse) {
      grid.newpage()
      #grid.draw(rbind(ggplotGrob(gg_beri), ggplotGrob(gg_beri_timecourse[1]), ggplotGrob(gg_beri_timecourse[2]), size = "last"))
      grid.draw(rbind(ggplotGrob(gg_beri), ggplotGrob(gg_beri_timecourse[[1]]), ggplotGrob(gg_beri_timecourse[[2]]), size = "last"))
    }
    mapply(plot_beri_line_and_timecourse_heatmap, gg_beri, gg_beri_timecourse)
  }
  
  #--------------------------------- Plot copus heatmap with beri engaged_counts as the heatmap values ---------------------------------
  if(run_beri & run_copus){
    plot_beri_and_copus_heatmap = function(beri, copus, use_color=T) {
      copus$Instructor_Student_Factor = factor(copus$Instructor_Student, levels=c("Student", "Instructor"))
      copus$code_type_count = 1
      copus[Instructor_Student=="Instructor", "code_type_count"]=2
      
      engaged_counts = beri[engaged==1, list(engaged_count=sum(code_type_count)), by=time]
      engaged_counts$Minutes = seq(0,2*(nrow(engaged_counts)-1),2)
      
      #sanity checks
      if(nrow(merge(copus, engaged_counts, by=c("Minutes", "time"))) != nrow(merge(copus, engaged_counts, by=c("time")))) {
        if(enforce_completeness){
          stop(paste("Either copus or beri data is missing data from a time period. Files:",beri$filename[1], copus$filename[1]))
        }
      }
      
      combined = merge(copus, engaged_counts, by=c("time", "Minutes"), all=TRUE)
      #combined$Minutes = seq(0,2*(nrow(combined)-1),2)
      
      
      #nrow(merge(copus, engaged_counts, by=c("time"), all=TRUE))
      #nrow(copus)
      #nrow(engaged_counts)
      #nrow(combined)
      
      if(use_color) {
        low_color = copus_beri_heatmap_low_color
        high_color = copus_beri_heatmap_high_color
      }else{
        low_color = "white"
        high_color = "black"
      }
      
      subtitle = NULL
      if(show_subtitles){
        #subtitle = paste0( "\n", copus$filename[1], "\n", beri$filename[1])
        subtitle = paste0( "\n", copus$filename[1])
      }
      
      gg_combined = (ggplot(data = combined, aes(x = as.factor(Minutes), y = Code_Name, fill=engaged_count)) +
                       #geom_tile() + #no boxes around each cell
                       geom_tile(colour="black") + #put boxes around each cell
                       scale_fill_gradient(limits=c(min(minimum_heatmap_lowerbound, min(combined$engaged_count)), max(combined$engaged_count)), low=low_color, high=high_color, name="Number Engaged") +
                       #scale_fill_hue() + #discrete colors instead of continuous range
                       facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
                       #coord_equal() + #square instead of rectangular cells
                       theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
                       ggtitle("Occurrence of Activity by Time", subtitle=paste("with Student Engagment", subtitle)) +
                       labs(x = "Minutes",y = "") +
                       theme(axis.ticks=element_blank()) +
                       theme(axis.text.x=element_text(size=x_axis_text_size, color="black")) +
                       theme(axis.text.y=element_text(size=y_axis_text_size, color="black")) +
                       #theme_bw() + #removes background color
                       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
                       theme(legend.position = "bottom", legend.key.width=unit(3,"cm")))
      return(gg_combined)
      
      #Plot BERI line above COPUS heatmap
      #grid.newpage()
      #grid.draw(rbind(ggplotGrob(gg_beri_minimal), ggplotGrob(gg_combined), size = "last"))
    }
    #i=2
    #plot_beri_and_copus_heatmap(beri[[i]], copus[[i]])
    
    gg_combined = vector("list", length = length(beri))
    for(i in 1:length(beri)){
      gg_combined[[i]] = plot_beri_and_copus_heatmap(beri[[i]], copus[[i]])
    }
    
    gg_combined
    for(i in 1:length(beri)) {
      grid.newpage()
      grid.draw(rbind(ggplotGrob(gg_beri[[i]]), ggplotGrob(gg_combined[[i]]), size = "last"))
    }
  }
  
  #--------------------------------- Plot tdop heatmap with beri engaged_counts as the heatmap values ---------------------------------
  if(run_beri & run_tdop){
    plot_beri_and_tdop_heatmap = function(beri, tdop, facet_factor="Subcategory", use_color=T) {
      #copus$Instructor_Student_Factor = factor(copus$Instructor_Student, levels=c("Student", "Instructor"))
      
      engaged_counts = beri[engaged==1, list(engaged_count=sum(code_type_count)), by=time]
      engaged_counts$Minutes = seq(0,2*(nrow(engaged_counts)-1),2)
      
      #sanity checks
      #if(nrow(merge(tdop, engaged_counts, by=c("Minutes", "time"))) != nrow(merge(tdop, engaged_counts, by=c("time")))) {
      #  if(enforce_completeness){
      #    stop(paste("Either tdop or beri data is missing data from a time period. Files:",beri$filename[1], tdop$filename[1]))
      #  }
      #}
      
      #combined = merge(tdop, engaged_counts, by=c("time", "Minutes"), all=TRUE)
      combined = merge(tdop, engaged_counts, by=c("Minutes"), all=TRUE)
      #combined$Minutes = seq(0,2*(nrow(combined)-1),2)
      
    
      if(use_color) {
        low_color = copus_beri_heatmap_low_color
        high_color = copus_beri_heatmap_high_color
      }else{
        low_color = "white"
        high_color = "black"
      }
      
      subtitle = NULL
      if(show_subtitles){
        #subtitle = paste0( "\n", copus$filename[1], "\n", beri$filename[1])
        subtitle = paste0( "\n", tdop$filename[1])
      }
      
      gg_combined = (ggplot(data = combined, aes(x = as.factor(Minutes), y = Short_Code, fill=engaged_count)) +
                       #geom_tile() + #no boxes around each cell
                       geom_tile(colour="black") + #put boxes around each cell
                       scale_fill_gradient(limits=c(min(minimum_heatmap_lowerbound, min(combined$engaged_count)), max(combined$engaged_count)), low=low_color, high=high_color, name="Number Engaged") +
                       #scale_fill_hue() + #discrete colors instead of continuous range
                       facet_wrap(as.formula(paste("~", facet_factor)), ncol=1, scales="free_y") +
                       #coord_equal() + #square instead of rectangular cells
                       theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
                       #ggtitle("Occurrence of Activity by Time", subtitle=paste("with Student Engagment", subtitle)) +
                       ggtitle("") +
                       labs(x = "Minutes",y = "") +
                       theme(axis.ticks=element_blank()) +
                       theme(axis.text.x=element_text(size=x_axis_text_size, color="black")) +
                       theme(axis.text.y=element_text(size=y_axis_text_size, color="black")) +
                       #theme_bw() + #removes background color
                       theme(plot.margin=unit(c(-.5,1,1,1), units='cm')) +  #t,r,b,l dimension order
                       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
                       theme(legend.position = "bottom", legend.key.width=unit(3,"cm")))
      
      
      g1 = ggplotGrob(gg_combined) 
      
      adjust_panel_heights = T
      beri_tdop_panel_heights_factor = 3 # 4 seems to produce good relative heights in beri/tdop combined plot. may need adjustment for other datasets
      if(adjust_panel_heights){
        
        # From 'df', get the number of unique codes  for each Category (facet)'.
        # That is, the number y-breaks in each panel.
        if(facet_factor=='Subcategory'){
          N = dlply(tdop, .(Subcategory), function(x) length(unique(x$Short_Code)))
        }else{
          N = dlply(tdop, .(Category), function(x) length(unique(x$Short_Code)))
        }
          
        # adjust panel heights relative to beri line plot for combined plotting
         N = lapply(N, function(x) x/beri_tdop_panel_heights_factor)
        
        # Get the items in the g1 layout corresponding to the panels.
        panels1 <- g1$layout$t[grepl("panel", g1$layout$name)]
        
        # Replace the default panel heights with relative heights
        g1$heights[panels1] <- unit(N, "null")
      }
      
      return(g1)
      
      
    }
    #i=2
    #plot_beri_and_copus_heatmap(beri[[i]], copus[[i]])
    
    gg_combined = vector("list", length = length(beri))
    for(i in 1:length(beri)){
      gg_combined[[i]] = plot_beri_and_tdop_heatmap(beri[[i]], tdop[[i]], facet_factor=facet_factor)
    }
    
    #gg_combined
    for(i in 1:length(beri)) {
      subtitle = paste0("\n",beri[[i]]$filename[1])
      gg_beri_for_heatmap = gg_beri[[i]] + 
                            labs(x="") + 
                            ggtitle("Occurrence of Activity by Time", subtitle=paste("with Student Engagment", subtitle)) +
                            theme(plot.margin=unit(c(1,1,-.5,1), units='cm')) +  #t,r,b,l dimension order
      grid.newpage()
      #g_beri_tdop_combined = rbind(ggplotGrob(gg_beri_for_heatmap), gg_combined[[i]], size = "last")
      #g_beri_tdop_combined <- set_panel_heights(g_beri_tdop_combined, list(unit(1,"null"), unit(1,"null")))
      
      #g_beri_tdop_combined = arrangeGrob(ggplotGrob(gg_beri_for_heatmap), gg_combined[[i]], ncol=1, heights=unit(c(.5, 2),c("null", "null")))
      #grid.draw(g_beri_tdop_combined)
      #grid.draw(rbind(ggplotGrob(gg_beri_for_heatmap), gg_combined[[i]], size = "last"))
      grid.draw(rbind(ggplotGrob(gg_beri_for_heatmap), gg_combined[[i]], size = "last"))
      
    }
  }
  
  
  
  #--------------------------------- Activities graphs ---------------------------------
  if(run_beri & run_copus){
    # Plot activity as a percentage of total class time
    plot_activities_percentage_time = function(beri, copus, use_color=T) {
      
      if(use_color) {
        low_color = copus_beri_heatmap_low_color
        high_color = copus_beri_heatmap_high_color
      }else{
        low_color = "white"
        high_color = "black"
      }
      
      subtitle = NULL
      if(show_subtitles){
        subtitle = paste0("\n", copus$filename[1], "\n", beri$filename[1])
      }
      
      copus$Instructor_Student_Factor = factor(copus$Instructor_Student, levels=c("Student", "Instructor"))
      copus$code_type_count = 1
      copus[Instructor_Student=="Instructor", "code_type_count"]=2
      engaged_counts = beri[engaged==1, list(engaged_count=sum(code_type_count)), by=time]
      engaged_counts$Minutes = seq(0,2*(nrow(engaged_counts)-1),2)
      #sanity checks
      if(nrow(merge(copus, engaged_counts, by=c("Minutes", "time"))) != nrow(merge(copus, engaged_counts, by=c("time")))) {
        if(enforce_completeness){
          stop(paste("Warning: Either copus or beri data is missing data from a time period: Files:", beri$filename[1], copus$filename[1]))
        }
      }
      combined = merge(copus, engaged_counts, by=c("time", "Minutes"), all=TRUE)
      copus_code_counts = combined[, list(Code_Count=.N, mean_engaged_count=mean(engaged_count)), by=list(Event, Instructor_Student_Factor, Code_Name)]
      copus_code_counts$Percentage = 100*copus_code_counts$Code_Count/length(unique(combined$Minutes))
      
      gg_activities_bar_time <-(ggplot(data=copus_code_counts, aes(x=reorder(Code_Name, Percentage), y=Percentage, fill=mean_engaged_count)) +
                                  geom_bar(stat="identity", color="black") +
                                  scale_fill_gradient(limits=c(min(minimum_heatmap_lowerbound, min(copus_code_counts$mean_engaged_count)), max(copus_code_counts$mean_engaged_count)), low=low_color,high=high_color, name='Number Engaged (avg)') +
                                  facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
                                  theme(legend.position = "bottom", legend.key.width=unit(3,"cm"))) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
        coord_flip() +
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
        ggtitle("Activity as a Percentage of Total Class Time", subtitle=paste("with Student Engagment", subtitle)) +
        labs(x="", y="Percentage of Total Class Time")  +
        geom_text(aes(y = Percentage + 3.5,    # nudge above top of bar
                      label = paste0(round(Percentage, digits=0), '%')),    # prettify
                  position = position_dodge(width = .9),
                  size = 8)
      #return(gg_activities_bar_time)
      gg_activities_bar_time
    }
    
    gg_activities_percentage_time = vector("list", length = length(beri))
    for(i in 1:length(beri)) {
      gg_activities_percentage_time[[i]] = plot_activities_percentage_time(beri[[i]], copus[[i]])
    }
    gg_activities_percentage_time
  }
  
  #--------------------------------- Aggregate Across COPUS Observations ---------------------------------
  if(run_copus){
    # Activity as a Percentage of Class Time (COPUS only)
    aggregate_copus_observations = function(copus) {
      copus$Instructor_Student_Factor = factor(copus$Instructor_Student, levels=c("Student", "Instructor"))
      copus$code_type_count = 1
      copus[Instructor_Student=="Instructor", "code_type_count"]=2
      copus_code_counts = copus[, list(Code_Count=.N), by=list(Event, Instructor_Student_Factor, Code_Name, code_type_count)]
      copus_code_counts$N_TimePeriods = length(unique(copus$Minutes))
      #copus_code_counts$Percentage = 100*copus_code_counts$Code_Count/length(unique(copus$Minutes))
      copus_code_counts$filename = copus$filename[1]
      #if(!is.null(copus$enrollment[1])){
      #  copus_code_counts$enrollment = copus$enrollment[1]
      #}else{
      #  copus_code_counts$enrollment = NA
      #}
      return(copus_code_counts)
    }
    
    plot_copus_activities_percentage_time = function(copus_code_counts) {
      
      subtitle = copus_code_counts$filename[1]
      
      gg_activities_bar <-(ggplot(data=copus_code_counts, aes(x=reorder(Code_Name, Percentage), y=Percentage, fill=code_type_count)) +
                             geom_bar(stat="identity", color="black") +
                             scale_fill_gradient(low="red", high="steelblue") +
                             #scale_fill_gradient(low=low_color,high=high_color, name='Number Engaged (avg)') +
                             facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
                             theme(legend.position = "bottom")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
        coord_flip() +
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
        ggtitle("Activity as a Percentage of Total Class Time", subtitle=subtitle) +
        labs(x="", y="Percentage of Total Class Time")  +
        theme(legend.position = "none") +
        geom_text(aes(y = Percentage + 4.0,    # nudge above top of bar (between 3.5 and 4.0)
                      label = paste0(round(Percentage, digits=0), '%')),    # prettify
                  position = position_dodge(width = .9),
                  size = 8)
      gg_activities_bar
      return(gg_activities_bar)
    }
    
    #plot average across observations. # The following note is outdated, because now all 2-min time periods are weighted equally: Note: Percentages are averaged over number of classroom observations, weighting each observation equally (as opposed to greater weighting for observations with more 2-min time periods)
    copus_code_counts = lapply(copus, aggregate_copus_observations)
    copus_code_counts_bound = rbindlist(copus_code_counts)
    #copus_code_counts_agg = copus_code_counts_bound[, list(Percentage=round(sum(Percentage)/length(copus_code_counts)), 0), by=list(Event, Instructor_Student_Factor, Code_Name, code_type_count)]
    copus_code_counts_agg = copus_code_counts_bound[, list(Code_Count=sum(Code_Count), 0), by=list(Event, Instructor_Student_Factor, Code_Name, code_type_count)]
    copus_code_counts_agg$Percentage = 100*copus_code_counts_agg$Code_Count / sum(unique(copus_code_counts_bound[,c("N_TimePeriods", "filename")])$N_TimePeriods)
    copus_code_counts_agg$filename = paste0('Averaged across ', length(copus_code_counts), ' classroom observations')
    gg_copus_agg = plot_copus_activities_percentage_time(copus_code_counts_agg)
    gg_copus_agg
  }
  
  #--------------------------------- Aggregate Across BERI + COPUS Observations ---------------------------------
  # Activity as a Percentage of Class Time HEATMAP (beri + copus)
  if(run_beri & run_copus){
    aggregate_copus_beri_observations = function(beri, copus) {
      copus$Instructor_Student_Factor = factor(copus$Instructor_Student, levels=c("Student", "Instructor"))
      copus$code_type_count = 1
      copus[Instructor_Student=="Instructor", "code_type_count"]=2
      engaged_counts = beri[engaged==1, list(engaged_count=sum(code_type_count)), by=time]
      engaged_counts$Minutes = seq(0,2*(nrow(engaged_counts)-1),2)
      #sanity checks
      if(nrow(merge(copus, engaged_counts, by=c("Minutes", "time"))) != nrow(merge(copus, engaged_counts, by=c("time")))) {
        if(enforce_completeness){
          stop("Warning: Either copus or beri data is missing data from a time period")
          print(paste("Files:",beri$filename[1], copus$filename[1]))
        }
      }
      copus_beri_combined = merge(copus, engaged_counts, by=c("time", "Minutes"), all=TRUE)
      #copus_code_counts = combined[, list(Code_Count=.N, mean_engaged_count=mean(engaged_count)), by=list(Event, Instructor_Student_Factor, Code_Name)]
      #copus_code_counts$Percentage = 100*copus_code_counts$Code_Count/length(unique(combined$Minutes))
      return(copus_beri_combined)
    }
    
    plot_copus_beri_activities_percentage_time_agg = function(copus_code_counts, use_color=T) {
      if(use_color) {
        low_color = copus_beri_heatmap_low_color
        high_color = copus_beri_heatmap_high_color
      }else{
        low_color = "white"
        high_color = "black"
      }
      
      subtitle = paste0("\n", copus_code_counts$filename[1])
      
      
      
      reorder_percentage <- function(Code_Name, Instructor_Student_Factor, Percentage) {
        #factor(Code_Name, levels = unique(Code_Name[order(Instructor_Student_Factor, Percentage)]))
        factor(Code_Name, levels = unique(Code_Name[order(Percentage)]))
      }
      
      #copus_code_counts = copus_code_counts[order(copus_code_counts$Percentage, copus_code_counts$Instructor_Student_Factor),] #sort by decreasing percentage. But not working
      gg_activities_bar_time <-(ggplot(data=copus_code_counts, aes(x=reorder_percentage(Code_Name, Instructor_Student_Factor, Percentage), y=Percentage, fill=mean_engaged_count)) +
                                  geom_bar(stat="identity", color="black") +
                                  scale_fill_gradient(limits=c(min(minimum_heatmap_lowerbound, min(copus_code_counts$mean_engaged_count)), max(copus_code_counts$mean_engaged_count)), low=low_color,high=high_color, name='Number Engaged (avg)') +
                                  facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
                                  theme(legend.position = "bottom", legend.key.width=unit(3,"cm"))) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
        coord_flip() +
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
        ggtitle("Activity as a Percentage of Total Class Time", subtitle=paste("with Student Engagment", subtitle)) +
        labs(x="", y="Percentage of Total Class Time")  +
        geom_text(aes(y = Percentage + 3.5,    # nudge above top of bar
                      label = paste0(round(Percentage, digits=0), '%')),    # prettify
                  position = position_dodge(width = .9),
                  size = 8)
      #return(gg_activities_bar_time)
      gg_activities_bar_time
      return(gg_activities_bar_time)
    }
    
    copus_beri_combined = vector("list", length = length(beri))
    for(i in 1:length(beri)){
      copus_beri_combined[[i]] = aggregate_copus_beri_observations(beri[[i]], copus[[i]])
      #print(length(names(copus_beri_combined[[i]])))
      #print(names(copus_beri_combined[[i]]))
    }
    copus_beri_combined_bound = rbindlist(copus_beri_combined)
    copus_beri_agg = copus_beri_combined_bound[, list(Code_Count=.N, mean_engaged_count=mean(engaged_count)), by=list(Event, Instructor_Student_Factor, Code_Name)]
    copus_beri_agg$Percentage = 100*copus_beri_agg$Code_Count/nrow(unique(copus_beri_combined_bound[,c("Minutes", "filename")]))
    copus_beri_agg$filename = paste0('Averaged across ', length(copus_beri_combined), ' classroom observations')
    plot_copus_beri_activities_percentage_time_agg(copus_beri_agg)
  }
  #names(copus_beri_combined_bound)
 
  
  #--------------------------------- Aggregate Across BERI Observations ---------------------------------
  # BERI Codes as a Percentage of total beri codes
  if(run_beri){
    aggregate_beri_observations = function(beri) {
      beri$Engaged_Factor = "Disengaged"
      beri[beri$engaged==1,]$Engaged_Factor = "Engaged"
      beri$Engaged_Factor = factor(beri$Engaged_Factor, levels=c("Engaged", "Disengaged"))
      
      beri = merge(beri, beri_codes, by='Code')
      beri$Code_Labels = beri$Short_Code_Name
      
      minutes_df = data.frame(time = levels(beri$time), Minutes=seq(0, length.out=length(levels(beri$time)), by=2))
      beri = merge(beri, minutes_df, by='time')
      return(beri)
    }
    
    plot_beri_codes_percentage_time_agg = function(beri_agg, use_color=T) {
      if(use_color) {
        low_color = beri_heatmap_low_color
        high_color = beri_heatmap_high_color
      }else{
        low_color = "white"
        high_color = "black"
      }
      
      subtitle = NULL
      if(show_subtitles){
        subtitle = beri_agg$filename[1]
      }
      
      
      beri_agg = merge(beri_agg, beri_codes, by='Code')
      beri_agg$Code_Label = beri_agg$Short_Code_Name
      
      reorder_percentage <- function(Code_Label, Percentage) {
        #factor(Code_Name, levels = unique(Code_Name[order(Instructor_Student_Factor, Percentage)]))
        factor(Code_Name, levels = unique(Code_Name[order(Percentage)]))
      }
      
      #copus_code_counts = copus_code_counts[order(copus_code_counts$Percentage, copus_code_counts$Instructor_Student_Factor),] #sort by decreasing percentage. But not working
      gg_activities_bar_beri <-(ggplot(data=beri_agg, aes(x=reorder(Code_Label, Percentage), y=Percentage, fill=Engaged_Factor)) +
                                  geom_bar(stat="identity", color="black") +
                                  #scale_fill_gradient(plow=low_color,high=high_color, name='Number Engaged (avg)') +
                                  facet_wrap(~Engaged_Factor, nrow=2, scales="free_y") +
                                  scale_fill_manual(values = c(beri_heatmap_high_color, beri_heatmap_low_color)) +
                                  theme(legend.position = "none", legend.key.width=unit(3,"cm"))) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
        coord_flip() +
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
        ggtitle("Engaged & Disengaged Behaviors\nas a Percentage of All Behaviors", subtitle=subtitle) +
        labs(x="", y="Percentage of Total BERI Codes")  +
        geom_text(aes(y = Percentage + 3.5,    # nudge above top of bar
                      label = paste0(round(Percentage, digits=1), '%')),    # prettify
                  position = position_dodge(width = .9),
                  size = 8)
      #gg_activities_bar_beri
      return(gg_activities_bar_beri)
    }
    
    beri_combined = vector("list", length = length(beri))
    for(i in 1:length(beri)){
      beri_combined[[i]] = aggregate_beri_observations(beri[[i]])
    }
    beri_combined_bound = rbindlist(beri_combined)
    beri_agg = beri_combined_bound[, list(sum_code_type_count=sum(code_type_count)), by=list(Code, Engaged_Factor)]
    beri_agg$Percentage = 100*beri_agg$sum_code_type_count/sum(beri_agg$sum_code_type_count)
    beri_agg$filename = paste0('Averaged across ', length(beri_combined), ' classroom observations')
    gg_activities_bar_beri = plot_beri_codes_percentage_time_agg(beri_agg, use_color=F)
    gg_activities_bar_beri
  }
  
  #--------------------------------- Aggregate Across TDOP Observations ---------------------------------
  if(run_tdop){
    # Activity as a Percentage of Class Time (TDOP only)
    aggregate_tdop_observations = function(tdop, facet_factor) {
      tdop = data.table(tdop)
      #tdop$Instructor_Student_Factor = factor(tdop$Instructor_Student, levels=c("Student", "Instructor"))
      tdop$code_type_count = 1
      #tdop[Instructor_Student=="Instructor", "code_type_count"]=2
      tdop_code_counts = tdop[, list(Code_Count=.N), by=list(get(facet_factor), Code, Short_Code, Code_Name, code_type_count)]
      tdop_code_counts = setnames(tdop_code_counts, 'get', 'facet_factor') # rename 'get' column
      tdop_code_counts$N_TimePeriods = length(unique(tdop$Minutes))
      #tdop_code_counts$Percentage = 100*tdop_code_counts$Code_Count/length(unique(tdop$Minutes))
      tdop_code_counts$filename = tdop$filename[1]
      #if(!is.null(tdop$enrollment[1])){
      #  tdop_code_counts$enrollment = tdop$enrollment[1]
      #}else{
      #  tdop_code_counts$enrollment = NA
      #}
      return(tdop_code_counts)
    }
    
    plot_tdop_activities_percentage_time = function(tdop_code_counts) {
      
      subtitle = tdop_code_counts$filename[1]
      
      gg_activities_bar <-(ggplot(data=tdop_code_counts, aes(x=reorder(Short_Code, Percentage), y=Percentage, fill=facet_factor)) +
                             geom_bar(stat="identity", color="black") +
                             scale_fill_discrete(guide=FALSE) +
                             #scale_fill_gradient(low="red", high="steelblue") +
                             #scale_fill_gradient(low=low_color,high=high_color, name='Number Engaged (avg)') +
                             facet_wrap(~facet_factor, ncol=1, scales="free_y") +
                             theme(legend.position = "bottom")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
        coord_flip() +
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
        ggtitle("Activity as a Percentage of Total Class Time", subtitle=subtitle) +
        labs(x="", y="Percentage of Total Class Time")  +
        theme(legend.position = "none") +
        geom_text(aes(y = Percentage + 3,    # nudge above top of bar (between about 3.0 and 4.0)
                      label = paste0(round(Percentage, digits=0), '%')),    # prettify
                  position = position_dodge(width = .9),
                  size = 5)
      #gg_activities_bar
      
      g1 = ggplotGrob(gg_activities_bar) 
      
      adjust_panel_heights = T
      if(adjust_panel_heights){
        
        # From 'df', get the number of unique codes  for each Category (facet)'.
        # That is, the number y-breaks in each panel.
        N = dlply(tdop_code_counts, .(facet_factor), function(x) length(unique(x$Short_Code)))
        
        # Get the items in the g1 layout corresponding to the panels.
        panels1 <- g1$layout$t[grepl("panel", g1$layout$name)]
        
        # Replace the default panel heights with relative heights
        g1$heights[panels1] <- unit(N, "null")
      }
      
      #return(g1)
      grid.newpage()
      grid.draw(g1)
    }
    
    #plot average across observations. # The following note is outdated, because now all 2-min time periods are weighted equally: Note: Percentages are averaged over number of classroom observations, weighting each observation equally (as opposed to greater weighting for observations with more 2-min time periods)
    tdop_code_counts = lapply(tdop, aggregate_tdop_observations, facet_factor=facet_factor)
    tdop_code_counts_bound = rbindlist(tdop_code_counts)
    #tdop_code_counts_agg = tdop_code_counts_bound[, list(Percentage=round(sum(Percentage)/length(tdop_code_counts)), 0), by=list(Event, Instructor_Student_Factor, Code_Name, code_type_count)]
    tdop_code_counts_agg = tdop_code_counts_bound[, list(Code_Count=sum(Code_Count), 0), by=list(Code, facet_factor, Short_Code, Code_Name, code_type_count)]
    tdop_code_counts_agg$Percentage = 100*tdop_code_counts_agg$Code_Count / sum(unique(tdop_code_counts_bound[,c("N_TimePeriods", "filename")])$N_TimePeriods)
    tdop_code_counts_agg$filename = paste0('Averaged across ', length(tdop_code_counts), ' classroom observations')
    gg_tdop_agg = plot_tdop_activities_percentage_time(tdop_code_counts_agg)
    #gg_tdop_agg
  }
  
  
  #--------------------------------- Aggregate Across BERI + TDOP Observations ---------------------------------
  if(run_beri & run_tdop){
    # Activity as a Percentage of Class Time HEATMAP (BERI + TDOP only)
    aggregate_tdop_beri_observations = function(tdop, beri) {
      tdop = data.table(tdop)
      #tdop$Instructor_Student_Factor = factor(tdop$Instructor_Student, levels=c("Student", "Instructor"))
      tdop$code_type_count = 1
      #tdop[Instructor_Student=="Instructor", "code_type_count"]=2
      
      engaged_counts = beri[engaged==1, list(engaged_count=sum(code_type_count)), by=time]
      engaged_counts$Minutes = seq(0,2*(nrow(engaged_counts)-1),2)
      
      tdop_beri_combined = merge(tdop, engaged_counts, by=c("Minutes"), all=TRUE)
      #tdop_code_counts = combined[, list(Code_Count=.N, mean_engaged_count=mean(engaged_count)), by=list(Event, Instructor_Student_Factor, Code_Name)]
      #tdop_code_counts$Percentage = 100*tdop_code_counts$Code_Count/length(unique(combined$Minutes))
      return(tdop_beri_combined)
      
      # tdop_code_counts = tdop[, list(Code_Count=.N), by=list(get(facet_factor), Code, Short_Code, Code_Name, code_type_count)]
      # tdop_code_counts = setnames(tdop_code_counts, 'get', 'facet_factor') # rename 'get' column
      # tdop_code_counts$N_TimePeriods = length(unique(tdop$Minutes))
      # #tdop_code_counts$Percentage = 100*tdop_code_counts$Code_Count/length(unique(tdop$Minutes))
      # tdop_code_counts$filename = tdop$filename[1]
      # #if(!is.null(tdop$enrollment[1])){
      # #  tdop_code_counts$enrollment = tdop$enrollment[1]
      # #}else{
      # #  tdop_code_counts$enrollment = NA
      # #}
      # return(tdop_code_counts)
    }
    
    plot_tdop_beri_activities_percentage_time_agg = function(tdop_beri_agg, use_color=T) {
      
      subtitle = tdop_beri_agg$filename[1]
      
      if(use_color) {
        low_color = copus_beri_heatmap_low_color
        high_color = copus_beri_heatmap_high_color
      }else{
        low_color = "white"
        high_color = "black"
      }
      
      gg_activities_bar <-(ggplot(data=tdop_beri_agg, aes(x=reorder(Short_Code, Percentage), y=Percentage, fill=mean_engaged_count)) +
                             geom_bar(stat="identity", color="black") +
                             #scale_fill_discrete(guide=FALSE) +
                             #scale_fill_gradient(low="red", high="steelblue") +
                             #scale_fill_gradient(low=low_color,high=high_color, name='Number Engaged (avg)') +
                             scale_fill_gradient(limits=c(min(minimum_heatmap_lowerbound, min(tdop_beri_agg$mean_engaged_count)), max(tdop_beri_agg$mean_engaged_count)), low=low_color,high=high_color, name='Number Engaged (avg)') +
                             facet_wrap(~facet_factor, ncol=1, scales="free_y") +
                             theme(legend.position = "bottom")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
        coord_flip() +
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
        ggtitle("Activity as a Percentage of Total Class Time", subtitle=subtitle) +
        labs(x="", y="Percentage of Total Class Time")  +
        #theme(legend.position = "none") +
        geom_text(aes(y = Percentage + 3.0,    # nudge above top of bar (between about 3.0 and 4.0)
                      label = paste0(round(Percentage, digits=0), '%')),    # prettify
                  position = position_dodge(width = .9),
                  size = 5)
      #gg_activities_bar
      
      g1 = ggplotGrob(gg_activities_bar) 
      
      adjust_panel_heights = T
      if(adjust_panel_heights){
        
        # From 'df', get the number of unique codes  for each Category (facet)'.
        # That is, the number y-breaks in each panel.
        N = dlply(tdop_beri_agg, .(facet_factor), function(x) length(unique(x$Short_Code)))
        
        # Get the items in the g1 layout corresponding to the panels.
        panels1 <- g1$layout$t[grepl("panel", g1$layout$name)]
        
        # Replace the default panel heights with relative heights
        g1$heights[panels1] <- unit(N, "null")
      }
      
      #return(g1)
      grid.newpage()
      grid.draw(g1)
    }
    
    tdop_beri_combined = mapply(aggregate_tdop_beri_observations, tdop, beri, SIMPLIFY=F)
    tdop_beri_combined_bound = rbindlist(tdop_beri_combined)
    tdop_beri_agg = tdop_beri_combined_bound[, list(Code_Count=.N, mean_engaged_count=mean(engaged_count)), by=list(Short_Code, Code, get(facet_factor), Code_Name)]
    tdop_beri_agg = setnames(tdop_beri_agg, 'get', 'facet_factor') #rename column
    tdop_beri_agg$Percentage = 100*tdop_beri_agg$Code_Count/nrow(unique(tdop_beri_combined_bound[,c("Minutes", "filename")]))
    tdop_beri_agg$filename = paste0('Averaged across ', length(tdop_beri_combined), ' classroom observations')
    plot_tdop_beri_activities_percentage_time_agg(tdop_beri_agg)
    
    # #plot average across observations. # The following note is outdated, because now all 2-min time periods are weighted equally: Note: Percentages are averaged over number of classroom observations, weighting each observation equally (as opposed to greater weighting for observations with more 2-min time periods)
    # tdop_code_counts = lapply(tdop, aggregate_tdop_observations, facet_factor=facet_factor)
    # tdop_code_counts_bound = rbindlist(tdop_code_counts)
    # #tdop_code_counts_agg = tdop_code_counts_bound[, list(Percentage=round(sum(Percentage)/length(tdop_code_counts)), 0), by=list(Event, Instructor_Student_Factor, Code_Name, code_type_count)]
    # tdop_code_counts_agg = tdop_code_counts_bound[, list(Code_Count=sum(Code_Count), 0), by=list(Code, facet_factor, Short_Code, Code_Name, code_type_count)]
    # tdop_code_counts_agg$Percentage = 100*tdop_code_counts_agg$Code_Count / sum(unique(tdop_code_counts_bound[,c("N_TimePeriods", "filename")])$N_TimePeriods)
    # tdop_code_counts_agg$filename = paste0('Averaged across ', length(tdop_code_counts), ' classroom observations')
    # gg_tdop_agg = plot_tdop_activities_percentage_time(tdop_code_counts_agg)
    # #gg_tdop_agg
  }
  
  
  
  
  
  
  if(figures_to_pdf){
    print(paste0('Saving visuals to file: ', plots_filepath))
    dev.off() # save plots to pdf file.
  }
 
#}

#generate_plots(instructor_directory)
#}
