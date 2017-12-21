# Author: James Foster

## Load packages
require(data.table)
require(ggplot2)
require(grid)

#--------------------------------- Import classroom observation data ---------------------------------
setwd('~/workspace/assett/shared/') #set working directory to folder containing observation .csv files

D1 = read.csv("./data/BERI.csv", header=TRUE); nrow(D1) # import BERI data
D2 = read.csv("./data/COPUS.csv", header=TRUE); nrow(D2) # import COPUS data

# Pull out an individual seat number from BERI, if seatNum > -1
seatNum = -1
if(seatNum > -1) {
  D1 = D1[startsWith(as.character(D1$Event), "9"),]
  print(paste("Analyzing single seat seat number:", seatNum))
  print(paste(nrow(D1), "rows remaining"))
}

# Filter out events that were de-selected, based on their Time.End seconds not being equal to :00 or Time.End minutes being odd (instead of even)
# note: last interval of observation has variable Time.End, and is currently excluded from analysis
# note: Notes also have variable Time.End, and are currently excluded from analysis
D1 = D1[second(strptime(D1$Time.End, format="%H:%M:%S")) == 0 & minute(strptime(D1$Time.End, format="%H:%M:%S"))%%2 == 0,]
D2 = D2[second(strptime(D2$Time.End, format="%H:%M:%S")) == 0 & minute(strptime(D2$Time.End, format="%H:%M:%S"))%%2 == 0,]
nrow(D1)
nrow(D2)

#Format time as string
D1$time = as.character(D1$Time.End) #format time as string
D2$time = as.character(D2$Time.End) #format time as string

# Find common start and end times, and keep only observations which intersect in time
start_time = min(intersect(D1$time, D2$time))
end_time = max(intersect(D1$time, D2$time))
D1 = D1[D1$time>=start_time & D1$time<=end_time,]
D2 = D2[D2$time>=start_time & D2$time<=end_time,]

#Exclude first and last n rows, if specified
exclude_n_start_rows = 0
exclude_n_end_rows = 0
D1 = D1[(exclude_n_start_rows+1):(nrow(D1)-exclude_n_end_rows),]
D2 = D2[(exclude_n_start_rows+1):(nrow(D2)-exclude_n_end_rows),]

nrow(D1)
nrow(D2)

#--------------------------------- Compile Individual Coder Data (BERI) ---------------------------------
code_set = c("D:_Comp","D:_Stud_Int","Distract","E:_Comp","E:_Instr_Int","E:_Stud_Int","Listen","Off","Read","Settle,_Pack","Unresp","Write")
engaged_codes = c("E:_Comp","E:_Instr_Int","E:_Stud_Int","Listen","Read","Write")
disengaged_codes = c("D:_Comp","D:_Stud_Int","Distract","Off","Settle,_Pack","Unresp")

aggregate_data_beri = function(D)
{
  #split Event column into separate Seat and Code columns
  dt = data.table(D)
  event_split = unlist(strsplit(as.character(dt$Event), "-", fixed=T))
  dt$Seat = event_split[seq(1, length(event_split), 2)]
  dt$Code = event_split[seq(2, length(event_split), 2)]
  
  #aggregate count of each code type by time
  dt[,list(codes_per_interval=length(Event)), by=time]
  dt_counts = dt[,list(code_type_count = .N), by=list(time, Code)]
  
  #add in codes that weren't assigned in each time period (setting their count to 0)
  used_codes_by_interval = dt[,list(codes=list(Code)), by=time]
  for(i in 1:nrow(used_codes_by_interval))
  {
    row = used_codes_by_interval[i,]
    unused_codes = setdiff(code_set, row$codes[[1]])
    for(j in 1:length(unused_codes))
    {
      dt_counts = rbind(dt_counts, data.frame(time=row$time, Code=unused_codes[j], code_type_count=0))
    }
  }
  dt_counts = dt_counts[order(time,Code),]

  #mark whether each code indiciates engaged (1) disengaged (0)
  dt_counts$engaged = 0
  dt_counts[dt_counts$Code %in% engaged_codes,]$engaged = 1
  dt_counts[dt_counts$Code %in% disengaged_codes,]$engaged = 0
  
  return(dt_counts)
}
beri = aggregate_data_beri(D1); nrow(D2)

#--------------------------------- Compile Individual Coder Data (COPUS) ---------------------------------
copus_codes = read.csv("./copus_codes.csv", header=T)
aggregate_data_copus = function(D)
{
  #split up events into Instructor vs. Student and Code, and merge with Code_Names from file
  dt = data.table(D)
  event_split = unlist(strsplit(as.character(dt$Event), "-", fixed=T))
  dt$Instructor_Student = event_split[seq(1, length(event_split), 2)]
  dt$Code = event_split[seq(2, length(event_split), 2)]
  #dt[,list(codes_per_interval=length(Event)), by=time]
  dt = merge(dt, copus_codes[,c("Event", "Code_Name")], by=c("Event"), all.x=TRUE)
  dt = dt[order(time),]

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
copus = aggregate_data_copus(D2);  nrow(copus)


#--------------------------------- Create beri line graph ---------------------------------
engaged_counts = beri[engaged==1, list(engaged_count=sum(code_type_count)), by=time]
engaged_counts$Minutes = seq(0,2*(nrow(engaged_counts)-1),2)

subtitle = ""
###### Plot BERI data as a line graph
#theme_set(theme_bw()) # Change the theme to my preference
gg_beri_line = ggplot(aes(x = Minutes, y = engaged_count), data = engaged_counts) + geom_line() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
  ggtitle("Student Engagement by Time", subtitle=subtitle) +
  labs(x="Minutes",y="Engagement") +
  theme(axis.text=element_text(size=12, color="black")) + 
  scale_x_continuous(limits=c(min(engaged_counts$Minutes), max(engaged_counts$Minutes)), expand=c(0,0))
gg_beri_line


###### Plot BERI data as a point graph
#theme_set(theme_bw()) # Change the theme to my preference
gg_beri_point = ggplot(aes(x = Minutes, y = engaged_count), data = engaged_counts) + geom_point() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
  ggtitle("Student Engagement by Time", subtitle=subtitle) +
  labs(x="Minutes",y="Engagement") +
  theme(axis.text=element_text(size=12, color="black")) + 
  scale_x_continuous(limits=c(min(engaged_counts$Minutes), max(engaged_counts$Minutes)), expand=c(0,0))
gg_beri_point

#--------------------------------- Create copus activity by time plot (combined instructor and student) ---------------------------------

#copus$Code_Factor = factor(copus$Code, levels=c("O", "W", "T/Q", "Prd", "OG", "WG", "CG", "Ind", "SP", "WC", "SQ", "AnQ", "L", 
#                                                 "Adm", "1o1", "MG", "CQ", "PQ", "FUp", "D/V", "RtW", "Lec"))#copus$Code_Names = factor(copus$Code_Name, levels=copus_codes$Code_Name, ordered=TRUE)
#copus$Code_Name_Factor = factor(copus$Code_Name, levels=copus_codes$Code_Name)
copus$Instructor_Student_Factor = factor(copus$Instructor_Student, levels=c("Student", "Instructor"))
copus$code_type_count = 1
copus[Instructor_Student=="Instructor", "code_type_count"]=2

gg_copus = (ggplot(data = copus, aes(x = as.factor(Minutes), y = Code_Name, fill=code_type_count)) +
  geom_tile(colour="black") +
  scale_fill_gradient(low="red", high="steelblue") +
  #coord_equal() + 
  facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
  ggtitle("Occurrence of Activity by Time") +
  labs(x = "",y = "") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
  theme(axis.ticks=element_blank()) + 
  theme(axis.text=element_text(size=8, color="black")) + 
  theme(legend.position = "none"))
gg_copus

#Plot BERI line above COPUS heatmap
grid.newpage()
grid.draw(rbind(ggplotGrob(gg_beri_line+ggtitle("")+labs(x="",y="Engagement")), ggplotGrob(gg_copus+ggtitle("")), size = "last"))

#--------------------------------- Plot copus heatmap with beri engaged_counts as the heatmap values ---------------------------------
combined = merge(copus, engaged_counts, by=c("time", "Minutes"), all=TRUE)

#sanity check
stopifnot(nrow(merge(copus, engaged_counts, by=c("Minutes", "time"))) == nrow(merge(copus, engaged_counts, by=c("time"))))

use_color = T
if(use_color) {
  low_color = "beige"
  high_color = "red"
}else{
  low_color = "white"
  high_color = "black"
}

gg_combined = (ggplot(data = combined, aes(x = as.factor(Minutes), y = Code_Name, fill=engaged_count)) +
              geom_tile() + #no boxes around each cell
              #geom_tile(colour="black") + #put boxes around each cell
              scale_fill_gradient(low=low_color, high=high_color, name="Number Engaged") +
              #scale_fill_hue() + #discrete colors instead of continuous range
              facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
              #coord_equal() + #square instead of rectangular cells
              theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
              ggtitle("Occurrence of Activity by Time", subtitle="with Student Engagment") +
              labs(x = "Minutes",y = "") + 
              theme(axis.ticks=element_blank()) + 
              theme(axis.text.x =element_text(size=8, color="black")) + 
              theme(axis.text.y =element_text(size=14, color="black")) + 
              #theme_bw() + #removes background color
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
              theme(legend.position = "bottom"))
gg_combined

#Plot BERI line above COPUS heatmap
grid.newpage()
grid.draw(rbind(ggplotGrob(gg_beri_line+ggtitle("")+labs(x="",y="Engagement")), ggplotGrob(gg_combined+ggtitle("")), size = "last"))
#grid.arrange(gg_beri, gg_combined, ncol = 1, heights = c(1, 2))

#--------------------------------- Activities Plots ---------------------------------

# Plot activity as a percentage of all codes
copus_code_counts = combined[, list(Code_Count=.N, mean_engaged_count=mean(engaged_count)), by=list(Event, Instructor_Student_Factor, Code_Name)]
copus_code_counts$PercentageCodes = 100*copus_code_counts$Code_Count/sum(copus_code_counts$Code_Count)
gg_activities_bar_codes <-(ggplot(data=copus_code_counts, aes(x=reorder(Code_Name, PercentageCodes), y=PercentageCodes, fill=mean_engaged_count)) +
  geom_bar(stat="identity", color="black") +
  scale_fill_gradient(low=low_color,high=high_color, name='Number Engaged (avg)') +
  facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
  theme(legend.position = "bottom")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
  ggtitle("Activity as a Percentage of all Codes", subtitle="with Student Engagment") +
  labs(x="", y="Percentage of All Codes")  +
  geom_text(aes(y = PercentageCodes + 1.5,    # nudge above top of bar
                label = paste0(round(PercentageCodes, digits=0), '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 5) 
gg_activities_bar_codes

# Plot activity as a percentage of total class time
copus_code_counts = combined[, list(Code_Count=.N, mean_engaged_count=mean(engaged_count)), by=list(Event, Instructor_Student_Factor, Code_Name)]
copus_code_counts$Percentage = 100*copus_code_counts$Code_Count/length(unique(combined$Minutes))
gg_activities_bar_time <-(ggplot(data=copus_code_counts, aes(x=reorder(Code_Name, Percentage), y=Percentage, fill=mean_engaged_count)) +
                       geom_bar(stat="identity", color="black") +
                       scale_fill_gradient(low=low_color,high=high_color, name='Number Engaged (avg)') +
                       facet_wrap(~Instructor_Student_Factor, nrow=2, scales="free_y") +
                       theme(legend.position = "bottom")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #removes gridlines
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) +
  ggtitle("Activity as a Percentage of Total Class Time", subtitle="with Student Engagment") +
  labs(x="", y="Percentage of Total Class Time")  +
  geom_text(aes(y = Percentage + 3.5,    # nudge above top of bar
                label = paste0(round(Percentage, digits=0), '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 5) 
gg_activities_bar_time


