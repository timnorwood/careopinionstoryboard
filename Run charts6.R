# Function to produce Run Charts
RunChart = function(measure, subgroup, title, xlabel, ylabel, scale) {
  
  library(ggplot2)
  #library(plyr)
  library(dplyr)
  
  subgroup = factor(subgroup, levels=unique(subgroup)) #Order the x axis values to occurance in data
  dataDF = data.frame(measure,subgroup) #Create dataframe from vector data
  
  uplim = max(dataDF$measure)*1.3 #Set the upper limit of the y axis to a factor of maximun value
  xbreaks = levels(subgroup)[c(T, rep(F, trunc(length(subgroup)/30,0)))]
  
  #This function sets scaler for the y axis (e.g 0.5 to 50%)
  scaler <- function(){
    function(x)x*scale
  }
  
if (length(subgroup) < 12)  {
  title = gsub( "Run ", "Line ", title)
  print(
    ggplot() +
      geom_line(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
      geom_point(aes(x=subgroup, y=measure, group = 1), data=dataDF) +  
      theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
      scale_y_continuous(labels=scaler(), limits=c(max(min(dataDF$measure)*0.66,0), uplim)) +
      xlab(xlabel) + ylab(ylabel) +
      ggtitle(title)
      #ggtitle(bquote(atop(.(title), atop(italic(.("Median not plotted. Run charts should have at least 12 points.")), "")))) 
      #ggtitle(expression(atop(title, atop(bold("Median not plotted. Run charts should have at least 12 points."), ""))))
  )
} else {
  
  shiftpos = 1
  newshiftpos = 1
  chrt = 0
  
  ### Calculate median for first [1:12]
  dataDF$median = median(dataDF$measure[1:12])
  dataDF$baselines = as.numeric(NA) # Create baseline variable but don't calculate until checked for 12 points with no shift
  
  for (loops in 1:12) {
    
    ### Calculate shifts
    #Calculate points above or below median and number each run
    dataDF$abovebelow = 0
    dataDF$abovebelow[dataDF$measure < dataDF$median] = -1
    dataDF$abovebelow[dataDF$measure > dataDF$median] = 1
    dataDF$abovebelow[dataDF$median == 0 & dataDF$measure == 0] = -1   # If the median and value are zero treat as below
    if(ylabel == "Percentage") { 
      dataDF$abovebelow[dataDF$median == 100 & dataDF$measure == 100] = 1 # If median and value are 100% treat as above
      uplim = 100
      }
    
    #When a point is on the median record the last time a point was above/below (with exception of 0 and 100%)
    dataDF$preserveTF = c(TRUE, dataDF$abovebelow[-1L] != 0) # The first row must be preserved if zero or not
    dataDF$preserveTF[c(FALSE,dataDF$median[-1L] != head(dataDF$median, -1))] = TRUE # preserve were median changes i.e. start new baseline
    dataDF$abovebelowpreserved <- dataDF$abovebelow[c(0, which(dataDF$preserveTF))[cumsum(dataDF$preserveTF) + 1]]
    
    #Number the runs above or below the median
    runChange = dataDF$abovebelowpreserved[-1L] != dataDF$abovebelowpreserved[-nrow(dataDF)] #List change points in data with TRUE
    runChange[dataDF$median[-1L] != dataDF$median[-nrow(dataDF)]] = TRUE #Start new run at rephase points
    dataDF$runNo = c(0,cumsum(runChange)) +1 #Number the runs
      
    # Create data frame with run lengths - excluding points on the center line
    runlengthDF = aggregate(measure ~ runNo, data=dataDF[dataDF$abovebelow != 0,], NROW)
    colnames(runlengthDF) <- c("runNo", "runlength")
    
    # Merge two dataframes - removing any existing runlength column
    # dataDF = merge(dataDF[,!(names(dataDF) %in% c("runlength"))], runlengthDF, all.x =TRUE)
    dataDF = left_join(dataDF[,!(names(dataDF) %in% c("runlength"))], runlengthDF) # using this join/merge instead because merge was reordering
    dataDF$runlength[dataDF$abovebelowpreserved == 0] = 0 # For cases where data starts with a zero
    dataDF$runlength[(dataDF$median == 0) & dataDF$measure == 0] = 0 # For cases where there's a run of zeros on a zero median - so not counted as a shift

    
    # For runs 6 or longer add data to highlights column
    dataDF$highlight = NA
    dataDF$highlight[dataDF$runlength >= 6] = dataDF$measure[dataDF$runlength >= 6]
        
    
    # Calculate the position of the next minimum 6 point shift (returns zero if no more shifts)
    newshiftpos = min(which.max(dataDF$runlength >= 6 & dataDF$runNo > dataDF$runNo[shiftpos]),nrow(dataDF))
    newsusshiftpos = min(which.max(dataDF$runlength >= 9 & dataDF$runNo > dataDF$runNo[shiftpos]),nrow(dataDF))
    
        # Mark as new baseline if
    # (a) there are 12 data points (not counting blanks or NAs); and
    # (b) the 12 points are stable (i.e. no shifts starting within the 12 points)
    # (c) allow if 12 points start with a shift
    if ((shiftpos+11 <= length(measure[!is.na(measure)])) & ((shiftpos+11 < newshiftpos) | newshiftpos==1))
    {dataDF$baselines[shiftpos:(shiftpos+11)] = dataDF$median[shiftpos:(shiftpos+11)]}
    
    
    chrt = chrt + 1
    
    # Plot with limits reset
    
    print(
      ggplot() +
        geom_line(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
        geom_point(aes(x=subgroup, y=measure, group = 1), data=dataDF) +  
        geom_line(aes(x=subgroup, y=median, group = 1), data=dataDF, linetype = "longdash", colour = "darkgray") +
        geom_line(aes(x=subgroup, y=baselines, group = 1), data=dataDF, linetype = "solid", colour = "darkgray") +
        geom_point(aes(x=subgroup, y=highlight, group = 1), data=dataDF, colour = "red") +   
        theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
        scale_y_continuous(labels=scaler(), limits=c(max(min(dataDF$measure)*0.66,0), uplim)) +
        #scale_x_continuous(breaks=pretty(subgroup, n=30)) +
        scale_x_discrete(breaks = xbreaks) +
        xlab(xlabel) + ylab(ylabel) +
        ggtitle(title)
    ) # ggplot chart
    
    if (newsusshiftpos == 1) break #If newsusshiftpos is zero there are no more sustained shifts and we can stop looping
    
    ### If next shift is more than 9 points, calculate temporary median.
    #if (dataDF$runlength[newshiftpos] >= 9) {
      dataDF$median[newsusshiftpos:nrow(dataDF)] = median(dataDF$measure[newsusshiftpos:min(newsusshiftpos+11,newsusshiftpos+dataDF$runlength[newsusshiftpos]-1, nrow(dataDF))])  # Median
      dataDF$baselines[newsusshiftpos:nrow(dataDF)] = NA
    #}
    
    shiftpos = newsusshiftpos #Make the next sustained shift position the start of the baseline in the next loop
    
} # End of For loop

} # End of 12 points If 

} # End of RunChart function


# measure = slBetween$Between
# subgroup = as.character(slBetween$Date)
# title = paste("Run chart of consecutive positive stories")
# ylabel = "Number of consecutuve +ve stories"
# xlabel = "Date of negative stories"
# scale = 1
# RunChart(o,w, chartTitle, "Number of consecutuve +ve stories", "Date of negative stories", 1)


# #Create dataframe  with some random data
# measure = round(c(rnorm(10, mean=13, sd=2), rnorm(150, mean=10, sd=2),rnorm(10, mean=11, sd=2)),0)
# subgroup = c(1:length(measure))
# RunChart(measure,subgroup,"Title here", "Random data", "Number", 1)
# 
# 
# #Create dataframe using some real data
# realdata = read.csv("H:/Work Related/R/SPC/pData.csv", header=TRUE)
# p = realdata$Percentage
# w = c(1:length(p))
# RunChart(p,w, "Percentage", "Percentage", "Sequence",100)
# 
#  
# realdata = read.csv("H:/Work Related/BR practice/BR Monthly.csv", header=TRUE)
# measure = realdata$GP.EA.paths/realdata$GP.list.size
# subgroup = realdata$Month
# RunChart(measure,subgroup,"Rate of Emergency Admissions at Mearns practice","Rate", "Month", 1000)

# realdata = read.csv("H:/Work Related/BR practice/BR weekly.csv", header=TRUE)
# num = realdata$Number.of.Emergency.Admission.paths
# denom = rep(1,length(num)) # for c chart just make denm 1
# subgroup = realdata$Week.ending # This is the week/month/year label
# ControlChart(num,denom,subgroup,"c","Number of emergency admissions paths for Mearns practice", "Number", "Week", 1)

# #Create dataframe  with some random data
# num = c(rnorm(30, mean=1.1, sd=0.09))
# denom = rnorm(length(num), mean=100, sd=300)
# subgroup = c(1:length(num))
# ControlChart(num,denom,subgroup,"u", "Title", 1000)
