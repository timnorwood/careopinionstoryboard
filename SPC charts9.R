# Function to produce Control Charts
ControlChart = function(num, denom, subgroup, type, title, ylabel, xlabel, scale, start=1) {
  
  library(ggplot2)
  
  subgroup = factor(subgroup, levels=unique(subgroup))
    
  dataDF = data.frame(num,denom,subgroup)
  dataDF$measure = dataDF$num / dataDF$denom
  head(dataDF)
  
  #This function sets scaler for the y axis (e.g 100 changes 0.5 to 50%)
  scaler <- function(){
    function(x)x*scale
  }
  
if (length(subgroup) < 20)  {
  
  uplim = max(dataDF$measure)*1.3
  
  # Plot
  print(
    ggplot() +
      geom_line(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
      geom_point(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
      theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
      scale_y_continuous(labels=scaler(), limits=c(max(min(dataDF$measure)*0.66,0), uplim)) +
      xlab(xlabel) + ylab(ylabel) +
      ggtitle(title)
  ) # ggplot chart
} else {
  
if (type == "p") {
      sdp = function(pct, denom) {
      s = sqrt((pct*(1-pct))/denom)
      return(s)
      } # End of p Function
      uplim = min(max(dataDF$measure)*1.3, 1) # Set y axis limit for chart
} else if (type == "u") {
      sdp = function(measure, denom) {
        s = sqrt(measure/denom)
      return(s)     
      } #End of u Function  
      uplim = max(dataDF$measure)*1.3
} else if (type == "c") {
      sdp = function(count, denom) {
        s = sqrt(count/denom)
      return(s)     
      } #End of c Function  
      uplim = max(dataDF$measure)*1.3
} else if (type == "I") {
      sdp = function(MR, denom) {
        s = sum(MR)/denom
      return(s)     
      } #End of I Function  
      uplim = max(dataDF$measure)*1.3
} else {
  print ("chart type not recognised")  
} #End of If


# Start used to change the position of the first baseline test

  shiftpos = start
  newshiftpos = start
  newsusshiftpos = start
  chrt = 0
  
  ### Calculate mean and standard deviations for first [1:20]
  ### Mean using sum of numerators and denominators
  dataDF$mean = sum(dataDF$num[start:(start+19)]) / sum(dataDF$denom[start:(start+19)]) 
  dataDF$sd = sdp(dataDF$mean, dataDF$denom) 
  dataDF$baselines = as.numeric(NA) # Create baseline variable but don't calculate until checked for 20 points with no shift
  
  if (type == "I") {
    dataDF$MR = abs(c(head(dataDF$num,-1) - tail(dataDF$num,-1),NA))
    dataDF$sd = mean(na.omit(dataDF$MR[start:(start+19)]))
    lim3 = 2.66
    lim2 = 1.77
  } else {
    lim3 = 3
    lim2 = 2
  } 
  
  for (loops in 1:10) {
    
    #Calculate points above or below mean and number each run
    dataDF$abovebelow = 0
    dataDF$abovebelow[dataDF$mean == 0] = -1   # If the mean is zero treat as below
    dataDF$abovebelow[dataDF$measure < dataDF$mean] = -1
    dataDF$abovebelow[dataDF$measure > dataDF$mean] = 1
    keep <- function(x) {
      L <- c(TRUE,x[-1L]!=0) # The first row must be preserved if zero or not
      idx <- c(0, which(L))[cumsum(L) + 1]
      return(x[idx])
    } #function to preserve last non zero number
    dataDF$abovebelowpreserved <- keep(dataDF$abovebelow)    
    runChange = dataDF$abovebelowpreserved[-1L] != dataDF$abovebelowpreserved[-nrow(dataDF)] #List change points in data with TRUE
    runChange[dataDF$mean[-1L] != dataDF$mean[-nrow(dataDF)]] = TRUE #Start new run at rephase points
    dataDF$runNo = c(0,cumsum(runChange)) +1 #Number the runs
    
    # Create data frame with run lengths - excluding points on the center line
    runlengthDF = aggregate(measure ~ runNo, data=dataDF[dataDF$abovebelow != 0,], NROW)
    colnames(runlengthDF) <- c("runNo", "runlength")
    # Merge to one data frame - removeing any existing runlength column
    dataDF = merge(dataDF[,!(names(dataDF) %in% c("runlength"))], runlengthDF, all.x =TRUE)
    dataDF$runlength[dataDF$abovebelowpreserved == 0] = 0 # For cases where data starts with a zero
    dataDF$runlength[dataDF$mean == 0 & dataDF$measure == 0] = 0 # For cases where there's a run of zeros on a zero mean
    
    # Create a column with data for runs 8 or longer only
    dataDF$highlight = NA
    dataDF$highlight[dataDF$runlength >= 8] = dataDF$measure[dataDF$runlength >= 8]
    dataDF$highlight[dataDF$measure > dataDF$mean+(lim3*dataDF$sd)] = dataDF$measure[dataDF$measure > dataDF$mean+(lim3*dataDF$sd)]
    dataDF$highlight[dataDF$measure < dataDF$mean-(lim3*dataDF$sd)] = dataDF$measure[dataDF$measure < dataDF$mean-(lim3*dataDF$sd)]
    

    # Calculate the position of the next minimum 8 point shift (returns zero if no more shifts)
    #newshiftpos = min(which.max(dataDF$runlength >= 8 & dataDF$runNo > dataDF$runNo[shiftpos]),nrow(dataDF))
    newshiftpos = min(which.max(dataDF$runlength >= 8),nrow(dataDF))    
    newsusshiftpos = min(which.max(dataDF$runlength >= 8),nrow(dataDF))
    
    ### Mark as new baseline if:
    # (a) there are at least 20 data points (not counting blanks or NAs); and
    # (b) the 20 points are stable (i.e. no shifts starting within the 20 points or no more at all) 
    #if ((shiftpos+19 <= length(num[!is.na(num)])) & (newshiftpos > shiftpos+19 | newshiftpos==1))
    if (length(!is.na(dataDF$num[shiftpos:nrow(dataDF)])) >= 20 & (newshiftpos > shiftpos+19 | newshiftpos==1)) 
      {dataDF$baselines[shiftpos:(shiftpos+19)] = dataDF$mean[shiftpos:(shiftpos+19)]}
     
    
    # # Mark as new baseline if there are 20 or more NAs in the next 20 points & no shifts
    # if ( (sum(is.na(dataDF$highlight[shiftpos:(shiftpos+19)])) >= 20) & 
    #      (shiftpos+19 <= length(dataDF$highlight))  )
    # {dataDF$baselines[shiftpos:(shiftpos+19)] = dataDF$mean[shiftpos:(shiftpos+19)]}
    
    chrt = chrt + 1
    
    # Plot
    print(
      ggplot() +
        geom_line(aes(x=subgroup, y=mean, group = 1), data=dataDF, linetype = "longdash", colour = "darkgray") +
        geom_line(aes(x=subgroup, y=mean+(lim2*sd), group = 1), data=dataDF, linetype = "longdash", colour = "#f8efef") +
        geom_line(aes(x=subgroup, y=mean+(lim3*sd), group = 1), data=dataDF, linetype = "longdash", colour = "#eee5e6" ) +   
        geom_line(aes(x=subgroup, y=mean-(lim2*sd), group = 1), data=dataDF, linetype = "longdash", colour = "#f8efef") +
        geom_line(aes(x=subgroup, y=mean-(lim3*sd), group = 1), data=dataDF, linetype = "longdash", colour = "#eee5e6") + 
        geom_line(aes(x=subgroup, y=baselines, group = 1), data=dataDF, linetype = "solid", colour = "darkgray") +
        geom_line(aes(x=subgroup, y=baselines+(lim2*sd), group = 1), data=dataDF, linetype = "solid", colour = "#f8efef") +
        geom_line(aes(x=subgroup, y=baselines+(lim3*sd), group = 1), data=dataDF, linetype = "solid", colour = "#eee5e6") +   
        geom_line(aes(x=subgroup, y=baselines-(lim2*sd), group = 1), data=dataDF, linetype = "solid", colour = "#f8efef") +
        geom_line(aes(x=subgroup, y=baselines-(lim3*sd), group = 1), data=dataDF, linetype = "solid", colour = "#eee5e6") + 
        geom_line(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
        geom_point(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
        geom_point(aes(x=subgroup, y=highlight, group = 1), data=dataDF, colour = "red") +   
        theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
        #  scale_x_date(breaks = "1 month") +
        scale_y_continuous(labels=scaler(), limits=c(max(min(dataDF$measure)*0.66,0), uplim)) +
        xlab(xlabel) + ylab(ylabel) +
        ggtitle(title)
    ) # ggplot chart
    

    if (newsusshiftpos == 1) break
    
    ### Calculate temporary mean using 20 num/denom totals from new shift position or end of data set, whichever is first 
    dataDF$mean[newsusshiftpos:nrow(dataDF)] = 
      sum(dataDF$num[newsusshiftpos:min(newsusshiftpos + 19, nrow(dataDF))]) /
        sum(dataDF$denom[newsusshiftpos:min(newsusshiftpos + 19, nrow(dataDF))]) 
    
    ### In cases where the new shift position is the same as the old one, calculate temporary mean from shift points
    ### This stops an endless loop
    ### Mean calculated using shift num/denom totals
    if (shiftpos == newsusshiftpos) {
      dataDF$mean[newsusshiftpos:nrow(dataDF)] = 
        sum(dataDF$num[newsusshiftpos:(newsusshiftpos+dataDF$runlength[newsusshiftpos]-1)]) /
        sum(dataDF$denom[newsusshiftpos:(newsusshiftpos+dataDF$runlength[newsusshiftpos]-1)]) 
    }
    
    dataDF$baselines[newsusshiftpos:nrow(dataDF)] = NA
    
    if (type == "I") {
      #CHECK if this needs to be total numerator /total denominator like mean calc above or if mean of means ok
      dataDF$sd[newsusshiftpos:nrow(dataDF)] = mean(na.omit(dataDF$MR[newsusshiftpos:min(newsusshiftpos+19, nrow(dataDF))]))
      #dataDF$sd[newsusshiftpos:nrow(dataDF)] = mean(na.omit(dataDF$MR[newsusshiftpos:min(newsusshiftpos+19, nrow(dataDF))]))
      }
    
    shiftpos = newsusshiftpos

    
} # End of For loop

} # End of >20 points If 

} # End of ControlChart function


#ControlChart(o,d,w,"p", chartTitle, "Percent", "Month", 100, 1)

# # Example data for issue with shiftpo and newshiftpos being the same and getting in an endless loop
# title = paste("Percentage of rated tags that are positive | ")
# num = WeekFrame$Proportion * WeekFrame$All
# denom = WeekFrame$All
# subgroup = WeekFrame$Month
## num = c(860, 930, 804, 1008, 849, 768, 799, 837, 1012, 1107, 987, 786, 1011, 1117, 1143, 1170, 1178, 931, 1110, 1303, 1250, 1505, 1486, 1048, 1574, 1524, 1705, 1601, 1630, 1454, 1622, 1741, 1616, 1884, 2033, 1467, 1480, 2278, 1663, 855, 1162, 1175, 1176, 1398, 1501, 1731, 1447, 1333, 1715, 1576, 1996, 2083, 2344, 2403, 1948, 2153, 2037, 2095, 2057, 2142, 2080, 1989, 2577, 2547, 2829, 2594, 2347)
## denom = c(1231, 1313, 1104, 1344, 1257, 1171, 1022, 1184, 1443, 1501, 1338, 1064, 1386, 1486, 1492, 1465, 1515, 1174, 1460, 1705, 1618, 1935, 1882, 1348, 2040, 1814, 2187, 2177, 2090, 1938, 2129, 2192, 2019, 2291, 2618, 1835, 1868, 2780, 1976, 1003, 1396, 1494, 1506, 1952, 1995, 2301, 1970, 1811, 2286, 2030, 2488, 2571, 3138, 3174, 2562, 2883, 2715, 2623, 2649, 2644, 2688, 2633, 3357, 3169, 3623, 3430, 2979)
## subgroup = c('2017-01', '2017-02', '2017-03', '2017-04', '2017-05', '2017-06', '2017-07', '2017-08', '2017-09', '2017-10', '2017-11', '2017-12', '2018-01', '2018-02', '2018-03', '2018-04', '2018-05', '2018-06', '2018-07', '2018-08', '2018-09', '2018-10', '2018-11', '2018-12', '2019-01', '2019-02', '2019-03', '2019-04', '2019-05', '2019-06', '2019-07', '2019-08', '2019-09', '2019-10', '2019-11', '2019-12', '2020-01', '2020-02', '2020-03', '2020-04', '2020-05', '2020-06', '2020-07', '2020-08', '2020-09', '2020-10', '2020-11', '2020-12', '2021-01', '2021-02', '2021-03', '2021-04', '2021-05', '2021-06', '2021-07', '2021-08', '2021-09', '2021-10', '2021-11', '2021-12', '2022-01', '2022-02', '2022-03', '2022-04', '2022-05', '2022-06', '2022-07')
# type = "p"
# xlabel = "x label"
# ylabel = "y label"
# start=1
# scale = 1
# ControlChart(num,denom,subgroup,"p", title, "Percent", "Month", 100, 1)

# # Create dataframe  with some random data
# num = c(rnorm(10, mean=15.1, sd=4.01), rnorm(20, mean=14.0, sd=3.1),rnorm(10, mean=20.5, sd=5.03))
# denom = rnorm(length(num), mean=10000, sd=500)
# subgroup = c(1:length(num))
# type = "u"
# xlabel = "x label"
# ylabel = "y label"
# scale = 1
# title = "Titeloi!"
# ControlChart(num,denom,subgroup,"u","Random data", "Number", "point", 1,1)

# chartTitle = paste("Percentage of rated tags that are positive | TEST")
# num = (WeekFrame$Proportion * WeekFrame$All)
# denom = WeekFrame$All
# subgroup = WeekFrame$Month
# type = "p"
# xlabel = "Percent"
# ylabel = "Month"
# scale = 100
# ControlChart(num,denom,subgroup,"p", chartTitle, "Percent", "Month", 100, 11)
# 
# library(plotly)
# ggplotly()

#Create dataframe using some real data
# realdata = read.csv("H:/Work Related/R/SPC/pData.csv", header=TRUE)
# p = realdata$Percentage
# d = realdata$Denominator
# o = realdata$Denominator * realdata$Percentage
# w = c(1:length(p))
# ControlChart(o,d,w,"p", "Percentage", "Percentage", "Sequence",100)

# #Create dataframe  with some random data
# num = c(rnorm(30, mean=1.1, sd=0.09))
# denom = rnorm(length(num), mean=100, sd=300)
# subgroup = c(1:length(num))
# ControlChart(num,denom,subgroup,"u", "Title", 1000)
