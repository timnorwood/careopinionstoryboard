### This is a wrapper function for the run chart code
### It plots either a monthly/weekly chart or cases between chart depending on the data inputted

SelectRunChart = function(sFrame, tFrame, label) {

if (nrow(sFrame) ==0 | nrow(tFrame) == 0) { return(print("No stories")) }
  
# Aggregate to monthly/weekly
WeekFrame = aggregate(cbind(tFrame$positive, tFrame$posneg) ~ tFrame$YearMonth, FUN = sum)
colnames(WeekFrame) = c("Month", "Positive","All")

if ((nrow(WeekFrame) >= 10) & (mean(tail(WeekFrame$All,12)) >=20)) { 
# if number of periods are 10 or more and each period is based on an average sample size of last 12 months/weeks 
# are at least 20, plot a run chart. 
  
  WeekFrame$Proportion = WeekFrame$Positive/WeekFrame$All
  WeekFrame = WeekFrame[WeekFrame$All != 0,] # Remove months with no positive or negative stories causing NAs
  
  chartTitle = paste("Run chart of percentage of rated tags that are positive | ", label , sep = "") ###Update and save for new analysis
  o = WeekFrame$Proportion * 100
  w = WeekFrame$Month
  RunChart(o,w, chartTitle, "Month","Percentage", 1)
  
  g = ggplot2::last_plot()
  gg = plotly_build(g)
  if (length(gg$x$data) == 5) {gg$x$data[[2]] = NULL} # a fix for random additional data in gg
  gg$x$data[[1]]$mode = "markers+lines"
  if (length(gg$x$data) > 1) {
    for (count in c(2:3)){gg$x$data[[count]]$mode = "lines"}
    gg$x$data[[4]]$mode = "markers"
  }
  
  # for (count in c(1:12)){
  #   #gg$x$data[[count]]$text <- ""
  #   gg$x$data[[count]]$text <- paste("Month:",w,"<br>Percentage:",round((gg$x$data[[count]]$y)*100,1),"<br>Number tags:",d)
  # }
  
} else if ((nrow(WeekFrame) >= 10) & (mean(tail(WeekFrame$All,12)) < 20)){ 
# if number of periods are 10 or more and each period is based on an average sample size of less than 20, plot a cases between chart.
# In other words the data has been collected for a while but the volume each period isn't big enough.
  
  slFrame0 = sFrame
  # Aggregate slFrame to unique story - do this to have unique story/NACSgroup rather than story/NACS/postcode/NACSgroup
  # also important for sorting by date
  slFrame = aggregate(cbind(PostID) ~ PostID + NACSgroup + tagPolSum + Date, data = slFrame0, FUN = NROW)
  colnames(slFrame) = c("PostID", "NACSgroup", "tagPolSum", "Date","NACSCount")
  
  # Create a cases-between data frame - a list of the number of cases between negative stories
  # Number each run from each negative story
  slFrame$negative = ifelse(slFrame$tagPolSum < 0 , c(1), c(0)) # 1 for between negative stories o for everything else
  slFrame$runNo = (c(0,cumsum(slFrame$negative)) +1)[-(length(slFrame$negative)+1)] # Number the runs from negative stories
  slFrame$Between = abs(slFrame$negative -1)
  slFrame$Date = as.Date(slFrame$Date, format = "%Y-%m-%d") # change Date from factor to date
  # Aggregate number of stories in each period 
  slB1 = aggregate(Date ~ runNo, data = slFrame, FUN=function(x) c(n = length(x), mx = max(x)))
  slB1 = as.data.frame(as.list(slB1)) # this step is needed due to a bug
  colnames(slB1) = c("RunNo","RunLength", "Date")
  slB1$Date = as.Date(slB1$Date, format = "%Y-%m-%d", origin = "1970-01-01") # change Date from factor to date
  slB1$RunLength = slB1$RunLength - 1
  
  slB2 = aggregate(Between ~ runNo, data = slFrame, FUN=sum)
  slBetween = merge(slB1,slB2, by.x="RunNo", by.y="runNo", all.x=TRUE)
  
  chartTitle = paste("Run chart of consecutive positive stories | ", label, sep = "")
  o = slBetween$Between
  w = as.character(slBetween$Date)
  RunChart(o,w, chartTitle, "Date of negative stories","Number of consecutuve +ve stories", 1)
  
  g = ggplot2::last_plot()
  gg = plotly_build(g)
  if (length(gg$x$data) == 5) {gg$x$data[[2]] = NULL} # a fix for random additional data in gg
  gg$x$data[[1]]$mode = "markers+lines"
  if (length(gg$x$data) > 1) {
    for (count in c(2:3)){gg$x$data[[count]]$mode = "lines"}
    gg$x$data[[4]]$mode = "markers"
  }
  
} else {
# In any other case plot a monthly/weekly line chart
  gg = print("Not enough data")
}

return(gg)
}  # End of function


#SelectRunChart(storyFrame, tagFrame, "PEOLCCC" )
