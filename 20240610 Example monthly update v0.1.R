
library(tidyverse)
#library(rmarkdown)
#devtools::install_github("jbkunst/d3wordcloud") # To install d3wordcloud from Github (no longer on CRAN)

# Get data from Care Opinion - this can take ~1 hour
GetFrom = "2024-06-01"
source( "20200903 GetStories v3.R")

# Load data
load(file = "data\\2tagFrameSC.rda")
load(file = "data\\2storyFrameSC.rda")
load(file = "data\\2nacFrameSC.rda")
load(file = "data\\2serviceFrameSC.rda") 
serviceFrameSC = serviceFrameSC %>% 
  filter(type == "hospital") %>% 
  rename(name = NACSname)
tagFrameSC = tagFrameSC %>%
  left_join(storyFrameSC %>% distinct(PostID, Date)) %>%
  select(PostID, Date, polarity, tagName, tagGroup, tagClass, tagName_orig)
storyFrameSC = storyFrameSC %>%
  left_join(nacFrameSC, by = ('PostID'))


# Define more generic parameters for storyboard e.g. date ranges
renderHBStoryboard <- function(hdr, label, shpName, OutName) {
  rmarkdown::render("20230601_storyboard_v1.2.Rmd", params = list(
    hdr = hdr,
    label = label,
    shpName = shpName,
    DataFrom = as.Date("2018-01-01"),  
    CloudsFrom = as.Date("2023-05-01"), # update date
    page_groups = c("Surgery","Medicine","Maternity","Emergency","Mental Health","AHP")), 
    output_file =  paste0("output//",OutName))
}

# Storyboard for stories about a NHS board - Change for you organisation
storyIDs = storyFrameSC$PostID[storyFrameSC$NACorg == "SS9"]
storyFrame = storyFrameSC[storyFrameSC$PostID %in% storyIDs,]
tagFrame = tagFrameSC[tagFrameSC$PostID %in% storyIDs,]
renderHBStoryboard(hdr = "Storyboard NHS Lothian", label = "LO", shpName = "Lothian", OutName = 'storyboard_LO.html')

# Storyboard for stories about a hospital - Change for your services
storyIDs = storyFrameSC$PostID[storyFrameSC$NACS %in% "A111H"]
storyFrame = storyFrameSC[storyFrameSC$PostID %in% storyIDs,]
tagFrame = tagFrameSC[tagFrameSC$PostID %in% storyIDs,]
renderHBStoryboard(hdr = "Storyboard Crosshouse", label = "Crosshouse", shpName = "Ayrshire and Arran", OutName = 'storyboard_UHC.html')
