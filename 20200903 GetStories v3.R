
## Thanks to Chris Beeley for his code for using both API 1 and 2.
## The code gets stories and tagged information by using Care Opinions APIs. 
## See discussion at https://chrisbeeley.net/?p=904 about why API2 needs loops 

#GetFrom = "2024-05-01"

library(tidyverse)
library(readxl)
library(httr)    # for GET(...)

API2key = Sys.getenv("API2key") # This is picked up from a .Renviron file in Documents
#usethis::edit_r_environ() # Use this to edit .Renviron from Rstudio

################ Get new story data using API 2 ########################

# produce empty list to lappend
opinionList = list()

# set skip at 0 and make continue TRUE until no stories are returned 
skip = 0
continue = TRUE

while(continue){
  cat('\r',"Getting stories",skip,"to",skip+99,"from Care Opinion")  
  opinions = GET(paste0(
    "https://www.careopinion.org.uk/api/v2/opinions?submittedonafter=",GetFrom,"&take=100&skip=",
    #"https://www.careopinion.org.uk/api/v2/opinions?take=100&skip=",
    skip),
    add_headers(Authorization = API2key))
  
  if(length(content(opinions)) == 0){ # if there are no stories then stop
    continue = FALSE
  }
  
  opinionList = c(opinionList, content(opinions)) # add the stories to the list
  skip = skip + 100 # increase skip, and repeat
  #Sys.sleep(0.2) # need to add this for the API limit of 5 requests per second  
}
cat('\n')

# I keep getting a message saying "Sorry! Something went wrong." Removes entries with this error.
cat(NROW(opinionList[opinionList == "Sorry! Something went wrong."]),'entries removed because they were empty \n') 
opinionList = opinionList[opinionList != "Sorry! Something went wrong."]

# Got an error later when some list elements contained one element not 15
cat(NROW(opinionList[lengths(opinionList) != 15]),'more entries removed because they were empty \n') 
opinionList = opinionList[lengths(opinionList) == 15]

# Pick out the data we want from opinionList
# if there are no new stories this entire bit is skipped
if(length(opinionList) > 0){
  
  storyData = data.frame("PostID" = unlist(lapply(opinionList, "[[", "id")),
                         "Title" = unlist(lapply(opinionList, "[[", "title")),
                         "Story" = unlist(lapply(opinionList, "[[", "body")),
                         "Date" = as.Date(substr(unlist(lapply(opinionList, "[[", "dateOfSubmission")), 1, 10)),
                         "criticality" = unlist(lapply(opinionList, "[[", "criticality")),
                         "progress" = unlist(lapply(opinionList, "[[", "progress")),
                         stringsAsFactors = FALSE
                         )
}

save(storyData, file = "data\\2storyData.rda")


################ Get tags data using API 2 ##############

tagsData = NULL

for (id in sort(storyData$PostID)) {
  cat('\r Getting tag information for ',id)
  tags = GET(paste0("https://www.careopinion.org.uk/api/v2/opinions/",id,"/tags"),
             add_headers(Authorization = API2key))
  tagsdf = data.frame(PostID = rep(id,length(content(tags))),
             tagName = unlist(lapply(content(tags), "[[", "name")),
             tagGroup = unlist(lapply(content(tags), "[[", "group")),
             polarity = unlist(lapply(content(tags), "[[", "polarity"))
             )
  tagsData = rbind(tagsData, tagsdf)
  Sys.sleep(0.15) # need to add this for the API limit of 5 requests per second
}
cat('\n')

save(tagsData, file = "data\\2tagsData.rda")


############### Get services lookup data using API2 #################

# produce empty list to lappend
serviceList = list()

# set skip at 0 and make continue TRUE until no stories are returned
skip = 0
continue = TRUE

while(continue){
  #while(skip <= 100000){
  service = GET(paste0(
    #"https://www.careopinion.org.uk/api/v2/healthservices?type=hospital&addedAfter=",GetFrom,"&take=100&skip=",
    "https://www.careopinion.org.uk/api/v2/healthservices?type=hospital&take=100&skip=",
    skip),
    add_headers(Authorization = API2key))

  if(length(content(service)) == 0){ # if there are no stories then stop
    continue = FALSE
  }

  serviceList = c(serviceList, content(service)) # add the stories to the list
  # increase skip, and repeat
  cat('\r',"Getting services", skip, "to",skip+99,"from Care Opinion")
  Sys.sleep(0.1) # need to add this for the API limit of 5 requests per second
  skip = skip + 100
}
cat('\n')

# Pick out the data we want from serviceList
# if there are no new services this entire bit is skipped
if(length(serviceList) > 0){

  # Need to first handle NULL values
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
  serviceList <- lapply(serviceList, nullToNA)

  # Create serviceData
  serviceData = data.frame("NACS" = unlist(lapply(serviceList, "[[", "nacs")),
                           "NACSname" = unlist(lapply(serviceList, "[[", "name")),
                           "type" = unlist(lapply(serviceList, "[[", "type")),
                           "organisation" = unlist(lapply(serviceList, "[[", "organisation")),
                           stringsAsFactors = FALSE
  )

  save(serviceData, file = "data\\2serviceData.rda")


  ## Create updated serviceFrameSC
  load(file = "data\\2serviceFrameSC.rda")

  # Update the master, adding any new tag or replacing with new data
  serviceFrameSC = serviceFrameSC %>%
    anti_join(serviceData, by = c("NACS")) %>%
    bind_rows(serviceData)

  save(serviceFrameSC, file = "data\\2serviceFrameSC.rda")

}

################ Get services data from story data ##################

# Extract nacs id from OpinionList

nacsList = NULL
for (j in 1:length(opinionList)) {
  idV = c()
  nacsV = c()
  for (i in 1:NROW(opinionList[[j]]$links)) {
    if(opinionList[[j]]$links[[i]]$rel == "healthservice")  {
      idV = c(idV, opinionList[[j]]$id)
      nacsV = c(nacsV, opinionList[[j]]$links[[i]]$id)
    }
  }
  nacsdf = data.frame(PostID = idV,
                      NACSid = nacsV)
  nacsList = rbind(nacsList, nacsdf)
}

# Match story id with service information
#nacsData = nacsData %>% left_join(serviceFrameSC)


# Get nacs information using API2
nacsInfo = NULL
for (id in unique(nacsList$NACSid)) {
  cat('\r Getting nacs information for ',id)
  nacs = GET(paste0("https://www.careopinion.org.uk/api/v2/healthservices/",gsub("\\s", "%20",id)),
             add_headers(Authorization = API2key))
  
  content_nacs = content(nacs)
  
  #Need to first handle NULL values
    nullToNA <- function(x) {
      x[sapply(x, is.null)] <- NA
      return(x)
    }
    content_nacs[sapply(content_nacs, is.null)] <- NA
    content_nacs <- lapply(content_nacs, nullToNA)
  
  nacsdf = data.frame(
                      NACSid = content_nacs$nacs,
                      NACSname = content_nacs$name,
                      NACtype = content_nacs$type,
                      NACorg = content_nacs$organisation,
                      NACS = content_nacs$siteNacs)  
  
  nacsInfo = rbind(nacsInfo, nacsdf)
  Sys.sleep(0.2) # need to add this for the API limit of 5 requests per second
} 

nacsData = nacsList %>% left_join(distinct(nacsInfo))

save(nacsData, file = "data\\2nacsData.rda")




################ Create master data files ################ 

################ nacsFrameSC ##################

## Master nacFrameSC 
load(file = "data\\2nacFrameSC.rda")

# Update the master, adding any new tag or replacing with new data
nacFrameSC = nacFrameSC %>%
  anti_join(nacsData) %>%
  bind_rows(nacsData)

### Add service level groupings from lookup
nacLookup = read.csv(paste("lookups\\20230106 nacLookup.csv"), stringsAsFactors = FALSE, header=TRUE)
# Add line to make upper/lower/title case

nacFrameSC = nacFrameSC %>% 
  select(-NACSgroup) %>%
  left_join(nacLookup) %>% 
  mutate(NACSgroup = ifelse(is.na(NACSgroup), "Other", NACSgroup))

save(nacFrameSC, file = "data\\2nacFrameSC.rda")


################ tagFrameSC ########################

# Get the latest master tagsData
load(file = "data\\2tagFrameSC.rda")

tagData = tagsData %>% 
  mutate(tagName = str_to_sentence(str_trim(as.character(tagName))),
         tagName_orig = tagName,
         PostID = as.numeric(as.character(PostID)),
         polarity = as.numeric(as.character(polarity))) # Convert to number


# Update the master, adding any new tags or replacing with new data
tagFrameSC = tagFrameSC %>%
  anti_join(tagData, by = c("PostID", "tagName_orig")) %>%
  bind_rows(tagData)


# Change polarity and tagName from lookup
tagLookup = read_excel("lookups\\20200611 tagLookup.xlsx", sheet = "tagLookup") %>%
  rename(tagName_orig = tagName) %>% 
  mutate(tagName_orig = str_to_sentence(tagName_orig),
         Rename = str_to_sentence(Rename))

orderedtags = read_excel("lookups\\20200611 tagLookup.xlsx", sheet = "orderedtags") %>%
  mutate(tagLevel = str_to_sentence(tagLevel)) %>% 
  pull(tagLevel)

tagFrameSC = tagFrameSC %>%
  select(-tagClass) %>% 
  left_join(tagLookup) %>% 
  mutate( polarity = ifelse(!is.na(myPolarity), myPolarity, polarity),
           tagName = ifelse(!is.na(Rename), Rename, tagName_orig),
          tagName = factor(tagName, levels = unique(c(orderedtags,unique(tagName), "Unclassified"))))  %>% #DOUBLE "UNCLASSIFIED" ADDITION IS CORRECT
  select(PostID, polarity, tagName_orig, tagName, tagGroup, tagClass)


## Save tagFrame 
save(tagFrameSC, file = "data\\2tagFrameSC.rda")

################ storyFrameSC ########################

### Match on an overall polarity and criticality etc to storyFrame
# Calculate overall polarity
tagPolSumDF = tagFrameSC %>% 
  #mutate(PostID = as.numeric(as.character(PostID))) %>%   
  group_by(PostID) %>% 
  dplyr::summarise(tagPolSum = sum(polarity)) 

# Add overall polarity and criticality etc
storyFrame = storyData %>% 
  left_join(tagPolSumDF) %>% 
  mutate(criticality = ifelse(is.na(criticality), "not known", criticality),
         Date = as.Date(Date, format = "%Y-%m-%d"))

# Get the latest master story data
load(file = "data\\2storyFrameSC.rda")


# Update the master, adding any new stories or replacing old with new
storyFrameSC = storyFrameSC %>%
  anti_join(storyFrame, by = c("PostID")) %>%
  bind_rows(storyFrame)

save(storyFrameSC, file = "data\\2storyFrameSC.rda")

print("Data read and saved!")


################ Validate data #######################
checkFromDate = as.Date("2024-03-01")
countcheck = storyFrameSC %>% filter(Date >= checkFromDate, Date < today()) %>% count() %>% pull()
cat('\nAccording to storyFrameSC, the number of submitted stories since', format(checkFromDate,"%d %B %y"),'is', countcheck, '. Check this against Care Opinion site by searching on date but not service (not even NHS Scotland, we want the full subcription data e.g. https://www.careopinion.org.uk/opinions?submittedonafter=2024-03-01). There are ususally 1 or 2 stories missing from the storyFrameSC each month.')



################ Lookup maintainance ################

### Write list of emotion tags without polarity - assign polarity if reasonable and add to tagLookup.xlsx
# write.csv(unique(tagFrame$tagName[tagFrame$tagGroup == "Emotion" & tagFrame$polarity == 0]), file = "input\\EmotionsCHECK.csv")

### Write list of negative tags from critical stories not in orderedtags - rename and assign tagClass and add to tagLookup.xlsx
allFrame = merge(storyFrameSC,tagFrameSC, by.x="PostID", by.y="PostID")
allFrame %>% 
   filter(polarity == -1,
          criticality %in% c(
            "minimally critical", "mildly critical",
            "moderately critical", "strongly critical", "severely critical"),
          is.na(tagClass),
          !tagGroup %in% c("Cause of disease", "Emotion", "Condition","Part of body"),
          Date > as.Date("2019-06-01")) %>% 
  distinct(PostID, polarity, tagName, tagName_orig) %>% 
   group_by(tagName_orig) %>%
   dplyr::summarise(count = n()) %>%
   write.csv((file = "lookups\\tagCHECK.csv"))


### Write list of NACS services without a group - assign NACSgroup and add to nacLookup.csv
# check specialty at https://www.medschools.ac.uk/studying-medicine/after-medical-school/specialties
nacFrameSC %>% 
  filter(NACtype == "service", NACSgroup =="Other") %>% 
  group_by(NACS, NACSname) %>% 
  count() %>% 
  write.csv(., file = "lookups\\nacsCHECK.csv")
