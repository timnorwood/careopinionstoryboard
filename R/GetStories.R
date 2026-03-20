
library(tidyverse)
library(readxl)
library(httr)    # for GET

GetFrom = "2026-03-15"

API2key = Sys.getenv("API2key") # This is picked up from a .Renviron file in Documents
#usethis::edit_r_environ() # Use this to edit .Renviron from Rstudio


###################### Get opinion/hospital data ########################

get_story_data <- function(request, from, api_key, sleep = 0.2) {
  
  start_time = proc.time()
  
  contentList = list() # produce empty list to lappend to later
  skip = 0 # set skip at 0 and make continue TRUE until no stories are returned 
  continue = TRUE
  errOut <- c()
  dataOut <- NULL
  
  # Function to convert NUll results to NA
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)}
  
  while(continue){
    cat("\rGetting stories/hospital data",skip,"to",skip+99,"from",from)  
    
    cont = GET(paste0(
      "https://www.careopinion.org.uk/api/v2/",request,as.Date(from),"&take=100&skip=",
      skip),
      add_headers(Authorization = api_key))
    Sys.sleep(sleep) # need to add this for the API limit of 5 requests per second  
    
    if (cont$status_code == 200) {
      
      if(length(content(cont)) == 0){ # if there are no stories then stop
        cat('\nNo more stories after',skip,"\n")
        continue = FALSE
      }
      
      contentList = c(contentList, content(cont)) # add the stories to the list
      skip = skip + 100 # increase skip, and repeat
      
    } else {
      errOut <- c(errOut, cont$url)
    }
  }
  cat("GET failed for", length(errOut), "requests\n")
  
  contentList <- lapply(contentList, nullToNA)
  
  ### ---- STORIES ---- ###
  if (grepl("opinions",request)) {
    if(length(contentList) > 0){
      dataOut = data.frame("PostID" = unlist(lapply(contentList, "[[", "id")),
                             "Title" = unlist(lapply(contentList, "[[", "title")),
                             "Story" = unlist(lapply(contentList, "[[", "body")),
                             "Date" = as.Date(substr(unlist(lapply(contentList, "[[", "dateOfSubmission")), 1, 10)),
                             "criticality" = unlist(lapply(contentList, "[[", "criticality")),
                             "progress" = unlist(lapply(contentList, "[[", "progress")),
                             "authorRole" = unlist(lapply(contentList, "[[", "authorRole")),
                             "authorAgeGroup" = unlist(lapply(contentList, "[[", "authorAgeGroup")),
                             stringsAsFactors = FALSE)
    }
  }
  
  ### ---- SERVICES ---- ###
  if (grepl("healthservices",request)) {
    if(length(contentList) > 0){
      dataOut = data.frame("NACS" = unlist(lapply(contentList, "[[", "nacs")),
                               "NACSname" = unlist(lapply(contentList, "[[", "name")),
                               "type" = unlist(lapply(contentList, "[[", "type")),
                               "organisation" = unlist(lapply(contentList, "[[", "organisation")),
                               stringsAsFactors = FALSE)
    }
  }
  
  
  cat("Time to get stories:", difftime(proc.time()[3], start_time[3], units = "mins"), "mins\n\n")
  
  return(list(data = dataOut, errors = errOut, list = contentList))
}

# Get stories
stories = get_story_data("opinions?submittedonafter=", 
                         from = "2026-03-15", 
                         API2key)
storyData  <- stories$data
storyurlERR <- stories$errors
opinionList <- stories$list
save(storyData, file = "data\\3storyData.rda")

# Get services information (hospitals only)
service = get_story_data("healthservices?type=hospital&addedAfter=", 
                          from = "2026-01-15", 
                          API2key)
serviceData  <- service$data
serviceyurlERR <- service$errors
save(serviceData, file = "data\\3serviceData.rda")


################ Get links about tags, services, and prems etc from stories  ##############

# Extract links from opinion data
linksData= NULL

for (post in 1:length(opinionList)) {
  
  pid = c() # post id
  rel = c() # link relation or the type of link (tags, rating etc)
  lnk = c() # the link itself
  lid = c() # link id
  
  for (link in 1:NROW(opinionList[[post]]$links)) {
    
    pid = opinionList[[post]]$id
    rel = opinionList[[post]]$links[[link]]$relation
    lnk = opinionList[[post]]$links[[link]]$link
    lid = opinionList[[post]]$links[[link]]$id
    
    linksdf = data.frame(PostID = pid,
                         relation = rel,
                         link = lnk,
                         id = lid)
    
    linksData = rbind(linksData, linksdf)
  }
}


# Define a function to request information from
get_tag_data <- function(links, type, api_key, sleep = 0.2) {
  
  dataOut <- NULL
  errOut <- c()
  start_time = proc.time()
  
  # Function to convert NUll results to NA
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)}
  
  for (lk in links) {
    
    cat('\rGetting', type, 'information for',
        gsub("https://www.careopinion.org.uk/api/v2","",lk),
        "                                                ")
    
    res <- GET(lk, add_headers(Authorization = api_key))
    cont <- content(res)
    
    if (res$status_code == 200) {
      
      ### ---- PREM ---- ###
      if (type == "prem") {
        if (length(cont$ratings) > 0) {
          
          df <- data.frame(
            PostID        = cont$opinionId,
            nacs          = cont$healthserviceNacs,
            premQ         = unlist(lapply(cont$ratings, "[[", "questionText")),
            premScore     = unlist(lapply(cont$ratings, "[[", "score")),
            premScoreText = unlist(lapply(cont$ratings, "[[", "scoreText"))
          )
          
          dataOut <- rbind(dataOut, df)
        }
      }
      
      ### ---- TAGS ---- ###
      if (type == "tags") {
        if (length(cont) > 0) {
          
          pid <- sub(".*opinions/([0-9]+)/tags.*", "\\1", lk)
          
          df <- data.frame(
            PostID   = pid,
            tagName  = unlist(lapply(cont, "[[", "name")),
            tagGroup = unlist(lapply(cont, "[[", "group")),
            polarity = unlist(lapply(cont, "[[", "polarity"))
          )
          
          dataOut <- rbind(dataOut, df)
        }
      }
      
      ### ---- SERVICES ---- ###
      if (type == "services") {
        if (length(cont) > 0) {
          
          cont <- lapply(cont, nullToNA)
          
          pid <- sub(".*opinions/([0-9]+)/healthservices.*", "\\1", lk)
          
          df <- data.frame(
            PostID   = pid,
            NACSid   = unlist(lapply(cont, "[[", "nacs")),
            NACSname = unlist(lapply(cont, "[[", "name")),
            NACtype  = unlist(lapply(cont, "[[", "type")),
            NACorg   = unlist(lapply(cont, "[[", "organisation")),
            NACS     = unlist(lapply(cont, "[[", "siteNacs"))
          )
          
          dataOut <- rbind(dataOut, df)
        }
      }
      
    } else {
      errOut <- c(errOut, res$url)
    }
    
    Sys.sleep(sleep)
  }
  
  cat("\nGET failed for", length(errOut), "requests.\n")
  cat("Time to get", type, "data:", difftime(proc.time()[3], start_time[3], units = "mins"), "mins\n\n")
  
  return(list(data = dataOut, errors = errOut))
}


# Get prem data
prem_cont <- get_tag_data(links   = linksData$link[linksData$relation == "ratings"],
                         type    = "prem",
                         api_key = API2key,
                         sleep   = 0.1)

premData  <- prem_cont$data
premurlERR <- prem_cont$errors
save(premData, file = "data\\3premData.rda")

# Get tag data
tags_cont <- get_tag_data(links   = linksData$link[linksData$relation == "tags"],
                         type    = "tags",
                         api_key = API2key,
                         sleep   = 0.2)

tagsData  <- tags_cont$data
tagurlERR <- tags_cont$errors
save(tagsData, file = "data\\3tagsData.rda")

# Get nacs data
nacs_cont <- get_tag_data(links   = linksData$link[linksData$relation == "healthservices"],
                             type    = "services",
                             api_key = API2key,
                             sleep   = 0.1)

nacsData   <- nacs_cont$data
nacsurlERR <- nacs_cont$errors
save(nacsData, file = "data\\3nacsData.rda")



##################### Create master data files ####################### 
######################################################################

start_time = Sys.time()

############## serviceFrameSC ################
load(file = "data\\2serviceFrameSC.rda")

# Update the master, adding any new tag or replacing with new data
serviceFrameSC = serviceFrameSC %>%
  anti_join(serviceData, by = c("NACS")) %>%
  bind_rows(serviceData)

save(serviceFrameSC, file = "data\\2serviceFrameSC.rda")


################ nacsFrameSC ##################

## Master nacFrameSC 
load(file = "data\\2nacFrameSC.rda")
load(file = "data\\2nacsData.rda")

# Update the master, adding any new tag or replacing with new data
nacFrameSC = nacFrameSC %>%
  anti_join(nacsData) %>%
  bind_rows(nacsData)

### Add service level groupings from lookup
nacLookup = read.csv(paste("lookups\\20240925 nacLookup.csv"), stringsAsFactors = FALSE, header=TRUE) %>% 
  mutate(NACSname = str_to_sentence(NACSname))

nacFrameSC = nacFrameSC %>% 
  select(-NACSgroup) %>%
  mutate(NACSname = str_to_sentence(NACSname)) %>% 
  left_join(nacLookup) %>% 
  mutate(NACSgroup = ifelse(is.na(NACSgroup), "Other", NACSgroup))

save(nacFrameSC, file = "data\\2nacFrameSC.rda")


################ tagFrameSC ########################

# Get the latest master tagsData
load(file = "data\\2tagFrameSC.rda")
load(file = "data\\2tagsData.rda")

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

load(file = "data\\2storyData.rda")

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

cat("Time to create masters:", Sys.time() - start_time)

################ Validate data #######################
countcheck = storyFrameSC %>% filter(Date >= as.Date(GetFrom), Date < today()) %>% count() %>% pull()
cat(sep = "",'\nAccording to storyFrameSC, the number of submitted stories since', format(as.Date(GetFrom),"%d %B %y"),'is', countcheck, '. Check this against Care Opinion site by searching on date but not service (not even NHS Scotland, we want the full subcription data) - https://www.careopinion.org.uk/opinions?submittedonafter=',GetFrom,' - there are ususally 1 or 2 stories missing from the storyFrameSC each month.')



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
         Date > as.Date("2022-06-01")) %>% 
  distinct(PostID, tagName_orig) %>% 
  count(tagName_orig) %>%
  write.csv((file = "lookups\\tagCHECK.csv"))


### Write list of NACS services without a group - assign NACSgroup and add to nacLookup.csv
# check specialty at https://www.medschools.ac.uk/studying-medicine/after-medical-school/specialties
nacFrameSC %>% 
  filter(NACtype == "service", NACSgroup =="Other") %>% 
  group_by(NACS, NACSname) %>% 
  count() %>% 
  write.csv(., file = "lookups\\nacsCHECK.csv")
