# What is Care Opinion

Care Opinion is a website for people to share their experiences of health and care services - https://www.careopinion.org.uk/info/about.

# What are Care Opinion Storyboards?

Care Opinion Storyboards summarise the experiences of people using health and care services shared on Care Opinion. They are primarily intended to help NHS organisations understand the experience of service users including the main themes and how experience changes over time. Storyboards are produced by the Data, Measurement & Business Intelligence team in Healthcare Improvement Scotland with support of Care Opinion under the Creative Commons licence for non-commercial re-use. 

Care Opinion provide access to the raw data of published stories, responses, tags, health services and treatment, through an API, among other ways. A subscription is required to use the API. The API will provide access to the same set of stories and responses as your subscription.

See this example - https://timnorwood.github.io/careopinionstoryboard/storyboard_example.html.

# How is feedback summarised?

Storyboards use a range of charts and visualisations to summarise the information tagged to feedback. Every person who tells their story on Care Opinion is asked to create tags in response to three questions:

What was good?
What could be improved?
How did you feel?

Each tag can be given a negative or positive polarity. If the storyteller chooses not to tag their story moderators may add tags.

Tag information about the services used are also recorded with each story and used in this analysis.

# The R scripts

Storyboards have been created using R and the Flexdashboard package. The main scripts in this respository are:

- GetStories: Reads data from Care Opinion using the API. You need a Care Opinion subscription to get an API key.
- Storyboard: Flexdashboard script for creating the storyboards. 
- (Example) Monthly update: Script to run GetStories and Storyboard and produce storyboards for different NHS boards and hospitals. 

Other scripts include functions and code that are used by the main scripts. These are: 

- Run charts, SPC charts and SelectRunChart: select and produce run charts and other SPC charts using the standard Healthcare Improvement Scotland approach. Chart selection is based on the number of data points and sample size of data points.
- Storyboard pages and Download button: used by Storyboard to produce pages and download button for p chart

Lookup files are used by Get stories to group the subject of feedback (e.g. staff, care, access to services) and services used (e.g. surgical, AHP etc).

# Getting started

1. Download or fork a copy of files.
2. Request an 'API key for use in HTTP header' from Care Opinion. To do this sign in to the Care Opinion website and go to https://www.careopinion.org.uk/mysubscriptions
3. Save your API key to your .Renviron file in the format: API2key = "SUBSCRIPTION_KEY youruniquelettersandnumbershere". This file can usually be found your Windows Documents folder. You can also edit it from Rstudio using usethis::edit_r_environ().
3. Open the Example monthly update script. Change the Getfrom date to at least two years prior. It will take around an hour to get two years data but 20 months are needed for some charts. In future updates Getfrom can be two months prior so it runs faster. Check the other parameters and amend these as required. Organisation and service codes used by Care Opinion can be found in the url of searches (e.g. the url for a search of stories about NHS Grampian is https://www.careopinion.org.uk/opinions?nacs=SN9)
4. Run the full Example monthly update script. If it's successful, storyboards will be saved in the output folder.