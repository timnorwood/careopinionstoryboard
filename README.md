# What is Care Opinion

Care Opinion is a website for people to share their experiences of health and care services - https://www.careopinion.org.uk/info/about.

# What are Care Opinion Storyboards?

Care Opinion Storyboards summarise the experiences of people using health and care services shared on Care Opinion. They are primarily intended to help NHS organisations understand the experience of service users including the main themes and how experience changes over time. Storyboards are produced by the Data, Measurement & Business Intellgence team in Healthcare Improvment Scotland with support of Care Opinion under the Creative Commons licence for non-commercial re-use. 

Care Opinion provide access to the raw data of published stories, responses, tags, healthservices and treatment, through an API, among other ways. A subscription is required to use the API. The API will provide access to the same set of stories and responses as your subscription.

See this example - https://timnorwood.github.io/storyboard/storyboard_example.html.

# How is feedback summarised?

Storyboards use a range of charts and visualisations to summarise the information tagged to feedback. Every person who tells their story on Care Opinion is asked to create tags in response to three questions:

What was good?
What could be improved?
How did you feel?

Each tag can be given a negative or positive polarity. If the storyteller chooses not to tag their story moderators may add tags.

Tag information about the services used are also recorded with each story and used in this analysis.

# The R scripts

Storyboards have been created using R and the Flexdashboard package. There are three main script in this respository:

- GetStories: Reads data from Care Opinion using the API(s). You need a Care Opinion subscription to use get and API key.
- Storyboard: Flexdashboard script for creating the storyboards
- Monthly update: Script to run GetStories and Storyboard and produce storyboards for different NHS boards and hospitals. 
- SPC: scripts to select and produce run charts and other SPC charts using the standard Healthcare Improvement Scotland approach. Chart selection is based on the number of data points and sample size of data points.
- Input: Not R script but lookup files for grouping the subject of feedback (e.g. staff, care, access to services) and services used (e.g. surgical, AHP etc).
