#load tidyverse for data manipulation
#load haven to read SPSS file
library(tidyverse)
library(haven)

#reading in csv version of the data, loading from local drive
dat <- read.csv(choose.files())

#structure of data and variables
str(dat)

## SUBSETTING & CLEANING DATA ##
#Screening requirements: Race_AA == 1, Screening_Female == 1, Screening_Age == 1, Screening_Consent == 1
#In words, Black women who are 18+ and consent to this survey
#Potentially look at completed surveys vs incomplete; How many were screened out and how many stopped after a certain point? Why did the ones who were not screened out not complete the survey? Connectivity issues, anxiety/depression related, or loss of interest? How do we handle these entries?

## Screening:
screenDat <- filter(dat, Race_AA == 1, Screening_Female == 1, Screening_Age == 1, Screening_Consent == 1)

## Variables: What to remove/include? Recode vs Original?


# Removing all definitely unneeded and open-ended response variables (i.e., end with "_text") 

closedDat <- screenDat %>% select(!c((StartDate:IPAddress),
                                    (RecordedDate:Covid_Wellbeing_Financial),
                                    (Followed_Protests:Protests_police_good_job),
                                    (Prior_Covid_experienced_racial_discrimination:GAD7_Q7_afraid_something_bad),
                                    Coping_other,
                                    Employment_status_other,
                                    Zip,
                                    (Focus_group_interview:id),
                                    ends_with("_text")))

#Filtering for those who completed at least half of the survey; only 207 didn't make it that far
#and the smallest value is 66%, which I think is enough...for now

complDat <- closedDat %>% filter(Progress > 50)
