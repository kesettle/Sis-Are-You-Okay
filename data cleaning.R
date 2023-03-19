#-----DATA LOADING-----#

#Load needed packages
library(tidyverse)

#Read file in from repo directory
dat <- read.csv('2022 BWMHS CINT Initial Survey_6.24.22.csv')

#Inspect structure of the data
str(dat)

#STARTING DIMENSIONS: (-0) 2631 obs. of (-0) 157 variables

#-----DATA PREPARATION-----#

#Screening requirements: Race_AA == 1, Screening_Female == 1, Screening_Age == 1, Screening_Consent == 1
#In words, Black women who are 18+ and consent to participating in this survey

screenDat <- filter(dat, Race_AA == 1, Screening_Female == 1, Screening_Age == 1, Screening_Consent == 1)

#CURRENT DIMENSIONS: (-371) 2260 obs. of (-0) 157 variables


#Variable subsetting criteria (i.e., what is being removed and why):
#1) No viable/meaningful information (redacted or just not usable)
#2) Open-ended responses (end with "_text")
#3) Majority of obs. are NA values (variables were either above 75% NAs or below 15%; former removed)
#4) Has recoded/standardized version to be used instead
#5) Composite scores or sums; retains redundancy and linear dependence from their components


#Tibble of NA proportions among variables
p_tbl <- tibble(var = names(screenDat),
               num = colSums(is.na(screenDat)),
               prop = colMeans(is.na(screenDat)))

#Names of variables that are mostly NAs
p_rm <- p_tbl$var[p_tbl$prop > 0.7]

#Subsetting variables based on defined criteria
closedDat <- screenDat %>% select(!c(all_of(p_rm),
                                    StartDate:Race_AA,
                                    Race_Openended,
                                    Screening_Female:Covid_Wellbeing_Financial,
                                    Followed_Protests:Protests_police_good_job,
                                    Prior_Covid_experienced_racial_discrimination:GAD7_Q7_afraid_something_bad,
                                    Coping_other,
                                    Employment_status_other,
                                    Zip,
                                    Focus_group_interview:id,
                                    ends_with(c("_Total_Score","_SUM", "_Coping", "_text"))))

#CURRENT DIMENSIONS: (-371) 2260 obs. of (-88) 68 variables


#Transformations
finiDat <- closedDat %>%
  mutate(Age = as.integer(Age)) %>%
  rename(Academia_status = Q102)

#All NA remaining NA values filtered out
okDat <- na.omit(finiDat)

#FINAL DIMENSIONS: (-810) 1450 obs. of (-88) 67 variables