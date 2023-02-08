#load tidyverse for data manipulation
#load haven to read SPSS file
library(tidyverse)
library(haven)

#read in SAV file, allow file choice (for now)
dat <- read_sav(choose.files())

#structure of data and variables
str(dat)

## SUBSETTING & CLEANING DATA ##
#Screening requirements: Race_AA == 1, Screening_Female == 1, Screening_Age == 1, Screening_Consent == 1
#In words, Black women who are 18+ and consent to this survey
#Potentially look at completed surveys vs incomplete; How many were screened out and how many stopped after a certain point? Why did the ones who were not screened out not complete the survey? Connectivity issues, anxiety/depression related, or loss of interest? How do we handle these entries?

## Screening:
screenDat <- filter(dat, Race_AA == 1, Screening_Female == 1, Screening_Age == 1, Screening_Consent == 1)

## Variables: What to remove/include? Recode vs Original?