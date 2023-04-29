##############################
#----------PACKAGES----------#
##############################
#Install packages as needed
#install.packages (c("GPArotation", "lavaan","nFactors", "parameters", "performance", "psych",
#                    "semPlot", "semTools", "tidyverse"))

#Load packages
library(GPArotation)
library(lavaan)
library(nFactors)
library(parameters)
library(performance)
library(psych)
library(semPlot)
library(semTools)
library(tidyverse)

##################################
#----------DATA LOADING----------#
##################################
#Read file in from repo directory
dat <- read.csv('2022 BWMHS CINT Initial Survey_6.24.22.csv')

#Inspect structure of the data
str(dat)

#STARTING DIMENSIONS: (-0) 2631 obs. of (-0) 157 variables

######################################
#----------DATA PREPARATION----------#
######################################
#Screening requirements: Race_AA == 1, Screening_Female == 1, Screening_Age == 1, Screening_Consent == 1
#In words, Black women who are 18+ and consent to participating in this survey

screenDat <- dplyr::filter(dat, Race_AA == 1, Screening_Female == 1, Screening_Age == 1, Screening_Consent == 1)
#CURRENT DIMENSIONS: (-371) 2260 obs. of (-0) 157 variables

#Add forgotten question PHQ9_Q1_Little_interest - estimated as average of other PHQ9 scores
screenDat$PHQ9_Q1_Little_interest_EST <- round((screenDat$PHQ9_Q2_Feeling_down + screenDat$PHQ9_Q3_Trouble_sleep + screenDat$PHQ9_Q4_tired_low_energy + screenDat$PHQ9_Q5_Poor_appetite_overeating + screenDat$PHQ9_Q6_Feeling_bad_about_self + screenDat$PHQ9_Q7_Trouble_concentrating + screenDat$PHQ9_Q8_thinking_slowly_fidgety + screenDat$PHQ9_Q9_SI_Thoughts)/8, digits = 0)

#Add PHQ9_Q1 recode
screenDat$PHQ9_Q1_Little_interest_EST_recode <- screenDat$PHQ9_Q1_Little_interest_EST - 1

#Update PHQ9_Total_Score
screenDat$PHQ9_Total_Score <- screenDat$PHQ9_Total_Score + screenDat$PHQ9_Q1_Little_interest_EST_recode

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
                                     ends_with(c("_Total_Score","_SUM", "_Coping", "_text")),
                                     PHQ9_Q1_Little_interest_EST))

#CURRENT DIMENSIONS: (-371) 2260 obs. of (-91) 68 variables

#Transformations
finiDat <- closedDat %>%
  mutate(Age = as.integer(Age)) %>%
  rename(Academia_status = Q102)

#All NA remaining NA values filtered out
okDat <- na.omit(finiDat)

#FINAL DIMENSIONS: (-810) 1450 obs. of (-91) 68 variables

#################################################
#----------EXPLORATORY FACTOR ANALYSIS----------#
#################################################

#Data manipulation for the composite score-focused version of our models:
#Total/Composite Scores for GAD and depression are kept, individual question vars held out (named `compDat`)

compDat <- screenDat %>% select(!c(all_of(p_rm),
                                   StartDate:Race_AA,
                                   Race_Openended,
                                   Screening_Female:Covid_Wellbeing_Financial,
                                   Followed_Protests:Protests_police_good_job,
                                   Prior_Covid_experienced_racial_discrimination:GAD7_Q7_afraid_something_bad,
                                   Coping_other,
                                   Employment_status_other,
                                   Zip,
                                   Focus_group_interview:id,
                                   ends_with(c("_SUM", "_Coping", "_text"))))

compDat <- compDat %>%
  mutate(Age = as.integer(Age)) %>%
  rename(Academia_status = Q102)

compDat <- na.omit(compDat)

compDat <- compDat %>% select(!c(GAD7_Q2_cannot_stop_worrying_recode:GAD7_Q7_afraid_something_bad_recode,
                                 PHQ9_Q2_Feeling_down_recode:PHQ9_Q10_Impact_of_Problems_recode,
                                 GAD7_Q1_Feeling_nervous_Recode,
                                 PHQ9_Q1_Little_interest_EST,PHQ9_Q1_Little_interest_EST_recode))

#DIMENSIONS: (-810) 1450 obs. of (-106) 53 variables

#-----Checking that datasets are suitable for factor analysis-----#

#okDat - Yes, very much so! KMO = 0.92
performance::check_factorstructure(okDat)

#compDat - Yes, to a slightly lower degree; KMO = 0.83
performance::check_factorstructure(compDat)


#-----Find principal components-----#


#Response(s) must be removed
PCokDat <- okDat[,-c(34,67)]
PCcmpDat <- compDat[,-c(34,53)]

PCok <- prcomp(~ ., data = PCokDat, scale = TRUE)
summary(PCok)

PCcmp <- prcomp(~ ., data = PCcmpDat, scale = TRUE)
summary(PCcmp)


#Contribution of variance levels out at around 4 to 5 PCs for both; may consider no more than that
par(mfrow = c(1, 2))
plot(PCok, type = 'l')
plot(PCcmp, type = 'l')

#-----Plots of variance explained by PCs-----#

#okDat
plot(PCok$sdev^2/sum(PCok$sdev^2), main = "PCs for okDat", xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
abline(v=15)
plot(cumsum(PCok$sdev^2/sum(PCok$sdev^2)), xlab = "Principal Component",
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')
abline(v = c(5,10,15), h = c(0.4,0.5,0.6))

#compDat
plot(PCcmp$sdev^2/sum(PCcmp$sdev^2), main = "PCs for compDat", xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
abline(v=15)
plot(cumsum(PCcmp$sdev^2/sum(PCcmp$sdev^2)), xlab = "Principal Component",
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')
abline(v = c(5,10,15), h = c(0.4,0.5,0.6))

#-----Estimate number of factors to retain with scree test-----#
#okDat - 15 seems to be the majority
okScr <- nScree(PCokDat)
okScr

#compDat - 14 seems to be the majority
cmpScr <- nScree(PCcmpDat)
cmpScr

#-----Examine the eigenvalues-----#

#okDat - The first 15 are > 1.0, so this suggests about 15 factors; the first 5 >> 1.0,
#which is consistent with the PCA
round(eigen(cor(PCokDat))$values, 4)

#compDat - The first 14 are > 1.0, so this suggests about 14 factors; the first 4 >> 1.0,
#which is, again, consistent with the PCA
round(eigen(cor(PCcmpDat))$values, 4)

#-----Compare EFA with k factors-----#

#Determining/confirming number of factors to retain based on various methods/metrics

#okDat - 15 is tied with other values, so should be reasonable; the highest of 65 is definitely not
ok.n <- n_factors(PCokDat)
plot(ok.n) + see::theme_modern()

#compDat - Highest is 3, definitely not sufficient for the dimensions; 14 has moderate stake
cmp.n <- n_factors(PCcmpDat)
plot(cmp.n) + see::theme_modern()


#-----Fit EFA models using psych package and output loadings to csv files-----#

#okDat, oblimin then varimax
faOK.oblimin <- fa(PCokDat, nfactors = 15, rotate = "oblimin") %>%
  model_parameters(sort = TRUE, threshold = "max")

write.csv(faOK.oblimin, "EFA_loadings_Oblimin_Raw.csv")

faOK.varimax <- fa(PCokDat, nfactors = 15, rotate = "varimax") %>%
  model_parameters(sort = TRUE, threshold = "max")

write.csv(faOK.varimax, "EFA_loadings_Varimax_Raw.csv")

#compDat, oblimin then varimax
faCmp.oblimin <- fa(PCcmpDat, nfactors = 14, rotate = "oblimin") %>%
  model_parameters(sort = TRUE, threshold = "max")

write.csv(faCmp.oblimin, "EFA_loadings_Oblimin_Total.csv")

faCmp.varimax <- fa(PCcmpDat, nfactors = 14, rotate = "varimax") %>%
  model_parameters(sort = TRUE, threshold = "max")

write.csv(faCmp.varimax, "EFA_loadings_Varimax_Total.csv")



###################################################
#----------STRUCTURAL EQUATIONS MODELING----------#
###################################################
#----Data Checks ----#
describe(okDat)

#depression=~PHQ9_Q4_tired_low_energy_recode+PHQ9_Q5_Poor_appetite_overeating_recode+PHQ9_Q3_Trouble_sleep_recode+PHQ9_Q6_Feeling_bad_about_self_recode+PHQ9_Q2_Feeling_down_recode+PHQ9_Q7_Trouble_concentrating_recode+PHQ9_Q8_thinking_slowly_fidgety_recode+PHQ9_Q9_SI_Thoughts_recode+PHQ9_Q10_Impact_of_Problems_recode+Coping_alcohol_drugs

#----Test Models for Structural Equation Modeling (SEM)----#
full_model <- "
anxiety=~GAD7_Q2_cannot_stop_worrying_recode+GAD7_Q7_afraid_something_bad_recode+GAD7_Q4_trouble_relaxing_recode+GAD7_Q1_Feeling_nervous_Recode+GAD7_Q5_restless_recode+GAD7_Q6_irritable_recode

depression=~PHQ9_Q4_tired_low_energy_recode+PHQ9_Q5_Poor_appetite_overeating_recode+PHQ9_Q3_Trouble_sleep_recode+PHQ9_Q6_Feeling_bad_about_self_recode+PHQ9_Q2_Feeling_down_recode+PHQ9_Q7_Trouble_concentrating_recode+PHQ9_Q8_thinking_slowly_fidgety_recode+PHQ9_Q9_SI_Thoughts_recode+PHQ9_Q10_Impact_of_Problems_recode+Coping_alcohol_drugs

racism_protest_sentiment=~Protests_sympathy_recode+Protests_appropriate_response_recode+Prior_Covid_Racism+Followed_Protests_recode+Covid_Racism_increased+Population_density

healthy_coping=~Coping_exercise+Coping_outdoors+Coping_pleasurable_activities+Coping_spiritual

pandemic_impact=~Covid_wellbeing_psychological_recode+Covid_wellbeing_overall_recode+Covid_Wellbeing_Physical_recode+Covid_wellbeing_financial_recode+Covid_wellbeing_social_recode

exp_racism=~Covid_experienced_racial_discrimination_recode

personal_id=~Protest_Participation+Political_views+Sexual_Orientation+Find_out_study+Age

covid_dx_status=~Covid_isolate
socioecon_status=~Education+Marital_status

govt_sentiment=~Protests_police_good_job_recode+Protests_propertydamanage_undermine_recode

emotional_coping=~Coping_distract_work+Coping_optimism+Coping_humor+Coping_not_show_emotions+Coping_emotional_support

politics_descisions=~Political_affliation+Covid_Vaccine

covid_impact_others=~Covid_death+Covid_family_Friends

employment_status=~Employment_status
"

sat.fit <- sem(full_model, data= okDat, std.lv=TRUE)
lavInspect(sat.fit, "cov.lv") 
summary(sat.fit, fit.measures= TRUE)

anxiety_model <- "
anxiety=~emotional_coping+GAD7_Q2_cannot_stop_worrying_recode+GAD7_Q7_afraid_something_bad_recode+GAD7_Q4_trouble_relaxing_recode+GAD7_Q1_Feeling_nervous_Recode+GAD7_Q5_restless_recode+GAD7_Q6_irritable_recode

emotional_coping=~Coping_distract_work+Coping_optimism+Coping_humor+Coping_not_show_emotions+Coping_emotional_support

pandemic_impact=~anxiety+Covid_wellbeing_psychological_recode+Covid_wellbeing_overall_recode+Covid_Wellbeing_Physical_recode+Covid_wellbeing_financial_recode+Covid_wellbeing_social_recode

covid_impact_others=~anxiety+Covid_death+Covid_family_Friends

politics_descisions=~anxiety+Political_affliation+Covid_Vaccine
"

sat.fit <- sem(anxiety_model, data= okDat, std.lv=TRUE)
summary(sat.fit, fit.measures= TRUE)
lavInspect(sat.fit, "cov.lv") 

semPaths(sat.fit, layout="tree2", nCharNodes = 0, residual=FALSE,
         whatLabels = "est", edge.label.cex = 1, node.label.cex = 1, 
         label.prop=0.9, edge.label.color = "black", rotation = 2, 
         equalizeManifests = TRUE, optimizeLatRes = TRUE, node.width = 2, 
         edge.width = 1.5, shapeMan = "rectangle", shapeLat = "circle", 
         shapeInt = "triangle", sizeMan = 5, sizeInt = 5, sizeLat = 5, 
         curve=2, unCol = "black", structural=TRUE, 
         filetype = "pdf", width = 8, height = 6, filename = "SEM Anxiety v2")


depression_model <- "
depression=~PHQ9_Q4_tired_low_energy_recode+PHQ9_Q5_Poor_appetite_overeating_recode+PHQ9_Q3_Trouble_sleep_recode+PHQ9_Q6_Feeling_bad_about_self_recode+PHQ9_Q2_Feeling_down_recode+PHQ9_Q7_Trouble_concentrating_recode+PHQ9_Q8_thinking_slowly_fidgety_recode+PHQ9_Q9_SI_Thoughts_recode+PHQ9_Q10_Impact_of_Problems_recode+Coping_alcohol_drugs

pandemic_impact=~depression+Covid_wellbeing_psychological_recode+Covid_wellbeing_overall_recode+Covid_Wellbeing_Physical_recode+Covid_wellbeing_financial_recode+Covid_wellbeing_social_recode

covid_impact_others=~pandemic_impact+Covid_death+Covid_family_Friends

healthy_coping=~pandemic_impact+Coping_exercise+Coping_outdoors+Coping_pleasurable_activities+Coping_spiritual
"

sat.fit <- sem(depression_model, data= okDat, std.lv=TRUE)
summary(sat.fit, fit.measures= TRUE)
sem$fit

lavInspect(sat.fit, "cov.lv") 

semPaths(sat.fit, layout="tree2", nCharNodes = 0, residual=FALSE,
         whatLabels = "est", edge.label.cex = 1, node.label.cex = 1, 
         label.prop=0.9, edge.label.color = "black", rotation = 2, 
         equalizeManifests = TRUE, optimizeLatRes = TRUE, node.width = 2, 
         edge.width = 1.5, shapeMan = "rectangle", shapeLat = "circle", 
         shapeInt = "triangle", sizeMan = 5, sizeInt = 5, sizeLat = 5, 
         curve=2, unCol = "black", structural=TRUE, 
         filetype = "pdf", width = 8, height = 6, filename = "SEM Depression v2")
