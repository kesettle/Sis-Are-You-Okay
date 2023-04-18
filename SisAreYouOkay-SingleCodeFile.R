##############################
#----------PACKAGES----------#
##############################
#Install packages as needed
install.packages (c("GPArotation", "lavaan","nFactors", "parameters", "performance", "psych",
                    "semPlot", "semTools", "tidyverse"))

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
                                     ends_with(c("_Total_Score","_SUM", "_Coping", "_text"))))

#CURRENT DIMENSIONS: (-371) 2260 obs. of (-88) 68 variables

#Transformations
finiDat <- closedDat %>%
  mutate(Age = as.integer(Age)) %>%
  rename(Academia_status = Q102)

#All NA remaining NA values filtered out
okDat <- na.omit(finiDat)

#FINAL DIMENSIONS: (-810) 1450 obs. of (-88) 67 variables

#################################################
#----------EXPLORATORY FACTOR ANALYSIS----------#
#################################################
#-----Checking that dataset is suitable for factor analysis-----#
#Yes, very much so!
performance::check_factorstructure(okDat)

#-----Find principal components-----#
#Response(s) must be removed
pcDat <- okDat[,-c(34,67)]

PCs <- prcomp(~ ., data = pcDat, scale = TRUE)
summary(PCs)


#Contribution of variance levels out at around 4 to 5 PCs; may consider no more than that
plot(PCs, type = 'l')

par(mfrow = c(1, 2))
plot(PCs$sdev^2/sum(PCs$sdev^2), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
abline(v=15)
plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component",
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')
abline(v = c(5,10,15), h = c(0.4,0.5,0.6))

#-----Estimate number of factors to retain with scree test-----#
#15 seems to be the majority
okScr <- nScree(pcDat)
okScr

#Examine the eigenvalues

#The first 15 are > 1.0, so this suggests about 15 factors; the first 5 >> 1.0,
#which is consistent with the PCA
round(eigen(cor(pcDat))$values, 4)

#-----Estimate and Compare EFA models with k = 5 vs 15 factors-----#

okEFA15 <- factanal(pcDat, factors = 15, scores = "regression", rotation = "oblimin")
#EFAPromax <- factanal(pcDat, factors = 15, scores = "regression", rotation = "promax")

print(okEFA15$loadings, sort = TRUE, cutoff = 0.3)

#-----Fit EFA using psych package-----#
faOK <- fa(pcDat, nfactors = 15) %>%
  model_parameters(sort = TRUE, threshold = "max")

#Determining number of factors to retain based on various methods/metrics
n <- n_factors(pcDat)

plot(n) + see::theme_modern()

###################################################
#----------STRUCTURAL EQUATIONS MODELING----------#
###################################################
#----Data Checks ----#
describe(okDat)

#----Test Models for Structural Equation Modeling (SEM)----#
full_model <- "
Factor1  =~ Factor9 + Covid_wellbeing_financial_recode + Covid_wellbeing_social_recode + Covid_Wellbeing_Physical_recode + Covid_wellbeing_overall_recode + Covid_wellbeing_psychological_recode + Covid_wellbeing_Emotional_recode 

Factor3  =~ Factor1 + Factor4 + Factor5 + Factor7 + Factor11 + Factor12 + PHQ9_Q6_Feeling_bad_about_self_recode + PHQ9_Q10_Impact_of_Problems_recode + PHQ9_Q2_Feeling_down_recode + PHQ9_Q7_Trouble_concentrating_recode + PHQ9_Q5_Poor_appetite_overeating_recode + PHQ9_Q3_Trouble_sleep_recode + PHQ9_Q4_tired_low_energy_recode

Factor4  =~ Factor6 + Prior_Covid_Racism + Covid_Racism_increased + Followed_Protests_recode + Protests_appropriate_response_recode + Protests_sympathy_recode + Protests_support_recode 

Factor5  =~ Factor7 + Factor14 + Coping_spiritual + Coping_pleasurable_activities + Coping_outdoors + Coping_exercise + Coping_nutrition
Factor6  =~ Prior_Covid_experienced_racial_discrimination_recode + Covid_experienced_racial_discrimination_recode
Factor7  =~ Factor14 + Coping_emotional_support + Coping_optimism + Coping_not_show_emotions + Coping_humor + Coping_distract_work + Coping_movies_TV_activities

Factor8  =~ Number_adults_household + Number_household
Factor9  =~ Covid_death + Covid_family_Friends + Covid_dx + Covid_isolate
Factor10 =~ Protests_police_good_job_recode + Approval_Trump_Protests_recode
Factor11 =~ Factor10 + Factor13 + PHQ9_Q9_SI_Thoughts_recode + Protest_Participation + Find_out_study + Marital_status + Sexual_Orientation + Political_views + Academia_status

Factor12 =~ Factor8 + Factor15 + Education + Income
Factor13 =~ Covid_Vaccine + Political_affliation + Vote_2020
Factor14 =~ GAD7_Q5_restless_recode + PHQ9_Q8_thinking_slowly_fidgety_recode + Coping_alcohol_drugs
Factor15 =~ Employment_status

"
sat.fit <- sem(full_model, data= okDat, std.lv=TRUE)
lavInspect(sat.fit, "cov.lv") 
summary(sat.fit, fit.measures= TRUE)

semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE, layout="tree2", structural=TRUE, nCharNodes=12, edge.label.cex=0.5)

full_model2 <- "
Factor1  =~ Covid_wellbeing_financial_recode + Covid_wellbeing_social_recode + Covid_Wellbeing_Physical_recode + Covid_wellbeing_overall_recode + Covid_wellbeing_psychological_recode + Covid_wellbeing_Emotional_recode 

Factor3  =~ Factor1 + Factor4 + Factor5 + Factor6 + Factor7 + Factor8 + Factor9 + Factor10 + Factor11 + Factor12 + Factor13 + Factor14 + Factor15 + PHQ9_Q6_Feeling_bad_about_self_recode + PHQ9_Q10_Impact_of_Problems_recode + PHQ9_Q2_Feeling_down_recode + PHQ9_Q7_Trouble_concentrating_recode + PHQ9_Q5_Poor_appetite_overeating_recode + PHQ9_Q3_Trouble_sleep_recode + PHQ9_Q4_tired_low_energy_recode

Factor4  =~ Prior_Covid_Racism + Covid_Racism_increased + Followed_Protests_recode + Protests_appropriate_response_recode + Protests_sympathy_recode + Protests_support_recode 

Factor5  =~ Coping_spiritual + Coping_pleasurable_activities + Coping_outdoors + Coping_exercise + Coping_nutrition
Factor6  =~ Prior_Covid_experienced_racial_discrimination_recode + Covid_experienced_racial_discrimination_recode
Factor7  =~ Coping_emotional_support + Coping_optimism + Coping_not_show_emotions + Coping_humor + Coping_distract_work + Coping_movies_TV_activities

Factor8  =~ Number_adults_household + Number_household
Factor9  =~ Covid_death + Covid_family_Friends + Covid_dx + Covid_isolate
Factor10 =~ Protests_police_good_job_recode + Approval_Trump_Protests_recode
Factor11 =~ PHQ9_Q9_SI_Thoughts_recode + Protest_Participation + Find_out_study + Marital_status + Sexual_Orientation + Political_views + Academia_status

Factor12 =~ Education + Income
Factor13 =~ Covid_Vaccine + Political_affliation + Vote_2020
Factor14 =~ GAD7_Q5_restless_recode + PHQ9_Q8_thinking_slowly_fidgety_recode + Coping_alcohol_drugs
Factor15 =~ Employment_status

"
sat.fit <- sem(full_model2, data= okDat, std.lv=TRUE)
summary(sat.fit, fit.measures= TRUE)

semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE, layout="tree", structural=TRUE, nCharNodes=12, edge.label.cex=0.5)

alt_model <- "
Factor3  =~ Factor5 + Factor6 + Factor8 + Factor12 + Factor13 + PHQ9_Q6_Feeling_bad_about_self_recode + PHQ9_Q10_Impact_of_Problems_recode + PHQ9_Q2_Feeling_down_recode + PHQ9_Q7_Trouble_concentrating_recode + PHQ9_Q5_Poor_appetite_overeating_recode + PHQ9_Q3_Trouble_sleep_recode + PHQ9_Q4_tired_low_energy_recode

Factor6  =~ Prior_Covid_experienced_racial_discrimination_recode + Covid_experienced_racial_discrimination_recode

Factor8  =~ Number_adults_household + Number_household
Factor12 =~ Education + Income
Factor13 =~ Covid_Vaccine + Political_affliation + Vote_2020

Factor5  =~ Coping_spiritual + Coping_pleasurable_activities + Coping_outdoors + Coping_exercise + Coping_nutrition
"

sat.fit <- sem(alt_model, data= okDat, std.lv=TRUE)
sem <- summary(sat.fit, fit.measures= TRUE)
sem$fit

semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE, layout="tree2", structural=TRUE, nCharNodes=12, edge.label.cex=1)

#final depression model
alt_model2 <- "
Factor3  =~ Factor7 + PHQ9_Q6_Feeling_bad_about_self_recode + PHQ9_Q10_Impact_of_Problems_recode + PHQ9_Q2_Feeling_down_recode + PHQ9_Q7_Trouble_concentrating_recode + PHQ9_Q5_Poor_appetite_overeating_recode + PHQ9_Q3_Trouble_sleep_recode + PHQ9_Q4_tired_low_energy_recode

Factor7  =~ Coping_emotional_support + Coping_optimism + Coping_not_show_emotions + Coping_humor + Coping_distract_work + Coping_movies_TV_activities
Factor14 =~  Factor3 + GAD7_Q5_restless_recode + PHQ9_Q8_thinking_slowly_fidgety_recode + Coping_alcohol_drugs

Factor1  =~ Factor14 + Covid_wellbeing_financial_recode + Covid_wellbeing_social_recode + Covid_Wellbeing_Physical_recode + Covid_wellbeing_overall_recode + Covid_wellbeing_psychological_recode + Covid_wellbeing_Emotional_recode 

Factor6  =~ Factor14 + Prior_Covid_experienced_racial_discrimination_recode + Covid_experienced_racial_discrimination_recode
"

sat.fit <- sem(alt_model2, data= okDat, std.lv=TRUE)
sem <- summary(sat.fit, fit.measures= TRUE)
sem$fit

semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE, layout="tree2", structural=TRUE, nCharNodes=12, edge.label.cex=1)

#final anxiety model
alt_model3 <- "
Factor2 =~ Factor5 + GAD7_Q6_irritable_recode + GAD7_Q1_Feeling_nervous_Recode + GAD7_Q4_trouble_relaxing_recode + GAD7_Q7_afraid_something_bad_recode

Factor5  =~ Coping_spiritual + Coping_pleasurable_activities + Coping_outdoors + Coping_exercise + Coping_nutrition

Factor6  =~ Factor2 + Prior_Covid_experienced_racial_discrimination_recode + Covid_experienced_racial_discrimination_recode

Factor9  =~ Factor2 + Covid_death + Covid_family_Friends + Covid_dx + Covid_isolate

Factor13 =~ Factor2 + Covid_Vaccine + Political_affliation + Vote_2020

"
sat.fit <- sem(alt_model3, data= okDat, std.lv=TRUE)
sem <- summary(sat.fit, fit.measures= TRUE)
sem$fit

semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE, layout="tree2", structural=TRUE, nCharNodes=12, edge.label.cex=1)


#----Structural Equation Modeling (SEM) Final Models----#
#full model
full_model <- "
Pandemic_Impact  =~ Covid_Impact + Covid_wellbeing_financial_recode + Covid_wellbeing_social_recode + Covid_Wellbeing_Physical_recode + Covid_wellbeing_overall_recode + Covid_wellbeing_psychological_recode + Covid_wellbeing_Emotional_recode 

Depression_Anxeity  =~ Pandemic_Impact + Racism_Sentiment + Healthy_Coping + Psychological_Coping + Personal_ID + Socioeconomic_Level + PHQ9_Q6_Feeling_bad_about_self_recode + PHQ9_Q10_Impact_of_Problems_recode + PHQ9_Q2_Feeling_down_recode + PHQ9_Q7_Trouble_concentrating_recode + PHQ9_Q5_Poor_appetite_overeating_recode + PHQ9_Q3_Trouble_sleep_recode + PHQ9_Q4_tired_low_energy_recode

Racism_Sentiment  =~ Exp_Racism + Prior_Covid_Racism + Covid_Racism_increased + Followed_Protests_recode + Protests_appropriate_response_recode + Protests_sympathy_recode + Protests_support_recode 

Healthy_Coping  =~ Psychological_Coping + Restless + Coping_spiritual + Coping_pleasurable_activities + Coping_outdoors + Coping_exercise + Coping_nutrition
Exp_Racism  =~ Prior_Covid_experienced_racial_discrimination_recode + Covid_experienced_racial_discrimination_recode
Psychological_Coping  =~ Restless + Coping_emotional_support + Coping_optimism + Coping_not_show_emotions + Coping_humor + Coping_distract_work + Coping_movies_TV_activities

Household_Size  =~ Number_adults_household + Number_household
Covid_Impact  =~ Covid_death + Covid_family_Friends + Covid_dx + Covid_isolate
Govt_Sentiment =~ Protests_police_good_job_recode + Approval_Trump_Protests_recode
Personal_ID =~ Govt_Sentiment + Politics_Descisions + PHQ9_Q9_SI_Thoughts_recode + Protest_Participation + Find_out_study + Marital_status + Sexual_Orientation + Political_views + Academia_status

Socioeconomic_Level =~ Household_Size + Employment_Status + Education + Income
Politics_Descisions =~ Covid_Vaccine + Political_affliation + Vote_2020

Restless =~ GAD7_Q5_restless_recode + PHQ9_Q8_thinking_slowly_fidgety_recode + Coping_alcohol_drugs
Employment_Status =~ Employment_status
"

#final depression model
alt_model2 <- "
Depression_Shame  =~ Psychological_Coping + PHQ9_Q6_Feeling_bad_about_self_recode + PHQ9_Q10_Impact_of_Problems_recode + PHQ9_Q2_Feeling_down_recode + PHQ9_Q7_Trouble_concentrating_recode + PHQ9_Q5_Poor_appetite_overeating_recode + PHQ9_Q3_Trouble_sleep_recode + PHQ9_Q4_tired_low_energy_recode

Psychological_Coping =~ Coping_emotional_support + Coping_optimism + Coping_not_show_emotions + Coping_humor + Coping_distract_work + Coping_movies_TV_activities
Restless =~  Depression_Shame + GAD7_Q5_restless_recode + PHQ9_Q8_thinking_slowly_fidgety_recode + Coping_alcohol_drugs

Pandemic_Impact  =~ Restless + Covid_wellbeing_financial_recode + Covid_wellbeing_social_recode + Covid_Wellbeing_Physical_recode + Covid_wellbeing_overall_recode + Covid_wellbeing_psychological_recode + Covid_wellbeing_Emotional_recode 

Exp_Racism  =~ Restless + Prior_Covid_experienced_racial_discrimination_recode + Covid_experienced_racial_discrimination_recode
"

#final anxiety model
alt_model3 <- "
Anxiety_Uneasy =~ Psychological_Coping + GAD7_Q6_irritable_recode + GAD7_Q1_Feeling_nervous_Recode + GAD7_Q4_trouble_relaxing_recode + GAD7_Q7_afraid_something_bad_recode

Psychological_Coping =~ Coping_emotional_support + Coping_optimism + Coping_not_show_emotions + Coping_humor + Coping_distract_work + Coping_movies_TV_activities

Exp_Racism  =~ Anxiety_Uneasy + Prior_Covid_experienced_racial_discrimination_recode + Covid_experienced_racial_discrimination_recode

Covid_Impact =~ Anxiety_Uneasy + Covid_death + Covid_family_Friends + Covid_dx + Covid_isolate

Political_Decisions =~ Anxiety_Uneasy + Covid_Vaccine + Political_affliation + Vote_2020
"
sem_full <- sem(full_model, data= okDat, std.lv=TRUE)
sem_depression <- sem(alt_model2, data= okDat, std.lv=TRUE)
sem_anxiety <- sem(alt_model3, data= okDat, std.lv=TRUE)

#summary(compareFit(sem_full, sem_depression, sem_anxiety, nested= TRUE))

semPaths(sem_depression, layout="tree2", nCharNodes = 0, residual=FALSE,
         whatLabels = "est", edge.label.cex = 1, node.label.cex = 1, 
         label.prop=0.9, edge.label.color = "black", rotation = 2, 
         equalizeManifests = TRUE, optimizeLatRes = TRUE, node.width = 2, 
         edge.width = 1.5, shapeMan = "rectangle", shapeLat = "circle", 
         shapeInt = "triangle", sizeMan = 5, sizeInt = 5, sizeLat = 5, 
         curve=2, unCol = "black", structural=TRUE, 
         filetype = "pdf", width = 8, height = 6, filename = "SEM Depression")

semPaths(sem_anxiety, layout="tree2", nCharNodes = 0, residual=FALSE,
         whatLabels = "est", edge.label.cex = 1, node.label.cex = 1, 
         label.prop=0.9, edge.label.color = "black", rotation = 2, 
         equalizeManifests = TRUE, optimizeLatRes = TRUE, node.width = 2, 
         edge.width = 1.5, shapeMan = "rectangle", shapeLat = "circle", 
         shapeInt = "triangle", sizeMan = 5, sizeInt = 5, sizeLat = 5, 
         curve=2, unCol = "black", structural=TRUE,
         filetype = "pdf", width = 8, height = 6, filename = "SEM Anxiety")
