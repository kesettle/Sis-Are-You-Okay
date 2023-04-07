#----Install and/or load needed libraries----#
#install.packages (c(" lavaan", "semTools", "semPlot"))
library(lavaan)
library(semTools)
library(semptools)
library(semPlot)
library(psych)


#----Data Checks ----#

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
