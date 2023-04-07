#----Install and/or load needed libraries----#
install.packages (c(" lavaan", "semTools", "semPlot"))
library(lavaan)
library(semTools)
library(semPlot)
library(psych)

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
