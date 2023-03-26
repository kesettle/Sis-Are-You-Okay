library(nFactors)
library(sem)
library(semPlot)
library(GPArotation)
library(tidyverse)



######################################
##Different rotations for 15 factors##
######################################
noneRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "none")

#oblique rotations:
obliminRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "oblimin")
#oblimaxRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "oblimax")
#quartiminRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "quartimin")
#simplimaxRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "simplimax")

#orthogonal rotations
#variminRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "varimin")
varimaxRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "varimax")
quartimaxRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "quartimax")
#parsimaxRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "parsimax")
#equamaxRot <- factanal(pcDat, factors = 15, scores = "regression", rotation = "equamax")

#write lambda matrices to CSV
write.csv(as.data.frame(noneRot$loadings[1:65,1:15]), "none.csv")
write.csv(as.data.frame(obliminRot$loadings[1:65,1:15]), "oblimin.csv")
#write.csv(as.data.frame(oblimaxRot$loadings[1:65,1:15]), "oblimax.csv")
#write.csv(as.data.frame(quartiminRot$loadings[1:65,1:15]), "quartimin.csv")
#write.csv(as.data.frame(simplimaxRot$loadings[1:65,1:15]), "simplimax.csv")
#write.csv(as.data.frame(variminRot$loadings[1:65,1:15]), "varimin.csv")
write.csv(as.data.frame(varimaxRot$loadings[1:65,1:15]), "varimax.csv")
write.csv(as.data.frame(quartimaxRot$loadings[1:65,1:15]), "quartimax.csv")
#write.csv(as.data.frame(parsimaxRot$loadings[1:65,1:15]), "parsimax.csv")
#write.csv(as.data.frame(equamaxRot$loadings[1:65,1:15]), "equamax.csv")

#Factor correlation maps for various rotations
corrplot::corrplot(cor(noneRot$loadings), title = "No Rotation")
corrplot::corrplot(cor(obliminRot$loadings), title = "Oblimin Rotation")
corrplot::corrplot(cor(varimaxRot$loadings), title = "Varimax Rotation")
corrplot::corrplot(cor(quartimaxRot$loadings), title = "Quartimax Rotation")
### oblimin = quartimax; according to ST 537 notes, loadings matrix not unique, so no surprises



##IN PROGRESS##
#path diagrams for SEM
#nonePath <- specifyModel(text = "
#                          Factor1 ->  Covid_isolate,                      -0.247, NA
#                          Factor1 ->  Protest_Participation,              -0.276, NA
#                          Factor1 ->  Coping_distract_work,               0.249,  NA
#                          Factor1 ->  Coping_alcohol_drugs,               0.405,  NA
#                          Factor1 ->  Coping_humor,                       0.376,  NA
#                          Factor1 ->  Coping_movies_TV_activities,        0.298,  NA
#                          Factor1 ->  Coping_not_show_emotions,           0.35,   NA
#                          Factor1 ->  GAD7_Q1_Feeling_nervous_Recode,     0.749,  NA
#                          Factor1 ->  GAD7_Q2_cannot_stop_worrying_recode,0.726,  NA
#                          Factor1 ->  GAD7_Q3_worrying_too_much_recode,   0.675,  NA
#                          Factor1 ->  GAD7_Q4_trouble_relaxing_recode
#                          Factor1 ->  GAD7_Q5_restless_recode
#                          Factor1 ->  GAD7_Q6_irritable_recode
#                          Factor1 ->  GAD7_Q7_afraid_something_bad_recode
#                          Factor1 ->  PHQ9_Q2_Feeling_down_recode
#                          Factor1 ->  PHQ9_Q3_Trouble_sleep_recode
#                          Factor1 ->  PHQ9_Q4_tired_low_energy_recode
#                          Factor1 ->  PHQ9_Q5_Poor_appetite_overeating_recode
#                          Factor1 ->  PHQ9_Q6_Feeling_bad_about_self_recode
#                          Factor1 ->  PHQ9_Q7_Trouble_concentrating_recode
#                          Factor1 ->  PHQ9_Q8_thinking_slowly_fidgety_recode
#                          Factor1 ->  PHQ9_Q9_SI_Thoughts_recode
#                          Factor1 ->  PHQ9_Q10_Impact_of_Problems_recode
#                          Factor1 ->  Covid_wellbeing_overall_recode
#                          Factor1 ->  Covid_Wellbeing_Physical_recode
#                          Factor1 ->  Covid_wellbeing_Emotional_recode
#                          Factor1 ->  Covid_wellbeing_psychological_recode
#                          Factor1 ->  Covid_wellbeing_social_recode
#                          Factor1 ->  Covid_wellbeing_financial_recode
#                          Factor1 ->  Prior_Covid_experienced_racial_discrimination_recode
#                          Factor1 ->  Covid_experienced_racial_discrimination_recode
#                          ")
#sem(nonePath, S=cor(pcDat))
