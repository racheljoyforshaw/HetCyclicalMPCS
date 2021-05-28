
familyPanelSurvey03 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f03,
                                     nest=TRUE)

familyPanelSurvey05 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight05,
                                 data=f05,
                                 nest=TRUE)

familyPanelSurvey07 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight07,
                                 data=f07,
                                 nest=TRUE)

familyPanelSurvey09 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f09,
                                     nest=TRUE)

familyPanelSurvey11 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight11,
                                 data=f11,
                                 nest=TRUE)

familyPanelSurvey13 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight13,
                                 data=f13,
                                 nest=TRUE)


write(toString(round(svymean(~consumption03,familyPanelSurvey03, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons03.txt"))
write(toString(round(svymean(~consumption05,familyPanelSurvey05, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons05.txt"))
write(toString(round(svymean(~consumption07,familyPanelSurvey07, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons07.txt"))

write(toString(round(svymean(~consumption09,familyPanelSurvey09, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons09.txt"))
write(toString(round(svymean(~consumption11,familyPanelSurvey11, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons11.txt"))
write(toString(round(svymean(~consumption13,familyPanelSurvey13, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons13.txt"))



write(toString(round(svymean(~consumption_plus05,familyPanelSurvey05, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons_plus05.txt"))
write(toString(round(svymean(~consumption_plus07,familyPanelSurvey07, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons_plus07.txt"))

write(toString(round(svymean(~consumption_plus09,familyPanelSurvey09, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons_plus09.txt"))
write(toString(round(svymean(~consumption_plus11,familyPanelSurvey11, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons_plus11.txt"))
write(toString(round(svymean(~consumption_plus13,familyPanelSurvey13, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/cons_plus13.txt"))



familyPanelSurvey03 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight03,
                                 data=f030507,
                                 nest=TRUE)

familyPanelSurvey05 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight05,
                                 data=f030507,
                                 nest=TRUE)

familyPanelSurvey07 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight07,
                                 data=f030507,
                                 nest=TRUE)

familyPanelSurvey09 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight09,
                                 data=f091113,
                                 nest=TRUE)

familyPanelSurvey11 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight11,
                                 data=f091113,
                                 nest=TRUE)

familyPanelSurvey13 <- svydesign(id=~primarySamplingUnit,
                                 strat=~stratification,
                                 weights=~longWeight13,
                                 data=f091113,
                                 nest=TRUE)

write(toString(round(svymean(~impConsumption03,familyPanelSurvey03, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/imp_cons03.txt"))
write(toString(round(svymean(~impConsumption05,familyPanelSurvey05, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/imp_cons05.txt"))
write(toString(round(svymean(~impConsumption07,familyPanelSurvey07, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/imp_cons07.txt"))

write(toString(round(svymean(~impConsumption09,familyPanelSurvey09, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/imp_cons09.txt"))
write(toString(round(svymean(~impConsumption11,familyPanelSurvey11, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/imp_cons11.txt"))
write(toString(round(svymean(~impConsumption13,familyPanelSurvey13, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/imp_cons13.txt"))




# income

write(toString(round(svymean(~income03,familyPanelSurvey03, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/inc03.txt"))
write(toString(round(svymean(~income05,familyPanelSurvey05, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/inc05.txt"))
write(toString(round(svymean(~income07,familyPanelSurvey07, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/inc07.txt"))

write(toString(round(svymean(~income09,familyPanelSurvey09, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/inc09.txt"))
write(toString(round(svymean(~income11,familyPanelSurvey11, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/inc11.txt"))
write(toString(round(svymean(~income13,familyPanelSurvey13, na.rm=TRUE),0)),file=paste0(getwd(),"/Results/inc13.txt"))


