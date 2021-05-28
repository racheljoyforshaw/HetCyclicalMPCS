# Do the rich save more? Dynan, Skinner, Zeldes 2004 JPE
# tracing out naive mps (active saving vs. income, total wealth change vs. income), mps (consumption vs.income)
#https://www.journals.uchicago.edu/doi/pdfplus/10.1086/381475

f030507_DSZ <-f030507
f091113_DSZ <-f091113


# variables to keep

f030507_DSZ_keeps <-
  c('uniqueID','stratification','primarySamplingUnit','longWeight03','consumption03','consumption05','consumption07','income03','income05','income07','totalWealth03','totalWealth07','activeSaving03','activeSaving05','activeSaving07','capChange03','capChange05','capChange07')

f091113_DSZ_keeps <-
  c('uniqueID','stratification','primarySamplingUnit','longWeight09','consumption09','consumption11','consumption13','income09','income11','income13','totalWealth09','totalWealth13','activeSaving09','activeSaving11','activeSaving13','capChange09','capChange11','capChange13')

f030507_DSZ <- f030507_DSZ[,f030507_DSZ_keeps]
f091113_DSZ <- f091113_DSZ[,f091113_DSZ_keeps]

rm(f030507_DSZ_keeps,f091113_DSZ_keeps)
# drop NA 
f030507_DSZ <- na.omit(f030507_DSZ)
f091113_DSZ <- na.omit(f091113_DSZ)


familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507_DSZ,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113_DSZ,
                                     nest=TRUE)



svy_quantiles03 <- svyquantile(~income03,familyPanelSurvey030507, seq(0, 1, 0.2))
svy_quantiles09 <- svyquantile(~income09,familyPanelSurvey091113, seq(0, 1, 0.2))

f030507_DSZ$svy_quantile <- ifelse(f030507_DSZ$income03<svy_quantiles03[2],1,
                               ifelse(f030507_DSZ$income03>=svy_quantiles03[2] & f030507_DSZ$income03<svy_quantiles03[3],2,
                                      ifelse(f030507_DSZ$income03>=svy_quantiles03[3] & f030507_DSZ$income03<svy_quantiles03[4],3,
                                             ifelse(f030507_DSZ$income03>=svy_quantiles03[4]& f030507_DSZ$income03<svy_quantiles03[5],4,
                                                    ifelse(f030507_DSZ$income03>=svy_quantiles03[5],5,NA)))))

f091113_DSZ$svy_quantile <- ifelse(f091113_DSZ$income09<svy_quantiles09[2],1,
                               ifelse(f091113_DSZ$income09>=svy_quantiles09[2] & f091113_DSZ$income09<svy_quantiles09[3],2,
                                      ifelse(f091113_DSZ$income09>=svy_quantiles09[3] & f091113_DSZ$income09<svy_quantiles09[4],3,
                                             ifelse(f091113_DSZ$income09>=svy_quantiles03[4]& f091113_DSZ$income09<svy_quantiles09[5],4,
                                                    ifelse(f091113_DSZ$income09>=svy_quantiles09[5],5,NA)))))

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507_DSZ,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113_DSZ,
                                     nest=TRUE)

rm(svy_quantiles03,svy_quantiles09)

# total wealth is 03->07 (4 years)
# income is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
for (j in c("030507","091113")){
  print("Total Wealth")
  print(j)
  for (i in c("1","2","3","4","5")){
    assign(paste0("DSZtotalWealth_",j,"_q",i),
           ((svymean(~eval(as.name(paste0("totalWealth",substr(j,5,6)))),
                     subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)
             -svymean(~eval(as.name(paste0("totalWealth",substr(j,1,2)))),
                      subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/4)/
             ((svymean(~eval(as.name(paste0("income",substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3))
    print(paste0("q",i,":",(unname(eval(as.name(paste0("DSZtotalWealth_",j,"_q",i)))[1]))))
  }
}

# active saving is 01->03 + 03->05 + 05->07 = 01->07 (6 years)
# income is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
for (j in c("030507","091113")){
  print("Active Saving")
  print(j)
  for (i in c("1","2","3","4","5")){
    assign(paste0("DSZactiveSaving_",j,"_q",i),
           ((svymean(~eval(as.name(paste0("activeSaving",substr(j,1,2)))),
                     subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)
             +svymean(~eval(as.name(paste0("activeSaving",substr(j,3,4)))),
                     subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)
             +svymean(~eval(as.name(paste0("activeSaving",substr(j,5,6)))),
                      subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3)/
             ((svymean(~eval(as.name(paste0("income",substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3))
    print(paste0("q",i,":",(unname(eval(as.name(paste0("DSZactiveSaving_",j,"_q",i)))[1]))))
  }
}


# capital change is 01->03 + 03->05 + 05->07 = 01->07 (6 years)
# income is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
for (j in c("030507","091113")){
  print("Capital Change")
  print(j)
  for (i in c("1","2","3","4","5")){
    assign(paste0("DSZcapChange_",j,"_q",i),
           ((svymean(~eval(as.name(paste0("capChange",substr(j,1,2)))),
                     subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)
             +svymean(~eval(as.name(paste0("capChange",substr(j,3,4)))),
                      subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)
             +svymean(~eval(as.name(paste0("capChange",substr(j,5,6)))),
                      subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3)/
             ((svymean(~eval(as.name(paste0("income",substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3))
    print(paste0("q",i,":",(unname(eval(as.name(paste0("DSZcapChange_",j,"_q",i)))[1]))))
  }
}

# consumption is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
# income is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
for (j in c("030507","091113")){
  print("Consumption")
  print(j)
  for (i in c("1","2","3","4","5")){
    assign(paste0("DSZconsumption_",j,"_q",i),
           ((svymean(~eval(as.name(paste0("consumption",substr(j,5,6)))),
                     subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
               svymean(~eval(as.name(paste0("consumption",substr(j,3,4)))),
                       subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
               svymean(~eval(as.name(paste0("consumption",substr(j,1,2)))),
                       subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3)/
             ((svymean(~eval(as.name(paste0("income",substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3))
    print(paste0("q",i,":",(unname(eval(as.name(paste0("DSZconsumption_",j,"_q",i)))[1]))))
  }
}

     

rm(familyPanelSurvey030507,familyPanelSurvey091113)
rm(i,j)
