familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113,
                                     nest=TRUE)



svy_quantiles03 <- svyquantile(~income03,familyPanelSurvey030507, seq(0, 1, 0.2),na.rm=TRUE)
svy_quantiles09 <- svyquantile(~income09,familyPanelSurvey091113, seq(0, 1, 0.2),na.rm=TRUE)

f030507$svy_quantile<- ifelse(f030507$income03<svy_quantiles03[2],1,
                              ifelse(f030507$income03>=svy_quantiles03[2] & f030507$income03<svy_quantiles03[3],2,
                                     ifelse(f030507$income03>=svy_quantiles03[3] & f030507$income03<svy_quantiles03[4],3,
                                            ifelse(f030507$income03>=svy_quantiles03[4]& f030507$income03<svy_quantiles03[5],4,
                                                   ifelse(f030507$income03>=svy_quantiles03[5],5,NA)))))

f091113$svy_quantile <- ifelse(f091113$income09<svy_quantiles09[2],1,
                               ifelse(f091113$income09>=svy_quantiles09[2] & f091113$income09<svy_quantiles09[3],2,
                                      ifelse(f091113$income09>=svy_quantiles09[3] & f091113$income09<svy_quantiles09[4],3,
                                             ifelse(f091113$income09>=svy_quantiles09[4]& f091113$income09<svy_quantiles09[5],4,
                                                    ifelse(f091113$income09>=svy_quantiles09[5],5,NA)))))

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113,
                                     nest=TRUE)



# plot 

for (k in c("impConsumption","income","activeSaving","capChange")){
  print(k)
  for (j in c("030507","091113")){
    print(j)
    for (i in c("1","2","3","4","5")){
      assign(paste0(k,"_",substr(j,1,2),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,1,2),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,1,2),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,3,4),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,3,4),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,3,4),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,5,6),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,5,6),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,5,6),"_q",i)))[1]))))
    }
  }
}



out_pct <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
                      "year" = c(2003,2003,2003,2003,2003,2005,2005,2005,2005,2005,2007,2007,2007,2007,2007,2009,2009,2009,2009,2009,2011,2011,2011,2011,2011,2013,2013,2013,2013,2013),
                      "impConsumption" = c(impConsumption_03_q1,impConsumption_03_q2,impConsumption_03_q3, impConsumption_03_q4, impConsumption_03_q5, 
                                        impConsumption_05_q1,impConsumption_05_q2,impConsumption_05_q3, impConsumption_05_q4, impConsumption_05_q5, 
                                        impConsumption_07_q1,impConsumption_07_q2,impConsumption_07_q3, impConsumption_07_q4, impConsumption_07_q5,
                                        impConsumption_09_q1,impConsumption_09_q2,impConsumption_09_q3, impConsumption_09_q4, impConsumption_09_q5, 
                                        impConsumption_11_q1,impConsumption_11_q2,impConsumption_11_q3, impConsumption_11_q4, impConsumption_11_q5, 
                                        impConsumption_13_q1,impConsumption_13_q2,impConsumption_13_q3, impConsumption_13_q4, impConsumption_13_q5),
                      "income" = c(income_03_q1,income_03_q2,income_03_q3, income_03_q4, income_03_q5, 
                                   income_05_q1,income_05_q2,income_05_q3, income_05_q4, income_05_q5, 
                                   income_07_q1,income_07_q2,income_07_q3, income_07_q4, income_07_q5,
                                   income_09_q1,income_09_q2,income_09_q3, income_09_q4, income_09_q5, 
                                   income_11_q1,income_11_q2,income_11_q3, income_11_q4, income_11_q5, 
                                   income_13_q1,income_13_q2,income_13_q3, income_13_q4, income_13_q5),
                      "activeSaving" = c(activeSaving_03_q1,activeSaving_03_q2,activeSaving_03_q3, activeSaving_03_q4, activeSaving_03_q5, 
                                         activeSaving_05_q1,activeSaving_05_q2,activeSaving_05_q3, activeSaving_05_q4, activeSaving_05_q5, 
                                         activeSaving_07_q1,activeSaving_07_q2,activeSaving_07_q3, activeSaving_07_q4, activeSaving_07_q5,
                                         activeSaving_09_q1,activeSaving_09_q2,activeSaving_09_q3, activeSaving_09_q4, activeSaving_09_q5, 
                                         activeSaving_11_q1,activeSaving_11_q2,activeSaving_11_q3, activeSaving_11_q4, activeSaving_11_q5, 
                                         activeSaving_13_q1,activeSaving_13_q2,activeSaving_13_q3, activeSaving_13_q4, activeSaving_13_q5),
                      "capChange" = c(capChange_03_q1,capChange_03_q2,capChange_03_q3, capChange_03_q4, capChange_03_q5, 
                                      capChange_05_q1,capChange_05_q2,capChange_05_q3, capChange_05_q4, capChange_05_q5, 
                                      capChange_07_q1,capChange_07_q2,capChange_07_q3, capChange_07_q4, capChange_07_q5,
                                      capChange_09_q1,capChange_09_q2,capChange_09_q3, capChange_09_q4, capChange_09_q5, 
                                      capChange_11_q1,capChange_11_q2,capChange_11_q3, capChange_11_q4, capChange_11_q5, 
                                      capChange_13_q1,capChange_13_q2,capChange_13_q3, capChange_13_q4, capChange_13_q5)

)

out_melt<-melt(out_pct, id=c("quantile","year"))
ggplot(data = out_melt, aes(x = quantile, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year) 






for (k in c("stocksAS","privateAnnuityAS","othRealEstateAS","farmBusinessAS","checkingAS","bondAS","vehicleAS","othDebtAS","housingAS")){
  print(k)
  for (j in c("030507","091113")){
    print(j)
    for (i in c("1","2","3","4","5")){
      assign(paste0(k,"_",substr(j,1,2),"_q",i),
             100*(svymean(~eval(as.name(paste0(k,substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)/ 
               svymean(~eval(as.name(paste0("income",substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i))))
      print(paste0(substr(j,1,2),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,1,2),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,3,4),"_q",i),
             100*(svymean(~eval(as.name(paste0(k,substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)/ 
               svymean(~eval(as.name(paste0("income",substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i))))
      print(paste0(substr(j,3,4),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,3,4),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,5,6),"_q",i),
             100*(svymean(~eval(as.name(paste0(k,substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)/ 
                    svymean(~eval(as.name(paste0("income",substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i))))
      print(paste0(substr(j,5,6),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,5,6),"_q",i)))[1]))))
    }
  }
}


out_pct <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
                      "year" = c(2003,2003,2003,2003,2003,2005,2005,2005,2005,2005,2007,2007,2007,2007,2007,2009,2009,2009,2009,2009,2011,2011,2011,2011,2011,2013,2013,2013,2013,2013),
                      "stocksAS" = c(stocksAS_03_q1,stocksAS_03_q2,stocksAS_03_q3, stocksAS_03_q4, stocksAS_03_q5, 
                                     stocksAS_05_q1,stocksAS_05_q2,stocksAS_05_q3, stocksAS_05_q4, stocksAS_05_q5, 
                                     stocksAS_07_q1,stocksAS_07_q2,stocksAS_07_q3, stocksAS_07_q4, stocksAS_07_q5,
                                     stocksAS_09_q1,stocksAS_09_q2,stocksAS_09_q3, stocksAS_09_q4, stocksAS_09_q5, 
                                     stocksAS_11_q1,stocksAS_11_q2,stocksAS_11_q3, stocksAS_11_q4, stocksAS_11_q5, 
                                     stocksAS_13_q1,stocksAS_13_q2,stocksAS_13_q3, stocksAS_13_q4, stocksAS_13_q5),
                      "privateAnnuityAS" = c(privateAnnuityAS_03_q1,privateAnnuityAS_03_q2,privateAnnuityAS_03_q3, privateAnnuityAS_03_q4, privateAnnuityAS_03_q5, 
                                             privateAnnuityAS_05_q1,privateAnnuityAS_05_q2,privateAnnuityAS_05_q3, privateAnnuityAS_05_q4, privateAnnuityAS_05_q5, 
                                             privateAnnuityAS_07_q1,privateAnnuityAS_07_q2,privateAnnuityAS_07_q3, privateAnnuityAS_07_q4, privateAnnuityAS_07_q5,
                                             privateAnnuityAS_09_q1,privateAnnuityAS_09_q2,privateAnnuityAS_09_q3, privateAnnuityAS_09_q4, privateAnnuityAS_09_q5, 
                                             privateAnnuityAS_11_q1,privateAnnuityAS_11_q2,privateAnnuityAS_11_q3, privateAnnuityAS_11_q4, privateAnnuityAS_11_q5, 
                                             privateAnnuityAS_13_q1,privateAnnuityAS_13_q2,privateAnnuityAS_13_q3, privateAnnuityAS_13_q4, privateAnnuityAS_13_q5),
                      "othRealEstateAS" = c(othRealEstateAS_03_q1,othRealEstateAS_03_q2,othRealEstateAS_03_q3, othRealEstateAS_03_q4, othRealEstateAS_03_q5, 
                                            othRealEstateAS_05_q1,othRealEstateAS_05_q2,othRealEstateAS_05_q3, othRealEstateAS_05_q4, othRealEstateAS_05_q5, 
                                            othRealEstateAS_07_q1,othRealEstateAS_07_q2,othRealEstateAS_07_q3, othRealEstateAS_07_q4, othRealEstateAS_07_q5,
                                            othRealEstateAS_09_q1,othRealEstateAS_09_q2,othRealEstateAS_09_q3, othRealEstateAS_09_q4, othRealEstateAS_09_q5, 
                                            othRealEstateAS_11_q1,othRealEstateAS_11_q2,othRealEstateAS_11_q3, othRealEstateAS_11_q4, othRealEstateAS_11_q5, 
                                            othRealEstateAS_13_q1,othRealEstateAS_13_q2,othRealEstateAS_13_q3, othRealEstateAS_13_q4, othRealEstateAS_13_q5),
                      "farmBusinessAS" = c(farmBusinessAS_03_q1,farmBusinessAS_03_q2,farmBusinessAS_03_q3, farmBusinessAS_03_q4, farmBusinessAS_03_q5, 
                                           farmBusinessAS_05_q1,farmBusinessAS_05_q2,farmBusinessAS_05_q3, farmBusinessAS_05_q4, farmBusinessAS_05_q5, 
                                           farmBusinessAS_07_q1,farmBusinessAS_07_q2,farmBusinessAS_07_q3, farmBusinessAS_07_q4, farmBusinessAS_07_q5,
                                           farmBusinessAS_09_q1,farmBusinessAS_09_q2,farmBusinessAS_09_q3, farmBusinessAS_09_q4, farmBusinessAS_09_q5, 
                                           farmBusinessAS_11_q1,farmBusinessAS_11_q2,farmBusinessAS_11_q3, farmBusinessAS_11_q4, farmBusinessAS_11_q5, 
                                           farmBusinessAS_13_q1,farmBusinessAS_13_q2,farmBusinessAS_13_q3, farmBusinessAS_13_q4, farmBusinessAS_13_q5),
                      "checkingAS" = c(checkingAS_03_q1,checkingAS_03_q2,checkingAS_03_q3, checkingAS_03_q4, checkingAS_03_q5, 
                                       checkingAS_05_q1,checkingAS_05_q2,checkingAS_05_q3, checkingAS_05_q4, checkingAS_05_q5, 
                                       checkingAS_07_q1,checkingAS_07_q2,checkingAS_07_q3, checkingAS_07_q4, checkingAS_07_q5,
                                       checkingAS_09_q1,checkingAS_09_q2,checkingAS_09_q3, checkingAS_09_q4, checkingAS_09_q5, 
                                       checkingAS_11_q1,checkingAS_11_q2,checkingAS_11_q3, checkingAS_11_q4, checkingAS_11_q5, 
                                       checkingAS_13_q1,checkingAS_13_q2,checkingAS_13_q3, checkingAS_13_q4, checkingAS_13_q5),
                      "bondAS" = c(bondAS_03_q1,bondAS_03_q2,bondAS_03_q3, bondAS_03_q4, bondAS_03_q5, 
                                   bondAS_05_q1,bondAS_05_q2,bondAS_05_q3, bondAS_05_q4, bondAS_05_q5, 
                                   bondAS_07_q1,bondAS_07_q2,bondAS_07_q3, bondAS_07_q4, bondAS_07_q5,
                                   bondAS_09_q1,bondAS_09_q2,bondAS_09_q3, bondAS_09_q4, bondAS_09_q5, 
                                   bondAS_11_q1,bondAS_11_q2,bondAS_11_q3, bondAS_11_q4, bondAS_11_q5, 
                                   bondAS_13_q1,bondAS_13_q2,bondAS_13_q3, bondAS_13_q4, bondAS_13_q5),
                      "vehicleAS" = c(vehicleAS_03_q1,vehicleAS_03_q2,vehicleAS_03_q3, vehicleAS_03_q4, vehicleAS_03_q5, 
                                      vehicleAS_05_q1,vehicleAS_05_q2,vehicleAS_05_q3, vehicleAS_05_q4, vehicleAS_05_q5, 
                                      vehicleAS_07_q1,vehicleAS_07_q2,vehicleAS_07_q3, vehicleAS_07_q4, vehicleAS_07_q5,
                                      vehicleAS_09_q1,vehicleAS_09_q2,vehicleAS_09_q3, vehicleAS_09_q4, vehicleAS_09_q5, 
                                      vehicleAS_11_q1,vehicleAS_11_q2,vehicleAS_11_q3, vehicleAS_11_q4, vehicleAS_11_q5, 
                                      vehicleAS_13_q1,vehicleAS_13_q2,vehicleAS_13_q3, vehicleAS_13_q4, vehicleAS_13_q5),
                      "othDebtAS" = c(othDebtAS_03_q1,othDebtAS_03_q2,othDebtAS_03_q3, othDebtAS_03_q4, othDebtAS_03_q5, 
                                      othDebtAS_05_q1,othDebtAS_05_q2,othDebtAS_05_q3, othDebtAS_05_q4, othDebtAS_05_q5, 
                                      othDebtAS_07_q1,othDebtAS_07_q2,othDebtAS_07_q3, othDebtAS_07_q4, othDebtAS_07_q5,
                                      othDebtAS_09_q1,othDebtAS_09_q2,othDebtAS_09_q3, othDebtAS_09_q4, othDebtAS_09_q5, 
                                      othDebtAS_11_q1,othDebtAS_11_q2,othDebtAS_11_q3, othDebtAS_11_q4, othDebtAS_11_q5, 
                                      othDebtAS_13_q1,othDebtAS_13_q2,othDebtAS_13_q3, othDebtAS_13_q4, othDebtAS_13_q5),
                      "housingAS" = c(housingAS_03_q1,housingAS_03_q2,housingAS_03_q3, housingAS_03_q4, housingAS_03_q5, 
                                      housingAS_05_q1,housingAS_05_q2,housingAS_05_q3, housingAS_05_q4, housingAS_05_q5, 
                                      housingAS_07_q1,housingAS_07_q2,housingAS_07_q3, housingAS_07_q4, housingAS_07_q5,
                                      housingAS_09_q1,housingAS_09_q2,housingAS_09_q3, housingAS_09_q4, housingAS_09_q5, 
                                      housingAS_11_q1,housingAS_11_q2,housingAS_11_q3, housingAS_11_q4, housingAS_11_q5, 
                                      housingAS_13_q1,housingAS_13_q2,housingAS_13_q3, housingAS_13_q4, housingAS_13_q5))
out_melt<-melt(out_pct, id=c("quantile","year"))
ggplot(data = out_melt, aes(x = quantile, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year) 



for (k in c("income","food", "housing","transportExp","childcare","healthExp","educExp")){
  print(k)
  for (j in c("030507","091113")){
    print(j)
    for (i in c("1","2","3","4","5")){
      if(j=="030507"){
        assign(paste0(k,"_","01_q",i),
               svymean(~eval(as.name(paste0(k,"01"))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      }
      else {
        assign(paste0(k,"_","07_q",i),
               svymean(~eval(as.name(paste0(k,"07"))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      }
      assign(paste0(k,"_",substr(j,1,2),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,1,2),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,1,2),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,3,4),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,3,4),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,3,4),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,5,6),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,5,6),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,5,6),"_q",i)))[1]))))
    }
  }
}



for (k in c("vacations","recreation","clothing")){
  print(k)
  for (j in c("030507","091113")){
    print(j)
    for (i in c("1","2","3","4","5")){
      if(j=="091113"){
        assign(paste0(k,"_",substr(j,1,2),"_q",i),
               svymean(~eval(as.name(paste0(k,substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
        print(paste0(substr(j,1,2),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,1,2),"_q",i)))[1]))))
      }
      assign(paste0(k,"_",substr(j,3,4),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,3,4),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,3,4),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,5,6),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,5,6),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,5,6),"_q",i)))[1]))))
    }
  }
}


out_pct <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
                      "year" = c(2003,2003,2003,2003,2003,2005,2005,2005,2005,2005,2007,2007,2007,2007,2007,2009,2009,2009,2009,2009,2011,2011,2011,2011,2011,2013,2013,2013,2013,2013),
                      "food" = c(food_03_q1/food_01_q1,food_03_q2/food_01_q2,food_03_q3/food_01_q3, food_03_q4/food_01_q4, food_03_q5/food_01_q5, 
                                 food_05_q1/food_03_q1,food_05_q2/food_03_q2,food_05_q3/food_03_q3, food_05_q4/food_03_q4, food_05_q5/food_03_q5, 
                                 food_07_q1/food_05_q1,food_07_q2/food_05_q2,food_07_q3/food_05_q3, food_07_q4/food_05_q4, food_07_q5/food_05_q5,
                                 food_09_q1/food_07_q1,food_09_q2/food_07_q2,food_09_q3/food_07_q3, food_09_q4/food_07_q4, food_09_q5/food_07_q5, 
                                 food_11_q1/food_09_q1,food_11_q2/food_09_q2,food_11_q3/food_09_q3, food_11_q4/food_09_q4, food_11_q5/food_09_q5, 
                                 food_13_q1/food_11_q1,food_13_q2/food_11_q2,food_13_q3/food_11_q3, food_13_q4/food_11_q4, food_13_q5/food_11_q5),
                      "housing" = c(housing_03_q1/housing_01_q1,housing_03_q2/housing_01_q2,housing_03_q3/housing_01_q3, housing_03_q4/housing_01_q4, housing_03_q5/housing_01_q5, 
                                    housing_05_q1/housing_03_q1,housing_05_q2/housing_03_q2,housing_05_q3/housing_03_q3, housing_05_q4/housing_03_q4, housing_05_q5/housing_03_q5, 
                                    housing_07_q1/housing_05_q1,housing_07_q2/housing_05_q2,housing_07_q3/housing_05_q3, housing_07_q4/housing_05_q4, housing_07_q5/housing_05_q5,
                                    housing_09_q1/housing_07_q1,housing_09_q2/housing_07_q2,housing_09_q3/housing_07_q3, housing_09_q4/housing_07_q4, housing_09_q5/housing_07_q5, 
                                    housing_11_q1/housing_09_q1,housing_11_q2/housing_09_q2,housing_11_q3/housing_09_q3, housing_11_q4/housing_09_q4, housing_11_q5/housing_09_q5, 
                                    housing_13_q1/housing_11_q1,housing_13_q2/housing_11_q2,housing_13_q3/housing_11_q3, housing_13_q4/housing_11_q4, housing_13_q5/housing_11_q5),
                      "transportExp" = c(transportExp_03_q1/transportExp_01_q1,transportExp_03_q2/transportExp_01_q2,transportExp_03_q3/transportExp_01_q3, transportExp_03_q4/transportExp_01_q4, transportExp_03_q5/transportExp_01_q5, 
                                         transportExp_05_q1/transportExp_03_q1,transportExp_05_q2/transportExp_03_q2,transportExp_05_q3/transportExp_03_q3, transportExp_05_q4/transportExp_03_q4, transportExp_05_q5/transportExp_03_q5, 
                                         transportExp_07_q1/transportExp_05_q1,transportExp_07_q2/transportExp_05_q2,transportExp_07_q3/transportExp_05_q3, transportExp_07_q4/transportExp_05_q4, transportExp_07_q5/transportExp_05_q5,
                                         transportExp_09_q1/transportExp_07_q1,transportExp_09_q2/transportExp_07_q2,transportExp_09_q3/transportExp_07_q3, transportExp_09_q4/transportExp_07_q4, transportExp_09_q5/transportExp_07_q5, 
                                         transportExp_11_q1/transportExp_09_q1,transportExp_11_q2/transportExp_09_q2,transportExp_11_q3/transportExp_09_q3, transportExp_11_q4/transportExp_09_q4, transportExp_11_q5/transportExp_09_q5, 
                                         transportExp_13_q1/transportExp_11_q1,transportExp_13_q2/transportExp_11_q2,transportExp_13_q3/transportExp_11_q3, transportExp_13_q4/transportExp_11_q4, transportExp_13_q5/transportExp_11_q5),
                      "childcare" = c(childcare_03_q1/childcare_01_q1,childcare_03_q2/childcare_01_q2,childcare_03_q3/childcare_01_q3, childcare_03_q4/childcare_01_q4, childcare_03_q5/childcare_01_q5, 
                                      childcare_05_q1/childcare_03_q1,childcare_05_q2/childcare_03_q2,childcare_05_q3/childcare_03_q3, childcare_05_q4/childcare_03_q4, childcare_05_q5/childcare_03_q5, 
                                      childcare_07_q1/childcare_05_q1,childcare_07_q2/childcare_05_q2,childcare_07_q3/childcare_05_q3, childcare_07_q4/childcare_05_q4, childcare_07_q5/childcare_05_q5,
                                      childcare_09_q1/childcare_07_q1,childcare_09_q2/childcare_07_q2,childcare_09_q3/childcare_07_q3, childcare_09_q4/childcare_07_q4, childcare_09_q5/childcare_07_q5, 
                                      childcare_11_q1/childcare_09_q1,childcare_11_q2/childcare_09_q2,childcare_11_q3/childcare_09_q3, childcare_11_q4/childcare_09_q4, childcare_11_q5/childcare_09_q5, 
                                      childcare_13_q1/childcare_11_q1,childcare_13_q2/childcare_11_q2,childcare_13_q3/childcare_11_q3, childcare_13_q4/childcare_11_q4, childcare_13_q5/childcare_11_q5),
                      "healthExp" = c(healthExp_03_q1/healthExp_01_q1,healthExp_03_q2/healthExp_01_q2,healthExp_03_q3/healthExp_01_q3, healthExp_03_q4/healthExp_01_q4, healthExp_03_q5/healthExp_01_q5, 
                                      healthExp_05_q1/healthExp_03_q1,healthExp_05_q2/healthExp_03_q2,healthExp_05_q3/healthExp_03_q3, healthExp_05_q4/healthExp_03_q4, healthExp_05_q5/healthExp_03_q5, 
                                      healthExp_07_q1/healthExp_05_q1,healthExp_07_q2/healthExp_05_q2,healthExp_07_q3/healthExp_05_q3, healthExp_07_q4/healthExp_05_q4, healthExp_07_q5/healthExp_05_q5,
                                      healthExp_09_q1/healthExp_07_q1,healthExp_09_q2/healthExp_07_q2,healthExp_09_q3/healthExp_07_q3, healthExp_09_q4/healthExp_07_q4, healthExp_09_q5/healthExp_07_q5, 
                                      healthExp_11_q1/healthExp_09_q1,healthExp_11_q2/healthExp_09_q2,healthExp_11_q3/healthExp_09_q3, healthExp_11_q4/healthExp_09_q4, healthExp_11_q5/healthExp_09_q5, 
                                      healthExp_13_q1/healthExp_11_q1,healthExp_13_q2/healthExp_11_q2,healthExp_13_q3/healthExp_11_q3, healthExp_13_q4/healthExp_11_q4, healthExp_13_q5/healthExp_11_q5),
                      "educExp" = c(educExp_03_q1/educExp_01_q1,educExp_03_q2/educExp_01_q2,educExp_03_q3/educExp_01_q3, educExp_03_q4/educExp_01_q4, educExp_03_q5/educExp_01_q5, 
                                    educExp_05_q1/educExp_03_q1,educExp_05_q2/educExp_03_q2,educExp_05_q3/educExp_03_q3, educExp_05_q4/educExp_03_q4, educExp_05_q5/educExp_03_q5, 
                                    educExp_07_q1/educExp_05_q1,educExp_07_q2/educExp_05_q2,educExp_07_q3/educExp_05_q3, educExp_07_q4/educExp_05_q4, educExp_07_q5/educExp_05_q5,
                                    educExp_09_q1/educExp_07_q1,educExp_09_q2/educExp_07_q2,educExp_09_q3/educExp_07_q3, educExp_09_q4/educExp_07_q4, educExp_09_q5/educExp_07_q5, 
                                    educExp_11_q1/educExp_09_q1,educExp_11_q2/educExp_09_q2,educExp_11_q3/educExp_09_q3, educExp_11_q4/educExp_09_q4, educExp_11_q5/educExp_09_q5, 
                                    educExp_13_q1/educExp_11_q1,educExp_13_q2/educExp_11_q2,educExp_13_q3/educExp_11_q3, educExp_13_q4/educExp_11_q4, educExp_13_q5/educExp_11_q5),
                      "vacations" = c(1,1,1, 1, 1, 
                                      1,1,1, 1, 1, 
                                      vacations_07_q1/vacations_05_q1,vacations_07_q2/vacations_05_q2,vacations_07_q3/vacations_05_q3, vacations_07_q4/vacations_05_q4, vacations_07_q5/vacations_05_q5,
                                      vacations_09_q1/vacations_07_q1,vacations_09_q2/vacations_07_q2,vacations_09_q3/vacations_07_q3, vacations_09_q4/vacations_07_q4, vacations_09_q5/vacations_07_q5, 
                                      vacations_11_q1/vacations_09_q1,vacations_11_q2/vacations_09_q2,vacations_11_q3/vacations_09_q3, vacations_11_q4/vacations_09_q4, vacations_11_q5/vacations_09_q5, 
                                      vacations_13_q1/vacations_11_q1,vacations_13_q2/vacations_11_q2,vacations_13_q3/vacations_11_q3, vacations_13_q4/vacations_11_q4, vacations_13_q5/vacations_11_q5),
                      "recreation" = c(1,1,1, 1, 1, 
                                       1,1,1, 1, 1, 
                                       recreation_07_q1/recreation_05_q1,recreation_07_q2/recreation_05_q2,recreation_07_q3/recreation_05_q3, recreation_07_q4/recreation_05_q4, recreation_07_q5/recreation_05_q5,
                                       recreation_09_q1/recreation_07_q1,recreation_09_q2/recreation_07_q2,recreation_09_q3/recreation_07_q3, recreation_09_q4/recreation_07_q4, recreation_09_q5/recreation_07_q5, 
                                       recreation_11_q1/recreation_09_q1,recreation_11_q2/recreation_09_q2,recreation_11_q3/recreation_09_q3, recreation_11_q4/recreation_09_q4, recreation_11_q5/recreation_09_q5, 
                                       recreation_13_q1/recreation_11_q1,recreation_13_q2/recreation_11_q2,recreation_13_q3/recreation_11_q3, recreation_13_q4/recreation_11_q4, recreation_13_q5/recreation_11_q5),
                      "clothing" = c(1,1,1, 1, 1, 
                                     1,1,1, 1, 1, 
                                     clothing_07_q1/clothing_05_q1,clothing_07_q2/clothing_05_q2,clothing_07_q3/clothing_05_q3, clothing_07_q4/clothing_05_q4, clothing_07_q5/clothing_05_q5,
                                     clothing_09_q1/clothing_07_q1,clothing_09_q2/clothing_07_q2,clothing_09_q3/clothing_07_q3, clothing_09_q4/clothing_07_q4, clothing_09_q5/clothing_07_q5, 
                                     clothing_11_q1/clothing_09_q1,clothing_11_q2/clothing_09_q2,clothing_11_q3/clothing_09_q3, clothing_11_q4/clothing_09_q4, clothing_11_q5/clothing_09_q5, 
                                     clothing_13_q1/clothing_11_q1,clothing_13_q2/clothing_11_q2,clothing_13_q3/clothing_11_q3, clothing_13_q4/clothing_11_q4, clothing_13_q5/clothing_11_q5)  
                      )
                      

out_melt<-melt(out_pct, id=c("quantile","year"))
ggplot(data = out_melt, aes(x = quantile, y =100*((value)-1), fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year) 




out_pct <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
                      "year" = c(2003,2003,2003,2003,2003,2005,2005,2005,2005,2005,2007,2007,2007,2007,2007,2009,2009,2009,2009,2009,2011,2011,2011,2011,2011,2013,2013,2013,2013,2013),
                      "food" = c(food_03_q1/income_03_q1,food_03_q2/income_03_q2,food_03_q3/income_03_q3, food_03_q4/income_03_q4, food_03_q5/income_03_q5, 
                                 food_05_q1/income_05_q1,food_05_q2/income_05_q2,food_05_q3/income_05_q3, food_05_q4/income_05_q4, food_05_q5/income_05_q5, 
                                 food_07_q1/income_07_q1,food_07_q2/income_07_q2,food_07_q3/income_07_q3, food_07_q4/income_07_q4, food_07_q5/income_07_q5,
                                 food_09_q1/income_09_q1,food_09_q2/income_09_q2,food_09_q3/income_09_q3, food_09_q4/income_09_q4, food_09_q5/income_09_q5, 
                                 food_11_q1/income_11_q1,food_11_q2/income_11_q2,food_11_q3/income_11_q3, food_11_q4/income_11_q4, food_11_q5/income_11_q5, 
                                 food_13_q1/income_13_q1,food_13_q2/income_13_q2,food_13_q3/income_13_q3, food_13_q4/income_13_q4, food_13_q5/income_13_q5),
                      "housing" = c(housing_03_q1/income_03_q1,housing_03_q2/income_03_q2,housing_03_q3/income_03_q3, housing_03_q4/income_03_q4, housing_03_q5/income_03_q5, 
                                    housing_05_q1/income_05_q1,housing_05_q2/income_05_q2,housing_05_q3/income_05_q3, housing_05_q4/income_05_q4, housing_05_q5/income_05_q5, 
                                    housing_07_q1/income_07_q1,housing_07_q2/income_07_q2,housing_07_q3/income_07_q3, housing_07_q4/income_07_q4, housing_07_q5/income_07_q5,
                                    housing_09_q1/income_09_q1,housing_09_q2/income_09_q2,housing_09_q3/income_09_q3, housing_09_q4/income_09_q4, housing_09_q5/income_09_q5, 
                                    housing_11_q1/income_11_q1,housing_11_q2/income_11_q2,housing_11_q3/income_11_q3, housing_11_q4/income_11_q4, housing_11_q5/income_11_q5, 
                                    housing_13_q1/income_13_q1,housing_13_q2/income_13_q2,housing_13_q3/income_13_q3, housing_13_q4/income_13_q4, housing_13_q5/income_13_q5),
                      "transportExp" = c(transportExp_03_q1/income_03_q1,transportExp_03_q2/income_03_q2,transportExp_03_q3/income_03_q3, transportExp_03_q4/income_03_q4, transportExp_03_q5/income_03_q5, 
                                         transportExp_05_q1/income_05_q1,transportExp_05_q2/income_05_q2,transportExp_05_q3/income_05_q3, transportExp_05_q4/income_05_q4, transportExp_05_q5/income_05_q5, 
                                         transportExp_07_q1/income_07_q1,transportExp_07_q2/income_07_q2,transportExp_07_q3/income_07_q3, transportExp_07_q4/income_07_q4, transportExp_07_q5/income_07_q5,
                                         transportExp_09_q1/income_09_q1,transportExp_09_q2/income_09_q2,transportExp_09_q3/income_09_q3, transportExp_09_q4/income_09_q4, transportExp_09_q5/income_09_q5, 
                                         transportExp_11_q1/income_11_q1,transportExp_11_q2/income_11_q2,transportExp_11_q3/income_11_q3, transportExp_11_q4/income_11_q4, transportExp_11_q5/income_11_q5, 
                                         transportExp_13_q1/income_13_q1,transportExp_13_q2/income_13_q2,transportExp_13_q3/income_13_q3, transportExp_13_q4/income_13_q4, transportExp_13_q5/income_13_q5),
                      "childcare" = c(childcare_03_q1/income_03_q1,childcare_03_q2/income_03_q2,childcare_03_q3/income_03_q3, childcare_03_q4/income_03_q4, childcare_03_q5/income_03_q5, 
                                      childcare_05_q1/income_05_q1,childcare_05_q2/income_05_q2,childcare_05_q3/income_05_q3, childcare_05_q4/income_05_q4, childcare_05_q5/income_05_q5, 
                                      childcare_07_q1/income_07_q1,childcare_07_q2/income_07_q2,childcare_07_q3/income_07_q3, childcare_07_q4/income_07_q4, childcare_07_q5/income_07_q5,
                                      childcare_09_q1/income_09_q1,childcare_09_q2/income_09_q2,childcare_09_q3/income_09_q3, childcare_09_q4/income_09_q4, childcare_09_q5/income_09_q5, 
                                      childcare_11_q1/income_11_q1,childcare_11_q2/income_11_q2,childcare_11_q3/income_11_q3, childcare_11_q4/income_11_q4, childcare_11_q5/income_11_q5, 
                                      childcare_13_q1/income_13_q1,childcare_13_q2/income_13_q2,childcare_13_q3/income_13_q3, childcare_13_q4/income_13_q4, childcare_13_q5/income_13_q5),
                      "healthExp" = c(healthExp_03_q1/income_03_q1,healthExp_03_q2/income_03_q2,healthExp_03_q3/income_03_q3, healthExp_03_q4/income_03_q4, healthExp_03_q5/income_03_q5, 
                                      healthExp_05_q1/income_05_q1,healthExp_05_q2/income_05_q2,healthExp_05_q3/income_05_q3, healthExp_05_q4/income_05_q4, healthExp_05_q5/income_05_q5, 
                                      healthExp_07_q1/income_07_q1,healthExp_07_q2/income_07_q2,healthExp_07_q3/income_07_q3, healthExp_07_q4/income_07_q4, healthExp_07_q5/income_07_q5,
                                      healthExp_09_q1/income_09_q1,healthExp_09_q2/income_09_q2,healthExp_09_q3/income_09_q3, healthExp_09_q4/income_09_q4, healthExp_09_q5/income_09_q5, 
                                      healthExp_11_q1/income_11_q1,healthExp_11_q2/income_11_q2,healthExp_11_q3/income_11_q3, healthExp_11_q4/income_11_q4, healthExp_11_q5/income_11_q5, 
                                      healthExp_13_q1/income_13_q1,healthExp_13_q2/income_13_q2,healthExp_13_q3/income_13_q3, healthExp_13_q4/income_13_q4, healthExp_13_q5/income_13_q5),
                      "educExp" = c(educExp_03_q1/income_03_q1,educExp_03_q2/income_03_q2,educExp_03_q3/income_03_q3, educExp_03_q4/income_03_q4, educExp_03_q5/income_03_q5, 
                                    educExp_05_q1/income_05_q1,educExp_05_q2/income_05_q2,educExp_05_q3/income_05_q3, educExp_05_q4/income_05_q4, educExp_05_q5/income_05_q5, 
                                    educExp_07_q1/income_07_q1,educExp_07_q2/income_07_q2,educExp_07_q3/income_07_q3, educExp_07_q4/income_07_q4, educExp_07_q5/income_07_q5,
                                    educExp_09_q1/income_09_q1,educExp_09_q2/income_09_q2,educExp_09_q3/income_09_q3, educExp_09_q4/income_09_q4, educExp_09_q5/income_09_q5, 
                                    educExp_11_q1/income_11_q1,educExp_11_q2/income_11_q2,educExp_11_q3/income_11_q3, educExp_11_q4/income_11_q4, educExp_11_q5/income_11_q5, 
                                    educExp_13_q1/income_13_q1,educExp_13_q2/income_13_q2,educExp_13_q3/income_13_q3, educExp_13_q4/income_13_q4, educExp_13_q5/income_13_q5),
                      "vacations" = c(0,0,0, 0, 0, 
                                      vacations_05_q1/income_05_q1,vacations_05_q2/income_05_q2,vacations_05_q3/income_05_q3, vacations_05_q4/income_05_q4, vacations_05_q5/income_05_q5,
                                      vacations_07_q1/income_07_q1,vacations_07_q2/income_07_q2,vacations_07_q3/income_07_q3, vacations_07_q4/income_07_q4, vacations_07_q5/income_07_q5,
                                      vacations_09_q1/income_09_q1,vacations_09_q2/income_09_q2,vacations_09_q3/income_09_q3, vacations_09_q4/income_09_q4, vacations_09_q5/income_09_q5, 
                                      vacations_11_q1/income_11_q1,vacations_11_q2/income_11_q2,vacations_11_q3/income_11_q3, vacations_11_q4/income_11_q4, vacations_11_q5/income_11_q5, 
                                      vacations_13_q1/income_13_q1,vacations_13_q2/income_13_q2,vacations_13_q3/income_13_q3, vacations_13_q4/income_13_q4, vacations_13_q5/income_13_q5),
                      "recreation" = c(0,0,0, 0, 0, 
                                       recreation_05_q1/income_05_q1,recreation_05_q2/income_05_q2,recreation_05_q3/income_05_q3, recreation_05_q4/income_05_q4, recreation_05_q5/income_05_q5,
                                       recreation_07_q1/income_07_q1,recreation_07_q2/income_07_q2,recreation_07_q3/income_07_q3, recreation_07_q4/income_07_q4, recreation_07_q5/income_07_q5,
                                       recreation_09_q1/income_09_q1,recreation_09_q2/income_09_q2,recreation_09_q3/income_09_q3, recreation_09_q4/income_09_q4, recreation_09_q5/income_09_q5, 
                                       recreation_11_q1/income_11_q1,recreation_11_q2/income_11_q2,recreation_11_q3/income_11_q3, recreation_11_q4/income_11_q4, recreation_11_q5/income_11_q5, 
                                       recreation_13_q1/income_13_q1,recreation_13_q2/income_13_q2,recreation_13_q3/income_13_q3, recreation_13_q4/income_13_q4, recreation_13_q5/income_13_q5),
                      "clothing" = c(0,0,0, 0, 0, 
                                     clothing_05_q1/income_05_q1,clothing_05_q2/income_05_q2,clothing_05_q3/income_05_q3, clothing_05_q4/income_05_q4, clothing_05_q5/income_05_q5,
                                     clothing_07_q1/income_07_q1,clothing_07_q2/income_07_q2,clothing_07_q3/income_07_q3, clothing_07_q4/income_07_q4, clothing_07_q5/income_07_q5,
                                     clothing_09_q1/income_09_q1,clothing_09_q2/income_09_q2,clothing_09_q3/income_09_q3, clothing_09_q4/income_09_q4, clothing_09_q5/income_09_q5, 
                                     clothing_11_q1/income_11_q1,clothing_11_q2/income_11_q2,clothing_11_q3/income_11_q3, clothing_11_q4/income_11_q4, clothing_11_q5/income_11_q5, 
                                     clothing_13_q1/income_13_q1,clothing_13_q2/income_13_q2,clothing_13_q3/income_13_q3, clothing_13_q4/income_13_q4, clothing_13_q5/income_13_q5)
                      )


out_melt<-melt(out_pct, id=c("quantile","year"))
ggplot(data = out_melt, aes(x = quantile, y =100*(value), fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year) 

