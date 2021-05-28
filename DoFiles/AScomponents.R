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



# plot contributions of asset classes 

for (k in c("stocksAS","privateAnnuityAS","othRealEstateAS","farmBusinessAS","checkingAS","bondAS","vehicleAS","othDebtAS","housingAS")){
  print(k)
for (j in c("030507","091113")){
  print(j)
  for (i in c("1","2","3","4","5")){
    assign(paste0(k,"_",j,"_q",i),
           100*((svymean(~eval(as.name(paste0(k,substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE)/4)/
             ((svymean(~eval(as.name(paste0("income",substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3)))
    print(paste0("q",i,":",(unname(eval(as.name(paste0(k,"_",j,"_q",i)))[1]))))
  }
}
}



out_pct <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                      "year" = c(2007,2007,2007,2007,2007,2013,2013,2013,2013,2013),
                      "stocksAS" = c(stocksAS_030507_q1,stocksAS_030507_q2,stocksAS_030507_q3, stocksAS_030507_q4, stocksAS_030507_q5, 
                                     stocksAS_091113_q1,stocksAS_091113_q2, stocksAS_091113_q3,stocksAS_091113_q4,stocksAS_091113_q4),
                      "privateAnnuityAS" = c(privateAnnuityAS_030507_q1,privateAnnuityAS_030507_q2,privateAnnuityAS_030507_q3,privateAnnuityAS_030507_q4,privateAnnuityAS_030507_q5,
                                             privateAnnuityAS_091113_q1,privateAnnuityAS_091113_q2,privateAnnuityAS_091113_q3,privateAnnuityAS_091113_q4,privateAnnuityAS_091113_q5),
                      "othRealEstateAS" = c(othRealEstateAS_030507_q1,othRealEstateAS_030507_q2,othRealEstateAS_030507_q3,othRealEstateAS_030507_q4,othRealEstateAS_030507_q5,
                                            othRealEstateAS_091113_q1,othRealEstateAS_091113_q2,othRealEstateAS_091113_q3,othRealEstateAS_091113_q4,othRealEstateAS_091113_q5),
                      "farmBusinessAS" = c(farmBusinessAS_030507_q1,farmBusinessAS_030507_q2,farmBusinessAS_030507_q3,farmBusinessAS_030507_q4,farmBusinessAS_030507_q5,
                                           farmBusinessAS_091113_q1,farmBusinessAS_091113_q2,farmBusinessAS_091113_q3,farmBusinessAS_091113_q4,farmBusinessAS_091113_q5),
                      "checkingAS" = c(checkingAS_030507_q1,checkingAS_030507_q2,checkingAS_030507_q3,checkingAS_030507_q4,checkingAS_030507_q5,
                                       checkingAS_091113_q1,checkingAS_091113_q2,checkingAS_091113_q3,checkingAS_091113_q4,checkingAS_091113_q5),
                      "bondAS" = c(bondAS_030507_q1,bondAS_030507_q2,bondAS_030507_q3,bondAS_030507_q4,bondAS_030507_q5,
                                   bondAS_091113_q1,bondAS_091113_q2,bondAS_091113_q3,bondAS_091113_q4,bondAS_091113_q5),
                      "vehicleAS" = c(vehicleAS_030507_q1, vehicleAS_030507_q2,vehicleAS_030507_q3,vehicleAS_030507_q4,vehicleAS_030507_q5,
                                      vehicleAS_091113_q1,vehicleAS_091113_q2,vehicleAS_091113_q3,vehicleAS_091113_q4,vehicleAS_091113_q5),
                      "othDebtAS" = c(othDebtAS_030507_q1,othDebtAS_030507_q2,othDebtAS_030507_q3,othDebtAS_030507_q4, othDebtAS_030507_q5,
                                      othDebtAS_091113_q1,othDebtAS_091113_q2,othDebtAS_091113_q3,othDebtAS_091113_q4,othDebtAS_091113_q5),
                      "housingAS" = c(housingAS_030507_q1,housingAS_030507_q2,housingAS_030507_q3,housingAS_030507_q4,housingAS_030507_q5,
                                      housingAS_091113_q1,housingAS_091113_q2,housingAS_091113_q3,housingAS_091113_q4,housingAS_091113_q5)
)

out_melt<-melt(out_pct, id=c("quantile","year"))
ggplot(data = out_melt, aes(x = quantile, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year) 
