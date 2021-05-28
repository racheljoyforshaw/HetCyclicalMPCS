###########################################################

# Work out percentage of total wealth changes attributed to active savings
#  Using method of Krueger,Mitman, Perri: Macroeconomics and Household Heterogeneity (2016)
# Keep the identity of the households fixed; 
# for example, to compute the 2004-2006 change in net worth for 
# Q1 of the net worth distribution we select all households in the
# bottom quintile of the wealth distribution in 2004, compute their average net worth
# in 2004 and 2006, and then calculate the percent difference between the two averages

#create a survey design 
familyPanelSurvey05_07_weight05 <- svydesign(id=~primarySamplingUnit,
                                             strat=~stratification, 
                                             weights=~longWeight05,
                                             data=f0507,
                                             nest=TRUE)

familyPanelSurvey05_07_weight07 <- svydesign(id=~primarySamplingUnit,
                                             strat=~stratification, 
                                             weights=~longWeight07,
                                             data=f0507,
                                             nest=TRUE)

familyPanelSurvey07_11_weight07 <- svydesign(id=~primarySamplingUnit,
                                             strat=~stratification, 
                                             weights=~longWeight07,
                                             data=f0711,
                                             nest=TRUE)
familyPanelSurvey07_11_weight11 <- svydesign(id=~primarySamplingUnit,
                                             strat=~stratification, 
                                             weights=~longWeight11,
                                             data=f0711,
                                             nest=TRUE)

# work out wealth quantiles
for (j in c("05_07")){
  assign(paste0("wealth_quantiles_05_",j),
         svyquantile(~eval(as.symbol(paste0("totalWealth",substr(j,1,2)))),
                     eval(as.name(paste0("familyPanelSurvey",j,"_weight",substr(j,1,2)))), 
                     quantiles=c(0.0,0.2,0.4,0.6,0.8,1.0),
                     na.rm=TRUE,
                     ci=TRUE))
}

for (j in c("07_11")){
  assign(paste0("wealth_quantiles_07_",j),
         svyquantile(~eval(as.symbol(paste0("totalWealth",substr(j,1,2)))),
                     eval(as.name(paste0("familyPanelSurvey",j,"_weight",substr(j,1,2)))), 
                     quantiles=c(0.0,0.2,0.4,0.6,0.8,1.0),
                     na.rm=TRUE,
                     ci=TRUE))
}

# mark wealth quantiles in data
familyPanelSurvey05_07_weight05$variables$wealthQuantile05 <- ifelse(f0507$totalWealth05<wealth_quantiles_05_05_07$quantiles[2],1,
                                                                     ifelse(f0507$totalWealth05>=wealth_quantiles_05_05_07$quantiles[2] & f0507$totalWealth05<wealth_quantiles_05_05_07$quantiles[3],2,
                                                                            ifelse(f0507$totalWealth05>=wealth_quantiles_05_05_07$quantiles[3] & f0507$totalWealth05<wealth_quantiles_05_05_07$quantiles[4],3,
                                                                                   ifelse(f0507$totalWealth05>=wealth_quantiles_05_05_07$quantiles[4]& f0507$totalWealth05<wealth_quantiles_05_05_07$quantiles[5],4,
                                                                                          ifelse(f0507$totalWealth05>=wealth_quantiles_05_05_07$quantiles[5],5,NA)))))
familyPanelSurvey07_11_weight07$variables$wealthQuantile07 <- ifelse(f0711$totalWealth07<wealth_quantiles_07_07_11$quantiles[2],1,
                                                                     ifelse(f0711$totalWealth07>=wealth_quantiles_07_07_11$quantiles[2] & f0711$totalWealth07<wealth_quantiles_07_07_11$quantiles[3],2,
                                                                            ifelse(f0711$totalWealth07>=wealth_quantiles_07_07_11$quantiles[3] & f0711$totalWealth07<wealth_quantiles_07_07_11$quantiles[4],3,
                                                                                   ifelse(f0711$totalWealth07>=wealth_quantiles_07_07_11$quantiles[4]& f0711$totalWealth07<wealth_quantiles_07_07_11$quantiles[5],4,
                                                                                          ifelse(f0711$totalWealth07>=wealth_quantiles_07_07_11$quantiles[5],5,NA)))))

# work out means

# wealth
for (i in c(1,2,3,4,5)){
  assign(paste0("meanWealth_05_q",i),
         svymean(~totalWealth05,
                 subset(familyPanelSurvey05_07_weight05,
                        familyPanelSurvey05_07_weight05$variables$wealthQuantile05==i),na.rm=TRUE))}
for (i in c(1,2,3,4,5)){
  assign(paste0("meanWealth_07_q",i),
         svymean(~totalWealth07,
                 subset(familyPanelSurvey05_07_weight07,
                        familyPanelSurvey05_07_weight05$variables$wealthQuantile07==i),na.rm=TRUE))}

for (i in c(1,2,3,4,5)){
  assign(paste0("meanWealth_11_q",i),
         svymean(~totalWealth11,
                 subset(familyPanelSurvey07_11_weight11,
                        familyPanelSurvey07_11_weight07$variables$wealthQuantile07==i),na.rm=TRUE))}

# active saving
for (i in c(1,2,3,4,5)){
  assign(paste0("activeSaving_07_q",i),
         svymean(~activeSaving07,
                 subset(familyPanelSurvey05_07_weight07,
                        familyPanelSurvey05_07_weight05$variables$wealthQuantile05==i),na.rm=TRUE))}

for (i in c(1,2,3,4,5)){
  assign(paste0("activeSaving_11_q",i),
         svymean(~activeSaving11,
                 subset(familyPanelSurvey07_11_weight11,
                        familyPanelSurvey07_11_weight07$variables$wealthQuantile07==i),na.rm=TRUE))}

# work out changes in mean wealth
for (i in c(1,2,3,4,5)){
  assign(paste0("meanWealth_change_07_q",i),
         eval(as.name(paste0("meanWealth_07_q",i)))[1] - eval(as.name(paste0("meanWealth_05_q",i)))[1])
}

for (i in c(1,2,3,4,5)){
  assign(paste0("meanWealth_change_11_q",i),
         eval(as.name(paste0("meanWealth_11_q",i)))[1] - eval(as.name(paste0("meanWealth_07_q",i)))[1])
}

# active savings as percentage of wealth
for (i in c(1,2,3,4,5)){
  assign(paste0("aS_percentageWealth_07_q",i),
         eval(as.name(paste0("activeSaving_07_q",i)))/abs(eval(as.name(paste0("meanWealth_change_07_q",i)))))
}

for (i in c(1,2,3,4,5)){
  assign(paste0("aS_percentageWealth_11_q",i),
         eval(as.name(paste0("activeSaving_11_q",i)))/abs(eval(as.name(paste0("meanWealth_change_11_q",i)))))
}

# print

print('Mean wealth change 07 (value)')
for (i in c(1,2,3,4,5)){
  print(eval(as.name(paste0("meanWealth_change_07_q",i)))/2)
}

print('Mean wealth change 11 (value)')
for (i in c(1,2,3,4,5)){
  print(eval(as.name(paste0("meanWealth_change_11_q",i)))/4) # note longer time period
}

print('Mean wealth change 07 in annualised %:')
for (i in c(1,2,3,4,5)){
  ifelse((eval(as.name(paste0("meanWealth_change_07_q",i)))/abs(eval(as.name(paste0("meanWealth_05_q",i)))))<0,
         print(abs(eval(as.name(paste0("meanWealth_change_07_q",i)))/abs(eval(as.name(paste0("meanWealth_05_q",i)))))^(1/2)*-100),
         print((eval(as.name(paste0("meanWealth_change_07_q",i)))/abs(eval(as.name(paste0("meanWealth_05_q",i)))))^(1/2)*100))  # note longer time period
}

print('Mean wealth change 11 in annualised %:')
for (i in c(1,2,3,4,5)){
  ifelse((eval(as.name(paste0("meanWealth_change_11_q",i)))/abs(eval(as.name(paste0("meanWealth_07_q",i)))))<0,
         print(abs(eval(as.name(paste0("meanWealth_change_11_q",i)))/abs(eval(as.name(paste0("meanWealth_07_q",i)))))^(1/4)*-100),
  print((eval(as.name(paste0("meanWealth_change_11_q",i)))/abs(eval(as.name(paste0("meanWealth_07_q",i)))))^(1/4)*100))  # note longer time period
}

print('Active savings as a percentage of wealth 07 in annualised %:')
for (i in c(1,2,3,4,5)){
  print(eval(as.name(paste0("aS_percentageWealth_07_q",i)))^(1/2)*100)
}

print('Active savings as a percentage of wealth 11 in annualised %:')
for (i in c(1,2,3,4,5)){
  print(eval(as.name(paste0("aS_percentageWealth_11_q",i)))^(1/4)*100) # note longer time period
}