  # Using method of Cooper (2013) RoES HOUSE PRICE FLUCTUATIONS: THE ROLE OF HOUSING WEALTH AS BORROWING COLLATERAL
  # ASSETS WITH CAPITAL GAINS - stocks, private annuity, other real estate, investment in business/farms
  # calculate active savings as amount invested between t-1 and t minus the amount removed


#   # stocks added - removed
# f01$stocksAS01 <- ifelse(f01$removedStocks01<0,f01$addedStocks01 + f01$removedStocks01, f01$addedStocks01 - f01$removedStocks01)
# f03$stocksAS03 <- ifelse(f03$removedStocks03<0,f03$addedStocks03 + f03$removedStocks03, f03$addedStocks03 - f03$removedStocks03)
# f05$stocksAS05 <- ifelse(f05$removedStocks05<0,f05$addedStocks05 + f05$removedStocks05, f05$addedStocks05 - f05$removedStocks05)
# f07$stocksAS07 <- ifelse(f07$removedStocks07<0,f07$addedStocks07 + f07$removedStocks07, f07$addedStocks07 - f07$removedStocks07)
# f09$stocksAS09 <- ifelse(f09$removedStocks09<0,f09$addedStocks09 + f09$removedStocks09, f09$addedStocks09 - f09$removedStocks09)
# f11$stocksAS11 <- ifelse(f11$removedStocks11<0,f11$addedStocks11 + f11$removedStocks11, f11$addedStocks11 - f11$removedStocks11)
# f13$stocksAS13 <- ifelse(f13$removedStocks13<0,f13$addedStocks13 + f13$removedStocks13, f13$addedStocks13 - f13$removedStocks13)
# 
#   
#   # private annuity
# f01$privateAnnuityAS01 <- ifelse(f01$removedPrivateAnnuity01<0,f01$addedPrivateAnnuity01 + f01$removedPrivateAnnuity01, f01$addedPrivateAnnuity01 - f01$removedPrivateAnnuity01)
# f03$privateAnnuityAS03 <- ifelse(f03$removedPrivateAnnuity03<0,f03$addedPrivateAnnuity03 + f03$removedPrivateAnnuity03, f03$addedPrivateAnnuity03 - f03$removedPrivateAnnuity03)
# f05$privateAnnuityAS05 <- ifelse(f05$removedPrivateAnnuity05<0,f05$addedPrivateAnnuity05 + f05$removedPrivateAnnuity05, f05$addedPrivateAnnuity05 - f05$removedPrivateAnnuity05)
# f07$privateAnnuityAS07 <- ifelse(f07$removedPrivateAnnuity07<0,f07$addedPrivateAnnuity07 + f07$removedPrivateAnnuity07, f07$addedPrivateAnnuity07 - f07$removedPrivateAnnuity07)
# f09$privateAnnuityAS09 <- ifelse(f09$removedPrivateAnnuity09<0,f09$addedPrivateAnnuity09 + f09$removedPrivateAnnuity09, f09$addedPrivateAnnuity09 - f09$removedPrivateAnnuity09)
# f11$privateAnnuityAS11 <- ifelse(f11$removedPrivateAnnuity11<0,f11$addedPrivateAnnuity11 + f11$removedPrivateAnnuity11, f11$addedPrivateAnnuity11 - f11$removedPrivateAnnuity11)
# f13$privateAnnuityAS13 <- ifelse(f13$removedPrivateAnnuity13<0,f13$addedPrivateAnnuity13 + f13$removedPrivateAnnuity13, f13$addedPrivateAnnuity13 - f13$removedPrivateAnnuity13)
#   
#   # other real estate
#   f01$othRealEstateAS01 <- ifelse(f01$removedOthRealEstate01<0,f01$addedOthRealEstate01 + f01$removedOthRealEstate01, f01$addedOthRealEstate01 - f01$removedOthRealEstate01)
#   f03$othRealEstateAS03 <- ifelse(f03$removedOthRealEstate03<0,f03$addedOthRealEstate03 + f03$removedOthRealEstate03, f03$addedOthRealEstate03 - f03$removedOthRealEstate03)
#   f05$othRealEstateAS05 <- ifelse(f05$removedOthRealEstate05<0,f05$addedOthRealEstate05 + f05$removedOthRealEstate05, f05$addedOthRealEstate05 - f05$removedOthRealEstate05)
#   f07$othRealEstateAS07 <- ifelse(f07$removedOthRealEstate07<0,f07$addedOthRealEstate07 + f07$removedOthRealEstate07, f07$addedOthRealEstate07 - f07$removedOthRealEstate07)
#   f09$othRealEstateAS09 <- ifelse(f09$removedOthRealEstate09<0,f09$addedOthRealEstate09 + f09$removedOthRealEstate09, f09$addedOthRealEstate09 - f09$removedOthRealEstate09)
#   f11$othRealEstateAS11 <- ifelse(f11$removedOthRealEstate11<0,f11$addedOthRealEstate11 + f11$removedOthRealEstate11, f11$addedOthRealEstate11 - f11$removedOthRealEstate11)
#   f13$othRealEstateAS13 <- ifelse(f13$removedOthRealEstate13<0,f13$addedOthRealEstate13 + f13$removedOthRealEstate13, f13$addedOthRealEstate13 - f13$removedOthRealEstate13)
#   
#   # farm/business
#   f01$farmBusinessAS01 <- ifelse(f01$removedFarmBusiness01<0,f01$addedFarmBusiness01 + f01$removedFarmBusiness01, f01$addedFarmBusiness01 - f01$removedFarmBusiness01)
#   f03$farmBusinessAS03 <- ifelse(f03$removedFarmBusiness03<0,f03$addedFarmBusiness03 + f03$removedFarmBusiness03, f03$addedFarmBusiness03 - f03$removedFarmBusiness03)
#   f05$farmBusinessAS05 <- ifelse(f05$removedFarmBusiness05<0,f05$addedFarmBusiness05 + f05$removedFarmBusiness05, f05$addedFarmBusiness05 - f05$removedFarmBusiness05)
#   f07$farmBusinessAS07 <- ifelse(f07$removedFarmBusiness07<0,f07$addedFarmBusiness07 + f07$removedFarmBusiness07, f07$addedFarmBusiness07 - f07$removedFarmBusiness07)
#   f09$farmBusinessAS09 <- ifelse(f09$removedFarmBusiness09<0,f09$addedFarmBusiness09 + f09$removedFarmBusiness09, f09$addedFarmBusiness09 - f09$removedFarmBusiness09)
#   f11$farmBusinessAS11 <- ifelse(f11$removedFarmBusiness11<0,f11$addedFarmBusiness11 + f11$removedFarmBusiness11, f11$addedFarmBusiness11 - f11$removedFarmBusiness11)
#   f13$farmBusinessAS13 <- ifelse(f13$removedFarmBusiness13<0,f13$addedFarmBusiness13 + f13$removedFarmBusiness13, f13$addedFarmBusiness13 - f13$removedFarmBusiness13)
#   
#   
  # need to merge the datasets over time by unique ID
  
  # f99_keeps <-
  #   c('uniqueID','checkingValue99','bondValue99','vehicleValue99',
  #     'othDebtValue99','mortgageDebt99','homeEquity99','wtrMoved99'
  #   )
  # 
  # f01_keeps <-
  #   c('uniqueID','longWeight01','checkingValue01','bondValue01','vehicleValue01',
  #     'othDebtValue01','mortgageDebt01','homeEquity01','wtrMoved01',#'stocksAS01','privateAnnuityAS01','othRealEstateAS01','farmBusinessAS01',
  #     'totalWealth01','income01',"consumption01",
  #         "food01","housing01","transportExp01","childcare01","healthExp01","educExp01"
  #     )
  
  f03_keeps <-
    c("uniqueID","primarySamplingUnit","longWeight03","stratification",'checkingValue03','bondValue03','vehicleValue03',
      'othDebtValue03','mortgageDebt03','homeEquity03','wtrMoved03',#'stocksAS03','privateAnnuityAS03','othRealEstateAS03','farmBusinessAS03',
      'totalWealth03','income03','consumption03',
      "educ03", "race03","kidsOut03","region03","exIncome03","kids03","famSize03","employed03",
       'age03',
      "food03","housing03","transportExp03","childcare03","healthExp03","educExp03","poorHTM03", 'selfEmp03','interestRate03'
)
  
  f05_keeps <-
    c("uniqueID","longWeight05",'checkingValue05','bondValue05','vehicleValue05','othDebtValue05','mortgageDebt05','homeEquity05',
      'wtrMoved05',#'stocksAS05','privateAnnuityAS05','othRealEstateAS05','farmBusinessAS05',
      'totalWealth05','income05','consumption05',
      "educ05", "race05","kidsOut05","region05","exIncome05","kids05","famSize05","employed05",
      'age05',
            "food05","housing05","transportExp05","childcare05","healthExp05","educExp05","poorHTM05","consumption_plus05", 'selfEmp05','interestRate05',
      'vacations05','recreation05','clothing05')
  
  f07_keeps <-
    c("uniqueID","longWeight07",'checkingValue07','bondValue07','vehicleValue07','othDebtValue07','mortgageDebt07','homeEquity07',
      'wtrMoved07',#'stocksAS07','privateAnnuityAS07','othRealEstateAS07','farmBusinessAS07',
      'totalWealth07','income07','consumption07',
      "educ07", "race07","kidsOut07","region07","exIncome07","kids07","famSize07","employed07",
      'age07',
      "food07","housing07","transportExp07","childcare07","healthExp07","educExp07","poorHTM07","consumption_plus07", 'selfEmp07','interestRate07',
      'vacations07','recreation07','clothing07')
  
  f09_keeps <-
    c("uniqueID","primarySamplingUnit","longWeight09","stratification",'checkingValue09','bondValue09','vehicleValue09',
      'othDebtValue09','mortgageDebt09','homeEquity09','wtrMoved09',#'stocksAS09','privateAnnuityAS09','othRealEstateAS09','farmBusinessAS09',
      'totalWealth09','income09','consumption09',
      "educ09", "race09","kidsOut09","region09","exIncome09","kids09","famSize09","employed09",
      'age09',
      "food09","housing09","transportExp09","childcare09","healthExp09","educExp09","poorHTM09","consumption_plus09", 'selfEmp09','interestRate09',
      'vacations09','recreation09','clothing09')
  
  f11_keeps <-
    c("uniqueID","longWeight11",'checkingValue11','bondValue11','vehicleValue11','othDebtValue11','mortgageDebt11','homeEquity11',
      'wtrMoved11',#'stocksAS11','privateAnnuityAS11','othRealEstateAS11','farmBusinessAS11',
      'totalWealth11','income11','consumption11',
      "educ11", "race11","kidsOut11","region11","exIncome11","kids11","famSize11","employed11",
      'age11',
      "food11","housing11","transportExp11","childcare11","healthExp11","educExp11","poorHTM11","consumption_plus11", 'selfEmp11','interestRate11',
      'vacations11','recreation11','clothing11')
  
  f13_keeps <-
    c("uniqueID","longWeight13",'checkingValue13','bondValue13','vehicleValue13','othDebtValue13','mortgageDebt13','homeEquity13',
      'wtrMoved13',#'stocksAS13','privateAnnuityAS13','othRealEstateAS13','farmBusinessAS13',
      'totalWealth13','income13','consumption13',
      "educ13", "race13","kidsOut13","region13","exIncome13","kids13","famSize13","employed13",
      'age13',
      "food13","housing13","transportExp13","childcare13","healthExp13","educExp13","poorHTM13","consumption_plus13", 'selfEmp13','interestRate13',
      'vacations13','recreation13','clothing13')
  
 # f99_as <- f99[,f99_keeps]
#  f01_as <- f01[,f01_keeps]
  f03_as <- f03[,f03_keeps]
  f05_as <- f05[,f05_keeps]
  f07_as <- f07[,f07_keeps]
  f09_as <- f09[,f09_keeps]
  f11_as <- f11[,f11_keeps]
  f11_as <- f11[,f11_keeps]
  f13_as <- f13[,f13_keeps]
  
  #rm(f99_keeps,f01_keeps,
  rm(f03_keeps,f05_keeps,f07_keeps,f09_keeps,f11_keeps,f13_keeps)
  
  # merge the datasets
  
  f0305_as <- merge(f03_as,f05_as, by=c('uniqueID'),all=FALSE)
  f030507 <- merge(f07_as,f0305_as, by=c('uniqueID'),all=FALSE)
  #f030507_as <- merge(f030507_as,f99_as,by=c('uniqueID'),all.x=TRUE,all.y=FALSE)
  #f030507 <- merge(f030507_as,f01_as,by=c('uniqueID'),all.x=TRUE,all.y=FALSE)
  rm(f0305_as,f03_as)#,f01_as,f99_as)
  
  
  f0911_as <- merge(f09_as,f11_as, by=c('uniqueID'),all=FALSE)
  f091113 <- merge(f13_as,f0911_as, by=c('uniqueID'),all=FALSE)
  #f091113_as <- merge(f091113_as,f05_as,by=c('uniqueID'),all.x=TRUE,all.y=FALSE)
  #f091113 <- merge(f091113_as,f07_as,by=c('uniqueID'),all.x=TRUE,all.y=FALSE)
  rm(f09_as,f11_as,f13_as)#,f07_as,f05_as)
  
#   # ASSETS WITHOUT SIGNIFICANT CAPITAL GAINS - checking accounts, bond holdings, vehicle values, non-collateralised debts
#   #  calculate active savings as value in time t minus value in time t-1
#   
#   # checking accounts
#   f030507$checkingAS01 <- f030507$checkingValue01 - f030507$checkingValue99
#   f030507$checkingAS03 <- f030507$checkingValue03 - f030507$checkingValue01
#   f030507$checkingAS05 <- f030507$checkingValue05 - f030507$checkingValue03
#   f030507$checkingAS07 <- f030507$checkingValue07 - f030507$checkingValue05
#   
#   f091113$checkingAS07 <- f091113$checkingValue07 - f091113$checkingValue05
#   f091113$checkingAS09 <- f091113$checkingValue09 - f091113$checkingValue07
#   f091113$checkingAS11 <- f091113$checkingValue11 - f091113$checkingValue09
#   f091113$checkingAS13 <- f091113$checkingValue13 - f091113$checkingValue11
#   
#   
#   # bond holdings
#   f030507$bondAS01 <- f030507$bondValue01 - f030507$bondValue99
#   f030507$bondAS03 <- f030507$bondValue03 - f030507$bondValue01
#   f030507$bondAS05 <- f030507$bondValue05 - f030507$bondValue03
#   f030507$bondAS07 <- f030507$bondValue07 - f030507$bondValue05
#   
#   f091113$bondAS07 <- f091113$bondValue07 - f091113$bondValue05
#   f091113$bondAS09 <- f091113$bondValue09 - f091113$bondValue07
#   f091113$bondAS11 <- f091113$bondValue11 - f091113$bondValue09
#   f091113$bondAS13 <- f091113$bondValue13 - f091113$bondValue11
#   
#   
#   # vehicle values
#   f030507$vehicleAS01 <- f030507$vehicleValue01 - f030507$vehicleValue99
#   f030507$vehicleAS03 <- f030507$vehicleValue03 - f030507$vehicleValue01
#   f030507$vehicleAS05 <- f030507$vehicleValue05 - f030507$vehicleValue03
#   f030507$vehicleAS07 <- f030507$vehicleValue07 - f030507$vehicleValue05
#   
#   f091113$vehicleAS07 <- f091113$vehicleValue07 - f091113$vehicleValue05
#   f091113$vehicleAS09 <- f091113$vehicleValue09 - f091113$vehicleValue07
#   f091113$vehicleAS11 <- f091113$vehicleValue11 - f091113$vehicleValue09
#   f091113$vehicleAS13 <- f091113$vehicleValue13 - f091113$vehicleValue11
#   
#   
#   # non-collateralised debts - negative so the other way around
#   
#   f030507$othDebtAS01 <- f030507$othDebtValue99 - f030507$othDebtValue01
#   f030507$othDebtAS03 <- f030507$othDebtValue01 - f030507$othDebtValue03
#   f030507$othDebtAS05 <- f030507$othDebtValue03 - f030507$othDebtValue05
#   f030507$othDebtAS07 <- f030507$othDebtValue05 - f030507$othDebtValue07
#   
#   f091113$othDebtAS07 <- f091113$othDebtValue05 - f091113$othDebtValue07
#   f091113$othDebtAS09 <- f091113$othDebtValue07 - f091113$othDebtValue09
#   f091113$othDebtAS11 <- f091113$othDebtValue09 - f091113$othDebtValue11
#   f091113$othDebtAS13 <- f091113$othDebtValue11 - f091113$othDebtValue13
#   
#   
#   # HOUSING
#   # calculate active saving by: if didn't move, debt in t-1 minus debt in t
#   #                             if did move, equity in t  minus equity in t-1
#   
#   f030507$housingAS01 <- ifelse(f030507$wtrMoved01==0, f030507$mortgageDebt99- f030507$mortgageDebt01,
#                                 ifelse(f030507$wtrMoved01==1, f030507$homeEquity01 - f030507$homeEquity99,NA)) 
#   f030507$housingAS03 <- ifelse(f030507$wtrMoved03==0, f030507$mortgageDebt01- f030507$mortgageDebt03,
#                                 ifelse(f030507$wtrMoved03==1, f030507$homeEquity03 - f030507$homeEquity01,NA)) 
#   f030507$housingAS05 <- ifelse(f030507$wtrMoved05==0, f030507$mortgageDebt03- f030507$mortgageDebt05,
#                                 ifelse(f030507$wtrMoved05==1, f030507$homeEquity07 - f030507$homeEquity05,NA)) 
#   f030507$housingAS07 <- ifelse(f030507$wtrMoved07==0, f030507$mortgageDebt05 - f030507$mortgageDebt07,
#                               ifelse(f030507$wtrMoved07==1, f030507$homeEquity07 - f030507$homeEquity05,NA)) 
#   
#   f091113$housingAS07 <- ifelse(f091113$wtrMoved07==0, f091113$mortgageDebt05- f091113$mortgageDebt07,
#                                 ifelse(f091113$wtrMoved07==1, f091113$homeEquity07 - f091113$homeEquity05,NA)) 
#   f091113$housingAS09 <- ifelse(f091113$wtrMoved09==0, f091113$mortgageDebt07- f091113$mortgageDebt09,
#                                 ifelse(f091113$wtrMoved09==1, f091113$homeEquity09 - f091113$homeEquity07,NA)) 
#   f091113$housingAS11 <- ifelse(f091113$wtrMoved11==0, f091113$mortgageDebt09- f091113$mortgageDebt11,
#                                 ifelse(f091113$wtrMoved11==1, f091113$homeEquity13 - f091113$homeEquity11,NA)) 
#   f091113$housingAS13 <- ifelse(f091113$wtrMoved13==0, f091113$mortgageDebt11 - f091113$mortgageDebt13,
#                               ifelse(f091113$wtrMoved13==1, f091113$homeEquity13 - f091113$homeEquity11,NA)) 
#   
#   
#   # SUM ALL ACTIVE SAVINGS
#   # DIVIDED BY TWO AS COVERS TWO YEARS
#   
#   f030507$activeSaving01 <- (f030507$stocksAS01 +
#     f030507$privateAnnuityAS01 +
#     f030507$othRealEstateAS01 + 
#     f030507$farmBusinessAS01 + 
#     f030507$checkingAS01 + 
#     f030507$bondAS01 + 
#     f030507$vehicleAS01 + 
#     f030507$othDebtAS01 + 
#     f030507$housingAS01)/2
#   
#   f030507$activeSaving03 <- (f030507$stocksAS03 +
#     f030507$privateAnnuityAS03 +
#     f030507$othRealEstateAS03 + 
#     f030507$farmBusinessAS03 + 
#     f030507$checkingAS03 + 
#     f030507$bondAS03 + 
#     f030507$vehicleAS03 + 
#     f030507$othDebtAS03 + 
#     f030507$housingAS03)/2
#   
#   f030507$activeSaving05 <- (f030507$stocksAS05 +
#     f030507$privateAnnuityAS05 +
#     f030507$othRealEstateAS05 + 
#     f030507$farmBusinessAS05 + 
#     f030507$checkingAS05 + 
#     f030507$bondAS05 + 
#     f030507$vehicleAS05 + 
#     f030507$othDebtAS05 + 
#     f030507$housingAS05)/2
#   
#   f030507$activeSaving07 <- (f030507$stocksAS07 +
#     f030507$privateAnnuityAS07 +
#     f030507$othRealEstateAS07 + 
#     f030507$farmBusinessAS07 + 
#     f030507$checkingAS07 + 
#     f030507$bondAS07 + 
#     f030507$vehicleAS07 + 
#     f030507$othDebtAS07 + 
#     f030507$housingAS07)/2
#   
#   f091113$activeSaving07 <- (f091113$stocksAS07 +
#     f091113$privateAnnuityAS07 +
#     f091113$othRealEstateAS07 + 
#     f091113$farmBusinessAS07 + 
#     f091113$checkingAS07 + 
#     f091113$bondAS07 + 
#     f091113$vehicleAS07 + 
#     f091113$othDebtAS07 + 
#     f091113$housingAS07)/2
#   
#   f091113$activeSaving09 <- (f091113$stocksAS09 +
#     f091113$privateAnnuityAS09 +
#     f091113$othRealEstateAS09 + 
#     f091113$farmBusinessAS09 + 
#     f091113$checkingAS09 + 
#     f091113$bondAS09 + 
#     f091113$vehicleAS09 + 
#     f091113$othDebtAS09 + 
#     f091113$housingAS09)/2
#   
#   f091113$activeSaving11 <- (f091113$stocksAS11 +
#     f091113$privateAnnuityAS11 +
#     f091113$othRealEstateAS11 + 
#     f091113$farmBusinessAS11 + 
#     f091113$checkingAS11 + 
#     f091113$bondAS11 + 
#     f091113$vehicleAS11 + 
#     f091113$othDebtAS11 + 
#     f091113$housingAS11)/2
#   
#   f091113$activeSaving13 <- (f091113$stocksAS13 +
#     f091113$privateAnnuityAS13 +
#     f091113$othRealEstateAS13 + 
#     f091113$farmBusinessAS13 + 
#     f091113$checkingAS13 + 
#     f091113$bondAS13 + 
#     f091113$vehicleAS13 + 
#     f091113$othDebtAS13 + 
#     f091113$housingAS13)/2
#   
#   
#   
#   
#   
#   # total wealth change
#   
#   f030507$dtotalWealth03 <- (f030507$totalWealth03 - f030507$totalWealth01)/2
#   f030507$dtotalWealth05 <- (f030507$totalWealth05 - f030507$totalWealth03)/2
#   f030507$dtotalWealth07 <- (f030507$totalWealth07 - f030507$totalWealth05)/2
#   
#   f091113$dtotalWealth09 <- (f091113$totalWealth09 - f091113$totalWealth07)/2
#   f091113$dtotalWealth11 <- (f091113$totalWealth11 - f091113$totalWealth09)/2
#   f091113$dtotalWealth13 <- (f091113$totalWealth13 - f091113$totalWealth11)/2
#   
#   
#   # capital change
#   
#   f030507$capChange03 <- f030507$dtotalWealth03 - f030507$activeSaving03
#   f030507$capChange05 <- f030507$dtotalWealth05 - f030507$activeSaving05
#   f030507$capChange07 <- f030507$dtotalWealth07 - f030507$activeSaving07
#   
#   f091113$capChange09 <- f091113$dtotalWealth09 - f091113$activeSaving09
#   f091113$capChange11 <- f091113$dtotalWealth11 - f091113$activeSaving11
#   f091113$capChange13 <- f091113$dtotalWealth13 - f091113$activeSaving13
#   
#   
# # imputed consumption
#   
# # f030507$impConsumption03<- ifelse((f030507$capChange03/2)<0,f030507$income03 - f030507$activeSaving03/2,f030507$income03 - f030507$activeSaving03/2- f030507$capChange03)
# # f030507$impConsumption05<- ifelse((f030507$capChange05/2)<0,f030507$income05 - f030507$activeSaving05/2,f030507$income05 - f030507$activeSaving05/2- f030507$capChange05)
# # f030507$impConsumption07<- ifelse((f030507$capChange07/2)<0,f030507$income07 - f030507$activeSaving07/2,f030507$income07 - f030507$activeSaving07/2- f030507$capChange07)
# # 
# # f091113$impConsumption09<- ifelse((f091113$capChange09/2)<0,f091113$income09 - f091113$activeSaving09/2,f091113$income09 - f091113$activeSaving09/2- f091113$capChange09)
# # f091113$impConsumption11<- ifelse((f091113$capChange11/2)<0,f091113$income11 - f091113$activeSaving11/2,f091113$income11 - f091113$activeSaving11/2- f091113$capChange11)
# # f091113$impConsumption13<- ifelse((f091113$capChange13/2)<0,f091113$income13 - f091113$activeSaving13/2,f091113$income13 - f091113$activeSaving13/2- f091113$capChange13)
# 
# # f030507$impConsumption03 <- ifelse((f030507$dtotalWealth03/2)<0,f030507$income03,f030507$income03 - f030507$dtotalWealth03/2)
# # f030507$impConsumption05 <- ifelse((f030507$dtotalWealth05/2)<0,f030507$income05,f030507$income05 - f030507$dtotalWealth05/2)
# # f030507$impConsumption07 <- ifelse((f030507$dtotalWealth07/2)<0,f030507$income07,f030507$income07 - f030507$dtotalWealth07/2)
# # 
# # f091113$impConsumption09 <- ifelse((f091113$dtotalWealth09/2)<0,f091113$income09,f091113$income09 - f091113$dtotalWealth09/2)
# # f091113$impConsumption11 <- ifelse((f091113$dtotalWealth11/2)<0,f091113$income11,f091113$income11 - f091113$dtotalWealth11/2)
# # f091113$impConsumption13 <- ifelse((f091113$dtotalWealth13/2)<0,f091113$income13,f091113$income13 - f091113$dtotalWealth13/2)
# 
# # f030507$impConsumption01 <- ifelse((f030507$income01 - f030507$activeSaving01)<0,0,f030507$income01 - f030507$activeSaving01)
# # f030507$impConsumption03 <- ifelse((f030507$income03 - f030507$activeSaving03)<0,0,f030507$income03 - f030507$activeSaving03)
# # f030507$impConsumption05 <- ifelse((f030507$income05 - f030507$activeSaving05)<0,0,f030507$income05 - f030507$activeSaving05)
# # f030507$impConsumption07 <- ifelse((f030507$income07 - f030507$activeSaving07)<0,0,f030507$income07 - f030507$activeSaving07)
# # 
# # f091113$impConsumption07 <- ifelse((f091113$income07 - f091113$activeSaving07)<0,0,f091113$income07 - f091113$activeSaving07)
# # f091113$impConsumption09 <- ifelse((f091113$income09 - f091113$activeSaving09)<0,0,f091113$income09 - f091113$activeSaving09)
# # f091113$impConsumption11 <- ifelse((f091113$income11 - f091113$activeSaving11)<0,0,f091113$income11 - f091113$activeSaving11)
# # f091113$impConsumption13 <- ifelse((f091113$income13 - f091113$activeSaving13)<0,0,f091113$income13 - f091113$activeSaving13)
#   
#   
#   f030507$impConsumption01 <- f030507$income01 - f030507$activeSaving01
#   f030507$impConsumption03 <- f030507$income03 - f030507$activeSaving03
#   f030507$impConsumption05 <- f030507$income05 - f030507$activeSaving05
#   f030507$impConsumption07 <- f030507$income07 - f030507$activeSaving07
#   
#   f091113$impConsumption07 <- f091113$income07 - f091113$activeSaving07
#   f091113$impConsumption09 <- f091113$income09 - f091113$activeSaving09
#   f091113$impConsumption11 <- f091113$income11 - f091113$activeSaving11
#   f091113$impConsumption13 <- f091113$income13 - f091113$activeSaving13
# 
# 
#f030507$impSaving01 <- ifelse(f030507$consumption01<=0, NA,f030507$income01 - f030507$consumption01)
f030507$impSaving03 <- ifelse(f030507$consumption03<=0, NA,f030507$income03 - f030507$consumption03)
f030507$impSaving05 <- ifelse(f030507$consumption05<=0, NA,f030507$income05 - f030507$consumption05)
f030507$impSaving07 <- ifelse(f030507$consumption07<=0, NA,f030507$income07 - f030507$consumption07)

#f091113$impSaving07 <- ifelse(f091113$consumption07<=0, NA,f091113$income07 - f091113$consumption07)
f091113$impSaving09 <- ifelse(f091113$consumption09<=0, NA,f091113$income09 - f091113$consumption09)
f091113$impSaving11 <- ifelse(f091113$consumption11<=0, NA,f091113$income11 - f091113$consumption11)
f091113$impSaving13 <- ifelse(f091113$consumption13<=0, NA,f091113$income13 - f091113$consumption13)


# drop variables no longer needed
#f030507 = f030507[,!grepl("99",names(f030507))]
#f091113 = f091113[,!grepl("05",names(f091113))]






