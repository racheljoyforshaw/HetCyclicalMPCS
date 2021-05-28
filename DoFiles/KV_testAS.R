# Kaplan Violante for active saving
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2007,09,11


##### CREATE VARIABLES ########
# create year of birth

# create year variable
f030507$year03 <- 2003
f030507$year05 <- 2005
f030507$year07 <- 2007
f070911$year07 <- 2007
f070911$year09 <- 2009
f070911$year11 <- 2011

f030507$yob03 <- f030507$year03 - f030507$age03 
f030507$yob05 <- f030507$year05 - f030507$age05 
f030507$yob07 <- f030507$year07 - f030507$age07

f070911$yob07 <- f070911$year07 - f070911$age07 
f070911$yob09 <- f070911$year09 - f070911$age09 
f070911$yob11 <- f070911$year11 - f070911$age11



# log activeSaving
min030507 <- min(min(f030507$activeSaving03,na.rm=TRUE),min(f030507$activeSaving05,na.rm=TRUE),min(f030507$activeSaving07,na.rm=TRUE))
f030507$logActiveSaving03 <- log(f030507$activeSaving03 + abs(min030507) +1)
f030507$logActiveSaving05 <- log(f030507$activeSaving05+ abs(min030507) +1)
f030507$logActiveSaving07 <- log(f030507$activeSaving07+ abs(min030507) +1)

min070911 <- min(min(f070911$activeSaving07,na.rm=TRUE),min(f070911$activeSaving09,na.rm=TRUE),min(f070911$activeSaving09,na.rm=TRUE))
f070911$logActiveSaving07 <- log(f070911$activeSaving07+ abs(min070911) +1)
f070911$logActiveSaving09 <- log(f070911$activeSaving09+ abs(min070911) +1)
f070911$logActiveSaving11 <- log(f070911$activeSaving11+ abs(min070911) +1)

#f070911 <- select(f070911,-c("consumption07","consumption09","consumption11"))
#f030507 <- select(f030507,-c("consumption03","consumption05","consumption07"))


# log income

f030507$logy03 <- log(f030507$income03)
f030507$logy05 <- log(f030507$income05)
f030507$logy07 <- log(f030507$income07)
f070911$logy07 <- log(f070911$income07)
f070911$logy09 <- log(f070911$income09)
f070911$logy11 <- log(f070911$income11)

f030507 <- select(f030507,-c("income01","income05","income07"))
f070911 <- select(f070911,-c("income05","income09","income11"))


# drop NA 
f030507 <- na.omit(f030507)
f070911 <- na.omit(f070911)

# quintiles
# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507,
                                     nest=TRUE)

familyPanelSurvey070911 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight07,
                                     data=f070911,
                                     nest=TRUE)
quintiles_03 <- svyquantile(~income03,familyPanelSurvey030507, seq(0, 1, 0.2))
quintiles_07 <- svyquantile(~income07,familyPanelSurvey070911, seq(0, 1, 0.2))


f030507$quintile <- ifelse(f030507$income03<quintiles_03[2],1,
                           ifelse(f030507$income03>=quintiles_03[2] & f030507$income03<quintiles_03[3],2,
                                  ifelse(f030507$income03>=quintiles_03[3] & f030507$income03<quintiles_03[4],3,
                                         ifelse(f030507$income03>=quintiles_03[4]& f030507$income03<quintiles_03[5],4,
                                                ifelse(f030507$income03>=quintiles_03[5],5,NA)))))

f070911$quintile <- ifelse(f070911$income07<quintiles_07[2],1,
                           ifelse(f070911$income07>=quintiles_07[2] & f070911$income07<quintiles_07[3],2,
                                  ifelse(f070911$income07>=quintiles_07[3] & f070911$income07<quintiles_07[4],3,
                                         ifelse(f070911$income07>=quintiles_07[4]& f070911$income07<quintiles_07[5],4,
                                                ifelse(f070911$income07>=quintiles_07[5],5,NA)))))


familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507,
                                     nest=TRUE)

familyPanelSurvey070911 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight07,
                                     data=f070911,
                                     nest=TRUE)
# consumption is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
# income is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
for (j in c("030507","070911")){
  print("Consumption")
  print(j)
  for (i in c("1","2","3","4","5")){
    assign(paste0("consumption_",j,"_q",i),
           ((svymean(~eval(as.name(paste0("consumption",substr(j,5,6)))),
                     subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
               svymean(~eval(as.name(paste0("consumption",substr(j,3,4)))),
                       subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
               svymean(~eval(as.name(paste0("consumption",substr(j,1,2)))),
                       subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3)/
             ((svymean(~eval(as.name(paste0("income",substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3))
    print(paste0("q",i,":",(unname(eval(as.name(paste0("consumption_",j,"_q",i)))[1]))))
  }
}




f030507_keeps <- c("uniqueID","primarySamplingUnit","stratification","black03","black05","black07",
                   "educ03","educ05","educ07",
                   "employed03","employed05","employed07",
                   "exIncome03","exIncome05","exIncome07",
                   "famSize03","famSize05","famSize07",
                   "kids03","kids05","kids07",
                   "kidsOut03","kidsOut05","kidsOut07",
                   "longWeight03","longWeight05","longWeight07",
                   "other03","other05","other07",
                   "logy03","logy05","logy07",
                   "region03","region05","region07",
                   "retired03","retired05","retired07",
                   "unemployed03","unemployed05","unemployed07",
                   "white03","white05","white07",
                   "year03","year05","year07",
                   "yob03","yob05","yob07",
                   "logActiveSaving03","logActiveSaving05","logActiveSaving07",
                   "age03","age05","age07","quintile")

f030507 <- f030507[,f030507_keeps]

f070911_keeps <- c("uniqueID","primarySamplingUnit","stratification","black07","black09","black11",
                   "educ07","educ09","educ11",
                   "employed07","employed09","employed11",
                   "exIncome07","exIncome09","exIncome11",
                   "famSize07","famSize09","famSize11",
                   "kids07","kids09","kids11",
                   "kidsOut07","kidsOut09","kidsOut11",
                   "longWeight07","longWeight09","longWeight11",
                   "other07","other09","other11",
                   "logy07","logy09","logy11",
                   "region07","region09","region11",
                   "retired07","retired09","retired11",
                   "unemployed07","unemployed09","unemployed11",
                   "white07","white09","white11",
                   "year07","year09","year11",
                   "yob07","yob09","yob11",
                   "logActiveSaving07","logActiveSaving09","logActiveSaving11",
                   "age07","age09","age11","quintile")

f070911 <- f070911[,f070911_keeps]
# wide to long

regressionData_030507 <- reshape(f030507, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f030507))),
                                              educ=c(grep("educ", colnames(f030507))),
                                              employed=c(grep("^employed", colnames(f030507))),
                                              exIncome=c(grep("exIncome", colnames(f030507))),
                                              famSize=c(grep("famSize", colnames(f030507))),
                                              kids=c(grep("kids[^Out]", colnames(f030507))),
                                              kidsOut=c(grep("kidsOut", colnames(f030507))),
                                              longWeight=c(grep("longWeight", colnames(f030507))),
                                              other=c(grep("other", colnames(f030507))), 
                                              logy=c(grep("logy",colnames(f030507))),
                                              region=c(grep("region", colnames(f030507))),
                                              retired=c(grep("retired",colnames(f030507))),
                                              unemployed=c(grep("unemployed", colnames(f030507))),
                                              white=c(grep("white", colnames(f030507))),
                                              year=c(grep("year", colnames(f030507))),
                                              yob=c(grep("yob",colnames(f030507))),
                                              logActiveSaving=c(grep("logActiveSaving",colnames(f030507))),
                                              age=c(grep("age",colnames(f030507)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logActiveSaving","age"), #,"bigCity"),
                                 times=c("03", "05","07"))


regressionData_070911 <- reshape(f070911, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f070911))),
                                              educ=c(grep("educ", colnames(f070911))),
                                              employed=c(grep("^employed", colnames(f070911))),
                                              exIncome=c(grep("exIncome", colnames(f070911))),
                                              famSize=c(grep("famSize", colnames(f070911))),
                                              kids=c(grep("kids[^Out]", colnames(f070911))),
                                              kidsOut=c(grep("kidsOut", colnames(f070911))),
                                              longWeight=c(grep("longWeight", colnames(f070911))),
                                              other=c(grep("other", colnames(f070911))), 
                                              logy=c(grep("logy",colnames(f070911))),
                                              region=c(grep("region", colnames(f070911))),
                                              retired=c(grep("retired",colnames(f070911))),
                                              unemployed=c(grep("unemployed", colnames(f070911))),
                                              white=c(grep("white", colnames(f070911))),
                                              year=c(grep("year", colnames(f070911))),
                                              yob=c(grep("yob",colnames(f070911))),
                                              logActiveSaving=c(grep("logActiveSaving",colnames(f070911))),
                                              age=c(grep("age",colnames(f070911)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logActiveSaving","age"), #,"bigCity"),
                                 times=c("07", "09","11"))


# create factors
regressionData_030507$year <- factor(regressionData_030507$year)
regressionData_030507$yob <- factor(regressionData_030507$yob)
regressionData_030507$educ <- factor(regressionData_030507$educ)
regressionData_030507$white <- factor(regressionData_030507$white)
regressionData_030507$black <- factor(regressionData_030507$black)
regressionData_030507$other <- factor(regressionData_030507$other)
regressionData_030507$famSize <- factor(regressionData_030507$famSize)
regressionData_030507$kids <- factor(regressionData_030507$kids)
regressionData_030507$employed <- factor(regressionData_030507$employed)
regressionData_030507$unemployed <- factor(regressionData_030507$unemployed)
regressionData_030507$retired <- factor(regressionData_030507$retired)
regressionData_030507$exIncome <- factor(regressionData_030507$exIncome)
regressionData_030507$region <- factor(regressionData_030507$region)
regressionData_030507$kidsOut <- factor(regressionData_030507$kidsOut)

regressionData_070911$year <- factor(regressionData_070911$year)
regressionData_070911$yob <- factor(regressionData_070911$yob)
regressionData_070911$educ <- factor(regressionData_070911$educ)
regressionData_070911$white <- factor(regressionData_070911$white)
regressionData_070911$black <- factor(regressionData_070911$black)
regressionData_070911$other <- factor(regressionData_070911$other)
regressionData_070911$famSize <- factor(regressionData_070911$famSize)
regressionData_070911$kids <- factor(regressionData_070911$kids)
regressionData_070911$employed <- factor(regressionData_070911$employed)
regressionData_070911$unemployed <- factor(regressionData_070911$unemployed)
regressionData_070911$retired <- factor(regressionData_070911$retired)
regressionData_070911$exIncome <- factor(regressionData_070911$exIncome)
regressionData_070911$region <- factor(regressionData_070911$region)
regressionData_070911$kidsOut <- factor(regressionData_070911$kidsOut)



# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=regressionData_030507,
                                     nest=TRUE)

familyPanelSurvey070911 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=regressionData_070911,
                                     nest=TRUE)





# do the regressions
# we first regress log income and log consumption expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log consumption d(cit) and
#log income d(yit).

income_030507.lm = svyglm(logy ~ year +
                            yob +
                            age +
                            educ +
                            white +
                            black +
                            other +
                            famSize + 
                            kids + 
                            employed +
                            unemployed +
                            retired +
                            exIncome +
                            region + 
                            kidsOut + 
                            educ*year +
                            white*year +
                            black*year +
                            other*year +
                            employed*year + 
                            unemployed*year +
                            retired*year +
                            region*year
                          ,familyPanelSurvey030507) 

regressionData_030507$resIncome <-income_030507.lm$residuals

income_070911.lm = svyglm(logy ~ year +
                            yob +
                            age +
                            educ +
                            white +
                            black +
                            other +
                            famSize + 
                            kids + 
                            employed +
                            unemployed +
                            retired +
                            exIncome +
                            region + 
                            kidsOut + 
                            educ*year +
                            white*year +
                            black*year +
                            other*year +
                            employed*year + 
                            unemployed*year +
                            retired*year +
                            region*year
                          , familyPanelSurvey070911) 
regressionData_070911$resIncome <-income_070911.lm$residuals


activeSaving_030507.lm = svyglm(logActiveSaving ~ year +
                                 yob +
                                 age +
                                 educ +
                                 white +
                                 black +
                                 other +
                                 famSize + 
                                 kids + 
                                 employed +
                                 unemployed +
                                 retired +
                                 exIncome +
                                 region + 
                                 kidsOut + 
                                 educ*year +
                                 white*year +
                                 black*year +
                                 other*year +
                                 employed*year + 
                                 unemployed*year +
                                 retired*year +
                                 region*year
                               , familyPanelSurvey030507) 
regressionData_030507$resAS <- activeSaving_030507.lm$residuals


activeSaving_070911.lm = svyglm(logActiveSaving ~ year +
                                 yob +
                                 age + 
                                 educ +
                                 white +
                                 black +
                                 other +
                                 famSize + 
                                 kids + 
                                 employed +
                                 unemployed +
                                 retired +
                                 exIncome +
                                 region + 
                                 kidsOut + 
                                 educ*year +
                                 white*year +
                                 black*year +
                                 other*year +
                                 employed*year + 
                                 unemployed*year +
                                 retired*year +
                                 region*year,
                               familyPanelSurvey070911) 
regressionData_070911$resAS <-activeSaving_070911.lm$residuals


covData_030507 = regressionData_030507[ , c("uniqueID","quintile","year","resAS","resIncome")]
covData_070911 = regressionData_070911[ , c("uniqueID","quintile","year","resAS","resIncome")]

covData_030507 <- reshape(covData_030507,idvar="uniqueID",direction="wide",v.names=c("resAS","resIncome"),timevar="year")
covData_070911 <- reshape(covData_070911,idvar="uniqueID",direction="wide",v.names=c("resAS","resIncome"),timevar="year")

covData_030507$dct <- covData_030507$resAS.2005 - covData_030507$resAS.2003
covData_030507$dyt <- covData_030507$resIncome.2005 - covData_030507$resIncome.2003
covData_030507$dytplus1 <- covData_030507$resIncome.2007 - covData_030507$resIncome.2005


covData_070911$dct <- covData_070911$resAS.2009 - covData_070911$resAS.2007
covData_070911$dyt <- covData_070911$resIncome.2009 - covData_070911$resIncome.2007
covData_070911$dytplus1 <- covData_070911$resIncome.2011 - covData_070911$resIncome.2007


for (i in c(1,2,3,4,5)){
  assign(paste0("MPC_030507_q",i),
         cov(subset(covData_030507$dct,covData_030507$quintile==i),subset(covData_030507$dytplus1,covData_030507$quintile==i))/
           cov(subset(covData_030507$dytplus1,covData_030507$quintile==i),subset(covData_030507$dyt,covData_030507$quintile==i))
  )
  print(eval(as.name(paste0("MPC_030507_q",i))))
}

for (i in c(1,2,3,4,5)){
  assign(paste0("MPC_070911_q",i),
         cov(subset(covData_070911$dct,covData_070911$quintile==i),subset(covData_070911$dytplus1,covData_070911$quintile==i))/
           cov(subset(covData_070911$dytplus1,covData_070911$quintile==i),subset(covData_070911$dyt,covData_070911$quintile==i))
  )
  print(eval(as.name(paste0("MPC_070911_q",i))))
}







# consumption is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
# income is in the previous year to the survey, so 2002, 2004, 2006 (3 years)
for (j in c("030507","070911")){
  print("Consumption")
  print(j)
  for (i in c("1","2","3","4","5")){
    assign(paste0("consumption_",j,"_q",i),
           ((svymean(~eval(as.name(paste0("consumption",substr(j,5,6)))),
                     subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
               svymean(~eval(as.name(paste0("consumption",substr(j,3,4)))),
                       subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
               svymean(~eval(as.name(paste0("consumption",substr(j,1,2)))),
                       subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3)/
             ((svymean(~eval(as.name(paste0("income",substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE) +
                 svymean(~eval(as.name(paste0("income",substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))/3))
    print(paste0("q",i,":",(unname(eval(as.name(paste0("consumption_",j,"_q",i)))[1]))))
  }
}



