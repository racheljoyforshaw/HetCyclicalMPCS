# Kaplan Violante - active saving
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2007,09,11

f030507_KVAS <- f030507
f091113_KVAS <- f091113

##### CREATE VARIABLES ########
# create year of birth

# create year variable
f030507_KVAS$year03 <- 2003
f030507_KVAS$year05 <- 2005
f030507_KVAS$year07 <- 2007

f091113_KVAS$year09 <- 2009
f091113_KVAS$year11 <- 2011
f091113_KVAS$year13 <- 2013

f030507_KVAS$yob03 <- f030507_KVAS$year03 - f030507_KVAS$age03 
f030507_KVAS$yob05 <- f030507_KVAS$year05 - f030507_KVAS$age05 
f030507_KVAS$yob07 <- f030507_KVAS$year07 - f030507_KVAS$age07

f091113_KVAS$yob09 <- f091113_KVAS$year09 - f091113_KVAS$age09 
f091113_KVAS$yob11 <- f091113_KVAS$year11 - f091113_KVAS$age11
f091113_KVAS$yob13 <- f091113_KVAS$year13 - f091113_KVAS$age13



# log activeSaving
min030507 <- min(min(f030507_KVAS$activeSaving03,na.rm=TRUE),min(f030507_KVAS$activeSaving05,na.rm=TRUE),min(f030507_KVAS$activeSaving07,na.rm=TRUE))
f030507_KVAS$logActiveSaving03 <- log(f030507_KVAS$activeSaving03 + abs(min030507) +1)
f030507_KVAS$logActiveSaving05 <- log(f030507_KVAS$activeSaving05+ abs(min030507) +1)
f030507_KVAS$logActiveSaving07 <- log(f030507_KVAS$activeSaving07+ abs(min030507) +1)

min091113 <- min(min(f091113_KVAS$activeSaving09,na.rm=TRUE),min(f091113_KVAS$activeSaving11,na.rm=TRUE),min(f091113_KVAS$activeSaving13,na.rm=TRUE))
f091113_KVAS$logActiveSaving09 <- log(f091113_KVAS$activeSaving09+ abs(min091113) +1)
f091113_KVAS$logActiveSaving11 <- log(f091113_KVAS$activeSaving11+ abs(min091113) +1)
f091113_KVAS$logActiveSaving13 <- log(f091113_KVAS$activeSaving13+ abs(min091113) +1)
rm(min030507, min091113)


# log income

f030507_KVAS$logy03 <- log(f030507_KVAS$income03)
f030507_KVAS$logy05 <- log(f030507_KVAS$income05)
f030507_KVAS$logy07 <- log(f030507_KVAS$income07)

f091113_KVAS$logy09 <- log(f091113_KVAS$income09)
f091113_KVAS$logy11 <- log(f091113_KVAS$income11)
f091113_KVAS$logy13 <- log(f091113_KVAS$income13)




f030507_KVAS_keeps <- c("uniqueID","primarySamplingUnit","stratification","black03","black05","black07",
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
                      "age03","age05","age07",
                      "income03")

f030507_KVAS <- f030507_KVAS[,f030507_KVAS_keeps]

f091113_KVAS_keeps <- c("uniqueID","primarySamplingUnit","stratification","black09","black11","black13",
                      "educ09","educ11","educ13",
                      "employed09","employed11","employed13",
                      "exIncome09","exIncome11","exIncome13",
                      "famSize09","famSize11","famSize13",
                      "kids09","kids11","kids13",
                      "kidsOut09","kidsOut11","kidsOut13",
                      "longWeight09","longWeight11","longWeight13",
                      "other09","other11","other13",
                      "logy09","logy11","logy13",
                      "region09","region11","region13",
                      "retired09","retired11","retired13",
                      "unemployed09","unemployed11","unemployed13",
                      "white09","white11","white13",
                      "year09","year11","year13",
                      "yob09","yob11","yob13",
                      "logActiveSaving09","logActiveSaving11","logActiveSaving13",
                      "age09","age11","age13",
                      "income09")

f091113_KVAS <- f091113_KVAS[,f091113_KVAS_keeps]

rm(f091113_KVAS_keeps,f030507_KVAS_keeps)

# drop NA 
f030507_KVAS <- na.omit(f030507_KVAS)
f091113_KVAS <- na.omit(f091113_KVAS)


# quintiles

# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507_KVAS,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113_KVAS,
                                     nest=TRUE)

quintiles_03 <- svyquantile(~income03,familyPanelSurvey030507, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_09 <- svyquantile(~income09,familyPanelSurvey091113, seq(0, 1, 0.2),NA.rm=TRUE)


f030507_KVAS$quintile <- ifelse(f030507_KVAS$income03<quintiles_03[2],1,
                              ifelse(f030507_KVAS$income03>=quintiles_03[2] & f030507_KVAS$income03<quintiles_03[3],2,
                                     ifelse(f030507_KVAS$income03>=quintiles_03[3] & f030507_KVAS$income03<quintiles_03[4],3,
                                            ifelse(f030507_KVAS$income03>=quintiles_03[4]& f030507_KVAS$income03<quintiles_03[5],4,
                                                   ifelse(f030507_KVAS$income03>=quintiles_03[5],5,NA)))))

f091113_KVAS$quintile <- ifelse(f091113_KVAS$income09<quintiles_09[2],1,
                              ifelse(f091113_KVAS$income09>=quintiles_09[2] & f091113_KVAS$income09<quintiles_09[3],2,
                                     ifelse(f091113_KVAS$income09>=quintiles_09[3] & f091113_KVAS$income09<quintiles_09[4],3,
                                            ifelse(f091113_KVAS$income09>=quintiles_09[4]& f091113_KVAS$income09<quintiles_09[5],4,
                                                   ifelse(f091113_KVAS$income09>=quintiles_09[5],5,NA)))))



familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507_KVAS,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113_KVAS,
                                     nest=TRUE)


# drop income

f030507_KVAS <- select(f030507_KVAS,-c("income03"))
f091113_KVAS <- select(f091113_KVAS,-c("income09"))


# wide to long

regressionData_030507 <- reshape(f030507_KVAS, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f030507_KVAS))),
                                              educ=c(grep("educ", colnames(f030507_KVAS))),
                                              employed=c(grep("^employed", colnames(f030507_KVAS))),
                                              exIncome=c(grep("exIncome", colnames(f030507_KVAS))),
                                              famSize=c(grep("famSize", colnames(f030507_KVAS))),
                                              kids=c(grep("kids[^Out]", colnames(f030507_KVAS))),
                                              kidsOut=c(grep("kidsOut", colnames(f030507_KVAS))),
                                              longWeight=c(grep("longWeight", colnames(f030507_KVAS))),
                                              other=c(grep("other", colnames(f030507_KVAS))), 
                                              logy=c(grep("logy",colnames(f030507_KVAS))),
                                              region=c(grep("region", colnames(f030507_KVAS))),
                                              retired=c(grep("retired",colnames(f030507_KVAS))),
                                              unemployed=c(grep("unemployed", colnames(f030507_KVAS))),
                                              white=c(grep("white", colnames(f030507_KVAS))),
                                              year=c(grep("year", colnames(f030507_KVAS))),
                                              yob=c(grep("yob",colnames(f030507_KVAS))),
                                              logActiveSaving=c(grep("logActiveSaving",colnames(f030507_KVAS))),
                                              age=c(grep("age",colnames(f030507_KVAS)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logActiveSaving","age"), #,"bigCity"),
                                 times=c("03", "05","07"))


regressionData_091113 <- reshape(f091113_KVAS, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f091113_KVAS))),
                                              educ=c(grep("educ", colnames(f091113_KVAS))),
                                              employed=c(grep("^employed", colnames(f091113_KVAS))),
                                              exIncome=c(grep("exIncome", colnames(f091113_KVAS))),
                                              famSize=c(grep("famSize", colnames(f091113_KVAS))),
                                              kids=c(grep("kids[^Out]", colnames(f091113_KVAS))),
                                              kidsOut=c(grep("kidsOut", colnames(f091113_KVAS))),
                                              longWeight=c(grep("longWeight", colnames(f091113_KVAS))),
                                              other=c(grep("other", colnames(f091113_KVAS))), 
                                              logy=c(grep("logy",colnames(f091113_KVAS))),
                                              region=c(grep("region", colnames(f091113_KVAS))),
                                              retired=c(grep("retired",colnames(f091113_KVAS))),
                                              unemployed=c(grep("unemployed", colnames(f091113_KVAS))),
                                              white=c(grep("white", colnames(f091113_KVAS))),
                                              year=c(grep("year", colnames(f091113_KVAS))),
                                              yob=c(grep("yob",colnames(f091113_KVAS))),
                                              logActiveSaving=c(grep("logActiveSaving",colnames(f091113_KVAS))),
                                              age=c(grep("age",colnames(f091113_KVAS)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logActiveSaving","age"), #,"bigCity"),
                                 times=c("09","11","13"))


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

regressionData_091113$year <- factor(regressionData_091113$year)
regressionData_091113$yob <- factor(regressionData_091113$yob)
regressionData_091113$educ <- factor(regressionData_091113$educ)
regressionData_091113$white <- factor(regressionData_091113$white)
regressionData_091113$black <- factor(regressionData_091113$black)
regressionData_091113$other <- factor(regressionData_091113$other)
regressionData_091113$famSize <- factor(regressionData_091113$famSize)
regressionData_091113$kids <- factor(regressionData_091113$kids)
regressionData_091113$employed <- factor(regressionData_091113$employed)
regressionData_091113$unemployed <- factor(regressionData_091113$unemployed)
regressionData_091113$retired <- factor(regressionData_091113$retired)
regressionData_091113$exIncome <- factor(regressionData_091113$exIncome)
regressionData_091113$region <- factor(regressionData_091113$region)
regressionData_091113$kidsOut <- factor(regressionData_091113$kidsOut)



# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=regressionData_030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=regressionData_091113,
                                     nest=TRUE)





# do the regressions
# we first regress log income and log activeSaving expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log activeSaving d(cit) and
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

income_091113.lm = svyglm(logy ~ year +
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
                          , familyPanelSurvey091113) 
regressionData_091113$resIncome <-income_091113.lm$residuals


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
regressionData_030507$resactiveSaving <- activeSaving_030507.lm$residuals


activeSaving_091113.lm = svyglm(logActiveSaving ~ year +
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
                               familyPanelSurvey091113) 
regressionData_091113$resactiveSaving <-activeSaving_091113.lm$residuals


covData_030507 = regressionData_030507[ , c("uniqueID","quintile","year","resactiveSaving","resIncome")]
covData_091113 = regressionData_091113[ , c("uniqueID","quintile","year","resactiveSaving","resIncome")]

covData_030507 <- reshape(covData_030507,idvar="uniqueID",direction="wide",v.names=c("resactiveSaving","resIncome"),timevar="year")
covData_091113 <- reshape(covData_091113,idvar="uniqueID",direction="wide",v.names=c("resactiveSaving","resIncome"),timevar="year")

covData_030507$dst <- covData_030507$resactiveSaving.2005 - covData_030507$resactiveSaving.2003
covData_030507$dyt <- covData_030507$resIncome.2005 - covData_030507$resIncome.2003
covData_030507$dytplus1 <- covData_030507$resIncome.2007 - covData_030507$resIncome.2005


covData_091113$dst <- covData_091113$resactiveSaving.2011 - covData_091113$resactiveSaving.2009
covData_091113$dyt <- covData_091113$resIncome.2011 - covData_091113$resIncome.2009
covData_091113$dytplus1 <- covData_091113$resIncome.2013 - covData_091113$resIncome.2011

# MPS
print("MPS 030507")
for (i in c(1,2,3,4,5)){
  assign(paste0("MPS_030507_q",i),
         abs(cov(subset(covData_030507$dst,covData_030507$quintile==i),subset(covData_030507$dytplus1,covData_030507$quintile==i))/
           cov(subset(covData_030507$dytplus1,covData_030507$quintile==i),subset(covData_030507$dyt,covData_030507$quintile==i)))
  )
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPS_030507_q",i))))))
}
print("MPS 091113")
for (i in c(1,2,3,4,5)){
  assign(paste0("MPS_091113_q",i),
         abs(cov(subset(covData_091113$dst,covData_091113$quintile==i),subset(covData_091113$dytplus1,covData_091113$quintile==i))/
           cov(subset(covData_091113$dytplus1,covData_091113$quintile==i),subset(covData_091113$dyt,covData_091113$quintile==i)))
  )
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPS_091113_q",i))))))
}


rm(i, covData_030507,covData_091113,quintiles_03,quintiles_09,regressionData_030507,regressionData_091113,
   activeSaving_030507.lm, activeSaving_091113.lm, income_030507.lm, income_091113.lm, familyPanelSurvey030507, familyPanelSurvey091113)