# Kaplan Violante - consumption - IV, weighted
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2009,11,13

f030507_KVC_wealth <- f030507
f091113_KVC_wealth <- f091113


##### CREATE VARIABLES ########
# create year of birth

# create year variable
f030507_KVC_wealth$year03 <- 2003
f030507_KVC_wealth$year05 <- 2005
f030507_KVC_wealth$year07 <- 2007

f091113_KVC_wealth$year09 <- 2009
f091113_KVC_wealth$year11 <- 2011
f091113_KVC_wealth$year13 <- 2013


# log consumption - move into positive
min_all <- min(min(f030507_KVC_wealth$consumption03,na.rm=TRUE),min(f030507_KVC_wealth$consumption05,na.rm=TRUE),min(f030507_KVC_wealth$consumption07,na.rm=TRUE),
               min(f091113_KVC_wealth$consumption09,na.rm=TRUE),min(f091113_KVC_wealth$consumption11,na.rm=TRUE),min(f091113_KVC_wealth$consumption13,na.rm=TRUE),
               min(f030507_KVC_wealth$income03,na.rm=TRUE),min(f030507_KVC_wealth$income05,na.rm=TRUE),min(f030507_KVC_wealth$income07,na.rm=TRUE),
               min(f091113_KVC_wealth$income09,na.rm=TRUE),min(f091113_KVC_wealth$income11,na.rm=TRUE),min(f091113_KVC_wealth$income13,na.rm=TRUE))

f030507_KVC_wealth$logconsumption03 <- log(f030507_KVC_wealth$consumption03 + abs(min_all) +1)
f030507_KVC_wealth$logconsumption05 <- log(f030507_KVC_wealth$consumption05+ abs(min_all) +1)
f030507_KVC_wealth$logconsumption07 <- log(f030507_KVC_wealth$consumption07+ abs(min_all) +1)

f091113_KVC_wealth$logconsumption09 <- log(f091113_KVC_wealth$consumption09+ abs(min_all) +1)
f091113_KVC_wealth$logconsumption11 <- log(f091113_KVC_wealth$consumption11+ abs(min_all) +1)
f091113_KVC_wealth$logconsumption13 <- log(f091113_KVC_wealth$consumption13+ abs(min_all) +1)


# log income - move into positive

f030507_KVC_wealth$logy03 <- log(f030507_KVC_wealth$income03 + abs(min_all) +1)
f030507_KVC_wealth$logy05 <- log(f030507_KVC_wealth$income05+ abs(min_all) +1)
f030507_KVC_wealth$logy07 <- log(f030507_KVC_wealth$income07+ abs(min_all) +1)

f091113_KVC_wealth$logy09 <- log(f091113_KVC_wealth$income09+ abs(min_all) +1)
f091113_KVC_wealth$logy11 <- log(f091113_KVC_wealth$income11+ abs(min_all) +1)
f091113_KVC_wealth$logy13 <- log(f091113_KVC_wealth$income13+ abs(min_all) +1)
rm(min_all)




f030507_KVC_wealth_keeps <- c("uniqueID","primarySamplingUnit","stratification","black03","black05","black07",
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
                       "logconsumption03","logconsumption05","logconsumption07",
                       "age03","age05","age07",
                       "income03","income05","income07",
                       "poorHTM03","poorHTM05","poorHTM07",
                       "richHTM03","richHTM05","richHTM07",
                       "totalWealth03","totalWealth05","totalWealth07",
                       "interestRate03","interestRate05","interestRate07")

f030507_KVC_wealth <- f030507_KVC_wealth[,f030507_KVC_wealth_keeps]

f091113_KVC_wealth_keeps <- c("uniqueID","primarySamplingUnit","stratification","black09","black11","black13",
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
                       "logconsumption09","logconsumption11","logconsumption13",
                       "age09","age11","age13",
                       "income09","income11","income13",
                       "poorHTM09","poorHTM11","poorHTM13",
                       "richHTM09","richHTM11","richHTM13",
                       "totalWealth09","totalWealth11","totalWealth13",
                       "interestRate09","interestRate11","interestRate13")

f091113_KVC_wealth <- f091113_KVC_wealth[,f091113_KVC_wealth_keeps]

rm(f091113_KVC_wealth_keeps,f030507_KVC_wealth_keeps)

# drop NA 
f030507_KVC_wealth <- na.omit(f030507_KVC_wealth)
f091113_KVC_wealth <- na.omit(f091113_KVC_wealth)


# quintiles

# survey design

# familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f030507_KVC_wealth,
#                                      nest=TRUE)
# 
# familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f091113_KVC_wealth,
#                                      nest=TRUE)

quintiles_03 <- quantile(f030507_KVC_wealth$totalWealth03, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_05 <- quantile(f030507_KVC_wealth$totalWealth05, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_07 <- quantile(f030507_KVC_wealth$totalWealth07, seq(0, 1, 0.2),NA.rm=TRUE)

quintiles_09 <- quantile(f091113_KVC_wealth$totalWealth09, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_11 <- quantile(f091113_KVC_wealth$totalWealth11, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_13 <- quantile(f091113_KVC_wealth$totalWealth13, seq(0, 1, 0.2),NA.rm=TRUE)


f030507_KVC_wealth$quintile03 <- ifelse(f030507_KVC_wealth$totalWealth03<quintiles_03[2],1,
                                 ifelse(f030507_KVC_wealth$totalWealth03>=quintiles_03[2] & f030507_KVC_wealth$totalWealth03<quintiles_03[3],2,
                                        ifelse(f030507_KVC_wealth$totalWealth03>=quintiles_03[3] & f030507_KVC_wealth$totalWealth03<quintiles_03[4],3,
                                               ifelse(f030507_KVC_wealth$totalWealth03>=quintiles_03[4]& f030507_KVC_wealth$totalWealth03<quintiles_03[5],4,
                                                      ifelse(f030507_KVC_wealth$totalWealth03>=quintiles_03[5],5,NA)))))
f030507_KVC_wealth$quintile05 <- ifelse(f030507_KVC_wealth$totalWealth05<quintiles_05[2],1,
                                 ifelse(f030507_KVC_wealth$totalWealth05>=quintiles_05[2] & f030507_KVC_wealth$totalWealth05<quintiles_05[3],2,
                                        ifelse(f030507_KVC_wealth$totalWealth05>=quintiles_05[3] & f030507_KVC_wealth$totalWealth05<quintiles_05[4],3,
                                               ifelse(f030507_KVC_wealth$totalWealth05>=quintiles_05[4]& f030507_KVC_wealth$totalWealth05<quintiles_05[5],4,
                                                      ifelse(f030507_KVC_wealth$totalWealth05>=quintiles_05[5],5,NA)))))
f030507_KVC_wealth$quintile07 <- ifelse(f030507_KVC_wealth$totalWealth07<quintiles_07[2],1,
                                 ifelse(f030507_KVC_wealth$totalWealth07>=quintiles_07[2] & f030507_KVC_wealth$totalWealth07<quintiles_07[3],2,
                                        ifelse(f030507_KVC_wealth$totalWealth07>=quintiles_07[3] & f030507_KVC_wealth$totalWealth07<quintiles_07[4],3,
                                               ifelse(f030507_KVC_wealth$totalWealth07>=quintiles_07[4]& f030507_KVC_wealth$totalWealth07<quintiles_07[5],4,
                                                      ifelse(f030507_KVC_wealth$totalWealth07>=quintiles_07[5],5,NA)))))
f091113_KVC_wealth$quintile09 <- ifelse(f091113_KVC_wealth$totalWealth09<quintiles_09[2],1,
                                 ifelse(f091113_KVC_wealth$totalWealth09>=quintiles_09[2] & f091113_KVC_wealth$totalWealth09<quintiles_09[3],2,
                                        ifelse(f091113_KVC_wealth$totalWealth09>=quintiles_09[3] & f091113_KVC_wealth$totalWealth09<quintiles_09[4],3,
                                               ifelse(f091113_KVC_wealth$totalWealth09>=quintiles_09[4]& f091113_KVC_wealth$totalWealth09<quintiles_09[5],4,
                                                      ifelse(f091113_KVC_wealth$totalWealth09>=quintiles_09[5],5,NA)))))
f091113_KVC_wealth$quintile11 <- ifelse(f091113_KVC_wealth$totalWealth11<quintiles_11[2],1,
                                 ifelse(f091113_KVC_wealth$totalWealth11>=quintiles_11[2] & f091113_KVC_wealth$totalWealth11<quintiles_11[3],2,
                                        ifelse(f091113_KVC_wealth$totalWealth11>=quintiles_11[3] & f091113_KVC_wealth$totalWealth11<quintiles_11[4],3,
                                               ifelse(f091113_KVC_wealth$totalWealth11>=quintiles_11[4]& f091113_KVC_wealth$totalWealth11<quintiles_11[5],4,
                                                      ifelse(f091113_KVC_wealth$totalWealth11>=quintiles_11[5],5,NA)))))
f091113_KVC_wealth$quintile13 <- ifelse(f091113_KVC_wealth$totalWealth13<quintiles_13[2],1,
                                 ifelse(f091113_KVC_wealth$totalWealth13>=quintiles_13[2] & f091113_KVC_wealth$totalWealth13<quintiles_13[3],2,
                                        ifelse(f091113_KVC_wealth$totalWealth13>=quintiles_13[3] & f091113_KVC_wealth$totalWealth13<quintiles_13[4],3,
                                               ifelse(f091113_KVC_wealth$totalWealth13>=quintiles_13[4]& f091113_KVC_wealth$totalWealth13<quintiles_13[5],4,
                                                      ifelse(f091113_KVC_wealth$totalWealth13>=quintiles_13[5],5,NA)))))



# familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f030507_KVC_wealth,
#                                      nest=TRUE)
# 
# familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f091113_KVC_wealth,
#                                      nest=TRUE)


# drop income

f030507_KVC_wealth <- select(f030507_KVC_wealth,-c("income03","income05","income07"))
f091113_KVC_wealth <- select(f091113_KVC_wealth,-c("income09","income11","income13"))


# wide to long

KVC_wealth_regressionData_030507 <- reshape(f030507_KVC_wealth, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                     varying=list(black=c(grep("black", colnames(f030507_KVC_wealth))),
                                                  educ=c(grep("educ", colnames(f030507_KVC_wealth))),
                                                  employed=c(grep("^employed", colnames(f030507_KVC_wealth))),
                                                  exIncome=c(grep("exIncome", colnames(f030507_KVC_wealth))),
                                                  famSize=c(grep("famSize", colnames(f030507_KVC_wealth))),
                                                  kids=c(grep("kids[^Out]", colnames(f030507_KVC_wealth))),
                                                  kidsOut=c(grep("kidsOut", colnames(f030507_KVC_wealth))),
                                                  longWeight=c(grep("longWeight", colnames(f030507_KVC_wealth))),
                                                  other=c(grep("other", colnames(f030507_KVC_wealth))), 
                                                  logy=c(grep("logy",colnames(f030507_KVC_wealth))),
                                                  region=c(grep("region", colnames(f030507_KVC_wealth))),
                                                  retired=c(grep("retired",colnames(f030507_KVC_wealth))),
                                                  unemployed=c(grep("unemployed", colnames(f030507_KVC_wealth))),
                                                  white=c(grep("white", colnames(f030507_KVC_wealth))),
                                                  year=c(grep("year", colnames(f030507_KVC_wealth))),
                                                  logconsumption=c(grep("logconsumption",colnames(f030507_KVC_wealth))),
                                                  age=c(grep("age",colnames(f030507_KVC_wealth))),
                                                  poorHTM=c(grep("poorHTM",colnames(f030507_KVC_wealth))),
                                                  richHTM=c(grep("richHTM",colnames(f030507_KVC_wealth))),
                                                  quintile=c(grep("quintile",colnames(f030507_KVC_wealth))),
                                                  totalWealth=c(grep("totalWealth",colnames(f030507_KVC_wealth))),
                                                  interestRate=c(grep("interestRate",colnames(f030507_KVC_wealth)))),
                                     v.names = c("black","educ","employed",
                                                 "exIncome","famSize","kids","kidsOut","longWeight",
                                                 "other","logy","region","retired","unemployed","white","year","logconsumption","age","poorHTM","richHTM","quintile","totalWealth","interestRate"), #,"bigCity"),
                                     times=c("03", "05","07"))


KVC_wealth_regressionData_091113 <- reshape(f091113_KVC_wealth, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                     varying=list(black=c(grep("black", colnames(f091113_KVC_wealth))),
                                                  educ=c(grep("educ", colnames(f091113_KVC_wealth))),
                                                  employed=c(grep("^employed", colnames(f091113_KVC_wealth))),
                                                  exIncome=c(grep("exIncome", colnames(f091113_KVC_wealth))),
                                                  famSize=c(grep("famSize", colnames(f091113_KVC_wealth))),
                                                  kids=c(grep("kids[^Out]", colnames(f091113_KVC_wealth))),
                                                  kidsOut=c(grep("kidsOut", colnames(f091113_KVC_wealth))),
                                                  longWeight=c(grep("longWeight", colnames(f091113_KVC_wealth))),
                                                  other=c(grep("other", colnames(f091113_KVC_wealth))), 
                                                  logy=c(grep("logy",colnames(f091113_KVC_wealth))),
                                                  region=c(grep("region", colnames(f091113_KVC_wealth))),
                                                  retired=c(grep("retired",colnames(f091113_KVC_wealth))),
                                                  unemployed=c(grep("unemployed", colnames(f091113_KVC_wealth))),
                                                  white=c(grep("white", colnames(f091113_KVC_wealth))),
                                                  year=c(grep("year", colnames(f091113_KVC_wealth))),
                                                  logconsumption=c(grep("logconsumption",colnames(f091113_KVC_wealth))),
                                                  age=c(grep("age",colnames(f091113_KVC_wealth))),
                                                  poorHTM=c(grep("poorHTM",colnames(f091113_KVC_wealth))),
                                                  richHTM=c(grep("richHTM",colnames(f091113_KVC_wealth))),
                                                  quintile=c(grep("quintile",colnames(f091113_KVC_wealth))),
                                                  totalWealth=c(grep("totalWealth",colnames(f091113_KVC_wealth))),
                                                  interestRate=c(grep("interestRate",colnames(f091113_KVC_wealth)))),
                                     v.names = c("black","educ","employed",
                                                 "exIncome","famSize","kids","kidsOut","longWeight",
                                                 "other","logy","region","retired","unemployed","white","year","logconsumption","age","poorHTM","richHTM","quintile","totalWealth","interestRate"), #,"bigCity"),
                                     times=c("09", "11","13"))

# create year of birth variable
KVC_wealth_regressionData_030507$yob <- KVC_wealth_regressionData_030507$year - KVC_wealth_regressionData_030507$age
KVC_wealth_regressionData_091113$yob <- KVC_wealth_regressionData_091113$year - KVC_wealth_regressionData_091113$age

# create factors
KVC_wealth_regressionData_030507$year <- factor(KVC_wealth_regressionData_030507$year)
KVC_wealth_regressionData_030507$yob <- factor(KVC_wealth_regressionData_030507$yob)
KVC_wealth_regressionData_030507$educ <- factor(KVC_wealth_regressionData_030507$educ)
KVC_wealth_regressionData_030507$white <- factor(KVC_wealth_regressionData_030507$white)
KVC_wealth_regressionData_030507$black <- factor(KVC_wealth_regressionData_030507$black)
KVC_wealth_regressionData_030507$other <- factor(KVC_wealth_regressionData_030507$other)
KVC_wealth_regressionData_030507$famSize <- factor(KVC_wealth_regressionData_030507$famSize)
KVC_wealth_regressionData_030507$kids <- factor(KVC_wealth_regressionData_030507$kids)
KVC_wealth_regressionData_030507$employed <- factor(KVC_wealth_regressionData_030507$employed)
KVC_wealth_regressionData_030507$unemployed <- factor(KVC_wealth_regressionData_030507$unemployed)
KVC_wealth_regressionData_030507$retired <- factor(KVC_wealth_regressionData_030507$retired)
KVC_wealth_regressionData_030507$exIncome <- factor(KVC_wealth_regressionData_030507$exIncome)
KVC_wealth_regressionData_030507$region <- factor(KVC_wealth_regressionData_030507$region)
KVC_wealth_regressionData_030507$kidsOut <- factor(KVC_wealth_regressionData_030507$kidsOut)
KVC_wealth_regressionData_030507$poorHTM <- factor(KVC_wealth_regressionData_030507$poorHTM)
KVC_wealth_regressionData_030507$richHTM <- factor(KVC_wealth_regressionData_030507$richHTM)
KVC_wealth_regressionData_030507$interestRate <- factor(KVC_wealth_regressionData_030507$interestRate)

KVC_wealth_regressionData_091113$year <- factor(KVC_wealth_regressionData_091113$year)
KVC_wealth_regressionData_091113$yob <- factor(KVC_wealth_regressionData_091113$yob)
KVC_wealth_regressionData_091113$educ <- factor(KVC_wealth_regressionData_091113$educ)
KVC_wealth_regressionData_091113$white <- factor(KVC_wealth_regressionData_091113$white)
KVC_wealth_regressionData_091113$black <- factor(KVC_wealth_regressionData_091113$black)
KVC_wealth_regressionData_091113$other <- factor(KVC_wealth_regressionData_091113$other)
KVC_wealth_regressionData_091113$famSize <- factor(KVC_wealth_regressionData_091113$famSize)
KVC_wealth_regressionData_091113$kids <- factor(KVC_wealth_regressionData_091113$kids)
KVC_wealth_regressionData_091113$employed <- factor(KVC_wealth_regressionData_091113$employed)
KVC_wealth_regressionData_091113$unemployed <- factor(KVC_wealth_regressionData_091113$unemployed)
KVC_wealth_regressionData_091113$retired <- factor(KVC_wealth_regressionData_091113$retired)
KVC_wealth_regressionData_091113$exIncome <- factor(KVC_wealth_regressionData_091113$exIncome)
KVC_wealth_regressionData_091113$region <- factor(KVC_wealth_regressionData_091113$region)
KVC_wealth_regressionData_091113$kidsOut <- factor(KVC_wealth_regressionData_091113$kidsOut)
KVC_wealth_regressionData_091113$poorHTM <- factor(KVC_wealth_regressionData_091113$poorHTM)
KVC_wealth_regressionData_091113$richHTM <- factor(KVC_wealth_regressionData_091113$richHTM)
KVC_wealth_regressionData_091113$interestRate <- factor(KVC_wealth_regressionData_091113$interestRate)


# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVC_wealth_regressionData_030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVC_wealth_regressionData_091113,
                                     nest=TRUE)





# do the regressions
# we first regress log income and log consumption expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log consumption d(cit) and
#log income d(yit).

KVC_wealth_income_030507.lm = svyglm(logy ~ year +
                                yob +
                                #age +
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
                                poorHTM +
                                richHTM + 
                                educ*year +
                                white*year +
                                black*year +
                                other*year +
                                employed*year + 
                                unemployed*year +
                                retired*year +
                                region*year +
                                #totalWealth + 
                                interestRate
                              ,familyPanelSurvey030507) 

KVC_wealth_regressionData_030507$resIncome <-KVC_wealth_income_030507.lm$residuals

KVC_wealth_income_091113.lm = svyglm(logy ~ year +
                                yob +
                                #age +
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
                                poorHTM +
                                richHTM + 
                                educ*year +
                                white*year +
                                black*year +
                                other*year +
                                employed*year + 
                                unemployed*year +
                                retired*year +
                                region*year +
                                #totalWealth + 
                                interestRate
                              , familyPanelSurvey091113) 
KVC_wealth_regressionData_091113$resIncome <-KVC_wealth_income_091113.lm$residuals


KVC_wealth_consumption_030507.lm = svyglm(logconsumption ~ year +
                                     yob +
                                     #age +
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
                                     poorHTM +
                                     richHTM + 
                                     educ*year +
                                     white*year +
                                     black*year +
                                     other*year +
                                     employed*year + 
                                     unemployed*year +
                                     retired*year +
                                     region*year +
                                     #totalWealth +
                                     interestRate
                                   , familyPanelSurvey030507) 
KVC_wealth_regressionData_030507$resconsumption <- KVC_wealth_consumption_030507.lm$residuals


KVC_wealth_consumption_091113.lm = svyglm(logconsumption ~ year +
                                     yob +
                                     #age + 
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
                                     poorHTM +
                                     richHTM + 
                                     educ*year +
                                     white*year +
                                     black*year +
                                     other*year +
                                     employed*year + 
                                     unemployed*year +
                                     retired*year +
                                     region*year + 
                                     #totalWealth + 
                                     interestRate,
                                   familyPanelSurvey091113) 
KVC_wealth_regressionData_091113$resconsumption <-KVC_wealth_consumption_091113.lm$residuals


KVC_wealth_covData_030507 = KVC_wealth_regressionData_030507[ , c("uniqueID","quintile","year","resconsumption","resIncome")]
KVC_wealth_covData_091113 = KVC_wealth_regressionData_091113[ , c("uniqueID","quintile","year","resconsumption","resIncome")]

# deal with outliers - take off bottom 0.1%
resconsumption_q_030507 <- quantile(KVC_wealth_covData_030507$resconsumption,seq(0,1,0.001),na.rm=TRUE)
resconsumption_q_091113 <- quantile(KVC_wealth_covData_091113$resconsumption,seq(0,1,0.001),na.rm=TRUE)

KVC_wealth_covData_030507 <- subset(KVC_wealth_covData_030507,KVC_wealth_covData_030507$resconsumption>resconsumption_q_030507[2] & KVC_wealth_covData_030507$resconsumption<resconsumption_q_030507[1000])
KVC_wealth_covData_091113 <- subset(KVC_wealth_covData_091113,KVC_wealth_covData_091113$resconsumption>resconsumption_q_091113[2] & KVC_wealth_covData_091113$resconsumption<resconsumption_q_091113[1000])

resIncome_q_030507 <- quantile(KVC_wealth_covData_030507$resIncome,seq(0,1,0.001),na.rm=TRUE)
resIncome_q_091113 <- quantile(KVC_wealth_covData_091113$resIncome,seq(0,1,0.001),na.rm=TRUE)

KVC_wealth_covData_030507 <- subset(KVC_wealth_covData_030507,KVC_wealth_covData_030507$resIncome>resIncome_q_030507[2] & KVC_wealth_covData_030507$resIncome<resIncome_q_030507[1000])
KVC_wealth_covData_091113 <- subset(KVC_wealth_covData_091113,KVC_wealth_covData_091113$resIncome>resIncome_q_091113[2] & KVC_wealth_covData_091113$resIncome<resIncome_q_091113[1000])

#plot residuals
plot(KVC_wealth_covData_030507$resIncome,KVC_wealth_covData_030507$resconsumption)
plot(KVC_wealth_covData_091113$resIncome,KVC_wealth_covData_091113$resconsumption)

KVC_wealth_covData_030507 <- reshape(KVC_wealth_covData_030507,idvar="uniqueID",direction="wide",v.names=c("quintile","resconsumption","resIncome"),timevar="year")
KVC_wealth_covData_091113 <- reshape(KVC_wealth_covData_091113,idvar="uniqueID",direction="wide",v.names=c("quintile","resconsumption","resIncome"),timevar="year")

KVC_wealth_covData_030507$dst <- KVC_wealth_covData_030507$resconsumption.2005 - KVC_wealth_covData_030507$resconsumption.2003
KVC_wealth_covData_030507$dyt <- KVC_wealth_covData_030507$resIncome.2005 - KVC_wealth_covData_030507$resIncome.2003
KVC_wealth_covData_030507$dytplus1 <- KVC_wealth_covData_030507$resIncome.2007 - KVC_wealth_covData_030507$resIncome.2005


KVC_wealth_covData_091113$dst <- KVC_wealth_covData_091113$resconsumption.2011 - KVC_wealth_covData_091113$resconsumption.2009
KVC_wealth_covData_091113$dyt <- KVC_wealth_covData_091113$resIncome.2011 - KVC_wealth_covData_091113$resIncome.2009
KVC_wealth_covData_091113$dytplus1 <- KVC_wealth_covData_091113$resIncome.2013 - KVC_wealth_covData_091113$resIncome.2011




# MPC_wealth
print("MPC_wealth as 07")
for (i in c(1,2,3,4,5)){
  MPC_wealth_mdl_030507 <- ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVC_wealth_covData_030507, subset=quintile.2003==i)
  assign(paste0("MPC_wealth_mdl_030507_q",i),
         MPC_wealth_mdl_030507)
  assign(paste0("MPC_wealth_030507_q",i),
         MPC_wealth_mdl_030507$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPC_wealth_mdl_030507)
  assign(paste0("MPC_wealth_CI_low_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPC_wealth_CI_up_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPC_wealth_CI_low_07_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPC_wealth_030507_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPC_wealth_CI_up_07_q",i))))))
}

# MPC_wealth
print("MPC_wealth as 13")
for (i in c(1,2,3,4,5)){
  MPC_wealth_mdl_091113 = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVC_wealth_covData_091113,subset=quintile.2009==i)
  assign(paste0("MPC_wealth_mdl_091113_q",i),
         MPC_wealth_mdl_091113)
  assign(paste0("MPC_wealth_091113_q",i),
         MPC_wealth_mdl_091113$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPC_wealth_mdl_091113)
  assign(paste0("MPC_wealth_CI_low_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPC_wealth_CI_up_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPC_wealth_CI_low_13_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPC_wealth_091113_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPC_wealth_CI_up_13_q",i))))))
}

out_MPC_wealth <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                      "year" = c("2007","2007","2007","2007","2007","2013","2013","2013","2013","2013"),
                      "MPC_wealth" = c(MPC_wealth_030507_q1,
                                MPC_wealth_030507_q2,
                                MPC_wealth_030507_q3,
                                MPC_wealth_030507_q4,
                                MPC_wealth_030507_q5,
                                MPC_wealth_091113_q1,
                                MPC_wealth_091113_q2,
                                MPC_wealth_091113_q3,
                                MPC_wealth_091113_q4,
                                MPC_wealth_091113_q5),
                      "MPC_wealth_CI_lower" = c(MPC_wealth_CI_low_07_q1,
                                         MPC_wealth_CI_low_07_q2,
                                         MPC_wealth_CI_low_07_q3,
                                         MPC_wealth_CI_low_07_q4,
                                         MPC_wealth_CI_low_07_q5,
                                         MPC_wealth_CI_low_13_q1,
                                         MPC_wealth_CI_low_13_q2,
                                         MPC_wealth_CI_low_13_q3,
                                         MPC_wealth_CI_low_13_q4,
                                         MPC_wealth_CI_low_13_q5),
                      "MPC_wealth_CI_upper" = c(MPC_wealth_CI_up_07_q1,
                                         MPC_wealth_CI_up_07_q2,
                                         MPC_wealth_CI_up_07_q3,
                                         MPC_wealth_CI_up_07_q4,
                                         MPC_wealth_CI_up_07_q5,
                                         MPC_wealth_CI_up_13_q1,
                                         MPC_wealth_CI_up_13_q2,
                                         MPC_wealth_CI_up_13_q3,
                                         MPC_wealth_CI_up_13_q4,
                                         MPC_wealth_CI_up_13_q5))
pdf(file=paste0(getwd(),"/Results/MPC_wealth.pdf"))
ggplot(data = out_MPC_wealth, aes(x = quantile, y = MPC_wealth, group=year, color=year)) +
  geom_line() + geom_point()+
  geom_ribbon(data= out_MPC_wealth,aes(ymin= MPC_wealth_CI_lower,ymax= MPC_wealth_CI_upper),alpha=0.3) +
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("MPC_wealth") +
  labs(color="Year")
dev.off()
ggplot(data = out_MPC_wealth, aes(x = quantile, y = MPC_wealth, group=year, color=year)) +
  geom_line() + geom_point()+
  geom_ribbon(data= out_MPC_wealth,aes(ymin= MPC_wealth_CI_lower,ymax= MPC_wealth_CI_upper),alpha=0.3) +
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("MPC_wealth") +
  labs(color="Year")

# whole sample MPC_wealth
MPC_wealth_mdl_030507_whole = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVC_wealth_covData_030507)
MPC_wealth_mdl_030507_whole$coefficients[2]
anderson.rubin.ci(MPC_wealth_mdl_030507_whole)
# write to text files
# coefficient
write(toString(round(MPC_wealth_mdl_030507_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPC_wealth_030507.txt"))
#standard error
write(toString(round(coef(summary(MPC_wealth_mdl_030507_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPC_wealth_030507_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPC_wealth_mdl_030507_whole))[2,4])),file=paste0(getwd(),"/Results/MPC_wealth_030507_stars.txt"))
#N
write(toString(MPC_wealth_mdl_030507_whole$nobs),file=paste0(getwd(),"/Results/MPC_wealth_030507_N.txt"))


MPC_wealth_mdl_091113_whole = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVC_wealth_covData_091113)
MPC_wealth_mdl_091113_whole$coefficients[2]
anderson.rubin.ci(MPC_wealth_mdl_091113_whole)
# write to text files
# coefficient
write(toString(round(MPC_wealth_mdl_091113_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPC_wealth_091113.txt"))
#standard error
write(toString(round(coef(summary(MPC_wealth_mdl_091113_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPC_wealth_091113_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPC_wealth_mdl_091113_whole))[2,4])),file=paste0(getwd(),"/Results/MPC_wealth_091113_stars.txt"))
#N
write(toString(MPC_wealth_mdl_091113_whole$nobs),file=paste0(getwd(),"/Results/MPC_wealth_091113_N.txt"))



rm(i,temp)#,KVC_wealth_covData_030507, KVC_wealth_covData_091113)
rm(list=ls(pattern="quintiles_"))
rm(list=ls(pattern="quantiles_"))
rm(list=ls(pattern="familyPanelSurvey"))


