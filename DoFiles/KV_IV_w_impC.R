# Kaplan Violante - impConsumption - IV, weighted
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2009,11,13

f030507_KVIC <- f030507
f091113_KVIC <- f091113


##### CREATE VARIABLES ########
# create year of birth

# create year variable
f030507_KVIC$year03 <- 2003
f030507_KVIC$year05 <- 2005
f030507_KVIC$year07 <- 2007

f091113_KVIC$year09 <- 2009
f091113_KVIC$year11 <- 2011
f091113_KVIC$year13 <- 2013


# log impConsumption - move into positive
min_all <- min(min(f030507_KVIC$impConsumption03,na.rm=TRUE),min(f030507_KVIC$impConsumption05,na.rm=TRUE),min(f030507_KVIC$impConsumption07,na.rm=TRUE),
               min(f091113_KVIC$impConsumption09,na.rm=TRUE),min(f091113_KVIC$impConsumption11,na.rm=TRUE),min(f091113_KVIC$impConsumption13,na.rm=TRUE),
               min(f030507_KVIC$income03,na.rm=TRUE),min(f030507_KVIC$income05,na.rm=TRUE),min(f030507_KVIC$income07,na.rm=TRUE),
               min(f091113_KVIC$income09,na.rm=TRUE),min(f091113_KVIC$income11,na.rm=TRUE),min(f091113_KVIC$income13,na.rm=TRUE))

f030507_KVIC$logimpConsumption03 <- log(f030507_KVIC$impConsumption03 + abs(min_all) +1)
f030507_KVIC$logimpConsumption05 <- log(f030507_KVIC$impConsumption05+ abs(min_all) +1)
f030507_KVIC$logimpConsumption07 <- log(f030507_KVIC$impConsumption07+ abs(min_all) +1)

f091113_KVIC$logimpConsumption09 <- log(f091113_KVIC$impConsumption09+ abs(min_all) +1)
f091113_KVIC$logimpConsumption11 <- log(f091113_KVIC$impConsumption11+ abs(min_all) +1)
f091113_KVIC$logimpConsumption13 <- log(f091113_KVIC$impConsumption13+ abs(min_all) +1)

# log income - move into positive

f030507_KVIC$logy03 <- log(f030507_KVIC$income03 + abs(min_all) +1)
f030507_KVIC$logy05 <- log(f030507_KVIC$income05+ abs(min_all) +1)
f030507_KVIC$logy07 <- log(f030507_KVIC$income07+ abs(min_all) +1)

f091113_KVIC$logy09 <- log(f091113_KVIC$income09+ abs(min_all) +1)
f091113_KVIC$logy11 <- log(f091113_KVIC$income11+ abs(min_all) +1)
f091113_KVIC$logy13 <- log(f091113_KVIC$income13+ abs(min_all) +1)
rm(min_all)




f030507_KVIC_keeps <- c("uniqueID","primarySamplingUnit","stratification","black03","black05","black07",
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
                        "logimpConsumption03","logimpConsumption05","logimpConsumption07",
                        "age03","age05","age07",
                        "income03","income05","income07",
                        "poorHTM03","poorHTM05","poorHTM07",
                        "richHTM03","richHTM05","richHTM07",
                        "totalWealth03","totalWealth05","totalWealth07",
                        "interestRate03","interestRate05","interestRate07")

f030507_KVIC <- f030507_KVIC[,f030507_KVIC_keeps]

f091113_KVIC_keeps <- c("uniqueID","primarySamplingUnit","stratification","black09","black11","black13",
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
                        "logimpConsumption09","logimpConsumption11","logimpConsumption13",
                        "age09","age11","age13",
                        "income09","income11","income13",
                        "poorHTM09","poorHTM11","poorHTM13",
                        "richHTM09","richHTM11","richHTM13",
                        "totalWealth09","totalWealth11","totalWealth13",
                        "interestRate09","interestRate11","interestRate13")

f091113_KVIC <- f091113_KVIC[,f091113_KVIC_keeps]

rm(f091113_KVIC_keeps,f030507_KVIC_keeps)

# drop NA 
f030507_KVIC <- na.omit(f030507_KVIC)
f091113_KVIC <- na.omit(f091113_KVIC)


# quintiles

# survey design

# familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f030507_KVIC,
#                                      nest=TRUE)
# 
# familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f091113_KVIC,
#                                      nest=TRUE)

quintiles_03 <- quantile(f030507_KVIC$income03, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_05 <- quantile(f030507_KVIC$income05, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_07 <- quantile(f030507_KVIC$income07, seq(0, 1, 0.2),NA.rm=TRUE)

quintiles_09 <- quantile(f091113_KVIC$income09, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_11 <- quantile(f091113_KVIC$income11, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_13 <- quantile(f091113_KVIC$income13, seq(0, 1, 0.2),NA.rm=TRUE)


f030507_KVIC$quintile03 <- ifelse(f030507_KVIC$income03<quintiles_03[2],1,
                                  ifelse(f030507_KVIC$income03>=quintiles_03[2] & f030507_KVIC$income03<quintiles_03[3],2,
                                         ifelse(f030507_KVIC$income03>=quintiles_03[3] & f030507_KVIC$income03<quintiles_03[4],3,
                                                ifelse(f030507_KVIC$income03>=quintiles_03[4]& f030507_KVIC$income03<quintiles_03[5],4,
                                                       ifelse(f030507_KVIC$income03>=quintiles_03[5],5,NA)))))
f030507_KVIC$quintile05 <- ifelse(f030507_KVIC$income05<quintiles_05[2],1,
                                  ifelse(f030507_KVIC$income05>=quintiles_05[2] & f030507_KVIC$income05<quintiles_05[3],2,
                                         ifelse(f030507_KVIC$income05>=quintiles_05[3] & f030507_KVIC$income05<quintiles_05[4],3,
                                                ifelse(f030507_KVIC$income05>=quintiles_05[4]& f030507_KVIC$income05<quintiles_05[5],4,
                                                       ifelse(f030507_KVIC$income05>=quintiles_05[5],5,NA)))))
f030507_KVIC$quintile07 <- ifelse(f030507_KVIC$income07<quintiles_07[2],1,
                                  ifelse(f030507_KVIC$income07>=quintiles_07[2] & f030507_KVIC$income07<quintiles_07[3],2,
                                         ifelse(f030507_KVIC$income07>=quintiles_07[3] & f030507_KVIC$income07<quintiles_07[4],3,
                                                ifelse(f030507_KVIC$income07>=quintiles_07[4]& f030507_KVIC$income07<quintiles_07[5],4,
                                                       ifelse(f030507_KVIC$income07>=quintiles_07[5],5,NA)))))
f091113_KVIC$quintile09 <- ifelse(f091113_KVIC$income09<quintiles_09[2],1,
                                  ifelse(f091113_KVIC$income09>=quintiles_09[2] & f091113_KVIC$income09<quintiles_09[3],2,
                                         ifelse(f091113_KVIC$income09>=quintiles_09[3] & f091113_KVIC$income09<quintiles_09[4],3,
                                                ifelse(f091113_KVIC$income09>=quintiles_09[4]& f091113_KVIC$income09<quintiles_09[5],4,
                                                       ifelse(f091113_KVIC$income09>=quintiles_09[5],5,NA)))))
f091113_KVIC$quintile11 <- ifelse(f091113_KVIC$income11<quintiles_11[2],1,
                                  ifelse(f091113_KVIC$income11>=quintiles_11[2] & f091113_KVIC$income11<quintiles_11[3],2,
                                         ifelse(f091113_KVIC$income11>=quintiles_11[3] & f091113_KVIC$income11<quintiles_11[4],3,
                                                ifelse(f091113_KVIC$income11>=quintiles_11[4]& f091113_KVIC$income11<quintiles_11[5],4,
                                                       ifelse(f091113_KVIC$income11>=quintiles_11[5],5,NA)))))
f091113_KVIC$quintile13 <- ifelse(f091113_KVIC$income13<quintiles_13[2],1,
                                  ifelse(f091113_KVIC$income13>=quintiles_13[2] & f091113_KVIC$income13<quintiles_13[3],2,
                                         ifelse(f091113_KVIC$income13>=quintiles_13[3] & f091113_KVIC$income13<quintiles_13[4],3,
                                                ifelse(f091113_KVIC$income13>=quintiles_13[4]& f091113_KVIC$income13<quintiles_13[5],4,
                                                       ifelse(f091113_KVIC$income13>=quintiles_13[5],5,NA)))))



# familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f030507_KVIC,
#                                      nest=TRUE)
# 
# familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f091113_KVIC,
#                                      nest=TRUE)


# drop income

f030507_KVIC <- select(f030507_KVIC,-c("income03","income05","income07"))
f091113_KVIC <- select(f091113_KVIC,-c("income09","income11","income13"))


# wide to long

KVIC_regressionData_030507 <- reshape(f030507_KVIC, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                      varying=list(black=c(grep("black", colnames(f030507_KVIC))),
                                                   educ=c(grep("educ", colnames(f030507_KVIC))),
                                                   employed=c(grep("^employed", colnames(f030507_KVIC))),
                                                   exIncome=c(grep("exIncome", colnames(f030507_KVIC))),
                                                   famSize=c(grep("famSize", colnames(f030507_KVIC))),
                                                   kids=c(grep("kids[^Out]", colnames(f030507_KVIC))),
                                                   kidsOut=c(grep("kidsOut", colnames(f030507_KVIC))),
                                                   longWeight=c(grep("longWeight", colnames(f030507_KVIC))),
                                                   other=c(grep("other", colnames(f030507_KVIC))), 
                                                   logy=c(grep("logy",colnames(f030507_KVIC))),
                                                   region=c(grep("region", colnames(f030507_KVIC))),
                                                   retired=c(grep("retired",colnames(f030507_KVIC))),
                                                   unemployed=c(grep("unemployed", colnames(f030507_KVIC))),
                                                   white=c(grep("white", colnames(f030507_KVIC))),
                                                   year=c(grep("year", colnames(f030507_KVIC))),
                                                   logimpConsumption=c(grep("logimpConsumption",colnames(f030507_KVIC))),
                                                   age=c(grep("age",colnames(f030507_KVIC))),
                                                   poorHTM=c(grep("poorHTM",colnames(f030507_KVIC))),
                                                   richHTM=c(grep("richHTM",colnames(f030507_KVIC))),
                                                   quintile=c(grep("quintile",colnames(f030507_KVIC))),
                                                   totalWealth=c(grep("totalWealth",colnames(f030507_KVIC))),
                                                   interestRate=c(grep("interestRate",colnames(f030507_KVIC)))),
                                      v.names = c("black","educ","employed",
                                                  "exIncome","famSize","kids","kidsOut","longWeight",
                                                  "other","logy","region","retired","unemployed","white","year","logimpConsumption","age","poorHTM","richHTM","quintile","totalWealth","interestRate"), #,"bigCity"),
                                      times=c("03", "05","07"))


KVIC_regressionData_091113 <- reshape(f091113_KVIC, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                      varying=list(black=c(grep("black", colnames(f091113_KVIC))),
                                                   educ=c(grep("educ", colnames(f091113_KVIC))),
                                                   employed=c(grep("^employed", colnames(f091113_KVIC))),
                                                   exIncome=c(grep("exIncome", colnames(f091113_KVIC))),
                                                   famSize=c(grep("famSize", colnames(f091113_KVIC))),
                                                   kids=c(grep("kids[^Out]", colnames(f091113_KVIC))),
                                                   kidsOut=c(grep("kidsOut", colnames(f091113_KVIC))),
                                                   longWeight=c(grep("longWeight", colnames(f091113_KVIC))),
                                                   other=c(grep("other", colnames(f091113_KVIC))), 
                                                   logy=c(grep("logy",colnames(f091113_KVIC))),
                                                   region=c(grep("region", colnames(f091113_KVIC))),
                                                   retired=c(grep("retired",colnames(f091113_KVIC))),
                                                   unemployed=c(grep("unemployed", colnames(f091113_KVIC))),
                                                   white=c(grep("white", colnames(f091113_KVIC))),
                                                   year=c(grep("year", colnames(f091113_KVIC))),
                                                   logimpConsumption=c(grep("logimpConsumption",colnames(f091113_KVIC))),
                                                   age=c(grep("age",colnames(f091113_KVIC))),
                                                   poorHTM=c(grep("poorHTM",colnames(f091113_KVIC))),
                                                   richHTM=c(grep("richHTM",colnames(f091113_KVIC))),
                                                   quintile=c(grep("quintile",colnames(f091113_KVIC))),
                                                   totalWealth=c(grep("totalWealth",colnames(f091113_KVIC))),
                                                   interestRate=c(grep("interestRate",colnames(f091113_KVIC)))),
                                      v.names = c("black","educ","employed",
                                                  "exIncome","famSize","kids","kidsOut","longWeight",
                                                  "other","logy","region","retired","unemployed","white","year","logimpConsumption","age","poorHTM","richHTM","quintile","totalWealth","interestRate"), #,"bigCity"),
                                      times=c("09", "11","13"))

# create year of birth variable
KVIC_regressionData_030507$yob <- KVIC_regressionData_030507$year - KVIC_regressionData_030507$age
KVIC_regressionData_091113$yob <- KVIC_regressionData_091113$year - KVIC_regressionData_091113$age

# create factors
KVIC_regressionData_030507$year <- factor(KVIC_regressionData_030507$year)
KVIC_regressionData_030507$yob <- factor(KVIC_regressionData_030507$yob)
KVIC_regressionData_030507$educ <- factor(KVIC_regressionData_030507$educ)
KVIC_regressionData_030507$white <- factor(KVIC_regressionData_030507$white)
KVIC_regressionData_030507$black <- factor(KVIC_regressionData_030507$black)
KVIC_regressionData_030507$other <- factor(KVIC_regressionData_030507$other)
KVIC_regressionData_030507$famSize <- factor(KVIC_regressionData_030507$famSize)
KVIC_regressionData_030507$kids <- factor(KVIC_regressionData_030507$kids)
KVIC_regressionData_030507$employed <- factor(KVIC_regressionData_030507$employed)
KVIC_regressionData_030507$unemployed <- factor(KVIC_regressionData_030507$unemployed)
KVIC_regressionData_030507$retired <- factor(KVIC_regressionData_030507$retired)
KVIC_regressionData_030507$exIncome <- factor(KVIC_regressionData_030507$exIncome)
KVIC_regressionData_030507$region <- factor(KVIC_regressionData_030507$region)
KVIC_regressionData_030507$kidsOut <- factor(KVIC_regressionData_030507$kidsOut)
KVIC_regressionData_030507$poorHTM <- factor(KVIC_regressionData_030507$poorHTM)
KVIC_regressionData_030507$richHTM <- factor(KVIC_regressionData_030507$richHTM)
KVIC_regressionData_030507$interestRate <- factor(KVIC_regressionData_030507$interestRate)

KVIC_regressionData_091113$year <- factor(KVIC_regressionData_091113$year)
KVIC_regressionData_091113$yob <- factor(KVIC_regressionData_091113$yob)
KVIC_regressionData_091113$educ <- factor(KVIC_regressionData_091113$educ)
KVIC_regressionData_091113$white <- factor(KVIC_regressionData_091113$white)
KVIC_regressionData_091113$black <- factor(KVIC_regressionData_091113$black)
KVIC_regressionData_091113$other <- factor(KVIC_regressionData_091113$other)
KVIC_regressionData_091113$famSize <- factor(KVIC_regressionData_091113$famSize)
KVIC_regressionData_091113$kids <- factor(KVIC_regressionData_091113$kids)
KVIC_regressionData_091113$employed <- factor(KVIC_regressionData_091113$employed)
KVIC_regressionData_091113$unemployed <- factor(KVIC_regressionData_091113$unemployed)
KVIC_regressionData_091113$retired <- factor(KVIC_regressionData_091113$retired)
KVIC_regressionData_091113$exIncome <- factor(KVIC_regressionData_091113$exIncome)
KVIC_regressionData_091113$region <- factor(KVIC_regressionData_091113$region)
KVIC_regressionData_091113$kidsOut <- factor(KVIC_regressionData_091113$kidsOut)
KVIC_regressionData_091113$poorHTM <- factor(KVIC_regressionData_091113$poorHTM)
KVIC_regressionData_091113$richHTM <- factor(KVIC_regressionData_091113$richHTM)
KVIC_regressionData_091113$interestRate <- factor(KVIC_regressionData_091113$interestRate)


# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVIC_regressionData_030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVIC_regressionData_091113,
                                     nest=TRUE)





# do the regressions
# we first regress log income and log impConsumption expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log impConsumption d(cit) and
#log income d(yit).

KVIC_income_030507.lm = svyglm(logy ~ year +
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
                                 totalWealth + 
                                 interestRate
                               ,familyPanelSurvey030507) 

KVIC_regressionData_030507$resIncome <-KVIC_income_030507.lm$residuals

KVIC_income_091113.lm = svyglm(logy ~ year +
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
                                 totalWealth + 
                                 interestRate
                               , familyPanelSurvey091113) 
KVIC_regressionData_091113$resIncome <-KVIC_income_091113.lm$residuals


KVIC_impConsumption_030507.lm = svyglm(logimpConsumption ~ year +
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
                                       totalWealth +
                                       interestRate
                                     , familyPanelSurvey030507) 
KVIC_regressionData_030507$resimpConsumption <- KVIC_impConsumption_030507.lm$residuals


KVIC_impConsumption_091113.lm = svyglm(logimpConsumption ~ year +
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
                                       totalWealth + 
                                       interestRate,
                                     familyPanelSurvey091113) 
KVIC_regressionData_091113$resimpConsumption <-KVIC_impConsumption_091113.lm$residuals


KVIC_covData_030507 = KVIC_regressionData_030507[ , c("uniqueID","quintile","year","resimpConsumption","resIncome")]
KVIC_covData_091113 = KVIC_regressionData_091113[ , c("uniqueID","quintile","year","resimpConsumption","resIncome")]

# deal with outliers - take off bottom 0.1%
resimpConsumption_q_030507 <- quantile(KVIC_covData_030507$resimpConsumption,seq(0,1,0.001),na.rm=TRUE)
resimpConsumption_q_091113 <- quantile(KVIC_covData_091113$resimpConsumption,seq(0,1,0.001),na.rm=TRUE)

KVIC_covData_030507 <- subset(KVIC_covData_030507,KVIC_covData_030507$resimpConsumption>resimpConsumption_q_030507[2] & KVIC_covData_030507$resimpConsumption<resimpConsumption_q_030507[1000])
KVIC_covData_091113 <- subset(KVIC_covData_091113,KVIC_covData_091113$resimpConsumption>resimpConsumption_q_091113[2] & KVIC_covData_091113$resimpConsumption<resimpConsumption_q_091113[1000])

resIncome_q_030507 <- quantile(KVIC_covData_030507$resIncome,seq(0,1,0.001),na.rm=TRUE)
resIncome_q_091113 <- quantile(KVIC_covData_091113$resIncome,seq(0,1,0.001),na.rm=TRUE)

KVIC_covData_030507 <- subset(KVIC_covData_030507,KVIC_covData_030507$resIncome>resIncome_q_030507[2] & KVIC_covData_030507$resIncome<resIncome_q_030507[1000])
KVIC_covData_091113 <- subset(KVIC_covData_091113,KVIC_covData_091113$resIncome>resIncome_q_091113[2] & KVIC_covData_091113$resIncome<resIncome_q_091113[1000])

#plot residuals
plot(KVIC_covData_030507$resIncome,KVIC_covData_030507$resimpConsumption)
plot(KVIC_covData_091113$resIncome,KVIC_covData_091113$resimpConsumption)

KVIC_covData_030507 <- reshape(KVIC_covData_030507,idvar="uniqueID",direction="wide",v.names=c("quintile","resimpConsumption","resIncome"),timevar="year")
KVIC_covData_091113 <- reshape(KVIC_covData_091113,idvar="uniqueID",direction="wide",v.names=c("quintile","resimpConsumption","resIncome"),timevar="year")

KVIC_covData_030507$dst <- KVIC_covData_030507$resimpConsumption.2005 - KVIC_covData_030507$resimpConsumption.2003
KVIC_covData_030507$dyt <- KVIC_covData_030507$resIncome.2005 - KVIC_covData_030507$resIncome.2003
KVIC_covData_030507$dytplus1 <- KVIC_covData_030507$resIncome.2007 - KVIC_covData_030507$resIncome.2005


KVIC_covData_091113$dst <- KVIC_covData_091113$resimpConsumption.2011 - KVIC_covData_091113$resimpConsumption.2009
KVIC_covData_091113$dyt <- KVIC_covData_091113$resIncome.2011 - KVIC_covData_091113$resIncome.2009
KVIC_covData_091113$dytplus1 <- KVIC_covData_091113$resIncome.2013 - KVIC_covData_091113$resIncome.2011




# MPIC
print("MPIC as 07")
for (i in c(1,2,3,4,5)){
  MPIC_mdl_030507 <- ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVIC_covData_030507, subset=quintile.2003==i)
  assign(paste0("MPIC_mdl_030507_q",i),
         MPIC_mdl_030507)
  assign(paste0("MPIC_030507_q",i),
         MPIC_mdl_030507$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPIC_mdl_030507)
  assign(paste0("MPIC_CI_low_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPIC_CI_up_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPIC_CI_low_07_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPIC_030507_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPIC_CI_up_07_q",i))))))
}

# MPIC
print("MPIC as 13")
for (i in c(1,2,3,4,5)){
  MPIC_mdl_091113 = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVIC_covData_091113,subset=quintile.2009==i)
  assign(paste0("MPIC_mdl_091113_q",i),
         MPIC_mdl_091113)
  assign(paste0("MPIC_091113_q",i),
         MPIC_mdl_091113$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPIC_mdl_091113)
  assign(paste0("MPIC_CI_low_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPIC_CI_up_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPIC_CI_low_13_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPIC_091113_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPIC_CI_up_13_q",i))))))
}

out_MPIC <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                       "year" = c("2007","2007","2007","2007","2007","2013","2013","2013","2013","2013"),
                       "MPIC" = c(MPIC_030507_q1,
                                  MPIC_030507_q2,
                                  MPIC_030507_q3,
                                  MPIC_030507_q4,
                                  MPIC_030507_q5,
                                  MPIC_091113_q1,
                                  MPIC_091113_q2,
                                  MPIC_091113_q3,
                                  MPIC_091113_q4,
                                  MPIC_091113_q5),
                       "MPIC_CI_lower" = c(MPIC_CI_low_07_q1,
                                           MPIC_CI_low_07_q2,
                                           MPIC_CI_low_07_q3,
                                           MPIC_CI_low_07_q4,
                                           MPIC_CI_low_07_q5,
                                           MPIC_CI_low_13_q1,
                                           MPIC_CI_low_13_q2,
                                           MPIC_CI_low_13_q3,
                                           MPIC_CI_low_13_q4,
                                           MPIC_CI_low_13_q5),
                       "MPIC_CI_upper" = c(MPIC_CI_up_07_q1,
                                           MPIC_CI_up_07_q2,
                                           MPIC_CI_up_07_q3,
                                           MPIC_CI_up_07_q4,
                                           MPIC_CI_up_07_q5,
                                           MPIC_CI_up_13_q1,
                                           MPIC_CI_up_13_q2,
                                           MPIC_CI_up_13_q3,
                                           MPIC_CI_up_13_q4,
                                           MPIC_CI_up_13_q5))
pdf(file=paste0(getwd(),"/Results/MPIC.pdf"))
ggplot(data = out_MPIC, aes(x = quantile, y = MPIC, group=year, color=year)) +
  geom_line() + geom_point()+
  geom_ribbon(data= out_MPIC,aes(ymin= MPIC_CI_lower,ymax= MPIC_CI_upper),alpha=0.3) +
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("MPIC") +
  labs(color="Year")
dev.off()
ggplot(data = out_MPIC, aes(x = quantile, y = MPIC, group=year, color=year)) +
  geom_line() + geom_point()+
  geom_ribbon(data= out_MPIC,aes(ymin= MPIC_CI_lower,ymax= MPIC_CI_upper),alpha=0.3) +
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("MPIC") +
  labs(color="Year")

# whole sample MPIC
MPIC_mdl_030507_whole = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVIC_covData_030507)
MPIC_mdl_030507_whole$coefficients[2]
anderson.rubin.ci(MPIC_mdl_030507_whole)
# write to text files
# coefficient
write(toString(round(MPIC_mdl_030507_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPIC_030507.txt"))
#standard error
write(toString(round(coef(summary(MPIC_mdl_030507_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPIC_030507_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPIC_mdl_030507_whole))[2,4])),file=paste0(getwd(),"/Results/MPIC_030507_stars.txt"))
#N
write(toString(MPIC_mdl_030507_whole$nobs),file=paste0(getwd(),"/Results/MPIC_030507_N.txt"))


MPIC_mdl_091113_whole = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVIC_covData_091113)
MPIC_mdl_091113_whole$coefficients[2]
anderson.rubin.ci(MPIC_mdl_091113_whole)
# write to text files
# coefficient
write(toString(round(MPIC_mdl_091113_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPIC_091113.txt"))
#standard error
write(toString(round(coef(summary(MPIC_mdl_091113_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPIC_091113_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPIC_mdl_091113_whole))[2,4])),file=paste0(getwd(),"/Results/MPIC_091113_stars.txt"))
#N
write(toString(MPIC_mdl_091113_whole$nobs),file=paste0(getwd(),"/Results/MPIC_091113_N.txt"))



rm(i,temp)#,KVIC_covData_030507, KVIC_covData_091113)
rm(list=ls(pattern="quintiles_"))
rm(list=ls(pattern="quantiles_"))
rm(list=ls(pattern="familyPanelSurvey"))


