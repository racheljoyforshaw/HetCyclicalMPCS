# Kaplan Violante - impSaving - IV, weighted
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2009,11,13

f030507_KVIS <- f030507
f091113_KVIS <- f091113


##### CREATE VARIABLES ########
# create year of birth

# create year variable
f030507_KVIS$year03 <- 2003
f030507_KVIS$year05 <- 2005
f030507_KVIS$year07 <- 2007

f091113_KVIS$year09 <- 2009
f091113_KVIS$year11 <- 2011
f091113_KVIS$year13 <- 2013


# log impSaving - move into positive
min_all <- min(min(f030507_KVIS$impSaving03,na.rm=TRUE),min(f030507_KVIS$impSaving05,na.rm=TRUE),min(f030507_KVIS$impSaving07,na.rm=TRUE),
               min(f091113_KVIS$impSaving09,na.rm=TRUE),min(f091113_KVIS$impSaving11,na.rm=TRUE),min(f091113_KVIS$impSaving13,na.rm=TRUE),
               min(f030507_KVIS$income03,na.rm=TRUE),min(f030507_KVIS$income05,na.rm=TRUE),min(f030507_KVIS$income07,na.rm=TRUE),
               min(f091113_KVIS$income09,na.rm=TRUE),min(f091113_KVIS$income11,na.rm=TRUE),min(f091113_KVIS$income13,na.rm=TRUE))

f030507_KVIS$logimpSaving03 <- log(f030507_KVIS$impSaving03 + abs(min_all) +1)
f030507_KVIS$logimpSaving05 <- log(f030507_KVIS$impSaving05+ abs(min_all) +1)
f030507_KVIS$logimpSaving07 <- log(f030507_KVIS$impSaving07+ abs(min_all) +1)

f091113_KVIS$logimpSaving09 <- log(f091113_KVIS$impSaving09+ abs(min_all) +1)
f091113_KVIS$logimpSaving11 <- log(f091113_KVIS$impSaving11+ abs(min_all) +1)
f091113_KVIS$logimpSaving13 <- log(f091113_KVIS$impSaving13+ abs(min_all) +1)


# log income - move into positive

f030507_KVIS$logy03 <- log(f030507_KVIS$income03 + abs(min_all) +1)
f030507_KVIS$logy05 <- log(f030507_KVIS$income05+ abs(min_all) +1)
f030507_KVIS$logy07 <- log(f030507_KVIS$income07+ abs(min_all) +1)

f091113_KVIS$logy09 <- log(f091113_KVIS$income09+ abs(min_all) +1)
f091113_KVIS$logy11 <- log(f091113_KVIS$income11+ abs(min_all) +1)
f091113_KVIS$logy13 <- log(f091113_KVIS$income13+ abs(min_all) +1)
rm(min_all)




f030507_KVIS_keeps <- c("uniqueID","primarySamplingUnit","stratification","black03","black05","black07",
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
                       "logimpSaving03","logimpSaving05","logimpSaving07",
                       "age03","age05","age07",
                       "income03","income05","income07",
                       "poorHTM03","poorHTM05","poorHTM07",
                       "richHTM03","richHTM05","richHTM07",
                       "totalWealth03","totalWealth05","totalWealth07",
                       "interestRate03","interestRate05","interestRate07")

f030507_KVIS <- f030507_KVIS[,f030507_KVIS_keeps]

f091113_KVIS_keeps <- c("uniqueID","primarySamplingUnit","stratification","black09","black11","black13",
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
                       "logimpSaving09","logimpSaving11","logimpSaving13",
                       "age09","age11","age13",
                       "income09","income11","income13",
                       "poorHTM09","poorHTM11","poorHTM13",
                       "richHTM09","richHTM11","richHTM13",
                       "totalWealth09","totalWealth11","totalWealth13",
                       "interestRate09","interestRate11","interestRate13")

f091113_KVIS <- f091113_KVIS[,f091113_KVIS_keeps]

rm(f091113_KVIS_keeps,f030507_KVIS_keeps)

# drop NA 
f030507_KVIS <- na.omit(f030507_KVIS)
f091113_KVIS <- na.omit(f091113_KVIS)


# quintiles

# survey design

# familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f030507_KVIS,
#                                      nest=TRUE)
# 
# familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f091113_KVIS,
#                                      nest=TRUE)

quintiles_03 <- quantile(f030507_KVIS$income03, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_05 <- quantile(f030507_KVIS$income05, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_07 <- quantile(f030507_KVIS$income07, seq(0, 1, 0.2),NA.rm=TRUE)

quintiles_09 <- quantile(f091113_KVIS$income09, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_11 <- quantile(f091113_KVIS$income11, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_13 <- quantile(f091113_KVIS$income13, seq(0, 1, 0.2),NA.rm=TRUE)


f030507_KVIS$quintile03 <- ifelse(f030507_KVIS$income03<quintiles_03[2],1,
                                 ifelse(f030507_KVIS$income03>=quintiles_03[2] & f030507_KVIS$income03<quintiles_03[3],2,
                                        ifelse(f030507_KVIS$income03>=quintiles_03[3] & f030507_KVIS$income03<quintiles_03[4],3,
                                               ifelse(f030507_KVIS$income03>=quintiles_03[4]& f030507_KVIS$income03<quintiles_03[5],4,
                                                      ifelse(f030507_KVIS$income03>=quintiles_03[5],5,NA)))))
f030507_KVIS$quintile05 <- ifelse(f030507_KVIS$income05<quintiles_05[2],1,
                                 ifelse(f030507_KVIS$income05>=quintiles_05[2] & f030507_KVIS$income05<quintiles_05[3],2,
                                        ifelse(f030507_KVIS$income05>=quintiles_05[3] & f030507_KVIS$income05<quintiles_05[4],3,
                                               ifelse(f030507_KVIS$income05>=quintiles_05[4]& f030507_KVIS$income05<quintiles_05[5],4,
                                                      ifelse(f030507_KVIS$income05>=quintiles_05[5],5,NA)))))
f030507_KVIS$quintile07 <- ifelse(f030507_KVIS$income07<quintiles_07[2],1,
                                 ifelse(f030507_KVIS$income07>=quintiles_07[2] & f030507_KVIS$income07<quintiles_07[3],2,
                                        ifelse(f030507_KVIS$income07>=quintiles_07[3] & f030507_KVIS$income07<quintiles_07[4],3,
                                               ifelse(f030507_KVIS$income07>=quintiles_07[4]& f030507_KVIS$income07<quintiles_07[5],4,
                                                      ifelse(f030507_KVIS$income07>=quintiles_07[5],5,NA)))))
f091113_KVIS$quintile09 <- ifelse(f091113_KVIS$income09<quintiles_09[2],1,
                                 ifelse(f091113_KVIS$income09>=quintiles_09[2] & f091113_KVIS$income09<quintiles_09[3],2,
                                        ifelse(f091113_KVIS$income09>=quintiles_09[3] & f091113_KVIS$income09<quintiles_09[4],3,
                                               ifelse(f091113_KVIS$income09>=quintiles_09[4]& f091113_KVIS$income09<quintiles_09[5],4,
                                                      ifelse(f091113_KVIS$income09>=quintiles_09[5],5,NA)))))
f091113_KVIS$quintile11 <- ifelse(f091113_KVIS$income11<quintiles_11[2],1,
                                 ifelse(f091113_KVIS$income11>=quintiles_11[2] & f091113_KVIS$income11<quintiles_11[3],2,
                                        ifelse(f091113_KVIS$income11>=quintiles_11[3] & f091113_KVIS$income11<quintiles_11[4],3,
                                               ifelse(f091113_KVIS$income11>=quintiles_11[4]& f091113_KVIS$income11<quintiles_11[5],4,
                                                      ifelse(f091113_KVIS$income11>=quintiles_11[5],5,NA)))))
f091113_KVIS$quintile13 <- ifelse(f091113_KVIS$income13<quintiles_13[2],1,
                                 ifelse(f091113_KVIS$income13>=quintiles_13[2] & f091113_KVIS$income13<quintiles_13[3],2,
                                        ifelse(f091113_KVIS$income13>=quintiles_13[3] & f091113_KVIS$income13<quintiles_13[4],3,
                                               ifelse(f091113_KVIS$income13>=quintiles_13[4]& f091113_KVIS$income13<quintiles_13[5],4,
                                                      ifelse(f091113_KVIS$income13>=quintiles_13[5],5,NA)))))



# familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f030507_KVIS,
#                                      nest=TRUE)
# 
# familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f091113_KVIS,
#                                      nest=TRUE)


# drop income

f030507_KVIS <- select(f030507_KVIS,-c("income03","income05","income07"))
f091113_KVIS <- select(f091113_KVIS,-c("income09","income11","income13"))


# wide to long

KVIS_regressionData_030507 <- reshape(f030507_KVIS, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                     varying=list(black=c(grep("black", colnames(f030507_KVIS))),
                                                  educ=c(grep("educ", colnames(f030507_KVIS))),
                                                  employed=c(grep("^employed", colnames(f030507_KVIS))),
                                                  exIncome=c(grep("exIncome", colnames(f030507_KVIS))),
                                                  famSize=c(grep("famSize", colnames(f030507_KVIS))),
                                                  kids=c(grep("kids[^Out]", colnames(f030507_KVIS))),
                                                  kidsOut=c(grep("kidsOut", colnames(f030507_KVIS))),
                                                  longWeight=c(grep("longWeight", colnames(f030507_KVIS))),
                                                  other=c(grep("other", colnames(f030507_KVIS))), 
                                                  logy=c(grep("logy",colnames(f030507_KVIS))),
                                                  region=c(grep("region", colnames(f030507_KVIS))),
                                                  retired=c(grep("retired",colnames(f030507_KVIS))),
                                                  unemployed=c(grep("unemployed", colnames(f030507_KVIS))),
                                                  white=c(grep("white", colnames(f030507_KVIS))),
                                                  year=c(grep("year", colnames(f030507_KVIS))),
                                                  logimpSaving=c(grep("logimpSaving",colnames(f030507_KVIS))),
                                                  age=c(grep("age",colnames(f030507_KVIS))),
                                                  poorHTM=c(grep("poorHTM",colnames(f030507_KVIS))),
                                                  richHTM=c(grep("richHTM",colnames(f030507_KVIS))),
                                                  quintile=c(grep("quintile",colnames(f030507_KVIS))),
                                                  totalWealth=c(grep("totalWealth",colnames(f030507_KVIS))),
                                                  interestRate=c(grep("interestRate",colnames(f030507_KVIS)))),
                                     v.names = c("black","educ","employed",
                                                 "exIncome","famSize","kids","kidsOut","longWeight",
                                                 "other","logy","region","retired","unemployed","white","year","logimpSaving","age","poorHTM","richHTM","quintile","totalWealth","interestRate"), #,"bigCity"),
                                     times=c("03", "05","07"))


KVIS_regressionData_091113 <- reshape(f091113_KVIS, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                     varying=list(black=c(grep("black", colnames(f091113_KVIS))),
                                                  educ=c(grep("educ", colnames(f091113_KVIS))),
                                                  employed=c(grep("^employed", colnames(f091113_KVIS))),
                                                  exIncome=c(grep("exIncome", colnames(f091113_KVIS))),
                                                  famSize=c(grep("famSize", colnames(f091113_KVIS))),
                                                  kids=c(grep("kids[^Out]", colnames(f091113_KVIS))),
                                                  kidsOut=c(grep("kidsOut", colnames(f091113_KVIS))),
                                                  longWeight=c(grep("longWeight", colnames(f091113_KVIS))),
                                                  other=c(grep("other", colnames(f091113_KVIS))), 
                                                  logy=c(grep("logy",colnames(f091113_KVIS))),
                                                  region=c(grep("region", colnames(f091113_KVIS))),
                                                  retired=c(grep("retired",colnames(f091113_KVIS))),
                                                  unemployed=c(grep("unemployed", colnames(f091113_KVIS))),
                                                  white=c(grep("white", colnames(f091113_KVIS))),
                                                  year=c(grep("year", colnames(f091113_KVIS))),
                                                  logimpSaving=c(grep("logimpSaving",colnames(f091113_KVIS))),
                                                  age=c(grep("age",colnames(f091113_KVIS))),
                                                  poorHTM=c(grep("poorHTM",colnames(f091113_KVIS))),
                                                  richHTM=c(grep("richHTM",colnames(f091113_KVIS))),
                                                  quintile=c(grep("quintile",colnames(f091113_KVIS))),
                                                  totalWealth=c(grep("totalWealth",colnames(f091113_KVIS))),
                                                  interestRate=c(grep("interestRate",colnames(f091113_KVIS)))),
                                     v.names = c("black","educ","employed",
                                                 "exIncome","famSize","kids","kidsOut","longWeight",
                                                 "other","logy","region","retired","unemployed","white","year","logimpSaving","age","poorHTM","richHTM","quintile","totalWealth","interestRate"), #,"bigCity"),
                                     times=c("09", "11","13"))

# create year of birth variable
KVIS_regressionData_030507$yob <- KVIS_regressionData_030507$year - KVIS_regressionData_030507$age
KVIS_regressionData_091113$yob <- KVIS_regressionData_091113$year - KVIS_regressionData_091113$age

# create factors for categorical variables
KVIS_regressionData_030507$educ <- factor(KVIS_regressionData_030507$educ)
KVIS_regressionData_030507$white <- factor(KVIS_regressionData_030507$white)
KVIS_regressionData_030507$black <- factor(KVIS_regressionData_030507$black)
KVIS_regressionData_030507$other <- factor(KVIS_regressionData_030507$other)
KVIS_regressionData_030507$employed <- factor(KVIS_regressionData_030507$employed)
KVIS_regressionData_030507$unemployed <- factor(KVIS_regressionData_030507$unemployed)
KVIS_regressionData_030507$retired <- factor(KVIS_regressionData_030507$retired)
KVIS_regressionData_030507$exIncome <- factor(KVIS_regressionData_030507$exIncome)
KVIS_regressionData_030507$region <- factor(KVIS_regressionData_030507$region)
KVIS_regressionData_030507$kidsOut <- factor(KVIS_regressionData_030507$kidsOut)
KVIS_regressionData_030507$poorHTM <- factor(KVIS_regressionData_030507$poorHTM)
KVIS_regressionData_030507$richHTM <- factor(KVIS_regressionData_030507$richHTM)


KVIS_regressionData_091113$educ <- factor(KVIS_regressionData_091113$educ)
KVIS_regressionData_091113$white <- factor(KVIS_regressionData_091113$white)
KVIS_regressionData_091113$black <- factor(KVIS_regressionData_091113$black)
KVIS_regressionData_091113$other <- factor(KVIS_regressionData_091113$other)
KVIS_regressionData_091113$employed <- factor(KVIS_regressionData_091113$employed)
KVIS_regressionData_091113$unemployed <- factor(KVIS_regressionData_091113$unemployed)
KVIS_regressionData_091113$retired <- factor(KVIS_regressionData_091113$retired)
KVIS_regressionData_091113$exIncome <- factor(KVIS_regressionData_091113$exIncome)
KVIS_regressionData_091113$region <- factor(KVIS_regressionData_091113$region)
KVIS_regressionData_091113$kidsOut <- factor(KVIS_regressionData_091113$kidsOut)
KVIS_regressionData_091113$poorHTM <- factor(KVIS_regressionData_091113$poorHTM)
KVIS_regressionData_091113$richHTM <- factor(KVIS_regressionData_091113$richHTM)



# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVIS_regressionData_030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVIS_regressionData_091113,
                                     nest=TRUE)





# do the regressions
# we first regress log income and log impSaving expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log impSaving d(cit) and
#log income d(yit).

KVIS_income_030507.lm = svyglm(logy ~ year +
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

KVIS_regressionData_030507$resIncome <-KVIS_income_030507.lm$residuals

KVIS_income_091113.lm = svyglm(logy ~ year +
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
KVIS_regressionData_091113$resIncome <-KVIS_income_091113.lm$residuals


KVIS_impSaving_030507.lm = svyglm(logimpSaving ~ year +
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
KVIS_regressionData_030507$resimpSaving <- KVIS_impSaving_030507.lm$residuals


KVIS_impSaving_091113.lm = svyglm(logimpSaving ~ year +
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
KVIS_regressionData_091113$resimpSaving <-KVIS_impSaving_091113.lm$residuals


KVIS_covData_030507 = KVIS_regressionData_030507[ , c("uniqueID","quintile","year","resimpSaving","resIncome")]
KVIS_covData_091113 = KVIS_regressionData_091113[ , c("uniqueID","quintile","year","resimpSaving","resIncome")]

# deal with outliers - take off bottom 0.1%
resimpSaving_q_030507 <- quantile(KVIS_covData_030507$resimpSaving,seq(0,1,0.001),na.rm=TRUE)
resimpSaving_q_091113 <- quantile(KVIS_covData_091113$resimpSaving,seq(0,1,0.001),na.rm=TRUE)

KVIS_covData_030507 <- subset(KVIS_covData_030507,KVIS_covData_030507$resimpSaving>resimpSaving_q_030507[2] & KVIS_covData_030507$resimpSaving<resimpSaving_q_030507[1000])
KVIS_covData_091113 <- subset(KVIS_covData_091113,KVIS_covData_091113$resimpSaving>resimpSaving_q_091113[2] & KVIS_covData_091113$resimpSaving<resimpSaving_q_091113[1000])

resIncome_q_030507 <- quantile(KVIS_covData_030507$resIncome,seq(0,1,0.001),na.rm=TRUE)
resIncome_q_091113 <- quantile(KVIS_covData_091113$resIncome,seq(0,1,0.001),na.rm=TRUE)

KVIS_covData_030507 <- subset(KVIS_covData_030507,KVIS_covData_030507$resIncome>resIncome_q_030507[2] & KVIS_covData_030507$resIncome<resIncome_q_030507[1000])
KVIS_covData_091113 <- subset(KVIS_covData_091113,KVIS_covData_091113$resIncome>resIncome_q_091113[2] & KVIS_covData_091113$resIncome<resIncome_q_091113[1000])

#plot residuals
plot(KVIS_covData_030507$resIncome,KVIS_covData_030507$resimpSaving)
plot(KVIS_covData_091113$resIncome,KVIS_covData_091113$resimpSaving)

KVIS_covData_030507 <- reshape(KVIS_covData_030507,idvar="uniqueID",direction="wide",v.names=c("quintile","resimpSaving","resIncome"),timevar="year")
KVIS_covData_091113 <- reshape(KVIS_covData_091113,idvar="uniqueID",direction="wide",v.names=c("quintile","resimpSaving","resIncome"),timevar="year")

KVIS_covData_030507$dst <- KVIS_covData_030507$resimpSaving.2005 - KVIS_covData_030507$resimpSaving.2003
KVIS_covData_030507$dyt <- KVIS_covData_030507$resIncome.2005 - KVIS_covData_030507$resIncome.2003
KVIS_covData_030507$dytplus1 <- KVIS_covData_030507$resIncome.2007 - KVIS_covData_030507$resIncome.2005


KVIS_covData_091113$dst <- KVIS_covData_091113$resimpSaving.2011 - KVIS_covData_091113$resimpSaving.2009
KVIS_covData_091113$dyt <- KVIS_covData_091113$resIncome.2011 - KVIS_covData_091113$resIncome.2009
KVIS_covData_091113$dytplus1 <- KVIS_covData_091113$resIncome.2013 - KVIS_covData_091113$resIncome.2011




# MPIS
print("MPIS as 07")
for (i in c(1,2,3,4,5)){
  MPIS_mdl_030507 <- ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVIS_covData_030507, subset=quintile.2003==i)
  assign(paste0("MPIS_mdl_030507_q",i),
         MPIS_mdl_030507)
  assign(paste0("MPIS_030507_q",i),
         MPIS_mdl_030507$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPIS_mdl_030507)
  assign(paste0("MPIS_CI_low_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPIS_CI_up_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPIS_CI_low_07_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPIS_030507_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPIS_CI_up_07_q",i))))))
}

# MPIS
print("MPIS as 13")
for (i in c(1,2,3,4,5)){
  MPIS_mdl_091113 = ivreg(dst ~  dyt, ~ dytplus1,x=TRUE, data=KVIS_covData_091113,subset=quintile.2009==i)
  assign(paste0("MPIS_mdl_091113_q",i),
         MPIS_mdl_091113)
  assign(paste0("MPIS_091113_q",i),
         MPIS_mdl_091113$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPIS_mdl_091113)
  assign(paste0("MPIS_CI_low_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPIS_CI_up_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPIS_CI_low_13_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPIS_091113_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPIS_CI_up_13_q",i))))))
}

out_MPIS <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                      "year" = c("2007","2007","2007","2007","2007","2013","2013","2013","2013","2013"),
                      "MPIS" = c(MPIS_030507_q1,
                                MPIS_030507_q2,
                                MPIS_030507_q3,
                                MPIS_030507_q4,
                                MPIS_030507_q5,
                                MPIS_091113_q1,
                                MPIS_091113_q2,
                                MPIS_091113_q3,
                                MPIS_091113_q4,
                                MPIS_091113_q5),
                      "MPIS_CI_lower" = c(MPIS_CI_low_07_q1,
                                         MPIS_CI_low_07_q2,
                                         MPIS_CI_low_07_q3,
                                         MPIS_CI_low_07_q4,
                                         MPIS_CI_low_07_q5,
                                         MPIS_CI_low_13_q1,
                                         MPIS_CI_low_13_q2,
                                         MPIS_CI_low_13_q3,
                                         MPIS_CI_low_13_q4,
                                         MPIS_CI_low_13_q5),
                      "MPIS_CI_upper" = c(MPIS_CI_up_07_q1,
                                         MPIS_CI_up_07_q2,
                                         MPIS_CI_up_07_q3,
                                         MPIS_CI_up_07_q4,
                                         MPIS_CI_up_07_q5,
                                         MPIS_CI_up_13_q1,
                                         MPIS_CI_up_13_q2,
                                         MPIS_CI_up_13_q3,
                                         MPIS_CI_up_13_q4,
                                         MPIS_CI_up_13_q5))
pdf(file=paste0(getwd(),"/Results/MPIS.pdf"))
ggplot(data = out_MPIS, aes(x = quantile, y = MPIS, group=year, color=year)) +
  geom_line() + geom_point()+
  scale_color_grey() +
  geom_ribbon(data= out_MPIS,aes(ymin= MPIS_CI_lower,ymax= MPIS_CI_upper),alpha=0.3) +
  xlab("Income Quintile") +
  ylab("MPIS") +
  labs(color="Year") +
  theme_pubr()
dev.off()
ggplot(data = out_MPIS, aes(x = quantile, y = MPIS, group=year, color=year)) +
  geom_line() + geom_point()+
  scale_color_grey() +
  geom_ribbon(data= out_MPIS,aes(ymin= MPIS_CI_lower,ymax= MPIS_CI_upper),alpha=0.3) +
  xlab("Income Quintile") +
  ylab("MPIS") +
  labs(color="Year") +
  theme_pubr()

# whole sample MPIS
MPIS_mdl_030507_whole = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVIS_covData_030507)
MPIS_mdl_030507_whole$coefficients[2]
anderson.rubin.ci(MPIS_mdl_030507_whole)
# write to text files
# coefficient
write(toString(round(MPIS_mdl_030507_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPIS_030507.txt"))
#standard error
write(toString(round(coef(summary(MPIS_mdl_030507_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPIS_030507_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPIS_mdl_030507_whole))[2,4])),file=paste0(getwd(),"/Results/MPIS_030507_stars.txt"))
#N
write(toString(MPIS_mdl_030507_whole$nobs),file=paste0(getwd(),"/Results/MPIS_030507_N.txt"))


MPIS_mdl_091113_whole = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVIS_covData_091113)
MPIS_mdl_091113_whole$coefficients[2]
anderson.rubin.ci(MPIS_mdl_091113_whole)
# write to text files
# coefficient
write(toString(round(MPIS_mdl_091113_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPIS_091113.txt"))
#standard error
write(toString(round(coef(summary(MPIS_mdl_091113_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPIS_091113_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPIS_mdl_091113_whole))[2,4])),file=paste0(getwd(),"/Results/MPIS_091113_stars.txt"))
#N
write(toString(MPIS_mdl_091113_whole$nobs),file=paste0(getwd(),"/Results/MPIS_091113_N.txt"))



rm(i,temp)#,KVIS_covData_030507, KVIS_covData_091113)
rm(list=ls(pattern="quintiles_"))
rm(list=ls(pattern="quantiles_"))
rm(list=ls(pattern="familyPanelSurvey"))


KVIS_covData_030507_PE <- KVIS_covData_030507[,c("uniqueID","dst","dyt","dytplus1")]
KVIS_covData_091113_PE <- KVIS_covData_091113[,c("uniqueID","dst","dyt","dytplus1")]

KVIS_covData_030507_PE <- rename(KVIS_covData_030507_PE,"dst_030507" = "dst","dyt_030507" = "dyt","dytplus1_030507" = "dytplus1")
KVIS_covData_091113_PE <- rename(KVIS_covData_091113_PE,"dst_091113" = "dst","dyt_091113" = "dyt","dytplus1_091113" = "dytplus1")
KVIS_covData_all_PE <- merge(KVIS_covData_091113_PE,KVIS_covData_030507_PE,by="uniqueID",all=TRUE)

eqn_030507 <- dst_030507 ~ -1 + dyt_030507
eqn_091113 <- dst_091113 ~ -1 + dyt_091113
system <- list(eqn_030507,eqn_091113)
inst1 <- ~ dytplus1_030507
inst2 <- ~ dytplus1_091113
instlist <- list( inst1, inst2 )

fit2sls2 <- systemfit( system, "2SLS", inst = instlist, data = KVIS_covData_all_PE )
print(fit2sls2)
linearHypothesis(fit2sls2,"eq1_dyt_030507=eq2_dyt_091113")


# to latex

print(xtable(KVIS_impSaving_030507.lm, type = "latex"),file=paste0(getwd(),"/Results/KVIS_impSaving_030507.tex"),floating=FALSE)
print(xtable(KVIS_impSaving_091113.lm, type = "latex"),file=paste0(getwd(),"/Results/KVIS_impSaving_091113.tex"),floating=FALSE)
print(xtable(KVIS_income_030507.lm, type = "latex"),file=paste0(getwd(),"/Results/KVIS_income_030507.tex"),floating=FALSE)
print(xtable(KVIS_income_091113.lm, type = "latex"),file=paste0(getwd(),"/Results/KVIS_income_091113.tex"),floating=FALSE)

writeLines(capture.output(stargazer(MPIS_mdl_030507_q1, MPIS_mdl_030507_q2,MPIS_mdl_030507_q3, MPIS_mdl_030507_q4,MPIS_mdl_030507_q5,
                                    float=FALSE, align=TRUE)),paste0(getwd(),"/Results/MPIS_mdl_030507.tex"))
writeLines(capture.output(stargazer(MPIS_mdl_091113_q1, MPIS_mdl_091113_q2,MPIS_mdl_091113_q3, MPIS_mdl_091113_q4,MPIS_mdl_091113_q5,
                                    float=FALSE, align=TRUE)),paste0(getwd(),"/Results/MPIS_mdl_091113.tex"))

writeLines(capture.output(stargazer(MPIS_mdl_030507_whole, MPIS_mdl_091113_whole,
                                    float=FALSE, align=TRUE)),paste0(getwd(),"/Results/MPIS_mdl_whole.tex"))
