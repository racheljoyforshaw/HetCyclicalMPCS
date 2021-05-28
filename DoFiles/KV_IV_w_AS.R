# Kaplan Violante - activeSaving - IV, weighted
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2009,11,13

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


# log activeSaving - move into positive
min_all <- min(min(f030507_KVAS$activeSaving03,na.rm=TRUE),min(f030507_KVAS$activeSaving05,na.rm=TRUE),min(f030507_KVAS$activeSaving07,na.rm=TRUE),
               min(f091113_KVAS$activeSaving09,na.rm=TRUE),min(f091113_KVAS$activeSaving11,na.rm=TRUE),min(f091113_KVAS$activeSaving13,na.rm=TRUE),
               min(f030507_KVAS$income03,na.rm=TRUE),min(f030507_KVAS$income05,na.rm=TRUE),min(f030507_KVAS$income07,na.rm=TRUE),
               min(f091113_KVAS$income09,na.rm=TRUE),min(f091113_KVAS$income11,na.rm=TRUE),min(f091113_KVAS$income13,na.rm=TRUE))

f030507_KVAS$logactiveSaving03 <- log(f030507_KVAS$activeSaving03 + abs(min_all) +1)
f030507_KVAS$logactiveSaving05 <- log(f030507_KVAS$activeSaving05+ abs(min_all) +1)
f030507_KVAS$logactiveSaving07 <- log(f030507_KVAS$activeSaving07+ abs(min_all) +1)

f091113_KVAS$logactiveSaving09 <- log(f091113_KVAS$activeSaving09+ abs(min_all) +1)
f091113_KVAS$logactiveSaving11 <- log(f091113_KVAS$activeSaving11+ abs(min_all) +1)
f091113_KVAS$logactiveSaving13 <- log(f091113_KVAS$activeSaving13+ abs(min_all) +1)


# log income - move into positive

f030507_KVAS$logy03 <- log(f030507_KVAS$income03 + abs(min_all) +1)
f030507_KVAS$logy05 <- log(f030507_KVAS$income05+ abs(min_all) +1)
f030507_KVAS$logy07 <- log(f030507_KVAS$income07+ abs(min_all) +1)

f091113_KVAS$logy09 <- log(f091113_KVAS$income09+ abs(min_all) +1)
f091113_KVAS$logy11 <- log(f091113_KVAS$income11+ abs(min_all) +1)
f091113_KVAS$logy13 <- log(f091113_KVAS$income13+ abs(min_all) +1)
rm(min_all)




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
                       "logactiveSaving03","logactiveSaving05","logactiveSaving07",
                       "age03","age05","age07",
                       "income03","income05","income07",
                       "poorHTM03","poorHTM05","poorHTM07",
                       "richHTM03","richHTM05","richHTM07",
                       "totalWealth03","totalWealth05","totalWealth07",
                       "interestRate03","interestRate05","interestRate07")

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
                       "logactiveSaving09","logactiveSaving11","logactiveSaving13",
                       "age09","age11","age13",
                       "income09","income11","income13",
                       "poorHTM09","poorHTM11","poorHTM13",
                       "richHTM09","richHTM11","richHTM13",
                       "totalWealth09","totalWealth11","totalWealth13",
                       "interestRate09","interestRate11","interestRate13")

f091113_KVAS <- f091113_KVAS[,f091113_KVAS_keeps]

rm(f091113_KVAS_keeps,f030507_KVAS_keeps)

# drop NA 
f030507_KVAS <- na.omit(f030507_KVAS)
f091113_KVAS <- na.omit(f091113_KVAS)


# quintiles

# survey design

# familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f030507_KVAS,
#                                      nest=TRUE)
# 
# familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f091113_KVAS,
#                                      nest=TRUE)

quintiles_03 <- quantile(f030507_KVAS$income03, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_05 <- quantile(f030507_KVAS$income05, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_07 <- quantile(f030507_KVAS$income07, seq(0, 1, 0.2),NA.rm=TRUE)

quintiles_09 <- quantile(f091113_KVAS$income09, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_11 <- quantile(f091113_KVAS$income11, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_13 <- quantile(f091113_KVAS$income13, seq(0, 1, 0.2),NA.rm=TRUE)


f030507_KVAS$quintile03 <- ifelse(f030507_KVAS$income03<quintiles_03[2],1,
                                 ifelse(f030507_KVAS$income03>=quintiles_03[2] & f030507_KVAS$income03<quintiles_03[3],2,
                                        ifelse(f030507_KVAS$income03>=quintiles_03[3] & f030507_KVAS$income03<quintiles_03[4],3,
                                               ifelse(f030507_KVAS$income03>=quintiles_03[4]& f030507_KVAS$income03<quintiles_03[5],4,
                                                      ifelse(f030507_KVAS$income03>=quintiles_03[5],5,NA)))))
f030507_KVAS$quintile05 <- ifelse(f030507_KVAS$income05<quintiles_05[2],1,
                                 ifelse(f030507_KVAS$income05>=quintiles_05[2] & f030507_KVAS$income05<quintiles_05[3],2,
                                        ifelse(f030507_KVAS$income05>=quintiles_05[3] & f030507_KVAS$income05<quintiles_05[4],3,
                                               ifelse(f030507_KVAS$income05>=quintiles_05[4]& f030507_KVAS$income05<quintiles_05[5],4,
                                                      ifelse(f030507_KVAS$income05>=quintiles_05[5],5,NA)))))
f030507_KVAS$quintile07 <- ifelse(f030507_KVAS$income07<quintiles_07[2],1,
                                 ifelse(f030507_KVAS$income07>=quintiles_07[2] & f030507_KVAS$income07<quintiles_07[3],2,
                                        ifelse(f030507_KVAS$income07>=quintiles_07[3] & f030507_KVAS$income07<quintiles_07[4],3,
                                               ifelse(f030507_KVAS$income07>=quintiles_07[4]& f030507_KVAS$income07<quintiles_07[5],4,
                                                      ifelse(f030507_KVAS$income07>=quintiles_07[5],5,NA)))))
f091113_KVAS$quintile09 <- ifelse(f091113_KVAS$income09<quintiles_09[2],1,
                                 ifelse(f091113_KVAS$income09>=quintiles_09[2] & f091113_KVAS$income09<quintiles_09[3],2,
                                        ifelse(f091113_KVAS$income09>=quintiles_09[3] & f091113_KVAS$income09<quintiles_09[4],3,
                                               ifelse(f091113_KVAS$income09>=quintiles_09[4]& f091113_KVAS$income09<quintiles_09[5],4,
                                                      ifelse(f091113_KVAS$income09>=quintiles_09[5],5,NA)))))
f091113_KVAS$quintile11 <- ifelse(f091113_KVAS$income11<quintiles_11[2],1,
                                 ifelse(f091113_KVAS$income11>=quintiles_11[2] & f091113_KVAS$income11<quintiles_11[3],2,
                                        ifelse(f091113_KVAS$income11>=quintiles_11[3] & f091113_KVAS$income11<quintiles_11[4],3,
                                               ifelse(f091113_KVAS$income11>=quintiles_11[4]& f091113_KVAS$income11<quintiles_11[5],4,
                                                      ifelse(f091113_KVAS$income11>=quintiles_11[5],5,NA)))))
f091113_KVAS$quintile13 <- ifelse(f091113_KVAS$income13<quintiles_13[2],1,
                                 ifelse(f091113_KVAS$income13>=quintiles_13[2] & f091113_KVAS$income13<quintiles_13[3],2,
                                        ifelse(f091113_KVAS$income13>=quintiles_13[3] & f091113_KVAS$income13<quintiles_13[4],3,
                                               ifelse(f091113_KVAS$income13>=quintiles_13[4]& f091113_KVAS$income13<quintiles_13[5],4,
                                                      ifelse(f091113_KVAS$income13>=quintiles_13[5],5,NA)))))



# familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f030507_KVAS,
#                                      nest=TRUE)
# 
# familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f091113_KVAS,
#                                      nest=TRUE)


# drop income

f030507_KVAS <- select(f030507_KVAS,-c("income03","income05","income07"))
f091113_KVAS <- select(f091113_KVAS,-c("income09","income11","income13"))


# wide to long

KVAS_regressionData_030507 <- reshape(f030507_KVAS, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
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
                                                  logactiveSaving=c(grep("logactiveSaving",colnames(f030507_KVAS))),
                                                  age=c(grep("age",colnames(f030507_KVAS))),
                                                  poorHTM=c(grep("poorHTM",colnames(f030507_KVAS))),
                                                  richHTM=c(grep("richHTM",colnames(f030507_KVAS))),
                                                  quintile=c(grep("quintile",colnames(f030507_KVAS))),
                                                  totalWealth=c(grep("totalWealth",colnames(f030507_KVAS))),
                                                  interestRate=c(grep("interestRate",colnames(f030507_KVAS)))),
                                     v.names = c("black","educ","employed",
                                                 "exIncome","famSize","kids","kidsOut","longWeight",
                                                 "other","logy","region","retired","unemployed","white","year","logactiveSaving","age","poorHTM","richHTM","quintile","totalWealth","interestRate"), #,"bigCity"),
                                     times=c("03", "05","07"))


KVAS_regressionData_091113 <- reshape(f091113_KVAS, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
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
                                                  logactiveSaving=c(grep("logactiveSaving",colnames(f091113_KVAS))),
                                                  age=c(grep("age",colnames(f091113_KVAS))),
                                                  poorHTM=c(grep("poorHTM",colnames(f091113_KVAS))),
                                                  richHTM=c(grep("richHTM",colnames(f091113_KVAS))),
                                                  quintile=c(grep("quintile",colnames(f091113_KVAS))),
                                                  totalWealth=c(grep("totalWealth",colnames(f091113_KVAS))),
                                                  interestRate=c(grep("interestRate",colnames(f091113_KVAS)))),
                                     v.names = c("black","educ","employed",
                                                 "exIncome","famSize","kids","kidsOut","longWeight",
                                                 "other","logy","region","retired","unemployed","white","year","logactiveSaving","age","poorHTM","richHTM","quintile","totalWealth","interestRate"), #,"bigCity"),
                                     times=c("09", "11","13"))

# create year of birth variable
KVAS_regressionData_030507$yob <- KVAS_regressionData_030507$year - KVAS_regressionData_030507$age
KVAS_regressionData_091113$yob <- KVAS_regressionData_091113$year - KVAS_regressionData_091113$age

# create factors for categorical variables
KVAS_regressionData_030507$educ <- factor(KVAS_regressionData_030507$educ)
KVAS_regressionData_030507$white <- factor(KVAS_regressionData_030507$white)
KVAS_regressionData_030507$black <- factor(KVAS_regressionData_030507$black)
KVAS_regressionData_030507$other <- factor(KVAS_regressionData_030507$other)
KVAS_regressionData_030507$employed <- factor(KVAS_regressionData_030507$employed)
KVAS_regressionData_030507$unemployed <- factor(KVAS_regressionData_030507$unemployed)
KVAS_regressionData_030507$retired <- factor(KVAS_regressionData_030507$retired)
KVAS_regressionData_030507$exIncome <- factor(KVAS_regressionData_030507$exIncome)
KVAS_regressionData_030507$region <- factor(KVAS_regressionData_030507$region)
KVAS_regressionData_030507$kidsOut <- factor(KVAS_regressionData_030507$kidsOut)
KVAS_regressionData_030507$poorHTM <- factor(KVAS_regressionData_030507$poorHTM)
KVAS_regressionData_030507$richHTM <- factor(KVAS_regressionData_030507$richHTM)


KVAS_regressionData_091113$educ <- factor(KVAS_regressionData_091113$educ)
KVAS_regressionData_091113$white <- factor(KVAS_regressionData_091113$white)
KVAS_regressionData_091113$black <- factor(KVAS_regressionData_091113$black)
KVAS_regressionData_091113$other <- factor(KVAS_regressionData_091113$other)
KVAS_regressionData_091113$employed <- factor(KVAS_regressionData_091113$employed)
KVAS_regressionData_091113$unemployed <- factor(KVAS_regressionData_091113$unemployed)
KVAS_regressionData_091113$retired <- factor(KVAS_regressionData_091113$retired)
KVAS_regressionData_091113$exIncome <- factor(KVAS_regressionData_091113$exIncome)
KVAS_regressionData_091113$region <- factor(KVAS_regressionData_091113$region)
KVAS_regressionData_091113$kidsOut <- factor(KVAS_regressionData_091113$kidsOut)
KVAS_regressionData_091113$poorHTM <- factor(KVAS_regressionData_091113$poorHTM)
KVAS_regressionData_091113$richHTM <- factor(KVAS_regressionData_091113$richHTM)



# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVAS_regressionData_030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVAS_regressionData_091113,
                                     nest=TRUE)





# do the regressions
# we first regress log income and log activeSaving expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log activeSaving d(cit) and
#log income d(yit).

KVAS_income_030507.lm = svyglm(logy ~ year +
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

KVAS_regressionData_030507$resIncome <-KVAS_income_030507.lm$residuals

KVAS_income_091113.lm = svyglm(logy ~ year +
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
KVAS_regressionData_091113$resIncome <-KVAS_income_091113.lm$residuals


KVAS_activeSaving_030507.lm = svyglm(logactiveSaving ~ year +
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
KVAS_regressionData_030507$resactiveSaving <- KVAS_activeSaving_030507.lm$residuals


KVAS_activeSaving_091113.lm = svyglm(logactiveSaving ~ year +
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
KVAS_regressionData_091113$resactiveSaving <-KVAS_activeSaving_091113.lm$residuals


KVAS_covData_030507 = KVAS_regressionData_030507[ , c("uniqueID","quintile","year","resactiveSaving","resIncome")]
KVAS_covData_091113 = KVAS_regressionData_091113[ , c("uniqueID","quintile","year","resactiveSaving","resIncome")]

# deal with outliers - take off bottom 0.1%
resactiveSaving_q_030507 <- quantile(KVAS_covData_030507$resactiveSaving,seq(0,1,0.001),na.rm=TRUE)
resactiveSaving_q_091113 <- quantile(KVAS_covData_091113$resactiveSaving,seq(0,1,0.001),na.rm=TRUE)

KVAS_covData_030507 <- subset(KVAS_covData_030507,KVAS_covData_030507$resactiveSaving>resactiveSaving_q_030507[2] & KVAS_covData_030507$resactiveSaving<resactiveSaving_q_030507[1000])
KVAS_covData_091113 <- subset(KVAS_covData_091113,KVAS_covData_091113$resactiveSaving>resactiveSaving_q_091113[2] & KVAS_covData_091113$resactiveSaving<resactiveSaving_q_091113[1000])

resIncome_q_030507 <- quantile(KVAS_covData_030507$resIncome,seq(0,1,0.001),na.rm=TRUE)
resIncome_q_091113 <- quantile(KVAS_covData_091113$resIncome,seq(0,1,0.001),na.rm=TRUE)

KVAS_covData_030507 <- subset(KVAS_covData_030507,KVAS_covData_030507$resIncome>resIncome_q_030507[2] & KVAS_covData_030507$resIncome<resIncome_q_030507[1000])
KVAS_covData_091113 <- subset(KVAS_covData_091113,KVAS_covData_091113$resIncome>resIncome_q_091113[2] & KVAS_covData_091113$resIncome<resIncome_q_091113[1000])

#plot residuals
plot(KVAS_covData_030507$resIncome,KVAS_covData_030507$resactiveSaving)
plot(KVAS_covData_091113$resIncome,KVAS_covData_091113$resactiveSaving)

KVAS_covData_030507 <- reshape(KVAS_covData_030507,idvar="uniqueID",direction="wide",v.names=c("quintile","resactiveSaving","resIncome"),timevar="year")
KVAS_covData_091113 <- reshape(KVAS_covData_091113,idvar="uniqueID",direction="wide",v.names=c("quintile","resactiveSaving","resIncome"),timevar="year")

KVAS_covData_030507$dst <- KVAS_covData_030507$resactiveSaving.2005 - KVAS_covData_030507$resactiveSaving.2003
KVAS_covData_030507$dyt <- KVAS_covData_030507$resIncome.2005 - KVAS_covData_030507$resIncome.2003
KVAS_covData_030507$dytplus1 <- KVAS_covData_030507$resIncome.2007 - KVAS_covData_030507$resIncome.2005


KVAS_covData_091113$dst <- KVAS_covData_091113$resactiveSaving.2011 - KVAS_covData_091113$resactiveSaving.2009
KVAS_covData_091113$dyt <- KVAS_covData_091113$resIncome.2011 - KVAS_covData_091113$resIncome.2009
KVAS_covData_091113$dytplus1 <- KVAS_covData_091113$resIncome.2013 - KVAS_covData_091113$resIncome.2011




# MPAS
print("MPAS as 07")
for (i in c(1,2,3,4,5)){
  MPAS_mdl_030507 <- ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVAS_covData_030507, subset=quintile.2003==i)
  assign(paste0("MPAS_mdl_030507_q",i),
         MPAS_mdl_030507)
  assign(paste0("MPAS_030507_q",i),
         MPAS_mdl_030507$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPAS_mdl_030507)
  assign(paste0("MPAS_CI_low_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPAS_CI_up_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPAS_CI_low_07_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPAS_030507_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPAS_CI_up_07_q",i))))))
}

# MPAS
print("MPAS as 13")
for (i in c(1,2,3,4,5)){
  MPAS_mdl_091113 = ivreg(dst ~  dyt, ~ dytplus1,x=TRUE, data=KVAS_covData_091113,subset=quintile.2009==i)
  assign(paste0("MPAS_mdl_091113_q",i),
         MPAS_mdl_091113)
  assign(paste0("MPAS_091113_q",i),
         MPAS_mdl_091113$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPAS_mdl_091113)
  assign(paste0("MPAS_CI_low_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPAS_CI_up_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPAS_CI_low_13_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPAS_091113_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPAS_CI_up_13_q",i))))))
}

out_MPAS <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                      "year" = c("2007","2007","2007","2007","2007","2013","2013","2013","2013","2013"),
                      "MPAS" = c(MPAS_030507_q1,
                                MPAS_030507_q2,
                                MPAS_030507_q3,
                                MPAS_030507_q4,
                                MPAS_030507_q5,
                                MPAS_091113_q1,
                                MPAS_091113_q2,
                                MPAS_091113_q3,
                                MPAS_091113_q4,
                                MPAS_091113_q5),
                      "MPAS_CI_lower" = c(MPAS_CI_low_07_q1,
                                         MPAS_CI_low_07_q2,
                                         MPAS_CI_low_07_q3,
                                         MPAS_CI_low_07_q4,
                                         MPAS_CI_low_07_q5,
                                         MPAS_CI_low_13_q1,
                                         MPAS_CI_low_13_q2,
                                         MPAS_CI_low_13_q3,
                                         MPAS_CI_low_13_q4,
                                         MPAS_CI_low_13_q5),
                      "MPAS_CI_upper" = c(MPAS_CI_up_07_q1,
                                         MPAS_CI_up_07_q2,
                                         MPAS_CI_up_07_q3,
                                         MPAS_CI_up_07_q4,
                                         MPAS_CI_up_07_q5,
                                         MPAS_CI_up_13_q1,
                                         MPAS_CI_up_13_q2,
                                         MPAS_CI_up_13_q3,
                                         MPAS_CI_up_13_q4,
                                         MPAS_CI_up_13_q5))
pdf(file=paste0(getwd(),"/Results/MPAS.pdf"))
ggplot(data = out_MPAS, aes(x = quantile, y = MPAS, group=year, color=year)) +
  geom_line() + geom_point()+
  geom_ribbon(data= out_MPAS,aes(ymin= MPAS_CI_lower,ymax= MPAS_CI_upper),alpha=0.3) +
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("MPAS") +
  labs(color="Year")
dev.off()
ggplot(data = out_MPAS, aes(x = quantile, y = MPAS, group=year, color=year)) +
  geom_line() + geom_point()+
  geom_ribbon(data= out_MPAS,aes(ymin= MPAS_CI_lower,ymax= MPAS_CI_upper),alpha=0.3) +
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("MPAS") +
  labs(color="Year")

# whole sample MPAS
MPAS_mdl_030507_whole = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVAS_covData_030507)
MPAS_mdl_030507_whole$coefficients[2]
anderson.rubin.ci(MPAS_mdl_030507_whole)
# write to text files
# coefficient
write(toString(round(MPAS_mdl_030507_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPAS_030507.txt"))
#standard error
write(toString(round(coef(summary(MPAS_mdl_030507_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPAS_030507_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPAS_mdl_030507_whole))[2,4])),file=paste0(getwd(),"/Results/MPAS_030507_stars.txt"))
#N
write(toString(MPAS_mdl_030507_whole$nobs),file=paste0(getwd(),"/Results/MPAS_030507_N.txt"))


MPAS_mdl_091113_whole = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=KVAS_covData_091113)
MPAS_mdl_091113_whole$coefficients[2]
anderson.rubin.ci(MPAS_mdl_091113_whole)
# write to text files
# coefficient
write(toString(round(MPAS_mdl_091113_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPAS_091113.txt"))
#standard error
write(toString(round(coef(summary(MPAS_mdl_091113_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPAS_091113_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPAS_mdl_091113_whole))[2,4])),file=paste0(getwd(),"/Results/MPAS_091113_stars.txt"))
#N
write(toString(MPAS_mdl_091113_whole$nobs),file=paste0(getwd(),"/Results/MPAS_091113_N.txt"))



rm(i,temp)#,KVAS_covData_030507, KVAS_covData_091113)
rm(list=ls(pattern="quintiles_"))
rm(list=ls(pattern="quantiles_"))
rm(list=ls(pattern="familyPanelSurvey"))


KVAS_covData_030507_PE <- KVAS_covData_030507[,c("uniqueID","dst","dyt","dytplus1")]
KVAS_covData_091113_PE <- KVAS_covData_091113[,c("uniqueID","dst","dyt","dytplus1")]

KVAS_covData_030507_PE <- rename(KVAS_covData_030507_PE,"dst_030507" = "dst","dyt_030507" = "dyt","dytplus1_030507" = "dytplus1")
KVAS_covData_091113_PE <- rename(KVAS_covData_091113_PE,"dst_091113" = "dst","dyt_091113" = "dyt","dytplus1_091113" = "dytplus1")
KVAS_covData_all_PE <- merge(KVAS_covData_091113_PE,KVAS_covData_030507_PE,by="uniqueID",all=TRUE)

eqn_030507 <- dst_030507 ~ -1 + dyt_030507
eqn_091113 <- dst_091113 ~ -1 + dyt_091113
system <- list(eqn_030507,eqn_091113)
inst1 <- ~ dytplus1_030507
inst2 <- ~ dytplus1_091113
instlist <- list( inst1, inst2 )

fit2sls2 <- systemfit( system, "2SLS", inst = instlist, data = KVAS_covData_all_PE )
print(fit2sls2)
linearHypothesis(fit2sls2,"eq1_dyt_030507=eq2_dyt_091113")


# to latex

print(xtable(KVAS_activeSaving_030507.lm, type = "latex"),file=paste0(getwd(),"/Results/KVAS_activeSaving_030507.tex"),floating=FALSE)
print(xtable(KVAS_activeSaving_091113.lm, type = "latex"),file=paste0(getwd(),"/Results/KVAS_activeSaving_091113.tex"),floating=FALSE)
print(xtable(KVAS_income_030507.lm, type = "latex"),file=paste0(getwd(),"/Results/KVAS_income_030507.tex"),floating=FALSE)
print(xtable(KVAS_income_091113.lm, type = "latex"),file=paste0(getwd(),"/Results/KVAS_income_091113.tex"),floating=FALSE)

writeLines(capture.output(stargazer(MPAS_mdl_030507_q1, MPAS_mdl_030507_q2,MPAS_mdl_030507_q3, MPAS_mdl_030507_q4,MPAS_mdl_030507_q5,
                                    float=FALSE, align=TRUE)),paste0(getwd(),"/Results/MPAS_mdl_030507.tex"))
writeLines(capture.output(stargazer(MPAS_mdl_091113_q1, MPAS_mdl_091113_q2,MPAS_mdl_091113_q3, MPAS_mdl_091113_q4,MPAS_mdl_091113_q5,
                                    float=FALSE, align=TRUE)),paste0(getwd(),"/Results/MPAS_mdl_091113.tex"))

writeLines(capture.output(stargazer(MPAS_mdl_030507_whole, MPAS_mdl_091113_whole,
                                    float=FALSE, align=TRUE)),paste0(getwd(),"/Results/MPAS_mdl_whole.tex"))
