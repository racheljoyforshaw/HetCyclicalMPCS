# Kaplan Violante - consumption - IV, weighted
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2009,11,13

f0913_balanced <- f0913
f0913_balanced <- f0913_balanced[, -grep("primarySamplingUnit", colnames(f0913_balanced))] 
f0913_balanced <- f0913_balanced[, -grep("stratification", colnames(f0913_balanced))] 
f0313_KV_balanced <- merge(f0307,f0913_balanced, by=c('uniqueID'),all=FALSE)
rm(f0913_balanced)

##### CREATE VARIABLES ########
# create year of birth

# create year variable
f0313_KV_balanced$year03 <- 2003
f0313_KV_balanced$year05 <- 2005
f0313_KV_balanced$year07 <- 2007
f0313_KV_balanced$year09 <- 2009
f0313_KV_balanced$year11 <- 2011
f0313_KV_balanced$year13 <- 2013


# log consumption - move into positive
min_all <- min(min(f0313_KV_balanced$consumption03,na.rm=TRUE),min(f0313_KV_balanced$consumption05,na.rm=TRUE),min(f0313_KV_balanced$consumption07,na.rm=TRUE),
               min(f0313_KV_balanced$consumption09,na.rm=TRUE),min(f0313_KV_balanced$consumption11,na.rm=TRUE),min(f0313_KV_balanced$consumption13,na.rm=TRUE),
               min(f0313_KV_balanced$income03,na.rm=TRUE),min(f0313_KV_balanced$income05,na.rm=TRUE),min(f0313_KV_balanced$income07,na.rm=TRUE),
               min(f0313_KV_balanced$income09,na.rm=TRUE),min(f0313_KV_balanced$income11,na.rm=TRUE),min(f0313_KV_balanced$income13,na.rm=TRUE))

f0313_KV_balanced$logconsumption03 <- log(f0313_KV_balanced$consumption03 + abs(min_all) +1)
f0313_KV_balanced$logconsumption05 <- log(f0313_KV_balanced$consumption05+ abs(min_all) +1)
f0313_KV_balanced$logconsumption07 <- log(f0313_KV_balanced$consumption07+ abs(min_all) +1)

f0313_KV_balanced$logconsumption09 <- log(f0313_KV_balanced$consumption09+ abs(min_all) +1)
f0313_KV_balanced$logconsumption11 <- log(f0313_KV_balanced$consumption11+ abs(min_all) +1)
f0313_KV_balanced$logconsumption13 <- log(f0313_KV_balanced$consumption13+ abs(min_all) +1)


# log income - move into positive

f0313_KV_balanced$logy03 <- log(f0313_KV_balanced$income03 + abs(min_all) +1)
f0313_KV_balanced$logy05 <- log(f0313_KV_balanced$income05+ abs(min_all) +1)
f0313_KV_balanced$logy07 <- log(f0313_KV_balanced$income07+ abs(min_all) +1)

f0313_KV_balanced$logy09 <- log(f0313_KV_balanced$income09+ abs(min_all) +1)
f0313_KV_balanced$logy11 <- log(f0313_KV_balanced$income11+ abs(min_all) +1)
f0313_KV_balanced$logy13 <- log(f0313_KV_balanced$income13+ abs(min_all) +1)
rm(min_all)




f0313_KV_balanced_keeps <- c("uniqueID","primarySamplingUnit","stratification",#"black03","black05","black07",
                       "educ03","educ05","educ07",
                       "employed03","employed05","employed07",
                       "exIncome03","exIncome05","exIncome07",
                       "famSize03","famSize05","famSize07",
                       "kids03","kids05","kids07",
                       "kidsOut03","kidsOut05","kidsOut07",
                       "longWeight03","longWeight05","longWeight07",
                       #"other03","other05","other07",
                       "logy03","logy05","logy07",
                       "region03","region05","region07",
                       #"retired03","retired05","retired07",
                       #"unemployed03","unemployed05","unemployed07",
                       #"white03","white05","white07",
                       "year03","year05","year07",
                       "logconsumption03","logconsumption05","logconsumption07",
                       "age03","age05","age07",
                       "income03","income05","income07",
                       "poorHTM03","poorHTM05","poorHTM07",
                       #"richHTM03","richHTM05","richHTM07",
                       "totalWealth03","totalWealth05","totalWealth07",
                       "race03","race05","race07",
                       #"black09","black11","black13",
                       "educ09","educ11","educ13",
                       "employed09","employed11","employed13",
                       "exIncome09","exIncome11","exIncome13",
                       "famSize09","famSize11","famSize13",
                       "kids09","kids11","kids13",
                       "kidsOut09","kidsOut11","kidsOut13",
                       "longWeight09","longWeight11","longWeight13",
                       #"other09","other11","other13",
                       "logy09","logy11","logy13",
                       "region09","region11","region13",
                       #"retired09","retired11","retired13",
                       #"unemployed09","unemployed11","unemployed13",
                       #"white09","white11","white13",
                       "year09","year11","year13",
                       "logconsumption09","logconsumption11","logconsumption13",
                       "age09","age11","age13",
                       "income09","income11","income13",
                       "poorHTM09","poorHTM11","poorHTM13",
                       #"richHTM09","richHTM11","richHTM13",
                       "totalWealth09","totalWealth11","totalWealth13",
                       "race09","race11","race13"
                       )

f0313_KV_balanced <- f0313_KV_balanced[,f0313_KV_balanced_keeps]

rm(f0313_KV_balanced_keeps)

# drop NA 
f0313_KV_balanced <- na.omit(f0313_KV_balanced)


# quintiles

# survey design

# familyPanelSurvey0313 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f0313_KV_balanced,
#                                      nest=TRUE)
# 
# familyPanelSurvey0313 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f0313_KV_balanced,
#                                      nest=TRUE)

quintiles_03 <- quantile(f0313_KV_balanced$income03, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_05 <- quantile(f0313_KV_balanced$income05, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_07 <- quantile(f0313_KV_balanced$income07, seq(0, 1, 0.2),NA.rm=TRUE)

quintiles_09 <- quantile(f0313_KV_balanced$income09, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_11 <- quantile(f0313_KV_balanced$income11, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_13 <- quantile(f0313_KV_balanced$income13, seq(0, 1, 0.2),NA.rm=TRUE)


f0313_KV_balanced$quintile03 <- ifelse(f0313_KV_balanced$income03<quintiles_03[2],1,
                                 ifelse(f0313_KV_balanced$income03>=quintiles_03[2] & f0313_KV_balanced$income03<quintiles_03[3],2,
                                        ifelse(f0313_KV_balanced$income03>=quintiles_03[3] & f0313_KV_balanced$income03<quintiles_03[4],3,
                                               ifelse(f0313_KV_balanced$income03>=quintiles_03[4]& f0313_KV_balanced$income03<quintiles_03[5],4,
                                                      ifelse(f0313_KV_balanced$income03>=quintiles_03[5],5,NA)))))
f0313_KV_balanced$quintile05 <- ifelse(f0313_KV_balanced$income05<quintiles_05[2],1,
                                 ifelse(f0313_KV_balanced$income05>=quintiles_05[2] & f0313_KV_balanced$income05<quintiles_05[3],2,
                                        ifelse(f0313_KV_balanced$income05>=quintiles_05[3] & f0313_KV_balanced$income05<quintiles_05[4],3,
                                               ifelse(f0313_KV_balanced$income05>=quintiles_05[4]& f0313_KV_balanced$income05<quintiles_05[5],4,
                                                      ifelse(f0313_KV_balanced$income05>=quintiles_05[5],5,NA)))))
f0313_KV_balanced$quintile07 <- ifelse(f0313_KV_balanced$income07<quintiles_07[2],1,
                                 ifelse(f0313_KV_balanced$income07>=quintiles_07[2] & f0313_KV_balanced$income07<quintiles_07[3],2,
                                        ifelse(f0313_KV_balanced$income07>=quintiles_07[3] & f0313_KV_balanced$income07<quintiles_07[4],3,
                                               ifelse(f0313_KV_balanced$income07>=quintiles_07[4]& f0313_KV_balanced$income07<quintiles_07[5],4,
                                                      ifelse(f0313_KV_balanced$income07>=quintiles_07[5],5,NA)))))
f0313_KV_balanced$quintile09 <- ifelse(f0313_KV_balanced$income09<quintiles_09[2],1,
                                 ifelse(f0313_KV_balanced$income09>=quintiles_09[2] & f0313_KV_balanced$income09<quintiles_09[3],2,
                                        ifelse(f0313_KV_balanced$income09>=quintiles_09[3] & f0313_KV_balanced$income09<quintiles_09[4],3,
                                               ifelse(f0313_KV_balanced$income09>=quintiles_09[4]& f0313_KV_balanced$income09<quintiles_09[5],4,
                                                      ifelse(f0313_KV_balanced$income09>=quintiles_09[5],5,NA)))))
f0313_KV_balanced$quintile11 <- ifelse(f0313_KV_balanced$income11<quintiles_11[2],1,
                                 ifelse(f0313_KV_balanced$income11>=quintiles_11[2] & f0313_KV_balanced$income11<quintiles_11[3],2,
                                        ifelse(f0313_KV_balanced$income11>=quintiles_11[3] & f0313_KV_balanced$income11<quintiles_11[4],3,
                                               ifelse(f0313_KV_balanced$income11>=quintiles_11[4]& f0313_KV_balanced$income11<quintiles_11[5],4,
                                                      ifelse(f0313_KV_balanced$income11>=quintiles_11[5],5,NA)))))
f0313_KV_balanced$quintile13 <- ifelse(f0313_KV_balanced$income13<quintiles_13[2],1,
                                 ifelse(f0313_KV_balanced$income13>=quintiles_13[2] & f0313_KV_balanced$income13<quintiles_13[3],2,
                                        ifelse(f0313_KV_balanced$income13>=quintiles_13[3] & f0313_KV_balanced$income13<quintiles_13[4],3,
                                               ifelse(f0313_KV_balanced$income13>=quintiles_13[4]& f0313_KV_balanced$income13<quintiles_13[5],4,
                                                      ifelse(f0313_KV_balanced$income13>=quintiles_13[5],5,NA)))))



# familyPanelSurvey0313 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight03,
#                                      data=f0313_KV_balanced,
#                                      nest=TRUE)
# 
# familyPanelSurvey0313 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight09,
#                                      data=f0313_KV_balanced,
#                                      nest=TRUE)


# drop income

f0313_KV_balanced <- select(f0313_KV_balanced,-c("income03","income05","income07"))
f0313_KV_balanced <- select(f0313_KV_balanced,-c("income09","income11","income13"))


# wide to long

KVC_balanced_regressionData_0313 <- reshape(f0313_KV_balanced, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                     varying=list(#black=c(grep("black", colnames(f0313_KV_balanced))),
                                                  educ=c(grep("educ", colnames(f0313_KV_balanced))),
                                                  employed=c(grep("^employed", colnames(f0313_KV_balanced))),
                                                  exIncome=c(grep("exIncome", colnames(f0313_KV_balanced))),
                                                  famSize=c(grep("famSize", colnames(f0313_KV_balanced))),
                                                  kids=c(grep("kids[^Out]", colnames(f0313_KV_balanced))),
                                                  kidsOut=c(grep("kidsOut", colnames(f0313_KV_balanced))),
                                                  longWeight=c(grep("longWeight", colnames(f0313_KV_balanced))),
                                                  #other=c(grep("other", colnames(f0313_KV_balanced))), 
                                                  logy=c(grep("logy",colnames(f0313_KV_balanced))),
                                                  region=c(grep("region", colnames(f0313_KV_balanced))),
                                                  #retired=c(grep("retired",colnames(f0313_KV_balanced))),
                                                  #unemployed=c(grep("unemployed", colnames(f0313_KV_balanced))),
                                                  #white=c(grep("white", colnames(f0313_KV_balanced))),
                                                  year=c(grep("year", colnames(f0313_KV_balanced))),
                                                  logconsumption=c(grep("logconsumption",colnames(f0313_KV_balanced))),
                                                  age=c(grep("age",colnames(f0313_KV_balanced))),
                                                  poorHTM=c(grep("poorHTM",colnames(f0313_KV_balanced))),
                                                  #richHTM=c(grep("richHTM",colnames(f0313_KV_balanced))),
                                                  quintile=c(grep("quintile",colnames(f0313_KV_balanced))),
                                                  totalWealth=c(grep("totalWealth",colnames(f0313_KV_balanced))),
                                                  race=c(grep("race",colnames(f0313_KV_balanced)))),
                                     v.names = c("educ","employed",
                                                 "exIncome","famSize","kids","kidsOut","longWeight",
                                                 "logy","region","year","logconsumption","age","poorHTM","quintile","totalWealth","race"), #,"bigCity"),
                                     times=c("03", "05","07","09", "11","13"))


# create year of birth variable
KVC_balanced_regressionData_0313$yob <- KVC_balanced_regressionData_0313$year - KVC_balanced_regressionData_0313$age

# create factors
KVC_balanced_regressionData_0313$year <- factor(KVC_balanced_regressionData_0313$year)
KVC_balanced_regressionData_0313$yob <- factor(KVC_balanced_regressionData_0313$yob)
KVC_balanced_regressionData_0313$educ <- factor(KVC_balanced_regressionData_0313$educ)
KVC_balanced_regressionData_0313$race <- factor(KVC_balanced_regressionData_0313$race)
KVC_balanced_regressionData_0313$employed <- factor(KVC_balanced_regressionData_0313$employed)
KVC_balanced_regressionData_0313$exIncome <- factor(KVC_balanced_regressionData_0313$exIncome)
KVC_balanced_regressionData_0313$region <- factor(KVC_balanced_regressionData_0313$region)
KVC_balanced_regressionData_0313$kidsOut <- factor(KVC_balanced_regressionData_0313$kidsOut)
KVC_balanced_regressionData_0313$poorHTM <- factor(KVC_balanced_regressionData_0313$poorHTM)

# divide wealth by 10000
KVC_balanced_regressionData_0313$totalWealth <- KVC_balanced_regressionData_0313$totalWealth/10000

# survey design

familyPanelSurvey0313 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight,
                                     data=KVC_balanced_regressionData_0313,
                                     nest=TRUE)




# do the regressions
# we first regress log income and log consumption expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log consumption d(cit) and
#log income d(yit).

KVC_balanced_income_0313.lm = svyglm(logy ~ year +
                                yob +
                                #age +
                                educ +
                                race +
                                #white +
                                #black +
                                #other +
                                famSize + 
                                kids + 
                                employed +
                                #unemployed +
                                #retired +
                                exIncome +
                                region + 
                                kidsOut + 
                                poorHTM +
                                #richHTM + 
                                #educ*year +
                                #white*year +
                                #black*year +
                                #other*year +
                                #employed*year + 
                                #unemployed*year +
                                #retired*year +
                                #region*year +
                                totalWealth #+ 
                                #interestRate
                              ,familyPanelSurvey0313) 

KVC_balanced_regressionData_0313$resIncome <-KVC_balanced_income_0313.lm$residuals


KVC_balanced_consumption_0313.lm = svyglm(logconsumption ~ year +
                                     yob +
                                     #age +
                                     educ +
                                     race +
                                     #white +
                                     #black +
                                     #other +
                                     famSize + 
                                     kids + 
                                     employed +
                                     #unemployed +
                                     #retired +
                                     exIncome +
                                     region + 
                                     kidsOut + 
                                     poorHTM +
                                     #richHTM + 
                                     # educ*year +
                                     # white*year +
                                     # black*year +
                                     # other*year +
                                     # employed*year + 
                                     # unemployed*year +
                                     # retired*year +
                                     # region*year +
                                     totalWealth #+
                                     #interestRate
                                   , familyPanelSurvey0313) 
KVC_balanced_regressionData_0313$resconsumption <- KVC_balanced_consumption_0313.lm$residuals



KVC_balanced_covData_0313 = KVC_balanced_regressionData_0313[ , c("uniqueID","quintile","year","resconsumption","resIncome")]

# deal with outliers - take off bottom 0.1%
resconsumption_q_0313 <- quantile(KVC_balanced_covData_0313$resconsumption,seq(0,1,0.001),na.rm=TRUE)
KVC_balanced_covData_0313 <- subset(KVC_balanced_covData_0313,KVC_balanced_covData_0313$resconsumption>resconsumption_q_0313[2] & KVC_balanced_covData_0313$resconsumption<resconsumption_q_0313[1000])

resIncome_q_0313 <- quantile(KVC_balanced_covData_0313$resIncome,seq(0,1,0.001),na.rm=TRUE)
KVC_balanced_covData_0313 <- subset(KVC_balanced_covData_0313,KVC_balanced_covData_0313$resIncome>resIncome_q_0313[2] & KVC_balanced_covData_0313$resIncome<resIncome_q_0313[1000])

#plot residuals
plot(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$resconsumption)

# make sure have observations for every year
KVC_balanced_covData_0313 <- KVC_balanced_covData_0313[KVC_balanced_covData_0313$uniqueID %in% names(which(table(KVC_balanced_covData_0313$uniqueID)==6)), ]


KVC_balanced_covData_0313$dct <- ifelse(KVC_balanced_covData_0313$year==2003,subset(KVC_balanced_covData_0313$resconsumption,KVC_balanced_covData_0313$year==2005) - subset(KVC_balanced_covData_0313$resconsumption,KVC_balanced_covData_0313$year==2003),
                                        ifelse(KVC_balanced_covData_0313$year==2005,NA,
                                        ifelse(KVC_balanced_covData_0313$year==2007,NA,
                                               ifelse(KVC_balanced_covData_0313$year==2009,subset(KVC_balanced_covData_0313$resconsumption,KVC_balanced_covData_0313$year==2011) - subset(KVC_balanced_covData_0313$resconsumption,KVC_balanced_covData_0313$year==2009),
                                                      ifelse(KVC_balanced_covData_0313$year==2011,NA,
                                                             ifelse(KVC_balanced_covData_0313$year==2013,NA,NA))))))

KVC_balanced_covData_0313$dyt <- ifelse(KVC_balanced_covData_0313$year==2003,subset(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$year==2005) - subset(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$year==2003),
                                        ifelse(KVC_balanced_covData_0313$year==2005,NA ,
                                               ifelse(KVC_balanced_covData_0313$year==2007,NA,
                                                      ifelse(KVC_balanced_covData_0313$year==2009,subset(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$year==2011) - subset(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$year==2009),
                                                             ifelse(KVC_balanced_covData_0313$year==2011,NA,
                                                                    ifelse(KVC_balanced_covData_0313$year==2013,NA,NA))))))

KVC_balanced_covData_0313$dytplus1 <- ifelse(KVC_balanced_covData_0313$year==2003,subset(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$year==2007) - subset(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$year==2005) ,
                                        ifelse(KVC_balanced_covData_0313$year==2005,NA,
                                               ifelse(KVC_balanced_covData_0313$year==2007,NA,
                                                      ifelse(KVC_balanced_covData_0313$year==2009,subset(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$year==2013) - subset(KVC_balanced_covData_0313$resIncome,KVC_balanced_covData_0313$year==2011),
                                                             ifelse(KVC_balanced_covData_0313$year==2011,NA,
                                                                    ifelse(KVC_balanced_covData_0313$year==2013,NA,NA))))))

#KVC_balanced_covData_0313 <- KVC_balanced_covData_0313[,c("uniqueID","quintile","dct","dyt","dytplus1","year")]

#KVC_balanced_covData_0313 <- reshape(KVC_balanced_covData_0313,idvar="uniqueID",direction="wide",v.names=c("quintile","dct","dyt","dytplus1"),timevar="year")

KVC_balanced_covData_0313$quintile.2003 <- ifelse(KVC_balanced_covData_0313$year==2003,subset(KVC_balanced_covData_0313$quintile,KVC_balanced_covData_0313$year==2003),
                                                  ifelse(KVC_balanced_covData_0313$year==2005,NA,
                                                         ifelse(KVC_balanced_covData_0313$year==2007,NA,
                                                                ifelse(KVC_balanced_covData_0313$year==2009,subset(KVC_balanced_covData_0313$quintile,KVC_balanced_covData_0313$year==2009),
                                                                       ifelse(KVC_balanced_covData_0313$year==2011,NA,
                                                                              ifelse(KVC_balanced_covData_0313$year==2013,NA,NA))))))
KVC_balanced_covData_0313 <- na.omit(KVC_balanced_covData_0313)
# MPC_balanced
print("MPC_balanced")
for (i in c(1,2,3,4,5)){
  MPC_balanced_mdl_0307 <- ivreg(dct ~ dyt, ~ dytplus1, x=TRUE, data=KVC_balanced_covData_0313, subset=quintile.2003==i)
  assign(paste0("MPC_balanced_mdl_0307_q",i),
         MPC_balanced_mdl_0307)
  assign(paste0("MPC_balanced_0307_q",i),
         MPC_balanced_mdl_0307$coefficients[2]
  )
  temp <- anderson.rubin.ci(MPC_balanced_mdl_0307)
  assign(paste0("MPC_balanced_CI_low_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPC_balanced_CI_up_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPC_balanced_CI_low_07_q",i))))))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPC_balanced_0307_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPC_balanced_CI_up_07_q",i))))))
}


out_MPC_balanced <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                      #"year" = c("2007","2007","2007","2007","2007"),
                      "MPC_balanced" = c(MPC_balanced_0307_q1,
                                MPC_balanced_0307_q2,
                                MPC_balanced_0307_q3,
                                MPC_balanced_0307_q4,
                                MPC_balanced_0307_q5),
                      "MPC_balanced_CI_lower" = c(MPC_balanced_CI_low_07_q1,
                                         MPC_balanced_CI_low_07_q2,
                                         MPC_balanced_CI_low_07_q3,
                                         MPC_balanced_CI_low_07_q4,
                                         MPC_balanced_CI_low_07_q5),
                      "MPC_balanced_CI_upper" = c(MPC_balanced_CI_up_07_q1,
                                         MPC_balanced_CI_up_07_q2,
                                         MPC_balanced_CI_up_07_q3,
                                         MPC_balanced_CI_up_07_q4,
                                         MPC_balanced_CI_up_07_q5))
pdf(file=paste0(getwd(),"/Results/MPC_balanced_whole.pdf"))
ggplot(data = out_MPC_balanced, aes(x = quantile, y = MPC_balanced)) +
  geom_line() + geom_point()+
  scale_color_grey() +
  geom_ribbon(data= out_MPC_balanced,aes(ymin= MPC_balanced_CI_lower,ymax= MPC_balanced_CI_upper),alpha=0.3) +
  xlab("Income Quintile") +
  ylab("MPC (balanced panel)") +
  theme_pubr()
dev.off()



# whole sample MPC_balanced
MPC_balanced_mdl_0307_whole = ivreg(dct ~ dyt, ~ dytplus1, x=TRUE, data=KVC_balanced_covData_0313,subset=(year==2005))
MPC_balanced_mdl_0307_whole$coefficients[2]
anderson.rubin.ci(MPC_balanced_mdl_0307_whole)
# write to text files
# coefficient
write(toString(round(MPC_balanced_mdl_0307_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPC_balanced_0313.txt"))
#standard error
write(toString(round(coef(summary(MPC_balanced_mdl_0307_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPC_balanced_0313_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPC_balanced_mdl_0307_whole))[2,4])),file=paste0(getwd(),"/Results/MPC_balanced_0313_stars.txt"))
#N
write(toString(MPC_balanced_mdl_0307_whole$nobs),file=paste0(getwd(),"/Results/MPC_balanced_0313_N.txt"))


MPC_balanced_mdl_0913_whole = ivreg(dct ~ dyt, ~ dytplus1, x=TRUE, data=KVC_balanced_covData_0313,subset=(year==2009))
MPC_balanced_mdl_0913_whole$coefficients[2]
anderson.rubin.ci(MPC_balanced_mdl_0913_whole)
# write to text files
# coefficient
write(toString(round(MPC_balanced_mdl_0913_whole$coefficients[2],3)),file=paste0(getwd(),"/Results/MPC_balanced_0913.txt"))
#standard error
write(toString(round(coef(summary(MPC_balanced_mdl_0913_whole))[2,2],3)),file=paste0(getwd(),"/Results/MPC_balanced_0913_stdErr.txt"))
# stars
write(toString(stars.pval(coef(summary(MPC_balanced_mdl_0913_whole))[2,4])),file=paste0(getwd(),"/Results/MPC_balanced_0913_stars.txt"))
#N
write(toString(MPC_balanced_mdl_0913_whole$nobs),file=paste0(getwd(),"/Results/MPC_balanced_0913_N.txt"))



rm(i,temp)#,KVC_balanced_covData_0313, KVC_balanced_covData_0313)
rm(list=ls(pattern="quintiles_"))
rm(list=ls(pattern="quantiles_"))
rm(list=ls(pattern="familyPanelSurvey"))

KVC_balanced_covData_0313_PE <- KVC_balanced_covData_0313[,c("uniqueID","dct","dyt","dytplus1","year")]
KVC_balanced_covData_0313_PE <- KVC_balanced_covData_0313[,c("uniqueID","dct","dyt","dytplus1","year")]

KVC_balanced_covData_0313_PE$dct_0307 <-ifelse(KVC_balanced_covData_0313_PE$year==2003,
                                                 KVC_balanced_covData_0313_PE$dct,NA)
KVC_balanced_covData_0313_PE$dyt_0307 <-ifelse(KVC_balanced_covData_0313_PE$year==2003,
                                                 KVC_balanced_covData_0313_PE$dyt,NA)
KVC_balanced_covData_0313_PE$dytplus1_0307 <-ifelse(KVC_balanced_covData_0313_PE$year==2003,
                                                 KVC_balanced_covData_0313_PE$dytplus1,NA)
KVC_balanced_covData_0313_PE$dct_0913 <-ifelse(KVC_balanced_covData_0313_PE$year==2009,
                                                 KVC_balanced_covData_0313_PE$dct,NA)
KVC_balanced_covData_0313_PE$dyt_0913 <-ifelse(KVC_balanced_covData_0313_PE$year==2009,
                                                 KVC_balanced_covData_0313_PE$dyt,NA)
KVC_balanced_covData_0313_PE$dytplus1_0913 <-ifelse(KVC_balanced_covData_0313_PE$year==2009,
                                                 KVC_balanced_covData_0313_PE$dytplus1,NA)

KVC_balanced_covData_0313_PE=na.omit(KVC_balanced_covData_0313_PE)

eqn_0307 <- dct_0307 ~ -1 + dyt_0307
eqn_0913 <- dct_0913 ~ -1 + dyt_0913
system <- list(eqn_0307,eqn_0913)
inst1 <- ~ dytplus1_0307
inst2 <- ~ dytplus1_0913
instlist <- list( inst1, inst2 )

fit2sls2 <- systemfit( system, "2SLS", inst = instlist, data = KVC_balanced_covData_0313_PE )
print(fit2sls2)
linearHypothesis(fit2sls2,"eq1_dyt_0307=eq2_dyt_0913")


# latex
writeLines(capture.output(stargazer(KVC_balanced_consumption_0313.lm,KVC_balanced_income_0313.lm,
                                    omit=c("yob"),omit.labels = ("Year of Birth"),
                                    omit.stat =c("ll","rsq","aic"),
                                    column.labels = c("2002-2012","2002-2012"),
                                    covariate.labels = c("Year=2004","Year=2006","Year=2008","Year=2010","Year=2012",
                                                         "Education=Medium","Education=High",
                                                         "Race=Black","Race=Other","Family Size","Number of Kids",
                                                         "Status=Unemployed","Status=Retired","Status=Inactive",
                                                         "Extra Family Income",
                                                         "Region=Midwest", "Region=South","Region=West",
                                                         "Kids outside Family Unit",
                                                         "Poor-HtM","Rich-HtM",
                                                         "Total Wealth (\\$1000s)",
                                                         "Constant"),
                                    dep.var.labels = c("log($\\widehat{c_{it}}$)","log($\\widehat{y_{it}}$)"),
                                    dep.var.caption="",
                                    float=FALSE, align=TRUE,style = "qje",no.space=TRUE)),
           paste0(getwd(),"/Results/KVC_balanced_consumption_0307.tex"))
#table.layout = "=d#-t=n"
#print(xtable(KVC_balanced_consumption_0307.lm, type = "latex"),file=paste0(getwd(),"/Results/KVC_balanced_consumption_0307.tex"),floating=FALSE)
#print(xtable(KVC_balanced_consumption_0913.lm, type = "latex"),file=paste0(getwd(),"/Results/KVC_balanced_consumption_0913.tex"),floating=FALSE)
#print(xtable(KVC_balanced_income_0307.lm, type = "latex"),file=paste0(getwd(),"/Results/KVC_balanced_income_0307.tex"),floating=FALSE)
#print(xtable(KVC_balanced_income_0913.lm, type = "latex"),file=paste0(getwd(),"/Results/KVC_balanced_income_0913.tex"),floating=FALSE)

writeLines(capture.output(stargazer(MPC_balanced_mdl_0307_q1, MPC_balanced_mdl_0307_q2,MPC_balanced_mdl_0307_q3, MPC_balanced_mdl_0307_q4,MPC_balanced_mdl_0307_q5,
                                    float=FALSE, align=TRUE,dep.var.caption="",dep.var.labels = c("$\\Delta \\widehat{c_{i,t}}$"),
                                    omit=c("Constant"), covariate.labels = c("$\\Delta \\widehat{y_{i,t}}$"),
                                    omit.stat =c("adj.rsq"))),paste0(getwd(),"/Results/MPC_balanced_mdl_0307.tex"))
writeLines(capture.output(stargazer(MPC_balanced_mdl_0913_q1, MPC_balanced_mdl_0913_q2,MPC_balanced_mdl_0913_q3, MPC_balanced_mdl_0913_q4,MPC_balanced_mdl_0913_q5,
                                    float=FALSE, align=TRUE,dep.var.caption="",dep.var.labels = c("$\\Delta \\widehat{c_{i,t}}$"),
                                    omit=c("Constant"), covariate.labels = c("$\\Delta \\widehat{y_{i,t}}$"),
                                    omit.stat =c("adj.rsq"))),paste0(getwd(),"/Results/MPC_balanced_mdl_0913.tex"))


