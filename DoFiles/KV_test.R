# Kaplan Violante - consumption
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2007,09,11

f030507_KV <- f030507
f091113_KV <- f091113

##### CREATE VARIABLES ########
# create year of birth

# create year variable
f030507_KV$year03 <- 2003
f030507_KV$year05 <- 2005
f030507_KV$year07 <- 2007
f091113_KV$year09 <- 2009
f091113_KV$year11 <- 2011
f091113_KV$year13 <- 2013

f030507_KV$yob03 <- f030507_KV$year03 - f030507_KV$age03 
f030507_KV$yob05 <- f030507_KV$year05 - f030507_KV$age05 
f030507_KV$yob07 <- f030507_KV$year07 - f030507_KV$age07

f091113_KV$yob09 <- f091113_KV$year09 - f091113_KV$age09 
f091113_KV$yob11 <- f091113_KV$year11 - f091113_KV$age11 
f091113_KV$yob13 <- f091113_KV$year13 - f091113_KV$age13



# log consumption
f030507_KV$logConsumption03 <- log(f030507_KV$consumption03)
f030507_KV$logConsumption05 <- log(f030507_KV$consumption05)
f030507_KV$logConsumption07 <- log(f030507_KV$consumption07)

f091113_KV$logConsumption09 <- log(f091113_KV$consumption09)
f091113_KV$logConsumption11 <- log(f091113_KV$consumption11)
f091113_KV$logConsumption13 <- log(f091113_KV$consumption13)


# log income

f030507_KV$logy03 <- log(f030507_KV$income03)
f030507_KV$logy05 <- log(f030507_KV$income05)
f030507_KV$logy07 <- log(f030507_KV$income07)
f091113_KV$logy09 <- log(f091113_KV$income09)
f091113_KV$logy11 <- log(f091113_KV$income11)
f091113_KV$logy13 <- log(f091113_KV$income13)





f030507_KV_keeps <- c("uniqueID","primarySamplingUnit","stratification","black03","black05","black07",
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
                      "logConsumption03","logConsumption05","logConsumption07",
                      "age03","age05","age07",
                      "income03")

f030507_KV <- f030507_KV[,f030507_KV_keeps]

f091113_KV_keeps  <- c("uniqueID","primarySamplingUnit","stratification","black09","black11","black13",
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
                          "logConsumption09","logConsumption11","logConsumption13",
                          "age09","age11","age13",
                          "income09")

f091113_KV <- f091113_KV[,f091113_KV_keeps]

rm(f091113_KV_keeps,f030507_KV_keeps)

# drop NA 
f030507_KV <- na.omit(f030507_KV)
f091113_KV <- na.omit(f091113_KV)


# quintiles

# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507_KV,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113_KV,
                                     nest=TRUE)
quintiles_03 <- svyquantile(~income03,familyPanelSurvey030507, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_09 <- svyquantile(~income09,familyPanelSurvey091113, seq(0, 1, 0.2),NA.rm=TRUE)


f030507_KV$quintile <- ifelse(f030507_KV$income03<quintiles_03[2],1,
                                ifelse(f030507_KV$income03>=quintiles_03[2] & f030507_KV$income03<quintiles_03[3],2,
                                       ifelse(f030507_KV$income03>=quintiles_03[3] & f030507_KV$income03<quintiles_03[4],3,
                                              ifelse(f030507_KV$income03>=quintiles_03[4]& f030507_KV$income03<quintiles_03[5],4,
                                                     ifelse(f030507_KV$income03>=quintiles_03[5],5,NA)))))

f091113_KV$quintile <- ifelse(f091113_KV$income09<quintiles_09[2],1,
                                ifelse(f091113_KV$income09>=quintiles_09[2] & f091113_KV$income09<quintiles_09[3],2,
                                       ifelse(f091113_KV$income09>=quintiles_09[3] & f091113_KV$income09<quintiles_09[4],3,
                                              ifelse(f091113_KV$income09>=quintiles_09[4]& f091113_KV$income09<quintiles_09[5],4,
                                                     ifelse(f091113_KV$income09>=quintiles_09[5],5,NA)))))



familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507_KV,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113_KV,
                                     nest=TRUE)


# drop income

f030507_KV <- select(f030507_KV,-c("income03"))
f091113_KV <- select(f091113_KV,-c("income09"))


# wide to long

regressionData_030507 <- reshape(f030507_KV, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f030507_KV))),
                                              educ=c(grep("educ", colnames(f030507_KV))),
                                              employed=c(grep("^employed", colnames(f030507_KV))),
                                              exIncome=c(grep("exIncome", colnames(f030507_KV))),
                                              famSize=c(grep("famSize", colnames(f030507_KV))),
                                              kids=c(grep("kids[^Out]", colnames(f030507_KV))),
                                              kidsOut=c(grep("kidsOut", colnames(f030507_KV))),
                                              longWeight=c(grep("longWeight", colnames(f030507_KV))),
                                              other=c(grep("other", colnames(f030507_KV))), 
                                              logy=c(grep("logy",colnames(f030507_KV))),
                                              region=c(grep("region", colnames(f030507_KV))),
                                              retired=c(grep("retired",colnames(f030507_KV))),
                                              unemployed=c(grep("unemployed", colnames(f030507_KV))),
                                              white=c(grep("white", colnames(f030507_KV))),
                                              year=c(grep("year", colnames(f030507_KV))),
                                              yob=c(grep("yob",colnames(f030507_KV))),
                                              logConsumption=c(grep("logConsumption",colnames(f030507_KV))),
                                              age=c(grep("age",colnames(f030507_KV)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logConsumption","age"), #,"bigCity"),
                                 times=c("03", "05","07"))


regressionData_091113 <- reshape(f091113_KV, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f091113_KV))),
                                              educ=c(grep("educ", colnames(f091113_KV))),
                                              employed=c(grep("^employed", colnames(f091113_KV))),
                                              exIncome=c(grep("exIncome", colnames(f091113_KV))),
                                              famSize=c(grep("famSize", colnames(f091113_KV))),
                                              kids=c(grep("kids[^Out]", colnames(f091113_KV))),
                                              kidsOut=c(grep("kidsOut", colnames(f091113_KV))),
                                              longWeight=c(grep("longWeight", colnames(f091113_KV))),
                                              other=c(grep("other", colnames(f091113_KV))), 
                                              logy=c(grep("logy",colnames(f091113_KV))),
                                              region=c(grep("region", colnames(f091113_KV))),
                                              retired=c(grep("retired",colnames(f091113_KV))),
                                              unemployed=c(grep("unemployed", colnames(f091113_KV))),
                                              white=c(grep("white", colnames(f091113_KV))),
                                              year=c(grep("year", colnames(f091113_KV))),
                                              yob=c(grep("yob",colnames(f091113_KV))),
                                              logConsumption=c(grep("logConsumption",colnames(f091113_KV))),
                                              age=c(grep("age",colnames(f091113_KV)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logConsumption","age"), #,"bigCity"),
                                 times=c("09", "11","13"))


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


consumption_030507.lm = svyglm(logConsumption ~ year +
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
regressionData_030507$resConsumption <- consumption_030507.lm$residuals


consumption_091113.lm = svyglm(logConsumption ~ year +
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
regressionData_091113$resConsumption <-consumption_091113.lm$residuals


covData_030507 = regressionData_030507[ , c("uniqueID","primarySamplingUnit","stratification","longWeight", "quintile","year","resConsumption","resIncome")]
covData_091113 = regressionData_091113[ , c("uniqueID","primarySamplingUnit","stratification","longWeight","quintile","year","resConsumption","resIncome")]

covData_030507 <- reshape(covData_030507,idvar="uniqueID",direction="wide",v.names=c("resConsumption","resIncome","longWeight"),timevar="year")
covData_091113 <- reshape(covData_091113,idvar="uniqueID",direction="wide",v.names=c("resConsumption","resIncome","longWeight"),timevar="year")

covData_030507$dct <- covData_030507$resConsumption.2005 - covData_030507$resConsumption.2003
covData_030507$dyt <- covData_030507$resIncome.2005 - covData_030507$resIncome.2003
covData_030507$dytplus1 <- covData_030507$resIncome.2007 - covData_030507$resIncome.2005


covData_091113$dct <- covData_091113$resConsumption.2011 - covData_091113$resConsumption.2009
covData_091113$dyt <- covData_091113$resIncome.2011 - covData_091113$resIncome.2009
covData_091113$dytplus1 <- covData_091113$resIncome.2013 - covData_091113$resIncome.2011


# covDataSurvey030507 <- svydesign(id=~primarySamplingUnit,
#                                      strat=~stratification,
#                                      weights=~longWeight.2003,
#                                      data=covData_030507,
#                                      nest=TRUE)
# 
# covDataSurvey091113 <- svydesign(id=~primarySamplingUnit,
#                                  strat=~stratification,
#                                  weights=~longWeight.2007,
#                                  data=covData_091113,
#                                  nest=TRUE)
# 
# for (i in c(1,2,3,4,5)){
#   var <-  svyvar(~dct + dytplus1, design = subset(covDataSurvey030507,quintile==i))
#   var2 <-  svyvar(~dytplus1 + dyt, design = subset(covDataSurvey030507,quintile==i))
#   print(var/var2,covariance=TRUE)
# }
# for (i in c(1,2,3,4,5)){
# var <-  svyvar(~dct + dytplus1, design = subset(covDataSurvey091113,quintile==i))
# var2 <-  svyvar(~dytplus1 + dyt, design = subset(covDataSurvey091113,quintile==i))
# print(var/var2,covariance=TRUE)
# }


# MPC
  print("MPC 030507")
  for (i in c(1,2,3,4,5)){
    assign(paste0("MPC_030507_q",i),
           cov(subset(covData_030507$dct,covData_030507$quintile==i),subset(covData_030507$dytplus1,covData_030507$quintile==i))/
             cov(subset(covData_030507$dytplus1,covData_030507$quintile==i),subset(covData_030507$dyt,covData_030507$quintile==i))
    )
    print(paste0("q",i,":",unname(eval(as.name(paste0("MPC_030507_q",i))))))
  }
  print("MPC 091113")
  for (i in c(1,2,3,4,5)){
    assign(paste0("MPC_091113_q",i),
           cov(subset(covData_091113$dct,covData_091113$quintile==i),subset(covData_091113$dytplus1,covData_091113$quintile==i))/
             cov(subset(covData_091113$dytplus1,covData_091113$quintile==i),subset(covData_091113$dyt,covData_091113$quintile==i))
    )
    print(paste0("q",i,":",unname(eval(as.name(paste0("MPC_091113_q",i))))))
  }
  
rm(i, covData_030507,covData_091113,quintiles_03,quintiles_09,regressionData_030507,regressionData_091113,
   consumption_030507.lm, consumption_091113.lm, income_030507.lm, income_091113.lm, familyPanelSurvey030507, familyPanelSurvey091113)
