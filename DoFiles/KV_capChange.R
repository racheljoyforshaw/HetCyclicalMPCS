# Kaplan Violante - capital change
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2007,09,11

f030507_KVCC <- f030507
f070911_KVCC <- f070911

##### CREATE VARIABLES ########
# create year of birth

# create year variable
f030507_KVCC$year03 <- 2003
f030507_KVCC$year05 <- 2005
f030507_KVCC$year07 <- 2007
f070911_KVCC$year07 <- 2007
f070911_KVCC$year09 <- 2009
f070911_KVCC$year11 <- 2011

f030507_KVCC$yob03 <- f030507_KVCC$year03 - f030507_KVCC$age03 
f030507_KVCC$yob05 <- f030507_KVCC$year05 - f030507_KVCC$age05 
f030507_KVCC$yob07 <- f030507_KVCC$year07 - f030507_KVCC$age07

f070911_KVCC$yob07 <- f070911_KVCC$year07 - f070911_KVCC$age07 
f070911_KVCC$yob09 <- f070911_KVCC$year09 - f070911_KVCC$age09 
f070911_KVCC$yob11 <- f070911_KVCC$year11 - f070911_KVCC$age11



# log capChange
min030507 <- min(min(f030507_KVCC$capChange03,na.rm=TRUE),min(f030507_KVCC$capChange05,na.rm=TRUE),min(f030507_KVCC$capChange07,na.rm=TRUE))
f030507_KVCC$logCapChange03 <- log(f030507_KVCC$capChange03 + abs(min030507) +1)
f030507_KVCC$logCapChange05 <- log(f030507_KVCC$capChange05+ abs(min030507) +1)
f030507_KVCC$logCapChange07 <- log(f030507_KVCC$capChange07+ abs(min030507) +1)

min070911 <- min(min(f070911_KVCC$capChange07,na.rm=TRUE),min(f070911_KVCC$capChange09,na.rm=TRUE),min(f070911_KVCC$capChange09,na.rm=TRUE))
f070911_KVCC$logCapChange07 <- log(f070911_KVCC$capChange07+ abs(min070911) +1)
f070911_KVCC$logCapChange09 <- log(f070911_KVCC$capChange09+ abs(min070911) +1)
f070911_KVCC$logCapChange11 <- log(f070911_KVCC$capChange11+ abs(min070911) +1)

rm(min030507, min070911)


# log income

f030507_KVCC$logy03 <- log(f030507_KVCC$income03)
f030507_KVCC$logy05 <- log(f030507_KVCC$income05)
f030507_KVCC$logy07 <- log(f030507_KVCC$income07)
f070911_KVCC$logy07 <- log(f070911_KVCC$income07)
f070911_KVCC$logy09 <- log(f070911_KVCC$income09)
f070911_KVCC$logy11 <- log(f070911_KVCC$income11)





f030507_KVCC_keeps <- c("uniqueID","primarySamplingUnit","stratification","black03","black05","black07",
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
                        "logCapChange03","logCapChange05","logCapChange07",
                        "age03","age05","age07",
                        "income03")

f030507_KVCC <- f030507_KVCC[,f030507_KVCC_keeps]

f070911_KVCC_keeps <- c("uniqueID","primarySamplingUnit","stratification","black07","black09","black11",
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
                        "logCapChange07","logCapChange09","logCapChange11",
                        "age07","age09","age11",
                        "income07")

f070911_KVCC <- f070911_KVCC[,f070911_KVCC_keeps]

rm(f070911_KVCC_keeps,f030507_KVCC_keeps)

# drop NA 
f030507_KVCC <- na.omit(f030507_KVCC)
f070911_KVCC <- na.omit(f070911_KVCC)


# quintiles

# survey design

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507_KVCC,
                                     nest=TRUE)

familyPanelSurvey070911 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight07,
                                     data=f070911_KVCC,
                                     nest=TRUE)
quintiles_03 <- svyquantile(~income03,familyPanelSurvey030507, seq(0, 1, 0.2),NA.rm=TRUE)
quintiles_07 <- svyquantile(~income07,familyPanelSurvey070911, seq(0, 1, 0.2),NA.rm=TRUE)


f030507_KVCC$quintile <- ifelse(f030507_KVCC$income03<quintiles_03[2],1,
                                ifelse(f030507_KVCC$income03>=quintiles_03[2] & f030507_KVCC$income03<quintiles_03[3],2,
                                       ifelse(f030507_KVCC$income03>=quintiles_03[3] & f030507_KVCC$income03<quintiles_03[4],3,
                                              ifelse(f030507_KVCC$income03>=quintiles_03[4]& f030507_KVCC$income03<quintiles_03[5],4,
                                                     ifelse(f030507_KVCC$income03>=quintiles_03[5],5,NA)))))

f070911_KVCC$quintile <- ifelse(f070911_KVCC$income07<quintiles_07[2],1,
                                ifelse(f070911_KVCC$income07>=quintiles_07[2] & f070911_KVCC$income07<quintiles_07[3],2,
                                       ifelse(f070911_KVCC$income07>=quintiles_07[3] & f070911_KVCC$income07<quintiles_07[4],3,
                                              ifelse(f070911_KVCC$income07>=quintiles_07[4]& f070911_KVCC$income07<quintiles_07[5],4,
                                                     ifelse(f070911_KVCC$income07>=quintiles_07[5],5,NA)))))



familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507_KVCC,
                                     nest=TRUE)

familyPanelSurvey070911 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight07,
                                     data=f070911_KVCC,
                                     nest=TRUE)


# drop income

f030507_KVCC <- select(f030507_KVCC,-c("income03"))
f070911_KVCC <- select(f070911_KVCC,-c("income07"))


# wide to long

regressionData_030507 <- reshape(f030507_KVCC, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f030507_KVCC))),
                                              educ=c(grep("educ", colnames(f030507_KVCC))),
                                              employed=c(grep("^employed", colnames(f030507_KVCC))),
                                              exIncome=c(grep("exIncome", colnames(f030507_KVCC))),
                                              famSize=c(grep("famSize", colnames(f030507_KVCC))),
                                              kids=c(grep("kids[^Out]", colnames(f030507_KVCC))),
                                              kidsOut=c(grep("kidsOut", colnames(f030507_KVCC))),
                                              longWeight=c(grep("longWeight", colnames(f030507_KVCC))),
                                              other=c(grep("other", colnames(f030507_KVCC))), 
                                              logy=c(grep("logy",colnames(f030507_KVCC))),
                                              region=c(grep("region", colnames(f030507_KVCC))),
                                              retired=c(grep("retired",colnames(f030507_KVCC))),
                                              unemployed=c(grep("unemployed", colnames(f030507_KVCC))),
                                              white=c(grep("white", colnames(f030507_KVCC))),
                                              year=c(grep("year", colnames(f030507_KVCC))),
                                              yob=c(grep("yob",colnames(f030507_KVCC))),
                                              logCapChange=c(grep("logCapChange",colnames(f030507_KVCC))),
                                              age=c(grep("age",colnames(f030507_KVCC)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logCapChange","age"), #,"bigCity"),
                                 times=c("03", "05","07"))


regressionData_070911 <- reshape(f070911_KVCC, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f070911_KVCC))),
                                              educ=c(grep("educ", colnames(f070911_KVCC))),
                                              employed=c(grep("^employed", colnames(f070911_KVCC))),
                                              exIncome=c(grep("exIncome", colnames(f070911_KVCC))),
                                              famSize=c(grep("famSize", colnames(f070911_KVCC))),
                                              kids=c(grep("kids[^Out]", colnames(f070911_KVCC))),
                                              kidsOut=c(grep("kidsOut", colnames(f070911_KVCC))),
                                              longWeight=c(grep("longWeight", colnames(f070911_KVCC))),
                                              other=c(grep("other", colnames(f070911_KVCC))), 
                                              logy=c(grep("logy",colnames(f070911_KVCC))),
                                              region=c(grep("region", colnames(f070911_KVCC))),
                                              retired=c(grep("retired",colnames(f070911_KVCC))),
                                              unemployed=c(grep("unemployed", colnames(f070911_KVCC))),
                                              white=c(grep("white", colnames(f070911_KVCC))),
                                              year=c(grep("year", colnames(f070911_KVCC))),
                                              yob=c(grep("yob",colnames(f070911_KVCC))),
                                              logCapChange=c(grep("logCapChange",colnames(f070911_KVCC))),
                                              age=c(grep("age",colnames(f070911_KVCC)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logCapChange","age"), #,"bigCity"),
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
# we first regress log income and log capChange expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log capChange d(cit) and
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


capChange_030507.lm = svyglm(logCapChange ~ year +
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
regressionData_030507$rescapChange <- capChange_030507.lm$residuals


capChange_070911.lm = svyglm(logCapChange ~ year +
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
regressionData_070911$rescapChange <-capChange_070911.lm$residuals


covData_030507 = regressionData_030507[ , c("uniqueID","quintile","year","rescapChange","resIncome")]
covData_070911 = regressionData_070911[ , c("uniqueID","quintile","year","rescapChange","resIncome")]

covData_030507 <- reshape(covData_030507,idvar="uniqueID",direction="wide",v.names=c("rescapChange","resIncome"),timevar="year")
covData_070911 <- reshape(covData_070911,idvar="uniqueID",direction="wide",v.names=c("rescapChange","resIncome"),timevar="year")

covData_030507$dst <- covData_030507$rescapChange.2005 - covData_030507$rescapChange.2003
covData_030507$dyt <- covData_030507$resIncome.2005 - covData_030507$resIncome.2003
covData_030507$dytplus1 <- covData_030507$resIncome.2007 - covData_030507$resIncome.2005


covData_070911$dst <- covData_070911$rescapChange.2009 - covData_070911$rescapChange.2007
covData_070911$dyt <- covData_070911$resIncome.2009 - covData_070911$resIncome.2007
covData_070911$dytplus1 <- covData_070911$resIncome.2011 - covData_070911$resIncome.2007

# MPS
print("MPS 030507")
for (i in c(1,2,3,4,5)){
  assign(paste0("MPS_030507_q",i),
         cov(subset(covData_030507$dst,covData_030507$quintile==i),subset(covData_030507$dytplus1,covData_030507$quintile==i))/
               cov(subset(covData_030507$dytplus1,covData_030507$quintile==i),subset(covData_030507$dyt,covData_030507$quintile==i))
  )
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPS_030507_q",i))))))
}
print("MPS 070911")
for (i in c(1,2,3,4,5)){
  assign(paste0("MPS_070911_q",i),
         cov(subset(covData_070911$dst,covData_070911$quintile==i),subset(covData_070911$dytplus1,covData_070911$quintile==i))/
               cov(subset(covData_070911$dytplus1,covData_070911$quintile==i),subset(covData_070911$dyt,covData_070911$quintile==i))
  )
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPS_070911_q",i))))))
}


rm(i, covData_030507,covData_070911,quintiles_03,quintiles_07,regressionData_030507,regressionData_070911,
   capChange_030507.lm, capChange_070911.lm, income_030507.lm, income_070911.lm, familyPanelSurvey030507, familyPanelSurvey070911)