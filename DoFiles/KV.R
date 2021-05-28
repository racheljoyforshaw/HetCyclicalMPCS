# Kaplan Violante
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2007,09,11

# sample selection

#We start with the PSID Core Sample and drop households with
#missing information on race, education, or state of residence, and those whose income
#grows more than 500 percent, falls by more than 80 percent, or is below $100. We drop
#households who have top-coded income or consumption. We also drop households that
#appear in the sample fewer than three consecutive times, because identification of the
#coefficients of interest requires a minimum of three periods. In our baseline calculations,
#we keep households where the head is 25-55 years old.

f01_keeps <-
  c("uniqueID",
    "income01")

f03_keeps <-
  c("uniqueID",
    "primarySamplingUnit",
    "longWeight03",
    "stratification",
    "income03",
    "educ03",
    "white03",
    "black03",
    "other03",
    "kidsOut03",
    "region03",
    "exIncome03",
    "kids03",
    "famSize03",
    "employed03",
    "unemployed03",
    "retired03",
    "totalWealth03",
    'consumption03',
    'age03'
  )

f05_keeps <-
  c("uniqueID",
    "longWeight05",
    "income05",
    "educ05",
    "white05",
    "black05",
    "other05",
    "kidsOut05",
    "region05",
    "exIncome05",
    "kids05",
    "famSize05",
    "employed05",
    "unemployed05",
    "retired05",
    "totalWealth05",
    'consumption05',
    'age05'
  )

f05extra_keeps <-
  c("uniqueID",
    "income05")

f07_keeps <-
  c("uniqueID",
    "longWeight07",
    "income07",
    "educ07",
    "white07",
    "black07",
    "other07",
    "kidsOut07",
    "region07",
    "exIncome07",
    "kids07",
    "famSize07",
    "employed07",
    "unemployed07",
    "retired07",
    "totalWealth07",
    'consumption07',
    'age07'
  )


f09_keeps <-
  c("uniqueID",
    "primarySamplingUnit",
    "stratification",
    "longWeight09",
    "income09",
    "educ09",
    "white09",
    "black09",
    "other09",
    "kidsOut09",
    "region09",
    "exIncome09",
    "kids09",
    "famSize09",
    "employed09",
    "unemployed09",
    "retired09",
    "totalWealth09",
    'consumption09',
    'age09'
  )

f11_keeps <-
  c("uniqueID",
    "longWeight11",
    "income11",
    "educ11",
    "white11",
    "black11",
    "other11",
    "kidsOut11",
    "region11",
    "exIncome11",
    "kids11",
    "famSize11",
    "employed11",
    "unemployed11",
    "retired11",
    "totalWealth11",
    'consumption11',
    'age11'
  )



f01_extras <- f01[,f01_keeps]
f03_regs <- f03[,f03_keeps]
f05_regs <- f05[,f05_keeps]
f05_extras <- f05[,f05extra_keeps]
f07_regs <- f07[,f07_keeps]
f09_regs <- f09[,f09_keeps]
f11_regs <- f11[,f11_keeps]

# SAMPLE SELECTION
# age - KV
f03_regs <- subset(f03_regs, f03_regs$age03<55&f03_regs$age03>25)
f05_regs <- subset(f05_regs, f05_regs$age05<55&f05_regs$age05>25)
f07_regs <- subset(f07_regs, f07_regs$age07<55&f07_regs$age07>25)
f09_regs <- subset(f09_regs, f09_regs$age09<55&f09_regs$age09>25)
f11_regs <- subset(f11_regs, f11_regs$age11<55&f11_regs$age11>25)



# need to merge the datasets over time by unique ID
f0507_regs <- merge(f05_regs,f07_regs, by=c('uniqueID'),all=FALSE)
f030507_regs <- merge(f03_regs,f0507_regs, by=c('uniqueID'),all=FALSE)
f030507_regs <-merge(f01_extras,f030507_regs, by=c('uniqueID'),all=FALSE) # need some extra bits

f0709_regs <- merge(f07_regs,f09_regs, by=c('uniqueID'),all=FALSE)
f070911_regs <- merge(f11_regs,f0709_regs, by=c('uniqueID'),all=FALSE)
f070911_regs <- merge(f05_extras,f070911_regs, by=c('uniqueID'),all=FALSE) # need some extra bits


rm(f0507_regs,f0709_regs,f05_regs, f07_regs, f03_regs, f09_regs, f11_regs, f01_extras, f05_extras)


# drop those whose income grows more than 500 percent, falls by more than 80 percent, or is below $100.
f030507_regs$incomeGrowth03 <- (f030507_regs$income03/f030507_regs$income01)-1
f030507_regs$incomeGrowth05 <- (f030507_regs$income05/f030507_regs$income03)-1
f030507_regs$incomeGrowth07 <- (f030507_regs$income07/f030507_regs$income05)-1

f070911_regs$incomeGrowth07 <- (f070911_regs$income07/f070911_regs$income05)-1
f070911_regs$incomeGrowth09 <- (f070911_regs$income09/f070911_regs$income07)-1
f070911_regs$incomeGrowth11 <- (f070911_regs$income11/f070911_regs$income09)-1

f030507_regs$income03 <- ifelse(f030507_regs$incomeGrowth03>5 | f030507_regs$incomeGrowth03< -0.8| f030507_regs$income03<100,NA,f030507_regs$income03)
f030507_regs$income05 <- ifelse(f030507_regs$incomeGrowth05>5 | f030507_regs$incomeGrowth05< -0.8| f030507_regs$income05<100,NA,f030507_regs$income05)
f030507_regs$income07 <- ifelse(f030507_regs$incomeGrowth07>5 | f030507_regs$incomeGrowth07< -0.8| f030507_regs$income07<100,NA,f030507_regs$income07)

f070911_regs$income07 <- ifelse(f070911_regs$incomeGrowth07>5 | f070911_regs$incomeGrowth07< -0.8| f070911_regs$income07<100,NA,f070911_regs$income07)
f070911_regs$income09 <- ifelse(f070911_regs$incomeGrowth09>5 | f070911_regs$incomeGrowth09< -0.8| f070911_regs$income09<100,NA,f070911_regs$income09)
f070911_regs$income11 <- ifelse(f070911_regs$incomeGrowth11>5 | f070911_regs$incomeGrowth11< -0.8| f070911_regs$income11<100,NA,f070911_regs$income11)


# money income - drop if <100 (from KV) or if food expenditure >income or housing expenditure>income
#f030507_regs <- subset(f030507_regs,f030507_regs$income03>100) # |f03$income03>f03$food03 & f03$income03>f03$housing03)
#f030507_regs <- subset(f030507_regs,f030507_regs$income05>100) # |f05$income05>f05$food05 & f05$income05>f05$housing05)
#f030507_regs <- subset(f030507_regs,f030507_regs$income07>100) # |f07$income07>f07$food07 & f07$income07>f07$housing07)
#f09_regs <- subset(f09_regs,f09_regs$income09>100) # |f09$income09>f09$food09 & f09$income09>f09$housing09)
#f11_regs <- subset(f11_regs,f11_regs$income11>100) # |f11$income11>f11$food11 & f11$income11>f11$housing11)


f030507_regs$logy03 <- log(f030507_regs$income03)
f030507_regs$logy05 <- log(f030507_regs$income05)
f030507_regs$logy07 <- log(f030507_regs$income07)

f070911_regs$logy07 <- log(f070911_regs$income07)
f070911_regs$logy09 <- log(f070911_regs$income09)
f070911_regs$logy11 <- log(f070911_regs$income11)

f030507_regs <- select(f030507_regs,-c("incomeGrowth03","incomeGrowth05","incomeGrowth07","income01","income05","income07"))
f070911_regs <- select(f070911_regs,-c("incomeGrowth07","incomeGrowth09","incomeGrowth11","income05","income09","income11"))


# log consumption
f030507_regs$logConsumption03 <- log(f030507_regs$consumption03)
f030507_regs$logConsumption05 <- log(f030507_regs$consumption05)
f030507_regs$logConsumption07 <- log(f030507_regs$consumption07)

f070911_regs$logConsumption07 <- log(f070911_regs$consumption07)
f070911_regs$logConsumption09 <- log(f070911_regs$consumption09)
f070911_regs$logConsumption11 <- log(f070911_regs$consumption11)


# create year variable
f030507_regs$year03 <- 2003
f030507_regs$year05 <- 2005
f030507_regs$year07 <- 2007
f070911_regs$year07 <- 2007
f070911_regs$year09 <- 2009
f070911_regs$year11 <- 2011

# year of birth

f030507_regs$yob03 <- f030507_regs$year03 - f030507_regs$age03 
f030507_regs$yob05 <- f030507_regs$year05 - f030507_regs$age05 
f030507_regs$yob07 <- f030507_regs$year07 - f030507_regs$age07

f070911_regs$yob07 <- f070911_regs$year07 - f070911_regs$age07 
f070911_regs$yob09 <- f070911_regs$year09 - f070911_regs$age09 
f070911_regs$yob11 <- f070911_regs$year11 - f070911_regs$age11



# drop NA  (households with missing information on race, education, or state of residence)
f030507_regs <- na.omit(f030507_regs)
f070911_regs <- na.omit(f070911_regs)


# calculate quintiles

# quintiles_03 <- quantile(f030507_regs$totalWealth03, probs=seq(0, 1, 0.2))
# quintiles_07 <- quantile(f070911_regs$totalWealth07, probs=seq(0, 1, 0.2))
# 
# 
# f030507_regs$quintile <- ifelse(f030507_regs$totalWealth03<quintiles_03[2],1,
#                                 ifelse(f030507_regs$totalWealth03>=quintiles_03[2] & f030507_regs$totalWealth03<quintiles_03[3],2,
#                                        ifelse(f030507_regs$totalWealth03>=quintiles_03[3] & f030507_regs$totalWealth03<quintiles_03[4],3,
#                                               ifelse(f030507_regs$totalWealth03>=quintiles_03[4]& f030507_regs$totalWealth03<quintiles_03[5],4,
#                                                      ifelse(f030507_regs$totalWealth03>=quintiles_03[5],5,NA)))))
# 
# f070911_regs$quintile <- ifelse(f070911_regs$totalWealth07<quintiles_07[2],1,
#                                 ifelse(f070911_regs$totalWealth07>=quintiles_07[2] & f070911_regs$totalWealth07<quintiles_07[3],2,
#                                        ifelse(f070911_regs$totalWealth07>=quintiles_07[3] & f070911_regs$totalWealth07<quintiles_07[4],3,
#                                               ifelse(f070911_regs$totalWealth07>=quintiles_07[4]& f070911_regs$totalWealth07<quintiles_07[5],4,
#                                                      ifelse(f070911_regs$totalWealth07>=quintiles_07[5],5,NA)))))



quintiles_03 <- quantile(f030507_regs$income03, probs=seq(0, 1, 0.2))
quintiles_07 <- quantile(f070911_regs$income07, probs=seq(0, 1, 0.2))


f030507_regs$quintile <- ifelse(f030507_regs$income03<quintiles_03[2],1,
                                ifelse(f030507_regs$income03>=quintiles_03[2] & f030507_regs$income03<quintiles_03[3],2,
                                       ifelse(f030507_regs$income03>=quintiles_03[3] & f030507_regs$income03<quintiles_03[4],3,
                                              ifelse(f030507_regs$income03>=quintiles_03[4]& f030507_regs$income03<quintiles_03[5],4,
                                                     ifelse(f030507_regs$income03>=quintiles_03[5],5,NA)))))

f070911_regs$quintile <- ifelse(f070911_regs$income07<quintiles_07[2],1,
                                ifelse(f070911_regs$income07>=quintiles_07[2] & f070911_regs$income07<quintiles_07[3],2,
                                       ifelse(f070911_regs$income07>=quintiles_07[3] & f070911_regs$income07<quintiles_07[4],3,
                                              ifelse(f070911_regs$income07>=quintiles_07[4]& f070911_regs$income07<quintiles_07[5],4,
                                                     ifelse(f070911_regs$income07>=quintiles_07[5],5,NA)))))

f030507_regs_keeps <- c("uniqueID","primarySamplingUnit","stratification","black03","black05","black07",
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
                      "age03","age05","age07","quintile")

f030507_regs <- f030507_regs[,f030507_regs_keeps]

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
                      "logConsumption07","logConsumption09","logConsumption11",
                      "age07","age09","age11",'quintile')

f070911_regs <- f070911_regs[,f070911_keeps]





# wide to long

regressionData_030507 <- reshape(f030507_regs, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f030507_regs))),
                                              educ=c(grep("educ", colnames(f030507_regs))),
                                              employed=c(grep("^employed", colnames(f030507_regs))),
                                              exIncome=c(grep("exIncome", colnames(f030507_regs))),
                                              famSize=c(grep("famSize", colnames(f030507_regs))),
                                              kids=c(grep("kids[^Out]", colnames(f030507_regs))),
                                              kidsOut=c(grep("kidsOut", colnames(f030507_regs))),
                                              longWeight=c(grep("longWeight", colnames(f030507_regs))),
                                              other=c(grep("other", colnames(f030507_regs))), 
                                              logy=c(grep("logy",colnames(f030507_regs))),
                                              region=c(grep("region", colnames(f030507_regs))),
                                              retired=c(grep("retired",colnames(f030507_regs))),
                                              unemployed=c(grep("unemployed", colnames(f030507_regs))),
                                              white=c(grep("white", colnames(f030507_regs))),
                                              year=c(grep("year", colnames(f030507_regs))),
                                              yob=c(grep("yob",colnames(f030507_regs))),
                                              logConsumption=c(grep("logConsumption",colnames(f030507_regs)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logConsumption"), #,"bigCity"),
                                 times=c("03", "05","07"))


regressionData_070911 <- reshape(f070911_regs, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(black=c(grep("black", colnames(f070911_regs))),
                                              educ=c(grep("educ", colnames(f070911_regs))),
                                              employed=c(grep("^employed", colnames(f070911_regs))),
                                              exIncome=c(grep("exIncome", colnames(f070911_regs))),
                                              famSize=c(grep("famSize", colnames(f070911_regs))),
                                              kids=c(grep("kids[^Out]", colnames(f070911_regs))),
                                              kidsOut=c(grep("kidsOut", colnames(f070911_regs))),
                                              longWeight=c(grep("longWeight", colnames(f070911_regs))),
                                              other=c(grep("other", colnames(f070911_regs))), 
                                              logy=c(grep("logy",colnames(f070911_regs))),
                                              region=c(grep("region", colnames(f070911_regs))),
                                              retired=c(grep("retired",colnames(f070911_regs))),
                                              unemployed=c(grep("unemployed", colnames(f070911_regs))),
                                              white=c(grep("white", colnames(f070911_regs))),
                                              year=c(grep("year", colnames(f070911_regs))),
                                              yob=c(grep("yob",colnames(f070911_regs))),
                                              logConsumption=c(grep("logConsumption",colnames(f070911_regs)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","logConsumption"),#,"bigCity")
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
#regressionData$bigCity <- factor(regressionData$bigCity)  ## this is all zeros
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
#regressionData$bigCity <- factor(regressionData$bigCity)  ## this is all zeros
regressionData_070911$kidsOut <- factor(regressionData_070911$kidsOut)







# do the regressions
# we first regress log income and log consumption expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log consumption d(cit) and
#log income d(yit).
income_030507.lm = lm(logy ~ year +
                        yob +
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
                      , data=regressionData_030507) 
regressionData_030507$resIncome <-resid(income_030507.lm)

income_070911.lm = lm(logy ~ year +
                        yob +
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
                      , data=regressionData_070911) 
regressionData_070911$resIncome <-resid(income_070911.lm)


activeSaving_030507.lm = lm(logConsumption ~ year +
                              yob +
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
                            , data=regressionData_030507) 
regressionData_030507$resAS <- resid(activeSaving_030507.lm)


activeSaving_070911.lm = lm(logConsumption ~ year +
                              yob +
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
                            data=regressionData_070911) 
regressionData_070911$resAS <-resid(activeSaving_070911.lm)



covData_030507 = regressionData_030507[ , c("uniqueID","quintile","year","resAS","resIncome")]
covData_070911 = regressionData_070911[ , c("uniqueID","quintile","year","resAS","resIncome")]

covData_030507 <- reshape(covData_030507,idvar="uniqueID",direction="wide",v.names=c("resAS","resIncome"),timevar="year")
covData_070911 <- reshape(covData_070911,idvar="uniqueID",direction="wide",v.names=c("resAS","resIncome"),timevar="year")

covData_030507$dst <- covData_030507$resAS.2005 - covData_030507$resAS.2003
covData_030507$dyt <- covData_030507$resIncome.2005 - covData_030507$resIncome.2003
covData_030507$dytplus1 <- covData_030507$resIncome.2007 - covData_030507$resIncome.2005


covData_070911$dst <- covData_070911$resAS.2009 - covData_070911$resAS.2007
covData_070911$dyt <- covData_070911$resIncome.2009 - covData_070911$resIncome.2007
covData_070911$dytplus1 <- covData_070911$resIncome.2011 - covData_070911$resIncome.2007


for (i in c(1,2,3,4,5)){
  assign(paste0("MPC_030507_q",i),
         cov(subset(covData_030507$dst,covData_030507$quintile==i),subset(covData_030507$dytplus1,covData_030507$quintile==i))/
           cov(subset(covData_030507$dyt,covData_030507$quintile==i),subset(covData_030507$dytplus1,covData_030507$quintile==i)) 
  )
  print(eval(as.name(paste0("MPC_030507_q",i))))
}

for (i in c(1,2,3,4,5)){
  assign(paste0("MPC_070911_q",i),
         cov(subset(covData_070911$dst,covData_070911$quintile==i),subset(covData_070911$dytplus1,covData_070911$quintile==i))/
           cov(subset(covData_070911$dyt,covData_070911$quintile==i),subset(covData_070911$dytplus1,covData_070911$quintile==i))
  )
  print(eval(as.name(paste0("MPC_070911_q",i))))
}




#print(cov(covData_030507$dst,covData_030507$dytplus1)/
#        var(covData_030507$dyt))

#print(cov(covData_070911$dst,covData_070911$dytplus1)/
#        var(covData_070911$dyt))


