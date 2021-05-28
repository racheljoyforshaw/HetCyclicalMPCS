# BPP/ Kaplan Violante
# need min of 3 periods for identification, so pre-recession sample is 2003,5,7; post-recession is 2007,09,11

# sample selection

#We start with the PSID Core Sample and drop households with
#missing information on race, education, or state of residence, and those whose income
#grows more than 500 percent, falls by more than 80 percent, or is below $100. We drop
#households who have top-coded income or consumption. We also drop households that
#appear in the sample fewer than three consecutive times, because identification of the
#coefficients of interest requires a minimum of three periods. In our baseline calculations,
#we keep households where the head is 25-55 years old.

f030507_keeps <-
  c("uniqueID",
    "primarySamplingUnit",
    "longWeight03",
    "longWeight05",
    "longWeight07",
    "stratification",
    "activeSaving03",
    "activeSaving05",
    "activeSaving07",
    "income01",
    "income03",
    "income05",
    "income07",
    "educ03",
    "educ05",
    "educ07",
    "white03",
    "white05",
    "white07",
    "black03",
    "black05",
    "black07",
    "other03",
    "other05",
    "other07",
    "kidsOut03",
    "kidsOut05",
    "kidsOut07",
    "region03",
    "region05",
    "region07",
    "year03",
    "year05",
    "year07",
    "exIncome03",
    "exIncome05",
    "exIncome07",
#    "bigCity03",
#    "bigCity05",
#    "bigCity07",
    "kids03",
    "kids05",
    "kids07",
    "famSize03",
    "famSize05",
    "famSize07",
    "employed03",
    "employed05",
    "employed07",
    "unemployed03",
    "unemployed05",
    "unemployed07",
    "retired03",
    "retired05",
    "retired07",
    "logy03",
    "logy05",
    "logy07",
    "yob03",
    "yob05",
    "yob07",
    "dtotalWealth03",
    "dtotalWealth05",
    "dtotalWealth07",
    "totalWealth03"
#    'impConsumption03',
#    'impConsumption05',
#    'impConsumption07'
  )

f070911_keeps <-
  c("uniqueID",
    "primarySamplingUnit",
    "longWeight07",
    "longWeight09",
    "longWeight11",
    "stratification",
    "activeSaving07",
    "activeSaving09",
    "activeSaving11",
    "income05",
    "income07",
    "income09",
    "income11",
    "educ07",
    "educ09",
    "educ11",
    "white07",
    "white09",
    "white11",
    "black07",
    "black09",
    "black11",
    "other07",
    "other09",
    "other11",
    "kidsOut07",
    "kidsOut09",
    "kidsOut11",
    "region07",
    "region09",
    "region11",
    "year07",
    "year09",
    "year11",
    "exIncome07",
    "exIncome09",
    "exIncome11",
#    "bigCity07",
#    "bigCity09",
#    "bigCity11",
    "kids07",
    "kids09",
    "kids11",
    "famSize07",
    "famSize09",
    "famSize11",
    "employed07",
    "employed09",
    "employed11",
    "unemployed07",
    "unemployed09",
    "unemployed11",
    "retired07",
    "retired09",
    "retired11",
    "logy07",
    "logy09",
    "logy11",
    "yob07",
    "yob09",
    "yob11",
    "dtotalWealth07",
    "dtotalWealth09",
    "dtotalWealth11",
    "totalWealth07"
#    'impConsumption07',
#    'impConsumption09',
#    'impConsumption11'
  )


f030507_regs <- f030507[ , f030507_keeps ]
f070911_regs <- f070911[ , f070911_keeps ]

# drop those whose income grows more than 500 percent, falls by more than 80 percent, or is below $100.
f030507_regs$incomeGrowth03 <- (f030507_regs$income03/f030507_regs$income01)-1
f030507_regs$incomeGrowth05 <- (f030507_regs$income05/f030507_regs$income03)-1
f030507_regs$incomeGrowth07 <- (f030507_regs$income07/f030507_regs$income05)-1

f030507_regs$income03 <- ifelse(f030507_regs$incomeGrowth03>5 | f030507_regs$incomeGrowth03< -0.8| f030507_regs$income03<100,NA,f030507_regs$incomeGrowth03)
f030507_regs$income05 <- ifelse(f030507_regs$incomeGrowth05>5 | f030507_regs$incomeGrowth05< -0.8| f030507_regs$income05<100,NA,f030507_regs$incomeGrowth05)
f030507_regs$income07 <- ifelse(f030507_regs$incomeGrowth07>5 | f030507_regs$incomeGrowth07< -0.8| f030507_regs$income07<100,NA,f030507_regs$incomeGrowth07)

f030507_regs <- select(f030507_regs,-c("incomeGrowth03","incomeGrowth05","incomeGrowth07","income01"))


f070911_regs$incomeGrowth07 <- (f070911_regs$income07/f070911_regs$income05)-1
f070911_regs$incomeGrowth09 <- (f070911_regs$income09/f070911_regs$income07)-1
f070911_regs$incomeGrowth11 <- (f070911_regs$income11/f070911_regs$income09)-1

f070911_regs$income07 <- ifelse(f070911_regs$incomeGrowth07>5 | f070911_regs$incomeGrowth07< -0.8| f070911_regs$income07<100,NA,f070911_regs$incomeGrowth07)
f070911_regs$income09 <- ifelse(f070911_regs$incomeGrowth09>5 | f070911_regs$incomeGrowth09< -0.8| f070911_regs$income09<100,NA,f070911_regs$incomeGrowth09)
f070911_regs$income11 <- ifelse(f070911_regs$incomeGrowth11>5 | f070911_regs$incomeGrowth11< -0.8| f070911_regs$income11<100,NA,f070911_regs$incomeGrowth11)

f070911_regs <- select(f070911_regs,-c("incomeGrowth07","incomeGrowth09","incomeGrowth11","income05"))

# shift active savings into positive (this doesn't affect changes (when evaluated at quintiles), means I can log)
scalar_030507 <-  abs(min(min(f030507_regs$activeSaving03,na.rm=TRUE),min(f030507_regs$activeSaving05,na.rm=TRUE),min(f030507_regs$activeSaving07,na.rm=TRUE))) + 1
scalar_070911 <- abs(min(min(f070911_regs$activeSaving07,na.rm=TRUE),min(f070911_regs$activeSaving09,na.rm=TRUE),min(f070911_regs$activeSaving11,na.rm=TRUE))) + 1

#f030507_regs <- f030507_regs[which(f030507_regs$activeSaving03 >0 & f030507_regs$activeSaving05 >0  & f030507_regs$activeSaving07 >0),]
#f070911_regs <- f070911_regs[which(f070911_regs$activeSaving07 >0 & f070911_regs$activeSaving09 >0  & f070911_regs$activeSaving11 >0),]


f030507_regs$logAS03 <- log(f030507_regs$activeSaving03 + scalar_030507)
f030507_regs$logAS05 <- log(f030507_regs$activeSaving05 + scalar_030507)
f030507_regs$logAS07 <- log(f030507_regs$activeSaving07 + scalar_030507)

f070911_regs$logAS07 <- log(f070911_regs$activeSaving07 + scalar_070911)
f070911_regs$logAS09 <- log(f070911_regs$activeSaving09 + scalar_070911)
f070911_regs$logAS11 <- log(f070911_regs$activeSaving11 + scalar_070911)

f030507_regs <- select(f030507_regs,-c("activeSaving03","activeSaving05","activeSaving07"))
f070911_regs <- select(f070911_regs,-c("activeSaving07","activeSaving09","activeSaving11"))

# shift dtotalWealth into positive (this doesn't affect changes (when evaluated at quintiles), means I can log)
k_scalar_030507 <- abs(min(min(f030507_regs$dtotalWealth03,na.rm=TRUE),min(f030507_regs$dtotalWealth05,na.rm=TRUE),min(f030507_regs$dtotalWealth07,na.rm=TRUE))) + 1
k_scalar_070911 <- abs(min(min(f070911_regs$dtotalWealth07,na.rm=TRUE),min(f070911_regs$dtotalWealth09,na.rm=TRUE),min(f070911_regs$dtotalWealth11,na.rm=TRUE))) + 1

f030507_regs$dtotalWealth03 <- log(f030507_regs$dtotalWealth03 + k_scalar_030507)
f030507_regs$dtotalWealth05 <- log(f030507_regs$dtotalWealth05 + k_scalar_030507)
f030507_regs$dtotalWealth07 <- log(f030507_regs$dtotalWealth07 + k_scalar_030507)

f070911_regs$dtotalWealth07 <- log(f070911_regs$dtotalWealth07 + k_scalar_070911)
f070911_regs$dtotalWealth09 <- log(f070911_regs$dtotalWealth09 + k_scalar_070911)
f070911_regs$dtotalWealth11 <- log(f070911_regs$dtotalWealth11 + k_scalar_070911)




# drop NA  (households with missing information on race, education, or state of residence)
f030507_regs <- na.omit(f030507_regs)
f070911_regs <- na.omit(f070911_regs)

# calculate quintiles
quintiles_03 <- quantile(f030507_regs$totalWealth03, probs=seq(0, 1, 0.2))
quintiles_07 <- quantile(f070911_regs$totalWealth07, probs=seq(0, 1, 0.2))


f030507_regs$quintile <- ifelse(f030507_regs$totalWealth03<quintiles_03[2],1,
                                ifelse(f030507_regs$totalWealth03>=quintiles_03[2] & f030507_regs$totalWealth03<quintiles_03[3],2,
                                       ifelse(f030507_regs$totalWealth03>=quintiles_03[3] & f030507_regs$totalWealth03<quintiles_03[4],3,
                                              ifelse(f030507_regs$totalWealth03>=quintiles_03[4]& f030507_regs$totalWealth03<quintiles_03[5],4,
                                                     ifelse(f030507_regs$totalWealth03>=quintiles_03[5],5,NA)))))

f070911_regs$quintile <- ifelse(f070911_regs$totalWealth07<quintiles_07[2],1,
                                ifelse(f070911_regs$totalWealth07>=quintiles_07[2] & f070911_regs$totalWealth07<quintiles_07[3],2,
                                       ifelse(f070911_regs$totalWealth07>=quintiles_07[3] & f070911_regs$totalWealth07<quintiles_07[4],3,
                                              ifelse(f070911_regs$totalWealth07>=quintiles_07[4]& f070911_regs$totalWealth07<quintiles_07[5],4,
                                                     ifelse(f070911_regs$totalWealth07>=quintiles_07[5],5,NA)))))

f030507_regs <- select(f030507_regs,-c("totalWealth03"))
f070911_regs <- select(f070911_regs,-c("totalWealth07"))


# wide to long

regressionData_030507 <- reshape(f030507_regs, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
        varying=list(logAS=c(grep("logAS", colnames(f030507_regs))),
                     #bigCity=c(grep("bigCity", colnames(f030507_regs))),
                     black=c(grep("black", colnames(f030507_regs))),
                     educ=c(grep("educ", colnames(f030507_regs))),
                     employed=c(grep("^employed", colnames(f030507_regs))),
                     exIncome=c(grep("exIncome", colnames(f030507_regs))),
                     famSize=c(grep("famSize", colnames(f030507_regs))),
                     income=c(grep("income", colnames(f030507_regs))),
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
                     dtotalWealth=c(grep("dtotalWealth",colnames(f030507_regs)))),
                     #impConsumption=c(grep("impConsumption",colnames(f030507_regs)))),
        v.names = c("logAS","black","educ","employed",
                    "exIncome","famSize","income","kids","kidsOut","longWeight",
                    "other","logy","region","retired","unemployed","white","year","yob","dtotalWealth"),#,"impConsumption","bigCity"),
        times=c("03", "05","07"))


regressionData_070911 <- reshape(f070911_regs, idvar=c("uniqueID","primarySamplingUnit","stratification"), direction="long", 
                                 varying=list(logAS=c(grep("logAS", colnames(f070911_regs))),
                                              #bigCity=c(grep("bigCity", colnames(f070911_regs))),
                                              black=c(grep("black", colnames(f070911_regs))),
                                              educ=c(grep("educ", colnames(f070911_regs))),
                                              employed=c(grep("^employed", colnames(f070911_regs))),
                                              exIncome=c(grep("exIncome", colnames(f070911_regs))),
                                              famSize=c(grep("famSize", colnames(f070911_regs))),
                                              income=c(grep("income", colnames(f070911_regs))),
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
                                              dtotalWealth=c(grep("dtotalWealth",colnames(f070911_regs)))),
                                              #impConsumption=c(grep("impConsumption",colnames(f070911_regs)))),
                                 v.names = c("logAS","black","educ","employed",
                                             "exIncome","famSize","income","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","yob","dtotalWealth"),#,"bigCity","impConsumption"),
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


activeSaving_030507.lm = lm(logAS ~ year +
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


activeSaving_070911.lm = lm(logAS ~ year +
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
  assign(paste0("MPS_030507_q",i),
         cov(subset(covData_030507$dst,covData_030507$quintile==i),subset(covData_030507$dytplus1,covData_030507$quintile==i))/
           var(subset(covData_030507$dyt,covData_030507$quintile==i)) #*(quintiles_03[i+1]/(quintiles_03[i+1]+scalar_030507+1))
         )
  print(eval(as.name(paste0("MPS_030507_q",i))))
  }

for (i in c(1,2,3,4,5)){
  assign(paste0("MPS_070911_q",i),
         cov(subset(covData_070911$dst,covData_070911$quintile==i),subset(covData_070911$dytplus1,covData_070911$quintile==i))/
          var(subset(covData_070911$dyt,covData_070911$quintile==i))
         )
  print(eval(as.name(paste0("MPS_070911_q",i))))
  }




print(cov(covData_030507$dst,covData_030507$dytplus1)/
  var(covData_030507$dyt))

print(cov(covData_070911$dst,covData_070911$dytplus1)/
        var(covData_070911$dyt))


