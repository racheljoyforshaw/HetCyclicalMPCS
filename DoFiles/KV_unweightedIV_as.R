# Kaplan Violante - active saving
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

# move as into positive
min030507 <- min(min(f030507_KVAS$activeSaving03,na.rm=TRUE),min(f030507_KVAS$activeSaving05,na.rm=TRUE),min(f030507_KVAS$activeSaving07,na.rm=TRUE))
min091113 <- min(min(f091113_KVAS$activeSaving09,na.rm=TRUE),min(f091113_KVAS$activeSaving11,na.rm=TRUE),min(f091113_KVAS$activeSaving13,na.rm=TRUE))

# log activeSaving
f030507_KVAS$logactiveSaving03 <- log(f030507_KVAS$activeSaving03 + abs(min030507) +1)
f030507_KVAS$logactiveSaving05 <- log(f030507_KVAS$activeSaving05 + abs(min030507) +1)
f030507_KVAS$logactiveSaving07 <- log(f030507_KVAS$activeSaving07 + abs(min030507) +1)

f091113_KVAS$logactiveSaving09 <- log(f091113_KVAS$activeSaving09 + abs(min091113) + 1)
f091113_KVAS$logactiveSaving11 <- log(f091113_KVAS$activeSaving11 + abs(min091113) + 1)
f091113_KVAS$logactiveSaving13 <- log(f091113_KVAS$activeSaving13 + abs(min091113) + 1)


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
                      "logactiveSaving03","logactiveSaving05","logactiveSaving07",
                      "age03","age05","age07",
                      "income03",
                      "poorHTM03","poorHTM05","poorHTM07",
                      "richHTM03","richHTM05","richHTM07")

f030507_KVAS <- f030507_KVAS[,f030507_KVAS_keeps]

f091113_KVAS_keeps  <- c("uniqueID","primarySamplingUnit","stratification","black09","black11","black13",
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
                       "income09",
                       "poorHTM09","poorHTM11","poorHTM13",
                       "richHTM09","richHTM11","richHTM13")

f091113_KVAS <- f091113_KVAS[,f091113_KVAS_keeps]

rm(f091113_KVAS_keeps,f030507_KVAS_keeps)

# drop NA 
#f030507_KVAS <- f030507_KVAS[complete.cases(f030507_KVAS[, c("logy03","logy05","logy07","logactiveSaving03","logactiveSaving05","logactiveSaving07")]),]
#f091113_KVAS <- f091113_KVAS[complete.cases(f091113_KVAS[, c("logy09","logy11","logy13","logactiveSaving09","logactiveSaving11","logactiveSaving13")]),]
f030507_KVAS <-na.omit(f030507_KVAS)
f091113_KVAS <-na.omit(f091113_KVAS)
# quintiles

# survey design

quintiles_03 <- quantile(f030507_KVAS$income03, seq(0, 1, 0.2),na.rm=TRUE)
quintiles_09 <- quantile(f091113_KVAS$income09, seq(0, 1, 0.2),na.rm=TRUE)


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




# drop income

f030507_KVAS <- f030507_KVAS[,!(names(f030507_KVAS) %in% "income03")]
f091113_KVAS <- f091113_KVAS[,!(names(f091113_KVAS) %in% "income09")]


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
                                              logactiveSaving=c(grep("logactiveSaving",colnames(f030507_KVAS))),
                                              age=c(grep("age",colnames(f030507_KVAS))),
                                              poorHTM=c(grep("poorHTM",colnames(f030507_KVAS))),
                                              richHTM=c(grep("richHTM",colnames(f030507_KVAS)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","logactiveSaving","age","poorHTM","richHTM"), #,"bigCity"),
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
                                              logactiveSaving=c(grep("logactiveSaving",colnames(f091113_KVAS))),
                                              age=c(grep("age",colnames(f091113_KVAS))),
                                              poorHTM=c(grep("poorHTM",colnames(f091113_KVAS))),
                                              richHTM=c(grep("richHTM",colnames(f091113_KVAS)))),
                                 v.names = c("black","educ","employed",
                                             "exIncome","famSize","kids","kidsOut","longWeight",
                                             "other","logy","region","retired","unemployed","white","year","logactiveSaving","age","poorHTM","richHTM"), #,"bigCity"),
                                 times=c("09", "11","13"))

regressionData_030507$yob <- regressionData_030507$year - regressionData_030507$age
regressionData_091113$yob <- regressionData_091113$year - regressionData_091113$age

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
regressionData_030507$poorHTM <- factor(regressionData_030507$poorHTM)
regressionData_030507$richHTM <- factor(regressionData_030507$richHTM)

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
regressionData_091113$poorHTM <- factor(regressionData_091113$poorHTM)
regressionData_091113$richHTM <- factor(regressionData_091113$richHTM)








# do the regressions
# we first regress log income and log activeSaving expenditures on
#year and cohort dummies, education, race, family structure, employment, geographic
#variables, and interactions of year dummies with education, race, employment, and
#region. We then construct the first-differenced residuals of log activeSaving d(cit) and
#log income d(yit).

income_as_030507.lm = lm(logy ~ year +
                        yob +
                        educ +
                        white +
                        black +
                        #other +
                        #famSize + 
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
                        #other*year +
                        employed*year + 
                        unemployed*year +
                        retired*year +
                        region*year
                      ,regressionData_030507) 
regressionData_030507 <- regressionData_030507 %>% add_residuals(income_as_030507.lm)	
names(regressionData_030507)[names(regressionData_030507)=="resid"] <-"resIncome"



income_as_091113.lm = lm(logy ~ year +
                        yob +
                        educ +
                        white +
                        black +
                        #other +
                        # famSize + 
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
                        #other*year +
                        employed*year + 
                        unemployed*year +
                        retired*year +
                        region*year
                      , regressionData_091113) 

regressionData_091113 <- regressionData_091113 %>% add_residuals(income_as_091113.lm)	
names(regressionData_091113)[names(regressionData_091113)=="resid"] <-"resIncome"

activeSaving_030507.lm = lm(logactiveSaving ~ year +
                             yob +
                             educ +
                             white +
                             black +
                             #other +
                             #famSize + 
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
                             #other*year +
                             employed*year + 
                             unemployed*year +
                             retired*year +
                             region*year
                           , regressionData_030507) 
regressionData_030507 <- regressionData_030507 %>% add_residuals(activeSaving_030507.lm)	
names(regressionData_030507)[names(regressionData_030507)=="resid"] <-"resactiveSaving"


activeSaving_091113.lm = lm(logactiveSaving ~ year +
                             yob +
                             educ +
                             white +
                             black +
                             #other +
                             #famSize + 
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
                             #other*year +
                             employed*year + 
                             unemployed*year +
                             retired*year +
                             region*year,
                           regressionData_091113) 
regressionData_091113 <- regressionData_091113 %>% add_residuals(activeSaving_091113.lm)	
names(regressionData_091113)[names(regressionData_091113)=="resid"] <-"resactiveSaving"


covData_as_030507 = regressionData_030507[ , c("uniqueID","primarySamplingUnit","stratification","longWeight", "quintile","year","resactiveSaving","resIncome")]
covData_as_091113 = regressionData_091113[ , c("uniqueID","primarySamplingUnit","stratification","longWeight","quintile","year","resactiveSaving","resIncome")]

covData_as_030507 <- reshape(covData_as_030507,idvar="uniqueID",direction="wide",v.names=c("resactiveSaving","resIncome","longWeight"),timevar="year")
covData_as_091113 <- reshape(covData_as_091113,idvar="uniqueID",direction="wide",v.names=c("resactiveSaving","resIncome","longWeight"),timevar="year")

covData_as_030507$dst <- covData_as_030507$resactiveSaving.2005 - covData_as_030507$resactiveSaving.2003
covData_as_030507$dyt <- covData_as_030507$resIncome.2005 - covData_as_030507$resIncome.2003
covData_as_030507$dytplus1 <- covData_as_030507$resIncome.2007 - covData_as_030507$resIncome.2005


covData_as_091113$dst <- covData_as_091113$resactiveSaving.2011 - covData_as_091113$resactiveSaving.2009
covData_as_091113$dyt <- covData_as_091113$resIncome.2011 - covData_as_091113$resIncome.2009
covData_as_091113$dytplus1 <- covData_as_091113$resIncome.2013 - covData_as_091113$resIncome.2011

library("AER")
library("ivpack")
# MPS
print("MPS as 07")
for (i in c(1,2,3,4,5)){
  mdl <- ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=covData_as_030507, subset=quintile==i)
  assign(paste0("MPS_030507_q",i),
         mdl$coefficients[2]
  )
  temp <- anderson.rubin.ci(mdl)
  assign(paste0("MPS_CI_low_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPS_CI_up_07_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPS_030507_q",i))))))
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPS_CI_low_07_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPS_CI_up_07_q",i))))))
}

# MPS
print("MPS as 13")
for (i in c(1,2,3,4,5)){
  mdl = ivreg(dst ~ dyt, ~ dytplus1, x=TRUE, data=covData_as_091113, subset=quintile==i)
  assign(paste0("MPS_091113_q",i),
         mdl$coefficients[2]
  )
  temp <- anderson.rubin.ci(mdl)
  assign(paste0("MPS_CI_low_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[1],3,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[1]))))
  assign(paste0("MPS_CI_up_13_q",i),
         as.numeric(substr(unlist(strsplit(temp$confidence.interval, split=" , "))[2],1,nchar(unlist(strsplit(temp$confidence.interval, split=" , "))[2])-3)))
  print(paste0("q",i,":",unname(eval(as.name(paste0("MPS_091113_q",i))))))
  print(paste0("CI low q",i,":",unname(eval(as.name(paste0("MPS_CI_low_13_q",i))))))
  print(paste0("CI high q",i,":",unname(eval(as.name(paste0("MPS_CI_up_13_q",i))))))
}

# MPS

out_MPS_KVAS <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                         "year" = c("2007","2007","2007","2007","2007","2013","2013","2013","2013","2013"),
                         "MPS" = c(MPS_030507_q1,
                                   MPS_030507_q2,
                                   MPS_030507_q3,
                                   MPS_030507_q4,
                                   MPS_030507_q5,
                                   MPS_091113_q1,
                                   MPS_091113_q2,
                                   MPS_091113_q3,
                                   MPS_091113_q4,
                                   MPS_091113_q5),
                         "MPS_CI_lower" = c(MPS_CI_low_07_q1,
                                            MPS_CI_low_07_q2,
                                            MPS_CI_low_07_q3,
                                            MPS_CI_low_07_q4,
                                            MPS_CI_low_07_q5,
                                            MPS_CI_low_13_q1,
                                            MPS_CI_low_13_q2,
                                            MPS_CI_low_13_q3,
                                            MPS_CI_low_13_q4,
                                            MPS_CI_low_13_q5),
                         "MPS_CI_upper" = c(MPS_CI_up_07_q1,
                                            MPS_CI_up_07_q2,
                                            MPS_CI_up_07_q3,
                                            MPS_CI_up_07_q4,
                                            MPS_CI_up_07_q5,
                                            MPS_CI_up_13_q1,
                                            MPS_CI_up_13_q2,
                                            MPS_CI_up_13_q3,
                                            MPS_CI_up_13_q4,
                                            MPS_CI_up_13_q5))
#pdf(file=paste0(getwd(),"/Results/MPS_KVAS.pdf"))
ggplot(data = out_MPS_KVAS, aes(x = quantile, y = MPS, group=year, color=year)) +
  geom_line() + geom_point()+
  geom_ribbon(data= out_MPS_KVAS,aes(ymin= MPS_CI_lower,ymax= MPS_CI_upper),alpha=0.3) +
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("MPS %") +
  labs(color="Year")
#dev.off()


