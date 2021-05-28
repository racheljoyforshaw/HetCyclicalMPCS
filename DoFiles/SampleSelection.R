# sample selection

# SAMPLE SELECTION

# age - KV
f030507 <- subset(f030507, f030507$age03<65&f030507$age03>22)
f030507 <- subset(f030507, f030507$age05<65&f030507$age05>22)
f030507 <- subset(f030507, f030507$age07<65&f030507$age07>22)

f091113 <- subset(f091113, f091113$age09<65&f091113$age09>22)
f091113 <- subset(f091113, f091113$age11<65&f091113$age11>22)
f091113 <- subset(f091113, f091113$age13<65&f091113$age13>22)

# drop if self employed 

# f030507 <- subset(f030507, f030507$selfEmp03!=1)
# f030507 <- subset(f030507, f030507$selfEmp05!=1)
# f030507 <- subset(f030507, f030507$selfEmp07!=1)
# 
# f091113 <- subset(f091113, f091113$selfEmp09!=1)
# f091113 <- subset(f091113, f091113$selfEmp11!=1)
# f091113 <- subset(f091113, f091113$selfEmp13!=1)


# drop those whose income =<0
# 
# f030507$income03 <- ifelse( f030507$income03<=0,NA,f030507$income03)
# f030507$income05 <- ifelse( f030507$income05<=0,NA,f030507$income05)
# f030507$income07 <- ifelse( f030507$income07<=0,NA,f030507$income07)
# 
# f091113$income09 <- ifelse( f091113$income09<=0,NA,f091113$income09)
# f091113$income11 <- ifelse(f091113$income11<=0,NA,f091113$income11)
# f091113$income13 <- ifelse(f091113$income13<=0,NA,f091113$income13)

# # get rid of top and bottom 1% in levels... 
# quantiles_030507_03 <-  quantile(f030507$income03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$income05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$income07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$income09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$income11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$income13,seq(0,1,0.01),na.rm=TRUE)
# 
# f030507$income03 <- ifelse(f030507$income03<quantiles_030507_03[2] |f030507$income03>quantiles_030507_03[100],NA,f030507$income03)
# f030507$income05 <- ifelse(f030507$income05<quantiles_030507_05[2] |f030507$income05>quantiles_030507_05[100],NA,f030507$income05)
# f030507$income07 <- ifelse(f030507$income07<quantiles_030507_07[2] |f030507$income07>quantiles_030507_07[100],NA,f030507$income07)
# 
# f091113$income09 <- ifelse(f091113$income09<quantiles_091113_09[2] |f091113$income09>quantiles_091113_09[100],NA,f091113$income09)
# f091113$income11 <- ifelse(f091113$income11<quantiles_091113_11[2] |f091113$income11>quantiles_091113_11[100],NA,f091113$income11)
# f091113$income13 <- ifelse(f091113$income13<quantiles_091113_13[2] |f091113$income13>quantiles_091113_13[100],NA,f091113$income13)
# 
# #...and in growth rates 
# 
# f030507$incomeGrowth03 <- (f030507$income03/f030507$income01)-1
# f030507$incomeGrowth05 <- (f030507$income05/f030507$income03)-1
# f030507$incomeGrowth07 <- (f030507$income07/f030507$income05)-1
# 
# f091113$incomeGrowth09 <- (f091113$income09/f091113$income07)-1
# f091113$incomeGrowth11 <- (f091113$income11/f091113$income09)-1
# f091113$incomeGrowth13 <- (f091113$income13/f091113$income11)-1
# 
# quantiles_030507_03 <-  quantile(f030507$incomeGrowth03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$incomeGrowth05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$incomeGrowth07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$incomeGrowth09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$incomeGrowth11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$incomeGrowth13,seq(0,1,0.01),na.rm=TRUE)
#  
# f030507$income03 <- ifelse(f030507$incomeGrowth03<quantiles_030507_03[2]  | f030507$incomeGrowth03>quantiles_030507_03[100],NA,f030507$income03)
# f030507$income05 <- ifelse(f030507$incomeGrowth05<quantiles_030507_05[2]  | f030507$incomeGrowth05>quantiles_030507_05[100],NA,f030507$income05)
# f030507$income07 <- ifelse(f030507$incomeGrowth07<quantiles_030507_07[2] | f030507$incomeGrowth07>quantiles_030507_07[100],NA,f030507$income07)
# 
# f091113$income09 <- ifelse(f091113$incomeGrowth09<quantiles_091113_09[2]  | f091113$incomeGrowth09>quantiles_091113_09[100],NA,f091113$income09)
# f091113$income11 <- ifelse(f091113$incomeGrowth11<quantiles_091113_11[2]  | f091113$incomeGrowth11>quantiles_091113_11[100],NA,f091113$income11)
# f091113$income13 <- ifelse(f091113$incomeGrowth13<quantiles_091113_13[2]  | f091113$incomeGrowth13>quantiles_091113_13[100],NA,f091113$income13)



# consumption

# drop if consumption<0
f030507$consumption03 <- ifelse( f030507$consumption03<=0,NA,f030507$consumption03)
f030507$consumption05 <- ifelse( f030507$consumption05<=0,NA,f030507$consumption05)
f030507$consumption07 <- ifelse( f030507$consumption07<=0,NA,f030507$consumption07)

f091113$consumption09 <- ifelse(f091113$consumption09<=0,NA,f091113$consumption09)
f091113$consumption11 <- ifelse(f091113$consumption11<=0,NA,f091113$consumption11)
f091113$consumption13 <- ifelse(f091113$consumption13<=0,NA,f091113$consumption13)


# get rid of odd extra peaks in consumption plus

# f05$vacations05 <- ifelse(f05$vacations05>f05$income05,NA,f05$vacations05)
# f07$vacations07 <- ifelse(f07$vacations07>f07$income07,NA,f07$vacations07)
# f09$vacations09 <- ifelse(f09$vacations09>f09$income09,NA,f09$vacations09)
# f11$vacations11 <- ifelse(f11$vacations11>f11$income11,NA,f11$vacations11)
# f13$vacations13 <- ifelse(f13$vacations13>f13$income13,NA,f13$vacations13) 
# 
# f05$clothing05 <- ifelse(f05$clothing05>f05$income05,NA,f05$clothing05)
# f07$clothing07 <- ifelse(f07$clothing07>f07$income07,NA,f07$clothing07)
# f09$clothing09 <- ifelse(f09$clothing09>f09$income09,NA,f09$clothing09)
# f11$clothing11 <- ifelse(f11$clothing11>f11$income11,NA,f11$clothing11)
# f13$clothing13 <- ifelse(f13$clothing13>f13$income13,NA,f13$clothing13)
# 
# 
# f05$recreation05 <- ifelse(f05$recreation05>f05$income05,NA,f05$recreation05)
# f07$recreation07 <- ifelse(f07$recreation07>f07$income07,NA,f07$recreation07)
# f09$recreation09 <- ifelse(f09$recreation09>f09$income09,NA,f09$recreation09)
# f11$recreation11 <- ifelse(f11$recreation11>f11$income11,NA,f11$recreation11)
# f13$recreation13 <- ifelse(f13$recreation13>f13$income13,NA,f13$recreation13)
# 
# f05$consumption_plus05 <- ifelse(f05$consumption_plus05>f05$income05,NA,f05$consumption_plus05)
# f07$consumption_plus07 <- ifelse(f07$consumption_plus07>f07$income07,NA,f07$consumption_plus07)
# f09$consumption_plus09 <- ifelse(f09$consumption_plus09>f09$income09,NA,f09$consumption_plus09)
# f11$consumption_plus11 <- ifelse(f11$consumption_plus11>f11$income11,NA,f11$consumption_plus11)
# f13$consumption_plus13 <- ifelse(f13$consumption_plus13>f13$income13,NA,f13$consumption_plus13)

# # get rid of top and bottom 1% in levels... 
# quantiles_030507_03 <-  quantile(f030507$consumption03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$consumption05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$consumption07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$consumption09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$consumption11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$consumption13,seq(0,1,0.01),na.rm=TRUE)
# 
# f030507$consumption03 <- ifelse(f030507$consumption03<quantiles_030507_03[2] |f030507$consumption03>quantiles_030507_03[100],NA,f030507$consumption03)
# f030507$consumption05 <- ifelse(f030507$consumption05<quantiles_030507_05[2] |f030507$consumption05>quantiles_030507_05[100],NA,f030507$consumption05)
# f030507$consumption07 <- ifelse(f030507$consumption07<quantiles_030507_07[2] |f030507$consumption07>quantiles_030507_07[100],NA,f030507$consumption07)
# 
# f091113$consumption09 <- ifelse(f091113$consumption09<quantiles_091113_09[2] |f091113$consumption09>quantiles_091113_09[100],NA,f091113$consumption09)
# f091113$consumption11 <- ifelse(f091113$consumption11<quantiles_091113_11[2] |f091113$consumption11>quantiles_091113_11[100],NA,f091113$consumption11)
# f091113$consumption13 <- ifelse(f091113$consumption13<quantiles_091113_13[2] |f091113$consumption13>quantiles_091113_13[100],NA,f091113$consumption13)
# 
# #...and growth rates
# f030507$consumptionGrowth03 <- (f030507$consumption03/f030507$consumption01)-1
# f030507$consumptionGrowth05 <- (f030507$consumption05/f030507$consumption03)-1
# f030507$consumptionGrowth07 <- (f030507$consumption07/f030507$consumption05)-1
# 
# f091113$consumptionGrowth09 <- (f091113$consumption09/f091113$consumption07)-1
# f091113$consumptionGrowth11 <- (f091113$consumption11/f091113$consumption09)-1
# f091113$consumptionGrowth13 <- (f091113$consumption13/f091113$consumption11)-1
# 
# quantiles_030507_03 <-  quantile(f030507$consumptionGrowth03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$consumptionGrowth05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$consumptionGrowth07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$consumptionGrowth09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$consumptionGrowth11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$consumptionGrowth13,seq(0,1,0.01),na.rm=TRUE)
# 
# f030507$consumption03 <- ifelse(f030507$consumptionGrowth03<quantiles_030507_03[2]  | f030507$consumptionGrowth03>quantiles_030507_03[100],NA,f030507$consumption03)
# f030507$consumption05 <- ifelse(f030507$consumptionGrowth05<quantiles_030507_05[2]  | f030507$consumptionGrowth05>quantiles_030507_05[100],NA,f030507$consumption05)
# f030507$consumption07 <- ifelse(f030507$consumptionGrowth07<quantiles_030507_07[2] | f030507$consumptionGrowth07>quantiles_030507_07[100],NA,f030507$consumption07)
# 
# f091113$consumption09 <- ifelse(f091113$consumptionGrowth09<quantiles_091113_09[2]  | f091113$consumptionGrowth09>quantiles_091113_09[100],NA,f091113$consumption09)
# f091113$consumption11 <- ifelse(f091113$consumptionGrowth11<quantiles_091113_11[2]  | f091113$consumptionGrowth11>quantiles_091113_11[100],NA,f091113$consumption11)
# f091113$consumption13 <- ifelse(f091113$consumptionGrowth13<quantiles_091113_13[2]  | f091113$consumptionGrowth13>quantiles_091113_13[100],NA,f091113$consumption13)




# activeSaving

# # restrict to be <= income
# f030507$activeSaving03 <- ifelse( f030507$activeSaving03>f030507$income03,NA,f030507$activeSaving03)
# f030507$activeSaving05 <- ifelse( f030507$activeSaving05>f030507$income05,NA,f030507$activeSaving05)
# f030507$activeSaving07 <- ifelse( f030507$activeSaving07>f030507$income07,NA,f030507$activeSaving07)
# 
# f091113$activeSaving09 <- ifelse( f091113$activeSaving09>f091113$income09,NA,f091113$activeSaving09)
# f091113$activeSaving11 <- ifelse(f091113$activeSaving11>f091113$income11,NA,f091113$activeSaving11)
# f091113$activeSaving13 <- ifelse(f091113$activeSaving13>f091113$income13,NA,f091113$activeSaving13)


# # get rid of top and bottom 1% in levels...
# 
# quantiles_030507_03 <-  quantile(f030507$activeSaving03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$activeSaving05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$activeSaving07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$activeSaving09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$activeSaving11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$activeSaving13,seq(0,1,0.01),na.rm=TRUE)
# 
# f030507$activeSaving03 <- ifelse(f030507$activeSaving03<quantiles_030507_03[2] |f030507$activeSaving03>quantiles_030507_03[100],NA,f030507$activeSaving03)
# f030507$activeSaving05 <- ifelse(f030507$activeSaving05<quantiles_030507_05[2] |f030507$activeSaving05>quantiles_030507_05[100],NA,f030507$activeSaving05)
# f030507$activeSaving07 <- ifelse(f030507$activeSaving07<quantiles_030507_07[2] |f030507$activeSaving07>quantiles_030507_07[100],NA,f030507$activeSaving07)
# 
# f091113$activeSaving09 <- ifelse(f091113$activeSaving09<quantiles_091113_09[2] |f091113$activeSaving09>quantiles_091113_09[100],NA,f091113$activeSaving09)
# f091113$activeSaving11 <- ifelse(f091113$activeSaving11<quantiles_091113_11[2] |f091113$activeSaving11>quantiles_091113_11[100],NA,f091113$activeSaving11)
# f091113$activeSaving13 <- ifelse(f091113$activeSaving13<quantiles_091113_13[2] |f091113$activeSaving13>quantiles_091113_13[100],NA,f091113$activeSaving13)
# 
# #...and in growth rates
# 
# f030507$activeSavingGrowth03 <- (f030507$activeSaving03/f030507$activeSaving01)-1
# f030507$activeSavingGrowth05 <- (f030507$activeSaving05/f030507$activeSaving03)-1
# f030507$activeSavingGrowth07 <- (f030507$activeSaving07/f030507$activeSaving05)-1
# 
# f091113$activeSavingGrowth09 <- (f091113$activeSaving09/f091113$activeSaving07)-1
# f091113$activeSavingGrowth11 <- (f091113$activeSaving11/f091113$activeSaving09)-1
# f091113$activeSavingGrowth13 <- (f091113$activeSaving13/f091113$activeSaving11)-1
# 
# 
# quantiles_030507_03 <-  quantile(f030507$activeSavingGrowth03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$activeSavingGrowth05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$activeSavingGrowth07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$activeSavingGrowth09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$activeSavingGrowth11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$activeSavingGrowth13,seq(0,1,0.01),na.rm=TRUE)
# 
# f030507$activeSaving03 <- ifelse(f030507$activeSavingGrowth03<quantiles_030507_03[2]  | f030507$activeSavingGrowth03>quantiles_030507_03[100],NA,f030507$activeSaving03)
# f030507$activeSaving05 <- ifelse(f030507$activeSavingGrowth05<quantiles_030507_05[2]  | f030507$activeSavingGrowth05>quantiles_030507_05[100],NA,f030507$activeSaving05)
# f030507$activeSaving07 <- ifelse(f030507$activeSavingGrowth07<quantiles_030507_07[2] | f030507$activeSavingGrowth07>quantiles_030507_07[100],NA,f030507$activeSaving07)
# 
# f091113$activeSaving09 <- ifelse(f091113$activeSavingGrowth09<quantiles_091113_09[2]  | f091113$activeSavingGrowth09>quantiles_091113_09[100],NA,f091113$activeSaving09)
# f091113$activeSaving11 <- ifelse(f091113$activeSavingGrowth11<quantiles_091113_11[2]  | f091113$activeSavingGrowth11>quantiles_091113_11[100],NA,f091113$activeSaving11)
# f091113$activeSaving13 <- ifelse(f091113$activeSavingGrowth13<quantiles_091113_13[2]  | f091113$activeSavingGrowth13>quantiles_091113_13[100],NA,f091113$activeSaving13)


# imputed consumption

# restrict to be > 0
# 
# f030507$impConsumption03 <- ifelse(f030507$impConsumption03<=0,NA,f030507$impConsumption03)
# f030507$impConsumption05 <- ifelse(f030507$impConsumption05<=0,NA,f030507$impConsumption05)
# f030507$impConsumption07 <- ifelse(f030507$impConsumption07<=0,NA,f030507$impConsumption07)
# 
# f091113$impConsumption09 <- ifelse(f091113$impConsumption09<=0,NA,f091113$impConsumption09)
# f091113$impConsumption11 <- ifelse(f091113$impConsumption11<=0,NA,f091113$impConsumption11)
# f091113$impConsumption13 <- ifelse(f091113$impConsumption13<=0,NA,f091113$impConsumption13)

# # get rid of top and bottom 1% in levels...
# 
# quantiles_030507_03 <-  quantile(f030507$impConsumption03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$impConsumption05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$impConsumption07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$impConsumption09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$impConsumption11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$impConsumption13,seq(0,1,0.01),na.rm=TRUE)
# 
# f030507$impConsumption03 <- ifelse(f030507$impConsumption03<quantiles_030507_03[2] |f030507$impConsumption03>quantiles_030507_03[100],NA,f030507$impConsumption03)
# f030507$impConsumption05 <- ifelse(f030507$impConsumption05<quantiles_030507_05[2] |f030507$impConsumption05>quantiles_030507_05[100],NA,f030507$impConsumption05)
# f030507$impConsumption07 <- ifelse(f030507$impConsumption07<quantiles_030507_07[2] |f030507$impConsumption07>quantiles_030507_07[100],NA,f030507$impConsumption07)
# 
# f091113$impConsumption09 <- ifelse(f091113$impConsumption09<quantiles_091113_09[2] |f091113$impConsumption09>quantiles_091113_09[100],NA,f091113$impConsumption09)
# f091113$impConsumption11 <- ifelse(f091113$impConsumption11<quantiles_091113_11[2] |f091113$impConsumption11>quantiles_091113_11[100],NA,f091113$impConsumption11)
# f091113$impConsumption13 <- ifelse(f091113$impConsumption13<quantiles_091113_13[2] |f091113$impConsumption13>quantiles_091113_13[100],NA,f091113$impConsumption13)
# 
# 
# #...and growth rates
# 
# f030507$impConsumptionGrowth03 <- (f030507$impConsumption03/f030507$impConsumption01)-1
# f030507$impConsumptionGrowth05 <- (f030507$impConsumption05/f030507$impConsumption03)-1
# f030507$impConsumptionGrowth07 <- (f030507$impConsumption07/f030507$impConsumption05)-1
# 
# f091113$impConsumptionGrowth09 <- (f091113$impConsumption09/f091113$impConsumption07)-1
# f091113$impConsumptionGrowth11 <- (f091113$impConsumption11/f091113$impConsumption09)-1
# f091113$impConsumptionGrowth13 <- (f091113$impConsumption13/f091113$impConsumption11)-1
# 
# 
# quantiles_030507_03 <-  quantile(f030507$impConsumptionGrowth03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$impConsumptionGrowth05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$impConsumptionGrowth07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$impConsumptionGrowth09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$impConsumptionGrowth11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$impConsumptionGrowth13,seq(0,1,0.01),na.rm=TRUE)
# 
# f030507$impConsumption03 <- ifelse(f030507$impConsumptionGrowth03<quantiles_030507_03[2]  | f030507$impConsumptionGrowth03>quantiles_030507_03[100],NA,f030507$impConsumption03)
# f030507$impConsumption05 <- ifelse(f030507$impConsumptionGrowth05<quantiles_030507_05[2]  | f030507$impConsumptionGrowth05>quantiles_030507_05[100],NA,f030507$impConsumption05)
# f030507$impConsumption07 <- ifelse(f030507$impConsumptionGrowth07<quantiles_030507_07[2] | f030507$impConsumptionGrowth07>quantiles_030507_07[100],NA,f030507$impConsumption07)
# 
# f091113$impConsumption09 <- ifelse(f091113$impConsumptionGrowth09<quantiles_091113_09[2]  | f091113$impConsumptionGrowth09>quantiles_091113_09[100],NA,f091113$impConsumption09)
# f091113$impConsumption11 <- ifelse(f091113$impConsumptionGrowth11<quantiles_091113_11[2]  | f091113$impConsumptionGrowth11>quantiles_091113_11[100],NA,f091113$impConsumption11)
# f091113$impConsumption13 <- ifelse(f091113$impConsumptionGrowth13<quantiles_091113_13[2]  | f091113$impConsumptionGrowth13>quantiles_091113_13[100],NA,f091113$impConsumption13)




# impSaving

# restrict to be <= income
#f030507$impSaving03 <- ifelse(f030507$impSaving03>f030507$income03,NA,f030507$impSaving03)
#f030507$impSaving05 <- ifelse( f030507$impSaving05>f030507$income05,NA,f030507$impSaving05)
#f030507$impSaving07 <- ifelse( f030507$impSaving07>f030507$income07,NA,f030507$impSaving07)

#f091113$impSaving09 <- ifelse( f091113$impSaving09>f091113$income09,NA,f091113$impSaving09)
#f091113$impSaving11 <- ifelse(f091113$impSaving11>f091113$income11,NA,f091113$impSaving11)
#f091113$impSaving13 <- ifelse(f091113$impSaving13>f091113$income13,NA,f091113$impSaving13)

# # get rid of top and bottom 1% in levels...
# 
# quantiles_030507_03 <-  quantile(f030507$impSaving03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$impSaving05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$impSaving07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$impSaving09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$impSaving11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$impSaving13,seq(0,1,0.01),na.rm=TRUE)
# 
# f030507$impSaving03 <- ifelse(f030507$impSaving03<quantiles_030507_03[2] |f030507$impSaving03>quantiles_030507_03[100],NA,f030507$impSaving03)
# f030507$impSaving05 <- ifelse(f030507$impSaving05<quantiles_030507_05[2] |f030507$impSaving05>quantiles_030507_05[100],NA,f030507$impSaving05)
# f030507$impSaving07 <- ifelse(f030507$impSaving07<quantiles_030507_07[2] |f030507$impSaving07>quantiles_030507_07[100],NA,f030507$impSaving07)
# 
# f091113$impSaving09 <- ifelse(f091113$impSaving09<quantiles_091113_09[2] |f091113$impSaving09>quantiles_091113_09[100],NA,f091113$impSaving09)
# f091113$impSaving11 <- ifelse(f091113$impSaving11<quantiles_091113_11[2] |f091113$impSaving11>quantiles_091113_11[100],NA,f091113$impSaving11)
# f091113$impSaving13 <- ifelse(f091113$impSaving13<quantiles_091113_13[2] |f091113$impSaving13>quantiles_091113_13[100],NA,f091113$impSaving13)
# 
# #...and in growth rates
# 
# f030507$impSavingGrowth03 <- (f030507$impSaving03/f030507$impSaving01)-1
# f030507$impSavingGrowth05 <- (f030507$impSaving05/f030507$impSaving03)-1
# f030507$impSavingGrowth07 <- (f030507$impSaving07/f030507$impSaving05)-1
# 
# f091113$impSavingGrowth09 <- (f091113$impSaving09/f091113$impSaving07)-1
# f091113$impSavingGrowth11 <- (f091113$impSaving11/f091113$impSaving09)-1
# f091113$impSavingGrowth13 <- (f091113$impSaving13/f091113$impSaving11)-1
# 
# 
# quantiles_030507_03 <-  quantile(f030507$impSavingGrowth03,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_05 <-  quantile(f030507$impSavingGrowth05,seq(0,1,0.01),na.rm=TRUE)
# quantiles_030507_07 <-  quantile(f030507$impSavingGrowth07,seq(0,1,0.01),na.rm=TRUE)
# 
# quantiles_091113_09 <-  quantile(f091113$impSavingGrowth09,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_11 <-  quantile(f091113$impSavingGrowth11,seq(0,1,0.01),na.rm=TRUE)
# quantiles_091113_13 <-  quantile(f091113$impSavingGrowth13,seq(0,1,0.01),na.rm=TRUE)
# 
# 
# f030507$impSaving03 <- ifelse(f030507$impSavingGrowth03<quantiles_030507_03[2]  | f030507$impSavingGrowth03>quantiles_030507_03[100],NA,f030507$impSaving03)
# f030507$impSaving05 <- ifelse(f030507$impSavingGrowth05<quantiles_030507_05[2]  | f030507$impSavingGrowth05>quantiles_030507_05[100],NA,f030507$impSaving05)
# f030507$impSaving07 <- ifelse(f030507$impSavingGrowth07<quantiles_030507_07[2] | f030507$impSavingGrowth07>quantiles_030507_07[100],NA,f030507$impSaving07)
# 
# f091113$impSaving09 <- ifelse(f091113$impSavingGrowth09<quantiles_091113_09[2]  | f091113$impSavingGrowth09>quantiles_091113_09[100],NA,f091113$impSaving09)
# f091113$impSaving11 <- ifelse(f091113$impSavingGrowth11<quantiles_091113_11[2]  | f091113$impSavingGrowth11>quantiles_091113_11[100],NA,f091113$impSaving11)
# f091113$impSaving13 <- ifelse(f091113$impSavingGrowth13<quantiles_091113_13[2]  | f091113$impSavingGrowth13>quantiles_091113_13[100],NA,f091113$impSaving13)


                                                                                                                                                                                                                              
# # for the balanced panel
# f09$income09 <- ifelse(f09$income09<100,NA,f09$income09)
# f13$income13 <- ifelse(f13$income13<100,NA,f13$income13)
# 
# f09$consumption09 <- ifelse(f09$consumption09<100,NA,f09$consumption09)
# f13$consumption13 <- ifelse(f13$consumption13<100,NA,f13$consumption13)

# drop growth variables


#f030507 <- f030507[,!(names(f030507) %in% c("incomeGrowth03","incomeGrowth05","incomeGrowth07","consumptionGrowth03","consumptionGrowth05","consumptionGrowth07"))]
#f091113 <- f091113[,!(names(f091113) %in% c("incomeGrowth09","incomeGrowth11","incomeGrowth13","consumptionGrowth09","consumptionGrowth11","consumptionGrowth13"))]
