taxData <- read.csv("TAXSIM/data_files/taxes_output_long_082019.csv", 
                    header = TRUE)

f01 <- merge(f01, subset(taxData, year=="2001"),by.x ="uniqueID", by.y="personid",all.x=TRUE,all.y=FALSE)
f03 <- merge(f03, subset(taxData, year=="2003"),by.x ="uniqueID", by.y="personid",all.x=TRUE,all.y=FALSE)
f05 <- merge(f05, subset(taxData, year=="2005"),by.x ="uniqueID", by.y="personid",all.x=TRUE,all.y=FALSE)
f07 <- merge(f07, subset(taxData, year=="2007"),by.x ="uniqueID", by.y="personid",all.x=TRUE,all.y=FALSE)
f09 <- merge(f09, subset(taxData, year=="2009"),by.x ="uniqueID", by.y="personid",all.x=TRUE,all.y=FALSE)
f11 <- merge(f11, subset(taxData, year=="2011"),by.x ="uniqueID", by.y="personid",all.x=TRUE,all.y=FALSE)
f13 <- merge(f13, subset(taxData, year=="2013"),by.x ="uniqueID", by.y="personid",all.x=TRUE,all.y=FALSE)



# for(i in shortYears[2:8])
# {
#   assign(paste0("tax",i),
#        read.csv(paste0("Data/fam20",i, "er/FAM20",i,"ER.csv"), header=TRUE))
#   assign(paste0("f",i),
#          merge(eval(as.name(paste0("f",i))),eval(as.name(paste0("tax",i))),all=FALSE, by.x=paste0("intNum",i), by.y = paste0("family_ID1_20",i)))
# }
# rm(tax01,tax03,tax05,tax07,tax09,tax11,tax13,i)
# 
# cpiData <- read.csv("Data/CPI_base_2015.csv",
#                     header = TRUE)



f01$preTaxIncome01 <- f01$income01
f03$preTaxIncome03 <- f03$income03
f05$preTaxIncome05 <- f05$income05
f07$preTaxIncome07 <- f07$income07
f09$preTaxIncome09 <- f09$income09
f11$preTaxIncome11 <- f11$income11
f13$preTaxIncome13 <- f13$income13



# 
# f01$income01 <- f01$preTaxIncome01 - f01$tax
# f03$income03 <- f03$preTaxIncome03 - f03$tax
# f05$income05 <- f05$preTaxIncome05 - f05$tax
# f07$income07 <- f07$preTaxIncome07 - f07$tax
# f09$income09 <- f09$preTaxIncome09 - f09$tax
# f11$income11 <- f11$preTaxIncome11 - f11$tax
# f13$income13 <- f13$preTaxIncome13 - f13$tax


f01$income01 <- f01$preTaxIncome01 - (f01$fu_fiitax + f01$fu_fica + f01$fu_siitax)
f03$income03 <- f03$preTaxIncome03 - (f03$fu_fiitax + f03$fu_fica + f03$fu_siitax)
f05$income05 <- f05$preTaxIncome05 - (f05$fu_fiitax + f05$fu_fica + f05$fu_siitax)
f07$income07 <- f07$preTaxIncome07 - (f07$fu_fiitax + f07$fu_fica + f07$fu_siitax)
f09$income09 <- f09$preTaxIncome09 - (f09$fu_fiitax + f09$fu_fica + f09$fu_siitax)
f11$income11 <- f11$preTaxIncome11 - (f11$fu_fiitax + f11$fu_fica + f11$fu_siitax)
f13$income13 <- f13$preTaxIncome13 - (f13$fu_fiitax + f13$fu_fica + f13$fu_siitax)




# # Get data ready for TAXSIM (which calculates after-tax income)
# 
# # state
# stateData <- read.csv("Data/StateCrossWalk.csv", 
#                       header = TRUE)
# 
# # need: uniqueID, year, SOI state, marital status, age of head, age of spouse, dependents, no children with Dependent Care Credit, no children with
# # Child Credit, Number of qualifying children for EITC, wage of head, wage of spouse, dividends, Interest Received,  Short Term Capital Gains or losses 
# #(asset held for less than a year), Long Term Capital Gains or losses,  Other property income, Other non-property income, 
# # pensions, Gross Social Security Benefits, Unemployment compensation, Other non-taxable transfer Income,  Rent Paid,
# # Real Estate taxes paid, Other Itemized deductions,  Child care expenses, mortgage Deductions
# 
# f01_keeps <- c("uniqueID","state01","marSta01", "age01","ageSpouse01","kids01","numDepend01", "income01", "childcare01")
# f03_keeps <- c("uniqueID","state03","marSta03", "age03","ageSpouse03","kids03","numDepend03", "income03", "childcare03")
# f05_keeps <- c("uniqueID","state05","marSta05", "age05","ageSpouse05","kids05","numDepend05", "income05", "childcare05")
# f07_keeps <- c("uniqueID","state07","marSta07", "age07","ageSpouse07","kids07","numDepend07", "income07", "childcare07")
# f09_keeps <- c("uniqueID","state09","marSta09", "age09","ageSpouse09","kids09","numDepend09", "income09", "childcare09")
# f11_keeps <- c("uniqueID","state11","marSta11", "age11","ageSpouse11","kids11","numDepend11", "income11", "childcare11")
# f13_keeps <- c("uniqueID","state13","marSta13", "age13","ageSpouse13","kids13","numDepend13", "income13", "childcare13")
# 
# 
# f01_taxsim <- f01[,f01_keeps]
# f03_taxsim <- f03[,f03_keeps]
# f05_taxsim <- f05[,f05_keeps]
# f07_taxsim <- f07[,f07_keeps]
# f09_taxsim <- f09[,f09_keeps]
# f11_taxsim <- f11[,f11_keeps]
# f13_taxsim <- f13[,f13_keeps]
# 
# 
# f01_taxsim <- subset(f01_taxsim, f01_taxsim$state01>0 & f01_taxsim$state<99 & f01_taxsim$marSta01<8 & f01_taxsim$childcare01>0)
# f03_taxsim <- subset(f03_taxsim, f03_taxsim$state03>0 & f03_taxsim$state<99 & f03_taxsim$marSta03<8 & f03_taxsim$childcare03>0)
# f05_taxsim <- subset(f05_taxsim, f05_taxsim$state05>0 & f05_taxsim$state<99 & f05_taxsim$marSta05<8 & f05_taxsim$childcare05>0)
# f07_taxsim <- subset(f07_taxsim, f07_taxsim$state07>0 & f07_taxsim$state<99 & f07_taxsim$marSta07<8 & f07_taxsim$childcare07>0)
# f09_taxsim <- subset(f09_taxsim, f09_taxsim$state09>0 & f09_taxsim$state<99 & f09_taxsim$marSta09<8 & f09_taxsim$childcare09>0)
# f11_taxsim <- subset(f11_taxsim, f11_taxsim$state11>0 & f11_taxsim$state<99 & f11_taxsim$marSta11<8 & f11_taxsim$childcare11>0)
# f13_taxsim <- subset(f13_taxsim, f13_taxsim$state13>0 & f13_taxsim$state<99 & f13_taxsim$marSta13<8 & f13_taxsim$childcare13>0)
# 
# # year
# f01_taxsim$year01 <- 2001
# f03_taxsim$year03 <- 2003
# f05_taxsim$year05 <- 2005
# f07_taxsim$year07 <- 2007
# f09_taxsim$year09 <- 2009
# f11_taxsim$year11 <- 2011
# f13_taxsim$year13 <- 2013
# 
# 
# # state
# f01_taxsim<- merge(f01_taxsim,stateData, by.x="state01",by.y="PSID_state", all = FALSE)
# f03_taxsim<- merge(f03_taxsim,stateData, by.x="state03",by.y="PSID_state", all = FALSE)
# f05_taxsim<- merge(f05_taxsim,stateData, by.x="state05",by.y="PSID_state", all = FALSE)
# f07_taxsim<- merge(f07_taxsim,stateData, by.x="state07",by.y="PSID_state", all = FALSE)
# f09_taxsim<- merge(f09_taxsim,stateData, by.x="state09",by.y="PSID_state", all = FALSE)
# f11_taxsim<- merge(f11_taxsim,stateData, by.x="state11",by.y="PSID_state", all = FALSE)
# f13_taxsim<- merge(f13_taxsim,stateData, by.x="state13",by.y="PSID_state", all = FALSE)
# 
# 
# f01_taxsim = f01_taxsim[,!(names(f01_taxsim) %in% "state01")]
# f03_taxsim = f03_taxsim[,!(names(f03_taxsim) %in% "state03")]
# f05_taxsim = f05_taxsim[,!(names(f05_taxsim) %in% "state05")]
# f07_taxsim = f07_taxsim[,!(names(f07_taxsim) %in% "state07")]
# f09_taxsim = f09_taxsim[,!(names(f09_taxsim) %in% "state09")]
# f11_taxsim = f11_taxsim[,!(names(f11_taxsim) %in% "state11")]
# f13_taxsim = f13_taxsim[,!(names(f13_taxsim) %in% "state13")]
# 
# # maritalStatus - coerce into taxsim definitions
# 
# f01_taxsim$marSta01 <- ifelse(f01_taxsim$marSta01==1,2,
#                               ifelse(f01_taxsim$marSta01==2|f01_taxsim$marSta01==3|f01_taxsim$marSta01==4,1,
#                                      ifelse(f01_taxsim$marSta01==5,6,NA)))
# f03_taxsim$marSta03 <- ifelse(f03_taxsim$marSta03==1,2,
#                               ifelse(f03_taxsim$marSta03==2|f03_taxsim$marSta03==3|f03_taxsim$marSta03==4,1,
#                                      ifelse(f03_taxsim$marSta03==5,6,NA)))
# f05_taxsim$marSta05 <- ifelse(f05_taxsim$marSta05==1,2,
#                               ifelse(f05_taxsim$marSta05==2|f05_taxsim$marSta05==3|f05_taxsim$marSta05==4,1,
#                                      ifelse(f05_taxsim$marSta05==5,6,NA)))
# f07_taxsim$marSta07 <- ifelse(f07_taxsim$marSta07==1,2,
#                               ifelse(f07_taxsim$marSta07==2|f07_taxsim$marSta07==3|f07_taxsim$marSta07==4,1,
#                                      ifelse(f07_taxsim$marSta07==5,6,NA)))
# f09_taxsim$marSta09 <- ifelse(f09_taxsim$marSta09==1,2,
#                               ifelse(f09_taxsim$marSta09==2|f09_taxsim$marSta09==3|f09_taxsim$marSta09==4,1,
#                                      ifelse(f09_taxsim$marSta09==5,6,NA)))
# f11_taxsim$marSta11 <- ifelse(f11_taxsim$marSta11==1,2,
#                               ifelse(f11_taxsim$marSta11==2|f11_taxsim$marSta11==3|f11_taxsim$marSta11==4,1,
#                                      ifelse(f11_taxsim$marSta11==5,6,NA)))
# f13_taxsim$marSta13 <- ifelse(f13_taxsim$marSta13==1,2,
#                               ifelse(f13_taxsim$marSta13==2|f13_taxsim$marSta13==3|f13_taxsim$marSta13==4,1,
#                                      ifelse(f13_taxsim$marSta13==5,6,NA)))
# 
# # dependents
# 
# f01_taxsim$numDepend01 <- f01_taxsim$kids01 + f01_taxsim$numDepend01
# f03_taxsim$numDepend03 <- f03_taxsim$kids03 + f03_taxsim$numDepend03
# f05_taxsim$numDepend05 <- f05_taxsim$kids05 + f05_taxsim$numDepend05
# f07_taxsim$numDepend07 <- f07_taxsim$kids07 + f07_taxsim$numDepend07
# f09_taxsim$numDepend09 <- f09_taxsim$kids09 + f09_taxsim$numDepend09
# f11_taxsim$numDepend11 <- f11_taxsim$kids11 + f11_taxsim$numDepend11
# f13_taxsim$numDepend13 <- f13_taxsim$kids13 + f13_taxsim$numDepend13
# 
# 
# f01_taxsim = f01_taxsim[,!(names(f01_taxsim) %in% "kids01")]
# f03_taxsim = f03_taxsim[,!(names(f03_taxsim) %in% "kids03")]
# f05_taxsim = f05_taxsim[,!(names(f05_taxsim) %in% "kids05")]
# f07_taxsim = f07_taxsim[,!(names(f07_taxsim) %in% "kids07")]
# f09_taxsim = f09_taxsim[,!(names(f09_taxsim) %in% "kids09")]
# f11_taxsim = f11_taxsim[,!(names(f11_taxsim) %in% "kids11")]
# f13_taxsim = f13_taxsim[,!(names(f13_taxsim) %in% "kids13")]
# 
# f01_taxsim$careCredit01 <- 0
# f03_taxsim$careCredit03 <- 0
# f05_taxsim$careCredit05 <- 0
# f07_taxsim$careCredit07 <- 0
# f09_taxsim$careCredit09 <- 0
# f11_taxsim$careCredit11 <- 0
# f13_taxsim$careCredit13 <- 0
# 
# 
# f01_taxsim$childCredit01 <- 0
# f03_taxsim$childCredit03 <- 0
# f05_taxsim$childCredit05 <- 0
# f07_taxsim$childCredit07 <- 0
# f09_taxsim$childCredit09 <- 0
# f11_taxsim$childCredit11 <- 0
# f13_taxsim$childCredit13 <- 0
# 
# f01_taxsim$numEITC01 <- 0
# f03_taxsim$numEITC03 <- 0
# f05_taxsim$numEITC05 <- 0
# f07_taxsim$numEITC07 <- 0
# f09_taxsim$numEITC09 <- 0
# f11_taxsim$numEITC11 <- 0
# f13_taxsim$numEITC13 <- 0
# 
# f01_taxsim$wageSpouse01 <- 0
# f03_taxsim$wageSpouse03 <- 0
# f05_taxsim$wageSpouse05 <- 0
# f07_taxsim$wageSpouse07 <- 0
# f09_taxsim$wageSpouse09 <- 0
# f11_taxsim$wageSpouse11 <- 0
# f13_taxsim$wageSpouse13 <- 0
# 
# f01_taxsim$dividends01 <- 0
# f03_taxsim$dividends03 <- 0
# f05_taxsim$dividends05 <- 0
# f07_taxsim$dividends07 <- 0
# f09_taxsim$dividends09 <- 0
# f11_taxsim$dividends11 <- 0
# f13_taxsim$dividends13 <- 0
# 
# f01_taxsim$interest01 <- 0
# f03_taxsim$interest03 <- 0
# f05_taxsim$interest05 <- 0
# f07_taxsim$interest07 <- 0
# f09_taxsim$interest09 <- 0
# f11_taxsim$interest11 <- 0
# f13_taxsim$interest13 <- 0
# 
# f01_taxsim$STcapGains01 <- 0
# f03_taxsim$STcapGains03 <- 0
# f05_taxsim$STcapGains05 <- 0
# f07_taxsim$STcapGains07 <- 0
# f09_taxsim$STcapGains09 <- 0
# f11_taxsim$STcapGains11 <- 0
# f13_taxsim$STcapGains13 <- 0
# 
# f01_taxsim$LTcapGains01 <- 0
# f03_taxsim$LTcapGains03 <- 0
# f05_taxsim$LTcapGains05 <- 0
# f07_taxsim$LTcapGains07 <- 0
# f09_taxsim$LTcapGains09 <- 0
# f11_taxsim$LTcapGains11 <- 0
# f13_taxsim$LTcapGains13 <- 0
# 
# f01_taxsim$othPropIncome01 <- 0
# f03_taxsim$othPropIncome03 <- 0
# f05_taxsim$othPropIncome05 <- 0
# f07_taxsim$othPropIncome07 <- 0
# f09_taxsim$othPropIncome09 <- 0
# f11_taxsim$othPropIncome11 <- 0
# f13_taxsim$othPropIncome13 <- 0
# 
# f01_taxsim$othNonPropIncome01 <- 0
# f03_taxsim$othNonPropIncome03 <- 0
# f05_taxsim$othNonPropIncome05 <- 0
# f07_taxsim$othNonPropIncome07 <- 0
# f09_taxsim$othNonPropIncome09 <- 0
# f11_taxsim$othNonPropIncome11 <- 0
# f13_taxsim$othNonPropIncome13 <- 0
# 
# f01_taxsim$pensions01 <- 0
# f03_taxsim$pensions03 <- 0
# f05_taxsim$pensions05 <- 0
# f07_taxsim$pensions07 <- 0
# f09_taxsim$pensions09 <- 0
# f11_taxsim$pensions11 <- 0
# f13_taxsim$pensions13 <- 0
# 
# f01_taxsim$SSbenefits01 <- 0
# f03_taxsim$SSbenefits03 <- 0
# f05_taxsim$SSbenefits05 <- 0
# f07_taxsim$SSbenefits07 <- 0
# f09_taxsim$SSbenefits09 <- 0
# f11_taxsim$SSbenefits11 <- 0
# f13_taxsim$SSbenefits13 <- 0
# 
# f01_taxsim$unempComp01 <- 0
# f03_taxsim$unempComp03 <- 0
# f05_taxsim$unempComp05 <- 0
# f07_taxsim$unempComp07 <- 0
# f09_taxsim$unempComp09 <- 0
# f11_taxsim$unempComp11 <- 0
# f13_taxsim$unempComp13 <- 0
# 
# f01_taxsim$nonTaxTransfer01 <- 0
# f03_taxsim$nonTaxTransfer03 <- 0
# f05_taxsim$nonTaxTransfer05 <- 0
# f07_taxsim$nonTaxTransfer07 <- 0
# f09_taxsim$nonTaxTransfer09 <- 0
# f11_taxsim$nonTaxTransfer11 <- 0
# f13_taxsim$nonTaxTransfer13 <- 0
# 
# f01_taxsim$rent01 <- 0
# f03_taxsim$rent03 <- 0
# f05_taxsim$rent05 <- 0
# f07_taxsim$rent07 <- 0
# f09_taxsim$rent09 <- 0
# f11_taxsim$rent11 <- 0
# f13_taxsim$rent13 <- 0
# 
# f01_taxsim$realEstateTax01 <- 0
# f03_taxsim$realEstateTax03 <- 0
# f05_taxsim$realEstateTax05 <- 0
# f07_taxsim$realEstateTax07 <- 0
# f09_taxsim$realEstateTax09 <- 0
# f11_taxsim$realEstateTax11 <- 0
# f13_taxsim$realEstateTax13 <- 0
# 
# f01_taxsim$othDeductions01 <- 0
# f03_taxsim$othDeductions03 <- 0
# f05_taxsim$othDeductions05 <- 0
# f07_taxsim$othDeductions07 <- 0
# f09_taxsim$othDeductions09 <- 0
# f11_taxsim$othDeductions11 <- 0
# f13_taxsim$othDeductions13 <- 0
# 
# f01_taxsim$mortgageDeduct01 <- 0
# f03_taxsim$mortgageDeduct03 <- 0
# f05_taxsim$mortgageDeduct05 <- 0
# f07_taxsim$mortgageDeduct07 <- 0
# f09_taxsim$mortgageDeduct09 <- 0
# f11_taxsim$mortgageDeduct11 <- 0
# f13_taxsim$mortgageDeduct13 <- 0
# 
# 
# # need: uniqueID, year, SOI state, marital status, age of head, age of spouse, dependents, no children with Dependent Care Credit, no children with
# # Child Credit, Number of qualifying children for EITC, wage of head, wage of spouse, dividends, Interest Received,  Short Term Capital Gains or losses 
# #(asset held for less than a year), Long Term Capital Gains or losses,  Other property income, Other non-property income, 
# # pensions, Gross Social Security Benefits, Unemployment compensation, Other non-taxable transfer Income,  Rent Paid,
# # Real Estate taxes paid, Other Itemized deductions,  Child care expenses, mortgage Deductions
# 
# # hack - fix
# 
# f01_taxsim$wageHead01 <- f01_taxsim$income01
# f03_taxsim$wageHead03 <- f03_taxsim$income03
# f05_taxsim$wageHead05 <- f05_taxsim$income05
# f07_taxsim$wageHead07 <- f07_taxsim$income07
# f09_taxsim$wageHead09 <- f09_taxsim$income09
# f11_taxsim$wageHead11 <- f11_taxsim$income11
# f13_taxsim$wageHead13 <- f13_taxsim$income13
# 
# f01_taxsim = f01_taxsim[,!(names(f01_taxsim) %in% "income01")]
# f03_taxsim = f03_taxsim[,!(names(f03_taxsim) %in% "income03")]
# f05_taxsim = f05_taxsim[,!(names(f05_taxsim) %in% "income05")]
# f07_taxsim = f07_taxsim[,!(names(f07_taxsim) %in% "income07")]
# f09_taxsim = f09_taxsim[,!(names(f09_taxsim) %in% "income09")]
# f11_taxsim = f11_taxsim[,!(names(f11_taxsim) %in% "income11")]
# f13_taxsim = f13_taxsim[,!(names(f13_taxsim) %in% "income13")]
# 
# 
# # reorder columns
# for(i in shortYears){
# assign(paste0("col_order",i),
#        c("uniqueID", paste0("year",i), "TAXSIM_state",
#          paste0("marSta",i), paste0("age",i), paste0("ageSpouse",i),
#          paste0("numDepend",i), paste0("careCredit",i), paste0("childCredit",i),
#          paste0("numEITC",i), paste0("wageHead",i),  paste0("wageSpouse",i), 
#          paste0("dividends",i),  paste0("interest",i),  paste0("STcapGains",i),
#          paste0("LTcapGains",i),  paste0("othPropIncome",i),  paste0("othNonPropIncome",i),
#          paste0("pensions",i),  paste0("SSbenefits",i), paste0("unempComp",i), 
#          paste0("nonTaxTransfer",i),  paste0("rent",i),  paste0("realEstateTax",i),
#          paste0("othDeductions",i),  paste0("childcare",i),  paste0("mortgageDeduct",i)
#          )
#        )
# }
# 
# 
# f01_taxsim <- f01_taxsim[, col_order01]
# f03_taxsim <- f03_taxsim[, col_order03]
# f05_taxsim <- f05_taxsim[, col_order05]
# f07_taxsim <- f07_taxsim[, col_order07]
# f09_taxsim <- f09_taxsim[, col_order09]
# f11_taxsim <- f11_taxsim[, col_order11]
# f13_taxsim <- f13_taxsim[, col_order13]
# 
# 
# write.table(f01_taxsim,file=paste0(getwd(),"/Data/f01_taxsim.txt"), row.names = FALSE, col.names = FALSE)