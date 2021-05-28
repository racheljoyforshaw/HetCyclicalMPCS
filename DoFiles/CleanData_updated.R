
# clear
rm(list = ls()); gc()
# set working directory.
setwd("/Users/Rachel/Documents/PhD/ActiveSaving/")
# packages
library(psidR)

years_pre <- c(2001,2003,2005,2007)
years_post <- c(2005, 2007, 2009, 2011)

cwf <- openxlsx::read.xlsx(system.file(package="psidR","psid-lists","psid.xlsx"))

# add consumption data to index
cons_df<-data.frame("FAMILY PUBLIC","EXPENDITURES","01>EXPENDITURES 02>Food 03>total annual cost, imputed:",NA,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                    "ER16515A1","ER20456A1","ER24138A1","ER28037A1","ER41027A1","ER46971A1","ER52395A1","ER58212A1","ER65410")
names(cons_df)<-names(cwf)
# housing expenditure
hous_df<-data.frame("FAMILY PUBLIC","EXPENDITURES","01>EXPENDITURES 02>Housing 03>total annual cost, imputed:",NA,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                    "ER16515A5","ER20456A5","ER24138A5","ER28037A5","ER41027A5","ER46971A5","ER52395A5","ER58212A5","ER65414")
names(hous_df)<-names(cwf)
# childcare expenditure
child_df<-data.frame("FAMILY PUBLIC","EXPENDITURES","01>EXPENDITURES 02>Child Care 03>total annual cost, imputed:",NA,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                    "ER16515D1","ER20456D1","ER24138D1","ER28037D2","ER41027D2","ER46971D2","ER52395D2","ER58212D2","ER65438")
names(child_df)<-names(cwf)
# dependents outside FU
depFU_df<-data.frame("FAMILY PUBLIC","EXPENDITURES","01>EXPENDITURES 02>Dependents Outside of Family Unit, Whether: 03>amount paid in support of:",NA,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                     "ER16515C9","ER20456C9","ER24138C9","ER28037D1","ER41027D1","ER46971D1","ER52395D1","ER58212D1","ER65437" )
names(depFU_df)<-names(cwf)
# healthcare
health_df<-data.frame("FAMILY PUBLIC","EXPENDITURES","01>EXPENDITURES 02>Health Care 03>total annual cost, imputed:",NA,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      "ER16515D2","ER20456D2","ER24138D2","ER28037D3","ER41027D3","ER46971D3","ER52395D3","ER58212D3","ER65439" )
names(health_df)<-names(cwf)
# transport
transport_df<-data.frame("FAMILY PUBLIC","EXPENDITURES","01>EXPENDITURES 02>Transportation 03>total annual cost, imputed:",NA,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         "ER16515B6","ER20456B6","ER24138B6","ER28037B7","ER41027B7","ER46971B7","ER52395B7","ER58212B7","ER65425" )
names(transport_df)<-names(cwf)



cwf <- rbind(cwf, cons_df,hous_df,child_df,depFU_df,health_df,transport_df)




# list family data
stocks <- getNamesPSID("ER19203",cwf,years=years_pre)
checking <- getNamesPSID("ER19216",cwf,years = years_pre)
mortgageDebt <- getNamesPSID("ER17052",cwf,years = years_pre)
ownRent <- getNamesPSID("ER17043",cwf,years = years_pre)
educ <- getNamesPSID("ER20457",cwf,years = years_pre)
race <- getNamesPSID("ER19989",cwf,years = years_pre)
kidsFU <- getNamesPSID("ER19172",cwf,years = years_pre)
state <- getNamesPSID("ER17004",cwf,years = years_pre)
exIncome <- getNamesPSID("ER20453",cwf,years = years_pre)
income <- getNamesPSID("er20456",cwf,years = years_pre)
kids <- getNamesPSID("er17016",cwf,years = years_pre)

food <- getNamesPSID("ER20456A1",cwf,years = years_pre)
housingExp  <- getNamesPSID("ER16515A5",cwf,years = years_pre)
childExp  <- getNamesPSID("ER16515D1",cwf,years = years_pre) 
depFUExp  <- getNamesPSID("ER16515C9",cwf,years = years_pre) 
healthExp  <- getNamesPSID("ER16515D2",cwf,years = years_pre)
transportExp  <- getNamesPSID("ER16515B6",cwf,years = years_pre)
famVars <- data.frame(year=years_pre,stocks=stocks,checking=checking,mortgageDebt=mortgageDebt,ownRent=ownRent,educ,race,kidsFU,state,exIncome,income,kids,
                      food=food,housingExp=housingExp,childExp,depFUExp,healthExp,transportExp)



f0107 <- build.panel(fam.vars=famVars,datadir="/Users/Rachel/Documents/PhD/ActiveSaving/Data/updated")