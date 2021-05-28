familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113,
                                     nest=TRUE)



svy_quantiles03 <- svyquantile(~income03,familyPanelSurvey030507, seq(0, 1, 0.2),na.rm=TRUE)
svy_quantiles09 <- svyquantile(~income09,familyPanelSurvey091113, seq(0, 1, 0.2),na.rm=TRUE)

f030507$svy_quantile<- ifelse(f030507$income03<svy_quantiles03[2],1,
                              ifelse(f030507$income03>=svy_quantiles03[2] & f030507$income03<svy_quantiles03[3],2,
                                     ifelse(f030507$income03>=svy_quantiles03[3] & f030507$income03<svy_quantiles03[4],3,
                                            ifelse(f030507$income03>=svy_quantiles03[4]& f030507$income03<svy_quantiles03[5],4,
                                                   ifelse(f030507$income03>=svy_quantiles03[5],5,NA)))))

f091113$svy_quantile <- ifelse(f091113$income09<svy_quantiles09[2],1,
                               ifelse(f091113$income09>=svy_quantiles09[2] & f091113$income09<svy_quantiles09[3],2,
                                      ifelse(f091113$income09>=svy_quantiles09[3] & f091113$income09<svy_quantiles09[4],3,
                                             ifelse(f091113$income09>=svy_quantiles09[4]& f091113$income09<svy_quantiles09[5],4,
                                                    ifelse(f091113$income09>=svy_quantiles09[5],5,NA)))))

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507,
                                     nest=TRUE)

familyPanelSurvey091113 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight09,
                                     data=f091113,
                                     nest=TRUE)



# plot 

for (k in c("impConsumption","consumption","consumption_plus","income")){
  print(k)
  for (j in c("030507","091113")){
    print(j)
    for (i in c("1","2","3","4","5")){
    
    if (j=="030507" & k=="consumption"){
      assign(paste0(k,"_","01","_q",i),
      svymean(~eval(as.name(paste0(k,"01"))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
    }
    else if(j=="091113"& k=="consumption"){
      assign(paste0(k,"_","07","_q",i),
      svymean(~eval(as.name(paste0(k,"07"))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
    }  
    if (substr(j,1,2)=="03" & k=="consumption_plus"){
      assign(paste0(k,"_",substr(j,1,2),"_q",i),0)
      print(paste0(substr(j,1,2),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,1,2),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,3,4),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,3,4),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,3,4),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,5,6),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,5,6),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,5,6),"_q",i)))[1]))))
    }
    else {
      assign(paste0(k,"_",substr(j,1,2),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,1,2)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,1,2),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,1,2),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,3,4),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,3,4)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,3,4),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,3,4),"_q",i)))[1]))))
      assign(paste0(k,"_",substr(j,5,6),"_q",i),
             svymean(~eval(as.name(paste0(k,substr(j,5,6)))),subset(eval(as.name(paste0("familyPanelSurvey",j))),svy_quantile==i),na.rm=TRUE))
      print(paste0(substr(j,5,6),"q",i,":",(unname(eval(as.name(paste0(k,"_",substr(j,5,6),"_q",i)))[1]))))
    }
    }
  }
}



out_pct <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
                      "year" = c(2002,2002,2002,2002,2002,2004,2004,2004,2004,2004,2006,2006,2006,2006,2006,2008,2008,2008,2008,2008,2010,2010,2010,2010,2010,2012,2012,2012,2012,2012),
                      "consumption" = c(consumption_03_q1,consumption_03_q2,consumption_03_q3, consumption_03_q4, consumption_03_q5, 
                                        consumption_05_q1,consumption_05_q2,consumption_05_q3, consumption_05_q4, consumption_05_q5, 
                                        consumption_07_q1,consumption_07_q2,consumption_07_q3, consumption_07_q4, consumption_07_q5,
                                        consumption_09_q1,consumption_09_q2,consumption_09_q3, consumption_09_q4, consumption_09_q5, 
                                        consumption_11_q1,consumption_11_q2,consumption_11_q3, consumption_11_q4, consumption_11_q5, 
                                        consumption_13_q1,consumption_13_q2,consumption_13_q3, consumption_13_q4, consumption_13_q5),
                                        "consumption_plus" = c(0,0,0, 0, 0, 
                                                               consumption_plus_05_q1,consumption_plus_05_q2,consumption_plus_05_q3, consumption_plus_05_q4, consumption_plus_05_q5, 
                                                               consumption_plus_07_q1,consumption_plus_07_q2,consumption_plus_07_q3, consumption_plus_07_q4, consumption_plus_07_q5,
                                                               consumption_plus_09_q1,consumption_plus_09_q2,consumption_plus_09_q3, consumption_plus_09_q4, consumption_plus_09_q5, 
                                                               consumption_plus_11_q1,consumption_plus_11_q2,consumption_plus_11_q3, consumption_plus_11_q4, consumption_plus_11_q5, 
                                                               consumption_plus_13_q1,consumption_plus_13_q2,consumption_plus_13_q3, consumption_plus_13_q4, consumption_plus_13_q5)
                      #"impConsumption" = c(impConsumption_03_q1,impConsumption_03_q2,impConsumption_03_q3, impConsumption_03_q4, impConsumption_03_q5, 
                      #                     impConsumption_05_q1,impConsumption_05_q2,impConsumption_05_q3, impConsumption_05_q4, impConsumption_05_q5, 
                      #                     impConsumption_07_q1,impConsumption_07_q2,impConsumption_07_q3, impConsumption_07_q4, impConsumption_07_q5,
                      #                     impConsumption_09_q1,impConsumption_09_q2,impConsumption_09_q3, impConsumption_09_q4, impConsumption_09_q5, 
                      #                     impConsumption_11_q1,impConsumption_11_q2,impConsumption_11_q3, impConsumption_11_q4, impConsumption_11_q5, 
                      #                     impConsumption_13_q1,impConsumption_13_q2,impConsumption_13_q3, impConsumption_13_q4, impConsumption_13_q5),
                      #"income" = c(income_03_q1,income_03_q2,income_03_q3, income_03_q4, income_03_q5, 
                      #             income_05_q1,income_05_q2,income_05_q3, income_05_q4, income_05_q5, 
                      #             income_07_q1,income_07_q2,income_07_q3, income_07_q4, income_07_q5,
                      #             income_09_q1,income_09_q2,income_09_q3, income_09_q4, income_09_q5, 
                      #             income_11_q1,income_11_q2,income_11_q3, income_11_q4, income_11_q5, 
                      #             income_13_q1,income_13_q2,income_13_q3, income_13_q4, income_13_q5)
                      
)

out_melt<-melt(out_pct, id=c("quantile","year"))
pdf(file=paste0(getwd(),"/Results/allConsumption.pdf"))
ggplot(data = out_melt, aes(x = quantile, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge")  + facet_grid(~ year) +
  theme_pubr() +
  xlab("Income Quintile") +
  ylab("Mean Nominal Value ($)") +
  scale_fill_grey(name = "", labels = c("Consumption", "Consumption Plus")) 
dev.off()




