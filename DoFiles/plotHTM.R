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

for (k in c("poorHTM","richHTM")){
  print(k)
  for (j in c("030507","091113")){
    print(j)
    for (i in c("1","2","3","4","5")){
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


out_pct <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
                      "year" = c(2003,2003,2003,2003,2003,2005,2005,2005,2005,2005,2007,2007,2007,2007,2007,2009,2009,2009,2009,2009,2011,2011,2011,2011,2011,2013,2013,2013,2013,2013),
                      "percent wealthy HTM" = c(richHTM_03_q1,richHTM_03_q2,richHTM_03_q3, richHTM_03_q4, richHTM_03_q5, 
                                      richHTM_05_q1,richHTM_05_q2,richHTM_05_q3, richHTM_05_q4, richHTM_05_q5, 
                                      richHTM_07_q1,richHTM_07_q2,richHTM_07_q3, richHTM_07_q4, richHTM_07_q5,
                                      richHTM_09_q1,richHTM_09_q2,richHTM_09_q3, richHTM_09_q4, richHTM_09_q5, 
                                      richHTM_11_q1,richHTM_11_q2,richHTM_11_q3, richHTM_11_q4, richHTM_11_q5, 
                                      richHTM_13_q1,richHTM_13_q2,richHTM_13_q3, richHTM_13_q4, richHTM_13_q5),
                      "percent poor HTM" = c(poorHTM_03_q1,poorHTM_03_q2,poorHTM_03_q3, poorHTM_03_q4, poorHTM_03_q5, 
                                             poorHTM_05_q1,poorHTM_05_q2,poorHTM_05_q3, poorHTM_05_q4, poorHTM_05_q5, 
                                             poorHTM_07_q1,poorHTM_07_q2,poorHTM_07_q3, poorHTM_07_q4, poorHTM_07_q5,
                                             poorHTM_09_q1,poorHTM_09_q2,poorHTM_09_q3, poorHTM_09_q4, poorHTM_09_q5, 
                                             poorHTM_11_q1,poorHTM_11_q2,poorHTM_11_q3, poorHTM_11_q4, poorHTM_11_q5, 
                                             poorHTM_13_q1,poorHTM_13_q2,poorHTM_13_q3, poorHTM_13_q4, poorHTM_13_q5)
                      
)

out_melt<-melt(out_pct, id=c("quantile","year"))
ggplot() +
  geom_bar(data = out_melt, aes(x = quantile, y = value, fill=variable),stat = "identity",position='dodge') +
  theme_pubr() +
  #scale_x_continuous(breaks=seq(2003,2013,2)) +
  xlab("Quintile") +
  ylab("Fraction") +
  facet_grid(~ year) +
  scale_fill_grey(name = "", labels = c("Wealthy HtM", "Poor HtM")) 

pdf(file=paste0(getwd(),"/Results/HtM_quintile.pdf"))
ggplot() +
  geom_bar(data = out_melt, aes(x = quantile, y = value, fill=variable),stat = "identity",position='dodge') +
  theme_pubr() +
  #scale_x_continuous(breaks=seq(2003,2013,2)) +
  xlab("Quintile") +
  ylab("Fraction") +
  facet_grid(~ year) +
  scale_fill_grey(name = "", labels = c("Wealthy HtM", "Poor HtM")) 
dev.off()


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



svy_quantiles03 <- svyquantile(~totalWealth03,familyPanelSurvey030507, seq(0, 1, 0.2),na.rm=TRUE)
svy_quantiles09 <- svyquantile(~totalWealth09,familyPanelSurvey091113, seq(0, 1, 0.2),na.rm=TRUE)

f030507$svy_quantile<- ifelse(f030507$totalWealth03<svy_quantiles03[2],1,
                              ifelse(f030507$totalWealth03>=svy_quantiles03[2] & f030507$totalWealth03<svy_quantiles03[3],2,
                                     ifelse(f030507$totalWealth03>=svy_quantiles03[3] & f030507$totalWealth03<svy_quantiles03[4],3,
                                            ifelse(f030507$totalWealth03>=svy_quantiles03[4]& f030507$totalWealth03<svy_quantiles03[5],4,
                                                   ifelse(f030507$totalWealth03>=svy_quantiles03[5],5,NA)))))
f091113$svy_quantile <- ifelse(f091113$totalWealth09<svy_quantiles09[2],1,
                               ifelse(f091113$totalWealth09>=svy_quantiles09[2] & f091113$totalWealth09<svy_quantiles09[3],2,
                                      ifelse(f091113$totalWealth09>=svy_quantiles09[3] & f091113$totalWealth09<svy_quantiles09[4],3,
                                             ifelse(f091113$totalWealth09>=svy_quantiles09[4]& f091113$totalWealth09<svy_quantiles09[5],4,
                                                    ifelse(f091113$totalWealth09>=svy_quantiles09[5],5,NA)))))

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


for (k in c("poorHTM","richHTM")){
  print(k)
  for (j in c("030507","091113")){
    print(j)
      assign(paste0(k,"_",substr(j,1,2)),
             svymean(~eval(as.name(paste0(k,substr(j,1,2)))),eval(as.name(paste0("familyPanelSurvey",j))),na.rm=TRUE))
      print(paste0(substr(j,1,2),":",(unname(eval(as.name(paste0(k,"_",substr(j,1,2))))[1]))))
      assign(paste0(k,"_",substr(j,3,4)),
             svymean(~eval(as.name(paste0(k,substr(j,3,4)))),eval(as.name(paste0("familyPanelSurvey",j))),na.rm=TRUE))
      print(paste0(substr(j,3,4),":",(unname(eval(as.name(paste0(k,"_",substr(j,3,4))))[1]))))
      assign(paste0(k,"_",substr(j,5,6)),
             svymean(~eval(as.name(paste0(k,substr(j,5,6)))),eval(as.name(paste0("familyPanelSurvey",j))),na.rm=TRUE))
      print(paste0(substr(j,5,6),":",(unname(eval(as.name(paste0(k,"_",substr(j,5,6))))[1])))) 
  
}
}

out_pct <- data.frame(
                      "year" = c(2003,2005,2007,2009,2011,2013),
                      "wealthy HTM" = c(richHTM_03, 
                                                richHTM_05, 
                                                richHTM_07,
                                                richHTM_09, 
                                                richHTM_11, 
                                                richHTM_13),
                      "poor HTM" = c(poorHTM_03, 
                                     poorHTM_05, 
                                     poorHTM_07,
                                     poorHTM_09, 
                                     poorHTM_11, 
                                     poorHTM_13)
                      
)
out_melt<-melt(out_pct, id=c("year"))
pdf(file=paste0(getwd(),"/Results/HtM.pdf"))
ggplot() +
  geom_bar(data = out_melt, aes(x = year, y = value, fill=variable),stat = "identity",position="dodge") +
  theme_pubr() +
 scale_x_continuous(breaks=seq(2003,2013,2)) +
  xlab("Year") +
  ylab("Fraction") +
  scale_fill_grey(name = "", labels = c("Wealthy HtM", "Poor HtM")) 
dev.off()