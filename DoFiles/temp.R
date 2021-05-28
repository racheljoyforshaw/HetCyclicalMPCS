familyPanelSurvey03_07 <- svydesign(id=~primarySamplingUnit,
                                    strat=~stratification,
                                    weights=~longWeight03,
                                    data=f0307,
                                    nest=TRUE)

familyPanelSurvey07_11 <- svydesign(id=~primarySamplingUnit,
                                    strat=~stratification,
                                    weights=~longWeight07,
                                    data=f0711,
                                    nest=TRUE)

svy_quantiles03 <- svyquantile(~totalWealth03,familyPanelSurvey03_07, seq(0, 1, 0.2))
svy_quantiles07 <- svyquantile(~totalWealth07,familyPanelSurvey07_11, seq(0, 1, 0.2))
f0307$svy_quantile <- ifelse(f0307$totalWealth03<svy_quantiles03[2],1,
                             ifelse(f0307$totalWealth03>=svy_quantiles03[2] & f0307$totalWealth03<svy_quantiles03[3],2,
                                    ifelse(f0307$totalWealth03>=svy_quantiles03[3] & f0307$totalWealth03<svy_quantiles03[4],3,
                                           ifelse(f0307$totalWealth03>=svy_quantiles03[4]& f0307$totalWealth03<svy_quantiles03[5],4,
                                                  ifelse(f0307$totalWealth03>=svy_quantiles03[5],5,NA)))))
f0711$svy_quantile <- ifelse(f0711$totalWealth07<svy_quantiles07[2],1,
                             ifelse(f0711$totalWealth07>=svy_quantiles07[2] & f0711$totalWealth07<svy_quantiles07[3],2,
                                    ifelse(f0711$totalWealth07>=svy_quantiles07[3] & f0711$totalWealth07<svy_quantiles07[4],3,
                                           ifelse(f0711$totalWealth07>=svy_quantiles07[4]& f0711$totalWealth07<svy_quantiles07[5],4,
                                                  ifelse(f0711$totalWealth07>=svy_quantiles07[5],5,NA)))))
familyPanelSurvey03_07_w03 <- svydesign(id=~primarySamplingUnit,
                                        strat=~stratification,
                                        weights=~longWeight03,
                                        data=f0307,
                                        nest=TRUE)
familyPanelSurvey03_07_w07 <- svydesign(id=~primarySamplingUnit,
                                        strat=~stratification,
                                        weights=~longWeight07,
                                        data=f0307,
                                        nest=TRUE)

familyPanelSurvey07_11_w07 <- svydesign(id=~primarySamplingUnit,
                                    strat=~stratification,
                                    weights=~longWeight07,
                                    data=f0711,
                                    nest=TRUE)
familyPanelSurvey07_11_w11 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight11,
                                     data=f0711,
                                     nest=TRUE)

# 03-07
f0307$totalWealthMeanSvy03 <- ifelse(f0307$svy_quantile==1,svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1)),
                                     ifelse(f0307$svy_quantile==2,svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2)),
                                            ifelse(f0307$svy_quantile==3,svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3)),
                                                   ifelse(f0307$svy_quantile==4,svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4)),
                                                          ifelse(f0307$svy_quantile==5,svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5)),NA)))))
f0307$totalWealthMeanSvy07 <- ifelse(f0307$svy_quantile==1,svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==1)),
                                     ifelse(f0307$svy_quantile==2,svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==2)),
                                            ifelse(f0307$svy_quantile==3,svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==3)),
                                                   ifelse(f0307$svy_quantile==4,svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==4)),
                                                          ifelse(f0307$svy_quantile==5,svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==5)),NA)))))

# 07-11
f0711$totalWealthMeanSvy07 <- ifelse(f0711$svy_quantile==1,svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1)),
                                     ifelse(f0711$svy_quantile==2,svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2)),
                                            ifelse(f0711$svy_quantile==3,svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3)),
                                                   ifelse(f0711$svy_quantile==4,svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4)),
                                                          ifelse(f0711$svy_quantile==5,svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5)),NA)))))
f0711$totalWealthMeanSvy11 <- ifelse(f0711$svy_quantile==1,svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==1)),
                                     ifelse(f0711$svy_quantile==2,svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==2)),
                                            ifelse(f0711$svy_quantile==3,svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==3)),
                                                   ifelse(f0711$svy_quantile==4,svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==4)),
                                                          ifelse(f0711$svy_quantile==5,svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==5)),NA)))))

# print active saving % wealth 03-07
100*svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==1),na.rm=TRUE)/(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==1),na.rm=TRUE) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1),na.rm=TRUE))
100*svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==2),na.rm=TRUE)/(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==2),na.rm=TRUE) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2),na.rm=TRUE))
100*svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==3),na.rm=TRUE)/(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==3),na.rm=TRUE) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3),na.rm=TRUE))
100*svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==4),na.rm=TRUE)/(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==4),na.rm=TRUE) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4),na.rm=TRUE))
100*svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==5),na.rm=TRUE)/(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==5),na.rm=TRUE) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5),na.rm=TRUE))


(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==1)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1)))/2
(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==2)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2)))/2
(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==3)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3)))/2
(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==4)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4)))/2
(svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==5)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5)))/2

100*((svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==1)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1)))/svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1)))^(1/4)
100*((svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==2)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2)))/svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2)))^(1/4)
100*((svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==3)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3)))/svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3)))^(1/4)
100*((svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==4)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4)))/svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4)))^(1/4)
100*((svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==5)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5)))/svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5)))^(1/4)


100*svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==1),na.rm=TRUE)/(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==1),na.rm=TRUE) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1),na.rm=TRUE))
100*svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==2),na.rm=TRUE)/(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==2),na.rm=TRUE) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2),na.rm=TRUE))
100*svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==3),na.rm=TRUE)/(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==3),na.rm=TRUE) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3),na.rm=TRUE))
100*svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==4),na.rm=TRUE)/(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==4),na.rm=TRUE) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4),na.rm=TRUE))
100*svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==5),na.rm=TRUE)/(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==5),na.rm=TRUE) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5),na.rm=TRUE))


(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==1)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1)))/4
(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==2)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2)))/4
(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==3)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3)))/4
(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==4)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4)))/4
(svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==5)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5)))/4

100*((svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==1)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1)))/svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1)))^(1/4)
100*((svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==2)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2)))/svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2)))^(1/4)
100*((svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==3)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3)))/svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3)))^(1/4)
100*((svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==4)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4)))/svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4)))^(1/4)
100*((svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==5)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5)))/svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5)))^(1/4)

out_0307 <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                  "year" = c(2003,2003,2003,2003,2003,2007,2007,2007,2007,2007),
                  "totalWealth" = c(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1)),
                                    svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2)),
                                    svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3)),
                                    svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4)),
                                    svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5)),
                                    svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1)),
                                    svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2)),
                                    svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3)),
                                    svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4)),
                                    svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5))))


ggplot(data = out_0307, aes(x = quantile, y = activeSaving)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year)

out_0711 <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                       "year" = c(2007,2007,2007,2007,2007,2011,2011,2011,2011,2011),
                       "totalWealth" = c(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1)),
                                         svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2)),
                                         svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3)),
                                         svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4)),
                                         svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5)),
                                         svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==1)),
                                         svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==2)),
                                         svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==3)),
                                         svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==4)),
                                         svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==5))))

ggplot(data = out_0711, aes(x = quantile, y = totalWealth)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year)


out_0307_as <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                          "year" = c(2007,2007,2007,2007,2007,2011,2011,2011,2011,2011),
                          "activeSaving" = c(svymean(~activeSaving07, subset(familyPanelSurvey03_07_w03,svy_quantile==1),na.rm=TRUE),
                                             svymean(~activeSaving07, subset(familyPanelSurvey03_07_w03,svy_quantile==2),na.rm=TRUE),
                                             svymean(~activeSaving07, subset(familyPanelSurvey03_07_w03,svy_quantile==3),na.rm=TRUE),
                                             svymean(~activeSaving07, subset(familyPanelSurvey03_07_w03,svy_quantile==4),na.rm=TRUE),
                                             svymean(~activeSaving07, subset(familyPanelSurvey03_07_w03,svy_quantile==5),na.rm=TRUE),
                                             svymean(~activeSaving11, subset(familyPanelSurvey07_11_w07,svy_quantile==1),na.rm=TRUE),
                                             svymean(~activeSaving11, subset(familyPanelSurvey07_11_w07,svy_quantile==2),na.rm=TRUE),
                                             svymean(~activeSaving11, subset(familyPanelSurvey07_11_w07,svy_quantile==3),na.rm=TRUE),
                                             svymean(~activeSaving11, subset(familyPanelSurvey07_11_w07,svy_quantile==4),na.rm=TRUE),
                                             svymean(~activeSaving11, subset(familyPanelSurvey07_11_w07,svy_quantile==5),na.rm=TRUE)))

ggplot(data = out_0307_as, aes(x = quantile, y = activeSaving)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year)

out <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                  "year" = c(2007,2007,2007,2007,2007,2011,2011,2011,2011,2011),
                  "totalWealthChange" = c((svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==1)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1))),
                                          (svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==2)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2))),
                                          (svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==3)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3))),
                                          (svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==4)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4))),
                                          (svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==5)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5))),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==1)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1))),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==2)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2))),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==3)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3))),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==4)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4))),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==5)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5)))),
                  "activeSaving" = c(svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==1),na.rm=TRUE),
                                     svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==2),na.rm=TRUE),
                                     svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==3),na.rm=TRUE),
                                     svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==4),na.rm=TRUE),
                                     svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==5),na.rm=TRUE),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==1),na.rm=TRUE),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==2),na.rm=TRUE),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==3),na.rm=TRUE),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==4),na.rm=TRUE),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==5),na.rm=TRUE)))


out_pct <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                  "year" = c(2007,2007,2007,2007,2007,2011,2011,2011,2011,2011),
                  "totalWealthChange" = c((svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==1)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1)))/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1),na.rm=TRUE)),
                                          (svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==2)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2)))/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2),na.rm=TRUE)),
                                          (svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==3)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3)))/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3),na.rm=TRUE)),
                                          (svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==4)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4)))/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4),na.rm=TRUE)),
                                          (svymean(~totalWealth07, subset(familyPanelSurvey03_07_w07,svy_quantile==5)) - svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5)))/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5),na.rm=TRUE)),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==1)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1)))/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1),na.rm=TRUE)),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==2)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2)))/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2),na.rm=TRUE)),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==3)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3)))/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3),na.rm=TRUE)),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==4)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4)))/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4),na.rm=TRUE)),
                                          (svymean(~totalWealth11, subset(familyPanelSurvey07_11_w11,svy_quantile==5)) - svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5)))/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5),na.rm=TRUE))),
                  "activeSaving" = c(svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==1),na.rm=TRUE)/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==1),na.rm=TRUE)),
                                     svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==2),na.rm=TRUE)/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==2),na.rm=TRUE)),
                                     svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==3),na.rm=TRUE)/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==3),na.rm=TRUE)),
                                     svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==4),na.rm=TRUE)/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==4),na.rm=TRUE)),
                                     svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==5),na.rm=TRUE)/abs(svymean(~totalWealth03, subset(familyPanelSurvey03_07_w03,svy_quantile==5),na.rm=TRUE)),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==1),na.rm=TRUE)/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==1),na.rm=TRUE)),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==2),na.rm=TRUE)/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==2),na.rm=TRUE)),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==3),na.rm=TRUE)/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==3),na.rm=TRUE)),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==4),na.rm=TRUE)/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==4),na.rm=TRUE)),
                                     svymean(~activeSaving11, subset(familyPanelSurvey07_11_w11,svy_quantile==5),na.rm=TRUE)/abs(svymean(~totalWealth07, subset(familyPanelSurvey07_11_w07,svy_quantile==5),na.rm=TRUE))))

out_melt<-melt(out, id=c("quantile","year"))
ggplot(data = out_melt, aes(x = quantile, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year) 


out_melt_pct<-melt(out_pct, id=c("quantile","year"))
ggplot(data = out_melt_pct, aes(x = quantile, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year)

tempSvy <- svydesign(id=~primarySamplingUnit,
                                        strat=~stratification,
                                        weights=~longWeight11,
                                        data=f11,
                                        nest=TRUE)
svy_quantiles_temp <- svyquantile(~totalWealth11,tempSvy, c(0.05, 0.10,0.25,0.5,0.75,0.9,0.95 ))





# average active saving for each quintile divided by average 5-year income

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                    strat=~stratification,
                                    weights=~longWeight03,
                                    data=f030507,
                                    nest=TRUE)

svy_quantiles03 <- svyquantile(~totalWealth03,familyPanelSurvey030507, seq(0, 1, 0.2))

f030507$svy_quantile <- ifelse(f030507$income03<svy_quantiles03[2],1,
                             ifelse(f030507$income03>=svy_quantiles03[2] & f030507$income03<svy_quantiles03[3],2,
                                    ifelse(f030507$income03>=svy_quantiles03[3] & f030507$income03<svy_quantiles03[4],3,
                                           ifelse(f030507$income03>=svy_quantiles03[4]& f030507$income03<svy_quantiles03[5],4,
                                                  ifelse(f030507$income03>=svy_quantiles03[5],5,NA)))))

familyPanelSurvey030507 <- svydesign(id=~primarySamplingUnit,
                                     strat=~stratification,
                                     weights=~longWeight03,
                                     data=f030507,
                                     nest=TRUE)

(svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==1),na.rm=TRUE)/4)/(svymean(~income03, subset(familyPanelSurvey030507,svy_quantile==1)))#,na.rm=TRUE) + svymean(~income05, subset(familyPanelSurvey030507,svy_quantile==1),na.rm=TRUE) + svymean(~income07, subset(familyPanelSurvey030507,svy_quantile==1),na.rm=TRUE))/3)
(svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==2),na.rm=TRUE)/4)/(svymean(~income03, subset(familyPanelSurvey030507,svy_quantile==2)))#,na.rm=TRUE) + svymean(~income05, subset(familyPanelSurvey030507,svy_quantile==2),na.rm=TRUE) + svymean(~income07, subset(familyPanelSurvey030507,svy_quantile==2),na.rm=TRUE))/3)
(svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==3),na.rm=TRUE)/4)/(svymean(~income03, subset(familyPanelSurvey030507,svy_quantile==3)))#,na.rm=TRUE) + svymean(~income05, subset(familyPanelSurvey030507,svy_quantile==3),na.rm=TRUE) + svymean(~income07, subset(familyPanelSurvey030507,svy_quantile==3),na.rm=TRUE))/3)
(svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==4),na.rm=TRUE)/4)/(svymean(~income03, subset(familyPanelSurvey030507,svy_quantile==4)))#,na.rm=TRUE) + svymean(~income05, subset(familyPanelSurvey030507,svy_quantile==4),na.rm=TRUE) + svymean(~income07, subset(familyPanelSurvey030507,svy_quantile==4),na.rm=TRUE))/3)
(svymean(~activeSaving07, subset(familyPanelSurvey03_07_w07,svy_quantile==5),na.rm=TRUE)/4)/(svymean(~income03, subset(familyPanelSurvey030507,svy_quantile==5)))#,na.rm=TRUE) + svymean(~income05, subset(familyPanelSurvey030507,svy_quantile==5),na.rm=TRUE) + svymean(~income07, subset(familyPanelSurvey030507,svy_quantile==5),na.rm=TRUE))/3)






