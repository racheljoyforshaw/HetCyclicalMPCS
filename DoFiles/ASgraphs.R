#rm(list = ls()); gc()

# subset data

# Do I want value changes of all wealth components? If so, need to import more data (mostly imputed...)
#f0507$stocksChange <-f0507$stockValue07 - f0507$stockValue05 
#f0507$privateAnnuityChange 
#f0507$othRealEstateChange
#f0507$farmBusinessChange 

f0507_keeps <-
  c("uniqueID",
    "primarySamplingUnit",
    "longWeight05",
    "longWeight07",
    "stratification",
    "stocksAS07",
    "privateAnnuityAS07",
    "othRealEstateAS07",
    "farmBusinessAS07",
    "checkingAS07",
    "bondAS07",
    "vehicleAS07",
    "othDebtAS07",
    "housingAS07",
    "activeSaving07",
    "totalWealth05",
    "totalWealth07"
  )
f0711_keeps <-
  c("uniqueID",
    "primarySamplingUnit",
    "longWeight07",
    "longWeight11",
    "stratification",
    "stocksAS11",
    "privateAnnuityAS11",
    "othRealEstateAS11",
    "farmBusinessAS11",
    "checkingAS11",
    "bondAS11",
    "vehicleAS11",
    "othDebtAS11",
    "housingAS11",
    "activeSaving11",
    "totalWealth07",
    "totalWealth11"
  )    

f0507_ASgraphs <- f0507[ , f0507_keeps ]
f0711_ASgraphs <- f0711[ , f0711_keeps ]


# create a year variable
f0507_ASgraphs$year07 <- 2007
f0711_ASgraphs$year11 <- 2011


# create a change in wealth variable
f0507_ASgraphs$totalWealthChange07 <- f0507_ASgraphs$totalWealth07-f0507_ASgraphs$totalWealth05
f0711_ASgraphs$totalWealthChange11 <- f0711_ASgraphs$totalWealth11-f0711_ASgraphs$totalWealth07

# trim data
# f0507_ASgraphs <- f0507_ASgraphs[f0507_ASgraphs$totalWealth05 < quantile(f0507_ASgraphs$totalWealth05, 0.95), ]
# f0711_ASgraphs <- f0711_ASgraphs[f0711_ASgraphs$totalWealth07 < quantile(f0711_ASgraphs$totalWealth07, 0.95), ]
# f0507_ASgraphs <- f0507_ASgraphs[f0507_ASgraphs$totalWealth05 > quantile(f0507_ASgraphs$totalWealth05, 0.05), ]
# f0711_ASgraphs <- f0711_ASgraphs[f0711_ASgraphs$totalWealth07 > quantile(f0711_ASgraphs$totalWealth07, 0.05), ]

#f0507_ASgraphs <- f0507_ASgraphs[f0507_ASgraphs$totalWealthChange07 < quantile(f0507_ASgraphs$totalWealthChange07, 0.95), ]
#f0711_ASgraphs <- f0711_ASgraphs[f0711_ASgraphs$totalWealthChange11 < quantile(f0711_ASgraphs$totalWealthChange11, 0.95), ]
#f0507_ASgraphs <- f0507_ASgraphs[f0507_ASgraphs$totalWealthChange07 > quantile(f0507_ASgraphs$totalWealthChange07, 0.05), ]
#f0711_ASgraphs <- f0711_ASgraphs[f0711_ASgraphs$totalWealthChange11 > quantile(f0711_ASgraphs$totalWealthChange11, 0.05), ]


# drop the extra wealth variables

#f0507_ASgraphs <- f0507_ASgraphs[ , !(names(f0507_ASgraphs) %in% c("totalWealth05"))]
#f0711_ASgraphs <- f0711_ASgraphs[ , !(names(f0711_ASgraphs) %in% c("totalWealth11"))]

# # join the 05-07 and 07-11 data together
# f0511_ASgraphs<- full_join(f0507_ASgraphs,f0711_ASgraphs,by = c("uniqueID",
#                                                                  "stratification",
#                                                                  "primarySamplingUnit"))
# # melt the data into long format
# require(data.table)
# f0511_ASgraphs <- melt(setDT(f0511_ASgraphs),
#      id=c("uniqueID","primarySamplingUnit","stratification"), 
#      measure=patterns("^longWeight",
#                       "^stocksAS",
#                       "^privateAnnuityAS",
#                       "^othRealEstateAS",
#                       "^farmBusinessAS",
#                       "^checkingAS",
#                       "^bondAS",
#                       "^vehicleAS",
#                       "^othDebtAS",
#                       "^housingAS",
#                       "^activeSaving",
#                       "^totalWealth",
#                       "^totalWealthChange",
#                       "^year"),
#      value.name=c("longWeight",
#                   "stocksAS",
#                   "privateAnnuityAS",
#                   "othRealEstateAS",
#                   "farmBusinessAS",
#                   "checkingAS",
#                   "bondAS",
#                   "vehicleAS",
#                   "othDebtAS",
#                   "housingAS",
#                   "activeSaving",
#                   "totalWealth",
#                   "totalWealthChange",
#                   "year"),na.rm=TRUE)

# make year a factor
f0507_ASgraphs$year <- as.factor(f0507_ASgraphs$year)
f0711_ASgraphs$year <- as.factor(f0711_ASgraphs$year)
# f0511_ASgraphs$year <- as.factor(f0511_ASgraphs$year)



familyPanelSurvey05_07 <- svydesign(id=~primarySamplingUnit,
                                    strat=~stratification,
                                    weights=~longWeight05,
                                    data=f0507_ASgraphs,
                                    nest=TRUE)
familyPanelSurvey07_11 <- svydesign(id=~primarySamplingUnit,
                                    strat=~stratification,
                                    weights=~longWeight07,
                                    data=f0711_ASgraphs,
                                    nest=TRUE)

# familyPanelSurvey05_11 <- svydesign(id=~primarySamplingUnit,
#                                              strat=~stratification,
#                                              weights=~longWeight,
#                                              data=f0511_ASgraphs,
#                                              nest=TRUE)


# out <- f0511_ASgraphs %>%
#   mutate(quantile = ifelse(year=="2007",cut(totalWealth, c(svyquantile(~totalWealth,subset(familyPanelSurvey05_11, year=="2007"),
#                                           quantiles=c(0.0,0.2,0.4,0.6,0.8,1.0),
#                                           na.rm=TRUE,
#                                           ci=FALSE)[c(1,2,3,4,5,6)]),
#                                           include.lowest = TRUE,
#                                           labels = c("q1", "q2","q3","q4","q5")),
#                            cut(totalWealth, c(svyquantile(~totalWealth,subset(familyPanelSurvey05_11, year=="2011"),
#                                                           quantiles=c(0.0,0.2,0.4,0.6,0.8,1.0),
#                                                           na.rm=TRUE,
#                                                           ci=FALSE)[c(1,2,3,4,5,6)]),
#                                include.lowest = TRUE,
#                                labels = c("q1", "q2","q3","q4","q5")))) %>%
#   group_by(year,quantile) %>%
#   summarize(asMean = weighted.mean(activeSaving, longWeight, na.rm=TRUE),
#             totalWealthChangeMean = weighted.mean(totalWealthChange,longWeight,na.rm=TRUE),
#               n = n()) #%>%
#   #mutate(percentASofWealth = if_else(is.na(if_else(year=="2007",100*(asMean/abs(totalWealthMean))^(1/2),100*(asMean/abs(totalWealthMean))^(1/4))),
#   #                                   if_else(year=="2007",-100*(abs(asMean)/abs(totalWealthMean))^(1/2),-100*(abs(asMean)/abs(totalWealthMean))^(1/4)),
#   #  if_else(year=="2007",100*(asMean/abs(totalWealthMean))^(1/2),100*(asMean/abs(totalWealthMean))^(1/4))))

out_0507 <- f0507_ASgraphs %>%
  mutate(quantile = cut(totalWealth05, c(svyquantile(~totalWealth05,familyPanelSurvey05_07,
                                                                       quantiles=c(0.0,0.2,0.4,0.6,0.8,1.0),
                                                                       na.rm=TRUE,
                                                                       ci=FALSE)[c(1,2,3,4,5,6)]),
                                            include.lowest = TRUE,
                                            labels = c("q1", "q2","q3","q4","q5"))) %>%
  group_by(quantile) %>%
  summarize(100*(asMean = weighted.mean(activeSaving07, longWeight07, na.rm=TRUE)/weighted.mean(totalWealth05,longWeight05,na.rm=TRUE)),
            totalWealthChangeMean = 100*(weighted.mean(totalWealthChange07,longWeight07,na.rm=TRUE)/weighted.mean(totalWealth05,longWeight05,na.rm=TRUE)),
            n = n()) #%>%
  #mutate(percentASofWealth =100*(asMean/abs(totalWealthChangeMean))^(1/4))

out_0711 <- f0711_ASgraphs %>%
  mutate(quantile = cut(totalWealth07, c(svyquantile(~totalWealth07,familyPanelSurvey07_11,
                                                     quantiles=c(0.0,0.2,0.4,0.6,0.8,1.0),
                                                     na.rm=TRUE,
                                                     ci=FALSE)[c(1,2,3,4,5,6)]),
                        include.lowest = TRUE,
                        labels = c("q1", "q2","q3","q4","q5"))) %>%
  # mutate(quantile = cut(totalWealth07, c(svyquantile(~totalWealth07,familyPanelSurvey07_11,
  #                                                    quantiles=c(0.0,0.2,0.4,0.6,0.8,1.0),
  #                                                    na.rm=TRUE,
  #                                                    ci=FALSE)[c(1,2,3,4,5,6)]),
  #                       include.lowest = TRUE,
  #                       labels = c("q1", "q2","q3","q4","q5"))) %>%
  group_by(year,quantile) %>%
  summarize(100*(asMean = weighted.mean(activeSaving11, longWeight11, na.rm=TRUE)/weighted.mean(totalWealth07,longWeight07,na.rm=TRUE)),
            totalWealthChangeMean = 100*(weighted.mean(totalWealthChange11,longWeight11,na.rm=TRUE)/weighted.mean(totalWealth07,longWeight07,na.rm=TRUE)),
            n = n()) #%>%
  #mutate(percentASofWealth =100*(asMean/abs(totalWealthChangeMean))^(1/4))

# join the two datasets
test <- full_join(out_0507,out_0711)

# melt asMean and totalWealthMean into long format
out_melt <- melt(test,id=c("year","quantile","n"))

ggplot(data = out_melt, aes(x = quantile, y = value, group = quantile, fill = variable)) +
  geom_bar(stat = "identity",position="stack") + facet_grid(~ year)
  #geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = -1)











#############################################################################################################
# Active saving by year, quantile with confidence intervals

strat_design <- f0511_ASgraphs %>%
  as_survey_design(strata = stratification, weight  = longWeight)

out_survey<- strat_design %>%
  mutate(quantile = cut(totalWealth, c(min(f0511_ASgraphs$totalWealth,na.rm=TRUE),
                                       svyquantile(~totalWealth,subset(familyPanelSurvey05_11, year=="2007"),
                                                   quantiles=c(0.2,0.4,0.6,0.8),
                                                   na.rm=TRUE,
                                                   ci=FALSE)[c(1,2,3,4,5)],
                                       max(f0511_ASgraphs$totalWealth,na.rm=TRUE)), include.lowest = TRUE,
                        labels = c("q1", "q2","q3","q4","q5"))) %>%
  group_by(year, quantile) %>%
  summarize(asMean = survey_mean(activeSaving,na.rm=TRUE,vartype="ci"),
            n = unweighted(n()))

ggplot(data = out_survey, aes(x = year, y = asMean, group = quantile, fill = quantile,
                       ymax = asMean_upp, ymin = asMean_low)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  theme_classic() +
  ylab("active saving (mean)") +
  scale_fill_brewer(
    palette = "Blues")



#############################################################################################################

