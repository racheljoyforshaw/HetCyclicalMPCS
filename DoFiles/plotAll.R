# plot all results

# DSZ
# active saving
out_as_DSZ <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                         "year" = c("02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12"),
                          "activeSaving" = c(DSZactiveSaving_030507_q1,
                                             DSZactiveSaving_030507_q2,
                                             DSZactiveSaving_030507_q3,
                                             DSZactiveSaving_030507_q4,
                                             DSZactiveSaving_030507_q5,
                                             DSZactiveSaving_091113_q1,
                                             DSZactiveSaving_091113_q2,
                                             DSZactiveSaving_091113_q3,
                                             DSZactiveSaving_091113_q4,
                                             DSZactiveSaving_091113_q5))

pdf(file=paste0(getwd(),"/Results/as_DSZ.pdf"))
ggplot(data = out_as_DSZ, aes(x = quantile, y = activeSaving, group=year, color=year)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("Active Saving %") +
  labs(color="Year")
dev.off()
ggplot(data = out_as_DSZ, aes(x = quantile, y = activeSaving, group=year, color=year)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("Active Saving %") +
  labs(color="Year")

# capitalChange
out_cc_DSZ <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                         "year" = c("02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12"),
                         "capChange" = c(DSZcapChange_030507_q1,
                                            DSZcapChange_030507_q2,
                                            DSZcapChange_030507_q3,
                                            DSZcapChange_030507_q4,
                                            DSZcapChange_030507_q5,
                                            DSZcapChange_091113_q1,
                                            DSZcapChange_091113_q2,
                                            DSZcapChange_091113_q3,
                                            DSZcapChange_091113_q4,
                                            DSZcapChange_091113_q5))
pdf(file=paste0(getwd(),"/Results/capChange_DSZ.pdf"))
ggplot(data = out_cc_DSZ, aes(x = quantile, y = capChange, group=year, color=year)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("Capital Change %") +
  labs(color="Year")
dev.off()
ggplot(data = out_cc_DSZ, aes(x = quantile, y = capChange, group=year, color=year)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("Capital Change %") +
  labs(color="Year")
# consumption
out_cons_DSZ <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                         "year" = c("03-07","03-07","03-07","03-07","03-07","09-13","09-13","09-13","09-13","09-13"),
                         "consumption" = c(DSZconsumption_030507_q1,
                                            DSZconsumption_030507_q2,
                                            DSZconsumption_030507_q3,
                                            DSZconsumption_030507_q4,
                                            DSZconsumption_030507_q5,
                                            DSZconsumption_091113_q1,
                                            DSZconsumption_091113_q2,
                                            DSZconsumption_091113_q3,
                                            DSZconsumption_091113_q4,
                                            DSZconsumption_091113_q5))
pdf(file=paste0(getwd(),"/Results/cons_DSZ.pdf"))
ggplot(data = out_cons_DSZ, aes(x = quantile, y = 100*consumption, group=year, color=year)) +
  geom_line(aes(linetype=year)) + geom_point()+
  scale_color_grey() +
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("Consumption %") +
  #labs(color="Year") +
  theme_pubr()
dev.off()
ggplot(data = out_cons_DSZ, aes(x = quantile, y = 100*consumption, group=year, color=year)) +
  geom_line(aes(linetype=year)) + geom_point()+
  scale_color_grey() +
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("Consumption %") +
  #labs(color="Year") +
  theme_pubr() 
#total wealth
out_tw_DSZ <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                           "year" = c("02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12"),
                           "totalWealth" = c(DSZtotalWealth_030507_q1,
                                             DSZtotalWealth_030507_q2,
                                             DSZtotalWealth_030507_q3,
                                             DSZtotalWealth_030507_q4,
                                             DSZtotalWealth_030507_q5,
                                             DSZtotalWealth_091113_q1,
                                             DSZtotalWealth_091113_q2,
                                             DSZtotalWealth_091113_q3,
                                             DSZtotalWealth_091113_q4,
                                             DSZtotalWealth_091113_q5))

pdf(file=paste0(getwd(),"/Results/tw_DSZ.pdf"))
ggplot(data = out_tw_DSZ, aes(x = quantile, y = 100*totalWealth, group=year, color=year)) +
  geom_line(aes(linetype=year)) + geom_point()+
  scale_color_grey() +
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("Total Net Wealth Change %") +
  #labs(color="Year") +
  theme_pubr()
dev.off()
ggplot(data = out_tw_DSZ, aes(x = quantile, y = 100*totalWealth, group=year, color=year)) +
  geom_line(aes(linetype=year)) + geom_point()+
  scale_color_grey() +
  theme_minimal() +
  xlab("Income Quintile") +
  ylab("Total Net Wealth Change %") +
  #labs(color="Year") +
  theme_pubr()


out_allWealth_DSZ <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                         "year" = c("02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12",
                                    "02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12",
                                    "02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12"),
                         "totalWealth" = c(DSZtotalWealth_030507_q1,
                                           DSZtotalWealth_030507_q2,
                                           DSZtotalWealth_030507_q3,
                                           DSZtotalWealth_030507_q4,
                                           DSZtotalWealth_030507_q5,
                                           DSZtotalWealth_091113_q1,
                                           DSZtotalWealth_091113_q2,
                                           DSZtotalWealth_091113_q3,
                                           DSZtotalWealth_091113_q4,
                                           DSZtotalWealth_091113_q5),
                         "capChange" = c(DSZcapChange_030507_q1,
                                         DSZcapChange_030507_q2,
                                         DSZcapChange_030507_q3,
                                         DSZcapChange_030507_q4,
                                         DSZcapChange_030507_q5,
                                         DSZcapChange_091113_q1,
                                         DSZcapChange_091113_q2,
                                         DSZcapChange_091113_q3,
                                         DSZcapChange_091113_q4,
                                         DSZcapChange_091113_q5),
                         "activeSaving" = c(DSZactiveSaving_030507_q1,
                                            DSZactiveSaving_030507_q2,
                                            DSZactiveSaving_030507_q3,
                                            DSZactiveSaving_030507_q4,
                                            DSZactiveSaving_030507_q5,
                                            DSZactiveSaving_091113_q1,
                                            DSZactiveSaving_091113_q2,
                                            DSZactiveSaving_091113_q3,
                                            DSZactiveSaving_091113_q4,
                                            DSZactiveSaving_091113_q5))

out_melt<-melt(out_allWealth_DSZ, id=c("quantile","year"))
pdf(file=paste0(getwd(),"/Results/allWealth_DSZ.pdf"))
ggplot(data = out_melt, aes(x = quantile, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year) 
dev.off()
ggplot(data = out_melt, aes(x = quantile, y = value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(~ year) 


# # consumption - all years
# out_cons_DSZ <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
#                            "year" = c("02-12","02-12","02-12","02-12","02-12"),
#                            "consumption" = c(DSZconsumption_0313_q1,
#                                              DSZconsumption_0313_q2,
#                                              DSZconsumption_0313_q3,
#                                              DSZconsumption_0313_q4,
#                                              DSZconsumption_0313_q5))
# pdf(file=paste0(getwd(),"/Results/cons_DSZ.pdf"))
# ggplot(data = out_cons_DSZ, aes(x = quantile, y = consumption, group=year, color=year)) +
#   geom_line() + geom_point()+
#   scale_color_brewer(palette="Paired")+
#   theme_minimal() +
#   xlab("Income Quintile") +
#   ylab("Consumption %") +
#   labs(color="Year") 
# dev.off()


# # KV
# # MPC
# 
# out_MPC_KV <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
#                          "year" = c("02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12"),
#                          "MPC" = c(MPC_0313_q1,
#                                            MPC_030507_q2,
#                                            MPC_030507_q3,
#                                            MPC_030507_q4,
#                                            MPC_030507_q5,
#                                            MPC_091113_q1,
#                                            MPC_091113_q2,
#                                            MPC_091113_q3,
#                                            MPC_091113_q4,
#                                            MPC_091113_q5))
# #pdf(file=paste0(getwd(),"/Results/MPC_KV.pdf"))
# ggplot(data = out_MPC_KV, aes(x = quantile, y = MPC, group=year, color=year)) +
#   geom_line() + geom_point()+
#   scale_color_brewer(palette="Paired")+
#   theme_minimal() +
#   xlab("Income Quintile") +
#   ylab("MPC %") +
#   labs(color="Year")
# #dev.off()
# 
# 
# # MPC on balanced panel
# 
# out_MPC_balanced <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
#                          "year" = c("02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12"),
#                          "MPC" = c(MPC_balanced_07_q1,
#                                    MPC_balanced_07_q2,
#                                    MPC_balanced_07_q3,
#                                    MPC_balanced_07_q4,
#                                    MPC_balanced_07_q5,
#                                    MPC_balanced_13_q1,
#                                    MPC_balanced_13_q2,
#                                    MPC_balanced_13_q3,
#                                    MPC_balanced_13_q4,
#                                    MPC_balanced_13_q5))
# #pdf(file=paste0(getwd(),"/Results/MPC_balanced.pdf"))
# ggplot(data = out_MPC_balanced, aes(x = quantile, y = MPC, group=year, color=year)) +
#   geom_line() + geom_point()+
#   scale_color_brewer(palette="Paired")+
#   theme_minimal() +
#   xlab("Income Quintile") +
#   ylab("MPC (balanced panel) %") +
#   labs(color="Year")
# #dev.off()
# 
# #MPS
# out_MPS_KV <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
#                          "year" = c("02-06","02-06","02-06","02-06","02-06","08-12","08-12","08-12","08-12","08-12"),
#                          "MPS" = c(MPS_030507_q1,
#                                    MPS_030507_q2,
#                                    MPS_030507_q3,
#                                    MPS_030507_q4,
#                                    MPS_030507_q5,
#                                    MPS_091113_q1,
#                                    MPS_091113_q2,
#                                    MPS_091113_q3,
#                                    MPS_091113_q4,
#                                    MPS_091113_q5))
# 
# ggplot(data = out_MPS_KV, aes(x = quantile, y = MPS, group=year, color=year)) +
#   geom_line() + geom_point()+
#   scale_color_brewer(palette="Paired")+
#   theme_minimal() +
#   xlab("Income Quintile") +
#   ylab("MPS %") +
#   labs(color="Year")
# 
# 
