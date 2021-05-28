# 2007 calibration - recession/expansion within model
expansion07 <- read.table("/Users/Rachel/Documents/PhD/KS/cstwMPC2007/Code/Mathematica/Results/MPCListDistSevenWithAggShockExpansionAlt.txt")
recession07 <- read.table("/Users/Rachel/Documents/PhD/KS/cstwMPC2007/Code/Mathematica/Results/MPCListDistSevenWithAggShockRecessionAlt.txt")

out_MPC_KS_07 <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                         "State" = c("expansion","expansion","expansion","expansion","expansion","recession","recession","recession","recession","recession"),
                         "MPC" = c(expansion07[6,1],
                                   expansion07[5,1],
                                   expansion07[4,1],
                                   expansion07[3,1],
                                   expansion07[2,1],
                                   recession07[6,1],
                                   recession07[5,1],
                                   recession07[4,1],
                                   recession07[3,1],
                                   recession07[2,1]))

pdf(file=paste0(getwd(),"/Results/MPC_07.pdf"))
ggplot(data = out_MPC_KS_07, aes(x = quantile, y = MPC, group=State, color=State)) +
  ylim(0,0.6) +
  geom_line() + geom_point()+
  scale_color_grey() +
  theme_pubr() + 
  xlab("Income Quintile") +
  ylab("MPC") +
  labs(color="State")
dev.off()

ggplot(data = out_MPC_KS_07, aes(x = quantile, y = MPC, group=State, color=State)) +
  ylim(0,0.6) +
  geom_line() + geom_point()+
  scale_color_grey() +
  theme_pubr() + 
  xlab("Income Quintile") +
  ylab("MPC") +
  labs(color="State")


# 2009 calibration - recession/expansion within model
expansion09 <- read.table("/Users/Rachel/Documents/PhD/KS/cstwMPC2009/Code/Mathematica/Results/MPCListDistSevenWithAggShockexpansionAlt.txt")
recession09 <- read.table("/Users/Rachel/Documents/PhD/KS/cstwMPC2009/Code/Mathematica/Results/MPCListDistSevenWithAggShockRecessionAlt.txt")

out_MPC_KS_09 <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                      "State" = c("expansion","expansion","expansion","expansion","expansion","recession","recession","recession","recession","recession"),
                      "MPC" = c(expansion09[6,1],
                                expansion09[5,1],
                                expansion09[4,1],
                                expansion09[3,1],
                                expansion09[2,1],
                                recession09[6,1],
                                recession09[5,1],
                                recession09[4,1],
                                recession09[3,1],
                                recession09[2,1]))
pdf(file=paste0(getwd(),"/Results/MPC_09.pdf"))
ggplot(data = out_MPC_KS_09, aes(x = quantile, y = MPC, group=State, color=State)) +
  ylim(0,0.6) +
  geom_line() + geom_point()+
  scale_color_grey() +
  theme_pubr() + 
  xlab("Income Quintile") +
  ylab("MPC") +
  labs(color="State")
dev.off()
ggplot(data = out_MPC_KS_09, aes(x = quantile, y = MPC, group=State, color=State)) +
  ylim(0,0.6) +
  geom_line() + geom_point()+
  scale_color_grey() +
  theme_pubr() + 
  xlab("Income Quintile") +
  ylab("MPC") +
  labs(color="State")

# compare baseline 2007/2009
baseline07 <- read.table("/Users/Rachel/Documents/PhD/KS/cstwMPC2007/Code/Mathematica/Results/MPCListDistSevenWithAggShockAlt.txt")
baseline09 <- read.table("/Users/Rachel/Documents/PhD/KS/cstwMPC2009/Code/Mathematica/Results/MPCListDistSevenWithAggShockAlt.txt")

out_MPC_KS_07_09 <- data.frame("quantile" = c(1,2,3,4,5,1,2,3,4,5),
                            "Year" = c("2007","2007","2007","2007","2007","2009","2009","2009","2009","2009"),
                            "MPC" = c(baseline07[6,1],
                                      baseline07[5,1],
                                      baseline07[4,1],
                                      baseline07[3,1],
                                      baseline07[2,1],
                                      baseline09[6,1],
                                      baseline09[5,1],
                                      baseline09[4,1],
                                      baseline09[3,1],
                                      baseline09[2,1]))

pdf(file=paste0(getwd(),"/Results/MPC_07_09.pdf"))
ggplot(data = out_MPC_KS_07_09, aes(x = quantile, y = MPC, group=Year, color=Year)) +
  ylim(0,0.6) +
  geom_line() + geom_point()+
  scale_color_grey() +
  theme_pubr() + 
  xlab("Income Quintile") +
  ylab("MPC") +
  labs(color="Year")
dev.off()


ggplot(data = out_MPC_KS_07_09, aes(x = quantile, y = MPC, group=Year, color=Year)) +
  ylim(0,0.6) +
  geom_line() + geom_point()+
  scale_color_grey() +
  theme_pubr() + 
  xlab("Income Quintile") +
  ylab("MPC") +
  labs(color="Year")