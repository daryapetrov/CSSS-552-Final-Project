
library(ggplot2)


# 3 candidates, 1 winner, 100, 1,000, and 10,000 voters with phigh=0.75
# missingess random 
results2 <- read.csv("3_1_100_1000_10000_phigh75.csv",row.names = 1)
results2.sub <- results2[which(results2$Voters %in% c(100,1000,10000)),] #10,000 didn't finish running
results2.sub$Voters = as.factor(results2.sub$Voters)
mean(results2.sub$Agreement[which(results2.sub$Voters==100,)])
mean(results2.sub$Agreement[which(results2.sub$Voters==1000,)])
mean(results2.sub$Agreement[which(results2.sub$Voters==10000,)])
ggplot(results2.sub, aes(x = Voters, y= Agreement, fill=Voters)) +
  geom_boxplot() +
  labs(title = "3 Unequal Candidates (3 high, 0 low)", x = "Number of Voters", y = "Agreement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=23),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.position = "none") +
  scale_fill_manual(values = c("lightgreen","lightblue", "pink")) + 
  ylim(0.75, 1)


# 6 candidates, 1 winner, 100, 1,000, and 10,000 voters with phigh=0.75
# missingess random 
results2_w6 <- read.csv("6_10_100_1000_10000_phigh75.csv",row.names = 1)
results2_w6$Voters = as.factor(results2_w6$Voters)
mean(results2_w6$Agreement[which(results2_w6$Voters==100,)])
mean(results2_w6$Agreement[which(results2_w6$Voters==1000,)])
mean(results2_w6$Agreement[which(results2_w6$Voters==10000,)])
ggplot(results2_w6 , aes(x = Voters, y= Agreement, fill=Voters)) +
  geom_boxplot() +
  labs(title = "6 Unequal Candidates (3 high, 3 low)", x = "Number of Voters", y = "Agreement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=23),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.position = "none") +
  scale_fill_manual(values = c("lightgreen","lightblue", "pink")) + 
  ylim(0.75, 1)


# 10 candidates, 1 winner, 100, 1,000, and 10,000 voters with phigh=0.75
# missingess random 
results2_w10 <- read.csv("10_100_1000_10000_phigh75.csv",row.names = 1)
results2_w10$Voters = as.factor(results2_w10$Voters)
mean(results2_w10$Agreement[which(results2_w10$Voters==100,)])
mean(results2_w10$Agreement[which(results2_w10$Voters==1000,)])
mean(results2_w10$Agreement[which(results2_w10$Voters==10000,)])
ggplot(results2_w10 , aes(x = Voters, y= Agreement, fill=Voters)) +
  geom_boxplot() +
  labs(title = "10 Unequal Candidates (3 high, 7 low)", x = "Number of Voters", y = "Agreement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=22.5),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.position = "none") +
  scale_fill_manual(values = c("lightgreen","lightblue", "pink")) + 
  ylim(0.75, 1)


################### nhigh=2 #########################################################

results2_nhigh2 <- read.csv("nhigh2_100_1000_10000_phigh75_try2.csv",row.names = 1)

results2_nhigh2_3 <- results2_nhigh2[which(results2_nhigh2$Candidates==3),]
results2_nhigh2_6 <- results2_nhigh2[which(results2_nhigh2$Candidates==6),]
results2_nhigh2_10 <- results2_nhigh2[which(results2_nhigh2$Candidates==10),]

results2_nhigh2_3$Voters = as.factor(results2_nhigh2_3$Voters)
mean(results2_nhigh2_3$Agreement[which(results2_nhigh2_3$Voters==100,)])
mean(results2_nhigh2_3$Agreement[which(results2_nhigh2_3$Voters==1000,)])
mean(results2_nhigh2_3$Agreement[which(results2_nhigh2_3$Voters==10000,)])
ggplot(results2_nhigh2_3 , aes(x = Voters, y= Agreement, fill=Voters)) +
  geom_boxplot() +
  labs(title = "3 Unequal Candidates (2 high, 1 low)", x = "Number of Voters", y = "Agreement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=22.5),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.position = "none") +
  scale_fill_manual(values = c("lightgreen","lightblue", "pink")) + 
  ylim(0.75, 1)

results2_nhigh2_6$Voters = as.factor(results2_nhigh2_6$Voters)
mean(results2_nhigh2_6$Agreement[which(results2_nhigh2_6$Voters==100,)])
mean(results2_nhigh2_6$Agreement[which(results2_nhigh2_6$Voters==1000,)])
mean(results2_nhigh2_6$Agreement[which(results2_nhigh2_6$Voters==10000,)])
ggplot(results2_nhigh2_6 , aes(x = Voters, y= Agreement, fill=Voters)) +
  geom_boxplot() +
  labs(title = "6 Unequal Candidates (2 high, 4 low)", x = "Number of Voters", y = "Agreement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=22.5),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.position = "none") +
  scale_fill_manual(values = c("lightgreen","lightblue", "pink")) + 
  ylim(0.75, 1)

results2_nhigh2_10$Voters = as.factor(results2_nhigh2_10$Voters)
mean(results2_nhigh2_10$Agreement[which(results2_nhigh2_10$Voters==100,)])
mean(results2_nhigh2_10$Agreement[which(results2_nhigh2_10$Voters==1000,)])
mean(results2_nhigh2_10$Agreement[which(results2_nhigh2_10$Voters==10000,)])
ggplot(results2_nhigh2_10 , aes(x = Voters, y= Agreement, fill=Voters)) +
  geom_boxplot() +
  labs(title = "10 Unequal Candidates (2 high, 8 low)", x = "Number of Voters", y = "Agreement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=22.5),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.position = "none") +
  scale_fill_manual(values = c("lightgreen","lightblue", "pink")) + 
  ylim(0.75, 1)




###################### ALASKA ######################
missing_probs_AL <- read.csv("missing_prob_Alaska.csv",row.names = 1)
missing_probs_AL <- as.vector(missing_probs_AL)$x
missing_probs_AL[6] <- 1 - sum(missing_probs_AL)
missing_dist <- rev(missing_probs_AL) #reverse the probs, so entry corresponds to prob of missingness for that index. 

missing_dist_5 <- missing_dist[-6] #remove the 0 probability for 5 missing 
missing_dist_Alaska_df <- as.data.frame(missing_dist_5)
missing_dist_Alaska_df$index <- as.factor(c(0,1,2,3,4))
ggplot(missing_dist_Alaska_df, aes(x = index, y = missing_dist_5)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Distribution of Missingness", x = "Number of Missing Ranks", y = "Probability") +
  theme_minimal()   +
  theme(plot.title = element_text(hjust = 0.4, face = "bold",size=22.5),
                          axis.text.x = element_text(size = 20),
                          axis.text.y = element_text(size = 20),
                          axis.title.y = element_text(size = 22),
                          axis.title.x = element_text(size = 22))



prob_cand_gov_df <- read.csv("Alaska_cand_probs.csv",row.names = 1)
prob_cand_gov_df$candidates <- c("Walker","Pierce","Gara","Dunleavy","Write-in")
ggplot(prob_cand_gov_df, aes(x = candidates, y = x)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "", x = "", y = "Probability") +
  theme_minimal()   +
  theme(plot.title = element_text(hjust = 0.4, face = "bold",size=22.5),
        axis.text.x = element_text(size = 20,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22))


results_AL <- read.csv("sim_AL_better.csv", row.names = 1)
results_AL$Voters = as.factor(results_AL$Voters)
ggplot(results_AL , aes(x = Voters, y= Agreement, fill=Voters)) +
  geom_boxplot() +
  labs(title = "AK Gov. Election w/ & w/o Missingness", x = "Number of Voters", y = "Agreement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=22),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.position = "none") +
  scale_fill_manual(values = c("lightgreen","lightblue", "pink")) + 
  ylim(0.75, 1)







############### MINNEAPOLIS ###############


#entry 1: probability of missing all 3 votes (0)
#entry 2: probability of missing 2 votes (the last 2)
#entry 3: probability of missing 1 vote (the last 1)
#entry 4: probability of missing 0 votes 
missing_probs_2021_MN <- read.csv("missing_prob_2021_MN.csv",row.names = 1)
missing_probs_2021_MN <- as.vector(missing_probs_2021_MN)$x
missing_probs_2021_MN[4] <- 1 - sum(missing_probs_2021_MN)
missing_dist_MN <- rev(missing_probs_2021_MN) #reverse the probs, so entry corresponds to prob of missingness for that index. 

missing_dist_MN_3 <-missing_dist_MN[-4] #remove the 0 probability for 3 missing 
missing_dist_MN_df <- as.data.frame(missing_dist_MN_3)
missing_dist_MN_df$index <- as.factor(c(0,1,2))
ggplot(missing_dist_MN_df, aes(x = index, y = missing_dist_MN_3)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Missingness in 2021 MN Election", x = "Number of Missing Ranks", y = "Probability") +
  theme_minimal()   +
  theme(plot.title = element_text(hjust = 0.4, face = "bold",size=22.5),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22))


prob_cand_MN_df <- read.csv("2021_MN_cand_probs.csv",row.names = 1)
prob_cand_MN_df$candidates <- rownames(prob_cand_MN_df)
prob_cand_MN_df$candidates_lastname <- c("Frey","Globus","Carney Jr","Nezhad","Knuth","Winter","Conner","Turner","Benjegerdes","Perry", "Awed","Nelson",
                                         "Harcus","David","Ward","Atkins","UWI","Johnson","Zimmerman")
prob_cand_MN_df_sorted <- prob_cand_MN_df[order(prob_cand_MN_df$x,decreasing=TRUE), ]
prob_cand_MN_df_sorted$candidates_lastname <- factor(prob_cand_MN_df_sorted$candidates_lastname, levels = prob_cand_MN_df_sorted$candidates_lastname)

ggplot(prob_cand_MN_df_sorted, aes(x = candidates_lastname, y = x)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "", x = "", y = "Probability") +
  theme_minimal()   +
  theme(plot.title = element_text(hjust = 0.4, face = "bold",size=22.5),
        axis.text.x = element_text(size = 20,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22))

results_MN <- read.csv("sim_2021_MN.csv", row.names = 1)
results_MN$Voters = as.factor(results_AL$Voters)
ggplot(results_MN , aes(x = Voters, y= Agreement, fill=Voters)) +
  geom_boxplot() +
  labs(title = "2021 MN Election w/ & w/o Strategic Voting ", x = "Number of Voters", y = "Agreement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.position = "none") +
  scale_fill_manual(values = c("lightgreen","lightblue", "pink")) + 
  ylim(0.75, 1)

############### STRATEGIC MISSINGNESS SIMULATION ###############
results_strategy4 <- read.csv("strategy4_final_wMiss_55_2_15_1_try2.csv", row.names = 1)

empty <- results_strategy4[1:8,]
empty$Frac4Wins <- 0
empty$Frac4Wins_Miss <- 0
empty$Prop2 <- rev(seq(0.36,0.43,.01))
empty$Prop1 <- 1 - rev(seq(0.36,0.43,.01))
empty$Agreement <- 1

results_strategy4 <- rbind(results_strategy4, empty)
results_strategy4.10000 <- results_strategy4[which(results_strategy4$Voters==10000),]
results_strategy4.10000.orig <- results_strategy4.10000[,c("Voters", "Prop1","Prop2","Frac4Wins")]
results_strategy4.10000.orig$Missing <- 0
colnames(results_strategy4.10000.orig) <- c("Voters", "Prop1","Prop2","Frac4Wins","Missing")
results_strategy4.10000.missing <- results_strategy4.10000[,c("Voters", "Prop1","Prop2","Frac4Wins_Miss")]
results_strategy4.10000.missing$Missing <- 1
colnames(results_strategy4.10000.missing) <- c("Voters", "Prop1","Prop2","Frac4Wins","Missing")
results_strategy4.10000.long <- rbind(results_strategy4.10000.orig,results_strategy4.10000.missing)
results_strategy4.10000.long$Missing <- factor(results_strategy4.10000.long$Missing, levels=c(0,1))



ggplot(results_strategy4.10000.long, aes(x = Prop2, y = Frac4Wins,fill=Missing)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.6) +
  labs(title = "Pop 10,000, 4 vs 1 Vote in Group 2, P = (.55,.2,.15,.1)", x = "Proportion of Population: Group 2 ", y = "Frac. Elections Cand. D Wins") +
  theme_minimal()   +
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size=17),
        axis.text.x = element_text(size = 17,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 19),
        axis.title.x = element_text(size = 19),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))+
  scale_fill_manual(values = c("0" = "orange", "1" = "blue")) +
  scale_x_continuous(breaks = seq(0.36, 0.66,0.03)) 

results_strategy4_only <- read.csv("strategy4_final_55_2_15_1_try2.csv", row.names = 1)


results_strategy4_only$Missing <- 1

empty.2 <- rbind(results_strategy4_only[1:10,], results_strategy4_only[1:10,])
empty.2$Frac4Wins <- 1
empty.2$Prop2 <- rev(seq(0.47,0.66,.01))
empty.2$Prop1 <- 1 -  rev(seq(0.47,0.66,.01))

results_strategy4_only<- rbind(results_strategy4_only, empty.2)

results_strategy4_only.10000 <-results_strategy4_only[which(results_strategy4_only$Voters==10000),]

ggplot(results_strategy4_only.10000, aes(x = Prop2, y = Frac4Wins, fill = Missing)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.6) +
  labs(title =  "Pop 10,000, 1 Vote in Group 2, P = (1,0,0,0)", x = "Proportion of Population: Group 2 ", y = "Frac. Elections Cand. D Wins",fill="Missing") +
  theme_minimal()   +
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size=17),
        axis.text.x = element_text(size = 17,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 19),
        axis.title.x = element_text(size = 19),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))+
  scale_x_continuous(breaks = seq(0.36, 0.66,0.03)) 

results_strategy4.1000 <- read.csv("strategy4_final_wMiss_55_2_15_1_try2_1000.csv", row.names = 1)

empty <- results_strategy4.1000[1:8,]
empty$Frac4Wins <- 0
empty$Frac4Wins_Miss <- 0
empty$Prop2 <- rev(seq(0.36,0.43,.01))
empty$Prop1 <- 1 - rev(seq(0.36,0.43,.01))
empty$Agreement <- 1

results_strategy4.1000 <- rbind(results_strategy4.1000, empty)


results_strategy4.1000.orig <- results_strategy4.1000[,c("Voters", "Prop1","Prop2","Frac4Wins")]
results_strategy4.1000.orig$Missing <- 0
colnames(results_strategy4.1000.orig) <- c("Voters", "Prop1","Prop2","Frac4Wins","Missing")
results_strategy4.1000.missing <- results_strategy4.1000[,c("Voters", "Prop1","Prop2","Frac4Wins_Miss")]
results_strategy4.1000.missing$Missing <- 1
colnames(results_strategy4.1000.missing) <- c("Voters", "Prop1","Prop2","Frac4Wins","Missing")
results_strategy4.1000.long <- rbind(results_strategy4.1000.orig,results_strategy4.1000.missing)
results_strategy4.1000.long$Missing <- factor(results_strategy4.1000.long$Missing, levels=c(0,1))



ggplot(results_strategy4.1000.long, aes(x = Prop2, y = Frac4Wins,fill=Missing)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.6) +
  labs(title = "Pop 1,000, 4 vs 1 Vote in Group 2, P = (.55,.2,.15,.1)", x = "Proportion of Population: Group 2 ", y = "Frac. Elections Cand. D Wins") +
  theme_minimal()   +
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size=17),
        axis.text.x = element_text(size = 17,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 19),
        axis.title.x = element_text(size = 19),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))+
  scale_fill_manual(values = c("0" = "orange", "1" = "blue")) +
  scale_x_continuous(breaks = seq(0.36, 0.66,0.03)) 


results_strategy4_only.1000  <- read.csv("strategy4_final_55_2_15_1_try2_1000.csv", row.names = 1)

results_strategy4_only.1000 $Missing <- 1

empty.2 <- rbind(results_strategy4_only.1000 [1:10,], results_strategy4_only.1000 [1:10,])
empty.2$Frac4Wins <- 1
empty.2$Prop2 <- rev(seq(0.47,0.66,.01))
empty.2$Prop1 <- 1 -  rev(seq(0.47,0.66,.01))

results_strategy4_only.1000 <- rbind(results_strategy4_only.1000 , empty.2)

ggplot(results_strategy4_only.1000, aes(x = Prop2, y = Frac4Wins, fill = Missing)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.6) +
  labs(title = "Pop 1,000, 1 Vote in Group 2, P = (1,0,0,0)", x = "Proportion of Population: Group 2 ", y = "Frac. Elections Cand. D Wins",fill="Missing") +
  theme_minimal()   +
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size=17),
        axis.text.x = element_text(size = 17,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 19),
        axis.title.x = element_text(size = 19),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))+
  scale_x_continuous(breaks = seq(0.36, 0.66,0.03)) 