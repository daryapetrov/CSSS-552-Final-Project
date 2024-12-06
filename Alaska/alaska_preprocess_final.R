library(vote)


alaska.gov <- read.csv("cvr_Alaska_11082022_GovernorLieutenantGovernor.csv")
alaska.gov <- as.data.frame(alaska.gov)

#intersect should be union, but it actually doesn't make a difference
options <- intersect(intersect(intersect(intersect(unique(alaska.gov$rank1), unique(alaska.gov$rank2)),unique(alaska.gov$rank3)),unique(alaska.gov$rank4)),unique(alaska.gov$rank5))
candidates <- c("Walker/Drygas","Pierce/Grunwald","Gara/Cook","Dunleavy/Dahlstrom","Write-in")
n_candidates <- length(candidates)
other <- c("skipped","overvote","Write-in")

alaska.gov.ranks <- alaska.gov[, c("rank1","rank2","rank3","rank4","rank5")] # 266693      5


subset_overvote <- c()
for(row in 1:dim(alaska.gov.ranks)[1]){
  if("overvote" %in% alaska.gov.ranks[row,]){
    subset_overvote = append(subset_overvote,row)
  }
}
alaska.gov.clean <- alaska.gov.ranks[-subset_overvote,]
n_voters <- dim(alaska.gov.clean)[1]

preferences.vote_orig <- as.data.frame(matrix(nrow = n_voters,ncol = n_candidates))
colnames(preferences.vote_orig) <- candidates

for(candidate in candidates){
  for(voter in 1:n_voters){
    rank = which(alaska.gov.clean[voter,] == candidate)
    if(length(rank)==1){
      preferences.vote_orig[voter,candidate] <- rank
    }else if(length(rank)>1){
      highest_rank = min(rank)
      preferences.vote_orig[voter,candidate] <- highest_rank
    }else{preferences.vote_orig[voter,candidate] <- 0}
  }
}


res_orig <- count.votes(preferences.vote_orig, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting



#I'm going to remove the invalid votes from my dataset so that they don't mess with the missingness
invalidVotes <- invalid.votes(stv(preferences.vote_orig)) #6271
rows_invalidVotes <- as.numeric(rownames(invalidVotes))
alaska.gov.clean.valid <- alaska.gov.clean[-rows_invalidVotes,]  #259611 obs
n_voters <- dim(alaska.gov.clean.valid)[1]
preferences.vote_orig.valid <- preferences.vote_orig[-rows_invalidVotes,] 


write.csv(preferences.vote_orig.valid, file = "Alaska_restructed_valid.csv", row.names = TRUE)

res_orig <- count.votes(preferences.vote_orig.valid, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting

total <- sum(res_orig$preferences[1,]) 
probabilities <- res_orig$preferences[1,]/ total

write.csv(probabilities, file = "Alaska_cand_probs.csv", row.names = TRUE)




#CALCULCATE MISSINGESS PROBABILITY

#this is 0 because I removed invalid votes
subset_undervote_1 <- c()
for(row in 1:dim(alaska.gov.clean.valid)[1]){
  if("skipped" == alaska.gov.clean.valid[row,1]){
    subset_undervote_1 = append(subset_undervote_1,row)
  }
}
subset_undervote_2 <- c()
for(row in 1:dim(alaska.gov.clean.valid)[1]){
  if("skipped" == alaska.gov.clean.valid[row,2]){
    subset_undervote_2 = append(subset_undervote_2,row)
  }
}
subset_undervote_3 <- c()
for(row in 1:dim(alaska.gov.clean.valid)[1]){
  if("skipped" != alaska.gov.clean.valid[row,2] & "skipped" == alaska.gov.clean.valid[row,3]){
    subset_undervote_3 = append(subset_undervote_3,row)
  }
}
subset_undervote_4 <- c()
for(row in 1:dim(alaska.gov.clean.valid)[1]){
  if("skipped" != alaska.gov.clean.valid[row,2] & "skipped" != alaska.gov.clean.valid[row,3] & "skipped" == alaska.gov.clean.valid[row,4]){
    subset_undervote_4 = append(subset_undervote_4,row)
  }
}
subset_undervote_5 <- c()
for(row in 1:dim(alaska.gov.clean.valid)[1]){
  if("skipped" != alaska.gov.clean.valid[row,2] & "skipped" != alaska.gov.clean.valid[row,3] & "skipped" != alaska.gov.clean.valid[row,4] & "skipped" == alaska.gov.clean.valid[row,5]){
    subset_undervote_5 = append(subset_undervote_5,row)
  }
}


n_undervote_1 <- length(subset_undervote_1)
n_undervote_2 <- length(subset_undervote_2)
n_undervote_3 <- length(subset_undervote_3)
n_undervote_4 <- length(subset_undervote_4)
n_undervote_5 <- length(subset_undervote_5)

missingness_prob <- c(n_undervote_1,n_undervote_2,n_undervote_3,n_undervote_4,n_undervote_5)*(1/n_voters)
write.csv(missingness_prob, file = "missing_prob_Alaska.csv", row.names = TRUE)




















