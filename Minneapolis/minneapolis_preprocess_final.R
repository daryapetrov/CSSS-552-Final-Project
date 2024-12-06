

library(vote)

data2021 <- read.csv("2021-Mayor-Cast-Vote-Record.csv")

unique(data2021$X1st.Choice)


#https://www.revisor.mn.gov/rules/8220/full
#315 overvotes. In Minneapolis, these are discareded. 
#The computer program must not count the votes cast by a voter for an office or question if the number of votes cast exceeds the number which the voter is entitled to vote for on that office or question, but it must record that there is an overvote condition as referred to in part 8220.0450, item C.
subset_overvote <- c()
for(row in 1:dim(data2021)[1]){
  if("overvote" %in% data2021[row,]){
    subset_overvote = append(subset_overvote,row)
  }
}


#remove the overvotes & remove the first and last row, not useful info. 
data2021.cleaned <- data2021[-subset_overvote,-c(1,5)]


options <- union(union(unique(data2021.cleaned$X1st.Choice), unique(data2021.cleaned$X2nd.Choice)),unique(data2021.cleaned$X3rd.Choice))

candidates <- options[options != "undervote"] #there are 18 candidates voted for. 
n_candidates <- length(candidates)
n_voters <- dim(data2021.cleaned)[1]

#CALCULCATE PROB OF EACH CANDIDATE

preferences.vote_orig <- as.data.frame(matrix(nrow = n_voters,ncol = n_candidates))
colnames(preferences.vote_orig) <- candidates

for(candidate in candidates){
  for(voter in 1:n_voters){
    rank = which(data2021.cleaned[voter,] == candidate)
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
invalidVotes <- invalid.votes(stv(preferences.vote_orig))
rows_invalidVotes <- as.numeric(rownames(invalidVotes))
data2021.cleaned.valid <- data2021.cleaned[-rows_invalidVotes,] 
n_voters <- dim(data2021.cleaned.valid)[1]
preferences.vote_orig.valid <- preferences.vote_orig[-rows_invalidVotes,] 


write.csv(preferences.vote_orig.valid, file = "2021_MN_restructed_valid.csv", row.names = TRUE)

res_orig <- count.votes(preferences.vote_orig.valid, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting

total <- sum(res_orig$preferences[1,]) #might need to change this line of code.
probabilities <- res_orig$preferences[1,]/ total

write.csv(probabilities, file = "2021_MN_cand_probs.csv", row.names = TRUE)


#CALCULCATE MISSINGESS PROBABILITY

#this is 0 because I removed invalid votes
subset_undervote_1 <- c()
for(row in 1:dim(data2021.cleaned.valid)[1]){
  if("undervote" == data2021.cleaned.valid[row,1]){
    subset_undervote_1 = append(subset_undervote_1,row)
  }
}
subset_undervote_2 <- c()
for(row in 1:dim(data2021.cleaned.valid)[1]){
  if("undervote" == data2021.cleaned.valid[row,2]){
    subset_undervote_2 = append(subset_undervote_2,row)
  }
}
subset_undervote_3 <- c()
for(row in 1:dim(data2021.cleaned.valid)[1]){
  if("undervote" != data2021.cleaned.valid[row,2] & "undervote" == data2021.cleaned.valid[row,3]){
    subset_undervote_3 = append(subset_undervote_3,row)
  }
}

n_undervote_1 <- length(subset_undervote_1)
n_undervote_2 <- length(subset_undervote_2)
n_undervote_3 <- length(subset_undervote_3)

missingness_prob <- c(n_undervote_1,n_undervote_2,n_undervote_3)*(1/n_voters)
write.csv(missingness_prob, file = "missing_prob_2021_MN.csv", row.names = TRUE)
