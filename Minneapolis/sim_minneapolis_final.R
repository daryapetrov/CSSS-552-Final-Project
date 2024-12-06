library(vote)
#data_2021_MN <- read.csv("2021_MN_restructed_valid.csv",row.names = 1)

#probs of candidates from 2021 MN election
prob_2021_MN <- read.csv("2021_MN_cand_probs.csv",row.names = 1)
candidates <- rownames(prob_2021_MN)
n_candidates <- length(candidates)
cand_probs_2021_MN <- as.vector(prob_2021_MN)$x

#entry 1: probability of missing all 3 votes (0)
#entry 2: probability of missing 2 votes (the last 2)
#entry 3: probability of missing 1 vote (the last 1)
#entry 4: probability of missing 0 votes 
missing_probs_2021_MN <- read.csv("missing_prob_2021_MN.csv",row.names = 1)
missing_probs_2021_MN <- as.vector(missing_probs_2021_MN)$x
missing_probs_2021_MN[4] <- 1 - sum(missing_probs_2021_MN)
missing_dist <- rev(missing_probs_2021_MN) #reverse the probs, so entry corresponds to prob of missingness for that index. 

#missing_dist:
#entry 1: probability of having 1 missing vote (i.e. the last one)
#entry 2: probability of having 2 missing votes (i.e. the last two)
#entry 3: probability of 3 missing votes (i.e. 0, this is impossible)


sim_RCV_missingness <- function(candidates, n_voters,probs, missing_dist, sims=100, iter=0,rank_max=3){
  n_candidates <- length(candidates)
  
  elected_missing <- c()
  elected_strategic <- c()
  
  for(sim in 1:sims){
    
    #I need to set a seed because the count.votes() function resets the seed - aggravating!!
    set.seed(sim + sims*iter) # + sims*iter ensures that I get different seeds for each of my iterations
    
    #each column is a voter
    voter_preferences <- replicate(n_voters, sample(candidates,size = rank_max, prob=probs)) #sampling w/o replacement 
    
    
    ### WITHOUT STRATEGIC MISSINGNESS, but regular missingness is present ###
    
    #sample amount of missingness for each voter
    possible_missingness <- 0:rank_max #each voter must have at least one vote
    
    voter_missingness <- replicate(n_voters, sample(possible_missingness,size=1,prob=missing_dist)) #this is the amount of missed ranks per candidate
    
    voter_preferences_missing <- voter_preferences
    
    #introduce missingness into the data 
    for(voter in 1:n_voters){
      n_missing <- voter_missingness[voter]
      if(n_missing == 0){
        next
      }
      else{
        #if 1 missing, voter_preferences_missing[rank_max,voter] = NA
        voter_preferences_missing[c(rank_max - 0:(n_missing-1)),voter] = NA
      }
    }
    
    #restructure data for use with "vote" packages, i.e. numerically rank candidates with 1 being the most preferred
    #now rows are voters
    preferences.vote_missing <- as.data.frame(matrix(nrow = n_voters,ncol = n_candidates))
    colnames(preferences.vote_missing) <- candidates
    
    #determine rank of candidates for each voter, or 0 if no rank (i.e. missing data)
    for(candidate in candidates){
      for(voter in 1:n_voters){
        rank = which(voter_preferences_missing[,voter] == candidate)
        if(length(rank)>0){
          preferences.vote_missing[voter,candidate] <- which(voter_preferences_missing[,voter] == candidate)
        }
        else{preferences.vote_missing[voter,candidate] <- 0}
      }
    }
    sink(tempfile())
    res_missing <- count.votes(preferences.vote_missing, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting
    sink()
    elected_missing[sim] <- res_missing$elected
    
    
    ### WITH STRATEGIC MISSINGNESS###
    preferences.vote_strategic <- preferences.vote_missing
    
    #Now, I want my missing data to be that everyone who votes for Knuth or Nezhad in 1st, they don't vote for FREY
    
    #this part only works when rank_max=3
    for(voter in 1:n_voters){
      rank_F <- preferences.vote_missing[voter,"Jacob Frey"]
      rank_K <- preferences.vote_missing[voter,"Kate Knuth"]
      rank_N <- preferences.vote_missing[voter,"Sheila Nezhad"]
      if(rank_F == 3 & (rank_K ==1 | rank_N == 1)){
        preferences.vote_strategic[voter,"Jacob Frey"] = 0
      }
      if(rank_F == 2 & (rank_K ==1 | rank_N == 1)){
        preferences.vote_strategic[voter,"Jacob Frey"] = 0
        rank3_candidate <- which(preferences.vote_missing[voter,]==3)
        if(length(rank3_candidate)>0){ #someone was ranked 3rd
          preferences.vote_strategic[voter,rank3_candidate] = 2
        }
      }
    }
    
    
    sink(tempfile())
    res_strategic <- count.votes(preferences.vote_strategic, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting
    sink()
    elected_strategic[sim] <- res_strategic$elected
    
  }
  return(list(elected_missing,elected_strategic))
}

res <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(res) <- c("Voters","Simulation","Agreement") 

n_voters <- c(100,1000,10000) #100,000 was taking too long. 
n_sims <- 100 #number of iterations per candidate/voter pair to make confidence interval
sims= 100 #simulations per each iteration


set.seed(10)

i=0
for(v in n_voters){
  start_time <- Sys.time()
  for(s in 1:n_sims){
    if(s/5==1){
      cat(v,"voters",", iteration", i,"\n")
    }
    i=i+1
    output <- sim_RCV_missingness(candidates=candidates, n_voters=v,probs=cand_probs_2021_MN, missing_dist=missing_dist, sims=sims,iter=s)
    reg_missing <- output[[1]]
    reg_strategic <- output[[2]]
    res[i,c("Voters","Simulation")] <- c(v,s)
    res[i,"Agreement"] <- sum(reg_missing == reg_strategic)/sims
  }
  end_time <- Sys.time()
  cat(v,"voters", end_time - start_time,"minutes", "\n")
}




write.csv(res, file = "sim_2021_MN.csv", row.names = TRUE)













