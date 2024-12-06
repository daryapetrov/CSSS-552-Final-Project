library(vote)


#probs of candidates from Alaska governor election. 
prob_cand_gov <- read.csv("prob_candidates_AK_gov.csv",row.names = 1)
candidates <- colnames(prob_cand_gov)
n_candidates <- length(candidates)
prob_cand_gov <- as.vector(t(prob_cand_gov)[,1])

#distribution of missingness from alaska governor election. 
# (ex. 2 = missingness occured at rank 2, so only 1 candidate ranked, so 3 candidates not ranked)
# a # represents the first position that missingness occured. 
missing_dist_gov <- read.csv("missing_dist_gov.csv",row.names = 1)
# a # represents the number of candidates each voter ranked
missing_dist_gov <- missing_dist_gov - 1
# a # represents the number of candidates the voter didn't rank
missing_dist_gov <- n_candidates - missing_dist_gov$x

# prob a voter didn't rank 0, i.e. ranked all 4 of them = 0.1738013


#I will put the probabilities of the candidates in the parameter "probs"
#I will put the missingness prob in the parameter "missing_dist"
sim_RCV_missingness <- function(candidates, n_voters,sims=100, iter=0,probs=NA,missing_dist){
  n_candidates <- length(candidates)
  
  elected_orig <- c()
  elected_missing <- c()
  
  
  for(sim in 1:sims){
    
    #I need to set a seed because the count.votes() function resets the seed - aggravating!!
    set.seed(sim + sims*iter) # + sims*iter ensures that I get different seeds for each of my iterations
    
    #simulate voter preferences w/ equal probabilities for each candidate
    probs <- rep(1/n_candidates,n_candidates) 
    
    voter_preferences <- replicate(n_voters, sample(candidates,prob=probs)) #sampling w/o replacement 
    
    
    ### WITHOUT MISSINGNESS ###
    
    #restructure data for use with "vote" packages, i.e. numerically rank candidates with 1 being the most preferred
    preferences.vote_orig <- as.data.frame(matrix(nrow = n_voters,ncol = n_candidates))
    colnames(preferences.vote_orig) <- candidates
    
    for(candidate in candidates){
      for(voter in 1:n_voters){
        preferences.vote_orig[voter,candidate] <- which(voter_preferences[,voter] == candidate)
      }
    }
    
    sink(tempfile())
    res_orig <- count.votes(preferences.vote_orig, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting
    sink()
    elected_orig[sim] <- res_orig$elected
    
    
    ### NOW SIMULATE MISSINGNESS ###
    
    #sample amount of missingness for each voter
    possible_missingness <- 0:(n_candidates-1) #each voter must have at least one vote
    
    #THIS LINE OF CODE IS WHAT I NEED TO CHANGE
    voter_missingness <- replicate(n_voters, sample(missing_dist,1)) #this is the amount of missed ranks per candidate 
    
    voter_preferences_missing <- voter_preferences
    
    #introduce missingness into the data 
    for(voter in 1:n_voters){
      n_missing <- voter_missingness[voter]
      if(n_missing == 0){
        next
      }
      else{
        voter_preferences_missing[c(n_candidates - 0:(n_missing-1)),voter] = NA
      }
    }
    
    #restructure data for use with "vote" packages, i.e. numerically rank candidates with 1 being the most preferred
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
    
  }
  return(list(elected_orig, elected_missing))
}


res <- as.data.frame(matrix(nrow=1,ncol=4))
colnames(res) <- c("Candidates","Voters","Simulation","Agreement") 

n_candidates <- c(length(candidates))
n_voters <- c(100,1000,10000) #100,000 was taking too long. 
n_sims <- 100 #number of iterations per candidate/voter pair to make confidence interval
sims= 100 #simulations per each iteration


set.seed(10)

i=0
for(c in n_candidates){
  for(v in n_voters){
    for(s in 1:n_sims){
      i=i+1
      output <- sim_RCV_missingness(candidates=candidates[1:c], n_voters=v,sims=sims,iter=s,probs=prob_cand_gov,missing_dist=missing_dist_gov)
      reg_orig <- output[[1]]
      reg_missing <- output[[2]]
      res[i,c("Candidates","Voters","Simulation")] <- c(c,v,s)
      res[i,"Agreement"] <- sum(reg_orig == reg_missing)/sims
    }
  }
}

write.csv(res, file = "alaska_gov_sim.csv", row.names = TRUE)
