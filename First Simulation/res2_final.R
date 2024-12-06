
library(vote)

#this is only going to work for two or three high candidates. 
sim_RCV_missingness <- function(candidates, n_voters,sims=100, equalp=FALSE,nhigh = 3,phigh=0.75,iter=0){
  n_candidates <- length(candidates)
  
  elected_orig <- c()
  elected_missing <- c()
  
  
  for(sim in 1:sims){
    
    #I need to set a seed because the count.votes() function resets the seed - aggravating!!
    set.seed(sim + sims*iter) # + sims*iter ensures that I get different seeds for each of my iterations
    
    #simulate voter preferences w/ equal probabilities for each candidate
    probs <- rep(1/n_candidates,n_candidates) 
    
    if(equalp==FALSE){
      
      rndm <- runif(1,min=1,max=3)/100 #random perturbation of high probabilities 
      
      if(n_candidates==nhigh){
        phigh <- 1
      }
      
      #perturbate high probabilities
      if(nhigh==3){
        operation <- sample(c(0,-1,1)) 
      }
      else{operation <- sample(c(-1,1)) }
      
      for(i in 1:n_candidates){
        probs[i] <- phigh/nhigh + operation[i]*rndm
      }
      
      #assign low probabilities 
      if(phigh != 1){
        nlow = n_candidates - nhigh
        plow = 1-phigh
        for(i in (nhigh+1):n_candidates){
          probs[i] = plow/nlow
        }
      }
    
    }
    
    
    #check that probabilities sum to 1
    if(all.equal(sum(probs),1) == FALSE){
      print("Error:probabilities did not sum to 1")
    }

        
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
    voter_missingness <- replicate(n_voters, sample(possible_missingness,1))
    
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


candidates <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

n_candidates <- c(3,6,10)
n_voters <- c(100,1000,10000) #100,000 was taking too long. 
n_sims <- 100 #number of iterations per candidate/voter pair to make confidence interval
sims=100 #simulations per each iteration


set.seed(10)

i=0
for(c in n_candidates){
  for(v in n_voters){
    start_time <- Sys.time()
    for(s in 1:n_sims){
      i=i+1
      output <- sim_RCV_missingness(candidates=candidates[1:c], n_voters=v,sims=sims,nhigh = 2, iter=s)
      reg_orig <- output[[1]]
      reg_missing <- output[[2]]
      res[i,c("Candidates","Voters","Simulation")] <- c(c,v,s)
      res[i,"Agreement"] <- sum(reg_orig == reg_missing)/sims
    }
    end_time <- Sys.time()
    cat(v,"voters", c, "candidates", "\n")
    print(as.numeric(end_time - start_time, units = "mins")) 
  }
}



# save results to csv file
write.csv(res, file = "nhigh2_100_1000_10000_phigh75_try2.csv", row.names = TRUE)





























