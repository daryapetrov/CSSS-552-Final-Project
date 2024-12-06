#I will first do the following simulation: 
#There are 4 candidates. 
#One subset of the population has the following preference, 1>2>3>4.
#The rest of the population only likes 4, and do not like the idea of redistributing their vote, 
#so they are only going to vote for 4. 
#Q: How big does the second group need to be to get 4 to win? 

library(vote)

candidates <- c("1","2","3","4")

group2.props <- rev(seq(0.44, 0.66,0.01))
group1.props <-1-group2.props

group1.probs <- c(.55,.2,.15,.1) #"1">"2">"3">"4"


sim_RCV_strategic <- function(candidates,n.voters,group1.prop,group1.probs, sims=100, iter=0){
  
  group2.probs <- rev(group1.probs)
  candidates.n <- length(candidates)
  group2.prop <- 1 - group1.prop
  
  group1.n <- round(group1.prop*n.voters)
  group2.n <- round(group2.prop*n.voters)
  n.voters <- group1.n + group2.n
  
  elected_all <- c()
  elected_missing <- c()
  
  for(sim in 1:sims){
    
    set.seed(sim + sims*iter)
    
    group1.voter_preferences <- replicate(group1.n, sample(candidates, prob=group1.probs)) #voters preferences are the columns, 4 x n
    group2.voter_preferences <- replicate(group2.n, sample(candidates, prob=group2.probs)) #voters preferences are the columns, 4 x n
    
    
    all_voter_preferences <- cbind(group1.voter_preferences,group2.voter_preferences)
    
    #restructure data for use with "vote" packages, i.e. numerically rank candidates with 1 being the most preferred
    #now rows are voters
    all_rank_data <- as.data.frame(matrix(nrow = n.voters,ncol = candidates.n ))
    colnames(all_rank_data) <- candidates
    
    #determine rank of candidates for each voter, or 0 if no rank (i.e. missing data)
    for(candidate in candidates){
      for(voter in 1:n.voters){
        rank = which(all_voter_preferences[,voter] == candidate)
        if(length(rank)>0){
          all_rank_data[voter,candidate] <- rank
        }
        else{all_rank_data[voter,candidate] <- 0}
      }
    }
    
    sink(tempfile())
    res_election_all <- count.votes(all_rank_data, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting
    sink()
    elected_all[sim] <- res_election_all$elected
    
    ###### WITH MISSINGNESS, i.e. no diluting vote for one group #######
    
    #make all data NA except the first vote 
    group2.voter_preferences_missing <- matrix(NA,nrow=candidates.n ,ncol=group2.n)
    group2.voter_preferences_missing[1,] <- group2.voter_preferences[1,]
    
    all_voter_preferences_miss <- cbind(group1.voter_preferences,group2.voter_preferences_missing)
    
    #restructure data for use with "vote" packages, i.e. numerically rank candidates with 1 being the most preferred
    #now rows are voters
    all_rank_data_miss <- as.data.frame(matrix(nrow = n.voters,ncol = candidates.n ))
    colnames(all_rank_data_miss) <- candidates
    
    #determine rank of candidates for each voter, or 0 if no rank (i.e. missing data)
    for(candidate in candidates){
      for(voter in 1:n.voters){
        rank = which(all_voter_preferences_miss[,voter] == candidate)
        if(length(rank)>0){
          all_rank_data_miss[voter,candidate] <- rank
        }
        else{all_rank_data_miss[voter,candidate] <- 0}
      }
    }
    
    sink(tempfile())
    res_election_miss <- count.votes(all_rank_data_miss, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting
    sink()
    elected_missing[sim] <- res_election_miss$elected
    
  
  }
  
  return(list(elected_all,elected_missing))

}


res <- as.data.frame(matrix(nrow=1,ncol=7))
colnames(res) <- c("Voters","Prop1","Prop2","Simulation","Frac4Wins","Frac4Wins_Miss","Agreement") 

n_voters <- c(1000)#c(100,1000,10000) #THIS NEEDS TO BE EVEN NUMBER
n_sims <- 1 #number of iterations per candidate/voter pair to make confidence interval
sims= 1000 #simulations per each iteration


set.seed(10)

# n.voters = 10
#s = 1
# group1.prop=0.5
# candidates <- c("1","2","3","4")
# group1.probs <- c(.97,.01,.01,.01) 
# sims=100
#group1.prop=0.8


i=0
for(v in n_voters){
  for(p in group1.props){
    start_time <- Sys.time()
    for(s in 1:n_sims){
      i=i+1
      output <- sim_RCV_strategic(candidates=candidates,n.voters=v,group1.prop=p,group1.probs=group1.probs, sims=sims,iter=s)
      reg_elected_all <- output[[1]]
      reg_elected_miss <-  output[[2]]
      res[i,c("Voters","Prop1","Prop2","Simulation")] <- c(v,p,1-p,s)
      res[i,"Frac4Wins"] <- sum(reg_elected_all == "4")/sims
      res[i,"Frac4Wins_Miss"] <- sum(reg_elected_miss == "4")/sims
      res[i,"Agreement"] <- sum(reg_elected_all == reg_elected_miss)/sims
    }
    end_time <- Sys.time()
    cat(v,"voters", p, "p", "\n")
    print(as.numeric(end_time - start_time, units = "mins")) 
  }
}

# n.voters=10
# group1.prop = 0.8
# sims=10
# iter=1
# sim=1



write.csv(res, file = "strategy4_final_wMiss_55_2_15_1_try2_1000.csv", row.names = TRUE)
