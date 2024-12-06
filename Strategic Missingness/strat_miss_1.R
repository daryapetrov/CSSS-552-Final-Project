#I will first do the following simulation: 
#There are 4 candidates. 
#One subset of the population has the following preference, 1>2>3>4.
#The rest of the population only likes 4, and do not like the idea of redistributing their vote, 
#so they are only going to vote for 4. 
#Q: How big does the second group need to be to get 4 to win? 

library(vote)

candidates <- c("1","2","3","4")

group2.props <- rev(seq(0.36,0.46,0.01))
group1.props <-1-group2.props

group1.probs <- c(.55,.2,.15,.1) #"1">"2">"3">"4"

#group2.probs <- c(.01,.01,.01,.97) #"1"<"2"<"3"<"4"


sim_RCV_strategic <- function(candidates,n.voters,group1.prop,group1.probs, sims=100, iter=0){
  
  candidates.n <- length(candidates)
  group2.prop <- 1 - group1.prop
  
  group1.n <- round(group1.prop*n.voters)
  group2.n <- round(group2.prop*n.voters)
  n.voters <- group1.n + group2.n
  
  elected <- c()
  
  for(sim in 1:sims){
    
    set.seed(sim + sims*iter)
    
    group1.voter_preferences <- replicate(group1.n, sample(candidates, prob=group1.probs)) #voters preferences are the columns, 4 x n
    
    #they only vote for 4. 
    group2.voter_preferences <- matrix(NA,nrow=candidates.n ,ncol=group2.n)
    group2.voter_preferences[1,]=4
    
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
    res_election <- count.votes(all_rank_data, method="stv", nseats = 1) #stv = single transferable vote = rank choice voting
    sink()
    elected[sim] <- res_election$elected
  
  }
  
  return(elected)

}


res <- as.data.frame(matrix(nrow=1,ncol=5))
colnames(res) <- c("Voters","Prop1","Prop2","Simulation","Frac4Wins") 

n_voters <- c(1000)#c(100,1000,10000) #100,000 was taking too long. #THIS NEEDS TO BE EVEN NUMBER
n_sims <- 1 #number of iterations per candidate/voter pair to make confidence interval
sims= 1000 #simulations per each iteration


set.seed(10)

i=0
for(v in n_voters){
  for(p in group1.props){
    start_time <- Sys.time()
    for(s in 1:n_sims){
      i=i+1
      output <- sim_RCV_strategic(candidates=candidates,n.voters=v,group1.prop=p,group1.probs=group1.probs, sims=sims,iter=s)
      reg_elected <- output
      res[i,c("Voters","Prop1","Prop2","Simulation")] <- c(v,p,1-p,s)
      res[i,"Frac4Wins"] <- sum(reg_elected == "4")/sims
    }
    end_time <- Sys.time()
    cat(v,"voters", p, "p", "\n")
    print(as.numeric(end_time - start_time, units = "mins")) 
  }
}




write.csv(res, file = "strategy4_final_55_2_15_1_try2_1000.csv", row.names = TRUE)
