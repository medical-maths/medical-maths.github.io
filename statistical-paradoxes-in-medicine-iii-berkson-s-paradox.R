# Statistical Paradoxes in Medicine III: Berkson's Paradox
set.seed(14142) # setting a seed means we can always reproduce the same results from our simulations – we can delete this if we want to get a different random result every time 

# 1. Plotting a Random Sample of One Thousand People with Colliditis
n <- 1000 
infected_sample <- matrix(0, nrow=n, ncol=2) # we initialise an empty matrix to store the data for each person
for (i in 1:n) {
  severity <- runif(1, min=0, max=20) # we assign each person in the sample a random severity score between 0 and 20
  cigarettes <- runif(1,0,20) # we assign each person in the sample a random average number of cigarettes smoked per day between 0 and 20
  infected_sample[i,1] <- severity
  infected_sample[i,2] <- cigarettes
}
plot(infected_sample[,1], infected_sample[,2], pch=4, cex=1.2, lwd=2.4, col="#808080",
     xlab="Colliditis Severity Score", ylab="Average Number of Cigarettes Smoked in a Day")
abline(lm(infected_sample[,1] ~ infected_sample[,2]), lwd=4.8, col="#009ed8",) # we plot the line of best fit

# 2. Plotting Only the Hospitalised People In Our Sample
# First we initialise a new matrix to store the data for each person in our sample who is hospitalised –
# those people who are not hospitalised will have NA NA in place of data for their rows of the matrix,
# and those who are in hospital will have a matching row to our infected_sample matrix:
hospital_infected_sample <- matrix(rep(NA, 2*n), nrow=n, ncol=2) 
for (i in 1:n) {
  dice_roll <- runif(1, min=0, max=1) # we generate random number between 0 and 1
  person_i <- infected_sample[i,]
  if(person_i[1] >= 15 && dice_roll <= 0.9) { # these people have a 90% chance of being in hospital
    hospital_infected_sample[i,] <- person_i
  } else if(person_i[2] >= 10 && dice_roll <= 0.6) { # these people have a 60% chance of being in hospital
    hospital_infected_sample[i,] <- person_i
  }
  else if(dice_roll <= 0.1) { # others have a 10% chance of being in hospital
    hospital_infected_sample[i,] <- person_i
  }
}
# Now we remove the people not in hospital from our sample:
hospital_infected_sample <- na.omit(hospital_infected_sample)
# And plot the result:
plot(hospital_infected_sample[,1], hospital_infected_sample[,2], pch=4, cex=1.2, lwd=2.4, col="#808080",
     xlab="Colliditis Severity Score", ylab="Average Number of Cigarettes Smoked in a Day")
abline(lm(hospital_infected_sample[,1] ~ hospital_infected_sample[,2]), lwd=4.8, col="#009ed8") # we plot the line of best fit