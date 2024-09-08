# Statistical Paradoxes in Medicine I: The False Positive Paradox
set.seed(27182) # setting a seed means we can always reproduce the same results from our simulations – we can delete this if we want to get a different random result every time 

# 1. Simulating the UK Population
# First we set up some variables:
UK_pop_size <- 68000000
chron_hep_b_prevalence <- 0.003
chron_hep_b_pop_size <- UK_pop_size * chron_hep_b_prevalence # we calculate 0.3% of the UK population to find the number of people that have chronic Hepatitis B
print(chron_hep_b_pop_size)
# Now we randomly pick 204,000 numbers from 1 to 68,000,000:
UK_pop <- 1:UK_pop_size # we create a vector made up of the numbers from 1 to 68,000,000
chron_hep_b_pop <- sample(UK_pop, size=chron_hep_b_pop_size, replace=FALSE) # we randomly samples 204,000 entries from our UK_pop vector
head(chron_hep_b_pop, n=10) # we print the first 10 entries of chron_hep_b_pop to check it looks as we expect
# Let's create a logical vector (one that only contains TRUE and FALSE values) to represent the chronic Hepatitis B status of every member of the UK population:
chron_hep_b_statuses <- rep(FALSE, times=UK_pop_size) # we initialise a vector the length of the UK population where every entry is FALSE
chron_hep_b_statuses[chron_hep_b_pop] <- TRUE # we change the entries corresponding to the people with chronic Hepatitis B to TRUE

# 2. Simulating Testing 1000 Random People
# First we randomly pick 1000 numbers from 1 to 68,000,000 to choose the people in our sample:
sample_size <- 1000
random_sample <- sample(UK_pop, size=sample_size, replace=FALSE)
# Before we begin to simulate test results we initialise variables to store the number of correct and incorrect test results:
number_accurate_results <- 0
number_inaccurate_results <- 0
number_true_positives <- 0
number_true_negatives <- 0
number_false_positives <- 0
number_false_negatives <- 0
# Now we can run a simulation: 
for (i in 1:sample_size) { # we set up a for loop to run through every person in our sample group
  true_chron_hep_b_status <- chron_hep_b_statuses[random_sample[i]] # for each person we first the logical vector we created above to find out if they do or don't have chronic Hepatitis B
  # To give them a correct test result with a probability of 99% we can randomly generate a real number between 0 and 1 and give them an accurate result if it is less than or equal to 0.99:
  random_number <- runif(1, min=0, max=1) # we sample from the continuous uniform distrubution U(0,1)
  if (random_number <= 0.99){
    number_accurate_results <- number_accurate_results + 1
    if (true_chron_hep_b_status == TRUE) { # if the person does have chronic Hepatitis B they get an accurate positive result
      number_true_positives <- number_true_positives + 1
    } else { # and if they don't they get an accurate negative result
      number_true_negatives <- number_true_negatives + 1
    }
  } else { # if the number we randomly generated is greater than 0.99 they will get an incorrect test result
    number_inaccurate_results <- number_inaccurate_results + 1
    if (true_chron_hep_b_status == TRUE) { # if the person does have chronic Hepatitis B they get a wrongly negative result
      number_false_negatives <- number_false_negatives + 1
    } else { # and if they don't they get a wrongly positive result
      number_false_positives <- number_false_positives + 1
    }
  }
}
# Let's correct and print our results in a vector to make them easy to interpret:
results_vector <- c("Number of Accurate Results:" = number_accurate_results, "Number of Inaccuate Results:" = number_inaccurate_results,
                    "Number of True Positives:" = number_true_positives, "Number of True Negatives:" = number_true_negatives,
                    "Number of False Positives:" = number_false_positives, "Number of False Negatives:" = number_false_negatives)
print(results_vector)
number_false_positives / (number_false_positives + number_true_positives)

# 3. Running 10,000 Simulations
# First we initialise a vector to store the probabilities we get from each simulation:
num_sims <- 10000
false_pos_probs <- rep(NA, times=num_sims)
# Now we can use similar code as in our original simulation, but this time nested inside a for loop so it runs 10,000 times:
for (n in 1:num_sims) {
  samp_size <- 1000
  rand_samp <- sample(UK_pop, size=sample_size, replace=FALSE)
  # For these simulations we're only interested in positive test results, so we don't need to store true and false negatives:
  num_true_pos <- 0
  num_false_pos <- 0
  for (i in 1:samp_size) {
    true_stat <- chron_hep_b_statuses[rand_samp[i]]
    rand_num <- runif(1, min=0, max=1) 
    # We can use 'if and' statements instead of nested if statements for brevity:
    if (rand_num <= 0.99 & true_stat == TRUE) { 
      num_true_pos <- num_true_pos + 1
    } else if (rand_num > 0.99 & true_stat == FALSE) {
      num_false_pos <- num_false_pos + 1
    }
  }
  prob_pos_false <- num_false_pos / (num_false_pos + num_true_pos)
  false_pos_probs[n] <- prob_pos_false
}
# Let's see what the first 10 probabilities we found in our ten thousand simulations were:
head(false_pos_probs, n=10)
# We can also find the average of all of our simulations: 
mean(false_pos_probs)
