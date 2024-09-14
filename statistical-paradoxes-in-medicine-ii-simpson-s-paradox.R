# Statistical Paradoxes in Medicine II: Simpson's Paradox
set.seed(31415) # setting a seed means we can always reproduce the same results from our simulations – we can delete this if we want to get a different random result every time 

# 1. Creating a Data Frame with the Results of the 1986 Study
# First we copy the data from the 1986 paper into R:
OS_surgeries <- 350 # the total number of open surgeries performed
OS_successes <- 273 # the total number of open surgeries which were successful
OS_large_surgeries <- 263 # the number of open surgeries performed on kidney stones smaller than 2cm in diameter
OS_large_successes <- 192 # the number of open surgeries on kidney stones smaller than 2cm in diameter which were successful
OS_small_surgeries <- OS_surgeries - OS_large_surgeries # the number of open surgeries performed on larger kidney stones 
OS_small_successes <- OS_successes - OS_large_successes # the number of open surgeries on larger kidney stones which were successful
PN_surgeries <- 350 # the total number of percutaneous nephrolithotomies performed
PN_successes <- 289 # the number of percutaneous nephrolithotomies which were successful
PN_large_surgeries <- 80 # the number of percutaneous nephrolithotomies performed on kidney stones smaller than 2cm in diameter
PN_large_successes <- 55 # the number of percutaneous nephrolithotomies on kidney stones smaller than 2cm in diameter which were successful
PN_small_surgeries <- PN_surgeries - PN_large_surgeries # the number of percutaneous nephrolithotomies performed on larger kidney stones 
PN_small_successes <- PN_successes - PN_large_successes # the number of percutaneous nephrolithotomies on larger kidney stones which were successful
# Now we can calculate the success rates and collect the results in a data frame:
kidney_df <- data.frame(
  Stone.Size = c("Small", "Large", "Both"),
  OS.Success.Rate = c(OS_small_successes/OS_small_surgeries, OS_large_successes/OS_large_surgeries, OS_successes/OS_surgeries),
  PN.Success.Rate = c(PN_small_successes/PN_small_surgeries, PN_large_successes/PN_large_surgeries, PN_successes/PN_surgeries)
)
print(kidney_df)

# 2. Simulating a Million Surgeries
# First we initialise a matrix to store the results we get for each patient's simulation:
simulated_patient_data <- matrix(rep(FALSE,3000000), nrow = 1000000, byrow = TRUE)
colnames(simulated_patient_data) <- c("Large Stone?", "Open Surgery?", "Treatment Success?")
for (i in 1:1000000) {
  # To give a patient a large kidney stone with a probability of 50%
  # we can randomly generate a real number between 0 and 1 
  # and give them an accurate result if it is less than or equal to 0.5:
  decide_stone_size <- runif(1, min=0, max=1)
  if (decide_stone_size <= 0.5) {
    simulated_patient_data[i,1] <- TRUE
    decide_treatment <- runif(1, min=0, max=1)
    # To make a patient with a large kidney stone three times more likely to receive an open surgery than a percutaneous nephrolithotomy
    # we can randomly generate a real number between 0 and 1 
    # and give them an open surgery if it is less than or equal to 0.75,
    # and a percutaneous nephrolithotomy if it is greater than 0.75:
    if (decide_treatment <= 0.75) {
      simulated_patient_data[i,2] <- TRUE
    } 
    # To give a patient with a large kidney stone a 70% chance of having a successful surgery
    # we can randomly generate a real number between 0 and 1 
    # and give them successful result if it is less than or equal to 0.7,
    # and an unsuccessful result if it is greater than 0.7:
    decide_success <- runif(1, min=0, max=1)
    if (decide_success <= 0.7) {
      simulated_patient_data[i,3] <- TRUE
    } 
  } else {
    # To make a patient with a small kidney stone three times more likely to receive a percutaneous nephrolithotomy than an open suregery
    # we can randomly generate a real number between 0 and 1 
    # and give them an open surgery if it is less than or equal to 0.25,
    # and a percutaneous nephrolithotomy if it is greater than 0.25:
    decide_treatment <- runif(1, min=0, max=1)
    if (decide_treatment <= 0.25) {
      simulated_patient_data[i,2] <- TRUE
    } 
    # To give a patient with a large kidney stone a 90% chance of having a successful surgery
    # we can randomly generate a real number between 0 and 1 
    # and give them successful result if it is less than or equal to 0.9,
    # and an unsuccessful result if it is greater than 0.9:
    decide_success <- runif(1, min=0, max=1)
    if (decide_success <= 0.9) {
      simulated_patient_data[i,3] <- TRUE
    } 
  }
}

# 3. Looking at the Results for the First 20 of Our Simulated Patients
head(simulated_patient_data, n=20)

# 4. Counting the Numbers of Procedures Performed and Successful Outcomes for Each Type of Surgery and Stone Size in Our Simulation
sim_surgeries <- 1000000
sim_successes <- sum(simulated_patient_data[,3])
sim_large_surgeries <- sum(simulated_patient_data[,1])
sim_large_successes <- sum(simulated_patient_data[,1] & simulated_patient_data[,3])
sim_small_surgeries <- sim_surgeries - sim_large_surgeries
sim_small_successes <- sim_successes - sim_large_successes
sim_OS_surgeries <- sum(simulated_patient_data[,2])
sim_OS_successes <- sum(simulated_patient_data[,2] & simulated_patient_data[,3])
sim_OS_large_surgeries <- sum(simulated_patient_data[,1] & simulated_patient_data[,2])
sim_OS_large_successes <- sum(simulated_patient_data[,1] & simulated_patient_data[,2] & simulated_patient_data[,3])
sim_OS_small_surgeries <- sim_OS_surgeries - sim_OS_large_surgeries 
sim_OS_small_successes <- sim_OS_successes - sim_OS_large_successes 
sim_PN_surgeries <- sim_surgeries - sim_OS_surgeries
sim_PN_successes <- sim_successes - sim_OS_successes
sim_PN_large_surgeries <- sim_large_surgeries - sim_OS_large_surgeries
sim_PN_large_successes <- sim_large_successes - sim_OS_large_successes
sim_PN_small_surgeries <- sim_small_surgeries - sim_OS_small_surgeries
sim_PN_small_successes <- sim_small_successes - sim_OS_small_successes
cat(paste0("The total number of surgeries performed on large kidney stones was ", sim_large_surgeries, ", of which ", sim_large_successes, " were successful.\n", 
           "The total number of surgeries performed on small kidney stones was ", sim_small_surgeries, ", of which ", sim_small_successes, " were successful.\n\n",
           "The total number of open surgeries performed was ", sim_OS_surgeries, ", of which ", sim_OS_successes, " were successful.\n",
           "Of these open surgeries, ", sim_OS_large_surgeries, " were on large kidney stones, with ", sim_OS_large_successes, " successful outcomes,\n",
           "and ", sim_OS_small_surgeries, " were on small kidney stones, with ", sim_OS_small_successes, " successful outcomes.\n\n",
           "The total number of percutaneous nephrolithotomies performed was ", sim_PN_surgeries, ", of which ", sim_PN_successes, " were successful.\n",
           "Of these percutaneous nephrolithotomies, ", sim_PN_large_surgeries, " were on large kidney stones, with ", sim_PN_large_successes, " successful outcomes,\n",
           "and ", sim_PN_small_surgeries, " were on small kidney stones, with ", sim_PN_small_successes, " successful outcomes."))

# 5. Calculating the Success Rates for Both Types of Surgery According to Stone Size and Storing the Results in a Data Frame
sim_kidney_df <- data.frame(
  Stone.Size = c("Small", "Large", "Both"),
  OS.Success.Rate = c(sim_OS_small_successes/sim_OS_small_surgeries, sim_OS_large_successes/sim_OS_large_surgeries, sim_OS_successes/sim_OS_surgeries),
  PN.Success.Rate = c(sim_PN_small_successes/sim_PN_small_surgeries, sim_PN_large_successes/sim_PN_large_surgeries, sim_PN_successes/sim_PN_surgeries)
)
print(sim_kidney_df)