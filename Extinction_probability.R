# This code is used to simulate the extinction probability in 100 years (i.e. the threshold for Vulnerable species)
# For other thresholds, use different time intervals (VU = 100 years; EN = 20 years; CR = 10 years) and...
# ... different extinction probabilities (VU = 10%; EN = 20%; CR = 50%)


# Total number of individuals
J <- 5000

# Create a sequence of 1 to 500 individuals
d <- seq(1,500, by=1)

The length of the time-interval (change for other threat levels)
time <- 100   # (VU = 100 years; EN = 20 years; CR = 10 years)

# The number of iterations
iterations<- 10000

# Create a blank vector of extinction probabilities for each level of abundance
ex.prob <- rep(NA, length(d))

# Run a loop for each abundance
for (i in 1:length(d)) {
  ext.vect <- rep(NA,iterations)
    # Run a loop for each iteration
	  for (j in 1:iterations){
			com.vect <- rep(NA,time)
      # Simulate ecological drift by resampling the population again and again
			com.vect[1] <- d[i]
		    for (k in 2:time) {
			    com.vect[k] <- sum(sample(c(1,0),J,replace=TRUE, prob=c(com.vect[k-1]/J, 1-(com.vect[k-1]/J))))
		    }
      # Calculate a vector is the species is present or absent  
	    ext.vect[j] <- ifelse(com.vect[time]==0,1,0)	
	    print(j)
		}
  # Count the proportion of times the population went extinct in the 10,000 iterations  
  ex.prob[i] <- sum(ext.vect)/length(ext.vect)
}

# Create a vector of extinction probabilities
VU <- ex.prob

# Save to file
write.table(cbind(VU,d),file= "VU.txt",	quote=T,sep="\t",row.names=F,col.names=T)


