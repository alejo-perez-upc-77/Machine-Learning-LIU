



set.seed(1234567890)


max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data

true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}

K=3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations

# Random initialization of the parameters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}
pi
mu

for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  
  # E-step: Computation of the fractional component assignments
  divisorz_1 <- matrix(nrow = dim(z)[1], ncol = dim(z)[2]); 
  for(i in 1:N){ # loop for multinomial matrix NxK  
    for(j in 1:K){
      divisorz_1[i,j] <- prod((mu[j,]**x[i,])*((1-mu[j,])**(1-x[i,])))           
    }
  }
  
  divisorz_2 <- matrix(pi, nrow=N, ncol=length(pi), byrow=TRUE) #pi's at the divisor
  divisor_z <- rowSums(divisorz_2*divisorz_1)
  
  z <- (divisorz_2*divisorz_1)/(divisor_z)
  
  
  #Log likelihood computation.
  ln_pi <- log(divisorz_2)
  z_loglike_part1 <- matrix(nrow = dim(z)[1], ncol = dim(z)[2]); 
  for(i in 1:N){ # loop for multinomial matrix NxK  
    for(j in 1:K){
      z_loglike_part1[i,j] <- sum((log(mu[j,])*x[i,])+((log(1-mu[j,]))*(1-x[i,])))           
    }
  }
  
  llik[it] <- sum(rowSums((z_loglike_part1+ln_pi)*z))
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console() 
  # Stop if the log likelihood has not changed significantly
  if((it != 1) && (abs(llik[it]-llik[it-1])<min_change)){
    break
  }
  
  #M-step: ML parameter estimation from the data and fractional component assignments
  n_estimators <- colSums(z) 
  
  pi <- n_estimators/N 
  
  for (j in 1:K){ # Let's set the new mu estimator through z (p(k|i))
    
    mu[j,] <- (1/n_estimators[j])*colSums(z[,j]*x)
    
  }
}

pi
mu
plot(llik[1:it], type="o")

