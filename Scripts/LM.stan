functions{
  vector linear_predictor(matrix X, vector beta, int n){
    vector[n] out = X * beta;
    return out;
  } 
}

data{
  int n;
  int n_pred;
  int Nbetas;
  vector[n] y;
  matrix[n, Nbetas] X;
  matrix[n_pred, Nbetas] X_pred;
}

parameters{  
  vector[Nbetas] beta;
  real<lower=0> sigma;
}

transformed parameters{
  real<lower=0> sigma2 = sigma^2;
}

model{
  // Linear regression model
  target += normal_lpdf(y | X * beta, sigma);

  // Alternative specificaton 1
  // y ~ normal(X * beta, sigma);

  // Alternative specificaton 2
  // for(i in 1:n){
  //    y[i] ~ normal(X[i,] * beta, sigma);
  // }

  // Alternative specificaton 3
  // target += normal_lpdf(y | linear_predictor(X, beta, n), sigma);

  // LOG-PRIORS             
  // Coefficients
  target += normal_lpdf(beta | 0, sqrt(10000));  
  // beta ~ normal(0, sqrt(10000)); 

  // Error standard deviation
  target += gamma_lpdf(sigma | 0.01, 0.01);
  // sigma ~ gamma(0.01, 0.01);
}

generated quantities{
  vector[n_pred] y_pred;

  // Prediction
  for(i in 1:n_pred){
     y_pred[i] = normal_rng(X_pred[i,] * beta, sigma);
  }
}
