model <- "
data {
  int<lower=0> T_obs;
  int<lower=0> T_cens;
  int<lower=0> wts_obs[T_obs];
  int<lower=0> wts_cens[T_cens];

  // censoring points/failure times
  real<lower=0> y_obs[T_obs];
  real<lower=0> y_cens[T_cens];
  // quantile to set prior on
  real<lower=0, upper=1> p;
}
transformed data{
real<lower=0> Q;
Q <- -1.0 * log(1-p);
}
parameters {
  real<lower=log(1000), upper=log(50000)> log_tp;
  real<lower=log(.3), upper=log(8)> log_beta;
}
transformed parameters {
  real<lower=0> eta;
  real<lower=0> beta;
  beta <- exp(log_beta);
  eta <- exp(log_tp)/(Q^(1/exp(log_beta)));
}
model {
  for(i in 1:T_obs){
    increment_log_prob(wts_obs[i] * weibull_log(y_obs[i], beta, eta));
  }
  for(i in 1:T_cens){
    increment_log_prob(wts_cens[i] * weibull_ccdf_log(y_cens[i], beta, eta));
  }

} "
model2 <- "
data {
int<lower=0> T_obs;
int<lower=0> T_cens;
int<lower=0> wts_obs[T_obs];
int<lower=0> wts_cens[T_cens];

// censoring points/failure times
real<lower=0> y_obs[T_obs];
real<lower=0> y_cens[T_cens];
// quantile to set prior on
real<lower=0, upper=1> p;
real mu;
real<lower=0> sigma2;
}
transformed data{
real<lower=0> Q;
Q <- -1.0 * log(1-p);
}
parameters {
real<lower=log(1000), upper=log(50000)> log_tp;
real log_beta;
}
transformed parameters {
real<lower=0> eta;
real<lower=0> beta;
beta <- exp(log_beta);
eta <- exp(log_tp)/(Q^(1/exp(log_beta)));
}
model {
for(i in 1:T_obs){
increment_log_prob(wts_obs[i] * weibull_log(y_obs[i], beta, eta));
}
for(i in 1:T_cens){
increment_log_prob(wts_cens[i] * weibull_ccdf_log(y_cens[i], beta, eta));
}

log_beta ~ normal(mu, sigma2);
} "
