common_beta <- "
data {
  int<lower=0> N_obs;
  int<lower=0> N_cens;
  int<lower=0> N_tr_obs;
  int<lower=0> N_tr_cens;
  int<lower=0> M;
  
  // right endpoints
  real<lower=0> y_obs[N_obs];
  real<lower=0> y_cens[N_cens];
  real<lower=0> y_tr_obs[N_tr_obs];
  real<lower=0> y_tr_cens[N_tr_cens];
  
  // truncation points
  real<lower=0> t_obs[N_tr_obs];
  real<lower=0> t_cens[N_tr_cens];
  
  // explanatory variable
  int<lower=1, upper=M> x_obs[N_obs];
  int<lower=1, upper=M> x_cens[N_cens];
  int<lower=1, upper=M> x_tr_obs[N_tr_obs];
  int<lower=1, upper=M> x_tr_cens[N_tr_cens];
}
parameters {
  vector[M] mu;
  real<lower=0> beta;
  //real m;
  //real<lower=0> C;
  //real<lower=0> lambda;
  //real<lower=0> b;
}
transformed parameters {
  vector<lower=0>[M] eta;
  //real<lower=0> a;
  eta <- exp(mu);
  //a <- lambda * b;
}
model {
  for(i in 1:N_obs){
    y_obs[i] ~ weibull(beta, eta[x_obs[i]]);
  }
  
  for(i in 1:N_cens){
    increment_log_prob(weibull_ccdf_log(y_cens[i], beta, eta[x_cens[i]]));
  }
  
  for(i in 1:N_tr_obs){
    increment_log_prob(        weibull_log(y_tr_obs[i], beta, eta[x_tr_obs[i]]));
    increment_log_prob(-1.0 * weibull_ccdf_log(t_obs[i], beta, eta[x_tr_obs[i]]));
  }
  
  for(i in 1:N_tr_cens){
    increment_log_prob(       weibull_ccdf_log(y_tr_cens[i], beta, eta[x_tr_cens[i]]));
    increment_log_prob(-1.0 * weibull_ccdf_log(    t_cens[i], beta, eta[x_tr_cens[i]]));
  }
  
  //mu ~ normal(10,1);
  beta ~ gamma(2,10);
  
  //priors (improper prior on m)
  //C ~ cauchy(0, 10);
  //lambda ~ exponential(10);
  //b ~ exponential(1);
} "

common_beta_hier_mu <- "
  data {
    int<lower=0> N_obs;
    int<lower=0> N_cens;
    int<lower=0> N_tr_obs;
    int<lower=0> N_tr_cens;
    int<lower=0> M;
    
    // right endpoints
    real<lower=0> y_obs[N_obs];
    real<lower=0> y_cens[N_cens];
    real<lower=0> y_tr_obs[N_tr_obs];
    real<lower=0> y_tr_cens[N_tr_cens];
    
    // truncation points
    real<lower=0> t_obs[N_tr_obs];
    real<lower=0> t_cens[N_tr_cens];
    
    // explanatory variable
    int<lower=1, upper=M> x_obs[N_obs];
    int<lower=1, upper=M> x_cens[N_cens];
    int<lower=1, upper=M> x_tr_obs[N_tr_obs];
    int<lower=1, upper=M> x_tr_cens[N_tr_cens];
  }
parameters {
  vector[M] mu;
  real<lower=0> beta;
  real m;
  real<lower=0> C;
  //real<lower=0> lambda;
  //real<lower=0> b;
}
transformed parameters {
  vector<lower=0>[M] eta;
  //real<lower=0> a;
  eta <- exp(mu);
  //a <- lambda * b;
}
model {
  for(i in 1:N_obs){
    y_obs[i] ~ weibull(beta, eta[x_obs[i]]);
  }
  
  for(i in 1:N_cens){
    increment_log_prob(weibull_ccdf_log(y_cens[i], beta, eta[x_cens[i]]));
  }
  
  for(i in 1:N_tr_obs){
    increment_log_prob(        weibull_log(y_tr_obs[i], beta, eta[x_tr_obs[i]]));
    increment_log_prob(-1.0 * weibull_ccdf_log(t_obs[i], beta, eta[x_tr_obs[i]]));
  }
  
  for(i in 1:N_tr_cens){
    increment_log_prob(       weibull_ccdf_log(y_tr_cens[i], beta, eta[x_tr_cens[i]]));
    increment_log_prob(-1.0 * weibull_ccdf_log(    t_cens[i], beta, eta[x_tr_cens[i]]));
  }
  
  mu ~ normal(m, C);
  beta ~ gamma(2,10);
  
  //priors (improper prior on m)
  C ~ cauchy(0, 10);
  //lambda ~ exponential(10);
  //b ~ exponential(1);
} "

hier_mu_and_beta <- "
data {
int<lower=0> N_obs;
int<lower=0> N_cens;
int<lower=0> N_tr_obs;
int<lower=0> N_tr_cens;
int<lower=0> M;

// right endpoints
real<lower=0> y_obs[N_obs];
real<lower=0> y_cens[N_cens];
real<lower=0> y_tr_obs[N_tr_obs];
real<lower=0> y_tr_cens[N_tr_cens];

// truncation points
real<lower=0> t_obs[N_tr_obs];
real<lower=0> t_cens[N_tr_cens];

// explanatory variable
int<lower=1, upper=M> x_obs[N_obs];
int<lower=1, upper=M> x_cens[N_cens];
int<lower=1, upper=M> x_tr_obs[N_tr_obs];
int<lower=1, upper=M> x_tr_cens[N_tr_cens];
}
parameters {
vector[M] mu;
vector<lower=0>[M] beta;
real m;
real<lower=0> C;
real<lower=0> lambda;
real<lower=0> b;
}
transformed parameters {
vector<lower=0>[M] eta;
real<lower=0> a;
eta <- exp(mu);
a <- lambda * b;
}
model {
for(i in 1:N_obs){
y_obs[i] ~ weibull(beta[x_obs[i]], eta[x_obs[i]]);
}

for(i in 1:N_cens){
increment_log_prob(weibull_ccdf_log(y_cens[i], beta[x_cens[i]], eta[x_cens[i]]));
}

for(i in 1:N_tr_obs){
increment_log_prob(        weibull_log(y_tr_obs[i], beta[x_tr_obs[i]], eta[x_tr_obs[i]]));
increment_log_prob(-1.0 * weibull_ccdf_log(t_obs[i], beta[x_tr_obs[i]], eta[x_tr_obs[i]]));
}

for(i in 1:N_tr_cens){
increment_log_prob(       weibull_ccdf_log(y_tr_cens[i], beta[x_tr_cens[i]], eta[x_tr_cens[i]]));
increment_log_prob(-1.0 * weibull_ccdf_log(    t_cens[i], beta[x_tr_cens[i]], eta[x_tr_cens[i]]));
}

mu ~ normal(m, C);
beta ~ gamma(a, b);

//priors (improper prior on m)
C ~ cauchy(0, 10);
lambda ~ exponential(1);
b ~ exponential(1);
} "