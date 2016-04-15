bearingCage <- read.table("../../../../../../RSplidaAlpha/RSplida_text_data/BearingCage.txt",
                          header=T)

library(rstan)
source("model.R")

data <- with(bearingCage, list(T_obs = sum(Status=="Failed"),
                               T_cens = sum(Status=="Censored"),
                               y_obs = Hours[Status=="Failed"],
                               y_cens = Hours[Status=="Censored"],
                               wts_obs = Weight[Status=="Failed"],
                               wts_cens = Weight[Status=="Censored"],
                               p = .1))

m <- stan_model(model_code = model)
s <- sampling(m, data=data, iter=5000)
pairs(s, pars=c("log_tp","beta"))
print(s)

#save(m, file="LUs_model.RData")
m <- load("LUs_model.RData")

#Add Ribbon
samples <- extract(s)

#Get Posterior Quantiles
get_quantiles <- function(begin, end, length, alpha, samples){
  seq <-seq(begin, end, length.out = length)
  par <- data.frame(beta = samples$beta, eta = samples$eta)
  q <- ldply(seq, function(t){
    probs <- sapply(1:4000, function(i) pweibull(t, par[i,1], par[i,2]))
    data.frame(time = t, lower = quantile(probs, alpha/2), median = quantile(probs, .5), upper = quantile(probs, 1-alpha/2))
  })
  return(q)
}

library(plyr)
ribbon <- get_quantiles(100,10000, 100, .1, samples)

library(ggplot2)
df <- subset(bearingCage, Status=="Failed")
df$prob <- (1:nrow(df)-.5)/sum(bearingCage$Weight)
ggplot(df, aes(x=Hours, y=prob)) + geom_point() +
  geom_ribbon(data=ribbon, aes(x = time, ymin=lower, ymax=upper, y=median), fill="lightblue",alpha=.5) +
  geom_line(data=ribbon, aes(x=time, y=median), color="blue")
###################################
m2 <- stan_model(model_code = model2)

data2 <- with(bearingCage, list(T_obs = sum(Status=="Failed"),
                               T_cens = sum(Status=="Censored"),
                               y_obs = Hours[Status=="Failed"],
                               y_cens = Hours[Status=="Censored"],
                               wts_obs = Weight[Status=="Failed"],
                               wts_cens = Weight[Status=="Censored"],
                               p = .1,
                               mu = (log(1.5)+log(3))/2,
                               sigma2 = (log(3)-log(1.5))/2/qnorm(.995)))
s2 <- sampling(m2, data=data2, iter=5000)

samples2 <- extract(s2)
ribbon2 <- get_quantiles(100,10000, 100, .1, samples2)

library(ggplot2)
ggplot(df, aes(x=Hours, y=prob)) + geom_point() +
  geom_ribbon(data=ribbon2, aes(x = time, ymin=lower, ymax=upper, y=median), fill="lightblue",alpha=.5) +
  geom_line(data=ribbon2, aes(x=time, y=median), color="blue")