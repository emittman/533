sev_inv <- function(p) log(-log(1-p))
sev_cdf <- function(q) 1 - exp(-exp(q))
require(scales) # trans_new() is in the scales library
sev_inv_trans = function() trans_new("sev_inv", sev_inv, sev_cdf)

weibull_points <- function(start, end, cens, trunc, beta=NULL, eta=NULL){
  #cens, trunc are indicators
  #line = 0: no line, line = 1: plot Weibull(eta, beta) cdf
  time <- sort(end[!cens])
  p <- (1:length(end[!cens]) - .5) / length(end) + pweibull(min(start), beta, eta)
  df <- data.frame(time = time, p = p, truncated = trunc[!cens])
  df
}
weibull_plot <- function(df, line=0, beta=NULL, eta=NULL){
  require(ggplot2)
  plot <- ggplot(data = df, aes(x=time, y=p)) +
    geom_point(aes(color = truncated, shape=truncated))
  if(line){
    plot <- plot + stat_function(fun = pweibull,
                                 args = list(shape = beta, scale = eta))
  }
  #plot <- plot + coord_trans(x = "log", y = "sev_inv") 
  
  return(plot)
}

head_to_head <- function(num1, num2){
  d_m1 <- d[d$model==num1,]
  d_m1$cens <- d_m1$failed == 0
  d_m1$trunc <- d_m1$start_time>24
  sbeta1 <- paste0("beta[",num1,"]",collapse="")
  seta1 <- paste0("eta[",num1,"]",collapse="")
  beta1 <- summary(s)$summary[sbeta1,"50%"]
  eta1 <- summary(s)$summary[seta1,"50%"]
  df1 <- with(d_m1, weibull_points(start_time, end_time, cens, trunc, beta1, eta1))
  df1$model <- num1
  d_m2 <- d[d$model==num2,]
  d_m2$cens <- d_m2$failed == 0
  d_m2$trunc <- d_m2$start_time>24
  sbeta2 <- paste0("beta[",num2,"]",collapse="")
  seta2 <- paste0("eta[",num2,"]",collapse="")
  beta2 <- summary(s)$summary[sbeta2,"50%"]
  eta2 <- summary(s)$summary[seta2,"50%"]
  df2 <- with(d_m2, weibull_points(start_time, end_time, cens, trunc, beta2, eta2))
  df2$model <- num2
  DF <- rbind(df1,df2)
  require(ggplot2)
  plot <- ggplot(data = DF, aes(x=time, y=p)) +
    geom_point(aes(color = factor(model), shape=truncated))
    plot <- plot + stat_function(fun = pweibull,
                                 args = list(shape = beta1, scale = eta1), color="red")
    plot <- plot + stat_function(fun = pweibull,
                                 args = list(shape = beta2, scale = eta2), color="cyan")
    return(plot)
}


#assumes d is data and s is stanfit
plot_cpu_model <- function(num, line){
  d_m <- d[d$model==num,]
  d_m$cens <- d_m$failed == 0
  d_m$trunc <- d_m$start_time>24
  sbeta <- paste0("beta[",num,"]",collapse="")
  seta <- paste0("eta[",num,"]",collapse="")
  beta <- summary(s)$summary[sbeta,"50%"]
  eta <- summary(s)$summary[seta,"50%"]
  p <- with(d_m, weibull_plot(start_time, end_time, cens, trunc, line, beta, eta))
  print(p)
}

select_pairs = function(fit, indices, p = c(1,2)){
  tp <- sapply(indices, function(x) paste(c("log_tp[",x,"]"),collapse=""))
  beta <- sapply(indices, function(x) paste(c("beta[",x,"]"),collapse=""))
  eta <- sapply(indices, function(x) paste(c("eta[",x,"]"),collapse=""))
  if(1 %in% p) pairs(fit, pars = c(beta, tp, "m1", "C1", "m2", "C2"))
  if(2 %in% p) plot(fit, pars = c(tp))
  if(3 %in% p) plot(fit, pars = c(eta))
}
select_pairs(s, 1, 1)
