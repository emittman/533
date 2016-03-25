sev_inv <- function(p) log(-log(1-p))
sev_cdf <- function(q) 1 - exp(-exp(q))
require(scales) # trans_new() is in the scales library
sev_inv_trans = function() trans_new("sev_inv", sev_inv, sev_cdf)


weibull_plot <- function(start, end, cens, trunc, line=0, beta=NULL, eta=NULL){
  require(ggplot2)
  #cens, trunc are indicators
  #line = 0: no line, line = 1: plot Weibull(eta, beta) cdf
  time <- sort(end[!cens])
  p <- (1:length(end[!cens]) - .5) / length(end) + pweibull(quantile(start, .25), beta, eta)
  df <- data.frame(time = time, p = p, truncated = trunc[!cens])
  plot <- ggplot(data = df, aes(x=time, y=p)) +
    geom_point(aes(color = truncated, shape=truncated))
  if(line){
    plot <- plot + stat_function(fun = pweibull,
                                 args = list(shape = beta, scale = eta))
  }
  #plot <- plot + coord_trans(x = "log", y = "sev_inv") 
  
  return(plot)
}

#assumes d is data and s is stanfit
plot_cpu_model <- function(num, line){
  d_m <- d[d$model==num,]
  d_m$cens <- d_m$failed == 0
  d_m$trunc <- d_m$start_time>24
  sbeta <- paste0("beta[",num,"]",collapse="")
  seta <- paste0("eta[",num,"]",collapse="")
  beta <- summary(s)$summary[sbeta,"25%"]
  eta <- summary(s)$summary[seta,"75%"]
  p <- with(d_m, weibull_plot(start_time, end_time, cens, trunc, line, beta, eta))
  print(p)
}

select_pairs = function(fit, indices, p = c(1,2)){
  tp <- sapply(indices, function(x) paste(c("log_tp[",x,"]"),collapse=""))
  beta <- sapply(indices, function(x) paste(c("beta[",x,"]"),collapse=""))
  if(1 %in% p) pairs(fit, pars = c(beta, tp, "m1", "C1", "m2", "C2"))
  if(2 %in% p) plot(fit, pars = c(tp))
}
select_pairs(s, 1:21, 2)
