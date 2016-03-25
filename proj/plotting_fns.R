sev_inv <- function(p) log(-log(1-p))
sev_cdf <- function(q) 1 - exp(-exp(q))
require(scales) # trans_new() is in the scales library
sev_inv_trans = function() trans_new("sev_inv", sev_inv, sev_cdf)


weibull_plot <- function(start, end, cens, trunc, line=0, eta=NULL, beta=NULL){
  require(ggplot2)
  #cens, trunc are indicators
  #line = 0: no line, line = 1: plot Weibull(eta, beta) cdf
  time <- sort(end[!cens])
  p <- (1:length(end[!cens]) - .5) / length(end)
  df <- data.frame(time = time, p = p, truncated = trunc[!cens])
  plot <- ggplot(data = df, aes(x=time, y=p)) +
    geom_point(aes(color = truncated, shape=truncated))
  if(line){
    plot <- plot + stat_function(fun = pweibull,
                                 args = list(scale = beta, shape = eta))
  }
  #plot <- plot + coord_trans(x = "log", y = "sev_inv") 
  
  return(plot)
}
  
eleven <- d[d$model==11,]
eleven$cens <- eleven$failed == 0
eleven$trunc <- eleven$start_time>24
p <- with(eleven, weibull_plot(start_time, end_time, cens, trunc, 1, .74, 4218449))
