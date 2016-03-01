

data <- frame.to.ld(data.frame( t = c(345,556,712,976,1000),
				     freq = c(  1,  1,  1,  1,9428),
				     cens = c(rep("Failed",4),"Right")),
			  response.column = 1,
			  censor.column = 3,
			  case.weight.column = 2  )
pdf("8_10.pdf")
par(mfrow=c(1,3))
mleprobplot(data, distribution="Weibull", parameter.fixed=c(F,T),theta.start=c(10,1/3),my.title="sigma = 1/3  (or beta = 3)")
mleprobplot(data, distribution="Weibull", parameter.fixed=c(F,T),theta.start=c(10,3),my.title="sigma = 3  (or beta = 1/3)")
mleprobplot(data, distribution="Weibull",my.title = "sigma unknown")
dev.off()
