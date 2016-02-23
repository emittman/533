#6.5

setwd("../../Users/emittman/Documents/GitHub/533/hw4")

tmp <- plot(lzbearing.ld)
tmp <- print(tmp)
bearing_tbl <- as.data.frame(tmp)

pdf("bearing_pp.pdf", width=5, height=11)
par(mfrow=c(3,1))
plot(lzbearing.ld)
plot(lzbearing.ld, "Lognormal")
plot(lzbearing.ld, "Weibull")
dev.off()


#6.7
alloy <- data.frame(t = c(18,32,39,53,59,68,77,78,93,100),
			  censor = c(rep("Failure",9), "Right"),
			  case.weights = c(rep(1,9), 91))

alloy.ld <- frame.to.ld(alloy, response.column = 1,
				censor.column = 2,
				case.weight.column = 3)

tmp <- plot(alloy.ld)
tmp <- print(tmp)
alloy_tbl <- as.data.frame(tmp)

saveRDS(alloy_tbl, file="alloy_df.rds")

pdf("alloy_pp.pdf")
par(mfrow=c(2,1))
plot(alloy.ld)
plot(alloy.ld, "Weibull")
dev.off()

dfz <- data.frame(x=log(alloy_tbl[2:10,1]),y=log(-log(1-((1:9)-.5)/100)))
pdf("weibull_plot.pdf")
probpaper("Weibull",x.range = c(15, 100), grid=TRUE, y.range = c(0.005,0.1))
points(dfz)
fit <- lm(y~x,dfz)
abline(fit$coef[1],fit$coef[2],lty=2)
dev.off()

#6.9

detector <- frame.to.ld(file=SplidaDataName("PhotoDetector.txt"),
response.column = c(1,2),
censor.column = 3,
case.weight.column = 4,
data.title = "detector failures",
skip = 3)

pdf("detector_pp.pdf", width=5, height=11)
par(mfrow=c(4,1))
plot(detector, "Exponential")
plot(detector, "Lognormal")
plot(detector, "Weibull")
plot(detector, "Normal")
dev.off()

#6.11
alloy.ld <- frame.to.ld(file=SplidaDataName("at7987.txt"),
response.column = 1,
censor.column = 2,
case.weight.column = 3,
data.title = "alloy 7987 failures",
skip = 1)

tmp <- plot(alloy.ld, "Lognormal")
alloy_df <- print(tmp)
saveRDS(alloy_df, file = "alloy_df.rds")

dfw <- data.frame(x = log(alloy_df[2:56,1]), y = qnorm(alloy_df[2:56,3]))
fit2 <- lm(x ~ y, dfw)

pnorm( ( log(200) - fit2$coef[1])/fit2$coef[2])
# 0.7238951 
tmp <- print(tmp)
tmp[c(27,28),]	#interpolates to 165
m <- log(165)
endpts <- tmp[c(4,50),]
s <- log(endpts[,1]) %*% c(-1,1) / (qnorm(endpts[,3]) %*% c(-1,1))	
hist(tmp[,1],20,freq=F)
curve(dlnorm(x,m,s),add=T,lty=2)
pnorm(log(200),m,s)

#6.12
tmp[c(43,44),]

#C6.16
bulb.ld <- frame.to.ld(file=SplidaDataName("bulb.txt"),
	response.column = 1,
	skip = 1,
	data.title = "a bunch of bulbs that failed.",
	time.units = "hours")

pdf("bulbs.pdf")
par(mfrow=c(2,1))
plot(bulb.ld, "Logistic")
abline(h=0,lty=2)
abline(v=median(bulb.ld[,1]), lty=2)
hist(bulb.ld[,1],freq=F, main = "bulb failure times")
curve(dlogis(x,mean(bulb.ld[,1]),sd(bulb.ld[,1])*sqrt(3)/pi),add=T, lty=2)
dev.off()

mean(bulb.ld[,1])