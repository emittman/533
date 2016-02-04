#3.4

setwd("../../Users/emittman/Documents/GitHub/533/hw3")

links <- data.frame(cycles = c(33,46,50,59,62,71,74,75,78,80),
			  Censor = c(rep("Failure",9),"Censored"),
			  case.weights = c(rep(1,8),2,2))

links.ld <- frame.to.ld(links, response.column = 1, censor.column = 2,
				data.title = "Link failure data",
				time.units = "cycles",
				case.weight.column = 3)

pointwise_links <- plot(links.ld, conf.level=.90)

capture.output(print(pointwise_links, conf.level=.90), file="data34.txt", append=TRUE)
########################################

#3.12

detectors <- data.frame(low_hrs = c(2000,2500,3000,3500,3600,3700,3800),
				high_hrs = c(2500,3000,3500,3600,3700,3800,3800),
				case.weights = c(1,1,2,1,1,1,21),
				Censor = c(rep("Interval",6),"Right"))
detectors.ld <- frame.to.ld(detectors, response.column=c(1,2), censor.column=4,
case.weight.column=3)

pdf("np_detectors.pdf")
pointwise_detectors <- plot(detectors.ld)
dev.off()

pdf("simul_detectors.pdf")
simul_detectors <- plot(detectors.ld, band.type = "Simultaneous")
dev.off()

capture.output(pointwise_detectors, file = "data312.txt",append=TRUE)
capture.output(print(simul_detectors, band.type = "Simultaneous"), file = "simul312.txt",append=TRUE)

