read.table(SplidaDataName("HeatExchanger.txt"))

setwd("../../Users/emittman/Documents/GitHub/533/hw3")

links <- data.frame(cycles = c(33,46,50,59,62,71,74,75,78,80),
			  Censor = c(rep("Failure",9),"Censored"),
			  case.weights = c(rep(1,8),2,2))

links.ld <- frame.to.ld(links, response.column = 1, censor.column = 2,
				data.title = "Link failure data",
				time.units = "thousands of cycles",
				case.weight.column = 3)

pointwise <- data.frame(plot(links.ld)[c("prob","sd")])

pointwise$lower <- with(pointwise, prob - qnorm(.95) * sd)
pointwise$upper <- with(pointwise, prob + qnorm(.95) * sd)

saveRDS(pointwise, file = "data34.rds")

########################################
