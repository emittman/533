llexp <- function(theta, sum, n, numcens, censpt){
	-(n-numcens) * log(theta) - 1/theta * (numcens * censpt + sum)
}

ex4_obj <- Vectorize(function(theta) {
	llexp(theta, 345+556+712+976, 9432, 9428, 1000)
})

max <- (345+556+712+976 + 9428*1000)/4
max_ll <- ex4_obj(max)
ex4_rll <- Vectorize(function(theta) {
	ex4_obj(theta) - max_ll
})


grid <- -100:100*100 + max
plot(cbind(grid, ex4_rll(grid)))
abline(v=max)

to_solve <- function(theta) qchisq(.95,1) + 2 * ex4_rll(theta)
ci <- NULL
ci[1] <- uniroot(to_solve, c(100000, max))[[1]]
ci[2] <- uniroot(to_solve, c(max,max+10000000))[[1]]

max_lambda <- 1/max
ci_lambda <- 1/ci

#7.4
lb <- 1000/-log(.05)

t_lb <- -lb * log(.9)
hz_ub <- 1/lb
F50_ub <- 1 - exp(-50/lb)
