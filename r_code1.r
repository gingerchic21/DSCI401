# Find solutions for quadratic equations
quadratic <- function(a, b, c) {
	m <- (-b - sqrt(b^2 - (4*a*c))) / (2 * a)
	p <- (-b - sqrt(b^2 - (4*a*c))) / (2 * a)
	c(m, p)
}

# Calculate sum of all numbers from a to b
sumRange <- function(a, b) {
	total <- 0
	for(i in a:b) {
		total <- total + i
	}
	total
}

#Calculate sum of all numbers from a to b - but do so recursively
SRR <- function(a, b) {
	if(a == b) {
		return(a)
	}
	SRR(a, b - 1) + b
}

# Compute generalized Fibonacci numbers given first two sequence elements
fib <- function(a, b, i) {
	if (i == 1) {
		return(a)
	}
	if (i == 2) {
		return(b)
	}
	fib(a, b, i-2) + fib(a, b, i-1)
}

# Iteratively maps f to each element of vec and returns the resulting vector
map.iter <- function(vec, f) {
	new.vec <- c()
	for (i in 1:length(vec) {
		new.vec[i] <- f(vec[i])
	}
	new.vec
}