# Calculates the moving average for given values
mov.avg <- function(values, n) {
  v <- values
  newv <- rep(NA, n)
  for(i in (1+n):length(v)) {
    newv[i] <- mean(v[(i-n):(i-1)])
  }
  newv
}

# Returns a list containing a vector of moving averages and vector of predicted and actual values
mov.avg.summary <- function(values, n) {
  v <- values
  ma <- rep(NA, n)
  for(i in (1+n):length(v)) {
    ma[i] <- mean(v[(i-n):(i-1)])
  }
  pa <- rep(NA, n)
  for(i in (1+n):length(v)) {
    pa[i] <- abs(v[i]-ma[i])
  }
  list(avgs=ma, errors=pa)
}

# Finds the numbers that are greater than or equal to a percent of all numbers in a vector
np.percentile.finder <- function(v, p) {
  values <- sort(v)
  percent <- ceiling(length(v)*p)
  values[percent]
}

# Summarizes each column of a dataframe if it is numeric
df.summarize <- function(dframe) {
  frame <- dframe
  for(col in 1:ncol(frame)) {
    out <- NULL
    if (sapply(col, is.numeric) == 'TRUE') {
      out <- paste("Column ", colnames(frame)[col], ": mean=", mean(frame[,col], na.rm=TRUE),
                   ", min=", min(frame[,col], na.rm=TRUE), ", max=", max(frame[,col], na.rm=TRUE),
                   ", sd=", sd(frame[,col], na.rm=TRUE), sep="")
    } else {
      out <- paste("Column", colnames(frame)[col], "is non-numeric")
    }
    print(out)
  }
}
