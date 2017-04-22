money.data <- read.csv("Salaries.csv")
money.data
summary(money.data)
plot(money.data)


plot(money.data$yrs.service, money.data$salary, col=5 + (money.data$sex=="Female"),
     xlab="Years Service", 
     ylab="Salary",
     main="Years Service vs. Salary")


# Add trend line
abline(lm(salary~yrs.service, data=subset(money.data, sex=="Male")), col="red", lwd=2)

abline(lm(salary~yrs.service, data=subset(money.data, sex=="Female")), col="green", lwd=2)

rank_marker <- function(rank) {
  r <- rank
  rmark = 0
  if(r == "Prof") {
    rmark = 2
  }
  else if(r == "AssocProf") {
    rmark = 1
  }
  rmark
}

money.data["rmark"] = sapply(money.data$rank, rank_marker)

plot(money.data$yrs.service, money.data$salary, col=4 + (money.data$rmark),
     xlab="Years Service", 
     ylab="Salary",
     main="Years Service vs. Salary")

# Add a legend
legend("topright", lty=1, col=4:6, pch=c(16, 17, 18), legend=c("Assistant", "Associate", "Prof"))

avg <- aggregate(yrs.service~rank, money.data, FUN=mean, na.rm=TRUE)
print(avg)

dframe("log price") <- log(dframe$price)