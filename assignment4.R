# Problem 1

money.data <- read.csv("Salaries.csv")
money.data
head(money.data)
summary(money.data)
plot(money.data)

# 1) Do men and women make a discernable difference overall in their salaries as
# their time as a professor progresses?
plot(money.data$yrs.service, money.data$salary, col=5 + (money.data$sex=="Female"),
     xlab="Years Service", 
     ylab="Salary",
     main="Years Service vs. Salary with Regards to Sex")
# Add a legend
legend("topright", lty=1, col=c(6,5), pch=c(16, 16), legend=c("Female", "Male"))
# Add trend line for women
abline(lm(salary~yrs.service, data=subset(money.data, sex=="Female")), col=6, lwd=2)
# Add trend line for men
abline(lm(salary~yrs.service, data=subset(money.data, sex=="Male")), col="blue", lwd=2)


# 2) Relative to salary, what are typical times in terms of years of service for each rank?

# Function to determine a numeric marker for rank
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

# Add a numeric marker column for rank
money.data["rmark"] = sapply(money.data$rank, rank_marker)

plot(money.data$yrs.service, money.data$salary, col=4 + (money.data$rmark),
     xlab="Years Service", 
     ylab="Salary",
     main="Years Service vs. Salary with Regards to Rank")
# Add a legend
legend("topright", lty=1, col=4:6, pch=c(16, 16, 16), legend=c("Assistant",
                                                               "Associate", "Prof"))

# Table with average times of service per rank
avg1 <- aggregate(yrs.service~rank, money.data, FUN=mean, na.rm=TRUE)
print(avg1)


# 3)	What general relationship exists between discipline and salary as time of
# service progresses?

plot(money.data$yrs.service, money.data$salary, col=5 + (money.data$discipline=="A"),
     xlab="Years Service", 
     ylab="Salary",
     main="Years Service vs. Salary with Regards to Discipline")
# Add a legend
legend("topright", lty=1, col=c(6,5), pch=c(16, 16), legend=c("A", "B"))
# Add trend line for discipline A
abline(lm(salary~yrs.service, data=subset(money.data, discipline=="A")), col=6, lwd=2)
# Add trend line for discipline B
abline(lm(salary~yrs.service, data=subset(money.data, discipline=="B")), col="blue", lwd=2)

# Table with average times of service per discipline
avg3 <- aggregate(yrs.service~discipline, money.data, FUN=mean, na.rm=TRUE)
print(avg3)


# Problem 2

library(MASS)
data(Boston)
?Boston # Boston documentation
Boston
head(Boston)
summary(Boston)
plot(Boston)


dframe("log price") <- log(dframe$price)