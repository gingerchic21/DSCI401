setwd("C:/Users/Maureen/Documents/School/College/Spring 2017/Data Analytics/Assignments")
courses <- read.csv("CPSC_courses.csv")
courses
head(courses)

summary(courses$Course)

par(mar=c(7,7,3,1), mgp = c(5.5, 1, 0))
plot(courses$Term~courses$Course, las=2, cex.axis=0.8,
      xlab="Course", 
      ylab="Term",
      main="Courses vs. Term")

# Function to determine a numeric marker for major courses
major_marker <- function(course) {
  c <- course
  cmark = 0
  if(c == "CPSC 330") {
    cmark = 1
  }
  cmark
}

# Add a numeric marker column for major courses
courses["cmark"] = sapply(courses$Course, major_marker)
