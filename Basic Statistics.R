
# Importing data
# install.packages("readxl")

library(readxl)
student <- read.csv("students.csv")
head(student)

# Categorical Variable

# Frequency
freq <- table(student$Sex)
names(freq)<-c("Male(0)", "Female(1)", "Unknown(2)")

# Relative frequency
prop.table(freq)

# Bar plot
barplot(freq, main = "Barplot Chart of Students", xlab = "Sex", ylab = "Count" )

# Pie chart

rd <-round(freq/sum(freq)*100, 2)
label <- paste(names(freq), "\n", rd, "%")

pie(freq, labels = label, main = "Pie Chart of Students")

# Continuous Variable

# install.packages("psych")

library(psych)
summary(student$Height)
describe(student$Height)

# stem-and-Leaf plot

stem(student$Height)

# Frequency

freq <- table(student$Height)
freq
prop.table(freq)

# Histogram

hist(student$Height, main = "Histogram of Students", xlab  = "Height")

# Box plot

boxplot(student$Height, main = "Boxplot of Students")

