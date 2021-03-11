# read Data1.txt & create data frame

Data1 <- read.delim("Data1.txt")

nrow(Data1)

data1 = subset(Data1, select = c("Gender", "EDU", "BF", "BM", "Happiness", "Peace"))

summary(data1)


urban.pop <- c(50, 47, 69, 47, 47, 57, 72, 42, 51, 40)
life.exp <- c(57, 72, 77, 65, 74, 75, 76, 59, 72, 60)
mean(life.exp)
sd(life.exp)
cor(urban.pop, life.exp)


life.lm <- lm(life.exp ~ urban.pop)
life.lm

plot(urban.pop, life.exp, col = "blue", pch = 19,
     main = "Urban Population Percent Vs. Life Expectancy",
     xlab = "Urban Population Percent (%)",
     ylab = "Life Expectancy (age)")

abline(life.lm, col='red')
