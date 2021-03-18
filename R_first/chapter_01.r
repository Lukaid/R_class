## chapter_01

# 1.1 패키지 설치하기

install.packages("multcomp")
library(multcomp)

detach(package : multcomp) # 메모리 공간 확보를 위해 패키지 내리기

# 1.2 맛보기

a <- 1
a
View(a)

trees
View(trees)

summary(trees)
boxplot(trees)
pairs(trees)


cars
lm.out1 <- lm(dist ~ speed, data = cars)
anova(lm.out1)

plot(dist ~ speed, data = cars)
abline(lm.out1, col='red')

chickwts
summary(chickwts)
boxplot(chickwts$weight)

boxplot(weight ~ feed, data = chickwts)

lm.out2 <- lm(weight ~ feed, data = chickwts)
# 분산분석 feed방식이 weight에 영향을 주는지 분석
anova(lm.out2)


# 1.3 데이터의 변수들을 이용하기

mean(Volume)
mean(tree$Volume)
attach(trees)
mean(Volume)
detach(trees)

with(trees, mean(Volume))

trees$Volume + trees$Girth + trees$Height
with(trees, Volume, Grith, Height)


# 1.5 데이터 읽기

chickwts <- read.csv("chickwts.csv")

install.packages("readxl")
library(readxl)

chickwts2 <- read_excel("chickwts.xls")
View(chickwts2)


# 1.6 연습

x <- c(1, 2, 3, 4)
y <- c(10.2, 9.7, 6.5, 5.1)
ex <- data.frame(x, y)
write.csv(ex, "ex.csv", row.names = FALSE)
ex <- read.csv("ex.csv")
ex


lm.ex <- lm(y ~ x, data = ex)
plot(y ~ x, data = ex)
abline(lm.ex, col = 'red')
