# chapter 2 Descriptive Statistics (기술통계)

# 2.2 평균, 분산, 표준편차, 변동계수

trees
View(trees)

length(trees$Volume)

trees$Volume[1]

with(trees, length(Volume))

attach(trees)
length(Volume)
mean(Volume)
var(Volume)

mean(Height)

n <- length(Volume)
n
ss <- var(Volume)
ss


variance <- ss*(n-1)/n
variance

sd(Volume)
sqrt(ss)

sd(Volume)/sqrt(n)

sd(Volume)/mean(Volume)


# 2.3 중위수, 사분위수, boxplot

fivenum(Volume)
quantile(Volume)
IQR(Volume)

fivenum(1:31)
sort(Volume)

boxplot(Volume, col = "red")
colors()
boxplot(Volume, col = "yellowgreen")

fivenum(Volume)[2] - 1.5*(IQR(Volume))
fivenum(Volume)[4] + 1.5*(IQR(Volume))


# 2.4 그래프

hist(Volume)
hist(Volume, probability = TRUE)
lines(density(Volume), col = 'blue')

stem(Volume)

# Quantile-Qunatile Plot, 정규분포와 근접한 지표
qqnorm(Volume)
qqline(Volume, col="blue")

x <- rnorm(n=31, mean = 0, sd = 1)
qqnorm(x)
qqline(x, col="red")


# 2.5 함수만들기

se = function(x) sd(x)/sqrt(length(x)) # 표준오차 공식
se(trees$Volume)

cv = function(x) sd(x)/mean(x) # 변동계수 공식
cv(trees$Volume)



# 2.7 Exercise (Data tree의 변수 Height을 이용하여 통계치를 구하고 그래프를 그려라)


attach(trees)
length(Height)
mean(Height)
var(Height)

n <- length(Height)
ss <- var(Height)

variance <- ss*(n-1)/n
variance

sd(Height)
sqrt(ss)

sd(Height)/sqrt(n)
sd(Height)/mean(Height)


# 2.7_1 중위수, 사분위수, boxplot

fivenum(Height)
quantile(Height)
IQR(Height)

sort(Height)

boxplot(Height, col = "yellowgreen")

fivenum(Height)[2] - 1.5*(IQR(Height))
fivenum(Height)[4] + 1.5*(IQR(Height))


# 2.7_2 그래프

hist(Height)
hist(Height, probability = TRUE)
lines(density(Height), col = 'blue')

# Quantile-Qunatile Plot, 정규분포와 근접한 지표
qqnorm(Height)
qqline(Height, col="blue")

x <- rnorm(n=31, mean = 0, sd = 1)
qqnorm(x)
qqline(x, col="red")


# 2.7_3 함수만들기

se = function(x) sd(x)/sqrt(length(x)) # 표준오차 공식
se(trees$Height)

cv = function(x) sd(x)/mean(x) # 변동계수 공식
cv(trees$Height)

