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
