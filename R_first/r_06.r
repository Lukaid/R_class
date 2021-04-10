# chapter 4

# 4.2, 4.3

x <- c(15.5, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.5, 0, 4.97)
length(x)
sum(x)
sum(x)/length(x)
mean(x)
sd(x)

mean(x) - 8.1
t = sqrt(length(x)) * (mean(x) - 8.1)/sd(x) # t통계랑
2 * (1 - pt(t, df = 9)) # p-value 양측
1 - pt(t, df = 9) # p-value 단측

qt()

# 4.4 분석과 해석
shapiro.test(x)

t.test(x, mu=8.1)
t.test(x, mu=8.1, conf.level = 0.99)

#4.5 선택사항
t.test(x, mu=8.1, alter = "greater")
t.test(x, mu=8.1, alter = "les")

#4.6 통계값 추출
out <- t.test(x, mu = 8.1)
names(out) 

out$statistic
out$p.value
t.test(x, mu=8.1)$statistic
t.test(x, mu=8.1)$p.value


#chapter 5

FT <- read.csv("FT.csv")
with(FT, shapiro.test(Postwt-Prewt)) # 정규분포인지 확인, 귀무가설이 정규분포이다.
with(FT, t.test(Postwt-Prewt)) # 쌍체
t.test(FT$Postwt-FT$Prewt)

FT$diff <- FT$Postwt-FT$Prewt
t.test(FT$diff)


anorexia <- read.csv("anorexia.csv")
anorexia[c(1, 3), ]
anorexia[, c(1, 2)]

anorexia[c(1, 3), c(1, 2)]
anorexia[anorexia$Treat == "FT", ]

anorexia$Treat %in% c("count", "FT")
anorexia$Treat != "CBT"

with(anorexia[anorexia$Treat == "FT",], t.test(Postwt - Prewt))


# 과제 1-1번
ano_Cont <- anorexia[anorexia$Treat == "Cont",]
shapiro.test(ano_Cont$Postwt - ano_Cont$Prewt)

# 과제 1-2번
t.test(ano_Cont$Postwt, ano_Cont$Prewt, paired = T)



# chapter 7 two sample t-test

dental <- read.csv("dental.csv")
dental


# 7.4 분석과 해석
boxplot(resp ~ treatment, data = dental, col = 'green')
boxplot(log(resp) ~ treatment, data = dental, col = 'red')

var.test(log(resp) ~ treatment, data = dental)
t.test(log(resp) ~ treatment, var.equal = T, data = dental)

out1 <- lm(log(resp) ~ treatment, data = dental)
summary(out1)

var.test(resp ~ treatment, data = dental)
t.test(resp ~ treatment, var.equal = F, data = dental)
t.test(resp ~ treatment, data = dental)

#7.6 정규성 검정

out <- lm(log(resp) ~ treatment, data = dental)
shapiro.test(resid(out))





# 과제 2-1
anorexia_2 <- anorexia[anorexia$Treat != "CBT", ]
anorexia_2$diff <- anorexia_2$Postwt - anorexia_2$Prewt
#anorexia_2

boxplot(diff ~ Treat, data = anorexia_2, col = 'green')

# 과제 2-2
var.test(diff ~ Treat, data = anorexia_2)

# 과제 2-3
t.test(diff ~ Treat, var.equal = T, data = anorexia_2)















