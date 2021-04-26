
# 복습 1. one sample t-test  표본의 평균과 모집단의 평균 비교

x <- c(15.5, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.5, 0, 4.97)

shapiro.test(x)         #정규성 검정, 0.05보다 크면 정규성이 있다.
t.test(x)
t.test(x, mu=8.1)
t.test(x, mu=8.1, conf.level = 0.99)
t.test(x, mu=8.1, alter = "greater")
t.test(x, mu=8.1, alter = "less")



#복습 2. paired t-test   한 표본의 전후 차이의 평균이 0인지

FT <- read.csv("FT.csv")

with(FT, shapiro.test(Postwt-Prewt))
with(FT, t.test(Postwt-Prewt))
t.test(FT$Postwt-FT$Prewt)               #일표본 t-test처럼 차이값의 표본 평균이 0인지에 대한 검정


FT$diff <- FT$Postwt-FT$Prewt            #차이값을 계산한 변수를 만들어서 하는 방법
t.test(FT$diff)


t.test(FT$Postwt, FT$Prewt, paired = T)     #paired t-test 하는 다른 방법 (결과 같음)



#복습 3. two sample t-test   두 표본의 평균의 차이가 0인지 

sales1 <- c(150, 140, 135, 90, 115, 120, 145,  135)     #예1
sales2 <- c(140, 120, 85, 75, 125, 120, 130, 110)


shapiro.test(sales1)
shapiro.test(sales2)

var.test(sales1, sales2)
t.test(sales1, sales2, var.equal = T)  


dental <- read.csv("dental.csv")                  #예2

var.test(resp~treatment, data = dental)           #등분산인지 확인 - 분산이 다르다 
t.test(resp~treatment, var.equal = F, data = dental)


with(dental, shapiro.test(resp))    #데이터의 정규성을 확인

dental$log_resp <- log(dental$resp)        #정규성을 가질 수 있도록 변환
with(dental, shapiro.test(log_resp))


with(dental, shapiro.test(resp))

var.test(log(resp)~treatment, data = dental)           #등분산인지 확인 - 분산이 같음
t.test(log(resp)~treatment, var.equal = T, data = dental)








###################################################
#정규성을 만족하지 못할 경우 비모수 검정 방법을 사용
###################################################

#Chapter 6 Wilcoxon Signed-Rank test

CBT <- read.csv("CBT.csv")
with(CBT, t.test(Postwt-Prewt))  #t-test           

with(CBT, shapiro.test(Postwt-Prewt))      #정규성 검정, 정규분포가 아님
with(CBT, wilcox.test(Postwt-Prewt))       #비모수 검정 실시   귀무가설 :  median = 0 이다. 

with(CBT, Postwt-Prewt)
sort(with(CBT, abs(Postwt-Prewt)))
with(CBT, wilcox.test(Postwt-Prewt, exact = F))


# 6.5 비모수 방법이 필요한 이유

x=c(1.40,  0.91, 1.12, 1.25, -0.67, -0.12, 1.86, 1.44, 0.34, 1.82)

t.test(x)       #귀무가설 기각
wilcox.test(x)  #귀무가설 기각
shapiro.test(x)

x1=c(1.40,  0.91, 1.12, 1.25, -0.67, -0.12, 10, 1.44, 0.34, 1.82)
t.test(x1)      #귀무가설 채택 ???
wilcox.test(x1) #귀무가설 기각
shapiro.test(x1)




# 6.6 연습문제
anorexia <- read.csv("anorexia.csv")
with(anorexia[anorexia$Treat=="CBT",], shapiro.test(Postwt-Prewt))
with(anorexia[anorexia$Treat=="CBT",], wilcox.test(Postwt-Prewt))


with(anorexia[anorexia$Treat=="FT",], shapiro.test(Postwt-Prewt))
with(anorexia[anorexia$Treat=="FT",], t.test(Postwt-Prewt))


with(anorexia[anorexia$Treat=="Cont",], shapiro.test(Postwt-Prewt))
with(anorexia[anorexia$Treat=="Cont",], t.test(Postwt-Prewt))





#Chapter 8  Wilcox Rank-Sum test 

dental <- read.csv("dental.csv")

with(dental, shapiro.test(resp))    #데이터의 정규성을 확인
wilcox.test(resp ~ treatment, data = dental)


dental$log_resp <- log(dental$resp)        #정규성을 가질 수 있도록 변환
with(dental, shapiro.test(log_resp))
wilcox.test(log(resp) ~ treatment, data = dental)   #로그 변환해도 순위가 바뀌지 않음. 동일 결과



#각 그룹의 데이터의 정규성 테스트 
with(dental[dental$treatment=="test", ], shapiro.test(resp))
with(dental[dental$treatment=="control", ], shapiro.test(resp))


