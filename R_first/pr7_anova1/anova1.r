#SalesRecord1 One way ANOVA 

library(readxl)                                        #import data file 1
salesRecord1 <- read_excel("salesRecord1.xlsx")
View(salesRecord1)


salesRecord1$PT<- factor(salesRecord1$payType)        # encode a vector as a factor
 

# 분산분석   
aov.out1 <- aov(SalesRec ~ PT, data = salesRecord1)   # 방법 1   aov()
summary(aov.out1)

gc.out1 <- lm(SalesRec ~ PT, data = salesRecord1)     # 방법 2   lm() & anova(), aov()
anova(gc.out1)
summary(gc.out1)





#chapter 12

#12.4 분석과 해석
PlantGrowth  <- read.csv("PlantGrowth.csv")
PlantGrowth
with(PlantGrowth, tapply(weight, group, mean))
with(PlantGrowth, tapply(weight, group, sd))
boxplot(weight~group, col = 'grey', data = PlantGrowth)

out <- lm(weight~group, data = PlantGrowth)
out
anova(out)
shapiro.test(resid(out))  # 잔차의 정규성 검정, 잔차가 정규성을 가져야 표본이 정규성을 가진다?


par(mfrow = c(2,2))    # 그래프 화면 분할
plot(out)


#12.5 다중비교(사후검정)

install.packages("multcomp")
library(multcomp)

out <- lm(weight ~ group, data = PlantGrowth)
dunnett <- glht(out, linfct = mcp(group ="Dunnett"))     #대조군과 2개의 비교군의 비교
summary(dunnett)
plot(dunnett)

tukey <- glht(out, linfct = mcp(group ="Tukey"))         #모든 비교 가능 조합 비교
summary(tukey)
plot(tukey)





#Chapter 13

kruskal.test(weight ~ group, data = PlantGrowth)          #일원분산분석의 비모수방법











#12.7 연습문제

# 치료전 몸무게의 일원분산분석
anorexia <- read.csv("anorexia.csv")
anorexia$Treat = relevel(anorexia$Treat, ref = "Cont")  
levels(anorexia$Treat)
boxplot(Prewt~Treat, data=anorexia, col ='grey') 

out <- lm(Prewt~Treat, data=anorexia)    
anova(out)
shapiro.test(resid(out))  # 잔차의 정규성 검정


# 치료전후 몸무게 차이에 대한 일원분산분석
anorexia$diff <- anorexia$Postwt-anorexia$Prewt
boxplot(diff~Treat, data=anorexia, col ='red') 

out <- lm(diff~Treat, data = anorexia)
anova(out)
shapiro.test(resid(out))

# 치료전후 몸무게 차이에 대한 일원분산분석 후 사후검정
install.packages("multcomp")
library(multcomp)


out <- lm(diff~Treat, data = anorexia)
dunnett <- glht(out, linfct = mcp(Treat ="Dunnett"))     #대조군과 2개의 비교군의 비교
summary(dunnett)
plot(dunnett)

tukey <- glht(out, linfct = mcp(Treat ="Tukey"))         #모든 비교 가능 조합 비교
summary(tukey)
plot(tukey)



par(mfrow = c(2,2))    # 그래프 화면 분할
plot(dunnett)
plot(tukey)

