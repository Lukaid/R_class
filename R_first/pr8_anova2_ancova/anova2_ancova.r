#Chapter 15 공분산 분석

anorexia <- read.csv("anorexia.csv")
anorexia

levels(anorexia$Treat)
anorexia$Treat <- relevel(anorexia$Treat, ref = 'Cont')
levels(anorexia$Treat)
anorexia$Treat <- factor(anorexia$Treat, levels=c('Cont', 'CBT', 'FT'))

#Postwt~Prewt+Treat 공분산 분석을 해도 같은 결과
out <- lm(Postwt~Prewt+Treat, data=anorexia)
anova(out)
summary(out)                         


library(multcomp)
dunnett <- glht(out, linfct <- mcp(Treat = 'Dunnett'))
summary(dunnett)
plot(dunnett)


#diff~Prewt+Treat 공분산 분석을 해도 같은 결과
anorexia$diff <- anorexia$Postwt-anorexia$Prewt
out2 <- lm(diff~Prewt+Treat, data=anorexia)
anova(out2)
summary(out2) # 자유도가 다름

library(multcomp)
dunnett <- glht(out2, linfct <- mcp(Treat = 'Dunnett'))
summary(dunnett)
plot(dunnett)


#manova

AD <- read.csv("manova.csv")
Y = cbind(AD$선호도, AD$태도, AD$구매의도)  #종속변수 구성
cor(Y)    #종속변수간 상관성 분석

out_manova <- manova(Y~AD$광고경험)    # MANOVA

summary(out_manova, test=c("Pillai"))         
summary(out_manova, test=c("Wilks"))
summary(out_manova, test=c("Hotelling-Lawley"))
summary(out_manova, test=c("Roy"))

summary.aov(out_manova)   # 개체간 효과분석


#당뇨병 데이터, 공분산분석과 분산분석의 검정결과가 다르게 나옴

tri <- read.csv('tri.csv')
tri
out <- lm(trichg ~ hgba1c + trt, data = tri)     #Triglyceride,  HemoglobinA1c
anova(out)

out <- lm(trichg~trt, data = tri)
anova(out)



#Two-way ANOVA - 강의 예제 

library(readxl)
salesRecord2 <- read_excel("salesRecord2.xlsx")
View(salesRecord2)

  # encode a vector as a factor
salesRecord2$PT<- factor(salesRecord2$payType) 
salesRecord2$GR<- factor(salesRecord2$Group) 

  # lm(), anova(), aov()
gc.out2 <- lm(SalesRec ~ GR + PT, data = salesRecord2)
anova(gc.out2)

aov.out2 <- aov(SalesRec ~ GR + PT, data = salesRecord2)
summary(aov.out2)


 
  #import data file 3 - 상호작용 있는 경우 

library(readxl)
salesRecord3 <- read_excel("salesRecord3.xlsx")
View(salesRecord3)

  # encode a vector as a factorr
salesRecord3$PT<- factor(salesRecord3$payType) 
salesRecord3$LOC<- factor(salesRecord3$City) 

  # lm(), anova(), aov()
gc.out3 <- lm(SalesRec ~ LOC + PT + LOC*PT, data = salesRecord3)
anova(gc.out3)

aov.out3 <- aov(SalesRec ~ LOC + PT + LOC*PT, data = salesRecord3)
summary(aov.out3)





#chapter 14

warpbreaks <- read.csv("warpbreaks.csv")
warpbreaks

levels(warpbreaks$tension)
levels(warpbreaks$wool)

warpbreaks$tension <- factor(warpbreaks$tension, levels = c("L", "M", "H"))
levels(warpbreaks$tension)

attach(warpbreaks)
tapply(breaks, wool, mean)
tapply(breaks, tension, mean)
tapply(breaks, list(wool, tension), mean)

boxplot(breaks~wool+tension, col = 'red', data = warpbreaks)

interaction.plot(tension, wool, breaks, col=c("red", "blue"))   #종속변수가 마지막,  교차하면 상호작용이 있음


out1 <- lm(breaks~wool+tension+wool*tension, data= warpbreaks)  #방법 1
out1 <- lm(breaks~wool*tension, data= warpbreaks)               #방법 2
anova(out1)


par(mfrow=c(2,2))                                     #회귀진단,  잔차 정규성 검정
plot(out1)
shapiro.test(resid(out1))


library(readxl)
warpbreaks <- read.csv("warpbreaks.csv")
View(warpbreaks)

gc.out2 <- aov(breaks ~ factor(tension) + factor(wool), data = warpbreaks)
anova(gc.out2)
