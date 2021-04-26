#피어슨 상관분석

library(readxl)
BodyRecord <- read_excel("BodyRecord.xlsx")
View(BodyRecord)

cor.test(BodyRecord$height, BodyRecord$weight)


#편상관분석



Data1 <- read_excel("Data1.xls")
tset1 <- subset(Data1, select = c("BF", "BM", "Happiness", "Peace"))
cor.test(tset1$BM, tset1$Happiness)

install.packages("psych")
library(psych)
pr <- partial.r(tset1, c(2,3), c(1))
pr


#서열척도 상관분석 - 스피어만 

CandiRecord <- read_excel("CandiRecord.xlsx")
View(CandiRecord)

cor.test(CandiRecord$prefOder, CandiRecord$moralOrder, method= "spearman")


#서열척도 상관분석 - 켈달의 타우

missKoreaRecord <- read_excel("missKoreaRecord.xlsx")
View(missKoreaRecord)

cor.test(missKoreaRecord$referee, missKoreaRecord$viewers, method= "kendall")





#교차분석 - 동질성 분석, 독립성 분석

Data2 <- read_excel("Data2.xls")
M=xtabs(~Group + AgeGroup, data = Data2)
M

ht.out1 <- chisq.test(M)
ht.out1


ht.out2 <- lm(AGE~Group, data=Data2)
anova(ht.out2)





#Chapter 9
x <- 1:10
y <- x^2


cor(x,y)
cor(x,y, method = 'kendall')
cor(x,y, method = 'spearman')

attitude   #r 내장 데이터
summary(attitude)
colMeans(attitude)
nrow(attitude)
ncol(attitude)
cov(attitude)
cor(attitude)
with(attitude, cor(rating, complaints))
with(attitude, cor.test(rating, complaints))
with(attitude, cor.test(rating, complaints, method = 'kendall'))




#2021 연구조사분석론 수강생 대상 조사 결과 이용 교차분석

SC_survey2021 <- read.csv('SC_survey2021.csv')

M1=xtabs(~Group + Gender, data = SC_survey2021)       #그룹에 성별이 골고루 있는지
M1
ht.out1 <- chisq.test(M1)                         
ht.out1


#중고등학교 때 통계수업을 좋아하는 정도와 중고등학교 통계성적의 상관성
with(SC_survey2021, cor.test(Pref_MH_ST, Gr_MH_ST))      

#그룹에 연령그룹이 골고루 있는지
M1=xtabs(~Group + AgeGroup, data = SC_survey2021)       
M1
ht.out1 <- chisq.test(M1)                         
ht.out1

