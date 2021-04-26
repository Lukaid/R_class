#피어슨 상관분석

library(readxl)
BodyRecord <- read_excel("BodyRecord.xlsx")
View(BodyRecord)

# 상관분석의 p-value는 상관계수가 0이라는 귀무가설에 대한 것
# 귀무가설이 채택되는 경우는 많지 않음
# 따라서 cor값을 더 중시하여 봐야함
cor.test(BodyRecord$height, BodyRecord$weight)


#편상관분석



Data1 <- read_excel("Data1.xls")
# 데이터의 일부분 뽑는 방법
tset1pr <- subset(Data1, select = c("BF", "BM", "Happiness", "Peace"))
cor.test(tset1$BM, tset1$Happiness)

# install.packages("psych")
library(psych)
# 편상관분석, c(2, 3)두개를 상관분석 할건데, 공통된 c(1)은 제외하고
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
# 상관분석이지만 명목변수인것들

Data2 <- read_excel("Data2.xls")

#Group과 Age Group이 동질한지? 관계를 보고싶은 두 데이터를 ~A  + B
M=xtabs(~Group + AgeGroup, data = Data2)
M

ht.out1 <- chisq.test(M)
ht.out1
# p-value = 0.1411 동질성이 있다는 귀무가설 기각 불가

# 얘는 분산분석, 명목 + 숫자, 3개이상 분산분석
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

