
#행렬 만들기 

x1 <- c(45,57,43,67,58, 39,57,35,58,57, 67,48,65,84,67, 90,35,57)
x2 <- c(56,68,69,79,55, 58,79,46,69,82, 84,52,74,68,48, 68,39,58)
x3 <- c(56,68,79,73,86, 54,58,79,47,69, 57,68,79,57,58, 57,58,58)
x4 <- c(63,85,83,45,54, 56,67,47,79,80, 75,53,57,67,46, 58,84,52)
x5 <- c(53,64,58,68,43, 48,63,55,70,78, 77,53,66,75,67, 87,56,46)
yy <- c(11,23,33,45,54, 45,56,22,45,65, 66,84,67,32,78, 85,56,90)
xx<- cbind(x1,x2,x3,x4,x5)   
round(cor(xx), 3)


#요인분석 사례

service <- read.csv('service.csv')

house <- read.csv('house.csv')
attach(house)
xx<- cbind(x1,x2,x3,x4,x5)

round(cor(xx), 3)

install.packages("psych")      #요인분석에 필요한 함수가 있는 패키지
library(psych)

cortest.bartlett(cor(xx))      #바틀렛 구형성 검증, 단위행렬 아님

eigen(cor(xx))$values          #고유값 , 2개의 요인이 적합 


(ff <- factanal(xx, factors = 2, scores = c("regression"), rotation = 'varimax'))

#요인분석

  
ff$loadings     #결과값 보기
ff$scores

names(ff)


#추가 분석 : 적재값 barpolt

cols = c("skyblue", "yellow", "lightgreen", "light gray", "light pink")
bf <- round(abs(ff$loadings), 3)
barplot(bf, main = "Bar Plot: factor loadings", col = cols)
for(i in 0:4) {
  text(0.7+1.2*i, bf[1, 1+i]/2, paste("안전성", bf[1, 1+i]), cex = 0.7)
  text(0.7+1.2*i, bf[1, 1+i]+bf[2, 1+i]/2, paste("보건성", bf[2, 1+i]), cex = 0.7)
  text(0.7+1.2*i, bf[1, 1+i]+bf[2, 1+i]+bf[3, 1+i]/2, paste("편리성", bf[3, 1+i]), cex = 0.7)
  text(0.7+1.2*i, bf[1, 1+i]+bf[2, 1+i]+bf[3, 1+i]+bf[4, 1+i]/2, paste("쾌적성", bf[4, 1+i]), cex = 0.7)
  text(0.7+1.2*i, bf[1, 1+i]+bf[2, 1+i]+bf[3, 1+i]+bf[4, 1+i]+bf[5, 1+i]/2, paste("효율성", bf[5, 1+i]), cex = 0.7)    }

#추가 분석 : 요인과 변수 위치

colnames(xx) = c("(안전성)","(보건성)","(편리성)","(쾌적성)","(효율성)" )
plot(ff$loadings[ , 1:2], type = "n", xlim = c(-0.3, 1.2))
text(ff$loadings[ , 1:2],colnames(xx))
grid()
title("Factor analysis: Positioning of Variables")



#추가분석 : 관찰치들의 요인점수

rownames(xx) <- c(seq(1:18))
plot(ff$scores[ , 1:2],type ="n")
text(ff$scores[ , 1:2],rownames(xx))
grid()
title("Factor analysis: Factor scores at obervations")


#추가분석 : scree plot

plot(eigen(cor(xx))$values, type= "b", 
     xlab = 'Eigenvalue Number', 
     ylab = 'Eigenvalue Scores')
abline(h=1)
title('Scree plot of Eigenvalues')
grid()






# 신뢰성 분석
install.packages("fmsb")
library(fmsb)

attitude
attitude1 <- attitude[2:7]
CronbachAlpha(attitude1)     

attitude2 <- attitude[3:7]
CronbachAlpha(attitude2)     

attitude3 <- cbind(attitude[2],attitude[4:7])
CronbachAlpha(attitude3)

attitude4 <- cbind(attitude[2:3],attitude[5:7])
CronbachAlpha(attitude4)

attitude5 <- cbind(attitude[2:4],attitude[6:7])
CronbachAlpha(attitude5)

attitude6 <- cbind(attitude[2:5],attitude[7])     
CronbachAlpha(attitude6)             # 제일 높은 값, critical 을 제외


