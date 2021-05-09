#Chapter 10

#10.4

cars <- read.csv('cars.csv')
cars                                   #차량의 속도와 정지거리 

out <- lm(dist~speed, data = cars)     #lm(종속변수~독립변수)
out
summary(out)                           #자세한 분석 결과


with(cars, cor(speed, dist))^2         #결정계수와 피어슨상관계수
with(cars, cor.test(speed, dist))      #상관성 검정 결과와의 비교

plot(dist~speed, data= cars, col = 'blue')     #산점도와 회귀선 그리기
abline(out, col = 'red')

summary(lm(dist~speed -1, data = cars))        # y절편을 제거한 회귀모형  
out <- lm(dist~speed -1, data = cars)
plot(dist~speed, data= cars, col = 'blue')
abline(out, col = 'red')

plot(log(dist)~speed, data= cars)                #종속변수 y 변환
plot(sqrt(dist)~speed, data= cars)


out2 <- lm(sqrt(dist)~speed -1, data = cars)     #최종모형
summary(out2)                                    #dist_i = 0.39675^2 * speed_i^2


par(mfrow = c(2,2))                              #모형의 회귀진단 (좋은 모형인지 확인)
plot(out2)

par(mfrow = c(1,1)) 
qqnorm(resid(out2))                              #잔차의 정규성 확인
qqline(resid(out2))
shapiro.test(resid(out2))
       

#10.5 the second example

one.comp<-read.csv('one_comp.csv')
one.comp                                  #약50mg 투여 후 시간별(hout) 혈중농도(mg/L)  

par(mfrow = c(2,2))  
plot(conc~time, data= one.comp)
plot(log(conc)~time, data= one.comp)
plot(conc~time, data= one.comp, log = "y")         # y축에 log변환 전 y값을 표시
 
summary(lm(log(conc)~time, data= one.comp))        #Ct = e^(3.72031-0.63771t) = 41.27719e^(-0.63771t)

out = lm(conc~time, data= one.comp) # 종속변수 치환을 안했을 경우
par(mfrow = c(2,2))
plot(out)

#10.8 
out <- lm(dist~speed, data=cars)
names(out)

B<- coef(out)            #회귀계수
yhat <- B[1]+B[2]*cars$speed
yhat                     #predicted value
fitted(out)

resid(out)               #잔차
cars$dist-yhat


summary(out)$sigma        # 추정값의 표준오차(sqrt(MSE))
sqrt(sum(resid(out)^2)/(length(cars$dist)-2))


#10. 9 simulation
out <- lm(sqrt(dist)~speed -1, data=cars)
dist <- rnorm(n=nrow(cars), mean = fitted(out), sd=summary(out)$sigma)^2
plot(dist~cars$speed)


out <- lm(log(conc)~time, data=one.comp)
conc <- exp(rnorm(n=nrow(one.comp), mean = fitted(out), sd=summary(out)$sigma))
plot(conc~one.comp$time)


#10.10 excercise

one.comp<-read.csv('one_comp.csv')
one.comp

out3 <- lm(log(conc)~time, data= one.comp)
summary(out3)                                          #Ct = e^(3.72031-0.63771t) = 41.27719e^(-0.63771t)

par(mfrow = c(1,1)) 
plot(log(conc)~time, data= one.comp, col = 'blue')     #산점도와 회귀선 그리기
abline(out3, col = 'red')

par(mfrow = c(2,2))                                   #모형의 회귀진단 (좋은 모형인지 확인)
plot(out3)

par(mfrow = c(1,1)) 
qqnorm(resid(out3))                                    #잔차의 정규성 확인
qqline(resid(out3))
shapiro.test(resid(out3))
