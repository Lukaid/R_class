#Chapter 11

# 변수가 많지 않은 경우 Backward elimination 권장

attitude_v <- attitude

pairs(attitude)                    #변수간 산포도를 그려줌
cor(attitude)                      #변수간 상관계수를 계산해줌

out <- lm(rating ~ ., data = attitude)      #모든 독립변수를 다 넣어서 회귀분석
summary(out)                                
anova(out)

library(car)    
out <- lm(rating ~ ., data = attitude)
vif(out)                                 #다중공선성
#vif값이 10 안넘으면 써도 됨?

out2 <- lm(rating ~ complaints+learning+advance, data = attitude)    #p-value가 큰 변수 제거 후 회귀분석
anova(out2, out)                                                     #모형 비교 anova(작은모형, 큰모형),  F-test
                             #두 모형간 차이가 없다면(p-value>0.05) 변수 제거해도 모형에 영향이 없다는 뜻


out3 <- lm(rating ~ complaints+learning, data = attitude)        #영향력이 작은 변수를 하나 더 제거 후 회귀분석
anova(out3, out2, out)                                           #모형 비교 anova(더 작은 모형, 작은모형, 큰모형)

summary(out2)
summary(out3)                #최종모형으로 선택, 회귀분석 결과 확인


par(mfrow=c(2,2))            #회귀진단
plot(out3)
shapiro.test(resid(out3))

vif(out3)

#추가적으로 더 제거

out4 <- lm(rating ~ complaints, data = attitude)        #영향력이 작은 변수를 하나 더 제거 후 회귀분석
anova(out4, out3)                                       #모형 비교 

summary(out4)                


par(mfrow=c(2,2))              #회귀진단
plot(out4)
shapiro.test(resid(out4))     #정규성을 만족하지 못함




#조정된 결정계수로 비교하여 모형 선택하는 경우 
# out --->    독립변수  6개     결정계수  0.7326     조정된 결정계수   0.6628
# out2 --->   독립변수  3개     결정계수  0.7256     조정된 결정계수   0.6939    ** 선택 
# out3 --->   독립변수  2개     결정계수  0.7080     조정된 결정계수   0.6864
# out4 --->   독립변수  1개     결정계수  0.6813     조정된 결정계수   0.6699





#두 번째 사례(연습문제)

library(MASS)

birthwt_v <- birthwt

out <- lm(bwt ~ age+lwt+factor(race)+smoke+ptl+ht+ui, data = birthwt)
anova(out)
summary(out)

out2 <- lm(bwt~lwt+factor(race)+smoke+ht+ui, data = birthwt)
anova(out2, out)

anova(out2)
summary(out2)

par(mfrow=c(2,2))            #회귀진단
plot(out2)
shapiro.test(resid(out2))

vif(out2)




#세 번째 사례(다항 회귀모형)

summary(lm(dist ~ speed + I(speed^2), data = cars))
with(cars, cor(speed, speed^2))

summary(lm(dist~speed + I(speed^2) -1, data = cars))

out_s <- lm(dist~speed + I(speed^2) -1, data = cars)
library(car)    
vif(out_s)        #다중공선성
