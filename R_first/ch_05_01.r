### 5강

TestResult <- read.csv("ch_05.csv", header = TRUE, stringsAsFactors=TRUE)
TestResult

summary(TestResult$TestResult)
min(TestResult$TestResult)
max(TestResult$TestResult)
range(TestResult$TestResult)
median(TestResult$TestResult)
quantile(TestResult$TestResult)

mean(TestResult$TestResult)
sd(TestResult$TestResult)
var(TestResult$TestResult)
sum(TestResult$TestResult)

install.packages("fBasics")
library(fBasics)

hist(TestResult$TestResult)
skewness(TestResult$TestResult) # 왜도
kurtosis(TestResult$TestResult) # 첨도

CV_TR <- sd(TestResult$TestResult)/mean(TestResult$TestResult) * 100
CV_TR

pearson_skewness <- 3 * (mean(TestResult$TestResult)-median(TestResult$TestResult))/sd(TestResult$TestResult)
pearson_skewness
