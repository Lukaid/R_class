
# t-test 1 : one sample t-test

sales <- c(140, 120, 85, 75, 125, 120, 130, 110)

t.test(sales)
t.test(sales, mu = 120)
t.test(sales, mu = 120, conf.level= 0.95)
t.test(sales, mu = 120, conf.level= 0.99)
t.test(sales, alternative = 'less', mu = 120)
t.test(sales, alternative = 'greater', mu = 120)


# t-test 2 : paired t-test

sales1 <- c(150, 140, 135, 90, 115, 120, 145,  135)
sales2 <- c(140, 120, 85, 75, 125, 120, 130, 110)

t.test(sales1, sales2, paired = T)
t.test(sales1, sales2, alternative = 'two.sided', paired = T)

Var_sales1 <- var(sales1)













# t-test 3 : Two Sample t-test
sales1 <- c(150, 140, 135, 90, 115, 120, 145,  135)
sales2 <- c(140, 120, 85, 75, 125, 120, 130, 110)

t.test(sales1, sales2, var.equal = T)  
t.test(sales1, sales2, var.equal = F)


# F-test 1 : Two Sample F-test

sales1 <- c(150, 140, 135, 90, 115, 120, 145,  135)
sales2 <- c(140, 120, 85, 75, 125, 120, 130, 110)

var.test(sales1, sales2)



# chisquater-test 1 : one Sample 

var_test_TwoSided <- function(n, Sigma0Squared, SSquared, alpha)
  {
    df <- n - 1
    v <- df*(SSquared)/Sigma0Squared
     # upper.critical <- qchisq((1-alpha), df, lower.tail = FALSE) # right-sided test
     # lower.critical <- qchisq((alpha), df, lower.tail = FALSE) # left-sided test
     upper.critical <- qchisq((1-alpha/2), df, lower.tail = FALSE) # two-sided test
     lower.critical <- qchisq(alpha/2, df, lower.tail = FALSE) # two-sided test
    
    print(paste("degrees of freedom = ", df), quote = FALSE)
    print(paste("population variance = ", Sigma0Squared), quote = FALSE)
    print(paste("sample variance = ", SSquared), quote = FALSE) 
    print(paste("significance level = ", alpha), quote = FALSE)
    print(paste("confidence level = ", 1-alpha), quote = FALSE)
    print("  ")
    print(paste((1-alpha)*100, "% confidence interval for variance"), quote=FALSE)
    print(paste("   upper critical limit = ", round(upper.critical, 2)), quote = FALSE)
    print(paste("   lower critical limit = ", round(lower.critical, 2)), quote = FALSE)
    print(paste("   chisq statistic      = ", round(v, 2)), quote = FALSE)
    print("  ")
    print(paste("P-value =", round(pchisq(v, df, lower.tail=FALSE),4)), quote = FALSE)
    }


    var_test_TwoSided(n=30, Sigma0Squared=1.2, SSquared=1.7, alpha=0.05)


   
    
    
     var_test_TwoSided(n=8, Sigma0Squared=400, SSquared=Var_sales1, alpha=0.05)
    

