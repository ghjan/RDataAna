alloy<-data.frame(
   x=c(37.0, 37.5, 38.0, 38.5, 39.0, 39.5, 40.0, 
       40.5, 41.0, 41.5, 42.0, 42.5, 43.0),
   y=c(3.40, 3.00, 3.00, 3.27, 2.10, 1.83, 1.53, 
       1.70, 1.80, 1.90, 2.35, 2.54, 2.90)
)
lm.sol<-lm(y~1+x+I(x^2),data=alloy)
summary(lm.sol)
> summary(lm.sol)

Call:
lm(formula = y ~ 1 + x + I(x^2), data = alloy)

Residuals:
        Min          1Q      Median          3Q         Max 
-0.33321678 -0.14221778 -0.07922078  0.05274725  0.84577423 

Coefficients:
                Estimate   Std. Error  t value   Pr(>|t|)    
(Intercept) 257.06961039  47.00295470  5.46922 0.00027336 ***
x           -12.62031968   2.35377071 -5.36175 0.00031824 ***
I(x^2)        0.15600400   0.02941581  5.30341 0.00034585 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3290432 on 10 degrees of freedom
Multiple R-squared:  0.7843106, Adjusted R-squared:  0.7411727 
F-statistic: 18.18148 on 2 and 10 DF,  p-value: 0.0004668139

xfit<-seq(37,43,len=200)
yfit<-predict(lm.sol, data.frame(x=xfit))
plot(alloy$x,alloy$y)
lines(xfit, yfit)

poly(alloy$x,2)

lm.pol<-lm(y~1+poly(x,2),data=alloy)
summary(lm.pol)
> summary(lm.pol)

Call:
lm(formula = y ~ 1 + poly(x, 2), data = alloy)

Residuals:
        Min          1Q      Median          3Q         Max 
-0.33321678 -0.14221778 -0.07922078  0.05274725  0.84577423 

Coefficients:
               Estimate  Std. Error  t value         Pr(>|t|)    
(Intercept)  2.40923077  0.09126017 26.39959 0.00000000014021 ***
poly(x, 2)1 -0.94435163  0.32904320 -2.86999       0.01666894 *  
poly(x, 2)2  1.74504957  0.32904320  5.30341       0.00034585 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3290432 on 10 degrees of freedom
Multiple R-squared:  0.7843106, Adjusted R-squared:  0.7411727 
F-statistic: 18.18148 on 2 and 10 DF,  p-value: 0.0004668139

xfit<-seq(37,43,len=200)
yfit<-predict(lm.pol, data.frame(x=xfit))
plot(alloy$x,alloy$y)
lines(xfit, yfit)

