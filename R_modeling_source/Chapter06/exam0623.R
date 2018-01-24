#内在非线性模型 使用极大似然估计 （或者非线性最小二乘法）
#nls()函数的用法
## 输入数据，构成数据框 
cl<-data.frame( 
   X=c(rep(2*4:21, c(2, 4, 4, 3, 3, 2, 3, 3, 3, 3, 2, 
       3, 2, 1, 2, 2, 1, 1))),
   Y=c(0.49, 0.49, 0.48, 0.47, 0.48, 0.47, 0.46, 0.46, 
       0.45, 0.43, 0.45, 0.43, 0.43, 0.44, 0.43, 0.43, 
       0.46, 0.45, 0.42, 0.42, 0.43, 0.41, 0.41, 0.40, 
       0.42, 0.40, 0.40, 0.41, 0.40, 0.41, 0.41, 0.40, 
       0.40, 0.40, 0.38, 0.41, 0.40, 0.40, 0.41, 0.38, 
       0.40, 0.40, 0.39, 0.39)
)

## 作非线性拟合，并输出各参数的估计值
nls.sol<-nls(Y~a+(0.49-a)*exp(-b*(X-8)), data=cl,
             start = list( a= 0.1, b = 0.01 ))
nls.sum<-summary(nls.sol); nls.sum

Formula: Y ~ a + (0.49 - a) * exp(-b * (X - 8))

Parameters:
     Estimate  Std. Error  t value               Pr(>|t|)    
a 0.390140062 0.005044928 77.33313 < 0.000000000000000222 ***
b 0.101632845 0.013360271  7.60709        0.0000000019935 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.01091273 on 42 degrees of freedom

Number of iterations to convergence: 19 
Achieved convergence tolerance: 0.00000136456

## 画出拟合曲线和散点图
xfit<-seq(8,44,len=200)
yfit<-predict(nls.sol, data.frame(X=xfit))
plot(cl$X, cl$Y)
lines(xfit,yfit)

> nls.sum$sigma
[1] 0.01091272696

## 计算偏导数和相应的Jacobi矩阵
fn<-function(a, b, X){
   f1 <- 1-exp(-b*(X-8))
   f2 <- -(0.49-a)*(X-8)*exp(-b*(X-8))
   cbind(f1,f2)
}
D<-fn(nls.sum$parameters[1,1], nls.sum$parameters[2,1], cl$X)

## 作theta的方差估计
theta.var<-nls.sum$sigma^2*solve(t(D)%*%D); theta.var

## 输出标准差(计算结果)
sqrt(theta.var[1,1])
sqrt(theta.var[2,2])

## 输出标准差(已有结果), 两者是相同的
nls.sum$parameters[,2]

setwd('R:/dataguru/RDataAna/R_modeling_source/Chapter06')
## 计算参数的区间估计
source("paramet.int.R")
paramet.int(nls.sol)
      Estimate          Left        Right
a 0.3901400625 0.38509513490 0.3951849901
b 0.1016328454 0.08827257404 0.1149931168