#单因素方差分析 One-way ANOVA
lamp<-data.frame(
   X=c(1600, 1610, 1650, 1680, 1700, 1700, 1780, 1500, 1640, 
       1400, 1700, 1750, 1640, 1550, 1600, 1620, 1640, 1600, 
       1740, 1800, 1510, 1520, 1530, 1570, 1640, 1600),
   A=factor(c(rep(1,7),rep(2,5), rep(3,8), rep(4,6)))
)
lamp.aov<-aov(X ~ A, data=lamp)
summary(lamp.aov)
> summary(lamp.aov)
            Df Sum Sq Mean Sq F value Pr(>F)
A            3  49212   16404   2.166  0.121
Residuals   22 166622    7574 

##零假设：各总体的均值相等
##检验统计量 F值=样板间方差/样本内方差
##如果p值<0.05, 就说明F落在拒绝域，可以拒绝零假设
##否则就不能拒绝零假设，可以认为总体均值是相等的。

source("anova.tab.R"); anova.tab(lamp.aov)

##画出箱线图
plot(lamp$X~lamp$A)

