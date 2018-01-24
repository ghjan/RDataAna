#体重，岁数->收缩压
blood<-data.frame(
   X1=c(76.0, 91.5, 85.5, 82.5, 79.0, 80.5, 74.5, 
        79.0, 85.0, 76.5, 82.0, 95.0, 92.5),
   X2=c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55, 
        40, 40, 20),
   Y= c(120, 141, 124, 126, 117, 125, 123, 125,
        132, 123, 132, 155, 147)
)
lm.sol<-lm(Y ~ X1+X2, data=blood)
summary(lm.sol)


y.res<-resid(lm.sol)
y.fit<-predict(lm.sol)
plot(y.res~y.fit)

y.rst<-rstandard(lm.sol)
plot(y.rst~y.fit)

##误差的独立性
library(car)
durbinWatsonTest(lm.sol)

##线性模型是否适合
##成分残差图(component plus residual)
crPlots(lm.sol, one.page=T, ask=F)

##等方差性
###记分检验 p>0.05 等方差性 否则就是异方差性
ncv.test(lm.sol)
###散点图
###car包spreadLevelPlot()函数创建一个添加了最佳拟合曲线的散点图，展示标准化残差绝对值与拟合值的关系。
### 同方差性的条件下，点应在线的周围水平随机分布。非水平的曲线暗示着异方差性。
spreadLevelPlot(lm.sol)


