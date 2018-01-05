######读入上证指数######
index.dt <- read.table("E:/dataguru/RDataAna/week15/TRD_Index.txt", header=TRUE)
head(index.dt)
tail(index.dt)
##上证指数
SH.index <- index.dt[index.dt$Indexcd == 1, -1]
head(SH.index)
tail(SH.index)

######xts包介绍######
library(xts)
#转变为xts格式
xtsible(SH.index)
	[1] FALSE
SH.index <- xts(SH.index[, -1], order.by = as.Date(SH.index$Trddt))
head(SH.index)
class(SH.index)
plot.xts(SH.index$Retindex)

#解除xts格式
##丢失了日期信息
head(coredata(SH.index))
class(coredata(SH.index))

#包里面自带的sample_matrix
#可以转换为xts数据格式的矩阵 一般来说行名是日期
data(sample_matrix)
tail(sample_matrix)
	               Open     High      Low    Close
	2007-06-25 47.20471 47.42772 47.13405 47.42772

xtsible(sample_matrix)
sample.xts <- as.xts(sample_matrix)
head(sample.xts)
class(sample.xts)
	[1] "xts" "zoo"
#xts数据子集
##交易日期作为标签，选取数据子集特别容易
SH.index.2012 <- SH.index["2012-01-01/2013-01-01"]
head(SH.index.2012, 3)
tail(SH.index.2012, 3)

SH.index.b2012 <- SH.index["/2013-01-01"]
head(SH.index.b2012, 3)

SH.index.2012 <- SH.index["2012"]
head(SH.index.2012,3)
tail(SH.index.2012,3)

SH.index.after2010 <- SH.index["2010-01-01/"]
head(SH.index.after2010, 3)
tail(SH.index.after2010, 3)

SH.index.2010MartoEnd <- SH.index["2010-03/2010"]
head(SH.index.2010MartoEnd,3)
tail(SH.index.2010MartoEnd,3)



######quantmod包介绍######
# 使用quantmod包和中国移动数据制图
library(quantmod)
getSymbols("CHL",src="yahoo")
barChart(CHL,theme="white")
set1=CHL[500:700]
candleChart(set1,theme="white")
chartSeries(CHL,theme="white")
chartSeries(CHL[,1],name="Open price for CHL",theme="white")
require(TTR)
chartSeries(CHL[600:764,],theme="white")
##添加指标MACD
addMACD()  
##添加布林带
addBBands()


######时间序列的描述性统计######
index.dt <- read.table("E:/dataguru/RDataAna/week15/TRD_Index.txt", header=TRUE)
##上证指数
SH.index <- index.dt[index.dt$Indexcd == 1, -1]
SH.ohlc <- SH.index[, c("Trddt", "Opnindex", "Hiindex", "Loindex", "Clsindex")]
SH.ohlc <- xts(SH.ohlc[, -1], order.by = as.Date(SH.ohlc$Trddt))
SH.ret <- weeklyReturn(SH.ohlc)
hist(SH.ret, breaks = 50, col = 'darkgreen', border = FALSE, 
     main = "Histogram of SH Index Weekly Return")
##描述性统计 需要先转换为matrix
summary(coredata(SH.ret))
##十分位数
quantile(coredata(SH.ret), probs = seq(0, 1, 0.1))
mean(coredata(SH.ret))
sd(coredata(SH.ret))

##我们有更加容易的方法获取描述性统计特性
library(fBasics)
basicStats(coredata(SH.ret))
	            weekly.returns
	nobs            255.000000
	NAs               0.000000
	Minimum          -0.078986
	Maximum           0.095738
	1. Quartile      -0.019884
	3. Quartile       0.018918
	Mean              0.000890
	Median            0.001307
	Sum               0.227019
	SE Mean           0.001829
	LCL Mean         -0.002711
	UCL Mean          0.004491
	Variance          0.000853
	Stdev             0.029200
	Skewness          0.222658  #偏度
	Kurtosis          0.133005  #超额峰度 原峰度-3

#自相关性	
##自己和自己的相关性
##我们期望时间序列时间有自相关性，以便根据过去的数据推断未来
######自相关初步判断#########
##自相关函数/自相关曲线ACF
###https://www.cnblogs.com/xuanlvshu/p/5410721.html
###自协方差 自相关系数
###偏自相关系数PACF 在AR模型里面非常重要
###偏相关系数是在排除了其他变量的影响之后两个变量之间的相关系数。
acf(SH.ret)
pacf(SH.ret)
######自相关性检验，判断是否为白噪声-纯粹的随机过程#########
Box.test(SH.ret, lag = 12)   #lag表示检验当前与过去lag期的相关系数是否为0
		Box-Pierce test

	data:  SH.ret
	X-squared = 19.682, df = 12, p-value = 0.07335

# 结论：
# 原假设是纯粹随机过程
# p-value>0.05,表明无法拒绝零假设
# 所以上证指数是个纯粹随机过程

######平稳性检验，单位根检验#####
# 时间序列是指将某种现象某一个统计指标在不同时间上的各个数值，按时间先后顺序排列而形成的序列。
# 平稳时间序列粗略地讲，一个时间序列，如果均值没有系统的变化（无趋势）、方差没有系统变化，
# 且严格消除了周期性变化，就称之是平稳的。
library(urca)
summary(ur.df(SH.ret, type = 'none'))
	############################################### 
	# Augmented Dickey-Fuller Test Unit Root Test # 
	############################################### 

	Test regression none 


	Call:
	lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)

	Residuals:
	      Min        1Q    Median        3Q       Max 
	-0.084836 -0.019150  0.000428  0.019196  0.092753 

	Coefficients:
	           Estimate Std. Error t value Pr(>|t|)    
	z.lag.1    -0.87929    0.08757 -10.041   <2e-16 ***
	z.diff.lag -0.09979    0.06269  -1.592    0.113    
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	Residual standard error: 0.02917 on 251 degrees of freedom
	Multiple R-squared:  0.4941,	Adjusted R-squared:  0.4901 
	F-statistic: 122.6 on 2 and 251 DF,  p-value: < 2.2e-16


	Value of test-statistic is: -10.0411 

	Critical values for test statistics: 
	      1pct  5pct 10pct
	tau1 -2.58 -1.95 -1.62

#Value of test-statistic比Critical values for test statistics的值都小
#就判断为没有单位根，即为平稳时间序列

时间序列 vs. 回归模型
	回归模型重点在于长期的一个趋势
	而时间序列里面还关注周期性变化：季节变动，更长周期变动，经济周期变动
	自回归现象


时间序列的组成部分
	长期趋势
	季节变动
	循环变动
		季节变动周期比较短
		循环周期比较长
		而且循环变动的周期未必是固定的
	不规则变动

时间序列的数学模型
	加法模型 Y=T+S+C+I 
		T:trend
		S:season
		C:cycle 
		I:随机扰动
	乘法模型 Y=T*S*C*I
		意味着这些部分不是独立的，是互相影响的

平稳时间序列 
	随机变量的均值和方差与时间无关
		变化的范围在一个狭长的矩形区域
	随机变量Yt和Ys的协方差只与时间差（步长）t-s有关
	对于平稳时间序列在数学上有比较丰富的处理手段，非平稳的时间序列通过差分等手段转化为平稳时间序列

平稳时间序列
	白噪声
		不同的时间下标𝜀𝑡和𝜀𝑠的协方差为0
		用于描述简单随机干扰
		泊松白噪声、布朗白噪声
	自回归模型（AR）
		AR(p)
	滑动平均模型(MA)
		描述自回归模部分的误差累计
		MA(q)
	自回归滑动平均模型(ARMA)
		ARMA(p,q)
		AR模型和MA模型是ARMA模型的特例

ARIMA模型
	Autoregressive Integrated Moving Average 差分自回归滑动平均模型 或者 求和自回归滑动平均模型
	ARIMA(p,d,q),AR是自回归，p为自回归项数；MA为滑动平均，q为滑动平均项数，d为使之成为平稳序列所做的差分次数（阶数）
	ARMA的扩展
	滞后算子表示法（自学）

15b结束


######CPI序列建模######
library(forecast)
#读取数据
CPI<-read.csv("D:/R Quant/Data/CPI/CPI.csv",header=T)
CPI <- na.omit(CPI)
CPI.xts <-xts(CPI[,-1],order.by = as.Date(CPI$time))
head(CPI.xts)
tail(CPI.xts)

CPI.xts.treat <- CPI.xts[1:(nrow(CPI.xts)-3),]#构建测试集
tail(CPI.xts.treat)
plot.xts(CPI.xts, main = "CPI 2001-2014")
#平稳性检验
library(urca)
summary(ur.df(CPI.xts.treat,lags=5,type = 'drift'))

library(tseries)
adf.test(CPI.xts.treat)

#白噪声检验
acf(CPI.xts.treat)
pacf(CPI.xts.treat)
Box.test(CPI.xts.treat, lag=12)

#模型拟合
CPI.arma <- auto.arima(CPI.xts.treat, stationary = TRUE, seasonal = FALSE, ic = 'aic')
summary(CPI.arma)
confint(CPI.arma) #计算系数的置信区间

#残差检验
accuracy(CPI.arma)
tsdiag(CPI.arma) 

#拟合值与原数据比较
plot(CPI.arma$x, lwd=2, col='darkgreen',
     main="CPI: Raw Data vs Fitted Values",
     ylab="CPI", xlab="time")
lines(fitted(CPI.arma),lty=1, lwd=2, col='red')

#预测并比较
pred.last3 <- t(predict(CPI.arma, n.ahead = 3)$pred[1:3])
pred.whole <- cbind(t(fitted(CPI.arma)), pred.last3)
CPI.xts.pred <- xts(t(pred.whole), order.by=as.Date(CPI$time))

plot(CPI.xts,main="CPI: Raw Data vs Predicted Values",
     ylab="CPI", xlab="time")
lines(CPI.xts.pred,lty=1, lwd=2, col='red')



######标普500（S&P500）指数的日对数收益率序列建模######
da=read.table("D:/data/d-sp55008.txt",header=T)
sp5=log(da[,7])
dsp5=diff(sp5)
plot(dsp5)

#平稳性检验
library(fUnitRoots)
m1=ar(dsp5,method='mle') # Based on AIC
m1$order
adfTest(sp5,lags=2,type=("ct"))
summary(ur.df(sp5,type="trend",lags=2))


#白噪声检验
acf(diff(dsp5))
pacf(diff(dsp5))
Box.test(diff(dsp5), lag=12)


#模型拟合
dsp5.arima <- auto.arima(dsp5, stationary =T, seasonal = F, ic = 'aic')
summary(dsp5.arima)
tsdiag(dsp5.arima)

eacf(diff(dsp5))

tdx=c(1:length(dsp5))
m3=arima(dsp5,order=c(1,1,2),xreg=tdx)
m3

tsdiag(m3)
