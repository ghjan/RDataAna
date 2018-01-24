#数据分析之美：决策树R语言实现
##http://blog.csdn.net/yangzhongblog/article/details/47151837
##R语言实现决策树
##数据集：ISLR包中的Carseats数据集
#1.准备数据
install.packages("tree")  
library(tree)  
library(ISLR)  
attach(Carseats)  
High=ifelse(Sales<=8,"No","Yes") #set high values by sales data to calssify  
Carseats=data.frame(Carseats,High) #include the high data into the data source  
fix(Carseats) 

#2.生成决策树
tree.carseats=tree(High~.-Sales,Carseats)  
summary(tree.carseats)  

#output training error is 9%  
    Classification tree:  
    tree(formula = High ~ . - Sales, data = Carseats)  
    Variables actually used in tree construction:  
    [1] "ShelveLoc"   "Price"       "Income"      "CompPrice"   "Population"   
    [6] "Advertising" "Age"         "US"           
    Number of terminal nodes:  27   
    Residual mean deviance:  0.4575 = 170.7 / 373   
    Misclassification error rate: 0.09 = 36 / 400 
##错误率9%

#3. 显示决策树
plot(tree.carseats )  
text(tree.carseats ,pretty =0)      

#4.Test Error
#prepare train data and test data  
#We begin by using the sample() function to split the set of observations sample()
# into two halves, by selecting a random subset of 200 observations out of the
#  original 400 observations.   
##看到这个树比较复杂，错误率不高，是不是过度拟合？

##把随机分成两组——训练集与测试集，查看决策树的正确率
set.seed(1)  
train=sample(1:nrow(Carseats),200)  
Carseats.test=Carseats[-train,]  
High.test=High[-train]  
#get the tree model with train data  
tree.carseats =tree(High~.-Sales , Carseats , subset =train )  
#get the test error with tree model, train data and predict method  
#predict is a generic function for predictions from the results of various model fitting functions.  
tree.pred = predict( tree.carseats , Carseats.test ,type ="class")  
table(tree.pred ,High.test)  
  High.test
  tree.pred No Yes
  No  98  28
  Yes 18  56
> (98+56) /200  
[1] 0.77
## 正确率 77%
##测试集和训练集效果差别很大
##说明已经过度拟合

#5.决策树剪枝

# Next, we consider whether pruning the tree might lead to improved results. The function cv.tree() performs cross-validation in order to cv.tree() determine the optimal level of tree complexity; cost complexity pruning is used in order to select a sequence of trees for consideration.
# 
# For regression trees, only the default, deviance, is accepted. For classification trees, the default is deviance and the alternative is misclass (number of misclassifications or total loss).
# We use the argument FUN=prune.misclass in order to indicate that we want the classification error rate to guide the cross-validation and pruning process, rather than the default for the cv.tree() function, which is deviance.
# 
# If the tree is regression tree,
# > plot(cv.boston$size ,cv.boston$dev ,type=’b ’)

set.seed(3)  
cv.carseats =cv.tree(tree.carseats ,FUN = prune.misclass ,K=10)  
#The cv.tree() function reports the number of terminal nodes of each tree 
#considered (size) as well as the corresponding error rate(dev) 
#and the value of the cost-complexity parameter used 
#(k, which corresponds to α.  
> names(cv.carseats )  
    [1] " size" "dev " "k" " method "  
> cv.carseats
    $size
    [1] 20 16  7  5  4  2  1

    $dev
    [1] 59 60 59 60 72 85 90

    $k
    [1] -Inf  0.0  2.0  2.5  3.0  9.0 18.0

    $method
    [1] "misclass"

    attr(,"class")
    [1] "prune"         "tree.sequence"

#plot the error rate with tree node size to see which node size is best  
plot(cv.carseats$size ,cv.carseats$dev ,type='b')  
  
# Note that, despite the name, dev corresponds to the 
#cross-validation error rate in this instance.
#The tree with 7 terminal nodes results in the lowest cross-validation error rate,
#with 59 cross-validation errors.
#We plot the error rate as a function of both size and k.  

prune.carseats = prune.misclass(tree.carseats , best =7)  
plot( prune.carseats )  
text( prune.carseats , pretty =0)  
  
#get test error again to see whether the this pruned tree perform on the test data set  
tree.pred = predict(prune.carseats , Carseats.test , type ="class")  
table(tree.pred ,High.test)  
             High.test
    tree.pred No Yes
          No  96  30
          Yes 20  54    
> (96+54) /200  
[1] 0.75

#剪枝以后，正确率变化
0.77->0.75

##树的剪枝2
set.seed(4)
cv.carseats2 =cv.tree(tree.carseats ,FUN = prune.misclass)  

names(cv.carseats2)

cv.carseats2
##dev表示错误率，最低错误率是60，对应到size是4
par(mfrow=c(1,2))
plot(cv.carseats2$size, cv.carseats2$dev, type='b')
plot(cv.carseats2$k, cv.carseats2$dev, type='b')
prune.carseats = prune.misclass(tree.carseats, best=4)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
             High.test
    tree.pred No Yes
          No  92  29
          Yes 24  55
> (92+55)/200
[1] 0.735          
##怎么越来越差了!!!
#
#画决策树第2种方法，画出来的树稍微好看些
library(rpart.plot)
rpart.plot(prune.carseats,branch=1,shadow.col="gray",
    box.col="green",border.col="blue",split.col="red",split.cex=1.2,main="决策树")
#
detach(Carseats)
          