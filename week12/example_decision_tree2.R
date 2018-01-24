#以鸢尾花数据集为例
library(rpart)

#第1步：工作目录和数据集的准备
setwd('R:/dataguru/RDataAna/week12')#设定当前的工作目录

#第2步：做训练集和测试集
set.seed(1)
sub<-sample(1:nrow(iris),round(nrow(iris)*2/3))
length(sub) #
data_train<-iris[sub,]#取2/3的数据做训练集
data_test<-iris[-sub,]#取1/3的数据做测试集
dim(data_train)#训练集行数和列数  100 5
dim(data_test) #测试集的行数和列数 100 5
table(data_train$Species) 
    setosa versicolor  virginica 
            32         36         32 
table(data_test$Species)
    setosa versicolor  virginica 
            18         14         18 

#第3步：用训练集建模，观察模型结果
# tree.both<-rpart(Species~ .,data=data_train,method='class',minsplit=20,minbucket=150,cp=0.00017)
tree.both<-rpart(Species~ .,data=data_train,method='class')
##minsplit：the minimum number of observations that must exist in a node in order for a split to be attempted.
##minbucket：the minimum number of observations in any terminal <leaf> node. 
###If only one of minbucket or minsplit is specified, 
###the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.
##cp:complexity parameter
summary(tree.both)
tree.both$variable.importance
 Petal.Width Petal.Length Sepal.Length  Sepal.Width 
    60.85957     56.58958     36.29702     21.48091
printcp(tree.both)
    Classification tree:
    rpart(formula = Species ~ ., data = data_train, method = "class")

    Variables actually used in tree construction:
    [1] Petal.Length Petal.Width 

    Root node error: 64/100 = 0.64

    n= 100 

           CP nsplit rel error xerror     xstd
    1 0.50000      0  1.000000 1.0625 0.072887
    2 0.45312      1  0.500000 0.5625 0.075000
    3 0.01000      2  0.046875 0.0625 0.030619
plotcp(tree.both,lwd=2) 

#第4步：画决策树
#4.1画决策树第1种方法，画出来的树比较简单
plot(tree.both)
text(tree.both,use.n=T,all=T,cex=0.9)

#4.2画决策树第2种方法，画出来的树稍微好看些
library(rpart.plot)
rpart.plot(tree.both,branch=1,shadow.col="gray",box.col="green",border.col="blue",
    split.col="red",split.cex=1.2,main="决策树")

#第5步：剪枝
#我们使用具有最小交叉验证误差的cp来建立回归树
cp=tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"]
cp #cp=0.01

#第6步：剪枝之后的树再次画图
tree.both2<-prune(tree.both,cp=tree.both$cptable[which.min(tree.both$cptable[,"xerror"]),"CP"])
summary(tree.both2)
tree.both2$variable.importance
printcp(tree.both2)
plotcp(tree.both2,lwd=2) 

library(rpart.plot)
rpart.plot(tree.both2,branch=1,shadow.col="gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2,main="决策树")

#第7步：输出规则。剪枝后的决策树规则，从规则中再找规律
library(rattle)
asRules(tree.both2)
    Rule number: 2 [Species=setosa cover=32 (32%) prob=1.00]
        Petal.Length< 2.45

    Rule number: 7 [Species=virginica cover=33 (33%) prob=0.00]
        Petal.Length>=2.45
        Petal.Width>=1.65

    Rule number: 6 [Species=versicolor cover=35 (35%) prob=0.00]
        Petal.Length>=2.45
        Petal.Width< 1.65
#第8步：在测试集上做预测
library(pROC)
pred.tree.both<-predict(tree.both,newdata=data_test)

#第9步：看模型的预测效果如何呀。正确的有多少，错误的有多少
predictScore<-data.frame(pred.tree.both)
rownames(predictScore) #看这个矩阵行的名字
colnames(predictScore)#看这个矩阵列的名字 "setosa"     "versicolor" "virginica"
##predictScore$Species<-'ok'  #在预测的矩阵后面多加一列Species
predictScore$Species <- apply(predictScore, 1, function(t) colnames(predictScore)[which.max(t)])

n<-table(data_test$Species,predictScore$Species)
n #看分布情况
                 setosa versicolor virginica
      setosa         18          0         0
      versicolor      0         14         0
      virginica       0          3        15
percantage<-c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]), n[3,3]/sum(n[3,]))
percantage
    [1] 1.0000000 1.0000000 0.8333333
    