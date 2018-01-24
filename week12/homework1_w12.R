#1 使用rpart包建立其内置的kyphosis数据集（其内容意义可以参考rpart的介绍文档）的决策树模型，
#使用rpart.plot包画出该模型的决策树 

#第1步：工作目录和数据集的准备
setwd('R:/dataguru/RDataAna/week12')#设定当前的工作目录
library(rpart)
#第2步：做训练集和测试集
set.seed(1)  
train=sample(1:nrow(kyphosis),ceiling(dim(kyphosis)[1]/2)  )
data_train=kyphosis[train,]
data_test=kyphosis[-train,]  

dim(data_train)#训练集行数和列数  41 4
dim(data_test) #测试集的行数和列数 40 4
table(data_train$Kyphosis) 
    absent present 
    34       7 
table(data_test$Kyphosis)
    absent present 
     30      10 

#第3步：用训练集建模，观察模型结果
##把随机分成两组——训练集与测试集，查看决策树的正确率

#get the tree model with train data  
tree.both<-rpart(Kyphosis~ .,data=data_train,method='class')
summary(tree.both)  
tree.both$variable.importance
    Variable importance
    Start 
      100 
printcp(tree.both)
    Variables actually used in tree construction:
    [1] Start

    Root node error: 7/41 = 0.17073

    n= 41 

           CP nsplit rel error xerror    xstd
    1 0.14286      0   1.00000 1.0000 0.34419
    2 0.01000      1   0.85714 1.2857 0.37862
    plotcp(tree.both,lwd=2) 

#第4步：画决策树
#4.1画决策树第1种方法，画出来的树比较简单
plot(tree.both)
text(tree.both,use.n=T,all=T,cex=0.9)

#4.2画决策树第2种方法，画出来的树稍微好看些
library(rpart.plot)
rpart.plot(tree.both,branch=1,shadow.col="gray",box.col="green",border.col="blue",
    split.col="red",split.cex=1.2,main="决策树")

#第5步：输出规则。剪枝后的决策树规则，从规则中再找规律
library(rattle)
asRules(tree.both)
     Rule number: 3 [Kyphosis=present cover=9 (22%) prob=0.56]
       Start< 8.5

     Rule number: 2 [Kyphosis=absent cover=32 (78%) prob=0.06]
       Start>=8.5

#第6步：在测试集上做预测
library(pROC)
pred.tree.both<-predict(tree.both,newdata=data_test)

#第7步：看模型的预测效果如何呀。正确的有多少，错误的有多少
predictScore<-data.frame(pred.tree.both)
rownames(predictScore) #看这个矩阵行的名字
colnames(predictScore)#看这个矩阵列的名字 "absent"  "present"
##在预测的矩阵后面多加一列kyphosis
predictScore$kyphosis <- apply(predictScore, 1, function(t) colnames(predictScore)[which.max(t)])

n<-table(data_test$Kyphosis,predictScore$kyphosis)
n #看分布情况
              absent present
      absent      26       4
      present      4       6
percantage<-c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]))
percantage
    [1] 0.8666667 0.6000000