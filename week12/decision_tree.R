##根据训练集合生成决策树
##输入：trainData 训练集，类型为数据框
##      strRoot 指明根节点的属性名称
##      strRootAttri 指明根节点的属性取值
##      nClass 指明训练集中第nClass列为分类结果
##      cAttri 向量，表示当前可用的特征集合，用列号表示
##      e 如果特征的较大信息增益小于e，则剩余作为一个分类，类频数较高的更为分类结果
##输出：决策树T
gen_decision_tree <- function(trainData, strRoot, strRootAttri, nClass, cAttri, e){
   # 树的描述，（上级节点名称、上级节点属性值、自己节点名称，自己节点的取值）
   decision_tree <- data.frame()
   nClass.freq <- count(trainData,nClass)   ##类别出现的频数
   nClass.freq <- arrange(nClass.freq, desc(freq))  ##按频数从低到高排列
   col.name <- names(trainData) ##trainData的列名
   ##1、如果D中所有属于同一类Ck，则T为单节点树
   if nrow(nClass.freq) == 1{
      rbind(decision_tree, c(strRoot, strRootAttri, nClass.freq[1,1], ''))
      return decision_tree
   }
   ##2、如果属性cAttri为空，将D中频数较高的类别返回
   if length(cAttri) == 0{
      rbind(decision_tree, c(strRoot, strRootAttri, nClass.freq[1,1], ''))
      return decision_tree
   }
   ##3、计算cAttri中各特征值对D的信息增益，选择信息增益较大的特征值Ag及其信息增益
   maxDA <- 0    #记录较大的信息增益
   maxAttriName <- ''   #记录较大信息增益对应的属性名称
   maxAttriIndex <- ''   #记录较大信息增益对应的属性列号
   for(i in cAttri){
      curDA <- g_DA(trainData,nClass,i)
      if (maxDA <= curDA){
         maxDA <- curDA
         maxAttriName <- col.name
      }
   }
   ##4、如果较大信息增益小于阈值e，将D中频数较高的类别返回
   if (maxDA < e){
      rbind(decision_tree, c(strRoot, strRootAttri, nClass.freq[1,1], ''))
      return decision_tree
   }
   ##5、否则，对Ag的每一可能值ai，依Ag=ai将D分割为若干非空子集Di
   ##   将Di中实例数较大的类作为标记，构建子节点
   ##   由节点及其子节点构成树T，返回T
   for (oneValue in unique(trainData[,maxAttriName])){
      sub.train <- trainData[which(trainData[,maxAttriName] == oneValue),]  #Di
      #sub.trian.freq <- count(sub.train,nClass)   ##类别出现的频数
      #sub.trian.freq <- arrange(sub.trian.freq, desc(freq))  ##按频数从低到高排列
      rbind(decision_tree, c(strRoot, strRootAttri, maxAttriName , oneValue))
      ##6、递归构建下一步
      # 剔除已经使用的属性
      next.cAttri <- cAttri[which(cAttri !=maxAttriIndex)]
      # 递归调用
      next.dt <-gen_decision_tree(sub.train, maxAttriName,
      oneValue, nClass, next.cAttri, e)
      rbind(decision_tree, next.dt)
   }
   names(decision_tree) <- c('preName','preValue','curName','curValue')
   decision_tree
}