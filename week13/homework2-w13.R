#8.4 应聘者数据 对变量分类
setwd('R:/dataguru/RDataAna/week13')
X<-read.table("applicant.data")
#相关系数作为相似系数
Y<-cor(X)
#距离矩阵
d<-as.dist(1-Y)
#聚类分析
hc1<-hclust(d) #最长距离法
hc2<-hclust(d,"average") #均值法
hc3<-hclust(d,"centroid") #重心法
hc4<-hclust(d,"ward") #Ward法

opar<-par(mfrow=c(2,2))
plot(hc1,hang=-1)
re1<-rect.hclust(hc1,k=5,border="green")
plot(hc2,hang=-1)
re2<-rect.hclust(hc2,k=5,border="green")
plot(hc3,hang=-1)
re3<-rect.hclust(hc3,k=5,border="green")
plot(hc4,hang=-1)
re4<-rect.hclust(hc4,k=5,border="green")
par(opar)

以ward法为例，变量的分组如下：

G1：AA（专业能力）
G2：APP（外貌）
G3：FL（求职信的形式) + EXP(经验) + SUIT（适应性）
G4: HON（诚实） +LA（讨人喜欢） + KJ(交际能力)
G5: POT（潜在能力） + LC（洞察力） +GSP（理解能力） + DRV（驾驶水平） + SC（自信心） + SMS（推销能力） +AMB（事业心）