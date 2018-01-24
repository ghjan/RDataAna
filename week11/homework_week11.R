#网格方法
setwd('R:/dataguru/RDataAna/week11')
##install.packages('rJava')
library(rJava)
source('MINE.r')
##小心 对于数据集很大的话 很容易造成R崩溃
MINE("WHO.csv","all.pairs")

--------------
**如果没相应的数据或过程去掉R命令的注释即可**

1.安装Java（步骤略）

2.修改**MINE.r**
原*.jinit(classpath="MINE.jar")*
修改为
*.jinit(classpath="MINE.jar", parameters="-Xms2048m")*


3.下载数据


```r
# download.file('http://www.exploredata.net/ftp/WHO.csv', 'WHO.csv')
```