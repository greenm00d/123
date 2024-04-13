rm(list = ls()) #清理环境
aa<- read.csv('clinical_all.csv')#数据名
#将数据转换成因子变量
names(aa)
for (i in names(aa)[c(4,6:11,13,14,16:24)]){aa[,i] <- as.factor(aa[,i])}#括号里是分类变量
str(aa)


###########################################################################################
##############################构建三线表（table 1）########################################
###########################################################################################
#library(table1)
#aa$ER分组<-factor(aa$ER分组)
#table1(~术前NLR分组+术前NLR+术前LMR分组+术前LMR+术前SIRI分组+术前SIRI
#       |ER分组,data=aa,overall="Total", topclass="Rtable1-zebra")

###########################################################################################
##############################构建三线表（tableone）########################################
###########################################################################################
library(tableone)
names(aa)
#1.指定分析的变量，不包括Y
myVars <- c( "AGE",  "AGE.GROUP",     "T" ,        "N"  ,       "ER" ,      
             "PR" ,       "HER2" ,     "KI67"  ,     "pCR",       "DLG3GROUP",
             "RADL_DFS",  "Patho_DFS", "RADL_OS",   "Patho_OS","RADL_pCR",
             "Patho_pCR","RADL_DLG3",  "Patho_DLG3")
#2.指定分类变量,也不包含Y
catVars <- c("AGE.GROUP", "T" ,        "N"  ,       "ER" ,      
              "PR" ,       "HER2" ,     "KI67"  ,     "pCR",       "DLG3GROUP",
             "RADL_DFS",  "Patho_DFS", "RADL_OS",   "Patho_OS","RADL_pCR",
             "Patho_pCR","RADL_DLG3",  "Patho_DLG3")
#3.检测连续变量是否为正态性分布
#批量的检测
#apply(aa[,c(1,13)],2,shapiro.test)
##单个的检测方法1
# apply(aa[1],2,shapiro.test)
##单个的检测方法2
shapiro.test(aa$AGE)#p>0.05符合正态分布

#4.指定非正态分布变量
# nonvar <- c("Age")

#5.期望值<5需要Fisher精确检验的变量
exactvars <- c("Position","Manopause"," BMI.group"," cT.group","HER2.group","NLR.group","LMR.group","ALB.group","TC.group","pT.group","pCR")

#6.制表
tab1<- CreateTableOne(vars = myVars, 
                      strata = "group",#分层 你的那不勒斯
                      data = aa, 
                      factorVars = catVars, 
                      addOverall = TRUE)
tab1 

#修改分类变量百分比位数为1位,连续变量小数位数为2位,调整P值小数位数为3位
tab4Mat <- print(tab1, 
                 #nonnormal = nonvar,#非正态分布用Mann-Whitney U 检验
                 # exact = exactvars, #Fisher精确检验
                 catDigits = 1,
                 contDigits = 2,
                 pDigits = 3, 
                 showAllLevels=TRUE,  
                 quote = FALSE, # 不显示引号
                 noSpaces = TRUE, # 删除用于在R控制台中对齐文本的空格
                 printToggle = F) # 展示输出的结果，F为不展示

## 保存为 CSV 格式文件，并命名为...
write.csv(tab4Mat, file = "table11.csv")







