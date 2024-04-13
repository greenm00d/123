#------------------------------------------------------------------------------
#载入R包和数据
#------------------------------------------------------------------------------
library(plyr)
library(car)
#1.清理工作环境
rm(list = ls()) 
#2.数据放入工作目录
aa<- read.csv('2.csv')
#3.查看变量性质
str(aa)
names(aa)
#4.分类变量一定要变为分类形式(factor)
for (i in names(aa)[c(2:21,23,25)]){aa[,i] <- as.factor(aa[,i])}
#5.再次查看变量性质
str(aa)
#6.查看变量名
names(aa)

#------------------------------------------------------------------------------
#1. 批量单因素单因素逻辑回归
#------------------------------------------------------------------------------
Uni_glm_model<- function(x){
  FML<-as.formula(paste0 ("pCR==1~",x))#结局变量
  glm1<-glm(FML,data=aa,family = binomial(link ="logit"))
  glm2<-summary(glm1)
  OR<-round(exp(coef(glm1)),2)
  SE<-glm2$coefficients[,2]
  CI5<-round(exp(coef(glm1)-1.96*SE),2)
  CI95<-round(exp(coef(glm1)+1.96*SE),2)
  CI<-paste0(CI5,'-',CI95)
  P<-round(glm2$coefficients[,4],3)
  Uni_glm_model <- data.frame('Characteristics'=x,
                              names <-rownames(glm2$coefficients),
                              'OR' = OR,
                              'CI' = CI,
                              'P' = P)[-1,]
  return(Uni_glm_model)}  
###选择分析的变量#################
names(aa)#查看变量名
variable.names<- colnames(aa)[c(2:20)];variable.names

###输出结果###########
Uni_glm <- lapply(variable.names, Uni_glm_model)
res_uni<- ldply(Uni_glm,data.frame);res_uni

#=====================5.优化表格：使表格变为HR (95% CI)+P风格-------------------------------------
res_uni$OR.CI95<-paste0(res_uni$OR," (",res_uni$CI,")")
res_uni<-res_uni[,-3:-4] #HR (95% CI)+P风格

#p四舍五入为0的改为<0.001
res_uni$P[res_uni$P==0]<-"<0.001"
View(res_uni)#查看单因素回归结果

write.csv(res_uni, file = "table2-test未检查VIF1.csv")

###########查看p<0.05的变量
res_uni$Characteristics[res_uni$P<0.05]
#对p＜0.05的做线性回归看VIF值
###做线性回归，不能使用分类变量，应该用数字变量#所以这里不要转factor#
####如果转factor了，看GVIF###但是会有一点偏差
fit1 = glm(M~ Age+Grade+T+N+Chemotherapy+Subtype+ER+PR+HER2
           , family = binomial(), 
           data = aa) 
vif(fit1)
####把表格中不想要的第N行删掉########
res_uni <- res_uni[-11,]
View(res_uni)
#####删除后再看是否有共线####
fit1 = glm(M~ Age+Grade+T+N+Chemotherapy++ER+PR+HER2
           , family = binomial(), 
           data = aa)  
vif(fit1)


#-------------------------------------------------------------------------------
#2. 多因素回归表
#-------------------------------------------------------------------------------
#1.提取单因素p<0.05变量
res_uni$Characteristics[res_uni$P<0.05]
#2.将其纳入多因素模型
mul_glm_model<- as.formula(paste0("M==1~",
                                  paste0(res_uni$Characteristics[res_uni$P<0.05],
                                         collapse = "+")))
mul_glm<-glm(mul_glm_model,
             family=binomial(link="logit"),
             data=aa)
glm3<-summary(mul_glm) 
#3.提取多因素回归的结果
mul_OR<-round(exp(glm3$coefficients[,1]),2)
mul_SE<-glm3$coefficients[,2]
mul_CI1<-round(exp(glm3$coefficients[,1]-1.96*mul_SE),2)
mul_CI2<-round(exp(glm3$coefficients[,1]+1.96*mul_SE),2)
mul_CI<-paste0(mul_CI1,'-',mul_CI2)
mul_P<-round(glm3$coefficients[,4],3)
#4.将提取的结果整合成表
res_mul<- data.frame("OR2"=mul_OR,"CI2"=mul_CI, "P2"=mul_P)[-1,]
res_mul

#改变OR风格
res_mul$OR2.CI95<-paste0(res_mul$OR2," (",res_mul$CI2,")")
res_mul<-res_mul[,-1:-2] #HR (95% CI)+P风格

#P值四舍五入为0的改为<0.001
res_mul$P2[res_mul$P2==0]<-"<0.001"#有=0的就改
res_mul
View(res_mul)

#-------------------------------------------------------------------------------
#3、两表合一
#-------------------------------------------------------------------------------
#1.删除单因素表的第一列，因为它无关紧要（见单因素的结果图）
res_uni<-res_uni[,-1]
#2.新的第一列命名为Characteristics
colnames(res_uni)[1] <- 'Characteristics'
res_uni
#3.多因素表的行名放入单元格中，也命名为Characteristics
res_mul<-cbind(rownames(res_mul),
               res_mul,row.names=NULL)
names(res_mul)[1]<-"Characteristics"
res_mul
#4.二表表以Characteristics列为标准进行合并
table2<- merge.data.frame(res_uni,res_mul,
                          by="Characteristics",
                          all = T,
                          sort = T)
#z注：merge.data.frame()函数使用前提是两表有相同列名，然后按该列的字段合并
#，正符合单因素与多因素表合并的需求。

#5.查看最终表格
View(table2)
## 保存为CSV 
write.csv(table2, file = "table2-train1.csv")

