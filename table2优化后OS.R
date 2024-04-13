#1.载入包
library(survival)
library(plyr)
library(car)
#2.清理工作环境
rm(list = ls()) 
#3.读入数据
aa<- read.csv('2.csv')
names(aa)
#4.数据转换
for (i in names(aa)[c(2:21,23,25)]){aa[,i] <- as.factor(aa[,i])}
str(aa)
#==========================一、批量单因素回归=====================================================
#1.模型的y-----------------------------------------------------------
y<- Surv(time = aa$OS,event = aa$OS.STATUS==1)

#2.批量单因素回归模型建立：Uni_cox_model
Uni_cox_model<- function(x){
          FML <- as.formula(paste0 ("y~",x))
          cox<- coxph(FML,data=aa)
          cox1<-summary(cox)
          HR <- round(cox1$coefficients[,2],2)
          PValue <- round(cox1$coefficients[,5],3)
          CI5 <-round(cox1$conf.int[,3],2)
          CI95 <-round(cox1$conf.int[,4],2)
          Uni_cox_model<- data.frame('Characteristics' =x,
                                      names <-rownames(cox1$conf.int),
                                     'HR' = HR,
                                     'CI5' = CI5,
                                     'CI95' = CI95,
                                     'Uni_P' = PValue
                                     )
          return(Uni_cox_model)}  

#---------------------3.将想要进行的单因素回归变量输入模型----------------------------------------
#3-1查看变量的名字
names(aa)
#3-2输入变量序号
variable.names<- colnames(aa)[c(2:13,18:21)] 

#---------------------4.输出结果-----------------------------------------------------------------
Uni_cox <- lapply(variable.names, Uni_cox_model)
Uni_cox<- ldply(Uni_cox,data.frame)

#=====================5.优化表格：使表格变为HR (95% CI)+P风格-------------------------------------
Uni_cox$HR.CI95<-paste0(Uni_cox$HR," (",Uni_cox$CI5,'-',Uni_cox$CI95,")")
Uni_cox<-Uni_cox[,-3:-5] #HR (95% CI)+P风格

# Uni_cox$CI<-paste(Uni_cox$CI5,'-',Uni_cox$CI95)
# Uni_cox<-Uni_cox[,-4:-6] #方法2：HR+95% CI+P 风格

#查看表格
View(Uni_cox)
write.csv(Uni_cox, file = "table2-OS未检查VIF.csv")
#查看p<0.05的变量
Uni_cox$Characteristics[Uni_cox$Uni_P<0.05]

#对p＜0.05的做线性回归看VIF值
###做线性回归，不能使用分类变量，应该用数字变量#所以这里不要转factor#
####如果转factor了，看GVIF^(1/(2*Df))###但是会有一点偏差
fit1 <- glm(OS.STATUS~ BMI.group+Naples.group+pT.group+pCR, 
            family = binomial(), 
            data = aa) 
#检测有无异常值，有则需去除
alias(fit1)
#查看vif值

vif(fit1)

####把表格中不想要的第N行删掉########
Uni_cox <- Uni_cox[-5,]
View(Uni_cox)

#####删除后再看是否有共线####
fit1 = glm(status~ Age+T+N+M+
             Systemic+Bone+Brain+Liver
           +Lung+Subtype+ER, family = binomial(), 
           data = aa) 
vif(fit1)
#=================================================================================================
#==================二、单因素中筛选p<0.05做多因素回归=============================================
#=================================================================================================
#1.提取单因素p<0.05变量
Uni_cox$Characteristics[Uni_cox$Uni_P<0.05]
#2.多因素模型建立
mul_cox_model<- as.formula(paste0 ("y~",
                                   paste0(Uni_cox$Characteristics[Uni_cox$Uni_P<0.05],
                                          collapse = "+")))
mul_cox<-coxph(mul_cox_model,data=aa)
cox4<-summary(mul_cox) 
  
#3.提取多因素回归的信息
mul_HR<- round(cox4$coefficients[,2],2) 
mul_PValue<- round(cox4$coefficients[,5],4) 
mul_CI1<-round(cox4$conf.int[,3],2)
mul_CI2<-round(cox4$conf.int[,4],2)

#4.优化，多因素结果成表
##4-1：HR(95%CI)+P风格
mul_HR.CI95<-paste(mul_HR,"(",mul_CI1,'-',mul_CI2,")")
mul_cox1<- data.frame("mul_HR.CI95"=mul_HR.CI95,"P"=mul_PValue)

##4-2：HR+95%CI+P风格
#mul_CI<-paste(mul_CI1,'-',mul_CI2)
#mul_cox1<- data.frame("HR"=mul_HR,"mul_CI"=mul_CI, "P"=mul_PValue)


#===================================================================================================
#==================  三、单因素多因素整合成一个表  =================================================
#===================================================================================================
#1.删除单因素表Uni_cox的第一列
Uni_cox<-Uni_cox[,-1]

#2.删除第一列后的单因素表Uni_cox的第一列命名为Characteristics
colnames(Uni_cox)[1] <- 'Characteristics'

#3.多因素表mul_cox1的行名放入单元格中，也命名为Characteristics
mul_cox1<-cbind(rownames(mul_cox1 ),mul_cox1,row.names=NULL);names(mul_cox1 )[1]<-"Characteristics"

#4.Uni_cox表和mul_cox1表以Characteristics列为标准合并
table2<- merge.data.frame(Uni_cox ,mul_cox1,by="Characteristics",all = T,sort = F)
#5.查看最终表格
View(table2)

## 保存为CSV 
write.csv(table2, file = "table2-OS.csv")
