########简单出图#####################
####################################
# setwd("C:\\Users\\PC\\Desktop\\相关性\\RADL_DLG3") # 设置路径，设置的路径是到桌面的，后边导出的东西在桌面上
# # 直接从CRAN中安装
# install.packages("ggplot2")
# 加载包
library(ggplot2) # 绘图
library(ggpubr) # 计算差异
# 清除环境
rm(list = ls())
# 加载鸢尾花数据
iris <- read.csv('DLG3.csv')
names(iris)
# 查看品种
table(iris$RADL_DLG3)
# 箱线图叠加散点图展示不同品种花瓣长度的差异
ggplot(iris, # 用来画图的数据集名称
       aes(RADL_DLG3, # 数据集的RADL_DLG3列作为横坐标
           DLG3))+ # 数据集的Petal.Length列作为纵坐标
  geom_jitter()+ # 添加散点图
  geom_boxplot() # 添加箱线图


############头上的灰框不好看，我们把它去掉~##############
#########################################################
# 像这种多组之间两两比较，需要提前设置一个list，里面包含的是两两比较的对象
# list = list(c("setRADL_DLG3a","versicolor"),c("virginica","versicolor"),c("setRADL_DLG3a","virginica")) 
list = list(c("low","high")) 

# 把数据转成纵性数据
df=reshape2::melt(iris, # 需要转换的数据
                  id.vars=17, # 第5列RADL_DLG3不动,这里是横坐标，用来分组的特征
                  measure.vars=4) # 第1列到第4列全部转换成纵性数据

ggplot(df, # 用来画图的数据集名称
       aes(RADL_DLG3, # 数据集的RADL_DLG3列作为横坐标
           value,  # 数据集的value列作为纵坐标
           color = RADL_DLG3, # 根据数据集的RADL_DLG3列设置箱线图的边框颜色
           fill = RADL_DLG3))+ # 根据数据集的RADL_DLG3列设置散点图和箱线图的填充颜色
  geom_jitter(width=.15)+ # 将散点图的宽度设置为0.15,缩窄一些
  geom_line(size = 1.5) + # Adjust the size parameter for boldness"
  geom_boxplot(width=.4, alpha=.2)+ # 将箱线图的宽度设置为0.4，缩窄了一些，不透明度设置为0.2，填充颜色变得很浅
  scale_color_manual(values = c("#BF5960","#6F99AD","#F9A363"))+ # 自定义边框颜色
  scale_fill_manual(values = c("#BF5960","#6F99AD","#F9A363"))+ # 自定义填充颜色，可以和边框颜色不一样，我觉得配套的比较好看，就用了一样的颜色
  theme_test()+ # 把背景颜色去掉并添加边框，可以修改为其他背景，比如theme_classic
  labs(y="IHC Score", x="")+ # 修改横纵坐标名称
  stat_compare_means(comparisons = list, method = "t.test", #像这种非配对的非正态分布的数据，可以用非参数检验方法，这里选的是非参的其中一种，叫秩和检验
                     size=4 # 将p值字体改小一些
                     #label = "p.signif")+ # 把p值改为星号
                     )+ # 把p值改为星号
  scale_y_continuous(expand = c(0.1,0.1))+ # 将y轴上下拉宽一点，这样被遮住的p值就可以呈现出来啦~
  facet_wrap(.~variable, # 同时做几个图，这几个图分别对应数据转换前的第1列到第4列
             nrow=4, ncol=5, # 图排成两行两列
             scale="free")+ # 横纵坐标不要统一成一样的，各个图根据自己情况来自动设置横纵坐标的范围
  theme(strip.background = element_blank(), #把头上的灰框去掉
        axis.text = element_text(color="black", size=10),# 加入 face = "bold" 使文字加粗
        axis.line = element_blank(),# 把横纵坐标的字体颜色改为黑色，看起来更沉稳
        panel.border = element_rect(color = "black", size = 2))  # Customize the plot border
ggsave("RADL_DLG3.pdf", width = 5, height = 6) # 导出pdf


# 以下是一些可用的method参数选项：
# 
# "t.test"：进行独立样本的t检验。
# "wilcox.test"：进行非参数的Wilcoxon符号秩检验。
# "kruskal.test"：进行Kruskal-Wallis检验。
# "friedman.test"：进行Friedman检验。
# "jtt"：进行Jonckheere-Terpstra检验。
# "brunner.langer"：进行Brunner-Langer检验。
# "van.elteren"：进行Van Elteren测试。
# "van.der.waerden"：进行Van der Waerden测试。