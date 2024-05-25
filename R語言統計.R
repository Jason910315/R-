#***基本統計學繪圖***----
install.packages("readxl")
install.packages("data.table")
install.packages("ggplot2")
library(ggplot2)
library(data.table)
library(readxl)
data(mpg)
##1. 直方圖hist() ----
#xlab為X軸的 label標題，與圖中數據無關
hist(mpg$cty,main = "Hist",xlab = "cty")

##2. 散佈圖plot() ----
plot(y = mpg$displ,x = mpg$hwy)

##3. 箱型圖boxplot() ----
boxplot(mpg$cty)

##4. 枝葉圖stem() ----
stem(mpg$displ)

#***ggplot套件***----
#公式:ggplot() + geom_套件

##1. 機率密度圖geom_density ----
#fill可填滿曲線下顏色
ggplot(data = mpg) + geom_density(aes(x = displ),fill = "grey50")

##2. 長條圖geom_bar() ----
ggplot(data = mpg) + geom_bar(aes(x = class))

##3. 散佈圖geom_point() ----
ggplot(data = mpg) + geom_point(aes(x = hwy,y = displ))

##4. 平滑曲線geom_smooth() ----
ggplot(data = mpg) + geom_smooth(aes(x = hwy,y = displ))

##5. 合併使用----
ggplot(data = mpg,aes(x = hwy,y = displ)) + geom_smooth() + geom_point()

##6. 線性圖geom_line() ----
data("economics")
ggplot(data = economics,aes(x = date, y = pop)) + geom_line()

#***機率分布***----
## normal distribution ----
#從常態分布中抽取隨機變數(預設為mean = 0,sd = 2，以下含是預設皆為此)
#rnorm，從常態分佈中抽取隨機變數
rnorm(n = 10)
#從平均為10，標準差為2的常態分佈中抽取10個隨機變數
rnorm(n = 10,mean = 10,sd =2)

#dnorm，常態分布的p.d.f(機率密度函數)值，函數計算常態分佈為 x 時的機率密度
dnorm(3,mean = 10,sd = 2)

#pnorm，常態分布的c.d.f(累積機率密度)，給定z值算機率值(表準常態分配使用Z值)
pnorm(c(-1.96,0,1.96))
pnorm(0.5) - pnorm(-0.5)
dnorm(0)
#上述兩者值會相似

#qnorm，給定機率值算x值
qnorm(0.975,mean = 0,sd = 1)  #機率0.975，z值約為1.96

###畫常態分佈圖 ----
randNorm <- rnorm(30000)   #先生成一個有30000筆資料的常態x變數，當x軸
randDensity <- dnorm(randNorm)   #算其機率密度，y軸
library(ggplot2)
#畫pdf
ggplot(data.frame(x = randNorm,y = randDensity)) + aes(x = x,y = y) + 
  geom_point() + labs(x = "Random Normal Variables",y = "Probability")

#畫cdf，累積上去的
randProb <- pnorm(randNorm)   #先計算常態分布的cdf值
ggplot(data.frame(x = randNorm,y = randProb)) + aes(x = x,y = y) + 
  geom_point() + labs(x = "Random Normal Variables",y = "Probability")

##2. binomial distribution ----

###機率 ----
#f(x = 3)代表二項實驗中3次成功的機率
#size：實驗次數(例如：投擲一個銅板次)；
#prob：每次實驗的成功機率(例如：銅板出現正面的機率)。

#隨機生成符合binomial的5個數字，size 即是npq裡的n(實驗次數),p = 0.3
rbinom(n = 5,size = 10,prob = 0.3 )   #5個隨機數字代表x，即成功次數，注意結果仍只有0、1兩種   
rbinom(n = 5,size = 1,prob = 0.3 ) #size為1即為柏努利分配，事件只能成功(1)或失敗(0)

#pdf，10次試驗中"三次成功"機率
dbinom(x = 3,size = 10,prob =0.3)

#cdf，10次試驗中"<=三次成功"機率，下述三者答案一樣
pbinom(q = 3,size = 10,prob = 0.3)
dbinom(x = 0,size = 10,prob = 0.3) + dbinom(x = 1,size = 10,prob = 0.3) + dbinom(x = 2,size = 10,prob = 0.3) + dbinom(x = 3,size = 10,prob = 0.3)
sum(dbinom(x = 0:3,size = 10,prob = 0.3))

#由機率值求出隨機變數
#當每次成功之機率為0.3且試驗10次，則10次內成功幾次的機率為0.4?
qbinom(p = 0.4,size = 10,prob = 0.3)

###畫pdf圖 ----
binomdata <- data.frame(Success = rbinom(n = 30000,size = 10,prob = 0.3))
ggplot(binomdata,aes(x = Success)) + geom_histogram(binwidth = 1)

#***基本統計分析***----

iris_df <- data.frame(iris)

#各項描述性統計
mean(iris_df$Sepal.Length,na.rm = TRUE)
median(iris_df$Sepal.Length)
sd(iris_df$Sepal.Length)
sum(iris_df$Sepal.Length)
quantile(iris_df$Sepal.Length,prod = c(0.25,0.75)) #百分位數
summary(iris_df$Sepal.Length) #綜合呈現
length(iris_df$Sepal.Length) #樣本數
table(iris_df$Sepal.Length) #次數分類表(各值出現次數)

##1. 常態分配檢定 ----
###1-1. 作檢定 ----
#p-value = 0.01 < 0.05即拒絕H0假設檢定，此不為常態分佈
shapiro.test(iris_df$Sepal.Length)  
shapiro.test(iris_df$Sepal.Width) 
#自行創建一個常態分佈並作檢定
norm <- rnorm(n = 1000)
shapiro.test(norm)  #p-value = 0.4885 > 0.05，不拒絕h0，代表其為常態分佈
ggplot(data.frame(x=norm), aes(norm)) + geom_histogram

#畫一下 QQ 圖（quantile-quantile plot），這種圖形可用來比較資料
#與常態分佈之間的差異，圖中那一條直線是理論值，如果是完美的常態分佈
#，所有的點就都會落在直線上。
ggplot(data.frame(x=iris_df$Sepal.Width), aes(sample=iris_df$Sepal.Width)) + stat_qq() + stat_qq_line()

###1-2. 檢測分組資料 ----
#aggregate(連續型變數 ~類別變數,資料名稱,統計變數)
aggregate(Sepal.Length ~Species,iris_df,mean)
aggregate(cbind(Sepal.Length,Sepal.Width) ~Species,iris_df,mean)

##2. 單一樣本t檢定 ----
#用於比較樣本資料與一個特定值之間是否有統計學差異
###雙尾檢定 ----
# H0:μ = 3。建材長度等於3cm
# H1:μ != 3。建材長度不等於3cm

t.test(iris_df$Sepal.Width,alternative = "two.sided",mu = 3) #雙尾檢定
#output:
#t = 1.611, df = 149, p-value = 0.1093  (p>0.05無法拒絕虛無假設h0)
#alternative hypothesis: true mean is not equal to 3 (對立假設為=3)
#95 percent confidence interval:
  #2.987010 3.127656  (95%信賴區間為)
#sample estimates:
  #mean of x 
#3.057333 (故此檢定結果為不拒絕h0，建材長度 = 3)

###單尾檢定 ----
#右尾檢定:greater、左尾檢定:less，以下是右尾
# H0:μ <= 3
# H1:μ > 3
t.test(iris_df$Sepal.Width,alternative = "greater",mu = 3) #左尾檢定

##3. 成對樣本t檢定 ----
#用於檢定成對樣本的平均值是否有統計學差異
#成對樣本是只同一組人物或物件在不同時間或情況下的資料差異
install.packages("UsingR")  #使用UsingR套件裡的father.son資料
library(UsingR)
data(father.son)
head(father.son)
#同樣使用t.tset，但paired = T 做成對樣本檢定
t.test(father.son$fheight,father.son$sheight,paired = T)

##4. 兩獨立樣本t檢定 ----
#兩個群體是獨立的，即一個群體的觀察值與另一個群體的觀察值無關聯

###4-1. 變異數相等 ----
#算計算兩組獨立樣本的平均數與標準差(取Species裡的其中兩組分類且取Width變數檢定)
library(dplyr)
iris_df%>%filter(grepl('s',Species))%>%group_by(Species)%>% summarise(size=n(),avg_length=mean(Sepal.Width),sd_length=sd(Sepal.Width))
#將Species的兩個類別的Sepal.Width設為物件
x1<-iris_df$Sepal.Width[iris_df$Species=="setosa"]
x2<-iris_df$Sepal.Width[iris_df$Species=="versicolor"]

shapiro.test(x1)
shapiro.test(x2)

iris_df%>%filter(grepl('s',Species))%>%ggplot(aes(x=Sepal.Width,fill=Species))+geom_histogram()

#檢定變異數var.test()，檢定母體變異數是否相等要用F檢定
var.test(iris_df$Sepal.Width[iris_df$Species=="setosa"],iris_df$Sepal.Width[iris_df$Species=="versicolor"],alternative = "two.sided")

#後續對兩獨立樣本作檢定，第一種方法(var.equal = T，變異數相等)
t.test(x1,x2, alternative = "two.sided", var.equal = T)

###4-2. 變異數不相等 ----
#先留下我們想要的資料
iris_df_2sample<- iris_df %>% filter(Species=="setosa" | Species=="versicolor") 
iris_df%>%filter(grepl('s',Species))%>%group_by(Species)%>% summarise(size=n(),avg_length=mean(Petal.Width),sd_length=sd(Petal.Width))

x1<-iris_df$Petal.Width[iris_df$Species=="setosa"]
x2<-iris_df$Petal.Width[iris_df$Species=="versicolor"]

shapiro.test(x1)
shapiro.test(x2)

#畫直方圖看看
iris_df_2sample %>% ggplot(aes(x=Petal.Width, fill=Species))+geom_histogram()

#不符合常態但符合大樣本n>=30
#所以可以用var.test()檢定變異數
var.test(Petal.Width~Species,iris_df_2sample,alternative = "two.sided" )
#此法結論變異數不相等(p-value<0.05拒絕h0假設，兩者變異數不相等)

#進行兩獨立樣本t檢定
t.test(Petal.Width~Species, data=iris_df_2sample, alternative = "two.sided", var.equal = T)

##5. 卡方檢定 ----
install.packages("MASS")
library("MASS")
#使用Cars93套件
data(Cars93)
library(dplyr)

#取出Compact、Midsize兩個類別並創造新變量Group
Cars93_sub<-Cars93 %>% filter(Type=="Compact" | Type=="Midsize") %>% mutate(Group = ifelse(Type== "Compact","Compact" ,"Midsize"))

#看air bag此變量是否跟type=compact, midsize是否有所不同
#先做成table列聯表格式
car93_t<-table(Cars93_sub$AirBags, Cars93_sub$Group)
class(car93_t)

###行列百分比 ----
#prop.table()列出兩變量的行列百分比關係，值為機率
prop.table(table(Cars93_sub$AirBags, Cars93_sub$Group))


