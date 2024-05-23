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

