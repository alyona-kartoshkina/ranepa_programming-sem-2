#здесь мы поработаем с классическим датасетом титаника и просто рассмотрим разные виды визуализаций

#install.packages('datasets')
#install.packages('titanic')

library(datasets)
library(titanic)

df_titanic = titanic_train
df_cars = cars

# обычный точечный график 
speed = df_cars[,1]
dist = df_cars[,2]

plot(speed,dist,xlab='speed',ylab='dist', col='black',lwd=3) #еще называют scatterplot 
title('Зависимость длины тормозного пути от скорости')
#col - цвет границы
#pch - вид символа
#cex - размер символа
#lwd - толщина границы

#давайте узнаем, какое значение скорости встречалось чаще всего и отметим его на графике
#значение, которое встречается чаще всего, называется модой. встроенной функции в R нет. напишем ее сами

moda <- function(x){
  tab = table(x) #имеет смысл для дискретных значений
  return(as.numeric(names(which.max(tab))))
}

abline(v=moda(speed),col='red')      

#давайте нанесем на график прямую, которая будет хорошо описывать зависимость

a = -17.5
b = 4

lines(speed,a+b*speed, col='blue', lw=3, lty=1)
#lty - вид линии

#поработаем немного с титаником
head(df_titanic)
names(df_titanic)
summary(df_titanic)

df_titanic = na.omit(df_titanic)
df_titanic = df_titanic[!is.na(df_titanic$Age),]




survived = df_titanic$Survived
class = df_titanic$Pclass
sex = df_titanic$Sex
age = df_titanic$Age

hist(age) #поглядим на распределение возраста - для непрерывных переменных!
barplot(table(class))
barplot(table(sex))



# какой средний возраст людей каждого пола ? 

aggregate(Age ~ Sex, data = df_titanic, mean)

# какая доля мужчин и женщин выживала  


# средний возраст выживших и погибших 


# какая доля людей каждого класса выжили


# сгруппировать долю выживших по полу и по классу



# сказка про apply, sapply, lapply

x <- matrix(c(2, 7, 1, 3, 6, 1), ncol = 2, byrow = TRUE)
y <- matrix(c(3, 7, 6, 3, 5, 9), ncol = 2, byrow = TRUE)
xy = rbind(x,y)

apply(xy,2,mean) # 1 - строка, 2 - столбец. на вход - матрица или датафрейм, на выход - массив
sapply(xy, mean)


lapply(df_cars, function(x){max(x)}) # на вход - вектор, список, датафрйем, на выход - список
sapply(df_cars, var) # на выход - вектор

