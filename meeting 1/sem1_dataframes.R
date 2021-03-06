#����� �� ���������� � ������������ ��������� �������� � ������ ���������� ������ ���� ������������

#install.packages('datasets')
#install.packages('titanic')

library(datasets)
library(titanic)

df_titanic = titanic_train
df_cars = cars

# ������� �������� ������ 
speed = df_cars[,1]
dist = df_cars[,2]

plot(speed,dist,xlab='speed',ylab='dist', col='black',lwd=3) #��� �������� scatterplot 
title('����������� ����� ���������� ���� �� ��������')
#col - ���� �������
#pch - ��� �������
#cex - ������ �������
#lwd - ������� �������

#������� ������, ����� �������� �������� ����������� ���� ����� � ������� ��� �� �������
#��������, ������� ����������� ���� �����, ���������� �����. ���������� ������� � R ���. ������� �� ����

moda <- function(x){
  tab = table(x) #����� ����� ��� ���������� ��������
  return(as.numeric(names(which.max(tab))))
}

abline(v=moda(speed),col='red')      

#������� ������� �� ������ ������, ������� ����� ������ ��������� �����������

a = -17.5
b = 4

lines(speed,a+b*speed, col='blue', lw=3, lty=1)
#lty - ��� �����

#���������� ������� � ���������
head(df_titanic)
names(df_titanic)
summary(df_titanic)

df_titanic = na.omit(df_titanic)
df_titanic = df_titanic[!is.na(df_titanic$Age),]




survived = df_titanic$Survived
class = df_titanic$Pclass
sex = df_titanic$Sex
age = df_titanic$Age

hist(age) #�������� �� ������������� �������� - ��� ����������� ����������!
barplot(table(class))
barplot(table(sex))



# ����� ������� ������� ����� ������� ���� ? 

aggregate(Age ~ Sex, data = df_titanic, mean)

# ����� ���� ������ � ������ ��������  


# ������� ������� �������� � �������� 


# ����� ���� ����� ������� ������ ������


# ������������� ���� �������� �� ���� � �� ������



# ������ ��� apply, sapply, lapply

x <- matrix(c(2, 7, 1, 3, 6, 1), ncol = 2, byrow = TRUE)
y <- matrix(c(3, 7, 6, 3, 5, 9), ncol = 2, byrow = TRUE)
xy = rbind(x,y)

apply(xy,2,mean) # 1 - ������, 2 - �������. �� ���� - ������� ��� ���������, �� ����� - ������
sapply(xy, mean)


lapply(df_cars, function(x){max(x)}) # �� ���� - ������, ������, ���������, �� ����� - ������
sapply(df_cars, var) # �� ����� - ������

