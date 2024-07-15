#install.packages("tidyverse")

rm(list = ls())

library(datasets)
data(iris)

tapply(iris$Sepal.Length,iris$Species,mean)
tapply(iris$Sepal.Length,iris$Species=="virginica",mean)
#ans = 6.588
#ans = apply(iris[, 1:4], 2, mean)
#ans = with(mtcars, tapply(mpg, cyl, mean))
#       sapply(split(mtcars$mpg, mtcars$cyl), mean)
#       tapply(mtcars$mpg, mtcars$cyl, mean)
#ans = 126.5779


iris_dt <- as.data.table(iris)
iris_dt[Species == "virginica",round(mean(Sepal.Length)) ]


tapply(iris$Sepal.Length, iris$Species, mean)

apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)

with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
hp<-tapply(mtcars$hp, mtcars$cyl, mean)
mtcars_dt <-as.data.frame( hp )
mtcars_dt$hp[1]-mtcars_dt$hp[3]

library(dplyr) 

data <- mtcars %>% 
  dplyr::select(cyl, hp) %>% 
  group_by(cyl) %>% 
  mutate(avg_mpg = mean(hp))

data = select(data,-c("hp"))

data = data %>% distinct(cyl,avg_mpg)


data[data$cyl==4,"avg_mpg"] - data[data$cyl==8,"avg_mpg"]









meanData = mtcars %>% mutate(
  cylll=mtcars$cyl
)

mtcars_Means <-colMeans(mtcars,group = cyl)


tapply(mtcars$hp, mtcars$cyl, mean)

 mtcars[,  .(mean_cols = mean(mtcars$hp)), by = mtcars$cyl]
 
 
rm(mtcars_dt)

round(abs(mtcars[cyl == 4, mean_cols] - mtcars[cyl == 8, mean_cols]))