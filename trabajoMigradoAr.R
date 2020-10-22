library(dplyr)
d <- read.csv("./train.csv ")
names(d)
str(d)
#### NOS QUEDAMOS CON LAS VARIABLES QUE NOS INTERESAN ####
datos <-select( d,YearBuilt,TotalBsmtSF,OverallQual,GrLivArea,GarageArea,SalePrice)
attach(datos)
names(datos)
str(datos)
#VEMOS LA CORRELACIÓN CON LA VARIABLE ####
cor.test(SalePrice,YearBuilt)
cor.test(SalePrice,TotalBsmtSF)
cor.test(SalePrice,OverallQual)
cor.test(SalePrice,GrLivArea)
cor.test(SalePrice,GarageArea)
#MODELO DE REGRESION LINEAL#### 
modeloRLineal <- lm(SalePrice ~ YearBuilt + TotalBsmtSF + OverallQual + GrLivArea +GarageArea )
summary(modelo_multiple)
##MODELO DE RANDOM FOREST####
install.packages("randomForest")
library(caret)
library(randomForest)
set.seed(2020)
modeloRForest <- randomFores(x=datos[training.ids,1:5],
                             y= datos[training.ids,5],
                             ntree =500,
                             keep.forest=TRUE)
#problemas con random forest