library(MASS)
library(dplyr)
library(tidyr)
library(skimr)
# Gr�ficos
# ==============================================================================
library(ggplot2)
library(ggpubr)
# Preprocesado y modelado####
# ==============================================================================
library(tree)
library(dplyr)
d <- read.csv("./train.csv ")
names(d)
str(d)
#### NOS QUEDAMOS CON LAS VARIABLES QUE NOS INTERESAN ####
# ==============================================================================
datos <-select( d,YearBuilt,TotalBsmtSF,OverallQual,GrLivArea,GarageArea,SalePrice)
attach(datos)
names(datos)
str(datos)
#VEMOS LA CORRELACI�N CON LA VARIABLE ####
# ==============================================================================
cor.test(SalePrice,YearBuilt)
cor.test(SalePrice,TotalBsmtSF)
cor.test(SalePrice,OverallQual)
cor.test(SalePrice,GrLivArea)
cor.test(SalePrice,GarageArea)
#MODELO DE REGRESION LINEAL#### 
# ==============================================================================
modeloRLineal <- lm(SalePrice ~ YearBuilt + TotalBsmtSF + OverallQual + GrLivArea +GarageArea )
summary(modeloRLineal)
##MODELO DE ARBOL DE REGRESI�NT####
# ==============================================================================
skim(datos)
# Divisi�n de los datos en train y test
# ==============================================================================
set.seed(123)
train <- sample(1:nrow(datos), size = nrow(datos)/2)
datos_train <- datos[train,]
datos_test  <- datos[-train,]
# Creaci�n y entrenamiento del modelo
# ==============================================================================
set.seed(123)
arbol_regresion <- tree::tree(
  formula = SalePrice ~ .,
  data    = datos_train,
  split   = "deviance",
  mincut  = 100,
  minsize = 200
)

summary(arbol_regresion)
# Estructura del �rbol creado
# ==============================================================================
par(mar = c(1,1,1,1))
plot(x = arbol_regresion, type = "proportional")
text(x = arbol_regresion, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")
# Pruning (const complexity pruning) por validaci�n cruzada
# ==============================================================================
# El �rbol se crece al m�ximo posible para luego aplicar el pruning
  arbol_regresion <- tree(
  formula = SalePrice ~ .,
  data    = datos_train,
  split   = "deviance",
  mincut  = 1,
  minsize = 2,
  mindev  = 0
)
# B�squeda por validaci�n cruzada
set.seed(123)
cv_arbol <- cv.tree(arbol_regresion, K = 5)

# Tama�o �ptimo encontrado
# ==============================================================================
 size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
 paste("Tama�o �ptimo encontrado:", size_optimo)
 resultados_cv <- data.frame(
   n_nodos  = cv_arbol$size,
   deviance = cv_arbol$dev,
   alpha    = cv_arbol$k
 )
 
 p1 <- ggplot(data = resultados_cv, aes(x = n_nodos, y = deviance)) +
   geom_line() + 
   geom_point() +
   geom_vline(xintercept = size_optimo, color = "red") +
   labs(title = "Error vs tama�o del �rbol") +
   theme_bw() 
 
 p2 <- ggplot(data = resultados_cv, aes(x = alpha, y = deviance)) +
   geom_line() + 
   geom_point() +
   labs(title = "Error vs penalizaci�n alpha") +
   theme_bw() 
 
 ggarrange(p1, p2)
 # Estructura del �rbol creado final
 # ==============================================================================
 arbol_final <- prune.tree(
   tree = arbol_regresion,
   best = size_optimo
 )
 
 par(mar = c(1,1,1,1))
 plot(x = arbol_final, type = "proportional")
 text(x = arbol_final, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")
 # Error de test del modelo inicial
 # ==============================================================================
 predicciones <- predict(arbol_regresion, newdata = datos_test)
 test_rmse    <- sqrt(mean((predicciones - datos_test$medv)^2))
 paste("Error de test (rmse) del �rbol inicial:", round(test_rmse,2))
 # Error de test del modelo final
 # ==============================================================================
 predicciones <- predict(arbol_final, newdata = datos_test)
 test_rmse    <- sqrt(mean((predicciones - datos_test$SalePrice)^2))
 paste("Error de test (rmse) del �rbol final:", round(test_rmse,2))


