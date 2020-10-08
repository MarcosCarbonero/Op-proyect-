library(datasets)
?iris
df<-iris 
head(df)
df
hist(df$Petal.Width,main =" European University - Histogram of Petal with ",
     xlab = "Petal width (in cm)")
rm(list = ls())
detach("package:datasets",unload = TRUE )
graphics.off()
cat("\014")
