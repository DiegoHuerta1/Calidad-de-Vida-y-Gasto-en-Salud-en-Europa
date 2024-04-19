library(readr)
library(tidyverse)
library(ggplot2)
library(stats)
library(MASS)
library(msm)
library(msmtools)
library(ISLR2)

# Regresion logistica para determinar si las 44 variables
# discriman a los paises de acuerdo
# al grupo que pertenecen de su esperanza de vida



# leer los datos
df <- read_csv("./datos_pca.csv")
print(names(df))

attach(df)

# codificar bien
categorias <- recode(Categoria,"Esperanza de vida baja"=0,
                  "Esperanza de vida alta"=1)



# quitar columnas no predictoras
df <- df[, !(names(df) %in% c("Country Name", "Categoria"))]
print(names(df))

# numero de columans
print(ncol(df))

# modelo de regresion logistica
modelo <- glm(categorias ~ ., data = df, family = binomial)

summary(modelo)

# solo para calculo
null_model <- glm(categorias ~ 1, data = df, family = binomial)


# ver las deviance
# Obtain null deviance
null_deviance <- deviance(null_model)

# Obtain residual deviance
residual_deviance <- deviance(modelo)

# Print the values
print(paste("Null Deviance:", null_deviance))
print(paste("Residual Deviance:", residual_deviance))

# si son las de: summary(modelo)

# hacer la resta
dif.dev<-null_deviance - residual_deviance

# Number of parameters estimated in the null model: 1 (intercept)
# Number of parameters estimated in the proposed model: 45
# Thus, the df in the chi square test is 44

pchisq(dif.dev, df=44, lower.tail = FALSE)

# el pvalue es 0.03
# hay evidencia en favor de nuestro modelo
# YEII

