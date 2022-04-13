#install.packages(c("readxl","tidyverse","caret","glm"))
#install.packages("broom")
#install.packages("RTools")
#install.packages("glmnet")
#install.packages("lars")
#install.packages("stargazer")
#install.packages("dismo")
#install.packages("gbm")
#install.packages("lmtest")
#install.packages("plm")


library(caret)
library(tidyverse)
library(broom)
library(readxl)
library(glmnet)
library(magrittr)
library(stargazer)
library(gbm)
library(dplyr)
library(lmtest)
library(plm)
library(gglasso)


#Data Cleaning

data <- readxl::read_xlsx("base_tcc.xlsx")


colunas_fator <- c("ano", "combust", "modelo", "marca",
                   "marca_trad","gas","ar","dir","vid","trv","kit conf",
                   "aut","abs","bag")

data[colunas_fator] <- lapply(data[colunas_fator], factor)



data <- data %>% mutate(tipo_modelo = case_when(POP == 1 ~    1,
                                                COMPAC == 1 ~ 2,
                                                HATCH == 1 ~  3,
                                                SEDPOP == 1 ~ 4,
                                                SEDMED == 1 ~ 5,
                                                SEDLUXO == 1 ~ 6,
                                                MINIVAN == 1 ~ 7,
                                                PERUA == 1 ~ 8
                                                  ))
data <- data %>% dplyr::select(-ano09, -ano10, -ano11, -ano12, -POP, -COMPAC ,
                               -HATCH, -SEDPOP, -SEDMED, -SEDLUXO, -MINIVAN, -PERUA)

data <- data %>% mutate(comp = comp * 1000)




modelos <- data.frame(modelo = levels(data$modelo), id_modelo = c(1:length(levels(data$modelo))))

combust <- data.frame(combust = levels(data$combust), id_combust = c(1:length(levels(data$combust))))
  
marca <- data.frame(marca = levels (data$marca), id_marca = c(1:length(levels(data$marca))))

data <- data %>% mutate(preco = log(preco)) %>% rename(logpreco = preco)


#converter modelo para numeros


data <- data %>% left_join(., modelos, by = "modelo") %>% select(-modelo) %>% 
  mutate(id_modelo = as.factor(id_modelo))

data <- data %>% left_join(., combust, by = "combust") %>% select(-combust) %>%
  mutate(id_combust = as.factor(id_combust))

data <- data %>% left_join(., marca, by = "marca") %>% select(-marca) %>%
  mutate(id_marca = as.factor(id_marca))


data <- data %>% mutate(
  id_modelo = as.factor(id_modelo),
  id_combust = as.factor(id_combust),
  id_marca = as.factor(id_marca),
  tipo_modelo = as.factor(tipo_modelo)
)


qntf <- function(a) {
  return(length(levels(a)))
}


group <- map2(data, 1:ncol(data), function(.x, .y) {
  if(is.null(levels(.x))){
    c(rep(.y, 1))
  }
  else(
    c(rep(.y,length(levels(.x))))[-1]
  ) 
}) %>% unlist(use.names= F)


set.seed(123)


##


train.x <- sample_frac(data, 0.8)

train_index <- as.numeric(rownames(train.x))

test.x <- data[-train_index, ] %>% select(-quant) %>% as.matrix()

train.x %<>% select(-quant) 


train.y <- train %>% select(quant)

test.y <- data %>% select(quant) %>% setdiff(train.y) %>% data.matrix(.)




#covariaveis



X_int <- model.matrix(~ (.)^2 -1 , data = train.x)

X <- model.matrix(~ .-1 , data = train.x)

share_formula <- formula(share ~ . - unidades_est - total_uni - 1)

model.matrix(~id_modelo - 1,train.x)


colbind(model)

#outcome
y <- train.y %>% scale(center = T, scale = F)

#Y <- data %>% dplyr::select(quant)

#10 fold cross-validation

lasso_cv <- cv.glmnet(X, y, alpha = 1,
                      standardize = T, nfolds = 10)


lasso_cv_int <- cv.glmnet(X_int, y, alpha = 1,
                      standardize = T, nfolds = 10)

best_lamb <- lasso_cv_int$lambda.min

plot(lasso_cv)


#modelo lasso
#como modelar: https://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome

modelo <- glmnet(X, y, alpha = 1)

modelo_cv <- glmnet(data.matrix(train.x), y, alpha = 1, lambda = lasso_cv$lambda.min)

modelo_cv_int <- glmnet(as.matrix(X_int), y, alpha = 1, lambda = lasso_cv_int$lambda.min)


plot(modelo, xvar = "norm", label = T)

plot(modelo, xvar = "lambda", label = T)


predictions_grp <- predict(object = modelo_grupo, newx = test.x, type = "coef")

predictions <- predict(modelo, newx = test.x, s = lasso_cv$lambda.min)

predictions_cv <- predict(object = modelo_cv, newx = test.x, s = lasso_cv$lambda.min)

predictions_cv_int <- predict(object = modelo_cv_int, newx = test.x, s = lasso_cv_int$lambda.min)



modelo_grupo <- gglasso(X, y, group = group, loss="ls")

cvgrupo <- cv.gglasso(X, y, group = group, loss="ls", nfolds = 10)

par(mfrow=c(1,3))
plot(modelo_grupo) # plots the coefficients against the log-lambda sequence 
plot(modelo_grupo,group=TRUE) # plots group norm against the log-lambda sequence 
plot(cvgrupo) # plots against the lambda sequence



RMSE(test.y, predictions)

RMSE(test.y[1:103], pred = predictions_cv)


plot(modelo, xvar = "norm", label = TRUE)

plot(modelo_cv, xvar = "norm", label = T)

plot(modelo, xvar="lambda")

plot(modelo_cv,xvar="lambda")

coef(modelo_cv)


plot(modelo_cv, xvar="norm")

plot(modelo_cv, xvar="lambda")

coef(modelo)

predict(modelo_cv, newx=test.data, )

plot(modelo_cv, xvar = "lambda")



coef(modelo_cv)

















#Logit

#sem o gradient boosting

##### Market Size



#estimando o tamanho do mercado em cada ano
#ou o Market Size
market_size <- lm(log(quant) ~ ano + modelo, data=data)




#app
#
#data4$unidades_est <- round(exp(market_size$fitted.values))
#data4 <- train.data %>% group_by(ano) %>%
#  summarise(unidades_totais_hat = sum(unidades_est)) %>%
#  left_join(data, ., by = "ano") %>%
#  mutate(share2 = unidades_est / unidades_totais_hat)
#
#total <- aggregate(unidades_est ~ ano + modelo, data = data, sum, na.rm = T) %>%
#  left_join(data, ., by=c("ano, modelo")) %>%
#  rename(unidades_est = unidades_totais_hat)
#  mutate(share2 = unidades_est / unidades_totais)

  
  
data3 <- data %>% mutate(unidades_est = round(exp(market_size$fitted.values))) %>%
  group_by(ano)%>%
  summarise(total_uni = sum(unidades_est)) %>%
  left_join(data,., by=c("ano")) %>%
  group_by(modelo) %>%
  mutate(share= unidades_est/total_uni)


######https://rpubs.com/rslbliss/r_logistic_ws


logit_model <- glm(share_formula, data=data3, family="binomial")  



coeftest(logit_model, vcov. = vcov, type = "HC1")

summary(data)



            