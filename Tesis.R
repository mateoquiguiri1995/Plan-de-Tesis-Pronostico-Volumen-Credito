# Libraries ---------------------------------------------------------------
#if (!require(randomForest)) install.packages('randomForest')
library(table1) #Summary statistics
library(kableExtra) # beautiful Tables
#library(Ckmeans.1d.dp)
library(xgboost) 
library(randomForest)
library(lmtest) #diagnostic checking in linear regression models
#library(interactions) #Graphing OLS Results
library(jtools) #OLS Results table
library(tidyverse) 
library(readxl)#Tidy 
library(janitor) #clean_names
library(lubridate) # time data
library(hrbrthemes) # color extra 'ggplot2' themes
#library(skimr) # Summary
library(readxl)
library(gridExtra) #grid plots
library(grid) # plots
library(lattice) # plots
library(forecast) 
library(xgboost)
library(caret) #predictive models
library(car) 
library(mice) #data imputatation
#library(VIM) #Missin Values
library(ggcorrplot)
#library(RColorBrewer)
library(viridis) 
library(PerformanceAnalytics)
library(tseries)
library(MLmetrics)
# Functions ---------------------------------------------------------------
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

# Data Import and Cleaning ------------------------------------------------

base_proyecto <- read_excel("base_proyecto.xlsx",sheet = "Hipotecario") %>% 
  rename(fecha = Month) %>% 
  clean_names()

# Data Transformation -----------------------------------------------
base_proyecto <- base_proyecto %>% 
  select(1,4,7:18) %>% 
  mutate(fecha = ymd(fecha),credito_total=inmobiliario) %>% 
  filter(fecha >= "2012-01-01")
  
base_proyecto <- base_proyecto[1:nrow(base_proyecto)-1,]
  #select(-c(n_operaciones,credito_total_miles)) 
# * Imputacion CART----
#base_proyecto_imp <- mice(base_proyecto, m=3, maxit = 50, method = 'cart', seed = 500)
#base_proyecto <- complete(base_proyecto_imp,1)
# Serie para modelo SARIMA
credito_total <- ts(base_proyecto$credito_total, frequency = 12, start = c(2010,10))
# Base original para ARIMAX
base_proyecto_o <- base_proyecto
ts_base_proyecto_o <- ts(base_proyecto_o[,-1], frequency=12, start = c(2010,10))
# Muestra de los datos
head(base_proyecto,10) %>%
  kbl() %>%
  kable_minimal()


# * Normalizacion CARET ----
base_proyecto_s <- scale(base_proyecto[,-1]) #matriz para escalar
sd <- attr(base_proyecto_s,'scaled:scale')
media <- attr(base_proyecto_s,'scaled:center')
base_proyecto_s <- as.data.frame(base_proyecto_s)
base_proyecto <- base_proyecto_s %>% mutate(fecha=ymd(base_proyecto$fecha))
#split_factor <- floor((nrow(base_proyecto)/20)*19)         #define % of training and test set
ts_base_proyecto <- ts(base_proyecto[,-10], frequency = 12, start = c(2010,10))
credito_total_n <- ts_base_proyecto[,1]
# Descriptive Statistics --------------------------------------------------
head(base_proyecto,10) %>%
  kbl() %>%
  kable_minimal()# Summary
#my_skim <- skim_with(numeric=sfl(hist=NULL))
#my_skim(base_proyecto) # Summary #OCULTAR
#skim(base_proyecto)
table1::table1(~., data = base_proyecto_o[,-1])


#base_proyecto_cor<-cor_pmat(base_proyecto[,-1],method='spearman') # Corr Spearman
chart.Correlation(base_proyecto[,-10], histogram = TRUE) # Corr Pearson

ggplot(base_proyecto_o, aes(x=fecha, y=credito_total)) +
  geom_line( color="#69b3a2") +
  #theme_ipsum() +
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  labs(title = "Colocacion Credito Credito Hipotecario",
       subtitle = "De Oct 2010 a Sep 2021",x="Fecha",y="Millones de $ (USD)",
       caption = "Fuente: BIESS")+
  scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")

g1<-ggplot(base_proyecto, aes(x=fecha, y=tasa_activa)) +
  geom_line() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title = "Tasa de Interes Activa",x="",y="Tasa %",caption = "Fuente: Banco Central del Ecuador")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")
g2<- ggplot(base_proyecto, aes(x=fecha, y=tasa_pasiva)) +
  geom_line() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title = "Tasa de Interés Pasiva",x="",y="Tasa %",caption = "Fuente: Banco Central del Ecuador")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")
g3<-ggplot(base_proyecto, aes(x=fecha, y=inflacion)) +
  geom_line() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title = "Inflación mensual",x="",y="Tasa %",caption = "Fuente: Banco Central del Ecuador")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")
g4<-ggplot(base_proyecto, aes(x=fecha, y=precio_wti)) +
  geom_line() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title = "Cotización barril WTI",x="",y="$ (USD)",caption = "Fuente: Banco Central del Ecuador")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")

g5<-grid.arrange(g1,g2,g3,g4,nrow=2)

# Google queries
colors <- c("Crédito Quirografario" = "black", "Crédito de Consumo" = "aquamarine1",
            "Crédito Quirografario BIESS" = "brown1","Crédito Consumo BIESS"="darkblue")
ggplot(base_proyecto, aes(x = fecha))+
  geom_line(aes(y = google_q1*sd[2]+media[2],color="Crédito Quirografario"),alpha=0.7)+
  geom_line(aes(y = google_q2*sd[3]+media[3],color="Crédito de Consumo"),alpha=0.7)+
  geom_line(aes(y = google_q3*sd[4]+media[4],color="Crédito Quirografario BIESS"),alpha=0.7)+
  geom_line(aes(y = google_q4*sd[5]+media[5],color="Crédito Consumo BIESS"),alpha=0.7)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")+
  labs(y="N Búsquedas",x="Fecha", color = "Serie")+
  scale_color_manual(values = colors)

#ggsave("g1.png",height = 12,width = 12,units = "cm")

# Estudio comparativo experimental ----------------------------------------

# ML data preparation -----------------------------------------------------
set.seed(28)
# Data Cleaning and Transforming
base_proyecto <- base_proyecto %>% mutate(year =as.factor(year(fecha)),yday = yday(fecha),
                                          month = as.factor(month(fecha)))

head(base_proyecto[,-10]) %>%
  kbl() %>%
  kable_minimal()


# OLS Model ---------------------------------------------------------------

base_proyecto_ols <- base_proyecto[,-10]
base_proyecto_ols$sq_google_q1 <- base_proyecto$google_q1^2
base_proyecto_ols$sq_google_q2 <- base_proyecto$google_q2^2
base_proyecto_ols$covid <- 0
base_proyecto_ols$covid[114:132] <- 1 
base_proyecto_ols <- base_proyecto_ols[,-10]
ols_model <- lm(credito_total~., data = base_proyecto_ols)
summary(ols_model)
#summ(ols_model)
interact_plot(ols_model, pred = google_q2, modx = covid, plot.points = TRUE) 
#base_proyecto$ols_pred <- predict(ols_model) # Prediction column 
ggplot(base_proyecto, aes(x=credito_total,y=ols_pred))+geom_point() # Pred vs Data
# Residual Diagnostic Test
# residualPlots(ols_model)
#base_proyecto_ols$resid <- resid(ols_model) #Test resid correlation with indep var, linearity of data
# hist(base_proyecto_ols$resid) #Test normalidad
# qqPlot(base_proyecto_ols$resid) #Test normalidad
# shapiro.test(base_proyecto_ols$resid) #Test normalidad
# ggplot(base_proyecto_ols,aes(x=1:nrow(base_proyecto_ols),y=resid))+geom_point() # Autocorrelation of residuals in R
# dwtest(ols_model) #Autocorrelation Durbin Watson test
# ncvTest(ols_model) # Breusch-Pagan test homoscedasticity
# Multicollinearity
#base_proyecto_ols %>% select(-c(1,10,11)) %>% chart.Correlation(., histogram = TRUE)
vif(ols_model)
#mean(base_proyecto_ols$resid)

# Split the data into training and test set

#base_proyecto <- base_proyecto[,-1]
# 
# train_index  <- createDataPartition(base_proyecto_ols$credito_total,p=0.8,list = FALSE)
# train_base_proyecto <- base_proyecto_ols[train_index,]
# test_base_proyecto <- base_proyecto_ols[-train_index,]
# # Build the model
# model1 <- lm(credito_total ~., data = train_base_proyecto)
# # Make predictions
# ols_predictions_test <- model1 %>% predict(test_base_proyecto)
# ols_predictions_train <- predict(model1)
# base_proyecto_ols$credito_total_pred <- append(ols_predictions_train,ols_predictions_train)
# # Forecast Plot
# ggplot(data=base_proyecto_arima)+geom_line(aes(fecha,credito_total*sd[1]+media[1]),col="#69b3a2")+
#   geom_line(aes(fecha,credito_total_pred*sd[1]+media[1]),col="gray")+theme(axis.text.x=element_text(angle=60, hjust=1))+
#   annotate("rect", xmin = as.Date("2021-03-01", "%Y-%m-%d"), 
#            xmax = as.Date ("2021-09-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf,
#            alpha = .1,fill = "red")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")+ylab("Credito Total")
# 
# # Model performance
# data.frame(
#   RMSE = caret::RMSE(ols_predictions, test_base_proyecto$credito_total),
#   MSE = MSE(ols_predictions, test_base_proyecto$credito_total),
#   #R2 = caret::R2(predictions, test_base_proyecto$credito_total),
#   MAPE = MAPE(ols_predictions, test_base_proyecto$credito_total)*100
# )

# ARIMA Model -------------------------------------------------------------------
# Decompose Plot
autoplot(decompose(credito_total, type = "additive")) + xlab("Time") + ylab("") + 
  ggtitle("Decomposition plot of the Credit Volume Time-Series") + 
  theme(plot.title = element_text(hjust = 0.5))
#autoplot(diff(log(credito_total))) # Getting the tserie stationary 
#adf.test(diff(log(credito_total)), alternative="stationary", k=0) # Test Estacionariedad

#Acf(diff(log(credito_total))) #ACF
#Pacf(diff(log(credito_total))) #PACF

# Split the data
arima_train <- ts(credito_total_n[1:split_factor],frequency=12,start = c(2010,10))           #training set
arima_test <- ts(credito_total_n[(split_factor+1):132],frequency=12,start = c(2021,03))              #test set

sarima<- arima(arima_train, c(1,1,3),seasonal = list(order = c(0,0,2),period= 12)) #ARIMA 301
summary(sarima)

arima_predictions_train <- (arima_train-sarima$residuals)
arima_predictions_list <- forecast(sarima,h=7,level = c(95))
arima_predictions_test <- arima_predictions_list$mean
arima_predictions <- append(arima_predictions_train,arima_predictions_test)

base_proyecto_arima <- data.frame(fecha = base_proyecto[,10],
                                  credito_total = credito_total_n,
                                  credito_total_pred = arima_predictions) %>% 
  mutate(residuals = credito_total-credito_total_pred)

# How the model fitted the data
# plot(credito_total) 
# lines((sarima$fitted*sd[1]+media[1]), col="red")
# Residual Diagnostic
# autoarima_res <- resid(autoARIMA)
# autoplot(autoarima_res)
# Acf(autoarima_res) #ACF
# Pacf(autoarima_res) #PACF
# adf.test(autoarima_res, alternative="stationary", k=0) # Test Estacionariedad
# Forecast Plot with confidence interval
#autoplot(forecast(arima_predictions_list))
# Forecast Plot
colors <- c("Valor Real" = "azure4", "Pronóstico" = "#69b3a2")
ggplot(base_proyecto_arima, aes(x = fecha))+
  geom_line(aes(y = as.numeric(credito_total)*sd[1]+media[1],color="Valor Real"))+
  geom_line(aes(y = credito_total_pred*sd[1]+media[1],color="Pronóstico"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  annotate("rect", xmin = as.Date("2021-03-01", "%Y-%m-%d"), 
           xmax = as.Date ("2021-09-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf,
           alpha = .1,fill = "red")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")+
  labs(y="Millones de $ (USD)",x="Fecha", color = "Serie")+
  scale_color_manual(values = colors)+
  labs(caption = "SARIMA (1,1,3) (0,0,2) (12)")

# Model Performance

mape_sarima <- mape(arima_train,arima_predictions_train)  #MAPE Si existe sobreajuste se debe eliminar variables de acuerdo al MSE
mse_sarima <- mean((arima_train-arima_predictions_train)^2) #MSE
rmse_sarima <- sqrt(mean((arima_train - arima_predictions_train)^2)) #RMSE
print(data.frame(MAPE=mape_sarima,MSE=mse_sarima,RMSE=rmse_sarima)) #Medidas de Ajuste

mape_sarima <- mape(arima_test,arima_predictions_test)  #MAPE Si existe sobreajuste se debe eliminar variables de acuerdo al MSE
mse_sarima <- mean((arima_test-arima_predictions_test)^2)  #MSE
rmse_sarima <- sqrt(mean((arima_test-arima_predictions_test)^2)) #RMSE
data.frame(MAPE=mape_sarima,MSE=mse_sarima,RMSE=rmse_sarima) #Medidas de Ajuste

# ARIMAX Model ---------------------------------------------------------------

# Decompose Plot
autoplot(decompose(ts_base_proyecto_o[,3], type = c("additive","multiplicative"))) + xlab("Time") + ylab("") + 
  ggtitle("Decomposition plot of the Credit Volume Time-Series") + 
  theme(plot.title = element_text(hjust = 0.5))
#autoplot(diff(ts_base_proyecto_o[,3])) # Getting the tserie stationary 
adf.test(diff(ts_base_proyecto_o[,3]), alternative="stationary", k=0) # Test Estacionariedad
pp.test(diff(ts_base_proyecto_o[,3]))

Acf(diff(ts_base_proyecto_o[,3]),lag.max = 40) #ACF
Pacf(diff(ts_base_proyecto_o[,3]),lag.max = 40) #PACF


# Split the data
arimax_train <- ts(base_proyecto_o[1:split_factor,c(2:4,9)],frequency=12,start = c(2010,10))           #training set
arimax_test <- ts(base_proyecto_o[(split_factor+1):132,c(2:4,9)],frequency=12,start = c(2021,3))
#test set

autoARIMAX <- auto.arima(arimax_train[,1],xreg = arimax_train[,c(3)], trace=TRUE)
summary(autoARIMAX)

arimax_predictions_train <- autoARIMAX$fitted
arimax_predictions_list <- autoARIMAX %>%  forecast(xreg=arimax_test[,c(3)])
arimax_predictions_test <- arimax_predictions_list$mean
arimax_predictions <- append(arimax_predictions_train,arimax_predictions_test)

base_proyecto_arimax <- data.frame(fecha=base_proyecto[,10],
                                   credito_total=credito_total_n*sd[1]+media[1],
                                   credito_total_pred=arimax_predictions) %>% 
  mutate(residuals=credito_total-credito_total_pred)

# How the model fitted the data
# plot(credito_total) 
# lines((sarima$fitted*sd[1]+media[1]), col="red")
# Residual Diagnostic
autoarimax_res <- resid(autoARIMAX)
whitenoise.test(autoarimax_res)
# autoplot(autoarima_res)
# Acf(autoarima_res) #ACF
# Pacf(autoarima_res) #PACF
adf.test(autoarimax_res, k=3) # Test Estacionariedad
Box.Ljung.Test(autoarimax_res, lag = 1) # H0:	The model does not exhibit lack of fit.
# Forecast Plot with confidence interval
#autoplot(forecast(arima_predictions_list))

# Forecast Plot
colors <- c("Valor Real" = "azure4", "Pronóstico" = "#69b3a2")
ggplot(base_proyecto_arimax, aes(x = fecha))+
  geom_line(aes(y = as.numeric(credito_total),color="Valor Real"))+
  geom_line(aes(y = credito_total_pred,color="Pronóstico"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  annotate("rect", xmin = as.Date("2021-03-01", "%Y-%m-%d"), 
           xmax = as.Date ("2021-09-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf,
           alpha = .1,fill = "red")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")+
  labs(y="Millones de $ (USD)",x="Fecha", color = "Serie")+
  scale_color_manual(values = colors)+
  labs(caption = "ARIMAX (1,1,3) (0,0,2) (12)")
# Model Performance

mape_sarimax <- mape((arimax_train[,1]-media[1])/sd[1],(arimax_predictions_train-media[1])/sd[1])  #MAPE Si existe sobreajuste se debe eliminar variables de acuerdo al MSE
mse_sarimax <- mean(((arimax_train[,1]-media[1])/sd[1]-(arimax_predictions_train-media[1])/sd[1])^2)  #MSE
rmse_sarimax <- sqrt(mean(((arimax_train[,1]-media[1])/sd[1] - (arimax_predictions_train-media[1])/sd[1])^2)) #RMSE
print(data.frame(MAPE=mape_sarimax,MSE=mse_sarimax,RMSE=rmse_sarimax)) #Medidas de Ajuste

mape_sarimax <- mape((arimax_test[,1]-media[1])/sd[1],(arimax_predictions_test-media[1])/sd[1])  #MAPE Si existe sobreajuste se debe eliminar variables de acuerdo al MSE
mse_sarimax <- mean(((arimax_test[,1]-media[1])/sd[1]-(arimax_predictions_test-media[1])/sd[1])^2)  #MSE
rmse_sarimax <- sqrt(mean(((arimax_test[,1]-media[1])/sd[1] - (arimax_predictions_test-media[1])/sd[1])^2)) #RMSE
print(data.frame(MAPE=mape_sarimax,MSE=mse_sarimax,RMSE=rmse_sarimax)) #Medidas de Ajuste

# RF Model ---------------------------------------------------------------

# Data Partition
split_factor <- floor((nrow(base_proyecto)/20)*19)   
ml_train <- base_proyecto[1:split_factor,-10]  #training set
ml_test <- base_proyecto[(split_factor+1):132,-10]   

#rf = randomForest(credito_total~year+google_q1+tasa_pasiva+google_q4+google_q2+precio_wti+ month, data = ml_train)
rf = randomForest(credito_total~., data = ml_train,importance=TRUE)

ImpData <- as.data.frame(importance(rf))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

rf_predictions_train = predict(rf, newdata = ml_train)

rf_predictions_test = predict(rf, newdata = ml_test)
predictions_plot <- append(rf_predictions_train,rf_predictions_test)
base_proyecto_rf <- data.frame(fecha=base_proyecto$fecha,credito_total=base_proyecto$credito_total)
base_proyecto_rf$credito_total_pred<-predictions_plot
base_proyecto_rf$residuals <- (base_proyecto_rf$credito_total - base_proyecto_rf$credito_total_pred)

# Forecast Plot
colors <- c("Valor Real" = "azure4", "Pronóstico" = "#69b3a2")
ggplot(base_proyecto_rf, aes(x = fecha))+
  geom_line(aes(y = credito_total*sd[1]+media[1],color="Valor Real"))+
  geom_line(aes(y = credito_total_pred*sd[1]+media[1],color="Pronóstico"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  annotate("rect", xmin = as.Date("2021-03-01", "%Y-%m-%d"), 
           xmax = as.Date ("2021-09-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf,
           alpha = .1,fill = "red")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")+
  labs(y="Millones de $ (USD)",x="Fecha", color = "Serie")+
  scale_color_manual(values = colors)+
  labs(caption = "Random Forest (RF)")
# Forecast Performance
rf_mape <- mape(ml_train$credito_total, rf_predictions_train)  #MAPE Si existe sobreajuste se debe eliminar variables de acuerdo al MSE
rf_mse <- mean((ml_train$credito_total-rf_predictions_train)^2)  #MSE
rf_rmse <- sqrt(mean((ml_train$credito_total-rf_predictions_train)^2)) #RMSE

print(data.frame(MAPE=rf_mape,MSE=rf_mse,RMSE=rf_rmse))

rf_mape <- mape(ml_test$credito_total, rf_predictions_test)  #MAPE Si existe sobreajuste se debe eliminar variables de acuerdo al MSE
rf_mse <- mean((ml_test$credito_total-rf_predictions_test)^2)  #MSE
rf_rmse <- sqrt(mean((ml_test$credito_total-rf_predictions_test)^2)) #RMSE
print("TEST ADJUSTMENT")
print(data.frame(MAPE=rf_mape,MSE=rf_mse,RMSE=rf_rmse))

# XG Boost Model----------------------------------------------------------
#Splitting train and test data set

split_factor = 119
xgb_train <- base_proyecto[1:split_factor,]

train_targets <- xgb_train$credito_total  #Y train

xgb_train[] <- sapply(xgb_train, as.numeric)
xgb_train <- select(xgb_train,-c(1,14))

xgb_test <- base_proyecto[(split_factor+1):nrow(base_proyecto),]

test_targets <- xgb_test$credito_total  #Y test

xgb_test[] <- sapply(xgb_test, as.numeric)
xgb_test <- select(xgb_test,-c(1,14))

#Cross-validation
xgb_trcontrol <- trainControl(
  method = "cv",
  number = 3,
  allowParallel = TRUE,
  verboseIter = FALSE
)
# xgb_trcontrol <- trainControl(
#   method = "timeslice",
#   initialWindow = 24,
#   fixedWindow = TRUE,
#   horizon = 6,
#   verboseIter = FALSE)

#Building parameters set
xgb_grid <- expand.grid(
  list(
    nrounds = c(500,1000,1500),
    max_depth = c(2,4,6), 
    colsample_bytree = 1, 
    eta = 0.3,
    gamma = 0,
    min_child_weight = 1,  
    subsample = 1)
)
#Building the model
model_xgb <- train(x = xgb_train,
                   y = train_targets,
                   trControl = xgb_trcontrol,
                   tuneGrid = xgb_grid,
                   method = "xgbTree",
                   verbose = TRUE)
model_xgb

xgb_imp <- xgb.importance(
  feature_names = colnames(xgb_train),
  model = model_xgb$finalModel)

xgb.ggplot.importance(xgb_imp,n_clusters = c(2))+ 
  ggtitle("Importancia de variables explicativas") +
  theme_bw()+
  theme(legend.position="none")

xgb_predictions_test <- predict(model_xgb, xgb_test)
# Creating variables for model Performance 
xgb_predictions_train <- predict(model_xgb)
xgb_predictions <- append(xgb_predictions_train,xgb_predictions_test)

base_proyecto_xgb <- data.frame(fecha=base_proyecto[,15],
                                credito_total=as.numeric(credito_total_n),
                                credito_total_pred=xgb_predictions) %>% 
  mutate(residuals=credito_total-credito_total_pred)
# Forecast Plot
colors <- c("Valor Real" = "azure4", "Pronóstico" = "#69b3a2")
ggplot(base_proyecto_xgb, aes(x = fecha))+
  geom_line(aes(y = credito_total*sd[1]+media[1],color="Valor Real"))+
  geom_line(aes(y = credito_total_pred*sd[1]+media[1],color="Pronóstico"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  annotate("rect", xmin = as.Date("2021-12-01", "%Y-%m-%d"), 
           xmax = as.Date ("2022-03-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf,
           alpha = .1,fill = "red")+scale_x_date(date_breaks="1 year",date_labels ="%m-%Y")+
  labs(y="Millones de $ (USD)",x="Fecha", color = "Serie")+
  scale_color_manual(values = colors)+
  labs(caption = "Random Forest (RF)")

# Model performance
# xgb_mape = MAPE(xgb_predictions_train,train_targets)*100
# xgb_mse = mean((train_targets- xgb_predictions_train)^2)
# xgb_rmse = RMSE(xgb_predictions_train,train_targets)
# 
# print(data.frame(MAPE=xgb_mape,MSE=xgb_mse,RMSE=xgb_rmse))

xgb_mape = MAPE((xgb_predictions_test*sd[1]+media[1]),(test_targets*sd[1]+media[1]))*100
xgb_mse = mean(((test_targets*sd[1]+media[1])- (xgb_predictions_test*sd[1]+media[1]))^2)
xgb_rmse = RMSE(xgb_predictions_test*sd[1]+media[1],test_targets*sd[1]+media[1])

print(data.frame(MAPE=xgb_mape,MSE=xgb_mse,RMSE=xgb_rmse))

#Feature importance
xgb_imp <- xgb.importance(
  feature_names = colnames(xgb_train),
  model = model_xgb$finalModel)

xgb.ggplot.importance(xgb_imp,n_clusters = c(2))+ 
  ggtitle("Importancia de variables explicativas") +
  theme_bw()+
  theme(legend.position="none")

xgb_imp$Importance

# Model Comparision -------------------------------------------------------
base_proyecto_comparacion <- base_proyecto[,c(10,1)]
base_proyecto_comparacion$pred_sarima <- base_proyecto_arima$credito_total_pred
base_proyecto_comparacion$pred_rf <- base_proyecto_rf$credito_total_pred
base_proyecto_comparacion$pred_xgb <- base_proyecto_xgb$credito_total_pred
base_proyecto_comparacion$pred_arimax <- base_proyecto_arimax$credito_total_pred

colors <- c("Valor Actual" = "black", "Sarima" = "green", "Random Forest" = "brown1","XGBoost"="blue", "Arimax"="chocolate4")
ggplot(base_proyecto_comparacion, aes(x = fecha))+
  annotate("rect", xmin = as.Date("2021-03-01", "%Y-%m-%d"), 
           xmax = as.Date ("2021-10-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf,
           alpha = .1,fill = "red")+
  geom_line(aes(y = credito_total*sd[1]+media[1],color="Valor Actual"),alpha=0.7)+
  geom_line(aes(y = pred_sarima*sd[1]+media[1],color="Sarima"),alpha=0.5)+
  geom_line(aes(y = pred_rf*sd[1]+media[1],color="Random Forest"),alpha=0.5)+
  geom_line(aes(y = pred_xgb*sd[1]+media[1],color="XGBoost"),alpha=0.5)+
  geom_line(aes(y = pred_arimax,color="Arimax"),alpha=0.5)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  coord_cartesian(xlim=as.Date(c("2019-01-01","2021-10-01")))+scale_x_date(date_breaks="3 months",
                                                                           date_labels ="%m-%Y")+
  labs(title = "Comparación de Modelos",y="Credito total en Millones de $ (USD)",x="Fecha", color = "Modelo")+
  scale_color_manual(values = colors)

#names <- c("Modelo","MAPE","MSE","RMSE")
modelo <- c("ARIMA","ARIMAX","RANDOM FOREST","XGBOOST")
mape <- c(93.7123,44.0226,51.5909,29.9703)
mse <- c(1.4291,0.4555,0.5299,0.2405)
rmse <- c(1.1954,0.6749,0.7279,0.4904)

base_proyecto_comparacion <- data.frame("MODELO"=modelo,"MAPE"=mape,"MSE"=mse,"RMSE"=rmse)
print(base_proyecto_comparacion)
