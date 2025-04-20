# 1. Estadísticas Descriptivas de las Ventas

# Librerías necesarias
library(ggplot2)
library(data.table)

# Cargar los datos
dataset_path <- file.choose()
df <- fread(dataset_path)

# Convertir la columna Date a formato fecha
df$Date <- as.Date(df$Date)

# Estadísticas descriptivas
summary_stats <- summary(df$`Total Amount`)
print("Estadísticas descriptivas de las ventas:")
print(summary_stats)

# Histograma con densidad
ggplot(df, aes(x = `Total Amount`)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribución del Total de Ventas") +
  xlab("Monto Total") +
  ylab("Densidad")

# 2. Modelo de Regresión Lineal

# Preparar los datos
df$Month <- month(df$Date)

# Variables categóricas a dummies
df$Gender <- factor(df$Gender)
df$`Product Category` <- factor(df$`Product Category`)
df_dummies <- model.matrix(~ Age + Quantity + `Price per Unit` + Month + Gender + `Product Category`, data = df)[, -1]

# Variable objetivo
y <- df$`Total Amount`

# Modelo de regresión lineal
modelo <- lm(y ~ ., data = as.data.frame(df_dummies))
summary(modelo)

# Predicciones
y_pred <- predict(modelo)

# Evaluación
rmse_lm <- sqrt(mean((y - y_pred)^2))
mae_lm <- mean(abs(y - y_pred))
cat(sprintf("\nRMSE: %.2f\n", rmse_lm))
cat(sprintf("MAE: %.2f\n", mae_lm))

# 3. Modelo ARIMA para Pronóstico de Ventas

# Agregar ventas diarias
daily_sales <- df %>%
  group_by(Date) %>%
  summarise(`Total Amount` = sum(`Total Amount`)) %>%
  arrange(Date)

# Serie temporal
ts_sales <- ts(daily_sales$`Total Amount`, frequency = 7)

# Visualizar serie
plot(ts_sales, main = "Ventas Diarias Totales", ylab = "Ventas Totales", xlab = "Tiempo")

# Dividir datos en entrenamiento y prueba (últimos 30 días)
train <- head(ts_sales, -30)
test <- tail(ts_sales, 30)

# Convertir test a serie temporal ts
test <- ts(tail(ts_sales, 30), start = end(train)[1] + (end(train)[2] / frequency(train)), frequency = frequency(train))
# Modelo ARIMA
model_arima <- auto.arima(train)
summary(model_arima)

# Pronóstico
forecast_arima <- forecast(model_arima, h = 30)

# Evaluación
rmse_arima <- rmse(test, forecast_arima$mean)
mae_arima <- mae(test, forecast_arima$mean)
cat(sprintf("\nARIMA RMSE: %.2f\n", rmse_arima))
cat(sprintf("ARIMA MAE: %.2f\n", mae_arima))

# Visualizar pronóstico
autoplot(forecast_arima) +
  autolayer(test, series = "Real", PI = FALSE) +
  ggtitle("Pronóstico de Ventas con ARIMA") +
  xlab("Fecha") +
  ylab("Ventas Totales") +
  theme_minimal()

# 4. Evaluación Comparativa de Modelos

comparacion <- data.frame(
  Modelo = c("Regresión Lineal", "ARIMA"),
  RMSE = c(rmse_lm, rmse_arima),
  MAE = c(mae_lm, mae_arima)
)
print(comparacion)
