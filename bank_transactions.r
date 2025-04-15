# ✅ 1. Cargar el dataset

# Cargamos la librería necesaria
library(data.table)

# Cargamos el dataset
dataset_path <- file.choose()

# Leemos el dataset
df <- fread(dataset_path)

# Ver los primeros registros
head(df)


# ✅ 2. Filtrar transacciones fraudulentas

# Transacciones fraudulentas
fraudulenta <- df[df$is_fraud == 1, ]

# Ver un resumen
summary(fraudulenta)

# ✅ 3. Filtrar transacciones no fraudulentas

# Transacciones no fraudulentas
non_fraudulenta <- df[df$is_fraud == 0, ]

# Ver un resumen
summary(non_fraudulenta)

# ✅ 4. Calcular media y desviación estándar

mean(df$Amount, na.rm = TRUE)           # Media
sd(df$Amount, na.rm = TRUE)             # Desviación estándar

# ✅ 5. Generar boxplots, histogramas y gráficos de densidad con ggplot2 y plotly

# Boxplot con ggplot2

library(ggplot2)

ggplot(transactions, aes(y = Amount)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot del Monto de Transacción",
       y = "Monto") +
  theme_minimal()

# Histograma con ggplot2
ggplot(transactions, aes(x = Amount)) +
  geom_histogram(binwidth = 500, fill = "lightgreen", color = "black") +
  labs(title = "Histograma del Monto de Transacción",
       x = "Monto", y = "Frecuencia") +
  theme_minimal()

# Gráfico de Densidad con ggplot2
ggplot(transactions, aes(x = Amount)) +
  geom_density(fill = "orange", alpha = 0.6) +
  labs(title = "Gráfico de Densidad del Monto de Transacción",
       x = "Monto") +
  theme_minimal()

# Boxplot interactivo con plotly
library(plotly)

plot_ly(transactions, y = ~Amount, type = "box", boxpoints = "all",
        jitter = 0.3, pointpos = -1.8,
        marker = list(color = 'rgba(7,40,89,0.5)'),
        line = list(color = 'rgba(7,40,89,1)')) %>%
  layout(title = "Boxplot Interactivo del Monto de Transacción",
         yaxis = list(title = "Monto"))

# Histograma interactivo con plotly
plot_ly(transactions, x = ~Amount, type = "histogram",
        marker = list(color = 'rgba(255,100,102,0.7)')) %>%
  layout(title = "Histograma Interactivo del Monto de Transacción",
         xaxis = list(title = "Monto"),
         yaxis = list(title = "Frecuencia"))

# Densidad interactiva con plotly
library(ggplot2)
library(plotly)

p <- ggplot(transactions, aes(x = Amount)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Gráfico de Densidad del Monto (Plotly)", x = "Monto") +
  theme_minimal()

ggplotly(p)
