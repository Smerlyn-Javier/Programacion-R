# Cargar las librerías necesarias
library(data.table)
library(ggplot2)
library(factoextra)
library(cluster)

# Cargar los datos del dataset
dataset <- file.choose()

# 1. Convertir los datos a data.table
df <- fread(dataset)

# Verificar la estructura de los datos
str(df)

# 2. Renombrar la columna "Spending Score (1-100)" a "Spending_Score" y "Annual Income (k$)" a "Annual_Income"
# (Nota: Asegúrate de que los nombres de las columnas coincidan exactamente con los del dataset)
setnames(df, "Spending Score (1-100)", "Spending_Score")
setnames(df, "Annual Income (k$)", "Annual_Income")

# Verificar los nombres de las columnas
print(colnames(df))

# 3. Filtrar clientes con más de 5 compras en los últimos 6 meses
# (Nota: Como el dataset no tiene información directa sobre número de compras,
# asumiremos que el Spending Score puede ser un proxy de frecuencia de compras)
# Filtramos clientes con Spending Score > 50 (asumiendo que esto representa alta frecuencia)
clientes_filtrados <- df[Spending_Score > 50, ]


# 4. Aplicar kmeans() para clasificar clientes
# Usaremos Annual Income y Spending Score para la segmentación
datos_segmentacion <- clientes_filtrados[, .(Annual_Income, Spending_Score)]


# Estandarizar los datos
datos_segmentacion_scaled <- scale(datos_segmentacion)

# Determinar el número óptimo de clusters
fviz_nbclust(datos_segmentacion_scaled, kmeans, method = "wss") +
  labs(subtitle = "Método del codo")

# Basado en el gráfico del codo, seleccionamos k=5 clusters
set.seed(123)
kmeans_result <- kmeans(datos_segmentacion_scaled, centers = 5, nstart = 25)


# Añadir la asignación de clusters al dataset
clientes_filtrados[, Cluster := as.factor(kmeans_result$cluster)]

# 4. Visualizar los clusters con factoextra
library(ggplot2)

# Convertir los datos a dataframe
plot_data <- as.data.frame(datos_segmentacion_scaled)
plot_data$cluster <- as.factor(kmeans_result$cluster)

# Crear el gráfico manualmente
ggplot(plot_data, aes(x = Annual_Income, y = Spending_Score, color = cluster)) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", linetype = 2, level = 0.95) +
  theme_bw() +
  labs(title = "Segmentación de Clientes",
       subtitle = "Clusters basados en Ingreso Anual y Puntuación de Gasto",
       x = "Ingreso Anual (estandarizado)",
       y = "Puntuación de Gasto (estandarizada)")

# 5. Analizar características de cada cluster y proponer estrategias
# Resumen estadístico por cluster
resumen_clusters <- clientes_filtrados[, .(
  Count = .N,
  Avg_Income = mean(`Annual Income (k$)`),
  Avg_Spending = mean(`Spending Score (1-100)`),
  Avg_Age = mean(Age)
), by = Cluster]

print(resumen_clusters)


# Proponer estrategias para cada segmento
estrategias <- data.table(
  Cluster = 1:5,
  Segmento = c("Clientes de Ingreso Medio-Alto y Alto Gasto",
               "Clientes de Ingreso Bajo y Alto Gasto",
               "Clientes de Ingreso Alto y Gasto Moderado",
               "Clientes de Ingreso Medio y Gasto Moderado-Alto",
               "Clientes de Ingreso Medio-Bajo y Alto Gasto"),
  Estrategia = c(
    "Ofrecer productos premium y programas de fidelización exclusivos",
    "Promociones frecuentes y planes de pago flexibles para mantener su lealtad",
    "Enfoque en experiencias de compra personalizadas y servicios de alto valor",
    "Paquetes combinados y ofertas de valor agregado",
    "Programas de recompensas y descuentos por volumen de compras"
  )
)

print(estrategias)


# Visualización adicional: Distribución de edad por cluster
ggplot(clientes_filtrados, aes(x = Cluster, y = Age, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Distribución de Edad por Segmento",
       x = "Cluster",
       y = "Edad") +
  theme_minimal()


# Visualización: Ingreso vs Gasto con clusters
ggplot(clientes_filtrados, aes(x = `Annual_Income`, y = `Spending_Score`, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Segmentación de Clientes por Ingreso y Gasto",
       x = "Ingreso Anual",
       y = "Puntuación de Gasto") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")