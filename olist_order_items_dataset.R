# Cargar los datos
order_items <- read.csv("olist_order_items_dataset.csv")

# Función para aplicar descuentos
aplicar_descuento <- function(precio, cantidad, categoria_cliente) {
  # Definir reglas de descuento
  if (categoria_cliente == "Premium" && cantidad >= 3) {
    descuento <- 0.15  # 15% de descuento para clientes premium con 3+ items
  } else if (categoria_cliente == "Regular" && cantidad >= 5) {
    descuento <- 0.10  # 10% de descuento para clientes regulares con 5+ items
  } else if (cantidad >= 10) {
    descuento <- 0.20  # 20% de descuento para cualquier cliente con 10+ items
  } else {
    descuento <- 0  # Sin descuento
  }
  
  # Calcular precio con descuento
  precio * (1 - descuento)
}

# Simular categorías de cliente (para este ejemplo)
set.seed(123)
order_items$categoria_cliente <- sample(c("Premium", "Regular"), 
                                        nrow(order_items), 
                                        replace = TRUE, 
                                        prob = c(0.3, 0.7))

# Calcular cantidad de items por orden
library(dplyr)
order_items <- order_items %>%
  group_by(order_id) %>%
  mutate(cantidad_por_orden = n()) %>%
  ungroup()

# Aplicar la función de descuento
order_items$precio_con_descuento <- mapply(aplicar_descuento,
                                           order_items$price,
                                           order_items$cantidad_por_orden,
                                           order_items$categoria_cliente)

# Análisis comparativo
summary(order_items$price)
summary(order_items$precio_con_descuento)

# Gráficos comparativos
library(ggplot2)

# Boxplot comparativo
ggplot(order_items) +
  geom_boxplot(aes(x = "Precio Original", y = price)) +
  geom_boxplot(aes(x = "Precio con Descuento", y = precio_con_descuento)) +
  labs(title = "Comparación de Precios Antes y Después del Descuento",
       y = "Precio", x = "") +
  theme_minimal()

# Histograma comparativo
ggplot(order_items) +
  geom_histogram(aes(x = price, fill = "Original"), alpha = 0.5, bins = 30) +
  geom_histogram(aes(x = precio_con_descuento, fill = "Con Descuento"), alpha = 0.5, bins = 30) +
  labs(title = "Distribución de Precios Antes y Después del Descuento",
       x = "Precio", y = "Frecuencia") +
  scale_fill_manual(values = c("Original" = "blue", "Con Descuento" = "red")) +
  theme_minimal()

# Análisis de ahorros
order_items <- order_items %>%
  mutate(ahorro = price - precio_con_descuento,
         porcentaje_descuento = (ahorro / price) * 100)

# Resumen de ahorros
summary(order_items$ahorro)
summary(order_items$porcentaje_descuento)

# Gráfico de ahorros por categoría de cliente
ggplot(order_items %>% filter(ahorro > 0)) +
  geom_boxplot(aes(x = categoria_cliente, y = ahorro, fill = categoria_cliente)) +
  labs(title = "Ahorros por Categoría de Cliente",
       x = "Categoría de Cliente", y = "Ahorro") +
  theme_minimal()