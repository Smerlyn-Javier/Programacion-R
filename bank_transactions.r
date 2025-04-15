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