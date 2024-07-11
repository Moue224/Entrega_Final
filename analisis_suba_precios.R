# Instalar y cargar las librerías necesarias
install.packages("tidyr")
install.packages("dplyr")
install.packages("readxl")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("quantmod")
install.packages("zoo")
library(tidyr)
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(quantmod)
library(zoo)



# Cargar el archivo Excel
datos <- read_excel('C:/Users/plasa/Documents/analisis de suba de precios/base_3.XLSX', col_names = TRUE)

# Reemplazar espacios en los nombres de las columnas por guiones bajos
names(datos) <- gsub(" ", "_", names(datos))


# Convertir la columna de fechas a formato Date
datos$Fecha <- as.Date(datos$Fecha_documento, format = "%Y-%m-%d")

# Descargar los tipos de cambio históricos
getSymbols("EURUSD=X", src = "yahoo", from = "2020-01-01", to = Sys.Date())
getSymbols("UYUUSD=X", src = "yahoo", from = "2020-01-01", to = Sys.Date())

# Convertir los datos a data frames y renombrar las columnas
eurusd <- data.frame(Date = index(`EURUSD=X`), EURUSD = coredata(Cl(`EURUSD=X`)))
uyuusd <- data.frame(Date = index(`UYUUSD=X`), UYUUSD = coredata(Cl(`UYUUSD=X`)))

head(eurusd)

# Asegurarse de que las fechas sean continuas (incluye fines de semana y feriados)
complete_dates <- data.frame(Date = seq(from = as.Date("2020-01-01"), to = Sys.Date(), by = "day"))



# Unir los datos descargados con las fechas completas e interpolar los valores faltantes
eurusd <- complete_dates %>%
  left_join(eurusd, by = "Date") %>%
  mutate(EURUSD.X.Close = zoo::na.approx(`EURUSD.X.Close`, na.rm = FALSE))

uyuusd <- complete_dates %>%
  left_join(uyuusd, by = "Date") %>%
  mutate(UYUUSD.X.Close = zoo::na.approx(`UYUUSD.X.Close`, na.rm = FALSE))

head(eurusd)

# Renombrar las columnas para el join posterior
eurusd <- eurusd %>% rename(Fecha_documento = Date, EURUSD = EURUSD.X.Close)
uyuusd <- uyuusd %>% rename(Fecha_documento = Date, UYUUSD = UYUUSD.X.Close)

head(eurusd)
head(uyuusd)

# Realiza un left join con los datos de las tasas de cambio
datos <- datos %>%
  left_join(eurusd, by = "Fecha_documento") %>%
  left_join(uyuusd, by = "Fecha_documento")

head(datos)

# Calcula la tasa a USD
datos <- datos %>%
  mutate(Tasa_a_USD = case_when(
    Moneda == "EUR" ~ EURUSD,
    Moneda == "UYU" ~ UYUUSD,
    Moneda == "USD" ~ 1,
    TRUE ~ NA_real_
  ))

# Convertir todos los precios a USD utilizando la tasa de cambio correspondiente a cada fecha
datos <- datos %>%
  mutate(Precio_USD = Valor_neto_de_pedido * Tasa_a_USD / Cantidad_de_pedido)

# Verifica los primeros registros del resultado
head(datos)

# Verifica los primeros registros del resultado
head(datos)


# Convertir todos los precios a USD utilizando la tasa de cambio correspondiente a cada fecha
datos <- datos %>%
  mutate(Precio_USD = Valor_neto_de_pedido * Tasa_a_USD / Cantidad_de_pedido)


# Exportar el data frame a un archivo Excel
write.xlsx(datos, file = "C:/Users/plasa/Documents/analisis de suba de precios/datos_con_TC.xlsx")



# Dividir los datos en dos periodos: antes y después del 01/01/2022
datos_antes_2022 <- datos %>% filter(Fecha < as.Date("2022-01-01"))
datos_despues_2022 <- datos %>% filter(Fecha >= as.Date("2022-01-01"))

# Identificar los materiales presentes en ambos periodos
materiales_antes <- unique(datos_antes_2022$Material)
materiales_despues <- unique(datos_despues_2022$Material)
materiales_comunes <- intersect(materiales_antes, materiales_despues)


# Filtrar los datos para conservar solo los materiales presentes en ambos periodos
datos_antes_2022 <- datos_antes_2022 %>% filter(Material %in% materiales_comunes)
datos_despues_2022 <- datos_despues_2022 %>% filter(Material %in% materiales_comunes)

# Calcular el promedio de precios por material en ambos periodos
promedio_antes_2022 <- datos_antes_2022 %>%
  group_by(Material) %>%
  summarise(Promedio_Precio_Antes = mean(Precio_USD, na.rm = TRUE))

promedio_despues_2022 <- datos_despues_2022 %>%
  group_by(Material) %>%
  summarise(Promedio_Precio_Despues = mean(Precio_USD, na.rm = TRUE))

# Unir los dos dataframes por el nombre del material
comparacion_precios <- merge(promedio_antes_2022, promedio_despues_2022, by = "Material")

head(comparacion_precios)

# Calcular la diferencia porcentual en los precios
comparacion_precios <- comparacion_precios %>%
  mutate(Cambio_Porcentual = ((Promedio_Precio_Despues - Promedio_Precio_Antes) / Promedio_Precio_Antes) * 100)

# Mostrar los primeros resultados
head(comparacion_precios)

# Filtrar valores no finitos en la columna Cambio_Porcentual
comparacion_precios <- comparacion_precios %>%
  filter(is.finite(Cambio_Porcentual))

# Verificar los datos después de filtrar
summary(comparacion_precios$Cambio_Porcentual)

# Eliminar valores extremos (opcional)
comparacion_precios <- comparacion_precios %>%
  filter(Cambio_Porcentual > -50 & Cambio_Porcentual < 150)

# Verificar de nuevo los datos después de eliminar valores extremos
summary(comparacion_precios$Cambio_Porcentual)

# Crear el histograma de los cambios porcentuales en los precios
ggplot(comparacion_precios, aes(x = Cambio_Porcentual)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Cambios Porcentuales en Precios",
       x = "Cambio Porcentual (%)",
       y = "Frecuencia") +
  theme_minimal()

# Calcular el impacto total
impacto_total <- mean(comparacion_precios$Cambio_Porcentual, na.rm = TRUE)

# Mostrar el impacto total
impacto_total

# Cargar la base de datos de unidades consumidas
consumo <- read_excel('C:/Users/plasa/Documents/analisis de suba de precios/consumo22-23.xlsx', col_names = TRUE)

# Revisar la estructura de la nueva base de datos
head(consumo)

# Filtrar el consumo para mantener solo los materiales presentes en comparacion_precios
consumo_filtrado <- consumo %>%
  filter(Material %in% comparacion_precios$Material)

# Verificar los primeros registros del resultado filtrado
head(consumo_filtrado)

# Unir las bases de datos por 'Material'
datos_unidos <- comparacion_precios %>%
  left_join(consumo_filtrado, by = "Material")

# Verificar los primeros registros del resultado unido
head(datos_unidos)

# Sustituir los valores NA en cant_consumida por 0
datos_unidos <- datos_unidos %>%
  mutate(cant_consumida = replace_na(cant_consumida, 0))

# Calcular el impacto ponderado
datos_unidos <- datos_unidos %>%
  mutate(Impacto_Ponderado = -cant_consumida * Cambio_Porcentual / -sum(cant_consumida, na.rm = TRUE))

# Verificar los primeros registros del resultado
head(datos_unidos)

# Calcular el impacto total
impacto_total <- sum(datos_unidos$Impacto_Ponderado, na.rm = TRUE)

# Mostrar el impacto total
impacto_total

# Filtrar materiales que tengan más de 5 documentos de compras asociados
materiales_con_mas_de_5 <- datos %>%
  group_by(Material) %>%
  filter(n() > 5) %>%
  ungroup()

# Seleccionar 10 materiales al azar
set.seed(125)  # Fijar semilla para reproducibilidad
materiales_seleccionados <- sample(unique(materiales_con_mas_de_5$Material), 10)

# Filtrar los datos para incluir solo los 10 materiales seleccionados
datos_seleccionados <- materiales_con_mas_de_5 %>%
  filter(Material %in% materiales_seleccionados)

# Calcular el precio de la primera compra para cada material
precio_primera_compra <- datos_seleccionados %>%
  group_by(Material) %>%
  summarise(Primera_Compra = min(Fecha)) %>%
  left_join(datos_seleccionados, by = c("Material", "Primera_Compra" = "Fecha")) %>%
  select(Material, Precio_USD) %>%
  rename(Precio_Primera_Compra = Precio_USD)

# Unir el precio de la primera compra con los datos seleccionados
datos_normalizados <- datos_seleccionados %>%
  left_join(precio_primera_compra, by = "Material") %>%
  mutate(Precio_Normalizado = Precio_USD / Precio_Primera_Compra)

# Graficar la evolución de los precios normalizados en el tiempo para los 10 materiales seleccionados
ggplot(datos_normalizados, aes(x = Fecha, y = Precio_Normalizado, color = Material, group = Material)) +
  geom_line() +
  labs(title = "Evolución de Precios Normalizados de Materiales Seleccionados en el Tiempo",
       x = "Fecha",
       y = "Precio Normalizado",
       color = "Material") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Agregar columna para indicar el tipo de compra (plaza, importación, NA)
# Crear una función para determinar el tipo de compra para cada material
determinar_tipo_compra <- function(proveedores) {
  proveedores_unicos <- unique(proveedores)
  if (all(startsWith(as.character(proveedores_unicos), "1"))) {
    return("Plaza")
  } else if (all(startsWith(as.character(proveedores_unicos), "5"))) {
    return("Importacion")
  } else {
    return(NA)
  }
}

# Calcular el tipo de compra para cada material
tipo_compra_por_material <- datos %>%
  group_by(Material) %>%
  summarise(Tipo_Compra_Material = determinar_tipo_compra(Proveedor))

# Unir el tipo de compra por material a la base de comparacion_precios
comparacion_precios <- comparacion_precios %>%
  left_join(tipo_compra_por_material, by = "Material")

# Verificar los primeros registros del resultado
head(comparacion_precios)

# Mostrar el impacto total
print(comparacion_precios)

# Crear el histograma de los cambios porcentuales en los precios, diferenciando por tipo de compra material
ggplot(comparacion_precios, aes(x = Cambio_Porcentual, fill = Tipo_Compra_Material)) +
  geom_histogram(binwidth = 10, position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Cambios Porcentuales en Precios por Tipo de Compra",
       x = "Cambio Porcentual (%)",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("Importacion" = "blue", "Plaza" = "red", "NA" = "gray")) +
  guides(fill=guide_legend(title="Tipo de Compra"))

# Mostrar el histograma
print(ggplot(comparacion_precios, aes(x = Cambio_Porcentual, fill = Tipo_Compra_Material)) +
        geom_histogram(binwidth = 10, position = "dodge", color = "black", alpha = 0.7) +
        labs(title = "Histograma de Cambios Porcentuales en Precios por Tipo de Compra",
             x = "Cambio Porcentual (%)",
             y = "Frecuencia") +
        theme_minimal() +
        scale_fill_manual(values = c("Importacion" = "blue", "Plaza" = "red", "NA" = "gray")) +
        guides(fill=guide_legend(title="Tipo de Compra")))
