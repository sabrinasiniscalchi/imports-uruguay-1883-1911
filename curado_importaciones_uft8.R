# install.packages("dplyr")
# library("dplyr")

#declaro funciones

escapar_parentesis <- function (str_con_parentesis) {
  string_curado <- gsub("\\(", "\\\\\\(", str_con_parentesis)
  string_curado <- gsub("\\)", "\\\\\\)", string_curado)
  string_curado
}

corregir_vector_por_correcciones <- function (vector, acorregir, reemplazar) {
  # si acorregir y reemplazar tienen diferente cantidad de filas
  # llevaria a errores, entonces simplemente devolvemos NA
  if(length(acorregir) != length(reemplazar)) {
    print("second and third  parameters should be of equal length")
    return(NA)
  }

  for (bien in vector) {
    # buscamos el indice de bien en acorregir
    ind <- match(bien, acorregir)

    # guardamos el valor por el cual tendriamos que reemplazar bien
    reemplazar_por <- reemplazar[ind]

    # si NO es na, se ejecuta lo que esta adentro del IF
    if (!is.na(reemplazar_por)) {
      # creamos una expresion regular, agregando "^" al principio y "$" al final
      # esto obliga a matchear exacto el nombre del bien
      expresion_regular <- paste("^", escapar_parentesis(bien), "$", sep = "")

      # Imprimimos el progreso para saber que esta trabajando
      print(paste("cambiando", expresion_regular, "por", reemplazar_por))

      # buscamos y reemplazamos todas las instancias matcheadas por el texto
      # a reemplazar
      vector <- gsub(expresion_regular, reemplazar_por, vector)
    }
  }

  as.factor(vector)
}

# defino el directorio base donde van a estar los archivos (reemplazar esta línea si se corre el script en una computadora diferente)
setwd("-")

# cargo el csv completo en "importaciones"
# strip.white quita los espacios antes y despues de cada valor en el csv
importaciones <- read.csv(
  file="importaciones_raw_v3.csv",
  strip.white=TRUE,
  fileEncoding="UTF-8",
  dec = ","
)

# transformo columnas a int
importaciones$valor <- as.numeric(importaciones$valor)
importaciones$cantidad <- as.numeric(importaciones$cantidad)

# cambiar a minusculas bienes y pais
importaciones$bien <- as.factor(tolower(importaciones$bien))
importaciones$pais <- as.factor(tolower(importaciones$pais))

# sacar tildes pais
importaciones$pais <- as.factor(gsub("á", "a", importaciones$pais))
importaciones$pais <- as.factor(gsub("é", "e", importaciones$pais))
importaciones$pais <- as.factor(gsub("í", "i", importaciones$pais))
importaciones$pais <- as.factor(gsub("ó", "o", importaciones$pais))
importaciones$pais <- as.factor(gsub("ú", "u", importaciones$pais))

# sacar tildes bien
importaciones$bien <- as.factor(gsub("á", "a", importaciones$bien))
importaciones$bien <- as.factor(gsub("é", "e", importaciones$bien))
importaciones$bien <- as.factor(gsub("í", "i", importaciones$bien))
importaciones$bien <- as.factor(gsub("ó", "o", importaciones$bien))
importaciones$bien <- as.factor(gsub("ú", "u", importaciones$bien))

# saco los bienes como un archivo aparte para poderlo ver mientras hago las correcciones.
# write.csv(as.factor(levels(importaciones$bien)), file="bienes_nombres_unicos.csv")

# curado de nombre de bienes
correcciones_raw <- read.csv(
  file="bienes_nombres_unicos_corregidos.csv",
  strip.white=TRUE,
  fileEncoding="UTF-8"
)

bienes_corregidos <- corregir_vector_por_correcciones(
  importaciones$bien,
  correcciones_raw$acorregir,
  correcciones_raw$reemplazar
)

importaciones_curadas <- data.frame(
  año=importaciones$año,
  bien=bienes_corregidos,
  pais=importaciones$pais,
  unidad_reportada=importaciones$unidad_reportada,
  unidad_estandarizada=importaciones$unidad_estandarizada,
  valor=importaciones$valor,
  cantidad=importaciones$cantidad,
  factor_conversion=importaciones$factor_conversion_cantidad
)

importaciones_curadas$cantidad_estandarizada <-
  importaciones_curadas$cantidad *
  importaciones_curadas$factor_conversion

importaciones_curadas$precio_implicito <-
  importaciones_curadas$cantidad_estandarizada /
  importaciones_curadas$valor

write.csv(
  importaciones_curadas,
  file="importaciones_curadas.csv",
  fileEncoding="UTF-8"
)
