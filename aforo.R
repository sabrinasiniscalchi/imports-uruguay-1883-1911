install.packages("dplyr")
library("dplyr")
setwd("-")

# traer los bienes que en todas las filas que aprezca tengan la misma unidad de medida
# y que esten presentes en los 28 años que estamos revisando
agrupar_bienes_aforo <- function (bienes, importaciones, cantidad_años) {
	muestra <- c()
	for (bien in bienes) {
		# traemos todas las filas en las que aparezca este bien
		ind <- importaciones$bien == bien

		# separamos los años unicos de esas filas
		años <- levels(as.factor(importaciones$año[ind]))

		# separamos las unidades reportadas unicas de esas filas
		unidades_de_medida <- levels(
			as.factor(
				as.character(
					importaciones$unidad_estandarizada[ind]
				)
			)
		)

		# si hay 28 años diferentes y hay una unica unidad reportada
		if (
			length(años) >= cantidad_años &&
			length(unidades_de_medida) == 1 &&
			unidades_de_medida != "valor" &&
      unidades_de_medida != "est"
		) {
			# lo agregamos a la muestra
			muestra <- c(muestra, bien)
		}
	}
	muestra
}

importaciones <- read.csv(
  file="importaciones_curadas.csv",
  strip.white=TRUE,
  fileEncoding="UTF-8",
  dec = "."
)

# defino vector con levels de los bienes
bienes <- levels(importaciones$bien)

bienes_muestra <- agrupar_bienes_aforo(bienes, importaciones, 25)

indice_muestra <- importaciones$bien %in% bienes_muestra

muestra <- data.frame(
	ano=importaciones$año[indice_muestra],
	bien=importaciones$bien[indice_muestra],
	pais=importaciones$pais[indice_muestra],
	precio=importaciones$precio_implicito[indice_muestra]
)

aforo <- muestra %>%
  group_by(ano, bien) %>%
  summarize(avg = mean(precio), sd = sd(precio))

aforo2 <-
  aforo %>%
  group_by(ano, bien) %>%
  summarize(sd_precio = sd(precio)) %>%
  filter(sd_precio >= 10)

aforo %>%
  ggplot(aes(x=ano, y=precio)) +
  geom_point() +
  geom_hline(aes(yintercept = mean(precio)))

# for(bien in bienes_muestra) {
# 	anos <- levels(as.factor(muestra$ano))
# 	for (ano in anos) {
# 		ind1 <- muestra$ano == ano
# 		ind2 <- muestra$bien == bien
# 	}
# }

#valores_x <- c()
#valores_y <- c()

#for (num in 1:28) {
#	agrupados <- agrupar_bienes_aforo(bienes, importaciones, num)
#	valores_x[num] <- num
#	valores_y[num] <- length(agrupados)
#}
#plot(valores_x, valores_y)
