### INTRODUCCION A R ###

#VECTORES
#Vector numérico
Vector <- c(1,2,3,4,5)

#Vector de carácteres
Vector2 <- c("a", "b", "c", "1", "2", "3")

#Vector lógico
Vector3 <- c(TRUE, FALSE, TRUE)

#Clase y estructura
class(Vector2)
str(Vector2)

Vector4 <- c(1,2,3,"a","b")


#ARREGLOS
#Arreglo de 2 dimensiones
Arreglo <- array(c(5, 10, 15, 20), dim = c(2,2))

#Arreglo de 3 dimensiones
Arreglo2 <- array(c(5, 10, 15, 20, 25, 30, 35, 40), dim = c(2,2,2))

#Poniéndole nombre a nuestro arreglo
Arreglo3 <- array(c(5, 10, 15, 20), dim = c(2,2), dimnames = list(c("a", "b"), c("c", "d")))



#MATRICES
#Matriz de dos dimensiones
Matriz <- matrix(c(1:20), nrow = 10, ncol = 2, byrow = FALSE)

#Matriz de dos dimensiones con 'byrow = TRUE'
Matriz2 <- matrix(c(1:20), nrow = 10, ncol = 2, byrow = TRUE)

#Matriz con nombres
Matriz3 <- matrix(c(1:10), nrow = 5, ncol = 2, byrow = FALSE, dimnames = list(c("sp1", "sp2", "sp3", "sp4", "sp5"), c("A", "B")))



#FACTORES
#Crear nuestro propio factor.
Factor <- factor(levels = 1:5, labels = "Especie")

#Convertir un vector en factor.
Factor2 <- factor(Vector2)



#DATAFRAME
#Dataframe de 2x2
DF <- data.frame(Especies = c("sp1", "sp2", "sp3", "sp4", "sp5"), Abundancias = c(7, 3, 4, 1, 9), Logico = c(TRUE, FALSE, FALSE, FALSE, TRUE))

#Dataframes más complejos
DF2 <- data.frame(Especie = c("sp1", "sp2", "sp3"), SitioA = c(3, 1, 1), SitioB = c(9, 1, 3), SitioC = c(1, 2, 5))

#Extraer valores de una columna
Especies <- factor(DF2$Especie)



#LISTAS
#'Bolsas' donde metemos todo
Lista <- list(Vector, Factor2, Factor, DF2, Matriz3)

#Sacar un valor de la lista por su índice
DF3 <- Lista[[4]]
