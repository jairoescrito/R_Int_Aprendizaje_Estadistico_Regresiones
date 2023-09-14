###########################
## 1.Conceptos Iniciales ##
###########################

#########################
# 1.1 Algebra Matricial #
#########################

# Una matriz es un arreglo de datos de forma "array" de orden nxp: n filas y p columnas, esto es,
# n observaciones y p variables
A<-matrix(c(1,2,-1,3,2,2<.,0,1,3,-1,2,1,-1,3,-1,-1,2),
          ncol = 4,dimnames = list(c("1","2","3","4"),c("x","y","z","t")))
#La matriz A representa un sistema de ecuaciones 4x4
#    x + 2y - z + 3t = -8
#    2x + 2z - t = 13
#    -x + y + z - t = 8
#    3x + 3y - z + 2t = -1

B<-matrix(c(1,-1,1,1,2,1,-3,1,1,-2,2,-1,1,-3,3,-3),
          ncol = 4, byrow = TRUE,dimnames =list(c("1","2","3","4"),c("x","y","z","t")))
#La matriz A representa un sistema de ecuaciones 4x4
#    x - y + z + t = 4
#    2x + 2y - 3z + t = 4
#    x - 2y + 2z - t = 3
#    x - 3y + 3z - 3t = 2
# Una matriz de orden nx1 es un vector
a<-array(c(-8,13,8,-1),c(4,1))
b<-matrix(c(4,4,3,2),ncol = 1)
# Cada vector representa el resultado de cada ecuación para cada sistema

####################################
# 1.1.1 Operaciones entre matrices #
####################################

# Suma y resta: suma o resta de matrices del mismo orden
A+B
B+A
a+b
b+a
# Producto por escalar: producto de un escalar por una matriz
2*A
2*a
-1*B
-1*b
# Producto escalar: producto entre arrays del mismo orden, esto es, producto 1 a 1
A*B
a*b
# Producto: producto entre matrices, el número de columnas de la primera matriz debe ser igual
# al número de filas de la segunda matriz
A%*%B #4x4 * 4x4 = 4x4
a%*%b #genera un error
# Transpuesta: intercambio de filas por columnas
t(b) #convirte al vector en un vector fila
a%*%t(b) #4x1 * 1x4 = 4x4
t(a)%*%b #1x4 * 4x1 = 1x1
# Determinante: escalar particular de una matriz (matrices cuadradas)
det(A)
det(B)
det(a) # Genera un error
# Inversa: matriz solución, solo para matrices con determinante diferente de 0 
# (variables independientes)
solve(A) #Inversa de A
solve(A)%*%a #Solución del sistema de ecuaciones que representa A y a
solve(A,a) #Otra forma de solucionar el sistema de ecuaciones
Solve(B)
Solve(B)%*%b
solve(B,b)
# Ejercicio: calcular determinante, solución para el sistema de ecuaciones
# valores y vectores propios de la matriz de coeficientes del sistema
#    1a + 1b + 1c  = 1
#    2a + 3b - 4c = 9
#    a - b + c = -1

####################################
# 1.1.2 Vectores y Valores Propios #
####################################

# Valores propios: medidas básicas de tamaño de una matriz, 
# que no se ven alteradas si hacemos un cambio de coordenadas
# Vectores propios: direcciones características de la matriz
# Ver archivo PDF auxiliar para explicar el concepto

lambda<-eigen(Data_Hitters)

lambdaA<-eigen(A)
lambdaB<-eigen(B)
# Valores propios
lambdaA$values
lambdaB$values
# Vectores Propios
lambdaA$vectors
lambdaB$vectors

