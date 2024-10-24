-------------------------Funciones Básicas--------------------------------------------------------------------

-- Ejercicio 1. Definir la función promedio3 tal que (promedio3 x y z) es el promedio aritmético de los números x, y y z. 
--Nombre de la funcion: promedio3            
--Descripcion: Calcula el promedio de tres numeros.
--Variables: x y z   Valor de retorno: promedio aritmetico
promedio3 :: Fractional a => a -> a -> a -> a
promedio3 x y z = (x + y + z )/3

-- Ejercicio 2. Definir la función sumaMonedas tal que
-- (sumaMonedas a b c d e) es la suma de las monedas  correspondientes a monedas de 1 peso, b de 2 pesos, c de 5 pesos, d 10 pesos y de de 20 pesos.
--Nombre de la funcion: sumaMonedas
--Descripcion:calcula la suma de monedas de acuerdo a su cantidad.
-- Variables: a(1), b(2), c(5), d(10),(20)    Valor de retorno: suma total de monedas.
sumaMonedas :: Num a => a -> a -> a -> a -> a -> a
sumaMonedas a b c d e = a + (b * 2) +(c * 5) + (d *10) + (e * 20)

-- Ejercicio 3. Definir la función volumenEsfera tal que (volumenEsfera r) es el volumen de la esfera de radio r.
--Nombre de la funcion: volumenEsfera
--Descripcion: Calcula el volumen de una esfera conociendo su radio 
--Variables: radio            Valor de retorno: volumen(Floating)
volumenEsfera :: Floating a => a -> a
volumenEsfera radio = ((4 * pi) * (radio^3 ))/3

-- Ejercicio 4. Definir la función areaDeCoronaCircular tal que (areaDeCoronaCircular r1 r2) es el área de una corona circular de
-- radio interior r1 y radio exterior r2.
--Nombre de la funcion: areaDeCoronaCircular
--Descripcion: calcula el area de una corona circular por medio del radio interior y exterior.
--Variables: radiointerior radio exterior                Valor de retorno: el area(Floating)
areaDeCoronaCircular :: Floating a => a -> a -> a
areaDeCoronaCircular radiointerior radioexterior = pi * ((radioexterior^2) - (radiointerior^2))

-- Ejercicio 5. Definir la función ultimaCifra tal que (ultimaCifra x) es la última cifra del número x. 
--Nombre de la funcion: ultimaCifra
--Descripcion: Retorna la ultima cifra de un numero
--Variables num                  Valor de retorno: ultima cifra(Int)
ultimaCifra :: Integral a => a -> a
ultimaCifra num = rem num 10

-- Ejercicio 6. Definir la función maxTres tal que (maxTres x y z) es el máximo de x, y y z.
--Nombre de la funcion: maxTres
--Descripcion: Recibre tres numeros y regresa cual es el mayor de ellos.
--Variables: num1 num2 num3                      Valor de retorno: mayor de tres numeros
maxTres :: Ord a => a -> a -> a -> a
maxTres num1 num2 num3 = max num1 (max num2 num3)

-- Ejercicio 7. Definir la función rota1 tal que (rota1 xs) es la lista obtenida poniendo el primer elemento de xs al final de la lista.
--Nombre de la funcion: rota1
--Descripcion: recibe una lista y regresa la misma lista poniendo el primer elemento al final
--Variables : Una lista (xs)                 Valor de retorno: lista 
rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]

-- Ejercicio 8. Definir la función rota tal que (rota n xs) es la lista obtenida poniendo los n primeros elementos de xs al final de la lista. 
--Nombre de la funcion: rota
--Descripcion: recibe un numero y una lista, y mueve los primeros n elementos de la lista al final.
--Variables: num (Cantidad de elementos), xs(lista de numeros)        Valor de retorno: lista modificada
rota :: Int -> [a] -> [a]
rota num xs = drop num xs ++ take num xs

-- Ejercicio 9. Definir la función rango tal que (rango xs) es la lista formada por el menor y mayor elemento de xs.
--Nombre de la funcion: rango
--Descripcion: recibe una lista , y encuentra el valor menor y mayor de la lista.
--Variables: lista(xs)        Valor de retorno: [valor minimo, valor maximo]
rango :: (Foldable t, Ord a) => t a -> [a]
rango xs = minimum xs : [maximum xs]

-- Ejercicio 10. Definir la función palindromo tal que (palindromo xs) se verifica si xs es un palíndromo; es decir, es lo mismo leer xs de izquierda a derecha que de derecha a izquierda.
--Nombre de la funcion: palindromo
--Descripcion:  Comprueba si una lista es palindromo o no
--Variables: lista(xs)                  Valor de retorno: True-palindromo, False-no palindromo
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

--Ejercicio 11. Definir la función interior tal que (interior xs) es la lista obtenida eliminando los extremos de la lista xs.
--Nombre de la funcion: interior
--Descripcion: Elimina el primer elemento y el ultimo elemento de una lista.
--Variables: lista(xs)                    Valor de retorno:[lista modificada]
interior :: [a] -> [a]
interior xs = tail (init xs)


--Ejercicio 13. Definir la función segmento tal que (segmento m n xs) es la lista de los elementos de xs comprendidos entre las posiciones m y n.
--Nombre de la funcion: segmento
--Descripcion: Regresa una sublista de una posicion m a n
--Variables: m(primer posicion) n(ultima posicion) lista(xs)             Valor de retorno: [sublista de m a n]
segmento :: Int -> Int -> [a] -> [a]
segmento m n xs = drop (m-1) (take n xs)

-- Ejercicio 14. Definir la función extremos tal que (extremos n xs) es la lista formada por los n primeros elementos de xs y los n finales elementos de xs.
--Nombre de la funcion: extremos
--Descripcion: regresa los primeros y ultimos elementos de una lista.
--Variables: n( cantidad de n elementos) lista(xs)           Valor de retorno:[lista modificada]
extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs

-- Ejercicio 15. Definir la función mediano tal que (mediano x y z) es el número mediano de los tres números x, y y z.
--Nombre de la funcion: mediano
--Descripcion: numero mediano entre x y z (numeros)
mediano :: (Num a, Ord a) => a -> a -> a -> a
mediano x y z = x + y + z- minimum [x,y,z] - maximum [x,y,z]

-- Ejercicio 16. Definir la función tresIguales tal que (tresIguales x y z) se verifica si los elementos x, y y z son iguales.
--Nombre de la funcion: tresIguales
--Descripcion: Verifica si tres numeros son iguales
--Variables: x (numero 1), y (numero 2), z (numero 3)  Valor de retorno: True,False
tresIguales :: Eq a => a -> a -> a -> Bool
tresIguales x y z = x == y && x ==z

-- Ejercicio 17. Definir la función tresDiferentes tal que
-- (tresDiferentes x y z) se verifica si los elementos x, y y z son distintos.
--Variables x y x (numeros)      Valor de retorno: True si los 3 son distintos, sino False.
tresDiferentes :: Eq a => a -> a -> a -> Bool
tresDiferentes x y z = x/=y && x/=z && y/=z

-- Ejercicio 18. Definir la función cuatroIguales tal que
-- (cuatroIguales x y z u) se verifica si los elementos x, y, z y u son iguales.
cuatroIguales :: Eq a => a -> a -> a -> a -> Bool
cuatroIguales x y z u = tresIguales x y z && z ==u

-- Ejercicio 1. Definir la función divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso contrario.
divisionSegura :: Double -> Double -> Double
divisionSegura _ 0 = 9999
divisionSegura x y = x / y

-- Ejercicio 2. Definir la función xor1 :: Bool -> Bool -> Bool
-- tal que (xor1 x y) es la disyunción excluyente de x e y, calculada a partir de la tabla de verdad.
xor1 :: Bool -> Bool -> Bool
xor1 True True  = False
xor1 True False = True
xor1 False True  = True
xor1 False False = False

-- Ejercicio 3.Definir la función mayorRectangulo :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integ
-- tal que (mayorRectangulo r1 r2) es el rectángulo de mayor área entre r1 y r2. 
mayorRectangulo :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
mayorRectangulo (r1,r2) (r3, r4)
  | r1*r2 >= r3*r4 = (r1,r2)
  | otherwise  = (r3,r4) 

-- Ejercicio 4. Definir la función intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p) es el punto obtenido intercambiando las coordenadas del punto p.
intercambia :: (a,b) -> (b,a)
intercambia (a,b)= (b,a)

-- Ejercicio 5. Definir la función
-- distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y p2.
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)

-- Ejercicio 6. Definir una función
-- ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando cíclicamente los elementos de la lista xs, pasando el último elemento al principio de la lista.
ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = last xs : init xs

-- Ejercicio 7. Definir la función
-- numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede construirse con los dígitos x e y. 
numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y = a*10 + b
    where a = max x y 
          b = min x y

-- Ejercicio 8. Definir la función
-- numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
-- tal que (numeroDeRaices a b c) es el número de raíces reales de la
-- ecuación a*x^2 + b*x + c = 0.
numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c
    | d > 0     = 2
    | d == 0    = 1
    | otherwise = 0
  where d = b^2 - 4 * a * c

-- Ejercicio 9. Definir la función
-- raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raíces reales de la
-- ecuación ax^2 + bx + c = 0.
raices :: Double -> Double -> Double -> [Double]
raices a b c 
    | d >= 0    = [(-b+e)/t,(-b-e)/t]
    | otherwise = []
    where d = b**2 - 4*a*c
          e = sqrt d
          t = 2*a

-- Ejercicio 10.Definir la función
-- area :: Double -> Double -> Double -> Double
-- tal que (area a b c) es el área del triángulo de lados a, b y c. 
area :: Double -> Double -> Double -> Double
area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2

-- Ejercicio 11.Definir la función
--interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la intersección de los intervalos i1 e i2. 
interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a1,b1] [a2,b2]
    | a <= b    = [a,b]
    | otherwise = []
    where a = max a1 a2
          b = min b1 b2

--Ejercicio 12.Definir la función
--linea :: Integer -> [Integer]
-- tal que (linea n) es la línea n-ésima de los triángulos aritméticos.         
linea :: Integer -> [Integer]
linea 1 = [1] 
linea n = [inicio..fin]
  where inicio = (n * (n - 1)) `div` 2 + 1
        fin = inicio + n - 1

-- Ejercicio 1. Definir por recursión la función
--potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al número natural n.
--variables num exp             Valor de retorno: el numero elevado a la potencia.
potencia :: Integer -> Integer -> Integer
potencia num1 exp
  |exp == 0  = 1
  |otherwise = num1 * potencia num1 (exp -1)

--Ejercicio 1
-- Definir la función mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el máximo común divisor de a y b
mcd :: Integer -> Integer -> Integer
mcd num1 0 = num1
mcd num1 num2 = mcd num2 (num1 `mod` num2)

-- Ejercicio 3, Definir por recursión la función
--pertenece :: Eq a => a -> [a] -> Bool
-- tal que (pertenece x xs) se verifica si x pertenece a la lista xs.
pertenece :: Eq t => t -> [t] -> Bool
pertenece _ [] = error "Lista vacia"
pertenece  n [x] = n == x
pertenece n (x:xs)= n == x || pertenece n xs

-- Ejercicio 4. Definir por recursión la función
--tomar :: Int -> [a] -> [a]
-- tal que (tomar n xs) es la lista de los n primeros elementos de xs.
tomar :: Int -> [a] -> [a]
tomar n _ 
 |n<=0 = []  
tomar _ [] = [] 
tomar n (x:xs) = x : tomar (n - 1) xs

-- Ejercicio 5. Definir, por comprensión, la función
--digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n.
digitosC :: Integer -> [Integer]
digitosC 0 =[]
digitosC num = digitosC (div num 10) ++ [mod num 10]  

-- Ejercicio 6. Definir, por recursión, la función
-- sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n.
sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR n = mod n 10 + sumaDigitosR (div n 10)

--Ejercicio 2.1.Definir la función ordenaRapida :: Ord a => [a] -> [a]
--Descripcion: (ordenaRapida xs) es la lista obtenida ordenando de menor a mayor los elementos de la lista xs.
ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) = ordenaRapida [y | y <- xs, y <= x ] ++ [x]++ ordenaRapida [y | y <- xs, y > x ]

--1.- Crea un nuevo tipo Estudiante con los siguientes atributos
-- Nombre, Apellido, Edad, Número de control 
data Estudiante = Estudiante{
                  nombre::String,
                  apellido::String,
                  edad:: Int,
                  numeroControl::Int
                } deriving Show

-- Generar una lista de un mínimo de 10 estudiantes.
listaEstudiante :: [Estudiante]
listaEstudiante = [Estudiante "Martha" "Gomez" 20 21160646,
                   Estudiante "Carlos" "Diaz" 29 21160658,
                   Estudiante "Alexia" "Perez" 21 21160623,
                   Estudiante "Angel" "Castellanos" 22 21160629,
                   Estudiante "Fernanda" "Lopez" 23 21160631,
                   Estudiante "Alan" "Carranza" 24 21180923,
                   Estudiante "Jazmin" "Gutierrez" 25 21160723,
                   Estudiante "Manuel" "Juarez" 19 19160789,
                   Estudiante "Sebastian" "Carrasco" 22 20160789,
                   Estudiante "Marcos" "Torres" 21 21160438]

--Descripcion: obtiene la edad de un elemento de tipo Estudiante
obtenerEdad:: Estudiante -> Int
obtenerEdad (Estudiante _ _ edad _ )= edad

--Descripcion: Ordena una lista por medio de su edad a traves de listas intencionales y recursion, menor a mayor y regresa la lista ordenada.
ordenada :: [Estudiante] -> [Estudiante]
ordenada []= []
ordenada (x:xs) = ordenada [y | y <- xs, obtenerEdad(y) <= obtenerEdad (x) ] ++ [x]++ ordenada [y | y <- xs, obtenerEdad(y) > obtenerEdad (x) ]

--Descripcion estudianteMenor: 
--estudianteMenor: Devuelve el primer estudiante en la lista ordenada (el estudiante con menor edad).
estudianteMenor :: Estudiante
estudianteMenor =head (ordenada listaEstudiante)

--Descripcion estudianteMayor: 
--estudianteMayor: Devuelve el ultimo estudiante en la lista ordenada (el estudiante con mayor edad).
estudianteMayor :: Estudiante
estudianteMayor = last (ordenada listaEstudiante) 

--Descripcion: promedioEdad:
--Calcula el promedio de edad de los estudiantes en una lista. Obtiene la edad de los los elementos de la lista estudiante
--y los suma, despues divide entre el num de elementos de la lista.
promedioEdad :: [Estudiante] -> Double
promedioEdad xs = fromIntegral (sumaEdad xs) / fromIntegral (length xs)
  where
    sumaEdad [] = 0
    sumaEdad (x:xs) = obtenerEdad x + sumaEdad xs

-- Estructura del árbol
--Se define un tipo de dato Arbol que puede ser una Hoja o un Nodo con un valor (de tipo a) y dos subárboles (izquierdo y derecho), sus elementos pueden ser mostrados y comparados.
data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show, Eq)

--Descripcion: Crea un nodo con el valor x recibido y con dos hojas (subarbol izquierdo y subarbol derecho).
generarNodo ::  a -> Arbol a 
generarNodo x = Nodo x Hoja Hoja

--Funcion insertar: Inserta un valor x en un árbol.
--Si el valor x es menor que el nodo actual, se inserta en el subárbol izquierdo; si es mayor, en el subárbol derecho. Si es igual, no se realiza ningún cambio.
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Hoja = generarNodo x
insertar x (Nodo a izq der)
 |x < a = (Nodo a (insertar x izq) der)
 |x > a = (Nodo a izq (insertar x der))
 |otherwise = Nodo a izq der

--Nombre de la funcion: insertarLis
--Descripcion: Inserta los elementos de una lista en un árbol uno por uno.
--Recibe una lista y los inserta en un arbol mediante la funcion insertar, al final regresa el arbol completo con sus elementos.
insertarLis :: Ord a => [a] -> Arbol a -> Arbol a
insertarLis [] arbol = arbol
insertarLis (x:xs) arbol= insertarLis xs arbolNuevo
 where 
    arbolNuevo = insertar x arbol


--Nombre de la funcion: buscarNodo
--Descripcion: busca un elemento dentro de un arbol, si el elemento se encuentra en el arbol, devuelve True, sino False.
--Variables: x(elemento a buscar), (Nodo a izq der)(Arbol)             Variable de retorno: True, False
buscarNodo :: (Ord a) => a -> Arbol a -> Bool
buscarNodo x Hoja = False
buscarNodo x (Nodo a izq der)
 | x == a = True
 | x < a = buscarNodo x izq
 | otherwise = buscarNodo x der


--Recorrido preorden, recibe un arbol y regresa una lista de sus elementos en preorden
--  preorden (raíz, izquierda, derecha)
preorden :: Arbol a -> [a]
preorden Hoja = []
preorden (Nodo raiz izq der) = [raiz] ++ preorden izq ++ preorden der

--Recorrido inorden, recibe un arbol y regresa una lista de sus elementos en inorden
-- inorden (izquierda, raíz, derecha)
inorden :: Arbol a -> [a]
inorden Hoja = []
inorden (Nodo raiz izq der) = inorden izq ++ [raiz] ++ inorden der

--Recorrido postorden, recibe un arbol y regresa una lista de sus elementos en postorden
-- postorden (izquierda, derecha, raíz)
postorden :: Arbol a -> [a]
postorden Hoja = []
postorden (Nodo raiz izq der) = postorden izq ++ postorden der ++ [raiz]


