-- Ejercicio 1. Definir la función promedio3 tal que (promedio3 x y z) 
promedio3 x y z = (x + y + z) / 3


-- Ejercicio 2. Definir la función sumaMonedas
sumaMonedas a b c d e = a * 1 + b * 2 + c * 5 + d * 10 + e * 20

--Ejercicio 3 Volumen esfera
volumenEsfera r = (4 / 3) * pi * r^3

--Ejercicio 4. Función areaDeCoronaCircular
areaDeCoronaCircular r1 r2 = pi * (r2^2 - r1^2)

-- Ejercicio 5. Función ultimaCifra
ultimaCifra x = rem x 10

-- Ejercicio 6. Definir la función maxTres
maxTres x y z = max (max x y) z

-- Ejercicio 7. Función rota1
rota1 xs = tail xs ++ [head xs]

-- Ejercicio 8. Función rota
rota n xs = drop n xs ++ take n xs

-- Ejercicio 9. Función rango
rango xs = [minimum xs, maximum xs]

-- Ejercicio 10. Función palindromo
palindromo xs = xs == reverse xs

-- Ejercicio 11. Función interior
interior xs = init (tail xs)

-- Ejercicio 13. Función segmento
segmento m n xs = take (n - m + 1) (drop (m - 1) xs)

-- Ejercicio 14. Función extremos
extremos n xs = take n xs ++ drop (length xs - n) xs

-- Ejercicio 15. Función mediano
mediano x y z = x + y + z - (minimum [x, y, z]) - (maximum [x, y, z])

-- Ejercicio 16. Función tresIguales
tresIguales x y z = x == y && y == z

-- Ejercicio 17. Función tresDiferentes
tresDiferentes x y z = x /= y && x /= z && y /= z

-- Ejercicio 18. Función cuatroIguales
cuatroIguales x y z u = tresIguales x y z && x == u


------------------------- Guardas y Patrones ------------------------

-- Ejercicio 1. Función divisionSegura
divisionSegura x y 
    | y /= 0    = x / y
    | otherwise = 9999.0

-- Ejercicio 2. Función xor1
xor1 x y 
    | x && not y = True   
    | not x && y = True  
    | otherwise = False    

-- Ejercicio 3. Función mayorRectangulo
mayorRectangulo (b1, h1) (b2, h2)
    | area1 > area2 = (b1, h1)   
    | area1 < area2 = (b2, h2)   
    | otherwise     = (b1, h1)   
    where
        area1 = b1 * h1   
        area2 = b2 * h2   

-- Ejercicio 4. Función intercambia      
intercambia (x, y)
    | x == y   = (y, x)   
    | otherwise = (y, x)  

-- Ejercicio 5. Función distancia
distancia (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2 = 0.0              -- Si son el mismo punto
    | otherwise  = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)  -- Cálculo normal

-- Ejercicio 6. Función ciclo
ciclo xs
    | null xs     = []    
    | length xs == 1 = xs        
    | otherwise = last xs : init xs  -- Mueve el último elemento al principio

-- Ejercicio 7. Función numeroMayor
numeroMayor x y
    | x >= y    = x * 10 + y
    | otherwise  = y * 10 + x

-- Ejercicio 8. Función numeroDeRaices
numeroDeRaices a b c
    | discriminante > 0 = 2
    | discriminante == 0 = 1
    | otherwise  = 0
    where discriminante = b^2 - 4 * a * c

-- Ejercicio 9. Función raices
raices a b c
    | discriminante < 0 = []
    | discriminante == 0 = [x]
    | otherwise         = [x1, x2]
    where
        discriminante = b^2 - 4 * a * c
        x = -b / (2 * a)
        x1 = (-b + sqrt discriminante) / (2 * a)
        x2 = (-b - sqrt discriminante) / (2 * a)

-- Ejercicio 10. Función area
area a b c
    | a + b <= c || a + c <= b || b + c <= a = 0.0
    | otherwise = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

-- Ejercicio 11. Función interseccion
interseccion [] _ = []
interseccion _ [] = []
interseccion [a, b] [c, d]
    | b < c || a > d = []
    | otherwise = [max a c, min b d]

-- Ejercicio 12. Función linea
linea n = [first + i | i <- [0..(n-1)]]
    where first = (n * (n - 1)) `div` 2 + 1



----------------------------Recursividad------------------------------------


-- Ejercicio 1: potencia
potencia x n 
    | n == 0    = 1
    | n > 0     = x * potencia x (n - 1)
    | otherwise = error "n debe ser un número natural"

-- Ejercicio 2: mcd
mcd a b 
    | b == 0    = a
    | otherwise = mcd b (mod a b)

-- Ejercicio 3: pertenece
pertenece _ [] = False
pertenece x (y:ys) 
    | x == y    = True
    | otherwise = pertenece x ys

-- Ejercicio 4: tomar
tomar n _ | n <= 0 = []
tomar _ [] = []
tomar n (x:xs) = x : tomar (n - 1) xs

-- Ejercicio 5. Función que devuelve la lista de los dígitos del número n utilizando recursividad.
digitosC 0 = []
digitosC n = digitosC (n `div` 10) ++ [n `mod` 10]


-- Ejercicio 6: Definir la función sumaDigitosR que suma los dígitos de un número.
sumaDigitosR 0 = 0
sumaDigitosR n = (n `mod` 10) + sumaDigitosR (n `div` 10)

-- Ejercicio 2.1 QuickSort.
ordenaRapida [] = []
ordenaRapida (x:xs) =
    let menores = ordenaRapida [y | y <- xs, y <= x]
        mayores = ordenaRapida [y | y <- xs, y > x]
    in menores ++ [x] ++ mayores



-------------------------------------------Tipo de Datos-----------------------------------------



-- Definición del nuevo tipo de dato Estudiante
-- Definición del nuevo tipo de dato Estudiante
data Estudiante = Estudiante {
    nombre      :: String,
    apellido    :: String,
    edad        :: Int,
    numControl  :: String
} deriving (Show)

-- Lista de estudiantes
estudiantes = [
    Estudiante "Juan" "Perez" 17 "001",
    Estudiante "Maria" "Gomez" 18 "002",
    Estudiante "Luis" "Martinez" 20 "003",
    Estudiante "Ana" "Lopez" 22 "004",
    Estudiante "Carlos" "Torres" 19 "005",
    Estudiante "Lucia" "Reyes" 21 "006",
    Estudiante "Jorge" "Morales" 16 "007",
    Estudiante "Sofia" "Sanchez" 24 "008",
    Estudiante "Ricardo" "Diaz" 20 "009",
    Estudiante "Valeria" "Castro" 19 "010"
 ]

-- Función para ordenar estudiantes por edad (QuickSort)
quickSort [] = []
quickSort (p:xs) = quickSort [y | y <- xs, edad y <= edad p] ++ [p] ++ quickSort [y | y <- xs, edad y > edad p]

-- Obtener estudiante menor
estudianteMenor = head . quickSort

-- Obtener estudiante mayor
estudianteMayor = last . quickSort

-- Calcular promedio de edades
promedioEdades estudiantes = fromIntegral (sum (map edad estudiantes)) / fromIntegral (length estudiantes)


--------------------------------------Arboles------------------------------


--Crear tipo de dato Arbol
data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Show, Eq)

generarNodo :: a -> Arbol a
generarNodo x = Nodo x Hoja Hoja

--Insertar
insertar x Hoja = generarNodo x
insertar x (Nodo a izq der)
 | x < a = Nodo a (insertar x izq) der
 | x > a = Nodo a izq (insertar x der)
 | otherwise = Nodo a izq der

--Buscar
buscar x Hoja = False
buscar x (Nodo a izq der)
  | x == a = True
  | x < a = buscar x izq
  | x > a = buscar x der

-- Recorrido en orden (in-orden)
inOrden Hoja = []
inOrden (Nodo a izq der) = inOrden izq ++ [a] ++ inOrden der

-- Recorrido post orden (post-orden)
posOrden Hoja = []
posOrden (Nodo a izq der) = posOrden izq ++ posOrden der ++ [a]

-- Recorrido pre orden (pre-orden)
preOrden Hoja = []
preOrden (Nodo a izq der) = [a] ++ preOrden izq ++ preOrden der
