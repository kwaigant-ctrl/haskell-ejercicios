saludar :: String -> String
saludar nombre = "Hola " ++ nombre

{-Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3, p.ej: 
Main> esMultiploDeTres 9 
True -}

esMultiploDeTres :: Int -> Bool
esMultiploDeTres num = mod num 3 == 0

{-Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero, p.ej. 
Main> esMultiploDe 3 12
True
-}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe num1 num2 = mod num2 num1 == 0 --el segundo es multiplo del primero (el resto de 2/1 es 0)

{-Definir la función cubo/1, devuelve el cubo de un número.-}

cubo :: Int -> Int
cubo n = n ^ 3 -- o n*n*n

{-Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
-}

area :: Int -> Int -> Int
area base altura = base * altura

{-Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 
400 o es divisible por 4 pero no es divisible por 100) Nota: Resolverlo reutilizando la función esMultiploDe/2
-}

esBiciesto :: Int -> Bool
--esBiciesto year = (mod year 400 == 0) || (mod year 4 == 0 && mod year 100 /= 0)
esBiciesto year = esMultiploDe 400 year || esMultiploDe 4 year && not(esMultiploDe 100 year)

{-Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.
-} --(0 °C × 9/5) + 32 = 32 °F

celsiusToFahr :: Float -> Float
celsiusToFahr c = c * (9/5) + 32

{-Definir la función fahrToCelsius/1, la inversa de la anterior.
-}

fahrToCelsius :: Float -> Float
fahrToCelsius f = (f - 32) * 5/9

{-Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. 
Decimos que hace frío si la temperatura es menor a 8 grados Celsius. 
-}

haceFrio :: Float -> Bool
haceFrio x = fahrToCelsius x < 8

{-Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta 
fórmula. m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)} 
Nota: Se puede utilizar gcd.
-}

mcm :: Int -> Int -> Int
mcm a b = div (a * b) (gcd a b)

{-Dispersión
Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos; 
cada medición es un entero que representa una cantidad de cm. 
P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
A partir de estos tres números, podemos obtener algunas conclusiones. 
Definir estas funciones: 

dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres 
argumentos, usando las versiones de dos elementos. De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas 
están en max y min, que estamos usando). 

diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión es chica, 
que son días locos si la dispersión es grande, y que son días normales si no son ni parejos ni locos. Una dispersión se considera chica 
si es de menos de 30 cm, y grande si es de más de un metro. 
Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas. 
-}

dispersion :: Int -> Int -> Int -> Int
dispersion n1 n2 n3 = max(max n1 n2) n3 - min(min n1 n2) n3  --max y min con 2 argumentos nomas

diasParejos :: Int -> Int -> Int -> Bool    -- le envia el true o false a dias normales
diasParejos n1 n2 n3 = dispersion n1 n2 n3 < 30

diasLocos :: Int -> Int -> Int -> Bool 
diasLocos n1 n2 n3 = dispersion n1 n2 n3 > 100

diasNormales :: Int -> Int -> Int -> String   --la funcion recibe 3 parametros para pasarselos a las subfunciones
diasNormales n1 n2 n3
    | diasParejos n1 n2 n3 = "Dispersion chica"    
    | diasLocos n1 n2 n3 = "Dispersion grande"     


{-En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. 
El peso de un pino se puede calcular a partir de la altura así:

3 kg x cm      hasta 3 metros, 
2 kg x cm      arriba de los 3 metros. (lo que falta despues de contar los primeros 3 metros)

1 metro = 100cm

P.ej. 2 metros ⇒  600 kg, 
5 metros (500 cm, 300+200cm)⇒  1300 kg.  -> 3*300 + 2*200

Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, 
un pino fuera de este rango no le sirve a la fábrica. 

Para esta situación: 
Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. 
Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. 
Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, y False 
en caso contrario. Usar composición en la definición. 
-}

pesoPino :: Int -> Int                                   -- recibe cm, devuelve kg
pesoPino altura
    | altura > 300 = 900 + (altura - 300)*2              -- sumo lo que pesaria la parte de abajo (900kg)
    | altura <= 300 = 3 * altura

esPesoUtil :: Int -> Bool
esPesoUtil kg = kg >= 400 && kg <= 1000

sirvePino :: Int -> Bool
sirvePino altura = esPesoUtil(pesoPino altura) 

{-Este ejercicio alguna vez se planteó como un Desafío Café con Leche: Implementar la función esCuadradoPerfecto/1, 
sin hacer operaciones con punto flotante. Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. 
Pensar que el primer cuadrado perfecto es 0, para llegar al 2do (1) sumo 1, para llegar al 3ro (4) sumo 3, para llegar 
al siguiente (9) sumo 5, después sumo 7, 9, 11 etc.. También algo de recursividad van a tener que usar. 

Los cuadrados perfectos se pueden generar sumando números impares consecutivos

preguntan si el nro es cuadrado perfecto-}

esCuadradoPerfecto :: Int -> Bool
esCuadradoPerfecto n = auxiliar n 1 -- aca declaro que i vale 1, para armar un contador. el auxiliar me devuelve un bool

auxiliar :: Int -> Int -> Bool
auxiliar n i
    | n == 0 = True       -- caso base
    | n < 0 = False
    | otherwise = auxiliar (n - i) (i + 2)  -- nuevos datos, le resto 1 a n, al contador se le suma 2 y se convierte en un nuevo nro impar

    